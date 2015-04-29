{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, RecordWildCards #-}

module Horus (
    app
) where

import Calculator
import Calculator.Data.AST
import Calculator.Data.Decimal
import Calculator.Data.Env
import Calculator.Data.Function (Function, showDeclaration)
import Control.Applicative (optional)
import Control.Exception (catch, ErrorCall(..))
import Control.Monad.Except (mapExcept, runExcept, liftIO, msum)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Aeson hiding (Result, Number)
import Data.Aeson.Types (parseMaybe, Pair)
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Happstack.Server hiding (body, result)
import Serializer()
import System.UUID.V4 (uuid)
import UserState
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.Map as Map

app :: AcidState UserDb -> ServerPart Response
app acid = msum
  [ dir "calculate" (calc acid)
  , dir "userInfo" (getUserInfo acid)       -- GET
  , dir "userInfo" (resetUserInfo acid)     -- DELETE
  , dir "userInfo" (modifyUserInfo acid)    -- POST (patch)
  ]

calc :: AcidState UserDb -> ServerPart Response
calc acid = do
        method POST
        rq <- askRq
        if contentType "application/json" rq then do
            maybeBody <- takeRequestBody rq
            case decode $ unBody $ fromMaybe (Body "") maybeBody :: Maybe Object of
                Just jsonContent -> do
                    let input = parseDefault "" "input" jsonContent
                    userId <- getUserId
                    addCookie Session $ mkCookie "user-id" userId
                    User{..} <- query' acid (UserState.GetUser userId)
                    let userPrefs = parseDefault prefs "prefs" jsonContent
                    res <- getReturnText input userPrefs $ Env variables (Map.map fst functions)
                    case res of 
                        (Left err) -> badRequest $ jsonResponse [ "error" .= err ]
                        (Right ans) -> do
                            let newVars = vars ans
                            let newFuncs = addFunctionText input $ copyFunctionText (funcs ans) functions
                            let addedFuncs = Map.differenceWith (\a b -> if a /= b then Just a else Nothing) newFuncs functions
                            update' acid (UserState.SetUser userId $ User newVars newFuncs userPrefs)
                            ok $ jsonResponse $ makeJSON (Map.mapWithKey (varsToJSON $ boundResults ans) $ vars ans)
                                                         (Map.mapWithKey funcsToJSON addedFuncs)
                                                         ("result" .= answer ans)
                Nothing -> badRequest $ toResponse ("Unable to parse body" :: String)
        else resp 415 $ toResponse ("Content type must be application/json" :: String)
        where -- Extract the user-entered body of the function
              getFunctionText input = dropWhile isSpace $ drop 1 $ dropWhile (not . (==) '=') input
              -- Add the correct function text for new or modified functions
              addFunctionText input = Map.map (\(f,name) -> if null name then (f, getFunctionText input) else (f,name))
              makeJSON vs fs res = [ "vars" .= vs
                                   , "funcs" .= fs
                                   , res
                                   ]
              -- Copy the text of the original functions to the new
              -- functions, with a default value of "".
              -- Union works since the user can't delete functions here
              copyFunctionText newFuncs = Map.unionWith (\new@(newF,_) (oldF, text) -> if newF == oldF then (newF,text) else new) (Map.map (\x -> (x,"")) newFuncs)

getUserInfo :: AcidState UserDb -> ServerPart Response
getUserInfo acid = do
    method GET
    userId <- getUserId
    User{..} <- query' acid (UserState.GetUser userId)
    addCookie Session $ mkCookie "user-id" userId
    -- no op calculation to force bound vars to get calculed
    res <- getReturnText "0" prefs (Env variables (Map.map fst functions))
    case res of
        (Left err) -> ok $ jsonResponse [ "error" .= err ]
        (Right ans) -> ok $ jsonResponse [ "vars" .= Map.mapWithKey (varsToJSON (boundResults ans)) (vars ans)
                                         , "funcs" .= Map.mapWithKey funcsToJSON functions
                                         , "prefs" .= prefs
                                         ]
    
varsToJSON :: Map String Decimal -> String -> AST -> Value
varsToJSON _ _ (Number n) = object [ "value" .= n ]
varsToJSON bound v expr =  object [ "value" .= (bound Map.! v)
                                  , "expr" .= show expr
                                  ]

funcsToJSON :: String -> (Function, String) -> Value
funcsToJSON k (f, text) = object [ "decl" .= (k ++ showDeclaration f)
                                       , "def" .= text
                                       ]

resetUserInfo :: AcidState UserDb -> ServerPart Response
resetUserInfo acid = do
    method DELETE
    userId <- getUserId
    update' acid $ UserState.SetUser userId UserState.newUser
    noContent $ toResponse ()

modifyUserInfo :: AcidState UserDb -> ServerPart Response
modifyUserInfo acid = do
    method POST
    rq <- askRq
    if contentType "application/json-patch+json" rq then do
        userId <- getUserId
        maybeBody <- takeRequestBody rq
        let b = unBody $ fromMaybe (Body "") maybeBody
        case decode b :: Maybe [Map String String] of
            Just values ->
                if all isRemove values then do
                    user <- query' acid $ UserState.GetUser userId
                    let updatedUser = foldl runAction (Just user) values
                    case updatedUser of
                        Just u -> do
                            update' acid $ UserState.SetUser userId u
                            ok $ toResponse ()
                        Nothing ->
                            badRequest $ toResponse ("Unable to apply patch" :: String)
                else
                    badRequest $ toResponse ("Unpermitted operation" :: String)
            Nothing -> badRequest $ toResponse ("Unable to parse body" :: String)
    else resp 415 $ toResponse ("Content type must be application/json-patch+json" :: String)
    where isRemove m = case Map.lookup "op" m of
                            Just o -> o == "remove"
                            Nothing -> False

runAction :: Maybe User -> Map String String -> Maybe User
runAction (Just User{..}) action
    | Map.size action == 2 =
        case Map.lookup "path" action of
            Just p  | "/vars/" `isPrefixOf` p ->
                        let Just varName = stripPrefix "/vars/" p
                        in if Map.member varName variables then
                            Just $ User (Map.delete varName variables) functions prefs
                        else
                            Nothing
                    | "/funcs/" `isPrefixOf` p ->
                        let Just funcName = stripPrefix "/funcs/" p
                        in if Map.member funcName functions then
                            Just $ User variables (Map.delete funcName functions) prefs
                        else
                            Nothing
                    | otherwise -> Nothing
            Nothing -> Nothing
    | otherwise = Nothing
runAction Nothing _ = Nothing

getUserId :: ServerPart String
getUserId = do userId <- optional $ lookCookieValue "user-id"
               case userId of
                   Nothing -> show <$> liftIO uuid
                   Just i -> return i

getReturnText :: String -> UserPrefs -> Env -> ServerPart (Either String Result)
getReturnText input prefs env = liftIO $ catch (return $! runExcept $ mapExcept formatException $ calculate input prefs env)
                                               (\(ErrorCall e) -> return $ Left $ "Invalid input: " ++ e)
                                where formatException (Left e) = Left $ "Invalid input: " ++ show e
                                      formatException r = r

parseDefault :: FromJSON a => a -> Text -> Object -> a
parseDefault defaultValue key = fromMaybe defaultValue . parseMaybe (.: key)

jsonResponse :: [Pair] -> Response
jsonResponse = addHeader "Content-Type" "application/json" . toResponse . encode . object

contentType :: String -> Request -> Bool
contentType ct rq = ct `isPrefixOf` rqCt rq
                    where rqCt r = Char8.unpack $ fromMaybe "" $ getHeader ("Content-Type" :: String) r
