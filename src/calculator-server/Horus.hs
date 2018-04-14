{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, RecordWildCards #-}

module Horus (
    app
) where

import Calculator
import Calculator.Data.AST
import Calculator.Data.Env
import Calculator.Error
import Control.Applicative (optional)
import Control.Exception (catch, ErrorCall(..))
import Control.Monad.Except (mapExcept, runExcept, liftIO, msum)
import Data.Aeson hiding (Result, Number)
import Data.Aeson.Types (parseMaybe, emptyArray)
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Happstack.Server hiding (body, result)
import Serializer
import System.UUID.V4 (uuid)
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
                    let userPrefs = parseDefault prefs "prefs" jsonContent
                    let saveFunc = update' acid . UserState.SetUser userId
                    safeRes <- getReturnText $ calculate input userPrefs $ Env (Map.map fst variables) (Map.map fst functions)
                    case safeRes of
                        (Left err) -> badRequest $ jsonResponse [ "error" .= err ]
                        (Right res) -> makeCalculationResponse saveFunc (user { prefs = userPrefs }) input res
                Nothing -> badRequest $ toResponse ("Unable to parse body" :: String)
        else resp 415 $ toResponse ("Content type must be application/json" :: String)

makeCalculationResponse :: (User -> ServerPart a) -> User -> String -> Result -> ServerPart Response
makeCalculationResponse saveFunc user input VarResult{..} = do
    let displayText = case value of
                          (Number _) -> ""
                          _ -> getDefinition input
    let user' =  user {
        variables = Map.alter (\_ -> Just (value, displayText)) name (variables user)
    }
    _ <- saveFunc user'
    ok $ jsonResponse [ "vars" .= Map.mapWithKey (varsToJSON boundResults) (variables user')
                      , "funcs" .= emptyArray
                      , "result" .= answer
                      ]

makeCalculationResponse saveFunc user input FuncResult{..} = do
    let decl = getDefinition input
    let user' = user {
        functions = Map.alter (\_ -> Just (def, decl)) name (functions user)
    }
    _ <- saveFunc user'
    ok $ jsonResponse [ "vars" .= object []
                      , "funcs" .= object [ pack name .= funcsToJSON name (def, decl) ]
                      , "result" .= (0 :: Integer)
                      ]

makeCalculationResponse saveFunc user _ CalcResult{..} = do
        _ <- saveFunc user
        ok $ jsonResponse [ "vars" .= emptyArray
                          , "funcs" .= emptyArray
                          , "result" .= answer ]

-- Extract the user-entered body of the function
getDefinition :: String -> String
getDefinition = dropWhile isSpace . drop 1 . dropWhile (/= '=')

getUserInfo :: AcidState UserDb -> ServerPart Response
getUserInfo acid = do
    method GET
    userId <- getUserId
    User{..} <- query' acid (UserState.GetUser userId)
    addCookie Session $ mkCookie "user-id" userId
    res <- getReturnText $ calculateBound prefs $ Env (Map.map fst variables) (Map.map fst functions)
    case res of
        (Left err) -> ok $ jsonResponse [ "error" .= err ]
        (Right boundResults) -> ok $ jsonResponse [ "vars" .= Map.mapWithKey (varsToJSON boundResults) variables
                                                  , "funcs" .= Map.mapWithKey funcsToJSON functions
                                                  , "prefs" .= prefs
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

getReturnText :: Safe a -> ServerPart (Either String a)
getReturnText safeRes = liftIO $ catch (return $! runExcept $ mapExcept formatException safeRes)
                                       (\(ErrorCall e) -> return $ Left $ "Invalid input: " ++ e)
                        where formatException (Left e) = Left $ "Invalid input: " ++ show e
                              formatException r = r

parseDefault :: FromJSON a => a -> Text -> Object -> a
parseDefault defaultValue key = fromMaybe defaultValue . parseMaybe (.: key)

contentType :: String -> Request -> Bool
contentType ct rq = ct `isPrefixOf` rqCt rq
                    where rqCt r = Char8.unpack $ fromMaybe "" $ getHeader ("Content-Type" :: String) r
