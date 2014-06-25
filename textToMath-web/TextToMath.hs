{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import Calculator
import Calculator.Functions (Function(..))
import Calculator.DeepSeq()

import qualified Data.Text.Lazy as T (unpack)
import Data.ByteString.Char8 (unpack)
import Happstack.Server hiding (body, result)
import Control.Exception (bracket)
import Control.Applicative (optional, (<$>))
import qualified Control.Exception.Lifted as CEL
import Debug.Trace (trace)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf, stripPrefix)
import Data.Number.CReal
import UserState
import Serializer
import Data.Acid (AcidState)
import Data.Acid.Local
import Data.Acid.Advanced (query', update')
import Control.Monad (msum)
import System.UUID.V4 (uuid)
import Control.DeepSeq (($!!))
import Text.JSON.String
import Text.JSON.Types
import qualified Data.Aeson as Aeson

config :: Conf
config = Conf { port        = 3000
              , validator   = Nothing
              , logAccess   = Just logMAccess
              , timeout     = 30
              , threadGroup = Nothing
              }

main :: IO ()
main = bracket (openLocalState UserState.emptyState)
               createCheckpointAndClose
               (\acid -> simpleHTTP config $ do
                   decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
                   myApp acid)

myApp :: AcidState UserDb -> ServerPart Response
myApp acid = msum
  [ dir "calculate" (calc acid)
  , dir "userInfo" (getUserInfo acid)	      -- GET
  , dir "userInfo" (resetUserInfo acid)	    -- DELETE
  , dir "userInfo" (modifyUserInfo acid)	  -- POST (patch)
  ]

calc :: AcidState UserDb -> ServerPart Response
calc acid =
    do
        method POST
        input <- lookText "input"
        userId <- getUserId
        User variables functions <- query' acid (UserState.GetUser userId)
        let crealVars = Map.map read variables
        result <- liftIO $ getReturnText (T.unpack input) crealVars functions
        addCookie Session $ mkCookie "user-id" userId
        case result of
            Left _ -> do
                let res = toResponse $ makeResponse (JSObject $ toJSObject []) (JSObject $ toJSObject []) result ""
                badRequest $ jsonResponse res
            Right ans -> do let newVars = vars ans
                            let newFuncs = funcs ans
                            update' acid (UserState.SetUser userId $ User (Map.map show newVars) newFuncs)
                            let serializedVars = serializeVars $ Map.differenceWith takeFirst newVars crealVars
                            let serializedFuncs = serializeFuncs $ Map.differenceWith takeFirst newFuncs functions
                            let res = toResponse $ makeResponse serializedVars serializedFuncs result ""
                            ok $ jsonResponse res
        where takeFirst a b = if a /= b then Just a else Nothing
              makeResponse vs fs res = showJSTopType $ JSObject $ toJSObject $
                [ ("newVars", vs)
                , ("newFuncs", fs)
                ] ++ serializeResult res

getUserInfo :: AcidState UserDb -> ServerPart Response
getUserInfo acid = do
    method GET
    userId <- getUserId
    User variables functions <- query' acid (UserState.GetUser userId)
    addCookie Session $ mkCookie "user-id" userId
    let res = toResponse $ serialize (Map.map read variables) functions ""
    ok $ jsonResponse res
    where serialize vs fs = showJSTopType $ JSObject $ toJSObject
                            [ ("newVars", serializeVars vs)
                            , ("newFuncs", serializeFuncs fs)
                            ]

resetUserInfo :: AcidState UserDb -> ServerPart Response
resetUserInfo acid = do
    method DELETE
    userId <- getUserId
    update' acid $ UserState.SetUser userId $ User Map.empty Map.empty
    noContent $ toResponse ()

modifyUserInfo :: AcidState UserDb -> ServerPart Response
modifyUserInfo acid = do
    method POST
    rq <- askRq
    userId <- getUserId
    let contentType = Data.ByteString.Char8.unpack $ fromMaybe "" $ getHeader ("Content-Type" :: String) rq
    maybeBody <- takeRequestBody rq
    let b = unBody $ fromMaybe (Body "") maybeBody
    if contentType == "application/json-patch+json" then
        case Aeson.decode b of
            Just values ->
                if all isRemove values then do
                    user <- query' acid $ UserState.GetUser userId
                    let newUser = foldl runAction (Just user) values
                    case newUser of
                        Just u -> do
                            update' acid $ UserState.SetUser userId u
                            ok $ toResponse ()
                        Nothing ->
                            badRequest $ toResponse ("Unable to apply patch" :: String)
                else
                    badRequest $ toResponse ("Unpermitted operation" :: String)
            Nothing -> badRequest $ toResponse ("Unable to parse body" :: String)
    else
        resp 415 $ toResponse ("Content type must be application/json-patch+json" :: String)
    where isRemove m = case Map.lookup "op" m of
                            Just o -> o == "remove"
                            Nothing -> False

runAction :: Maybe User -> Map String String -> Maybe User
runAction (Just (User vs fs)) action
    | Map.size action == 2 =
        case Map.lookup "path" action of
            Just p  | "/var/" `isPrefixOf` p ->
                        let Just varName = stripPrefix "/var/" p
                        in if Map.member varName vs then
                            Just $ User (Map.delete varName vs) fs
                        else
                            Nothing
                    | "/func/" `isPrefixOf` p ->
                        let Just funcName = stripPrefix "/func/" p
                        in if Map.member funcName fs then
                            Just $ User vs $ Map.delete funcName fs
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

getReturnText :: String -> Map String CReal -> Map String Function -> IO (Either String Result)
getReturnText input variables functions = CEL.catch (CEL.evaluate $!! result)
                                                    (\e -> trace ("Caught error " ++ show (e :: CEL.ErrorCall)) $ return $ Left ("Invalid input" :: String))
                                                    where result = Right $ calculate input variables functions

jsonResponse :: Response -> Response
jsonResponse = addHeader "Content-Type" "application/json"
