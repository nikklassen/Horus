{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import Calculator
import Calculator.Functions (Function(..))
import Calculator.DeepSeq()

import Data.Text.Lazy (unpack)
import Happstack.Server hiding (body, result)
import Control.Exception (bracket)
import Control.Applicative (optional, (<$>))
import qualified Control.Exception.Lifted as CEL
import Debug.Trace (trace)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map (map, differenceWith)
import Data.Number.CReal
import UserState
import Serializer
import Data.Acid (AcidState)
import Data.Acid.Local
import Data.Acid.Advanced (query', update')
import Control.Monad (msum)
import System.UUID.V4 (uuid)
import Control.DeepSeq (($!!))
import Text.JSON.String (showJSTopType)
import Text.JSON.Types

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
  , dir "userInfo" (getUserInfo acid)
  ]

calc :: AcidState UserDb -> ServerPart Response
calc acid =
    do
        method POST
        input <- lookText "input"
        userId <- getUserId
        User variables functions <- query' acid (UserState.GetUser userId)
        let crealVars = Map.map read variables
        result <- liftIO $ getReturnText (unpack input) crealVars functions
        addCookie Session $ mkCookie "user-id" userId
        case result of
            Left _ -> do setResponseCode 422
                         ok $ toResponse $ makeResponse (JSObject $ toJSObject []) (JSObject $ toJSObject []) result ""
            Right ans -> do let newVars = vars ans
                            let newFuncs = funcs ans
                            update' acid (UserState.SetUser userId $ User (Map.map show newVars) newFuncs)
                            let serializedVars = serializeVars $ Map.differenceWith takeFirst newVars crealVars
                            let serializedFuncs = serializeFuncs $ Map.differenceWith takeFirst newFuncs functions
                            ok $ toResponse $ makeResponse serializedVars serializedFuncs result ""
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
    ok $ toResponse $ serialize (Map.map read variables) functions ""
    where serialize vs fs = showJSTopType $ JSObject $ toJSObject
                            [ ("newVars", serializeVars vs)
                            , ("newFuncs", serializeFuncs fs)
                            ]

getUserId :: ServerPart String
getUserId = do userId <- optional $ lookCookieValue "user-id"
               case userId of
                   Nothing -> show <$> liftIO uuid
                   Just i -> return i

getReturnText :: String -> Map String CReal -> Map String Function -> IO (Either String Result)
getReturnText input variables functions = CEL.catch (CEL.evaluate $!! result)
                                                    (\e -> trace ("Caught error " ++ show (e :: CEL.ErrorCall)) $ return $ Left ("Invalid input" :: String))
                                                    where result = Right $ calculate input variables functions
