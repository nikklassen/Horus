{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import Calculator
import Calculator.Functions (Function(..))
import Calculator.DeepSeq()

import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server hiding (body, result)
import Text.Blaze.Html5 (Html, (!), p, toHtml, button, label, table, tr, th, tbody, thead)
import Text.Blaze.Html5.Attributes (href, class_, type_, action, name)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Snippets
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
  , homePage
  ]

template :: Text -> Html -> Response
template title htmlBody = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
      H.link ! A.rel "stylesheet" ! href "/css/lib/bootstrap.min.css"
      H.script ! A.src  "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js" $ return ()
    H.body $ do
      header
      H.div ! class_ "container" $
        H.div ! class_ "starter-template" $
          htmlBody
      H.footer $
        p "Created by Nik Klassen"

homePage :: ServerPart Response
homePage =
    ok $ template "TextToMath" $ do
        H.script ! A.src "/js/homepage.js" $ return ()
        H.script ! A.src "/js/lib/jquery.cookie.js" $ return ()
        H.link ! A.rel "stylesheet" ! href "/css/homepage.css"
        H.div ! A.style "float: left; width: 80%" $
            H.div ! A.style "margin: 0 auto; width: 90%" $ do
                H.form ! action "/calculate" ! A.id "math-form" ! A.method "POST" $ do
                    H.div ! class_ "form-group" $ do
                        label ! A.for "input" $ "Enter your equation below"
                        H.input ! type_ "textarea" ! name "input" ! A.id "input" ! class_ "form-control"
                    button ! type_ "submit" ! class_ "btn btn-default" $ "Calculate"
                p ! A.id "result" $ return ()
        H.div ! A.style "float: right; width: 20%" $ do
            table ! A.style "margin: 0 auto" ! class_ "table table-bordered" $ do
                thead $
                    tr $ do
                        th "Name"
                        th "Value"
                tbody ! A.id "vars" $ return ()
                tbody ! A.id "funcs" $ return ()
            button ! A.style "margin-top: 5px" ! A.id "reset-vars" ! class_ "btn btn-danger" $ "Reset"

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
