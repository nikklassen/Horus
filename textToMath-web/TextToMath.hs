{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where

import Calculator
import Calculator.Functions (Function(..))
import Calculator.DeepSeq()

import Data.Text (Text)
import Data.Text.Lazy (unpack, pack, splitOn)
import Happstack.Server hiding (body, result)
import Text.JSON.Generic hiding (Result)
import Text.Blaze.Html5 (Html, (!), p, toHtml, button, label, table, tr, th, tbody, thead)
import Text.Blaze.Html5.Attributes (href, class_, type_, action, name)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Snippets
import Control.Exception (bracket)
import Control.Applicative (optional)
import qualified Control.Exception.Lifted as CEL
import Debug.Trace (trace)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map (map, toList, assocs)
import Data.Maybe (fromMaybe)
import Data.Number.CReal
import UserState
import Data.Acid (AcidState)
import Data.Acid.Local
import Data.Acid.Advanced (query', update')
import Control.Monad (msum)
import System.UUID.V4 (uuid)
import Control.DeepSeq (($!!))

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
  [ dir "calculate" (calcPage acid)
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
            button ! A.style "margin-top: 5px" ! A.id "reset-vars" ! class_ "btn btn-danger" $ "Reset"

calcPage :: AcidState UserDb -> ServerPart Response
calcPage acid =
    do
        method POST
        input <- lookText "input"
        userId <- optional $ lookCookieValue "user-id"
        User variables functions <- query' acid (UserState.GetUser $ fromMaybe "" userId)
        result <- liftIO $ getReturnText (unpack input) (Map.map read variables) functions
        case result of
            Left errMsg -> do
                setResponseCode 422
                internalServerError $ toResponse errMsg
            Right ans -> do
                uuidValue <- liftIO uuid
                let newVars = vars ans
                let newFuncs = funcs ans
                update' acid (UserState.SetUser (show uuidValue) $ User (Map.map show newVars) newFuncs)
                addCookies [(Session, mkCookie "vars" $ serializeVars newVars)]
                addCookies [(Session, mkCookie "funcs" $ serializeFuncs newFuncs)]
                addCookies [(Session, mkCookie "user-id" $ show uuidValue)]
                ok $ toResponse $ show $ answer ans

getReturnText :: String -> Map String CReal -> Map String Function -> IO (Either String Result)
getReturnText input variables functions = CEL.catch (CEL.evaluate $!! result)
                                                    (\e -> trace ("Caught error " ++ show (e :: CEL.ErrorCall)) $ return $ Left ("Invalid input" :: String))
                                                    where result = Right $ calculate input variables functions

serializeVars :: Map String CReal -> String
serializeVars variables = encodeJSON $ Map.toList $ Map.map show variables

serializeFuncs :: Map String Function -> String
serializeFuncs functions = encodeJSON $ map split $ Map.assocs functions
                           where split :: (String, Function) -> (String, String)
                                 split (k, v) = let [lhs, rhs] = splitOn " = " $ pack $ k ++ show v
                                                in (unpack lhs, unpack rhs)
