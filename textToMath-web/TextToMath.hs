{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Main where
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.JSON.Generic hiding (Result)
import Text.Blaze.Html5 (Html, (!), p, toHtml, h1, button, label, table, tr, th, tbody, thead)
import Text.Blaze.Html5.Attributes (href, class_, type_, action, name)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Snippets
import Control.Exception (evaluate, ErrorCall)
import Control.Applicative (optional)
import qualified Control.Exception.Lifted as CEL
import Debug.Trace (trace)
import Control.Monad.IO.Class (liftIO)
import Data.Map (fromList, Map, map, toList)
import Data.Maybe (fromMaybe)
import Data.Number.CReal
import Calculator

config = Just ServerConfig { port      = 80
                           , ramQuota  = 1000000
                           , diskQuota = 20000000
                           , tmpDir    = "/tmp/"
                           }

main :: IO ()
main = serve config myApp

myApp :: ServerPart Response
myApp = msum
  [ dir "resources" $ serveDirectory DisableBrowsing [] "resources"
  , dir "calculate" calcPage
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
      H.link ! A.rel "stylesheet" ! href "/resources/css/bootstrap.min.css"
      H.script ! A.src  "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js" $ return ()
    H.body $ do
      header
      H.div ! class_ "container" $
        H.div ! class_ "starter-template" $
          body
      H.footer $
        p "Created by Nik Klassen"

homePage :: ServerPart Response
homePage =
    ok $ template "TextToMath" $ do
        H.script ! A.src "resources/js/homepage.js" $ return ()
        H.script ! A.src "resources/lib/jquery.cookie.js" $ return ()
        H.link ! A.rel "stylesheet" ! href "/resources/css/homepage.css"
        h1 "Hello!"
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
                        th $ "Name"
                        th $ "Value"
                tbody ! A.id "vars" $ return ()
            button ! A.style "margin-top: 5px" ! A.id "reset-vars" ! class_ "btn btn-danger" $ "Reset"

calcPage :: ServerPart Response
calcPage =
    do
        method POST
        input <- lookText "input"
        cookieVars <- optional $ lookCookieValue "vars"
        let variables = parseCookie cookieVars
        result <- liftIO $ getReturnText (unpack input) variables
        case result of
            Left errMsg -> do setResponseCode 422
                              internalServerError $ toResponse errMsg
            Right ans -> do addCookies [(Session, mkCookie "vars" $ serializeVars $ vars ans )]
                            ok $ toResponse $
                                case answer ans of
                                    Nothing -> "0"
                                    Just a -> show a

getReturnText :: String -> Map String CReal -> IO (Either String Result)
getReturnText input variables = CEL.catch result
                                          (\e -> trace ("Caught error " ++ show (e :: ErrorCall)) $ return $ Left ("Invalid input" :: String))
                                where result = Control.Exception.evaluate $ Right $ calculate input variables

parseCookie :: Maybe String -> Map String CReal
parseCookie cookieVars = fromList $ Prelude.map readValues (decodeJSON (fromMaybe "[]" cookieVars) :: [(String, String)])
                         where readValues = \(k,v) -> (k, read v :: CReal)

serializeVars :: Map String CReal -> String
serializeVars variables = encodeJSON $ toList $ Data.Map.map show variables
