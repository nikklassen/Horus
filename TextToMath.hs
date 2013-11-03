{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, ul, li, div, textarea, h1)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, class_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Snippets

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
  [ dir "static"  $ serveDirectory DisableBrowsing [] "."
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
      H.link ! A.rel "stylesheet" ! href "/static/css/bootstrap.min.css"
      H.link ! A.rel "stylesheet" ! href "/static/css/homepage.css"
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
        h1 "Hello!"
        p "Enter your equation below"
        textarea ! A.id "math-input" $ return ()
