{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Snippets(
    header
) where

import Text.Blaze.Html5 (Html, (!), a, toHtml, button, li, ul)
import Text.Blaze.Html5.Attributes (class_, type_, href)
import Text.Blaze.Internal (dataAttribute)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

header :: Html
header =
    H.div ! class_ "navbar navbar-inverse navbar-fixed-top" $
      H.div ! class_ "container" $
        H.div ! class_ "navbar-header" $
          H.div ! class_ "navbar-brand" $
            "TextToMath"
