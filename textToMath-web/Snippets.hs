{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Snippets(
    header
) where

import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5.Attributes (class_)
import qualified Text.Blaze.Html5 as H

header :: Html
header =
    H.div ! class_ "navbar navbar-inverse navbar-fixed-top" $
        H.div ! class_ "container" $
            H.div ! class_ "navbar-header" $
                H.div ! class_ "navbar-brand" $
                    "TextToMath"
