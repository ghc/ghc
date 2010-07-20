-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Themes
-- Copyright   :  (c) Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Themes (
    CssTheme(..),
    
    cssFiles, styleSheet, stylePickers, styleMenu
    )
    where

import Haddock.Backends.Xhtml.Utils (onclick)
import Haddock.Utils (iconFile)

import Data.List (nub)

import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as XHtml


-- Standard set of style sheets, first is the preferred

data CssTheme = CssTheme {
  themeName :: String,
  themeHref :: String,
  themeFiles :: [FilePath]
  }


themes :: [CssTheme]
themes = [
    CssTheme "Classic" "xhaddock.css" ["xhaddock.css", iconFile],
    CssTheme "Tibbe"   "thaddock.css" ["thaddock.css", iconFile],
    CssTheme "Snappy"  "shaddock.css" ["shaddock.css", iconFile]
    ]

cssFiles :: [String]
cssFiles = nub (concatMap themeFiles themes)

styleSheet :: Html
styleSheet = toHtml $ zipWith mkLink themes rels
  where
    rels = ("stylesheet" : repeat "alternate stylesheet")
    mkLink (CssTheme aTitle aRef _) aRel =
       (thelink ! [href aRef, rel aRel, thetype "text/css", XHtml.title aTitle]) noHtml

stylePickers :: [Html]
stylePickers = map mkPicker themes
  where
    mkPicker (CssTheme aTitle aRef _) = 
      let js = "setActiveStyleSheet('" ++ aRef ++ "'); return false;" in
      anchor ! [href "#", onclick js] << aTitle

styleMenu :: Html
styleMenu = thediv ! [identifier "style-menu-holder"] << [
    anchor ! [ href "#", onclick js ] << "Style\9662",
    unordList stylePickers ! [ identifier "style-menu", theclass "hide" ]
  ]
  where
    js = "styleMenu(); return false;"
