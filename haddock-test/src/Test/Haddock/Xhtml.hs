{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}


module Test.Haddock.Xhtml
    ( Xhtml(..)
    , parseXhtml, dumpXhtml
    , stripLinks, stripFooter
    ) where


import Data.Generics.Aliases
import Data.Generics.Schemes

import Text.XML.Light


newtype Xhtml = Xhtml
    { xhtmlElement :: Element
    } deriving Eq


-- TODO: Find a way to avoid warning about orphan instances.
deriving instance Eq Element
deriving instance Eq Content
deriving instance Eq CData


parseXhtml :: String -> Maybe Xhtml
parseXhtml = fmap Xhtml . parseXMLDoc


dumpXhtml :: Xhtml -> String
dumpXhtml = ppElement . xhtmlElement


stripLinks :: Xhtml -> Xhtml
stripLinks =
    Xhtml . everywhere (mkT unlink) . xhtmlElement
  where
    unlink attr@(Attr { attrKey = key })
        | qName key == "href" = attr { attrVal = "#" }
        | otherwise = attr


stripFooter :: Xhtml -> Xhtml
stripFooter =
    Xhtml . everywhere (mkT defoot) . xhtmlElement
  where
    defoot el
        | isFooter el = el { elContent = [] }
        | otherwise = el
    isFooter el = any isFooterAttr $ elAttribs el
    isFooterAttr (Attr { .. }) = and
        [ qName attrKey == "id"
        , attrVal == "footer"
        ]
