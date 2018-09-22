{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Haddock.Xhtml
    ( Xml(..)
    , parseXml, dumpXml
    , stripLinks, stripLinksWhen, stripAnchorsWhen, stripFooter
    ) where


import Data.Generics.Aliases
import Data.Generics.Schemes

import Text.XML.Light
import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Xhtml


newtype Xml = Xml
    { xmlElement :: Element
    } deriving Eq


deriving instance Eq Element
deriving instance Eq Content
deriving instance Eq CData


parseXml :: String -> Maybe Xml
parseXml = fmap Xml . parseXMLDoc


dumpXml :: Xml -> String
dumpXml = Xhtml.renderHtmlFragment. xmlElementToXhtml . xmlElement


stripLinks :: Xml -> Xml
stripLinks = stripLinksWhen (const True)


stripLinksWhen :: (String -> Bool) -> Xml -> Xml
stripLinksWhen p =
    processAnchors unlink
  where
    unlink attr@(Attr { attrKey = key, attrVal = val })
        | qName key == "href" && p val = attr { attrVal = "#" }
        | otherwise = attr


stripAnchorsWhen :: (String -> Bool) -> Xml -> Xml
stripAnchorsWhen p =
    processAnchors unname
  where
    unname attr@(Attr { attrKey = key, attrVal = val })
        | qName key == "name" && p val = attr { attrVal = "" }
        | otherwise = attr


processAnchors :: (Attr -> Attr) -> Xml -> Xml
processAnchors f = Xml . everywhere (mkT f) . xmlElement


stripFooter :: Xml -> Xml
stripFooter =
    Xml . everywhere (mkT defoot) . xmlElement
  where
    defoot el
        | isFooter el = el { elContent = [] }
        | otherwise = el
    isFooter el = any isFooterAttr $ elAttribs el
    isFooterAttr (Attr { .. }) = and
        [ qName attrKey == "id"
        , attrVal == "footer"
        ]


xmlElementToXhtml :: Element -> Html
xmlElementToXhtml (Element { .. }) =
    Xhtml.tag (qName elName) contents ! attrs
  where
    contents = mconcat $ map xmlContentToXhtml elContent
    attrs = map xmlAttrToXhtml elAttribs


xmlContentToXhtml :: Content -> Html
xmlContentToXhtml (Elem el) = xmlElementToXhtml el
xmlContentToXhtml (Text text) = Xhtml.toHtml $ cdData text
xmlContentToXhtml (CRef _) = Xhtml.noHtml


xmlAttrToXhtml :: Attr -> HtmlAttr
xmlAttrToXhtml (Attr { .. }) = Xhtml.strAttr (qName attrKey) attrVal
