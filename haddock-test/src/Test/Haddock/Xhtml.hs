{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}


module Test.Haddock.Xhtml
    ( Xhtml(..)
    , parseXhtml, dumpXhtml
    , stripLinks, stripLinksWhen, stripAnchorsWhen, stripFooter
    ) where


import Data.Generics.Aliases
import Data.Generics.Schemes

import Text.XML.Light
import Text.XHtml


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
stripLinks = stripLinksWhen (const True)


stripLinksWhen :: (String -> Bool) -> Xhtml -> Xhtml
stripLinksWhen p =
    processAnchors unlink
  where
    unlink attr@(Attr { attrKey = key, attrVal = val })
        | qName key == "href" && p val = attr { attrVal = "#" }
        | otherwise = attr


stripAnchorsWhen :: (String -> Bool) -> Xhtml -> Xhtml
stripAnchorsWhen p =
    processAnchors unname
  where
    unname attr@(Attr { attrKey = key, attrVal = val })
        | qName key == "name" && p val = attr { attrVal = "" }
        | otherwise = attr


processAnchors :: (Attr -> Attr) -> Xhtml -> Xhtml
processAnchors f = Xhtml . everywhere (mkT f) . xhtmlElement


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


xmlElementToXhtml :: Element -> Html
xmlElementToXhtml (Element { .. }) =
    tag (qName elName) contents ! attrs
  where
    contents = mconcat $ map xmlContentToXhtml elContent
    attrs = map xmlAttrToXhtml elAttribs


xmlContentToXhtml :: Content -> Html
xmlContentToXhtml (Elem el) = xmlElementToXhtml el
xmlContentToXhtml (Text text) = toHtml $ cdData text
xmlContentToXhtml (CRef cref) = noHtml


xmlAttrToXhtml :: Attr -> HtmlAttr
xmlAttrToXhtml (Attr { .. }) = strAttr (qName attrKey) attrVal
