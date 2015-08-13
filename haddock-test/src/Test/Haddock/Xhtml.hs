{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}


module Test.Haddock.Xhtml where


import Control.Monad

import Data.Generics.Aliases
import Data.Generics.Schemes

import Text.XML.Light


deriving instance Eq Content
deriving instance Eq Element
deriving instance Eq CData


readXml :: FilePath -> IO (Maybe Element)
readXml = liftM parseXMLDoc . readFile


strip :: Element -> Element
strip = stripFooter . stripLinks


stripLinks :: Element -> Element
stripLinks =
    everywhere (mkT unlink)
  where
    unlink attr@(Attr { attrKey = key })
        | qName key == "href" = attr { attrVal = "#" }
        | otherwise = attr


stripFooter :: Element -> Element
stripFooter =
    everywhere (mkT defoot)
  where
    defoot elem
        | isFooter elem = elem { elContent = [] }
        | otherwise = elem
    isFooter elem = any isFooterAttr $ elAttribs elem
    isFooterAttr (Attr { .. }) = and
        [ qName attrKey == "id"
        , attrVal == "footer"
        ]
