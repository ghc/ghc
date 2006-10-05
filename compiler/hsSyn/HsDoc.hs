module HsDoc (
  HsDoc(..),
  LHsDoc,
  docAppend,
  docParagraph,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import RdrName
import Outputable
import SrcLoc

import Data.Char (isSpace)

data HsDoc id
  = DocEmpty
  | DocAppend (HsDoc id) (HsDoc id)
  | DocString String
  | DocParagraph (HsDoc id)
  | DocIdentifier [id]
  | DocModule String
  | DocEmphasis (HsDoc id)
  | DocMonospaced (HsDoc id)
  | DocUnorderedList [HsDoc id]
  | DocOrderedList [HsDoc id]
  | DocDefList [(HsDoc id, HsDoc id)]
  | DocCodeBlock (HsDoc id)
  | DocURL String
  | DocAName String
  deriving (Eq, Show)

type LHsDoc a = Located (HsDoc a)

instance Outputable (HsDoc a) where
  ppr _ = text "<document comment>"

ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- used to make parsing easier; we group the list items later
docAppend :: HsDoc id -> HsDoc id -> HsDoc id
docAppend (DocUnorderedList ds1) (DocUnorderedList ds2)
  = DocUnorderedList (ds1++ds2)
docAppend (DocUnorderedList ds1) (DocAppend (DocUnorderedList ds2) d)
  = DocAppend (DocUnorderedList (ds1++ds2)) d
docAppend (DocOrderedList ds1) (DocOrderedList ds2)
  = DocOrderedList (ds1++ds2)
docAppend (DocOrderedList ds1) (DocAppend (DocOrderedList ds2) d)
  = DocAppend (DocOrderedList (ds1++ds2)) d
docAppend (DocDefList ds1) (DocDefList ds2)
  = DocDefList (ds1++ds2)
docAppend (DocDefList ds1) (DocAppend (DocDefList ds2) d)
  = DocAppend (DocDefList (ds1++ds2)) d
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend d1 d2
  = DocAppend d1 d2

-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph :: HsDoc id -> HsDoc id
docParagraph (DocMonospaced p)
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1)
    (DocAppend (DocMonospaced p) (DocString s2)))
  | all isSpace s1 && all isSpace s2
  = DocCodeBlock p
docParagraph (DocAppend (DocMonospaced p) (DocString s2))
  | all isSpace s2
  = DocCodeBlock p
docParagraph p
  = DocParagraph p
