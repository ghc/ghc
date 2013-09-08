{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haddock.Doc (
  docAppend
, docParagraph
, combineDocumentation
) where

import Data.Maybe
import Data.Monoid
import Haddock.Types
import Data.Char (isSpace)

-- We put it here so that we can avoid a circular import
-- anything relevant imports this module anyway
instance Monoid (Doc id) where
  mempty  = DocEmpty
  mappend = docAppend

combineDocumentation :: Documentation name -> Maybe (Doc name)
combineDocumentation (Documentation Nothing Nothing) = Nothing
combineDocumentation (Documentation mDoc mWarning)   = Just (fromMaybe mempty mWarning `mappend` fromMaybe mempty mDoc)

docAppend :: Doc id -> Doc id -> Doc id
docAppend (DocDefList ds1) (DocDefList ds2) = DocDefList (ds1++ds2)
docAppend (DocDefList ds1) (DocAppend (DocDefList ds2) d) = DocAppend (DocDefList (ds1++ds2)) d
docAppend (DocOrderedList ds1) (DocOrderedList ds2) = DocOrderedList (ds1 ++ ds2)
docAppend (DocUnorderedList ds1) (DocUnorderedList ds2) = DocUnorderedList (ds1 ++ ds2)
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend (DocString s1) (DocString s2) = DocString (s1 ++ s2)
docAppend (DocAppend d (DocString s1)) (DocString s2) = DocAppend d (DocString (s1 ++ s2))
docAppend (DocString s1) (DocAppend (DocString s2) d) = DocAppend (DocString (s1 ++ s2)) d
docAppend d1 d2 = DocAppend d1 d2

-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph :: Doc id -> Doc id
docParagraph (DocMonospaced p)
  = DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1
  = DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocString s1)
    (DocAppend (DocMonospaced p) (DocString s2)))
  | all isSpace s1 && all isSpace s2
  = DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocMonospaced p) (DocString s2))
  | all isSpace s2
  = DocCodeBlock (docCodeBlock p)
docParagraph p
  = DocParagraph p


-- Drop trailing whitespace from @..@ code blocks.  Otherwise this:
--
--    -- @
--    -- foo
--    -- @
--
-- turns into (DocCodeBlock "\nfoo\n ") which when rendered in HTML
-- gives an extra vertical space after the code block.  The single space
-- on the final line seems to trigger the extra vertical space.
--
docCodeBlock :: Doc id -> Doc id
docCodeBlock (DocString s)
  = DocString (reverse $ dropWhile (`elem` " \t") $ reverse s)
docCodeBlock (DocAppend l r)
  = DocAppend l (docCodeBlock r)
docCodeBlock d = d
