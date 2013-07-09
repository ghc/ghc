{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haddock.Doc (
  docAppend,
  docParagraph,
  combineStringNodes,
  combineDocumentation
  ) where

import Data.Maybe
import Data.Monoid
import Haddock.Types
import Data.Char (isSpace)
import Control.Arrow ((***))

-- We put it here so that we can avoid a circular import
-- anything relevant imports this module anyway
instance Monoid (Doc id) where
  mempty  = DocEmpty
  mappend = docAppend

combineDocumentation :: Documentation name -> Maybe (Doc name)
combineDocumentation (Documentation Nothing Nothing) = Nothing
combineDocumentation (Documentation mDoc mWarning)   = Just (fromMaybe mempty mWarning `mappend` fromMaybe mempty mDoc)

-- used to make parsing easier; we group the list items later
docAppend :: Doc id -> Doc id -> Doc id
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

-- | This is a hack that joins neighbouring 'DocString's into a single one.
-- This is done to ease up the testing and doesn't change the final result
-- as this would be done later anyway.
combineStringNodes :: Doc id -> Doc id
combineStringNodes (DocAppend (DocString x) (DocString y)) = DocString (x ++ y)
combineStringNodes (DocAppend (DocString x) (DocAppend (DocString y) z)) =
  tryjoin (DocAppend (DocString (x ++ y)) (combineStringNodes z))
combineStringNodes (DocAppend x y) = tryjoin (DocAppend (combineStringNodes x) (combineStringNodes y))
combineStringNodes (DocParagraph x) = DocParagraph (combineStringNodes x)
combineStringNodes (DocWarning x) = DocWarning (combineStringNodes x)
combineStringNodes (DocEmphasis x) = DocEmphasis (combineStringNodes x)
combineStringNodes (DocMonospaced x) = DocMonospaced (combineStringNodes x)
combineStringNodes (DocUnorderedList xs) = DocUnorderedList (map combineStringNodes xs)
combineStringNodes (DocOrderedList x) = DocOrderedList (map combineStringNodes x)
combineStringNodes (DocDefList xs) = DocDefList (map (combineStringNodes *** combineStringNodes) xs)
combineStringNodes (DocCodeBlock x) = DocCodeBlock (combineStringNodes x)
combineStringNodes x = x

tryjoin :: Doc id -> Doc id
tryjoin (DocAppend (DocString x) (DocString y)) = DocString (x ++ y)
tryjoin (DocAppend (DocString x) (DocAppend (DocString y) z)) = DocAppend (DocString (x ++ y)) z
tryjoin (DocAppend (DocAppend x (DocString y)) (DocString z))
  = tryjoin (DocAppend (combineStringNodes x) (DocString $ y ++ z))
tryjoin x = x
