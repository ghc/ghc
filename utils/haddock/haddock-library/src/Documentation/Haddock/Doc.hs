module Documentation.Haddock.Doc
  ( docParagraph
  , docAppend
  , docConcat
  , metaDocConcat
  , metaDocAppend
  , emptyMetaDoc
  , metaAppend
  , metaConcat
  ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Documentation.Haddock.Types

docConcat :: [DocH mod id] -> DocH mod id
docConcat = foldr docAppend DocEmpty

-- | Concat using 'metaAppend'.
metaConcat :: [Meta] -> Meta
metaConcat = foldr metaAppend emptyMeta

-- | Like 'docConcat' but also joins the 'Meta' info.
metaDocConcat :: [MetaDoc mod id] -> MetaDoc mod id
metaDocConcat = foldr metaDocAppend emptyMetaDoc

-- | We do something perhaps unexpected here and join the meta info
-- in ‘reverse’: this results in the metadata from the ‘latest’
-- paragraphs taking precedence.
metaDocAppend :: MetaDoc mod id -> MetaDoc mod id -> MetaDoc mod id
metaDocAppend
  (MetaDoc{_meta = m, _doc = d})
  (MetaDoc{_meta = m', _doc = d'}) =
    MetaDoc{_meta = m' `metaAppend` m, _doc = d `docAppend` d'}

-- | This is not a monoidal append, it uses '<|>' for the '_version' and
-- '_package'.
metaAppend :: Meta -> Meta -> Meta
metaAppend (Meta v1) (Meta v2) = Meta (v1 <|> v2)

emptyMetaDoc :: MetaDoc mod id
emptyMetaDoc = MetaDoc{_meta = emptyMeta, _doc = DocEmpty}

emptyMeta :: Meta
emptyMeta = Meta Nothing

docAppend :: DocH mod id -> DocH mod id -> DocH mod id
docAppend (DocDefList ds1) (DocDefList ds2) = DocDefList (ds1 ++ ds2)
docAppend (DocDefList ds1) (DocAppend (DocDefList ds2) d) = DocAppend (DocDefList (ds1 ++ ds2)) d
docAppend (DocOrderedList ds1) (DocOrderedList ds2) = DocOrderedList (ds1 ++ ds2)
docAppend (DocOrderedList ds1) (DocAppend (DocOrderedList ds2) d) = DocAppend (DocOrderedList (ds1 ++ ds2)) d
docAppend (DocUnorderedList ds1) (DocUnorderedList ds2) = DocUnorderedList (ds1 ++ ds2)
docAppend (DocUnorderedList ds1) (DocAppend (DocUnorderedList ds2) d) = DocAppend (DocUnorderedList (ds1 ++ ds2)) d
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend (DocString s1) (DocString s2) = DocString (s1 ++ s2)
docAppend (DocAppend d (DocString s1)) (DocString s2) = DocAppend d (DocString (s1 ++ s2))
docAppend (DocString s1) (DocAppend (DocString s2) d) = DocAppend (DocString (s1 ++ s2)) d
docAppend d1 d2 = DocAppend d1 d2

-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph :: DocH mod id -> DocH mod id
docParagraph (DocMonospaced p) =
  DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1 =
      DocCodeBlock (docCodeBlock p)
docParagraph
  ( DocAppend
      (DocString s1)
      (DocAppend (DocMonospaced p) (DocString s2))
    )
    | all isSpace s1 && all isSpace s2 =
        DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocMonospaced p) (DocString s2))
  | all isSpace s2 =
      DocCodeBlock (docCodeBlock p)
docParagraph p =
  DocParagraph p

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
docCodeBlock :: DocH mod id -> DocH mod id
docCodeBlock (DocString s) =
  DocString (reverse $ dropWhile (`elem` " \t") $ reverse s)
docCodeBlock (DocAppend l r) =
  DocAppend l (docCodeBlock r)
docCodeBlock d = d
