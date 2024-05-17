{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haddock.Doc
  ( module Documentation.Haddock.Doc
  , docCodeBlock
  , combineDocumentation
  ) where

import Data.Maybe
import Documentation.Haddock.Doc
import Haddock.Types
import Haddock.Utils (mkMeta)

combineDocumentation :: Documentation name -> Maybe (MDoc name)
combineDocumentation (Documentation Nothing Nothing) = Nothing
combineDocumentation (Documentation mDoc mWarning) =
  Just
    ( maybe emptyMetaDoc mkMeta mWarning
        `metaDocAppend` fromMaybe emptyMetaDoc mDoc
    )

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
