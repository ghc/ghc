
{-

this test file was extracted from Text.Pandoc.Readers.Odt.ContentReader,
which caused a panic in GHC.Tc.Class.Instance.hasFixedRuntimeRep:

error: panic! (the 'impossible' happened)

  hasFixedRuntimeRep: not of form 'TYPE rep'
  ty = c_anCXp[tau:0]
  ki = k_anCXl[tau:0]
  frrOrig = The arrow command `returnA -< anchorElem_anC3K'
            does not have a fixed runtime representation.

-}

{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module PandocArrowCmd where

import Control.Arrow
import Control.Category

maybeAddAnchorFrom :: OdtReader i p
                   -> OdtReaderSafe i i
maybeAddAnchorFrom anchorReader =
  keepingTheValue_etc_etc
  >>>
  proc (inlines, fAnchorElem) -> do
  case fAnchorElem of
    Right anchorElem -> returnA -< anchorElem
    Left _           -> returnA -< inlines

-----

keepingTheValue_etc_etc :: OdtReader i (b, Either a b)
keepingTheValue_etc_etc = undefined

data OdtState
type OdtReader      a b = ArrowState OdtState a b
type OdtReaderSafe  a b = ArrowState OdtState a (Either () b)

newtype ArrowState state a b = ArrowState
  { runArrowState :: (state, a) -> (state, b) }

instance Category    (ArrowState s) where {}
instance Arrow       (ArrowState s) where {}
instance ArrowChoice (ArrowState s) where {}
