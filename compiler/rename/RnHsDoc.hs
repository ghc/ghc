module RnHsDoc ( rnMbDocHdr, rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import GhcPrelude

import Outputable
import TcRnTypes
import TcRnMonad
import HsSyn
import RdrName
import Name
import RnEnv

rnMbDocHdr :: Maybe (LHsDoc RdrName) -- ^ A module header, possibly.
           -> TcGblEnv
           -> RnM TcGblEnv
rnMbDocHdr mb_doc tcg_env = do
  rn_doc <- rnMbLHsDoc mb_doc
  pure tcg_env { tcg_doc_hdr = rn_doc }

rnMbLHsDoc :: Maybe (LHsDoc RdrName) -> RnM (Maybe (LHsDoc Name))
rnMbLHsDoc = traverse rnLHsDoc

rnLHsDoc :: LHsDoc RdrName -> RnM (LHsDoc Name)
rnLHsDoc = traverse rnHsDoc

rnHsDoc :: HsDoc RdrName -> RnM (HsDoc Name)
rnHsDoc (HsDoc s ids) = do
  gre <- tcg_rdr_env <$> getGblEnv
  pure (HsDoc s (rnHsDocIdentifier gre <$> ids))

rnHsDocIdentifier :: GlobalRdrEnv
                  -> HsDocIdentifier RdrName
                  -> HsDocIdentifier Name
rnHsDocIdentifier gre (HsDocIdentifier span [rdr_name]) =
  HsDocIdentifier span names
  where
    -- Try to look up all the names in the GlobalRdrEnv that match
    -- the names.
    names = concatMap (\c -> map gre_name (lookupGRE_RdrName c gre)) choices
    -- Generate the choices for the possible kind of thing this
    -- is.
    choices = dataTcOccs rdr_name
rnHsDocIdentifier _ hsDocId =
  pprPanic "rnHsDocIdentifier" (ppr hsDocId)
