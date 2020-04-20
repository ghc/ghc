{-# LANGUAGE ViewPatterns #-}

module GHC.Rename.Doc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import GHC.Prelude

import GHC.Tc.Types
import GHC.Hs
import GHC.Types.SrcLoc


rnMbLHsDoc :: Maybe LHsDocString -> RnM (Maybe LHsDocString)
rnMbLHsDoc mb_doc = case mb_doc of
  Just doc -> do
    doc' <- rnLHsDoc doc
    return (Just doc')
  Nothing -> return Nothing

rnLHsDoc :: LHsDocString -> RnM LHsDocString
rnLHsDoc (L pos doc) = do
  doc' <- rnHsDoc doc
  return (L pos doc')

rnHsDoc :: HsDocString -> RnM HsDocString
rnHsDoc = pure
