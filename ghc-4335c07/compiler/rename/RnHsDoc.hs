
module RnHsDoc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import GhcPrelude

import TcRnTypes
import HsSyn
import SrcLoc


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
rnHsDoc (HsDocString s) = return (HsDocString s)

