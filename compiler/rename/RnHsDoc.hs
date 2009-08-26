
module RnHsDoc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import TcRnTypes
import RnEnv       ( dataTcOccs, lookupGreRn_maybe )
import HsSyn

import RdrName     ( RdrName, gre_name )
import Name        ( Name )
import SrcLoc      ( Located(..) )
import Outputable  ( ppr, defaultUserStyle )


rnMbHsDoc :: Maybe HsDocString -> RnM (Maybe HsDocString)
rnMbHsDoc mb_doc = case mb_doc of
  Just doc -> do
    doc' <- rnHsDoc doc
    return (Just doc')
  Nothing -> return Nothing

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

ids2string :: [RdrName] -> String
ids2string []    = []
ids2string (x:_) = show $ ppr x defaultUserStyle

rnHsDoc :: HsDocString -> RnM HsDocString
rnHsDoc (HsDocString s) = return (HsDocString s)

