module RnHsDoc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc, rnMbHsDoc ) where

import TcRnMonad   ( RnM )
import RnEnv       ( dataTcOccs, lookupGreRn_maybe )
import HsDoc       ( HsDoc(..) )

import RdrName     ( RdrName, isRdrDataCon, isRdrTc, gre_name )
import Name        ( Name )
import SrcLoc      ( Located(..) )
import Outputable  ( ppr, defaultUserStyle )

import Data.List   ( (\\) )
import Debug.Trace ( trace )

rnMbHsDoc mb_doc = case mb_doc of
  Just doc -> do
    doc' <- rnHsDoc doc
    return (Just doc')
  Nothing -> return Nothing

rnMbLHsDoc mb_doc = case mb_doc of
  Just doc -> do
    doc' <- rnLHsDoc doc
    return (Just doc')
  Nothing -> return Nothing

rnLHsDoc (L pos doc) = do
  doc' <- rnHsDoc doc
  return (L pos doc')

ids2string []    = []
ids2string (x:_) = show $ ppr x defaultUserStyle

rnHsDoc :: HsDoc RdrName -> RnM (HsDoc Name)
rnHsDoc doc = case doc of 
  
  DocEmpty -> return DocEmpty

  DocAppend a b -> do
    a' <- rnHsDoc a 
    b' <- rnHsDoc b
    return (DocAppend a' b')

  DocString str -> return (DocString str)

  DocParagraph doc -> do
    doc' <- rnHsDoc doc
    return (DocParagraph doc')

  DocIdentifier ids -> do
    let choices = concatMap dataTcOccs ids
    mb_gres <- mapM lookupGreRn_maybe choices 
    case [gre_name gre | Just gre <- mb_gres] of
      [] -> return (DocString (ids2string ids))
      ids' -> return (DocIdentifier ids')

  DocModule str -> return (DocModule str)

  DocEmphasis doc -> do
    doc' <- rnHsDoc doc
    return (DocEmphasis doc')

  DocMonospaced doc -> do
    doc' <- rnHsDoc doc 
    return (DocMonospaced doc')
 
  DocUnorderedList docs -> do
    docs' <- mapM rnHsDoc docs
    return (DocUnorderedList docs')

  DocOrderedList docs -> do
    docs' <- mapM rnHsDoc docs
    return (DocOrderedList docs')

  DocDefList list -> do
    list' <- mapM (\(a,b) -> do
      a' <- rnHsDoc a
      b' <- rnHsDoc b
      return (a', b')) list
    return (DocDefList list')

  DocCodeBlock doc -> do
    doc' <- rnHsDoc doc
    return (DocCodeBlock doc')

  DocURL str -> return (DocURL str)

  DocAName str -> return (DocAName str)
