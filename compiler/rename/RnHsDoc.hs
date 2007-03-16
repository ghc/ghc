module RnHsDoc ( rnHaddock, rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import TcRnTypes
import TcRnMonad   ( RnM )
import RnEnv       ( dataTcOccs, lookupGreRn_maybe )
import HsSyn

import RdrName     ( RdrName, gre_name )
import Name        ( Name )
import SrcLoc      ( Located(..) )
import Outputable  ( ppr, defaultUserStyle )


rnHaddock :: HaddockModInfo RdrName -> Maybe (HsDoc RdrName)
	  -> TcGblEnv -> RnM TcGblEnv
rnHaddock module_info maybe_doc tcg_env
  = do	{ rn_module_doc <- rnMbHsDoc maybe_doc ;

		-- Rename the Haddock module info 
	; rn_description <- rnMbHsDoc (hmi_description module_info)
	; let { rn_module_info = module_info { hmi_description = rn_description } }

	; return (tcg_env { tcg_doc = rn_module_doc, 
			    tcg_hmi = rn_module_info }) }

rnMbHsDoc :: Maybe (HsDoc RdrName) -> RnM (Maybe (HsDoc Name))
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
