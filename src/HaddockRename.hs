--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockRename (
	RnM, runRn, runRnFM,	-- the monad (instance of Monad)

	renameExportList, 
	renameDecl,
	renameExportItems,
	renameDoc, resolveDoc,
  ) where

import HaddockTypes
import HsSyn

import FiniteMap
import Monad

-- -----------------------------------------------------------------------------
-- Monad for renaming

-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in 
-- the environment.

newtype GenRnM n a = RnM {unRn :: (n -> Maybe HsQName) -> (a,[n])}

type RnM a = GenRnM HsQName a

instance Monad (GenRnM n) where
  (>>=) = thenRn
  return = returnRn   

returnRn a   = RnM (\lkp -> (a,[]))
m `thenRn` k = RnM (\lkp -> case unRn m lkp of 
				(a,out1) -> case unRn (k a) lkp of
						(b,out2) -> (b,out1++out2))

getLookupRn = RnM (\lkp -> (lkp,[]))
outRn name = RnM (\lkp -> ((),[name]))

lookupRn :: (HsQName -> a) -> HsQName -> RnM a
lookupRn and_then name = do
  lkp <- getLookupRn
  case lkp name of
	Nothing -> do outRn name; return (and_then name)
	Just maps_to -> return (and_then maps_to)

runRnFM :: FiniteMap HsQName HsQName -> RnM a -> (a,[HsQName])
runRnFM env rn = unRn rn (lookupFM env)

runRn :: (n -> Maybe HsQName) -> GenRnM n a -> (a,[n])
runRn lkp rn = unRn rn lkp

-- -----------------------------------------------------------------------------
-- Renaming source code & documentation

renameExportList :: [HsExportSpec] -> RnM [HsExportSpec]
renameExportList spec = mapM renameExport spec
  where
    renameExport (HsEVar x) = lookupRn HsEVar x
    renameExport (HsEAbs x) = lookupRn HsEAbs x
    renameExport (HsEThingAll x) = lookupRn HsEThingAll x
    renameExport (HsEThingWith x cs)
	= do cs' <- mapM (lookupRn id) cs
	     lookupRn (\x' -> HsEThingWith x' cs') x
    renameExport (HsEModuleContents m) = return (HsEModuleContents m)
    renameExport (HsEGroup lev str) = return (HsEGroup lev str)

renameDecl :: HsDecl -> RnM HsDecl
renameDecl decl
  = case decl of
	HsTypeDecl loc t args ty -> do
	    ty <- renameType ty
	    return (HsTypeDecl loc t args ty)
	HsDataDecl loc ctx t args cons drv -> do
	    cons <- mapM renameConDecl cons
	    return (HsDataDecl loc ctx t args cons drv)
        HsNewTypeDecl loc ctx t args con drv -> do
	    con <- renameConDecl con
	    return (HsNewTypeDecl loc ctx t args con drv)
        HsClassDecl loc qt decls -> do
	    qt <- renameClassHead qt
	    decls <- mapM renameDecl decls
	    return (HsClassDecl loc qt decls)
	HsTypeSig loc fs qt -> do
	    qt <- renameType qt
	    return (HsTypeSig loc fs qt)
	HsForeignImport loc cc safe ent n ty -> do
	    ty <- renameType ty
	    return (HsForeignImport loc cc safe ent n ty)
	_ -> 
	    return decl

renameClassHead (HsForAllType tvs ctx ty) = do
  ctx <- mapM renamePred ctx
  return (HsForAllType tvs ctx ty)
renameClassHead ty = do
  return ty

renameConDecl (HsConDecl loc nm tys maybe_doc) = do
  tys <- mapM renameBangTy tys
  return (HsConDecl loc nm tys maybe_doc)
renameConDecl (HsRecDecl loc nm fields maybe_doc) = do
  fields <- mapM renameField fields
  return (HsRecDecl loc nm fields maybe_doc)

renameField (HsFieldDecl ns ty doc) = do 
  ty <- renameBangTy ty
  return (HsFieldDecl ns ty doc)

renameBangTy (HsBangedTy ty)   = HsBangedTy   `liftM` renameType ty
renameBangTy (HsUnBangedTy ty) = HsUnBangedTy `liftM` renameType ty

renamePred (c,tys) = do
  tys <- mapM renameType tys
  lookupRn (\c' -> (c',tys)) c

renameType (HsForAllType tvs ctx ty) = do
  ctx <- mapM renamePred ctx
  ty <- renameType ty
  return (HsForAllType tvs ctx ty)
renameType (HsTyFun arg res) = do
  arg <- renameType arg
  res <- renameType res
  return (HsTyFun arg res)
renameType (HsTyTuple b tys) = do
  tys <- mapM renameType tys
  return (HsTyTuple b tys)
renameType (HsTyApp ty arg) = do
  ty <- renameType ty
  arg <- renameType arg
  return (HsTyApp ty arg)
renameType (HsTyVar nm) =
  return (HsTyVar nm)
renameType (HsTyCon nm) =
  lookupRn HsTyCon nm

-- -----------------------------------------------------------------------------
-- Renaming documentation

-- Renaming documentation is done by "marking it up" from ordinary Doc
-- into (Rn Doc), which can then be renamed with runRn.
markupRename :: DocMarkup HsQName (RnM Doc)
markupRename = Markup {
  markupEmpty         = return DocEmpty,
  markupString        = return . DocString,
  markupParagraph     = liftM DocParagraph,
  markupAppend        = liftM2 DocAppend,
  markupIdentifier    = lookupRn DocIdentifier,
  markupModule        = return . DocModule,
  markupEmphasis      = liftM DocEmphasis,
  markupMonospaced    = liftM DocMonospaced,
  markupUnorderedList = liftM DocUnorderedList . sequence,
  markupOrderedList   = liftM DocOrderedList . sequence,
  markupCodeBlock     = liftM DocCodeBlock,
  markupURL	      = return . DocURL
  }

renameDoc = markup markupRename

markupResolveDoc :: DocMarkup String (GenRnM String Doc)
markupResolveDoc = Markup {
  markupEmpty         = return DocEmpty,
  markupString        = return . DocString,
  markupParagraph     = liftM DocParagraph,
  markupAppend        = liftM2 DocAppend,
  markupIdentifier    = lookupIdString,
  markupModule        = return . DocModule,
  markupEmphasis      = liftM DocEmphasis,
  markupMonospaced    = liftM DocMonospaced,
  markupUnorderedList = liftM DocUnorderedList . sequence,
  markupOrderedList   = liftM DocOrderedList . sequence,
  markupCodeBlock     = liftM DocCodeBlock,
  markupURL	      = return . DocURL
  }

resolveDoc = markup markupResolveDoc

lookupIdString :: String -> GenRnM String Doc
lookupIdString str = do
  fn <- getLookupRn
  case fn str of
	Nothing -> return (DocString str)
	Just n  -> return (DocIdentifier n)

-- -----------------------------------------------------------------------------

renameExportItems items = mapM rn items
  where
 	rn (ExportGroup lev id doc) 
	   = do doc <- renameDoc doc
	        return (ExportGroup lev id doc)
	rn (ExportDecl decl)
	   = do decl <- renameDecl decl
		return (ExportDecl decl)
