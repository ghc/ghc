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
	renameDoc, renameMaybeDoc,
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
    renameExport (HsEThingWith x cs) = do
	cs' <- mapM (lookupRn id) cs
	lookupRn (\x' -> HsEThingWith x' cs') x
    renameExport (HsEModuleContents m) = return (HsEModuleContents m)
    renameExport (HsEGroup lev doc) = do
	doc <- renameDoc doc
	return (HsEGroup lev doc)
    renameExport (HsEDoc doc) = do
	doc <- renameDoc doc
	return (HsEDoc doc)
    renameExport (HsEDocNamed str) = return (HsEDocNamed str)


renameDecl :: HsDecl -> RnM HsDecl
renameDecl decl
  = case decl of
	HsTypeDecl loc t args ty doc -> do
	    ty <- renameType ty
	    doc <- renameMaybeDoc doc
	    return (HsTypeDecl loc t args ty doc)
	HsDataDecl loc ctx t args cons drv doc -> do
	    cons <- mapM renameConDecl cons
	    doc <- renameMaybeDoc doc
	    return (HsDataDecl loc ctx t args cons drv doc)
        HsNewTypeDecl loc ctx t args con drv doc -> do
	    con <- renameConDecl con
	    doc <- renameMaybeDoc doc
	    return (HsNewTypeDecl loc ctx t args con drv doc)
        HsClassDecl loc ctxt nm tvs fds decls doc -> do
	    ctxt <- mapM renamePred ctxt
	    decls <- mapM renameDecl decls
	    doc <- renameMaybeDoc doc
	    return (HsClassDecl loc ctxt nm tvs fds decls doc)
	HsTypeSig loc fs qt doc -> do
	    qt <- renameType qt
	    doc <- renameMaybeDoc doc
	    return (HsTypeSig loc fs qt doc)
	HsForeignImport loc cc safe ent n ty doc -> do
	    ty <- renameType ty
	    doc <- renameMaybeDoc doc
	    return (HsForeignImport loc cc safe ent n ty doc)
	HsInstDecl loc ctxt asst decls -> do
	    ctxt <- mapM renamePred ctxt
	    asst <- renamePred asst
	    return (HsInstDecl loc ctxt asst decls)
	HsDocCommentNamed loc name doc -> do
	    doc <- renameDoc doc
	    return (HsDocCommentNamed loc name doc)
	_ -> 
	    return decl

renameConDecl (HsConDecl loc nm tvs ctxt tys doc) = do
  tys <- mapM renameBangTy tys
  doc <- renameMaybeDoc doc
  return (HsConDecl loc nm tvs ctxt tys doc)
renameConDecl (HsRecDecl loc nm tvs ctxt fields doc) = do
  fields <- mapM renameField fields
  doc <- renameMaybeDoc doc
  return (HsRecDecl loc nm tvs ctxt fields doc)

renameField (HsFieldDecl ns ty doc) = do 
  ty <- renameBangTy ty
  doc <- renameMaybeDoc doc
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
renameType (HsTyDoc ty doc) = do
  ty <- renameType ty
  doc <- renameDoc doc
  return (HsTyDoc ty doc)

-- -----------------------------------------------------------------------------
-- Renaming documentation

-- Renaming documentation is done by "marking it up" from ordinary Doc
-- into (Rn Doc), which can then be renamed with runRn.
markupRename :: DocMarkup [HsQName] (RnM Doc)
markupRename = Markup {
  markupEmpty         = return DocEmpty,
  markupString        = return . DocString,
  markupParagraph     = liftM DocParagraph,
  markupAppend        = liftM2 DocAppend,
  markupIdentifier    = lookupForDoc,
  markupModule        = return . DocModule,
  markupEmphasis      = liftM DocEmphasis,
  markupMonospaced    = liftM DocMonospaced,
  markupUnorderedList = liftM DocUnorderedList . sequence,
  markupOrderedList   = liftM DocOrderedList . sequence,
  markupCodeBlock     = liftM DocCodeBlock,
  markupURL	      = return . DocURL
  }

renameDoc = markup markupRename

renameMaybeDoc Nothing = return Nothing
renameMaybeDoc (Just doc) = Just `liftM` renameDoc doc

-- ---------------------------------------------------------------------------
-- Looking up names in documentation

lookupForDoc :: [HsQName] -> RnM Doc
lookupForDoc qns = do
  lkp <- getLookupRn
  case [ n | Just n <- map lkp qns ] of
	ns@(_:_) -> return (DocIdentifier ns)
	[] -> -- if we were given a qualified name, but there's nothing
	      -- matching that name in scope, then just assume its existence
	      -- (this means you can use qualified names in doc strings wihout
	      -- worrying about whether the entity is in scope).
	      let quals = filter isQualified qns in
	      if (not (null quals)) then
		return (DocIdentifier quals)
	      else
		-- no qualified names: just replace this name with its
		-- string representation.
		return (DocString (show (head qns)))
 where
   isQualified (Qual m i) = True
   isQualified _ = False
   
-- -----------------------------------------------------------------------------

renameExportItems items = mapM rn items
  where
	rn (ExportModule mod)
	   = return (ExportModule mod)
 	rn (ExportGroup lev id doc) 
	   = do doc <- renameDoc doc
	        return (ExportGroup lev id doc)
	rn (ExportDecl x decl) -- x is an original name, don't rename it
	   = do decl <- renameDecl decl
		return (ExportDecl x decl)
	rn (ExportDoc doc)
	   = do doc <- renameDoc doc
		return (ExportDoc doc)
