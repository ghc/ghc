--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module HaddockRename (
	RnM, runRn, runRnFM, runRnUnqualFM, -- the monad (instance of Monad)

	renameExportList, 
	renameDecl,
	renameExportItems, renameInstHead,
	renameDoc, renameMaybeDoc,
  ) where

import HaddockTypes
import HaddockUtil	( unQual )
import HsSyn2
import Map ( Map )
import qualified Map hiding ( Map )

import Monad

-- -----------------------------------------------------------------------------
-- Monad for renaming

-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in 
-- the environment.

newtype GenRnM n a = 
  RnM { unRn :: (n -> (Bool,HsQName))	-- name lookup function
             -> (a,[n])
      }

type RnM a = GenRnM HsQName a

instance Monad (GenRnM n) where
  (>>=) = thenRn
  return = returnRn   

returnRn :: a -> GenRnM n a
returnRn a   = RnM (\_ -> (a,[]))
thenRn :: GenRnM n a -> (a -> GenRnM n b) -> GenRnM n b
m `thenRn` k = RnM (\lkp -> case unRn m lkp of 
				(a,out1) -> case unRn (k a) lkp of
						(b,out2) -> (b,out1++out2))

getLookupRn :: RnM (HsQName -> (Bool,HsQName))
getLookupRn = RnM (\lkp -> (lkp,[]))
outRn :: HsQName -> RnM ()
outRn name = RnM (\_ -> ((),[name]))

lookupRn :: (HsQName -> a) -> HsQName -> RnM a
lookupRn and_then name = do
  lkp <- getLookupRn
  case lkp name of
	(False,maps_to) -> do outRn name; return (and_then maps_to)
	(True, maps_to) -> return (and_then maps_to)

runRnFM :: Map HsQName HsQName -> RnM a -> (a,[HsQName])
runRnFM env rn = unRn rn lkp 
  where lkp n = case Map.lookup n env of
		  Nothing -> (False, n) -- leave the qualified name
		  Just q  -> (True,  q)

-- like runRnFM, but if it can't find a mapping for a name,
-- it leaves an unqualified name in place instead.
runRnUnqualFM :: Map HsQName HsQName -> RnM a -> (a,[HsQName])
runRnUnqualFM env rn = unRn rn lkp 
  where lkp n = case Map.lookup n env of
		  Nothing -> (False, unQual n) -- remove the qualifier
		  Just q  -> (True, q)

runRn :: (n -> (Bool,HsQName)) -> GenRnM n a -> (a,[n])
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
    renameExport (HsEGroup lev doc0) = do
	doc <- renameDoc doc0
	return (HsEGroup lev doc)
    renameExport (HsEDoc doc0) = do
	doc <- renameDoc doc0
	return (HsEDoc doc)
    renameExport (HsEDocNamed str) = return (HsEDocNamed str)


renameDecl :: HsDecl -> RnM HsDecl
renameDecl decl
  = case decl of
	HsTypeDecl loc t args ty0 doc0 -> do
	    ty <- renameType ty0
	    doc <- renameMaybeDoc doc0
	    return (HsTypeDecl loc t args ty doc)
	HsDataDecl loc ctx0 t args cons0 drv0 doc0 -> do
	    ctx <- renameContext ctx0
	    cons <- mapM renameConDecl cons0
	    drv <- mapM (lookupRn id) drv0
	    doc <- renameMaybeDoc doc0
	    return (HsDataDecl loc ctx t args cons drv doc)
        HsNewTypeDecl loc ctx0 t args con0 drv0 doc0 -> do
	    ctx <- renameContext ctx0
	    con <- renameConDecl con0
	    drv <- mapM (lookupRn id) drv0
	    doc <- renameMaybeDoc doc0
	    return (HsNewTypeDecl loc ctx t args con drv doc)
        HsClassDecl loc ctxt0 nm tvs fds decls0 doc0 -> do
	    ctxt <- renameContext ctxt0
	    decls <- mapM renameDecl decls0
	    doc <- renameMaybeDoc doc0
	    return (HsClassDecl loc ctxt nm tvs fds decls doc)
	HsTypeSig loc fs qt0 doc0 -> do
	    qt <- renameType qt0
	    doc <- renameMaybeDoc doc0
	    return (HsTypeSig loc fs qt doc)
	HsForeignImport loc cc safe ent n ty0 doc0 -> do
	    ty <- renameType ty0
	    doc <- renameMaybeDoc doc0
	    return (HsForeignImport loc cc safe ent n ty doc)
	HsInstDecl loc ctxt0 asst0 decls -> do
	    ctxt <- renameContext ctxt0
	    asst <- renamePred asst0
	    return (HsInstDecl loc ctxt asst decls)
	HsDocCommentNamed loc name doc0 -> do
	    doc <- renameDoc doc0
	    return (HsDocCommentNamed loc name doc)
	_ -> 
	    return decl

renameConDecl :: HsConDecl -> RnM HsConDecl
renameConDecl (HsConDecl loc nm tvs ctxt tys0 doc0) = do
  tys <- mapM renameBangTy tys0
  doc <- renameMaybeDoc doc0
  return (HsConDecl loc nm tvs ctxt tys doc)
renameConDecl (HsRecDecl loc nm tvs ctxt fields0 doc0) = do
  fields <- mapM renameField fields0
  doc <- renameMaybeDoc doc0
  return (HsRecDecl loc nm tvs ctxt fields doc)

renameField :: HsFieldDecl -> RnM HsFieldDecl
renameField (HsFieldDecl ns ty0 doc0) = do 
  ty <- renameBangTy ty0
  doc <- renameMaybeDoc doc0
  return (HsFieldDecl ns ty doc)

renameBangTy :: HsBangType -> RnM HsBangType
renameBangTy (HsBangedTy ty)   = HsBangedTy   `liftM` renameType ty
renameBangTy (HsUnBangedTy ty) = HsUnBangedTy `liftM` renameType ty

renameContext :: HsContext -> RnM HsContext
renameContext = mapM renamePred

renameIPContext :: HsIPContext -> RnM HsIPContext
renameIPContext cs = mapM renameCtxt cs
 where
   renameCtxt (HsIP n t)   = liftM (HsIP n) (renameType t)
   renameCtxt (HsAssump c) = liftM HsAssump (renamePred c)

renamePred :: (HsQName,[HsType]) -> RnM (HsQName,[HsType])
renamePred (c,tys0) = do
  tys <- mapM renameType tys0
  lookupRn (\c' -> (c',tys)) c

renameType :: HsType -> RnM HsType
renameType (HsForAllType tvs ctx0 ty0) = do
  ctx <- renameIPContext ctx0
  ty <- renameType ty0
  return (HsForAllType tvs ctx ty)
renameType (HsTyFun arg0 res0) = do
  arg <- renameType arg0
  res <- renameType res0
  return (HsTyFun arg res)
renameType (HsTyIP n ty0) = do
  ty <- renameType ty0
  return (HsTyIP n ty0)
renameType (HsTyTuple b tys0) = do
  tys <- mapM renameType tys0
  return (HsTyTuple b tys)
renameType (HsTyApp ty0 arg0) = do
  ty <- renameType ty0
  arg <- renameType arg0
  return (HsTyApp ty arg)
renameType (HsTyVar nm) =
  return (HsTyVar nm)
renameType (HsTyCon nm) =
  lookupRn HsTyCon nm
renameType (HsTyDoc ty0 doc0) = do
  ty <- renameType ty0
  doc <- renameDoc doc0
  return (HsTyDoc ty doc)

renameInstHead :: InstHead -> RnM InstHead
renameInstHead (ctx,asst) = do
  ctx <- renameContext ctx
  asst <- renamePred asst
  return (ctx,asst)

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
  markupDefList       = liftM DocDefList . mapM markupDef,
  markupCodeBlock     = liftM DocCodeBlock,
  markupURL	      = return . DocURL,
  markupAName	      = return . DocAName
  }

markupDef (ma,mb) = do a <- ma; b <- mb; return (a,b)

renameDoc :: Doc -> RnM Doc
renameDoc = markup markupRename

renameMaybeDoc :: Maybe Doc -> RnM (Maybe Doc)
renameMaybeDoc Nothing = return Nothing
renameMaybeDoc (Just doc) = Just `liftM` renameDoc doc

-- ---------------------------------------------------------------------------
-- Looking up names in documentation

lookupForDoc :: [HsQName] -> RnM Doc
lookupForDoc qns = do
  lkp <- getLookupRn
  case [ n | (True,n) <- map lkp qns ] of
	ns@(_:_) -> return (DocIdentifier ns)
	[] -> -- if we were given a qualified name, but there's nothing
	      -- matching that name in scope, then just assume its existence
	      -- (this means you can use qualified names in doc strings wihout
	      -- worrying about whether the entity is in scope).
	      let quals = filter isQualified qns in
	      if (not (null quals)) then
		return (DocIdentifier quals)
	      else do
		outRn (head qns)
		-- no qualified names: just replace this name with its
		-- string representation.
		return (DocString (show (head qns)))
 where
   isQualified (Qual _ _) = True
   isQualified _ = False
   
-- -----------------------------------------------------------------------------

renameExportItems :: [ExportItem] -> RnM [ExportItem]
renameExportItems items = mapM rn items
  where
	rn (ExportModule mod0)
	   = return (ExportModule mod0)
 	rn (ExportGroup lev id0 doc0) 
	   = do doc <- renameDoc doc0
	        return (ExportGroup lev id0 doc)
	rn (ExportDecl x decl0 insts) -- x is an original name, don't rename it
	   = do decl <- renameDecl decl0
		insts <- mapM renameInstHead insts
		return (ExportDecl x decl insts)
	rn (ExportNoDecl x y subs)
	   = do y' <- lookupRn id y
	        subs' <- mapM (lookupRn id) subs
		return (ExportNoDecl x y' subs')
	rn (ExportDoc doc0)
	   = do doc <- renameDoc doc0
		return (ExportDoc doc)
