--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module HaddockRename (
	RnM, runRn, runRnFM, -- the monad (instance of Monad)

	--renameExportList, 
	--renameDecl,
	--renameExportItems, renameInstHead,
	--renameDoc, renameMaybeDoc,
  renameMaybeDoc, renameExportItems,
  ) where

import HaddockTypes
import HaddockUtil	( unQual )
--import HsSyn2
import Map ( Map )
import qualified Map hiding ( Map )

import Prelude hiding ( mapM )
import Control.Monad hiding ( mapM )
import Data.Traversable

import GHC

-- -----------------------------------------------------------------------------
-- Monad for renaming

-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in 
-- the environment.

newtype GenRnM n a = 
  RnM { unRn :: (n -> (Bool, DocName))	-- name lookup function
             -> (a,[n])
      }

type RnM a = GenRnM Name a

instance Monad (GenRnM n) where
  (>>=) = thenRn
  return = returnRn   

returnRn :: a -> GenRnM n a
returnRn a   = RnM (\_ -> (a,[]))
thenRn :: GenRnM n a -> (a -> GenRnM n b) -> GenRnM n b
m `thenRn` k = RnM (\lkp -> case unRn m lkp of 
				(a,out1) -> case unRn (k a) lkp of
						(b,out2) -> (b,out1++out2))

getLookupRn :: RnM (Name -> (Bool, DocName))
getLookupRn = RnM (\lkp -> (lkp,[]))
outRn :: Name -> RnM ()
outRn name = RnM (\_ -> ((),[name]))

lookupRn :: (DocName -> a) -> Name -> RnM a
lookupRn and_then name = do
  lkp <- getLookupRn
  case lkp name of
	(False,maps_to) -> do outRn name; return (and_then maps_to)
	(True, maps_to) -> return (and_then maps_to)

runRnFM :: Map Name Name -> RnM a -> (a,[Name])
runRnFM env rn = unRn rn lkp 
  where lkp n = case Map.lookup n env of
		  Nothing -> (False, NoLink n) 
		  Just q  -> (True,  Link q)

runRn :: (n -> (Bool,DocName)) -> GenRnM n a -> (a,[n])
runRn lkp rn = unRn rn lkp

renameExportItems :: [ExportItem2 Name] -> RnM [ExportItem2 DocName]
renameExportItems items = mapM renameExportItem items

renameMaybeDoc :: Maybe (HsDoc Name) -> RnM (Maybe (HsDoc DocName))
renameMaybeDoc mbDoc = mapM renameDoc mbDoc

renameDoc :: HsDoc Name -> RnM (HsDoc DocName)
renameDoc doc = case doc of

  DocEmpty -> return DocEmpty

  DocAppend a b -> do
    a' <- renameDoc a
    b' <- renameDoc b
    return (DocAppend a' b')

  DocString str -> return (DocString str)

  DocParagraph doc -> do
    doc' <- renameDoc doc
    return (DocParagraph doc')

  DocIdentifier ids -> do
    lkp <- getLookupRn
    case [ n | (True, n) <- map lkp ids ] of
      ids'@(_:_) -> return (DocIdentifier ids')
      [] -> return (DocIdentifier (map Link ids))

  DocModule str -> return (DocModule str)

  DocEmphasis doc -> do
    doc' <- renameDoc doc
    return (DocEmphasis doc')

  DocMonospaced doc -> do
    doc' <- renameDoc doc
    return (DocMonospaced doc')

  DocUnorderedList docs -> do
    docs' <- mapM renameDoc docs
    return (DocUnorderedList docs')

  DocOrderedList docs -> do
    docs' <- mapM renameDoc docs
    return (DocOrderedList docs')

-- -----------------------------------------------------------------------------
-- Renaming source code & documentation
{-

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
-}

renameInstHead = undefined


renameDecl = undefined

renameExportItem :: ExportItem2 Name -> RnM (ExportItem2 DocName)
renameExportItem item = case item of 
  ExportModule2 mod -> return (ExportModule2 mod)
  ExportGroup2 lev id doc -> do
    doc' <- renameDoc doc
    return (ExportGroup2 lev id doc')
  ExportDecl2 x decl doc instances -> do
    decl' <- renameDecl decl
    doc' <- mapM renameDoc doc
    instances' <- mapM renameInstHead instances
    return (ExportDecl2 x decl' doc' instances')
  ExportNoDecl2 x y subs -> do
    y' <- lookupRn id y
    subs' <- mapM (lookupRn id) subs
    return (ExportNoDecl2 x y' subs')
  ExportDoc2 doc -> do
    doc' <- renameDoc doc
    return (ExportDoc2 doc')
