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
import BasicTypes
import SrcLoc 
import Bag

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

-- -----------------------------------------------------------------------------
-- Renaming 

renameExportItems :: [ExportItem2 Name] -> RnM [ExportItem2 DocName]
renameExportItems items = mapM renameExportItem items

renameMaybeDoc :: Maybe (HsDoc Name) -> RnM (Maybe (HsDoc DocName))
renameMaybeDoc mbDoc = mapM renameDoc mbDoc

renameLDoc (L loc doc) = return . L loc =<< renameDoc doc

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
  DocDefList docs -> do
    docs' <- mapM (\(a,b) -> do
      a' <- renameDoc a
      b' <- renameDoc b
      return (a',b')) docs
    return (DocDefList docs')  
  DocCodeBlock doc -> do
    doc' <- renameDoc doc
    return (DocCodeBlock doc')
  DocURL str -> return (DocURL str) 
  DocAName str -> return (DocAName str)

rename = lookupRn id 
renameL (L loc name) = return . L loc =<< rename name

renameLPred (L loc p) = return . L loc =<< renamePred p

renamePred :: HsPred Name -> RnM (HsPred DocName)
renamePred (HsClassP name types) = do
  name' <- rename name 
  types' <- mapM renameLType types
  return (HsClassP name' types')
renamePred (HsIParam (Dupable name) t) = do
  name' <- rename name
  t' <- renameLType t
  return (HsIParam (Dupable name') t')
renamePred (HsIParam (Linear name) t) = do
  name' <- rename name
  t' <- renameLType t
  return (HsIParam (Linear name') t')

renameLType (L loc t) = return . L loc =<< renameType t

renameType t = case t of 
  HsForAllTy expl tyvars lcontext ltype -> do
    tyvars' <- mapM renameLTyVarBndr tyvars
    lcontext' <- renameLContext lcontext 
    ltype' <- renameLType ltype
    return (HsForAllTy expl tyvars' lcontext' ltype')

  HsTyVar n -> return . HsTyVar =<< rename n
  HsBangTy b ltype -> return . HsBangTy b =<< renameLType ltype
  
  HsAppTy a b -> do
    a' <- renameLType a
    b' <- renameLType b
    return (HsAppTy a' b')

  HsFunTy a b -> do     
    a' <- renameLType a
    b' <- renameLType b
    return (HsFunTy a' b')

  HsListTy t -> return . HsListTy =<< renameLType t
  HsPArrTy t -> return . HsPArrTy =<< renameLType t

  HsTupleTy b ts -> return . HsTupleTy b =<< mapM renameLType ts

  HsOpTy a (L loc op) b -> do
    op' <- rename op
    a' <- renameLType a
    b' <- renameLType b
    return (HsOpTy a' (L loc op') b')

  HsParTy t -> return . HsParTy =<< renameLType t

  HsNumTy n -> return (HsNumTy n)

  HsPredTy p -> return . HsPredTy =<< renamePred p

  HsKindSig t k -> do
    t' <- renameLType t
    return (HsKindSig t' k)

  HsDocTy t doc -> do
    t' <- renameLType t
    doc' <- renameLDoc doc
    return (HsDocTy t' doc')

  _ -> error "renameType"
 
renameLTyVarBndr (L loc tv) = do
  name' <- rename (hsTyVarName tv)
  return $ L loc (replaceTyVarName tv name')
    
renameLContext (L loc context) = do
  context' <- mapM renameLPred context
  return (L loc context')

renameInstHead :: InstHead2 Name -> RnM (InstHead2 DocName)
renameInstHead (preds, className, types) = do
  preds' <- mapM renamePred preds
  className' <- rename className
  types' <- mapM renameType types
  return (preds', className', types')

renameLDecl (L loc d) = return . L loc =<< renameDecl d

renameDecl d = case d of
  TyClD d doc -> do
    d' <- renameTyClD d
    doc' <- renameMaybeDoc doc
    return (TyClD d' doc')
  SigD s doc -> do
    s' <- renameSig s
    doc' <- renameMaybeDoc doc
    return (SigD s' doc')
  ForD d doc -> do
    d' <- renameForD d
    doc' <- renameMaybeDoc doc
    return (ForD d' doc')
  _ -> error "renameDecl"

renameTyClD d = case d of
  ForeignType name a b -> do
    name' <- renameL name
    return (ForeignType name' a b)

  TyData x lcontext lname ltyvars k cons _ -> do
    lcontext' <- renameLContext lcontext
    lname' <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    cons' <- mapM renameLCon cons
    -- we don't need the derivings
    return (TyData x lcontext' lname' ltyvars' k cons' Nothing) 
 
  TySynonym lname ltyvars ltype -> do
    lname' <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    ltype' <- renameLType ltype
    return (TySynonym lname' ltyvars' ltype')

  ClassDecl lcontext lname ltyvars lfundeps lsigs _ _ -> do
    lcontext' <- renameLContext lcontext
    lname' <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    lfundeps' <- mapM renameLFunDep lfundeps 
    lsigs' <- mapM renameLSig lsigs
    -- we don't need the default methods or the already collected doc entities
    return (ClassDecl lcontext' lname' ltyvars' lfundeps' lsigs' emptyBag [])
 
  where
    renameLCon (L loc con) = return . L loc =<< renameCon con
    renameCon (ConDecl lname expl ltyvars lcontext details restype mbldoc) = do
      lname' <- renameL lname 
      ltyvars' <- mapM renameLTyVarBndr ltyvars
      lcontext' <- renameLContext lcontext
      details' <- renameDetails details
      restype' <- renameResType restype
      mbldoc' <- mapM renameLDoc mbldoc
      return (ConDecl lname' expl ltyvars' lcontext' details' restype' mbldoc') 

    renameDetails (RecCon fields) = return . RecCon =<< mapM renameField fields
    renameDetails (PrefixCon ps) = return . PrefixCon =<< mapM renameLType ps
    renameDetails (InfixCon a b) = do
      a' <- renameLType a
      b' <- renameLType b
      return (InfixCon a' b')
  
    renameField (HsRecField id arg doc) = do
      id' <- renameL id
      arg' <- renameLType arg
      doc' <- mapM renameLDoc doc 
      return (HsRecField id' arg' doc')

    renameResType (ResTyH98) = return ResTyH98
    renameResType (ResTyGADT t) = return . ResTyGADT =<< renameLType t

    renameLFunDep (L loc (xs, ys)) = do
      xs' <- mapM rename xs
      ys' <- mapM rename ys
      return (L loc (xs', ys'))
   
    renameLSig (L loc sig) = return . L loc =<< renameSig sig
      
renameSig sig = case sig of 
  TypeSig lname ltype -> do 
    lname' <- renameL lname
    ltype' <- renameLType ltype
    return (TypeSig lname' ltype')
  SpecSig lname ltype x -> do
    lname' <- renameL lname
    ltype' <- renameLType ltype
    return (SpecSig lname' ltype' x)
  InlineSig lname x -> do
    lname' <- renameL lname
    return (InlineSig lname' x)   
  SpecInstSig t -> return . SpecInstSig =<< renameLType t
  FixSig fsig -> return . FixSig =<< renameFixitySig fsig
  where
    renameFixitySig (FixitySig lname x) = do
      lname' <- renameL lname
      return (FixitySig lname' x)

renameForD (ForeignImport lname ltype x y) = do
  lname' <- renameL lname
  ltype' <- renameLType ltype
  return (ForeignImport lname' ltype' x y)
renameForD (ForeignExport lname ltype x y) = do
  lname' <- renameL lname
  ltype' <- renameLType ltype
  return (ForeignExport lname' ltype' x y)

renameExportItem :: ExportItem2 Name -> RnM (ExportItem2 DocName)
renameExportItem item = case item of 
  ExportModule2 mod -> return (ExportModule2 mod)
  ExportGroup2 lev id doc -> do
    doc' <- renameDoc doc
    return (ExportGroup2 lev id doc')
  ExportDecl2 x decl doc instances -> do
    decl' <- renameLDecl decl
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
