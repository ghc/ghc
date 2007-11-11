--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Interface.Rename (renameInterface) where


import Haddock.Types
import Haddock.GHC.Utils

import GHC hiding (NoLink)
import Name
import BasicTypes
import SrcLoc 
import Bag (emptyBag)
import Outputable
import Util (thenCmp)

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map hiding ( Map )
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Arrow
import Control.Monad hiding (mapM)


renameInterface :: LinkEnv -> Interface -> ErrMsgM Interface
renameInterface renamingEnv mod =

  -- first create the local env, where every name exported by this module
  -- is mapped to itself, and everything else comes from the global renaming
  -- env
  let localEnv = foldl fn renamingEnv (ifaceVisibleExports mod)
        where fn env name = Map.insert name (nameSetMod name (ifaceMod mod)) env
      
      docs = Map.toList (ifaceDocMap mod)
      renameMapElem (k,d) = do d' <- renameDoc d; return (k, d') 

      -- rename names in the exported declarations to point to things that
      -- are closer to, or maybe even exported by, the current module.
      (renamedExportItems, missingNames1)
        = runRnFM localEnv (renameExportItems (ifaceExportItems mod))

      (rnDocMap, missingNames2) 
        = runRnFM localEnv (liftM Map.fromList (mapM renameMapElem docs))

      (finalModuleDoc, missingNames3)
        = runRnFM localEnv (renameMaybeDoc (ifaceDoc mod))

      -- combine the missing names and filter out the built-ins, which would
      -- otherwise allways be missing. 
      missingNames = nub $ filter isExternalName
                    (missingNames1 ++ missingNames2 ++ missingNames3)

      -- filter out certain built in type constructors using their string 
      -- representation. TODO: use the Name constants from the GHC API.
--      strings = filter (`notElem` ["()", "[]", "(->)"]) 
--                (map pretty missingNames)
      strings = map pretty . filter (\n -> not (isSystemName n || isBuiltInSyntax n)) $ missingNames
     
  in do
    -- report things that we couldn't link to. Only do this for non-hidden
    -- modules.
    when (OptHide `notElem` ifaceOptions mod && not (null strings)) $
	  tell ["Warning: " ++ show (ppr (ifaceMod mod) defaultUserStyle) ++ 
		": could not find link destinations for:\n"++
		"   " ++ concat (map (' ':) strings) ]

    return $ mod { ifaceRnDoc = finalModuleDoc,
                   ifaceRnDocMap = rnDocMap,
                   ifaceRnExportItems = renamedExportItems }


--------------------------------------------------------------------------------
-- Monad for renaming
--
-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in 
-- the environment.
--------------------------------------------------------------------------------


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


runRnFM :: LinkEnv -> RnM a -> (a,[Name])
runRnFM env rn = unRn rn lkp 
  where 
    lkp n = case Map.lookup n env of
      Nothing -> (False, NoLink n) 
      Just q  -> (True, Link q)


--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------


keep n = NoLink n
keepL (L loc n) = L loc (NoLink n)


rename = lookupRn id 
renameL (L loc name) = return . L loc =<< rename name


renameExportItems :: [ExportItem Name] -> RnM [ExportItem DocName]
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
      [] -> return (DocIdentifier (map NoLink ids))
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


renameLPred (L loc p) = return . L loc =<< renamePred p


renamePred :: HsPred Name -> RnM (HsPred DocName)
renamePred (HsClassP name types) = do
  name'  <- rename name 
  types' <- mapM renameLType types
  return (HsClassP name' types')
renamePred (HsIParam (IPName name) t) = do
  name' <- rename name
  t'    <- renameLType t
  return (HsIParam (IPName name') t')


renameLType (L loc t) = return . L loc =<< renameType t


renameType t = case t of 
  HsForAllTy expl tyvars lcontext ltype -> do
    tyvars'   <- mapM renameLTyVarBndr tyvars
    lcontext' <- renameLContext lcontext 
    ltype'    <- renameLType ltype
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
    a'  <- renameLType a
    b'  <- renameLType b
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


renameInstHead :: InstHead Name -> RnM (InstHead DocName)
renameInstHead (preds, className, types) = do
  preds' <- mapM renamePred preds
  className' <- rename className
  types' <- mapM renameType types
  return (preds', className', types')


renameLDecl (L loc d) = return . L loc =<< renameDecl d


renameDecl d = case d of
  TyClD d -> do
    d' <- renameTyClD d
    return (TyClD d')
  SigD s -> do
    s' <- renameSig s
    return (SigD s')
  ForD d -> do
    d' <- renameForD d
    return (ForD d')
  _ -> error "renameDecl"


renameTyClD d = case d of
  ForeignType _ _ _ -> error "renameTyClD" -- I'm guessing these can't be exported
 -- ForeignType name a b -> do
 --   name' <- renameL name
 --   return (ForeignType name' a b)

  TyData x lcontext lname ltyvars _ k cons _ -> do
    lcontext' <- renameLContext lcontext
    ltyvars'  <- mapM renameLTyVarBndr ltyvars
    cons'     <- mapM renameLCon cons
    -- I don't think we need the derivings, so we return Nothing
    -- We skip the type patterns too. TODO: find out what they are :-)
    return (TyData x lcontext' (keepL lname) ltyvars' Nothing k cons' Nothing) 
 
  TySynonym lname ltyvars typat ltype -> do
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    ltype'   <- renameLType ltype
    -- We skip type patterns here as well.
    return (TySynonym (keepL lname) ltyvars' Nothing ltype')

  ClassDecl lcontext lname ltyvars lfundeps lsigs _ _ _ -> do
    lcontext' <- renameLContext lcontext
    ltyvars'  <- mapM renameLTyVarBndr ltyvars
    lfundeps' <- mapM renameLFunDep lfundeps 
    lsigs'    <- mapM renameLSig lsigs
    -- we don't need the default methods or the already collected doc entities
    -- we skip the ATs for now.
    return (ClassDecl lcontext' (keepL lname) ltyvars' lfundeps' lsigs' emptyBag [] [])
 
  where
    renameLCon (L loc con) = return . L loc =<< renameCon con
    renameCon (ConDecl lname expl ltyvars lcontext details restype mbldoc) = do
      ltyvars'  <- mapM renameLTyVarBndr ltyvars
      lcontext' <- renameLContext lcontext
      details'  <- renameDetails details
      restype'  <- renameResType restype
      mbldoc'   <- mapM renameLDoc mbldoc
      return (ConDecl (keepL lname) expl ltyvars' lcontext' details' restype' mbldoc') 

    renameDetails (RecCon fields) = return . RecCon =<< mapM renameField fields
    renameDetails (PrefixCon ps) = return . PrefixCon =<< mapM renameLType ps
    renameDetails (InfixCon a b) = do
      a' <- renameLType a
      b' <- renameLType b
      return (InfixCon a' b')

    renameField (ConDeclField name t doc) = do
      t'   <- renameLType t
      doc' <- mapM renameLDoc doc
      return (ConDeclField (keepL name) t' doc')

    renameResType (ResTyH98) = return ResTyH98
    renameResType (ResTyGADT t) = return . ResTyGADT =<< renameLType t

    renameLFunDep (L loc (xs, ys)) = return (L loc (map keep xs, map keep ys))
   
    renameLSig (L loc sig) = return . L loc =<< renameSig sig

      
renameSig sig = case sig of 
  TypeSig (L loc name) ltype -> do 
    ltype' <- renameLType ltype
    return (TypeSig (L loc (keep name)) ltype')
  -- we have filtered out all other kinds of signatures in Interface.Create


renameForD (ForeignImport lname ltype x) = do
  ltype' <- renameLType ltype
  return (ForeignImport (keepL lname) ltype' x)
renameForD (ForeignExport lname ltype x) = do
  ltype' <- renameLType ltype
  return (ForeignExport (keepL lname) ltype' x)


renameExportItem :: ExportItem Name -> RnM (ExportItem DocName)
renameExportItem item = case item of 
  ExportModule mod -> return (ExportModule mod)
  ExportGroup lev id doc -> do
    doc' <- renameDoc doc
    return (ExportGroup lev id doc')
  ExportDecl x decl doc instances -> do
    decl' <- renameLDecl decl
    doc'  <- mapM renameDoc doc
    instances' <- mapM renameInstHead instances
    return (ExportDecl x decl' doc' instances')
  ExportNoDecl x y subs -> do
    y'    <- lookupRn id y
    subs' <- mapM (lookupRn id) subs
    return (ExportNoDecl x y' subs')
  ExportDoc doc -> do
    doc' <- renameDoc doc
    return (ExportDoc doc')
