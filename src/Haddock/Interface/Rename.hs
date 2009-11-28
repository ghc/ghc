----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.Rename
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Haddock.Interface.Rename (renameInterface) where


import Haddock.Types
import Haddock.GhcUtils

import GHC hiding (NoLink)
import Name
import BasicTypes
import Bag (emptyBag)

import Data.List
import qualified Data.Map as Map hiding ( Map )
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad hiding (mapM)


renameInterface :: LinkEnv -> Bool -> Interface -> ErrMsgM Interface
renameInterface renamingEnv warnings iface =

  -- first create the local env, where every name exported by this module
  -- is mapped to itself, and everything else comes from the global renaming
  -- env
  let localEnv = foldl fn renamingEnv (ifaceVisibleExports iface)
        where fn env name = Map.insert name (ifaceMod iface) env

      docMap   = Map.map (\(_,x,_) -> x) (ifaceDeclMap iface)

      -- make instance docs into 'docForDecls'
      instDocs = [ (name, (Just doc, Map.empty))
                 | (name, doc) <- Map.toList (ifaceInstanceDocMap iface) ]

      docs     = Map.toList docMap ++ instDocs
      renameMapElem (k,d) = do d' <- renameDocForDecl d; return (k, d')

      -- rename names in the exported declarations to point to things that
      -- are closer to, or maybe even exported by, the current module.
      (renamedExportItems, missingNames1)
        = runRnFM localEnv (renameExportItems (ifaceExportItems iface))

      (rnDocMap, missingNames2)
        = runRnFM localEnv (liftM Map.fromList (mapM renameMapElem docs))

      (finalModuleDoc, missingNames3)
        = runRnFM localEnv (renameMaybeDoc (ifaceDoc iface))

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
    unless (OptHide `elem` ifaceOptions iface || null strings || not warnings) $
      tell ["Warning: " ++ moduleString (ifaceMod iface) ++
            ": could not find link destinations for:\n"++
            unwords ("   " : strings) ]

    return $ iface { ifaceRnDoc         = finalModuleDoc,
                     ifaceRnDocMap      = rnDocMap,
                     ifaceRnExportItems = renamedExportItems }


--------------------------------------------------------------------------------
-- Monad for renaming
--
-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in
-- the environment.
--------------------------------------------------------------------------------


newtype GenRnM n a =
  RnM { unRn :: (n -> (Bool, DocName))  -- name lookup function
             -> (a,[n])
      }

type RnM a = GenRnM Name a

instance Monad (GenRnM n) where
  (>>=) = thenRn
  return = returnRn

returnRn :: a -> GenRnM n a
returnRn a   = RnM (const (a,[]))
thenRn :: GenRnM n a -> (a -> GenRnM n b) -> GenRnM n b
m `thenRn` k = RnM (\lkp -> case unRn m lkp of
  (a,out1) -> case unRn (k a) lkp of
    (b,out2) -> (b,out1++out2))

getLookupRn :: RnM (Name -> (Bool, DocName))
getLookupRn = RnM (\lkp -> (lkp,[]))
outRn :: Name -> RnM ()
outRn name = RnM (const ((),[name]))

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
      Nothing  -> (False, Undocumented n)
      Just mdl -> (True,  Documented n mdl)


--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------


rename :: Name -> RnM DocName
rename = lookupRn id


renameL :: Located Name -> RnM (Located DocName)
renameL = mapM rename


renameExportItems :: [ExportItem Name] -> RnM [ExportItem DocName]
renameExportItems = mapM renameExportItem


renameDocForDecl :: (Maybe (Doc Name), FnArgsDoc Name) -> RnM (Maybe (Doc DocName), FnArgsDoc DocName)
renameDocForDecl (mbDoc, fnArgsDoc) = do
  mbDoc' <- renameMaybeDoc mbDoc
  fnArgsDoc' <- renameFnArgsDoc fnArgsDoc
  return (mbDoc', fnArgsDoc')


renameMaybeDoc :: Maybe (Doc Name) -> RnM (Maybe (Doc DocName))
renameMaybeDoc = mapM renameDoc


renameLDocHsSyn :: LHsDocString -> RnM LHsDocString
renameLDocHsSyn = return


renameDoc :: Doc Name -> RnM (Doc DocName)
renameDoc d = case d of
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
      [] -> return (DocIdentifier (map Undocumented ids))
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
  DocPic str -> return (DocPic str)
  DocAName str -> return (DocAName str)


renameFnArgsDoc :: FnArgsDoc Name -> RnM (FnArgsDoc DocName)
renameFnArgsDoc = mapM renameDoc


renameLPred :: LHsPred Name -> RnM (LHsPred DocName)
renameLPred = mapM renamePred


renamePred :: HsPred Name -> RnM (HsPred DocName)
renamePred (HsClassP name types) = do
  name'  <- rename name
  types' <- mapM renameLType types
  return (HsClassP name' types')
renamePred (HsEqualP type1 type2) = do
  type1' <- renameLType type1
  type2' <- renameLType type2
  return (HsEqualP type1' type2')
renamePred (HsIParam (IPName name) t) = do
  name' <- rename name
  t'    <- renameLType t
  return (HsIParam (IPName name') t')


renameLType :: LHsType Name -> RnM (LHsType DocName)
renameLType = mapM renameType


renameType :: HsType Name -> RnM (HsType DocName)
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

  HsListTy ty -> return . HsListTy =<< renameLType ty
  HsPArrTy ty -> return . HsPArrTy =<< renameLType ty

  HsTupleTy b ts -> return . HsTupleTy b =<< mapM renameLType ts

  HsOpTy a (L loc op) b -> do
    op' <- rename op
    a'  <- renameLType a
    b'  <- renameLType b
    return (HsOpTy a' (L loc op') b')

  HsParTy ty -> return . HsParTy =<< renameLType ty

  HsNumTy n -> return (HsNumTy n)

  HsPredTy p -> return . HsPredTy =<< renamePred p

  HsKindSig ty k -> do
    ty' <- renameLType ty
    return (HsKindSig ty' k)

  HsDocTy ty doc -> do
    ty' <- renameLType ty
    doc' <- renameLDocHsSyn doc
    return (HsDocTy ty' doc')

  _ -> error "renameType"


renameLTyVarBndr :: LHsTyVarBndr Name -> RnM (LHsTyVarBndr DocName)
renameLTyVarBndr (L loc tv) = do
  name' <- rename (hsTyVarName tv)
  return $ L loc (replaceTyVarName tv name')


renameLContext :: Located [LHsPred Name] -> RnM (Located [LHsPred DocName])
renameLContext (L loc context) = do
  context' <- mapM renameLPred context
  return (L loc context')


renameInstHead :: InstHead Name -> RnM (InstHead DocName)
renameInstHead (preds, className, types) = do
  preds' <- mapM renamePred preds
  className' <- rename className
  types' <- mapM renameType types
  return (preds', className', types')


renameLDecl :: LHsDecl Name -> RnM (LHsDecl DocName)
renameLDecl (L loc d) = return . L loc =<< renameDecl d


renameDecl :: HsDecl Name -> RnM (HsDecl DocName)
renameDecl decl = case decl of
  TyClD d -> do
    d' <- renameTyClD d
    return (TyClD d')
  SigD s -> do
    s' <- renameSig s
    return (SigD s')
  ForD d -> do
    d' <- renameForD d
    return (ForD d')
  InstD d -> do
    d' <- renameInstD d
    return (InstD d')
  _ -> error "renameDecl"


renameLTyClD :: LTyClDecl Name -> RnM (LTyClDecl DocName)
renameLTyClD (L loc d) = return . L loc =<< renameTyClD d


renameTyClD :: TyClDecl Name -> RnM (TyClDecl DocName)
renameTyClD d = case d of
  ForeignType lname b -> do
    lname' <- renameL lname
    return (ForeignType lname' b)

  TyFamily flav lname ltyvars kind -> do
    lname'   <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    return (TyFamily flav lname' ltyvars' kind)

  TyData x lcontext lname ltyvars typats k cons _ -> do
    lcontext' <- renameLContext lcontext
    lname'    <- renameL lname
    ltyvars'  <- mapM renameLTyVarBndr ltyvars
    typats'   <- mapM (mapM renameLType) typats
    cons'     <- mapM renameLCon cons
    -- I don't think we need the derivings, so we return Nothing
    return (TyData x lcontext' lname' ltyvars' typats' k cons' Nothing)

  TySynonym lname ltyvars typats ltype -> do
    lname'   <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    ltype'   <- renameLType ltype
    typats'  <- mapM (mapM renameLType) typats
    return (TySynonym lname' ltyvars' typats' ltype')

  ClassDecl lcontext lname ltyvars lfundeps lsigs _ ats _ -> do
    lcontext' <- renameLContext lcontext
    lname'    <- renameL lname
    ltyvars'  <- mapM renameLTyVarBndr ltyvars
    lfundeps' <- mapM renameLFunDep lfundeps
    lsigs'    <- mapM renameLSig lsigs
    ats'      <- mapM renameLTyClD ats
    -- we don't need the default methods or the already collected doc entities
    return (ClassDecl lcontext' lname' ltyvars' lfundeps' lsigs' emptyBag ats' [])

  where
    renameLCon (L loc con) = return . L loc =<< renameCon con
    renameCon decl@(ConDecl { con_name = lname, con_qvars = ltyvars
                            , con_cxt = lcontext, con_details = details
                            , con_res = restype, con_doc = mbldoc }) = do
      lname'    <- renameL lname
      ltyvars'  <- mapM renameLTyVarBndr ltyvars
      lcontext' <- renameLContext lcontext
      details'  <- renameDetails details
      restype'  <- renameResType restype
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_name = lname', con_qvars = ltyvars', con_cxt = lcontext'
                   , con_details = details', con_res = restype', con_doc = mbldoc' })

    renameDetails (RecCon fields) = return . RecCon =<< mapM renameField fields
    renameDetails (PrefixCon ps) = return . PrefixCon =<< mapM renameLType ps
    renameDetails (InfixCon a b) = do
      a' <- renameLType a
      b' <- renameLType b
      return (InfixCon a' b')

    renameField (ConDeclField name t doc) = do
      name' <- renameL name
      t'   <- renameLType t
      doc' <- mapM renameLDocHsSyn doc
      return (ConDeclField name' t' doc')

    renameResType (ResTyH98) = return ResTyH98
    renameResType (ResTyGADT t) = return . ResTyGADT =<< renameLType t

    renameLFunDep (L loc (xs, ys)) = do
      xs' <- mapM rename xs
      ys' <- mapM rename ys
      return (L loc (xs', ys'))

    renameLSig (L loc sig) = return . L loc =<< renameSig sig


renameSig :: Sig Name -> RnM (Sig DocName)
renameSig sig = case sig of
  TypeSig lname ltype -> do
    lname' <- renameL lname
    ltype' <- renameLType ltype
    return (TypeSig lname' ltype')
  -- we have filtered out all other kinds of signatures in Interface.Create
  _ -> error "expected TypeSig"


renameForD :: ForeignDecl Name -> RnM (ForeignDecl DocName)
renameForD (ForeignImport lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLType ltype
  return (ForeignImport lname' ltype' x)
renameForD (ForeignExport lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLType ltype
  return (ForeignExport lname' ltype' x)


renameInstD :: InstDecl Name -> RnM (InstDecl DocName)
renameInstD (InstDecl ltype _ _ lATs) = do
  ltype' <- renameLType ltype
  lATs' <- mapM renameLTyClD lATs
  return (InstDecl ltype' emptyBag [] lATs')


renameExportItem :: ExportItem Name -> RnM (ExportItem DocName)
renameExportItem item = case item of
  ExportModule mdl -> return (ExportModule mdl)
  ExportGroup lev id_ doc -> do
    doc' <- renameDoc doc
    return (ExportGroup lev id_ doc')
  ExportDecl decl doc subs instances -> do
    decl' <- renameLDecl decl
    doc'  <- renameDocForDecl doc
    subs' <- mapM renameSub subs
    instances' <- forM instances $ \(inst, idoc) -> do
      inst' <- renameInstHead inst
      idoc' <- mapM renameDoc idoc
      return (inst', idoc')
    return (ExportDecl decl' doc' subs' instances')
  ExportNoDecl x subs -> do
    x'    <- lookupRn id x
    subs' <- mapM (lookupRn id) subs
    return (ExportNoDecl x' subs')
  ExportDoc doc -> do
    doc' <- renameDoc doc
    return (ExportDoc doc')


renameSub :: (Name, DocForDecl Name) -> RnM (DocName, DocForDecl DocName)
renameSub (n,doc) = do
  n' <- rename n
  doc' <- renameDocForDecl doc
  return (n', doc')
