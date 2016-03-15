{-# LANGUAGE RecordWildCards #-}
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


import Data.Traversable (mapM)

import Haddock.GhcUtils
import Haddock.Types

import Bag (emptyBag)
import GHC hiding (NoLink)
import Name

import Control.Applicative
import Control.Monad hiding (mapM)
import Data.List
import qualified Data.Map as Map hiding ( Map )
import Prelude hiding (mapM)


renameInterface :: DynFlags -> LinkEnv -> Bool -> Interface -> ErrMsgM Interface
renameInterface dflags renamingEnv warnings iface =

  -- first create the local env, where every name exported by this module
  -- is mapped to itself, and everything else comes from the global renaming
  -- env
  let localEnv = foldl fn renamingEnv (ifaceVisibleExports iface)
        where fn env name = Map.insert name (ifaceMod iface) env

      -- rename names in the exported declarations to point to things that
      -- are closer to, or maybe even exported by, the current module.
      (renamedExportItems, missingNames1)
        = runRnFM localEnv (renameExportItems (ifaceExportItems iface))

      (rnDocMap, missingNames2) = runRnFM localEnv (mapM renameDoc (ifaceDocMap iface))

      (rnArgMap, missingNames3) = runRnFM localEnv (mapM (mapM renameDoc) (ifaceArgMap iface))

      (renamedOrphanInstances, missingNames4)
        = runRnFM localEnv (mapM renameDocInstance (ifaceOrphanInstances iface))

      (finalModuleDoc, missingNames5)
        = runRnFM localEnv (renameDocumentation (ifaceDoc iface))

      -- combine the missing names and filter out the built-ins, which would
      -- otherwise always be missing.
      missingNames = nub $ filter isExternalName  -- XXX: isExternalName filters out too much
                    (missingNames1 ++ missingNames2 ++ missingNames3
                     ++ missingNames4 ++ missingNames5)

      -- filter out certain built in type constructors using their string
      -- representation. TODO: use the Name constants from the GHC API.
--      strings = filter (`notElem` ["()", "[]", "(->)"])
--                (map pretty missingNames)
      strings = map (pretty dflags) . filter (\n -> not (isSystemName n || isBuiltInSyntax n)) $ missingNames

  in do
    -- report things that we couldn't link to. Only do this for non-hidden
    -- modules.
    unless (OptHide `elem` ifaceOptions iface || null strings || not warnings) $
      tell ["Warning: " ++ moduleString (ifaceMod iface) ++
            ": could not find link destinations for:\n"++
            unwords ("   " : strings) ]

    return $ iface { ifaceRnDoc         = finalModuleDoc,
                     ifaceRnDocMap      = rnDocMap,
                     ifaceRnArgMap      = rnArgMap,
                     ifaceRnExportItems = renamedExportItems,
                     ifaceRnOrphanInstances = renamedOrphanInstances}


--------------------------------------------------------------------------------
-- Monad for renaming
--
-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in
-- the environment.
--------------------------------------------------------------------------------


newtype RnM a =
  RnM { unRn :: (Name -> (Bool, DocName))  -- name lookup function
             -> (a,[Name])
      }

instance Monad RnM where
  (>>=) = thenRn
  return = pure

instance Functor RnM where
  fmap f x = do a <- x; return (f a)

instance Applicative RnM where
  pure = returnRn
  (<*>) = ap

returnRn :: a -> RnM a
returnRn a   = RnM (const (a,[]))
thenRn :: RnM a -> (a -> RnM b) -> RnM b
m `thenRn` k = RnM (\lkp -> case unRn m lkp of
  (a,out1) -> case unRn (k a) lkp of
    (b,out2) -> (b,out1++out2))

getLookupRn :: RnM (Name -> (Bool, DocName))
getLookupRn = RnM (\lkp -> (lkp,[]))

outRn :: Name -> RnM ()
outRn name = RnM (const ((),[name]))

lookupRn :: Name -> RnM DocName
lookupRn name = do
  lkp <- getLookupRn
  case lkp name of
    (False,maps_to) -> do outRn name; return maps_to
    (True, maps_to) -> return maps_to


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
rename = lookupRn


renameL :: Located Name -> RnM (Located DocName)
renameL = mapM rename


renameExportItems :: [ExportItem Name] -> RnM [ExportItem DocName]
renameExportItems = mapM renameExportItem


renameDocForDecl :: DocForDecl Name -> RnM (DocForDecl DocName)
renameDocForDecl (doc, fnArgsDoc) =
  (,) <$> renameDocumentation doc <*> renameFnArgsDoc fnArgsDoc


renameDocumentation :: Documentation Name -> RnM (Documentation DocName)
renameDocumentation (Documentation mDoc mWarning) =
  Documentation <$> mapM renameDoc mDoc <*> mapM renameDoc mWarning


renameLDocHsSyn :: LHsDocString -> RnM LHsDocString
renameLDocHsSyn = return


renameDoc :: Traversable t => t Name -> RnM (t DocName)
renameDoc = traverse rename

renameFnArgsDoc :: FnArgsDoc Name -> RnM (FnArgsDoc DocName)
renameFnArgsDoc = mapM renameDoc


renameLType :: LHsType Name -> RnM (LHsType DocName)
renameLType = mapM renameType

renameLSigType :: LHsSigType Name -> RnM (LHsSigType DocName)
renameLSigType = renameImplicit renameLType

renameLSigWcType :: LHsSigWcType Name -> RnM (LHsSigWcType DocName)
renameLSigWcType = renameImplicit (renameWc renameLType)

renameLKind :: LHsKind Name -> RnM (LHsKind DocName)
renameLKind = renameLType

renameMaybeLKind :: Maybe (LHsKind Name) -> RnM (Maybe (LHsKind DocName))
renameMaybeLKind = traverse renameLKind

renameFamilyResultSig :: LFamilyResultSig Name -> RnM (LFamilyResultSig DocName)
renameFamilyResultSig (L loc NoSig)
    = return (L loc NoSig)
renameFamilyResultSig (L loc (KindSig ki))
    = do { ki' <- renameLKind ki
         ; return (L loc (KindSig ki')) }
renameFamilyResultSig (L loc (TyVarSig bndr))
    = do { bndr' <- renameLTyVarBndr bndr
         ; return (L loc (TyVarSig bndr')) }

renameInjectivityAnn :: LInjectivityAnn Name -> RnM (LInjectivityAnn DocName)
renameInjectivityAnn (L loc (InjectivityAnn lhs rhs))
    = do { lhs' <- renameL lhs
         ; rhs' <- mapM renameL rhs
         ; return (L loc (InjectivityAnn lhs' rhs')) }

renameMaybeInjectivityAnn :: Maybe (LInjectivityAnn Name)
                          -> RnM (Maybe (LInjectivityAnn DocName))
renameMaybeInjectivityAnn = traverse renameInjectivityAnn

renameType :: HsType Name -> RnM (HsType DocName)
renameType t = case t of
  HsForAllTy { hst_bndrs = tyvars, hst_body = ltype } -> do
    tyvars'   <- mapM renameLTyVarBndr tyvars
    ltype'    <- renameLType ltype
    return (HsForAllTy { hst_bndrs = tyvars', hst_body = ltype' })

  HsQualTy { hst_ctxt = lcontext , hst_body = ltype } -> do
    lcontext' <- renameLContext lcontext
    ltype'    <- renameLType ltype
    return (HsQualTy { hst_ctxt = lcontext', hst_body = ltype' })

  HsTyVar (L l n) -> return . HsTyVar . L l =<< rename n
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
  HsIParamTy n ty -> liftM (HsIParamTy n) (renameLType ty)
  HsEqTy ty1 ty2 -> liftM2 HsEqTy (renameLType ty1) (renameLType ty2)

  HsTupleTy b ts -> return . HsTupleTy b =<< mapM renameLType ts

  HsOpTy a (L loc op) b -> do
    op' <- rename op
    a'  <- renameLType a
    b'  <- renameLType b
    return (HsOpTy a' (L loc op') b')

  HsParTy ty -> return . HsParTy =<< renameLType ty

  HsKindSig ty k -> do
    ty' <- renameLType ty
    k' <- renameLKind k
    return (HsKindSig ty' k')

  HsDocTy ty doc -> do
    ty' <- renameLType ty
    doc' <- renameLDocHsSyn doc
    return (HsDocTy ty' doc')

  HsTyLit x -> return (HsTyLit x)

  HsRecTy a               -> HsRecTy <$> mapM renameConDeclFieldField a
  HsCoreTy a              -> pure (HsCoreTy a)
  HsExplicitListTy  a b   -> HsExplicitListTy  a <$> mapM renameLType b
  HsExplicitTupleTy a b   -> HsExplicitTupleTy a <$> mapM renameLType b
  HsSpliceTy _ _          -> error "renameType: HsSpliceTy"
  HsWildCardTy a          -> HsWildCardTy <$> renameWildCardInfo a
  HsAppsTy _              -> error "renameType: HsAppsTy"

renameLHsQTyVars :: LHsQTyVars Name -> RnM (LHsQTyVars DocName)
renameLHsQTyVars (HsQTvs { hsq_implicit = _, hsq_explicit = tvs })
  = do { tvs' <- mapM renameLTyVarBndr tvs
       ; return (HsQTvs { hsq_implicit = error "haddock:renameLHsQTyVars", hsq_explicit = tvs', hsq_dependent = error "haddock:renameLHsQTyVars" }) }
                -- This is rather bogus, but I'm not sure what else to do

renameLTyVarBndr :: LHsTyVarBndr Name -> RnM (LHsTyVarBndr DocName)
renameLTyVarBndr (L loc (UserTyVar (L l n)))
  = do { n' <- rename n
       ; return (L loc (UserTyVar (L l n'))) }
renameLTyVarBndr (L loc (KindedTyVar (L lv n) kind))
  = do { n' <- rename n
       ; kind' <- renameLKind kind
       ; return (L loc (KindedTyVar (L lv n') kind')) }

renameLContext :: Located [LHsType Name] -> RnM (Located [LHsType DocName])
renameLContext (L loc context) = do
  context' <- mapM renameLType context
  return (L loc context')

renameWildCardInfo :: HsWildCardInfo Name -> RnM (HsWildCardInfo DocName)
renameWildCardInfo (AnonWildCard  (L l name)) = AnonWildCard . L l <$> rename name

renameInstHead :: InstHead Name -> RnM (InstHead DocName)
renameInstHead InstHead {..} = do
  cname <- rename ihdClsName
  kinds <- mapM renameType ihdKinds
  types <- mapM renameType ihdTypes
  itype <- case ihdInstType of
    ClassInst { .. } -> ClassInst
        <$> mapM renameType clsiCtx
        <*> renameLHsQTyVars clsiTyVars
        <*> mapM renameSig clsiSigs
        <*> mapM renamePseudoFamilyDecl clsiAssocTys
    TypeInst  ts -> TypeInst  <$> traverse renameType ts
    DataInst  dd -> DataInst  <$> renameTyClD dd
  return InstHead
    { ihdClsName = cname
    , ihdKinds = kinds
    , ihdTypes = types
    , ihdInstType = itype
    }

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

renameLThing :: (a Name -> RnM (a DocName)) -> Located (a Name) -> RnM (Located (a DocName))
renameLThing fn (L loc x) = return . L loc =<< fn x

renameTyClD :: TyClDecl Name -> RnM (TyClDecl DocName)
renameTyClD d = case d of
--  TyFamily flav lname ltyvars kind tckind -> do
  FamDecl { tcdFam = decl } -> do
    decl' <- renameFamilyDecl decl
    return (FamDecl { tcdFam = decl' })

  SynDecl { tcdLName = lname, tcdTyVars = tyvars, tcdRhs = rhs, tcdFVs = _fvs } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    rhs'     <- renameLType rhs
    return (SynDecl { tcdLName = lname', tcdTyVars = tyvars', tcdRhs = rhs', tcdFVs = placeHolderNames })

  DataDecl { tcdLName = lname, tcdTyVars = tyvars, tcdDataDefn = defn, tcdFVs = _fvs } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    defn'     <- renameDataDefn defn
    return (DataDecl { tcdLName = lname', tcdTyVars = tyvars', tcdDataDefn = defn', tcdDataCusk = PlaceHolder, tcdFVs = placeHolderNames })

  ClassDecl { tcdCtxt = lcontext, tcdLName = lname, tcdTyVars = ltyvars
            , tcdFDs = lfundeps, tcdSigs = lsigs, tcdATs = ats, tcdATDefs = at_defs } -> do
    lcontext' <- renameLContext lcontext
    lname'    <- renameL lname
    ltyvars'  <- renameLHsQTyVars ltyvars
    lfundeps' <- mapM renameLFunDep lfundeps
    lsigs'    <- mapM renameLSig lsigs
    ats'      <- mapM (renameLThing renameFamilyDecl) ats
    at_defs'  <- mapM renameLTyFamDefltEqn at_defs
    -- we don't need the default methods or the already collected doc entities
    return (ClassDecl { tcdCtxt = lcontext', tcdLName = lname', tcdTyVars = ltyvars'
                      , tcdFDs = lfundeps', tcdSigs = lsigs', tcdMeths= emptyBag
                      , tcdATs = ats', tcdATDefs = at_defs', tcdDocs = [], tcdFVs = placeHolderNames })

  where
    renameLFunDep (L loc (xs, ys)) = do
      xs' <- mapM rename (map unLoc xs)
      ys' <- mapM rename (map unLoc ys)
      return (L loc (map noLoc xs', map noLoc ys'))

    renameLSig (L loc sig) = return . L loc =<< renameSig sig

renameFamilyDecl :: FamilyDecl Name -> RnM (FamilyDecl DocName)
renameFamilyDecl (FamilyDecl { fdInfo = info, fdLName = lname
                             , fdTyVars = ltyvars, fdResultSig = result
                             , fdInjectivityAnn = injectivity }) = do
    info'        <- renameFamilyInfo info
    lname'       <- renameL lname
    ltyvars'     <- renameLHsQTyVars ltyvars
    result'      <- renameFamilyResultSig result
    injectivity' <- renameMaybeInjectivityAnn injectivity
    return (FamilyDecl { fdInfo = info', fdLName = lname'
                       , fdTyVars = ltyvars', fdResultSig = result'
                       , fdInjectivityAnn = injectivity' })


renamePseudoFamilyDecl :: PseudoFamilyDecl Name
                       -> RnM (PseudoFamilyDecl DocName)
renamePseudoFamilyDecl (PseudoFamilyDecl { .. }) =  PseudoFamilyDecl
    <$> renameFamilyInfo pfdInfo
    <*> renameL pfdLName
    <*> mapM renameLType pfdTyVars
    <*> renameFamilyResultSig pfdKindSig


renameFamilyInfo :: FamilyInfo Name -> RnM (FamilyInfo DocName)
renameFamilyInfo DataFamily     = return DataFamily
renameFamilyInfo OpenTypeFamily = return OpenTypeFamily
renameFamilyInfo (ClosedTypeFamily eqns)
  = do { eqns' <- mapM (mapM renameLTyFamInstEqn) eqns
       ; return $ ClosedTypeFamily eqns' }

renameDataDefn :: HsDataDefn Name -> RnM (HsDataDefn DocName)
renameDataDefn (HsDataDefn { dd_ND = nd, dd_ctxt = lcontext, dd_cType = cType
                           , dd_kindSig = k, dd_cons = cons }) = do
    lcontext' <- renameLContext lcontext
    k'        <- renameMaybeLKind k
    cons'     <- mapM (mapM renameCon) cons
    -- I don't think we need the derivings, so we return Nothing
    return (HsDataDefn { dd_ND = nd, dd_ctxt = lcontext', dd_cType = cType
                       , dd_kindSig = k', dd_cons = cons', dd_derivs = Nothing })

renameCon :: ConDecl Name -> RnM (ConDecl DocName)
renameCon decl@(ConDeclH98 { con_name = lname, con_qvars = ltyvars
                           , con_cxt = lcontext, con_details = details
                           , con_doc = mbldoc }) = do
      lname'    <- renameL lname
      ltyvars'  <- traverse renameLHsQTyVars ltyvars
      lcontext' <- traverse renameLContext lcontext
      details'  <- renameDetails details
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_name = lname', con_qvars = ltyvars', con_cxt = lcontext'
                   , con_details = details', con_doc = mbldoc' })

  where
    renameDetails (RecCon (L l fields)) = do
      fields' <- mapM renameConDeclFieldField fields
      return (RecCon (L l fields'))
    renameDetails (PrefixCon ps) = return . PrefixCon =<< mapM renameLType ps
    renameDetails (InfixCon a b) = do
      a' <- renameLType a
      b' <- renameLType b
      return (InfixCon a' b')

renameCon decl@(ConDeclGADT { con_names = lnames
                            , con_type = lty
                            , con_doc = mbldoc }) = do
      lnames'   <- mapM renameL lnames
      lty'      <- renameLSigType lty
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_names = lnames'
                   , con_type = lty', con_doc = mbldoc' })

renameConDeclFieldField :: LConDeclField Name -> RnM (LConDeclField DocName)
renameConDeclFieldField (L l (ConDeclField names t doc)) = do
  names' <- mapM renameLFieldOcc names
  t'   <- renameLType t
  doc' <- mapM renameLDocHsSyn doc
  return $ L l (ConDeclField names' t' doc')

renameLFieldOcc :: LFieldOcc Name -> RnM (LFieldOcc DocName)
renameLFieldOcc (L l (FieldOcc lbl sel)) = do
  sel' <- rename sel
  return $ L l (FieldOcc lbl sel')

renameSig :: Sig Name -> RnM (Sig DocName)
renameSig sig = case sig of
  TypeSig lnames ltype -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigWcType ltype
    return (TypeSig lnames' ltype')
  ClassOpSig is_default lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigType sig_ty
    return (ClassOpSig is_default lnames' ltype')
  PatSynSig lname sig_ty -> do
    lname' <- renameL lname
    sig_ty' <- renameLSigType sig_ty
    return $ PatSynSig lname' sig_ty'
  FixSig (FixitySig lnames fixity) -> do
    lnames' <- mapM renameL lnames
    return $ FixSig (FixitySig lnames' fixity)
  MinimalSig src (L l s) -> do
    s' <- traverse renameL s
    return $ MinimalSig src (L l s')
  -- we have filtered out all other kinds of signatures in Interface.Create
  _ -> error "expected TypeSig"


renameForD :: ForeignDecl Name -> RnM (ForeignDecl DocName)
renameForD (ForeignImport lname ltype co x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignImport lname' ltype' co x)
renameForD (ForeignExport lname ltype co x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignExport lname' ltype' co x)


renameInstD :: InstDecl Name -> RnM (InstDecl DocName)
renameInstD (ClsInstD { cid_inst = d }) = do
  d' <- renameClsInstD d
  return (ClsInstD { cid_inst = d' })
renameInstD (TyFamInstD { tfid_inst = d }) = do
  d' <- renameTyFamInstD d
  return (TyFamInstD { tfid_inst = d' })
renameInstD (DataFamInstD { dfid_inst = d }) = do
  d' <- renameDataFamInstD d
  return (DataFamInstD { dfid_inst = d' })

renameClsInstD :: ClsInstDecl Name -> RnM (ClsInstDecl DocName)
renameClsInstD (ClsInstDecl { cid_overlap_mode = omode
                            , cid_poly_ty =ltype, cid_tyfam_insts = lATs
                            , cid_datafam_insts = lADTs }) = do
  ltype' <- renameLSigType ltype
  lATs'  <- mapM (mapM renameTyFamInstD) lATs
  lADTs' <- mapM (mapM renameDataFamInstD) lADTs
  return (ClsInstDecl { cid_overlap_mode = omode
                      , cid_poly_ty = ltype', cid_binds = emptyBag
                      , cid_sigs = []
                      , cid_tyfam_insts = lATs', cid_datafam_insts = lADTs' })


renameTyFamInstD :: TyFamInstDecl Name -> RnM (TyFamInstDecl DocName)
renameTyFamInstD (TyFamInstDecl { tfid_eqn = eqn })
  = do { eqn' <- renameLTyFamInstEqn eqn
       ; return (TyFamInstDecl { tfid_eqn = eqn'
                               , tfid_fvs = placeHolderNames }) }

renameLTyFamInstEqn :: LTyFamInstEqn Name -> RnM (LTyFamInstEqn DocName)
renameLTyFamInstEqn (L loc (TyFamEqn { tfe_tycon = tc, tfe_pats = pats, tfe_rhs = rhs }))
  = do { tc' <- renameL tc
       ; pats' <- renameImplicit (mapM renameLType) pats
       ; rhs' <- renameLType rhs
       ; return (L loc (TyFamEqn { tfe_tycon = tc'
                                 , tfe_pats = pats'
                                 , tfe_rhs = rhs' })) }

renameLTyFamDefltEqn :: LTyFamDefltEqn Name -> RnM (LTyFamDefltEqn DocName)
renameLTyFamDefltEqn (L loc (TyFamEqn { tfe_tycon = tc, tfe_pats = tvs, tfe_rhs = rhs }))
  = do { tc'  <- renameL tc
       ; tvs' <- renameLHsQTyVars tvs
       ; rhs' <- renameLType rhs
       ; return (L loc (TyFamEqn { tfe_tycon = tc'
                                 , tfe_pats = tvs'
                                 , tfe_rhs = rhs' })) }

renameDataFamInstD :: DataFamInstDecl Name -> RnM (DataFamInstDecl DocName)
renameDataFamInstD (DataFamInstDecl { dfid_tycon = tc, dfid_pats = pats, dfid_defn = defn })
  = do { tc' <- renameL tc
       ; pats' <- renameImplicit (mapM renameLType) pats
       ; defn' <- renameDataDefn defn
       ; return (DataFamInstDecl { dfid_tycon = tc'
                                 , dfid_pats = pats'
                                 , dfid_defn = defn', dfid_fvs = placeHolderNames }) }

renameImplicit :: (in_thing -> RnM out_thing)
               -> HsImplicitBndrs Name in_thing
               -> RnM (HsImplicitBndrs DocName out_thing)
renameImplicit rn_thing (HsIB { hsib_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsIB { hsib_body = thing'
                      , hsib_vars = PlaceHolder }) }

renameWc :: (in_thing -> RnM out_thing)
         -> HsWildCardBndrs Name in_thing
         -> RnM (HsWildCardBndrs DocName out_thing)
renameWc rn_thing (HsWC { hswc_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsWC { hswc_body = thing'
                      , hswc_wcs = PlaceHolder, hswc_ctx = Nothing }) }

renameDocInstance :: DocInstance Name -> RnM (DocInstance DocName)
renameDocInstance (inst, idoc, L l n) = do
  inst' <- renameInstHead inst
  n' <- rename n
  idoc' <- mapM renameDoc idoc
  return (inst', idoc',L l n')

renameExportItem :: ExportItem Name -> RnM (ExportItem DocName)
renameExportItem item = case item of
  ExportModule mdl -> return (ExportModule mdl)
  ExportGroup lev id_ doc -> do
    doc' <- renameDoc doc
    return (ExportGroup lev id_ doc')
  ExportDecl decl doc subs instances fixities splice -> do
    decl' <- renameLDecl decl
    doc'  <- renameDocForDecl doc
    subs' <- mapM renameSub subs
    instances' <- forM instances renameDocInstance
    fixities' <- forM fixities $ \(name, fixity) -> do
      name' <- lookupRn name
      return (name', fixity)
    return (ExportDecl decl' doc' subs' instances' fixities' splice)
  ExportNoDecl x subs -> do
    x'    <- lookupRn x
    subs' <- mapM lookupRn subs
    return (ExportNoDecl x' subs')
  ExportDoc doc -> do
    doc' <- renameDoc doc
    return (ExportDoc doc')


renameSub :: (Name, DocForDecl Name) -> RnM (DocName, DocForDecl DocName)
renameSub (n,doc) = do
  n' <- rename n
  doc' <- renameDocForDecl doc
  return (n', doc')
