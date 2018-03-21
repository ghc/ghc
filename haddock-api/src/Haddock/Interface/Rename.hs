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
import RdrName (RdrName(Exact))
import PrelNames (eqTyCon_RDR)

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
      missingNames = nubByName id $ filter isExternalName  -- XXX: isExternalName filters out too much
                    (missingNames1 ++ missingNames2 ++ missingNames3
                     ++ missingNames4 ++ missingNames5)

      -- Filter out certain built in type constructors using their string
      -- representation.
      --
      -- Note that since the renamed AST represents equality constraints as
      -- @HasOpTy t1 eqTyCon_RDR t2@ (and _not_ as @HsEqTy t1 t2@), we need to
      -- manually filter out 'eqTyCon_RDR' (aka @~@).
      strings = [ pretty dflags n
                | n <- missingNames
                , not (isSystemName n)
                , not (isBuiltInSyntax n)
                , Exact n /= eqTyCon_RDR
                ]

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


renameExportItems :: [ExportItem GhcRn] -> RnM [ExportItem DocNameI]
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


renameLType :: LHsType GhcRn -> RnM (LHsType DocNameI)
renameLType = mapM renameType

renameLSigType :: LHsSigType GhcRn -> RnM (LHsSigType DocNameI)
renameLSigType = renameImplicit renameLType

renameLSigWcType :: LHsSigWcType GhcRn -> RnM (LHsSigWcType DocNameI)
renameLSigWcType = renameWc (renameImplicit renameLType)

renameLKind :: LHsKind GhcRn -> RnM (LHsKind DocNameI)
renameLKind = renameLType

renameMaybeLKind :: Maybe (LHsKind GhcRn) -> RnM (Maybe (LHsKind DocNameI))
renameMaybeLKind = traverse renameLKind

renameFamilyResultSig :: LFamilyResultSig GhcRn -> RnM (LFamilyResultSig DocNameI)
renameFamilyResultSig (L loc NoSig)
    = return (L loc NoSig)
renameFamilyResultSig (L loc (KindSig ki))
    = do { ki' <- renameLKind ki
         ; return (L loc (KindSig ki')) }
renameFamilyResultSig (L loc (TyVarSig bndr))
    = do { bndr' <- renameLTyVarBndr bndr
         ; return (L loc (TyVarSig bndr')) }

renameInjectivityAnn :: LInjectivityAnn GhcRn -> RnM (LInjectivityAnn DocNameI)
renameInjectivityAnn (L loc (InjectivityAnn lhs rhs))
    = do { lhs' <- renameL lhs
         ; rhs' <- mapM renameL rhs
         ; return (L loc (InjectivityAnn lhs' rhs')) }

renameMaybeInjectivityAnn :: Maybe (LInjectivityAnn GhcRn)
                          -> RnM (Maybe (LInjectivityAnn DocNameI))
renameMaybeInjectivityAnn = traverse renameInjectivityAnn

renameType :: HsType GhcRn -> RnM (HsType DocNameI)
renameType t = case t of
  HsForAllTy { hst_bndrs = tyvars, hst_body = ltype } -> do
    tyvars'   <- mapM renameLTyVarBndr tyvars
    ltype'    <- renameLType ltype
    return (HsForAllTy { hst_bndrs = tyvars', hst_body = ltype' })

  HsQualTy { hst_ctxt = lcontext , hst_body = ltype } -> do
    lcontext' <- renameLContext lcontext
    ltype'    <- renameLType ltype
    return (HsQualTy { hst_ctxt = lcontext', hst_body = ltype' })

  HsTyVar ip (L l n) -> return . HsTyVar ip . L l =<< rename n
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
  HsSumTy ts -> HsSumTy <$> mapM renameLType ts

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
  HsExplicitListTy i a b  -> HsExplicitListTy i a <$> mapM renameLType b
  HsExplicitTupleTy a b   -> HsExplicitTupleTy a <$> mapM renameLType b
  HsSpliceTy s _          -> renameHsSpliceTy s
  HsWildCardTy a          -> HsWildCardTy <$> renameWildCardInfo a
  HsAppsTy _              -> error "renameType: HsAppsTy"

-- | Rename splices, but _only_ those that turn out to be for types.
-- I think this is actually safe for our possible inputs:
--
--  * the input is from after GHC's renamer, so should have an 'HsSpliced'
--  * the input is typechecked, and only 'HsSplicedTy' should get through that
--
renameHsSpliceTy :: HsSplice GhcRn -> RnM (HsType DocNameI)
renameHsSpliceTy (HsSpliced _ (HsSplicedTy t)) = renameType t
renameHsSpliceTy (HsSpliced _ _) = error "renameHsSpliceTy: not an HsSplicedTy"
renameHsSpliceTy _ = error "renameHsSpliceTy: not an HsSpliced"

renameLHsQTyVars :: LHsQTyVars GhcRn -> RnM (LHsQTyVars DocNameI)
renameLHsQTyVars (HsQTvs { hsq_implicit = _, hsq_explicit = tvs })
  = do { tvs' <- mapM renameLTyVarBndr tvs
       ; return (HsQTvs { hsq_implicit = error "haddock:renameLHsQTyVars", hsq_explicit = tvs', hsq_dependent = error "haddock:renameLHsQTyVars" }) }
                -- This is rather bogus, but I'm not sure what else to do

renameLTyVarBndr :: LHsTyVarBndr GhcRn -> RnM (LHsTyVarBndr DocNameI)
renameLTyVarBndr (L loc (UserTyVar (L l n)))
  = do { n' <- rename n
       ; return (L loc (UserTyVar (L l n'))) }
renameLTyVarBndr (L loc (KindedTyVar (L lv n) kind))
  = do { n' <- rename n
       ; kind' <- renameLKind kind
       ; return (L loc (KindedTyVar (L lv n') kind')) }

renameLContext :: Located [LHsType GhcRn] -> RnM (Located [LHsType DocNameI])
renameLContext (L loc context) = do
  context' <- mapM renameLType context
  return (L loc context')

renameWildCardInfo :: HsWildCardInfo GhcRn -> RnM (HsWildCardInfo DocNameI)
renameWildCardInfo (AnonWildCard  (L l name)) = AnonWildCard . L l <$> rename name

renameInstHead :: InstHead GhcRn -> RnM (InstHead DocNameI)
renameInstHead InstHead {..} = do
  cname <- rename ihdClsName
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
    , ihdTypes = types
    , ihdInstType = itype
    }

renameLDecl :: LHsDecl GhcRn -> RnM (LHsDecl DocNameI)
renameLDecl (L loc d) = return . L loc =<< renameDecl d

renamePats :: [(HsDecl GhcRn, DocForDecl Name)] -> RnM [(HsDecl DocNameI, DocForDecl DocName)]
renamePats = mapM
  (\(d,doc) -> do { d'   <- renameDecl d
                  ; doc' <- renameDocForDecl doc
                  ; return (d',doc')})

renameDecl :: HsDecl GhcRn -> RnM (HsDecl DocNameI)
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
  DerivD d -> do
    d' <- renameDerivD d
    return (DerivD d')
  _ -> error "renameDecl"

renameLThing :: (a GhcRn -> RnM (a DocNameI)) -> Located (a GhcRn) -> RnM (Located (a DocNameI))
renameLThing fn (L loc x) = return . L loc =<< fn x

renameTyClD :: TyClDecl GhcRn -> RnM (TyClDecl DocNameI)
renameTyClD d = case d of
--  TyFamily flav lname ltyvars kind tckind -> do
  FamDecl { tcdFam = decl } -> do
    decl' <- renameFamilyDecl decl
    return (FamDecl { tcdFam = decl' })

  SynDecl { tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdRhs = rhs, tcdFVs = _fvs } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    rhs'     <- renameLType rhs
    return (SynDecl { tcdLName = lname', tcdTyVars = tyvars', tcdFixity = fixity, tcdRhs = rhs', tcdFVs = placeHolderNames })

  DataDecl { tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdDataDefn = defn, tcdFVs = _fvs } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    defn'     <- renameDataDefn defn
    return (DataDecl { tcdLName = lname', tcdTyVars = tyvars', tcdFixity = fixity, tcdDataDefn = defn', tcdDataCusk = PlaceHolder, tcdFVs = placeHolderNames })

  ClassDecl { tcdCtxt = lcontext, tcdLName = lname, tcdTyVars = ltyvars, tcdFixity = fixity
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
                      , tcdFixity = fixity
                      , tcdFDs = lfundeps', tcdSigs = lsigs', tcdMeths= emptyBag
                      , tcdATs = ats', tcdATDefs = at_defs', tcdDocs = [], tcdFVs = placeHolderNames })

  where
    renameLFunDep (L loc (xs, ys)) = do
      xs' <- mapM rename (map unLoc xs)
      ys' <- mapM rename (map unLoc ys)
      return (L loc (map noLoc xs', map noLoc ys'))

    renameLSig (L loc sig) = return . L loc =<< renameSig sig

renameFamilyDecl :: FamilyDecl GhcRn -> RnM (FamilyDecl DocNameI)
renameFamilyDecl (FamilyDecl { fdInfo = info, fdLName = lname
                             , fdTyVars = ltyvars
                             , fdFixity = fixity
                             , fdResultSig = result
                             , fdInjectivityAnn = injectivity }) = do
    info'        <- renameFamilyInfo info
    lname'       <- renameL lname
    ltyvars'     <- renameLHsQTyVars ltyvars
    result'      <- renameFamilyResultSig result
    injectivity' <- renameMaybeInjectivityAnn injectivity
    return (FamilyDecl { fdInfo = info', fdLName = lname'
                       , fdTyVars = ltyvars'
                       , fdFixity = fixity
                       , fdResultSig = result'
                       , fdInjectivityAnn = injectivity' })


renamePseudoFamilyDecl :: PseudoFamilyDecl GhcRn
                       -> RnM (PseudoFamilyDecl DocNameI)
renamePseudoFamilyDecl (PseudoFamilyDecl { .. }) =  PseudoFamilyDecl
    <$> renameFamilyInfo pfdInfo
    <*> renameL pfdLName
    <*> mapM renameLType pfdTyVars
    <*> renameFamilyResultSig pfdKindSig


renameFamilyInfo :: FamilyInfo GhcRn -> RnM (FamilyInfo DocNameI)
renameFamilyInfo DataFamily     = return DataFamily
renameFamilyInfo OpenTypeFamily = return OpenTypeFamily
renameFamilyInfo (ClosedTypeFamily eqns)
  = do { eqns' <- mapM (mapM (mapM renameTyFamInstEqn)) eqns
       ; return $ ClosedTypeFamily eqns' }

renameDataDefn :: HsDataDefn GhcRn -> RnM (HsDataDefn DocNameI)
renameDataDefn (HsDataDefn { dd_ND = nd, dd_ctxt = lcontext, dd_cType = cType
                           , dd_kindSig = k, dd_cons = cons }) = do
    lcontext' <- renameLContext lcontext
    k'        <- renameMaybeLKind k
    cons'     <- mapM (mapM renameCon) cons
    -- I don't think we need the derivings, so we return Nothing
    return (HsDataDefn { dd_ND = nd, dd_ctxt = lcontext', dd_cType = cType
                       , dd_kindSig = k', dd_cons = cons'
                       , dd_derivs = noLoc [] })

renameCon :: ConDecl GhcRn -> RnM (ConDecl DocNameI)
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

renameConDeclFieldField :: LConDeclField GhcRn -> RnM (LConDeclField DocNameI)
renameConDeclFieldField (L l (ConDeclField names t doc)) = do
  names' <- mapM renameLFieldOcc names
  t'   <- renameLType t
  doc' <- mapM renameLDocHsSyn doc
  return $ L l (ConDeclField names' t' doc')

renameLFieldOcc :: LFieldOcc GhcRn -> RnM (LFieldOcc DocNameI)
renameLFieldOcc (L l (FieldOcc lbl sel)) = do
  sel' <- rename sel
  return $ L l (FieldOcc lbl sel')

renameSig :: Sig GhcRn -> RnM (Sig DocNameI)
renameSig sig = case sig of
  TypeSig lnames ltype -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigWcType ltype
    return (TypeSig lnames' ltype')
  ClassOpSig is_default lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigType sig_ty
    return (ClassOpSig is_default lnames' ltype')
  PatSynSig lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    sig_ty' <- renameLSigType sig_ty
    return $ PatSynSig lnames' sig_ty'
  FixSig (FixitySig lnames fixity) -> do
    lnames' <- mapM renameL lnames
    return $ FixSig (FixitySig lnames' fixity)
  MinimalSig src (L l s) -> do
    s' <- traverse renameL s
    return $ MinimalSig src (L l s')
  -- we have filtered out all other kinds of signatures in Interface.Create
  _ -> error "expected TypeSig"


renameForD :: ForeignDecl GhcRn -> RnM (ForeignDecl DocNameI)
renameForD (ForeignImport lname ltype co x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignImport lname' ltype' co x)
renameForD (ForeignExport lname ltype co x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignExport lname' ltype' co x)


renameInstD :: InstDecl GhcRn -> RnM (InstDecl DocNameI)
renameInstD (ClsInstD { cid_inst = d }) = do
  d' <- renameClsInstD d
  return (ClsInstD { cid_inst = d' })
renameInstD (TyFamInstD { tfid_inst = d }) = do
  d' <- renameTyFamInstD d
  return (TyFamInstD { tfid_inst = d' })
renameInstD (DataFamInstD { dfid_inst = d }) = do
  d' <- renameDataFamInstD d
  return (DataFamInstD { dfid_inst = d' })

renameDerivD :: DerivDecl GhcRn -> RnM (DerivDecl DocNameI)
renameDerivD (DerivDecl { deriv_type = ty
                        , deriv_strategy = strat
                        , deriv_overlap_mode = omode }) = do
  ty' <- renameLSigType ty
  return (DerivDecl { deriv_type = ty'
                    , deriv_strategy = strat
                    , deriv_overlap_mode = omode })

renameClsInstD :: ClsInstDecl GhcRn -> RnM (ClsInstDecl DocNameI)
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


renameTyFamInstD :: TyFamInstDecl GhcRn -> RnM (TyFamInstDecl DocNameI)
renameTyFamInstD (TyFamInstDecl { tfid_eqn = eqn })
  = do { eqn' <- renameTyFamInstEqn eqn
       ; return (TyFamInstDecl { tfid_eqn = eqn' }) }

renameTyFamInstEqn :: TyFamInstEqn GhcRn -> RnM (TyFamInstEqn DocNameI)
renameTyFamInstEqn eqn
  = renameImplicit rename_ty_fam_eqn eqn
  where
    rename_ty_fam_eqn
      :: FamEqn GhcRn (HsTyPats GhcRn) (LHsType GhcRn)
      -> RnM (FamEqn DocNameI (HsTyPats DocNameI) (LHsType DocNameI))
    rename_ty_fam_eqn (FamEqn { feqn_tycon = tc, feqn_pats = pats
                              , feqn_fixity = fixity, feqn_rhs = rhs })
      = do { tc' <- renameL tc
           ; pats' <- mapM renameLType pats
           ; rhs' <- renameLType rhs
           ; return (FamEqn { feqn_tycon  = tc'
                            , feqn_pats   = pats'
                            , feqn_fixity = fixity
                            , feqn_rhs    = rhs' }) }

renameLTyFamDefltEqn :: LTyFamDefltEqn GhcRn -> RnM (LTyFamDefltEqn DocNameI)
renameLTyFamDefltEqn (L loc (FamEqn { feqn_tycon = tc, feqn_pats = tvs
                                    , feqn_fixity = fixity, feqn_rhs = rhs }))
  = do { tc'  <- renameL tc
       ; tvs' <- renameLHsQTyVars tvs
       ; rhs' <- renameLType rhs
       ; return (L loc (FamEqn { feqn_tycon  = tc'
                               , feqn_pats   = tvs'
                               , feqn_fixity = fixity
                               , feqn_rhs    = rhs' })) }

renameDataFamInstD :: DataFamInstDecl GhcRn -> RnM (DataFamInstDecl DocNameI)
renameDataFamInstD (DataFamInstDecl { dfid_eqn = eqn })
  = do { eqn' <- renameImplicit rename_data_fam_eqn eqn
       ; return (DataFamInstDecl { dfid_eqn = eqn' }) }
  where
    rename_data_fam_eqn
      :: FamEqn GhcRn (HsTyPats GhcRn) (HsDataDefn GhcRn)
      -> RnM (FamEqn DocNameI (HsTyPats DocNameI) (HsDataDefn DocNameI))
    rename_data_fam_eqn (FamEqn { feqn_tycon = tc, feqn_pats = pats
                                , feqn_fixity = fixity, feqn_rhs = defn })
      = do { tc' <- renameL tc
           ; pats' <- mapM renameLType pats
           ; defn' <- renameDataDefn defn
           ; return (FamEqn { feqn_tycon  = tc'
                            , feqn_pats   = pats'
                            , feqn_fixity = fixity
                            , feqn_rhs    = defn' }) }

renameImplicit :: (in_thing -> RnM out_thing)
               -> HsImplicitBndrs GhcRn in_thing
               -> RnM (HsImplicitBndrs DocNameI out_thing)
renameImplicit rn_thing (HsIB { hsib_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsIB { hsib_body = thing'
                      , hsib_vars = PlaceHolder
                      , hsib_closed = PlaceHolder }) }

renameWc :: (in_thing -> RnM out_thing)
         -> HsWildCardBndrs GhcRn in_thing
         -> RnM (HsWildCardBndrs DocNameI out_thing)
renameWc rn_thing (HsWC { hswc_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsWC { hswc_body = thing'
                      , hswc_wcs = PlaceHolder }) }

renameDocInstance :: DocInstance GhcRn -> RnM (DocInstance DocNameI)
renameDocInstance (inst, idoc, L l n, m) = do
  inst' <- renameInstHead inst
  n' <- rename n
  idoc' <- mapM renameDoc idoc
  return (inst', idoc', L l n', m)

renameExportItem :: ExportItem GhcRn -> RnM (ExportItem DocNameI)
renameExportItem item = case item of
  ExportModule mdl -> return (ExportModule mdl)
  ExportGroup lev id_ doc -> do
    doc' <- renameDoc doc
    return (ExportGroup lev id_ doc')
  ExportDecl decl pats doc subs instances fixities splice -> do
    decl' <- renameLDecl decl
    pats' <- renamePats pats
    doc'  <- renameDocForDecl doc
    subs' <- mapM renameSub subs
    instances' <- forM instances renameDocInstance
    fixities' <- forM fixities $ \(name, fixity) -> do
      name' <- lookupRn name
      return (name', fixity)
    return (ExportDecl decl' pats' doc' subs' instances' fixities' splice)
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
