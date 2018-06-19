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
import Outputable ( panic )
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
renameFamilyResultSig (L loc (NoSig _))
    = return (L loc (NoSig noExt))
renameFamilyResultSig (L loc (KindSig _ ki))
    = do { ki' <- renameLKind ki
         ; return (L loc (KindSig noExt ki')) }
renameFamilyResultSig (L loc (TyVarSig _ bndr))
    = do { bndr' <- renameLTyVarBndr bndr
         ; return (L loc (TyVarSig noExt bndr')) }
renameFamilyResultSig (L _ (XFamilyResultSig _)) = panic "haddock:renameFamilyResultSig"

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
    return (HsForAllTy { hst_xforall = NoExt, hst_bndrs = tyvars', hst_body = ltype' })

  HsQualTy { hst_ctxt = lcontext , hst_body = ltype } -> do
    lcontext' <- renameLContext lcontext
    ltype'    <- renameLType ltype
    return (HsQualTy { hst_xqual = NoExt, hst_ctxt = lcontext', hst_body = ltype' })

  HsTyVar _ ip (L l n) -> return . HsTyVar NoExt ip . L l =<< rename n
  HsBangTy _ b ltype -> return . HsBangTy NoExt b =<< renameLType ltype

  HsStarTy _ isUni -> return (HsStarTy NoExt isUni)

  HsAppTy _ a b -> do
    a' <- renameLType a
    b' <- renameLType b
    return (HsAppTy NoExt a' b')

  HsFunTy _ a b -> do
    a' <- renameLType a
    b' <- renameLType b
    return (HsFunTy NoExt a' b')

  HsListTy _ ty -> return . (HsListTy NoExt) =<< renameLType ty
  HsIParamTy _ n ty -> liftM (HsIParamTy NoExt n) (renameLType ty)

  HsTupleTy _ b ts -> return . HsTupleTy NoExt b =<< mapM renameLType ts
  HsSumTy _ ts -> HsSumTy NoExt <$> mapM renameLType ts

  HsOpTy _ a (L loc op) b -> do
    op' <- rename op
    a'  <- renameLType a
    b'  <- renameLType b
    return (HsOpTy NoExt a' (L loc op') b')

  HsParTy _ ty -> return . (HsParTy NoExt) =<< renameLType ty

  HsKindSig _ ty k -> do
    ty' <- renameLType ty
    k' <- renameLKind k
    return (HsKindSig NoExt ty' k')

  HsDocTy _ ty doc -> do
    ty' <- renameLType ty
    doc' <- renameLDocHsSyn doc
    return (HsDocTy NoExt ty' doc')

  HsTyLit _ x -> return (HsTyLit NoExt x)

  HsRecTy _ a               -> HsRecTy NoExt <$> mapM renameConDeclFieldField a
  (XHsType (NHsCoreTy a))   -> pure (XHsType (NHsCoreTy a))
  HsExplicitListTy i a b  -> HsExplicitListTy i a <$> mapM renameLType b
  HsExplicitTupleTy a b   -> HsExplicitTupleTy a <$> mapM renameLType b
  HsSpliceTy _ s          -> renameHsSpliceTy s
  HsWildCardTy a          -> HsWildCardTy <$> renameWildCardInfo a

-- | Rename splices, but _only_ those that turn out to be for types.
-- I think this is actually safe for our possible inputs:
--
--  * the input is from after GHC's renamer, so should have an 'HsSpliced'
--  * the input is typechecked, and only 'HsSplicedTy' should get through that
--
renameHsSpliceTy :: HsSplice GhcRn -> RnM (HsType DocNameI)
renameHsSpliceTy (HsSpliced _ _ (HsSplicedTy t)) = renameType t
renameHsSpliceTy (HsSpliced _ _ _) = error "renameHsSpliceTy: not an HsSplicedTy"
renameHsSpliceTy _ = error "renameHsSpliceTy: not an HsSpliced"

renameLHsQTyVars :: LHsQTyVars GhcRn -> RnM (LHsQTyVars DocNameI)
renameLHsQTyVars (HsQTvs { hsq_explicit = tvs })
  = do { tvs' <- mapM renameLTyVarBndr tvs
       ; return (HsQTvs { hsq_ext = noExt
                        , hsq_explicit = tvs' }) }
renameLHsQTyVars (XLHsQTyVars _) = panic "haddock:renameLHsQTyVars"

renameLTyVarBndr :: LHsTyVarBndr GhcRn -> RnM (LHsTyVarBndr DocNameI)
renameLTyVarBndr (L loc (UserTyVar x (L l n)))
  = do { n' <- rename n
       ; return (L loc (UserTyVar x (L l n'))) }
renameLTyVarBndr (L loc (KindedTyVar x (L lv n) kind))
  = do { n' <- rename n
       ; kind' <- renameLKind kind
       ; return (L loc (KindedTyVar x (L lv n') kind')) }
renameLTyVarBndr (L _ (XTyVarBndr _ )) = error "haddock:renameLTyVarBndr"

renameLContext :: Located [LHsType GhcRn] -> RnM (Located [LHsType DocNameI])
renameLContext (L loc context) = do
  context' <- mapM renameLType context
  return (L loc context')

renameWildCardInfo :: HsWildCardInfo -> RnM HsWildCardInfo
renameWildCardInfo (AnonWildCard  (L l name)) = return (AnonWildCard (L l name))

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
  TyClD _ d -> do
    d' <- renameTyClD d
    return (TyClD noExt d')
  SigD _ s -> do
    s' <- renameSig s
    return (SigD noExt s')
  ForD _ d -> do
    d' <- renameForD d
    return (ForD noExt d')
  InstD _ d -> do
    d' <- renameInstD d
    return (InstD noExt d')
  DerivD _ d -> do
    d' <- renameDerivD d
    return (DerivD noExt d')
  _ -> error "renameDecl"

renameLThing :: (a GhcRn -> RnM (a DocNameI)) -> Located (a GhcRn) -> RnM (Located (a DocNameI))
renameLThing fn (L loc x) = return . L loc =<< fn x

renameTyClD :: TyClDecl GhcRn -> RnM (TyClDecl DocNameI)
renameTyClD d = case d of
--  TyFamily flav lname ltyvars kind tckind -> do
  FamDecl { tcdFam = decl } -> do
    decl' <- renameFamilyDecl decl
    return (FamDecl { tcdFExt = noExt, tcdFam = decl' })

  SynDecl { tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdRhs = rhs } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    rhs'     <- renameLType rhs
    return (SynDecl { tcdSExt = noExt, tcdLName = lname', tcdTyVars = tyvars'
                    , tcdFixity = fixity, tcdRhs = rhs' })

  DataDecl { tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdDataDefn = defn } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    defn'     <- renameDataDefn defn
    return (DataDecl { tcdDExt = noExt, tcdLName = lname', tcdTyVars = tyvars'
                     , tcdFixity = fixity, tcdDataDefn = defn' })

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
                      , tcdATs = ats', tcdATDefs = at_defs', tcdDocs = [], tcdCExt = NoExt })
  XTyClDecl _ -> panic "haddock:renameTyClD"

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
    return (FamilyDecl { fdExt = noExt, fdInfo = info', fdLName = lname'
                       , fdTyVars = ltyvars'
                       , fdFixity = fixity
                       , fdResultSig = result'
                       , fdInjectivityAnn = injectivity' })
renameFamilyDecl (XFamilyDecl _) = panic "renameFamilyDecl"


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
    return (HsDataDefn { dd_ext = noExt
                       , dd_ND = nd, dd_ctxt = lcontext', dd_cType = cType
                       , dd_kindSig = k', dd_cons = cons'
                       , dd_derivs = noLoc [] })
renameDataDefn (XHsDataDefn _) = panic "haddock:renameDataDefn"

renameCon :: ConDecl GhcRn -> RnM (ConDecl DocNameI)
renameCon decl@(ConDeclH98 { con_name = lname, con_ex_tvs = ltyvars
                           , con_mb_cxt = lcontext, con_args = details
                           , con_doc = mbldoc }) = do
      lname'    <- renameL lname
      ltyvars'  <- mapM renameLTyVarBndr ltyvars
      lcontext' <- traverse renameLContext lcontext
      details'  <- renameDetails details
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_ext = noExt, con_name = lname', con_ex_tvs = ltyvars'
                   , con_mb_cxt = lcontext'
                   , con_args = details', con_doc = mbldoc' })

renameCon decl@(ConDeclGADT { con_names = lnames, con_qvars = ltyvars
                            , con_mb_cxt = lcontext, con_args = details
                            , con_res_ty = res_ty
                            , con_doc = mbldoc }) = do
      lnames'   <- mapM renameL lnames
      ltyvars'  <- renameLHsQTyVars ltyvars
      lcontext' <- traverse renameLContext lcontext
      details'  <- renameDetails details
      res_ty'   <- renameLType res_ty
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_g_ext = noExt, con_names = lnames', con_qvars = ltyvars'
                   , con_mb_cxt = lcontext', con_args = details'
                   , con_res_ty = res_ty', con_doc = mbldoc' })
renameCon (XConDecl _) = panic "haddock:renameCon"

renameDetails :: HsConDeclDetails GhcRn -> RnM (HsConDeclDetails DocNameI)
renameDetails (RecCon (L l fields)) = do
  fields' <- mapM renameConDeclFieldField fields
  return (RecCon (L l fields'))
renameDetails (PrefixCon ps) = return . PrefixCon =<< mapM renameLType ps
renameDetails (InfixCon a b) = do
  a' <- renameLType a
  b' <- renameLType b
  return (InfixCon a' b')

renameConDeclFieldField :: LConDeclField GhcRn -> RnM (LConDeclField DocNameI)
renameConDeclFieldField (L l (ConDeclField _ names t doc)) = do
  names' <- mapM renameLFieldOcc names
  t'   <- renameLType t
  doc' <- mapM renameLDocHsSyn doc
  return $ L l (ConDeclField noExt names' t' doc')
renameConDeclFieldField (L _ (XConDeclField _)) = panic "haddock:renameConDeclFieldField"

renameLFieldOcc :: LFieldOcc GhcRn -> RnM (LFieldOcc DocNameI)
renameLFieldOcc (L l (FieldOcc sel lbl)) = do
  sel' <- rename sel
  return $ L l (FieldOcc sel' lbl)
renameLFieldOcc (L _ (XFieldOcc _)) = error "haddock:renameLFieldOcc"

renameSig :: Sig GhcRn -> RnM (Sig DocNameI)
renameSig sig = case sig of
  TypeSig _ lnames ltype -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigWcType ltype
    return (TypeSig noExt lnames' ltype')
  ClassOpSig _ is_default lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigType sig_ty
    return (ClassOpSig noExt is_default lnames' ltype')
  PatSynSig _ lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    sig_ty' <- renameLSigType sig_ty
    return $ PatSynSig noExt lnames' sig_ty'
  FixSig _ (FixitySig _ lnames fixity) -> do
    lnames' <- mapM renameL lnames
    return $ FixSig noExt (FixitySig noExt lnames' fixity)
  MinimalSig _ src (L l s) -> do
    s' <- traverse renameL s
    return $ MinimalSig noExt src (L l s')
  -- we have filtered out all other kinds of signatures in Interface.Create
  _ -> error "expected TypeSig"


renameForD :: ForeignDecl GhcRn -> RnM (ForeignDecl DocNameI)
renameForD (ForeignImport _ lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignImport noExt lname' ltype' x)
renameForD (ForeignExport _ lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignExport noExt lname' ltype' x)
renameForD (XForeignDecl _) = panic "haddock:renameForD"


renameInstD :: InstDecl GhcRn -> RnM (InstDecl DocNameI)
renameInstD (ClsInstD { cid_inst = d }) = do
  d' <- renameClsInstD d
  return (ClsInstD { cid_d_ext = noExt, cid_inst = d' })
renameInstD (TyFamInstD { tfid_inst = d }) = do
  d' <- renameTyFamInstD d
  return (TyFamInstD { tfid_ext = noExt, tfid_inst = d' })
renameInstD (DataFamInstD { dfid_inst = d }) = do
  d' <- renameDataFamInstD d
  return (DataFamInstD { dfid_ext = noExt, dfid_inst = d' })
renameInstD (XInstDecl _) = panic "haddock:renameInstD"

renameDerivD :: DerivDecl GhcRn -> RnM (DerivDecl DocNameI)
renameDerivD (DerivDecl { deriv_type = ty
                        , deriv_strategy = strat
                        , deriv_overlap_mode = omode }) = do
  ty'    <- renameLSigWcType ty
  strat' <- mapM (mapM renameDerivStrategy) strat
  return (DerivDecl { deriv_ext = noExt
                    , deriv_type = ty'
                    , deriv_strategy = strat'
                    , deriv_overlap_mode = omode })
renameDerivD (XDerivDecl _) = panic "haddock:renameDerivD"

renameDerivStrategy :: DerivStrategy GhcRn -> RnM (DerivStrategy DocNameI)
renameDerivStrategy StockStrategy    = pure StockStrategy
renameDerivStrategy AnyclassStrategy = pure AnyclassStrategy
renameDerivStrategy NewtypeStrategy  = pure NewtypeStrategy
renameDerivStrategy (ViaStrategy ty) = ViaStrategy <$> renameLSigType ty

renameClsInstD :: ClsInstDecl GhcRn -> RnM (ClsInstDecl DocNameI)
renameClsInstD (ClsInstDecl { cid_overlap_mode = omode
                            , cid_poly_ty =ltype, cid_tyfam_insts = lATs
                            , cid_datafam_insts = lADTs }) = do
  ltype' <- renameLSigType ltype
  lATs'  <- mapM (mapM renameTyFamInstD) lATs
  lADTs' <- mapM (mapM renameDataFamInstD) lADTs
  return (ClsInstDecl { cid_ext = noExt, cid_overlap_mode = omode
                      , cid_poly_ty = ltype', cid_binds = emptyBag
                      , cid_sigs = []
                      , cid_tyfam_insts = lATs', cid_datafam_insts = lADTs' })
renameClsInstD (XClsInstDecl _) = panic "haddock:renameClsInstD"


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
           ; return (FamEqn { feqn_ext    = noExt
                            , feqn_tycon  = tc'
                            , feqn_pats   = pats'
                            , feqn_fixity = fixity
                            , feqn_rhs    = rhs' }) }
    rename_ty_fam_eqn (XFamEqn _) = panic "haddock:renameTyFamInstEqn"

renameLTyFamDefltEqn :: LTyFamDefltEqn GhcRn -> RnM (LTyFamDefltEqn DocNameI)
renameLTyFamDefltEqn (L loc (FamEqn { feqn_tycon = tc, feqn_pats = tvs
                                    , feqn_fixity = fixity, feqn_rhs = rhs }))
  = do { tc'  <- renameL tc
       ; tvs' <- renameLHsQTyVars tvs
       ; rhs' <- renameLType rhs
       ; return (L loc (FamEqn { feqn_ext    = noExt
                               , feqn_tycon  = tc'
                               , feqn_pats   = tvs'
                               , feqn_fixity = fixity
                               , feqn_rhs    = rhs' })) }
renameLTyFamDefltEqn (L _ (XFamEqn _)) = panic "haddock:renameLTyFamDefltEqn"

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
           ; return (FamEqn { feqn_ext    = noExt
                            , feqn_tycon  = tc'
                            , feqn_pats   = pats'
                            , feqn_fixity = fixity
                            , feqn_rhs    = defn' }) }
    rename_data_fam_eqn (XFamEqn _) = panic "haddock:renameDataFamInstD"

renameImplicit :: (in_thing -> RnM out_thing)
               -> HsImplicitBndrs GhcRn in_thing
               -> RnM (HsImplicitBndrs DocNameI out_thing)
renameImplicit rn_thing (HsIB { hsib_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsIB { hsib_body = thing'
                      , hsib_ext = noExt }) }
renameImplicit _ (XHsImplicitBndrs _) = panic "haddock:renameImplicit"

renameWc :: (in_thing -> RnM out_thing)
         -> HsWildCardBndrs GhcRn in_thing
         -> RnM (HsWildCardBndrs DocNameI out_thing)
renameWc rn_thing (HsWC { hswc_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsWC { hswc_body = thing'
                      , hswc_ext = noExt }) }
renameWc _ (XHsWildCardBndrs _) = panic "haddock:renameWc"

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
