{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
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

import GHC.Data.Bag (emptyBag)
import GHC hiding (NoLink)
import GHC.Types.Name
import GHC.Types.Name.Reader (RdrName(Exact))
import GHC.Builtin.Types (eqTyCon_RDR)

import Control.Applicative
import Control.Arrow ( first )
import Control.Monad hiding (mapM)
import Data.List (intercalate)
import qualified Data.Map as Map hiding ( Map )
import qualified Data.Set as Set
import Prelude hiding (mapM)
import GHC.HsToCore.Docs

-- | Traverse docstrings and ASTs in the Haddock interface, renaming 'Name' to
-- 'DocName'.
--
-- What this really boils down to is: for each 'Name', figure out which of the
-- modules that export the name is the preferred place to link to.
--
-- The renamed output gets written into fields in the Haddock interface record
-- that were previously left empty.
renameInterface :: DynFlags -> [String] -> LinkEnv -> Bool -> Interface -> ErrMsgM Interface
renameInterface _dflags ignoredSymbols renamingEnv warnings iface =

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

      qualifiedName n = (moduleNameString $ moduleName $ nameModule n) <> "." <> getOccString n

      ignoreSet = Set.fromList ignoredSymbols

      strings = [ qualifiedName n

                | n <- missingNames
                , not (qualifiedName n `Set.member` ignoreSet)
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
            intercalate "\n\t- "  ("" : strings) ]

    return $ iface { ifaceRnDoc         = finalModuleDoc,
                     ifaceRnDocMap      = rnDocMap,
                     ifaceRnArgMap      = rnArgMap,
                     ifaceRnExportItems = renamedExportItems,
                     ifaceRnOrphanInstances = renamedOrphanInstances}


--------------------------------------------------------------------------------
-- Monad for renaming
--------------------------------------------------------------------------------


-- | The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in
-- the environment.
newtype RnM a =
  RnM { unRn :: (Name -> (Bool, DocName))
                -- Name lookup function. The 'Bool' indicates that if the name
                -- was \"found\" in the environment.

             -> (a, [Name] -> [Name])
                -- Value returned, as well as a difference list of the names not
                -- found
      }

instance Monad RnM where
  m >>= k = RnM $ \lkp -> let (a, out1) = unRn m lkp
                              (b, out2) = unRn (k a) lkp
                          in (b, out1 . out2)

instance Functor RnM where
  fmap f (RnM lkp) = RnM (first f . lkp)

instance Applicative RnM where
  pure a = RnM (const (a, id))
  mf <*> mx = RnM $ \lkp -> let (f, out1) = unRn mf lkp
                                (x, out2) = unRn mx lkp
                            in (f x, out1 . out2)

-- | Look up a 'Name' in the renaming environment.
lookupRn :: Name -> RnM DocName
lookupRn name = RnM $ \lkp ->
  case lkp name of
    (False,maps_to) -> (maps_to, (name :))
    (True, maps_to) -> (maps_to, id)

-- | Look up a 'Name' in the renaming environment, but don't warn if you don't
-- find the name. Prefer to use 'lookupRn' whenever possible.
lookupRnNoWarn :: Name -> RnM DocName
lookupRnNoWarn name = RnM $ \lkp -> (snd (lkp name), id)

-- | Run the renamer action using lookup in a 'LinkEnv' as the lookup function.
-- Returns the renamed value along with a list of `Name`'s that could not be
-- renamed because they weren't in the environment.
runRnFM :: LinkEnv -> RnM a -> (a, [Name])
runRnFM env rn = let (x, dlist) = unRn rn lkp in (x, dlist [])
  where
    lkp n | isTyVarName n = (True, Undocumented n)
          | otherwise = case Map.lookup n env of
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


renameDoc :: Traversable t => t (Wrap Name) -> RnM (t (Wrap DocName))
renameDoc = traverse (traverse rename)

renameFnArgsDoc :: FnArgsDoc Name -> RnM (FnArgsDoc DocName)
renameFnArgsDoc = mapM renameDoc


renameLType :: LHsType GhcRn -> RnM (LHsType DocNameI)
renameLType = mapM renameType

renameLTypeArg :: LHsTypeArg GhcRn -> RnM (LHsTypeArg DocNameI)
renameLTypeArg (HsValArg ty) = do { ty' <- renameLType ty
                                     ; return $ HsValArg ty' }
renameLTypeArg (HsTypeArg l ki) = do { ki' <- renameLKind ki
                                     ; return $ HsTypeArg l ki' }
renameLTypeArg (HsArgPar sp) = return $ HsArgPar sp

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
    = return (L loc (NoSig noExtField))
renameFamilyResultSig (L loc (KindSig _ ki))
    = do { ki' <- renameLKind ki
         ; return (L loc (KindSig noExtField ki')) }
renameFamilyResultSig (L loc (TyVarSig _ bndr))
    = do { bndr' <- renameLTyVarBndr bndr
         ; return (L loc (TyVarSig noExtField bndr')) }

renameInjectivityAnn :: LInjectivityAnn GhcRn -> RnM (LInjectivityAnn DocNameI)
renameInjectivityAnn (L loc (InjectivityAnn lhs rhs))
    = do { lhs' <- renameL lhs
         ; rhs' <- mapM renameL rhs
         ; return (L loc (InjectivityAnn lhs' rhs')) }

renameMaybeInjectivityAnn :: Maybe (LInjectivityAnn GhcRn)
                          -> RnM (Maybe (LInjectivityAnn DocNameI))
renameMaybeInjectivityAnn = traverse renameInjectivityAnn

renameArrow :: HsArrow GhcRn -> RnM (HsArrow DocNameI)
renameArrow (HsUnrestrictedArrow u) = return (HsUnrestrictedArrow u)
renameArrow (HsLinearArrow u) = return (HsLinearArrow u)
renameArrow (HsExplicitMult u p) = HsExplicitMult u <$> renameLType p

renameType :: HsType GhcRn -> RnM (HsType DocNameI)
renameType t = case t of
  HsForAllTy { hst_tele = tele, hst_body = ltype } -> do
    tele'  <- renameHsForAllTelescope tele
    ltype' <- renameLType ltype
    return (HsForAllTy { hst_xforall = noExtField
                       , hst_tele = tele', hst_body = ltype' })

  HsQualTy { hst_ctxt = lcontext , hst_body = ltype } -> do
    lcontext' <- renameLContext lcontext
    ltype'    <- renameLType ltype
    return (HsQualTy { hst_xqual = noExtField, hst_ctxt = lcontext', hst_body = ltype' })

  HsTyVar _ ip (L l n) -> return . HsTyVar noExtField ip . L l =<< rename n
  HsBangTy _ b ltype -> return . HsBangTy noExtField b =<< renameLType ltype

  HsStarTy _ isUni -> return (HsStarTy noExtField isUni)

  HsAppTy _ a b -> do
    a' <- renameLType a
    b' <- renameLType b
    return (HsAppTy noExtField a' b')

  HsAppKindTy _ a b -> do
    a' <- renameLType a
    b' <- renameLKind b
    return (HsAppKindTy noExtField a' b')

  HsFunTy _ w a b -> do
    a' <- renameLType a
    b' <- renameLType b
    w' <- renameArrow w
    return (HsFunTy noExtField w' a' b')

  HsListTy _ ty -> return . (HsListTy noExtField) =<< renameLType ty
  HsIParamTy _ n ty -> liftM (HsIParamTy noExtField n) (renameLType ty)

  HsTupleTy _ b ts -> return . HsTupleTy noExtField b =<< mapM renameLType ts
  HsSumTy _ ts -> HsSumTy noExtField <$> mapM renameLType ts

  HsOpTy _ a (L loc op) b -> do
    op' <- rename op
    a'  <- renameLType a
    b'  <- renameLType b
    return (HsOpTy noExtField a' (L loc op') b')

  HsParTy _ ty -> return . (HsParTy noExtField) =<< renameLType ty

  HsKindSig _ ty k -> do
    ty' <- renameLType ty
    k' <- renameLKind k
    return (HsKindSig noExtField ty' k')

  HsDocTy _ ty doc -> do
    ty' <- renameLType ty
    doc' <- renameLDocHsSyn doc
    return (HsDocTy noExtField ty' doc')

  HsTyLit _ x -> return (HsTyLit noExtField x)

  HsRecTy _ a               -> HsRecTy noExtField <$> mapM renameConDeclFieldField a
  (XHsType (NHsCoreTy a))   -> pure (XHsType (NHsCoreTy a))
  HsExplicitListTy i a b  -> HsExplicitListTy i a <$> mapM renameLType b
  HsExplicitTupleTy a b   -> HsExplicitTupleTy a <$> mapM renameLType b
  HsSpliceTy _ s          -> renameHsSpliceTy s
  HsWildCardTy a          -> pure (HsWildCardTy a)

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
       ; return (HsQTvs { hsq_ext = noExtField
                        , hsq_explicit = tvs' }) }

renameHsForAllTelescope :: HsForAllTelescope GhcRn -> RnM (HsForAllTelescope DocNameI)
renameHsForAllTelescope tele = case tele of
  HsForAllVis   x bndrs -> do bndrs' <- mapM renameLTyVarBndr bndrs
                              pure $ HsForAllVis x bndrs'
  HsForAllInvis x bndrs -> do bndrs' <- mapM renameLTyVarBndr bndrs
                              pure $ HsForAllInvis x bndrs'

renameLTyVarBndr :: LHsTyVarBndr flag GhcRn -> RnM (LHsTyVarBndr flag DocNameI)
renameLTyVarBndr (L loc (UserTyVar x fl (L l n)))
  = do { n' <- rename n
       ; return (L loc (UserTyVar x fl (L l n'))) }
renameLTyVarBndr (L loc (KindedTyVar x fl (L lv n) kind))
  = do { n' <- rename n
       ; kind' <- renameLKind kind
       ; return (L loc (KindedTyVar x fl (L lv n') kind')) }

renameLContext :: Located [LHsType GhcRn] -> RnM (Located [LHsType DocNameI])
renameLContext (L loc context) = do
  context' <- mapM renameLType context
  return (L loc context')

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
    return (TyClD noExtField d')
  SigD _ s -> do
    s' <- renameSig s
    return (SigD noExtField s')
  ForD _ d -> do
    d' <- renameForD d
    return (ForD noExtField d')
  InstD _ d -> do
    d' <- renameInstD d
    return (InstD noExtField d')
  DerivD _ d -> do
    d' <- renameDerivD d
    return (DerivD noExtField d')
  _ -> error "renameDecl"

renameLThing :: (a GhcRn -> RnM (a DocNameI)) -> Located (a GhcRn) -> RnM (Located (a DocNameI))
renameLThing fn (L loc x) = return . L loc =<< fn x

renameTyClD :: TyClDecl GhcRn -> RnM (TyClDecl DocNameI)
renameTyClD d = case d of
--  TyFamily flav lname ltyvars kind tckind -> do
  FamDecl { tcdFam = decl } -> do
    decl' <- renameFamilyDecl decl
    return (FamDecl { tcdFExt = noExtField, tcdFam = decl' })

  SynDecl { tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdRhs = rhs } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    rhs'     <- renameLType rhs
    return (SynDecl { tcdSExt = noExtField, tcdLName = lname', tcdTyVars = tyvars'
                    , tcdFixity = fixity, tcdRhs = rhs' })

  DataDecl { tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdDataDefn = defn } -> do
    lname'    <- renameL lname
    tyvars'   <- renameLHsQTyVars tyvars
    defn'     <- renameDataDefn defn
    return (DataDecl { tcdDExt = noExtField, tcdLName = lname', tcdTyVars = tyvars'
                     , tcdFixity = fixity, tcdDataDefn = defn' })

  ClassDecl { tcdCtxt = lcontext, tcdLName = lname, tcdTyVars = ltyvars, tcdFixity = fixity
            , tcdFDs = lfundeps, tcdSigs = lsigs, tcdATs = ats, tcdATDefs = at_defs } -> do
    lcontext' <- renameLContext lcontext
    lname'    <- renameL lname
    ltyvars'  <- renameLHsQTyVars ltyvars
    lfundeps' <- mapM renameLFunDep lfundeps
    lsigs'    <- mapM renameLSig lsigs
    ats'      <- mapM (renameLThing renameFamilyDecl) ats
    at_defs'  <- mapM (mapM renameTyFamDefltD) at_defs
    -- we don't need the default methods or the already collected doc entities
    return (ClassDecl { tcdCtxt = lcontext', tcdLName = lname', tcdTyVars = ltyvars'
                      , tcdFixity = fixity
                      , tcdFDs = lfundeps', tcdSigs = lsigs', tcdMeths= emptyBag
                      , tcdATs = ats', tcdATDefs = at_defs', tcdDocs = [], tcdCExt = noExtField })

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
    return (FamilyDecl { fdExt = noExtField, fdInfo = info', fdLName = lname'
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
    return (HsDataDefn { dd_ext = noExtField
                       , dd_ND = nd, dd_ctxt = lcontext', dd_cType = cType
                       , dd_kindSig = k', dd_cons = cons'
                       , dd_derivs = noLoc [] })

renameCon :: ConDecl GhcRn -> RnM (ConDecl DocNameI)
renameCon decl@(ConDeclH98 { con_name = lname, con_ex_tvs = ltyvars
                           , con_mb_cxt = lcontext, con_args = details
                           , con_doc = mbldoc }) = do
      lname'    <- renameL lname
      ltyvars'  <- mapM renameLTyVarBndr ltyvars
      lcontext' <- traverse renameLContext lcontext
      details'  <- renameDetails details
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_ext = noExtField, con_name = lname', con_ex_tvs = ltyvars'
                   , con_mb_cxt = lcontext'
                   , con_args = details', con_doc = mbldoc' })

renameCon decl@(ConDeclGADT { con_names = lnames, con_qvars = ltyvars
                            , con_mb_cxt = lcontext, con_args = details
                            , con_res_ty = res_ty
                            , con_doc = mbldoc }) = do
      lnames'   <- mapM renameL lnames
      ltyvars'  <- mapM renameLTyVarBndr ltyvars
      lcontext' <- traverse renameLContext lcontext
      details'  <- renameDetails details
      res_ty'   <- renameLType res_ty
      mbldoc'   <- mapM renameLDocHsSyn mbldoc
      return (decl { con_g_ext = noExtField, con_names = lnames', con_qvars = ltyvars'
                   , con_mb_cxt = lcontext', con_args = details'
                   , con_res_ty = res_ty', con_doc = mbldoc' })

renameHsScaled :: HsScaled GhcRn (LHsType GhcRn)
               -> RnM (HsScaled DocNameI (LHsType DocNameI))
renameHsScaled (HsScaled w ty) = HsScaled <$> renameArrow w <*> renameLType ty

renameDetails :: HsConDeclDetails GhcRn -> RnM (HsConDeclDetails DocNameI)
renameDetails (RecCon (L l fields)) = do
  fields' <- mapM renameConDeclFieldField fields
  return (RecCon (L l fields'))
                               -- This causes an assertion failure
--renameDetails (PrefixCon ps) = -- return . PrefixCon =<< mapM (_renameLType) ps
renameDetails (PrefixCon ps) = PrefixCon <$> mapM renameHsScaled ps
renameDetails (InfixCon a b) = do
  a' <- renameHsScaled a
  b' <- renameHsScaled b
  return (InfixCon a' b')

renameConDeclFieldField :: LConDeclField GhcRn -> RnM (LConDeclField DocNameI)
renameConDeclFieldField (L l (ConDeclField _ names t doc)) = do
  names' <- mapM renameLFieldOcc names
  t'   <- renameLType t
  doc' <- mapM renameLDocHsSyn doc
  return $ L l (ConDeclField noExtField names' t' doc')

renameLFieldOcc :: LFieldOcc GhcRn -> RnM (LFieldOcc DocNameI)
renameLFieldOcc (L l (FieldOcc sel lbl)) = do
  sel' <- rename sel
  return $ L l (FieldOcc sel' lbl)

renameSig :: Sig GhcRn -> RnM (Sig DocNameI)
renameSig sig = case sig of
  TypeSig _ lnames ltype -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigWcType ltype
    return (TypeSig noExtField lnames' ltype')
  ClassOpSig _ is_default lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    ltype' <- renameLSigType sig_ty
    return (ClassOpSig noExtField is_default lnames' ltype')
  PatSynSig _ lnames sig_ty -> do
    lnames' <- mapM renameL lnames
    sig_ty' <- renameLSigType sig_ty
    return $ PatSynSig noExtField lnames' sig_ty'
  FixSig _ (FixitySig _ lnames fixity) -> do
    lnames' <- mapM renameL lnames
    return $ FixSig noExtField (FixitySig noExtField lnames' fixity)
  MinimalSig _ src (L l s) -> do
    s' <- traverse (traverse lookupRnNoWarn) s
    return $ MinimalSig noExtField src (L l s')
  -- we have filtered out all other kinds of signatures in Interface.Create
  _ -> error "expected TypeSig"


renameForD :: ForeignDecl GhcRn -> RnM (ForeignDecl DocNameI)
renameForD (ForeignImport _ lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignImport noExtField lname' ltype' x)
renameForD (ForeignExport _ lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLSigType ltype
  return (ForeignExport noExtField lname' ltype' x)


renameInstD :: InstDecl GhcRn -> RnM (InstDecl DocNameI)
renameInstD (ClsInstD { cid_inst = d }) = do
  d' <- renameClsInstD d
  return (ClsInstD { cid_d_ext = noExtField, cid_inst = d' })
renameInstD (TyFamInstD { tfid_inst = d }) = do
  d' <- renameTyFamInstD d
  return (TyFamInstD { tfid_ext = noExtField, tfid_inst = d' })
renameInstD (DataFamInstD { dfid_inst = d }) = do
  d' <- renameDataFamInstD d
  return (DataFamInstD { dfid_ext = noExtField, dfid_inst = d' })

renameDerivD :: DerivDecl GhcRn -> RnM (DerivDecl DocNameI)
renameDerivD (DerivDecl { deriv_type = ty
                        , deriv_strategy = strat
                        , deriv_overlap_mode = omode }) = do
  ty'    <- renameLSigWcType ty
  strat' <- mapM (mapM renameDerivStrategy) strat
  return (DerivDecl { deriv_ext = noExtField
                    , deriv_type = ty'
                    , deriv_strategy = strat'
                    , deriv_overlap_mode = omode })

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
  return (ClsInstDecl { cid_ext = noExtField, cid_overlap_mode = omode
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
      :: FamEqn GhcRn (LHsType GhcRn)
      -> RnM (FamEqn DocNameI (LHsType DocNameI))
    rename_ty_fam_eqn (FamEqn { feqn_tycon = tc, feqn_bndrs = bndrs
                              , feqn_pats = pats, feqn_fixity = fixity
                              , feqn_rhs = rhs })
      = do { tc' <- renameL tc
           ; bndrs' <- traverse (mapM renameLTyVarBndr) bndrs
           ; pats' <- mapM renameLTypeArg pats
           ; rhs' <- renameLType rhs
           ; return (FamEqn { feqn_ext    = noExtField
                            , feqn_tycon  = tc'
                            , feqn_bndrs  = bndrs'
                            , feqn_pats   = pats'
                            , feqn_fixity = fixity
                            , feqn_rhs    = rhs' }) }

renameTyFamDefltD :: TyFamDefltDecl GhcRn -> RnM (TyFamDefltDecl DocNameI)
renameTyFamDefltD = renameTyFamInstD

renameDataFamInstD :: DataFamInstDecl GhcRn -> RnM (DataFamInstDecl DocNameI)
renameDataFamInstD (DataFamInstDecl { dfid_eqn = eqn })
  = do { eqn' <- renameImplicit rename_data_fam_eqn eqn
       ; return (DataFamInstDecl { dfid_eqn = eqn' }) }
  where
    rename_data_fam_eqn
      :: FamEqn GhcRn (HsDataDefn GhcRn)
      -> RnM (FamEqn DocNameI (HsDataDefn DocNameI))
    rename_data_fam_eqn (FamEqn { feqn_tycon = tc, feqn_bndrs = bndrs
                                , feqn_pats = pats, feqn_fixity = fixity
                                , feqn_rhs = defn })
      = do { tc' <- renameL tc
           ; bndrs' <- traverse (mapM renameLTyVarBndr) bndrs
           ; pats' <- mapM renameLTypeArg pats
           ; defn' <- renameDataDefn defn
           ; return (FamEqn { feqn_ext    = noExtField
                            , feqn_tycon  = tc'
                            , feqn_bndrs  = bndrs'
                            , feqn_pats   = pats'
                            , feqn_fixity = fixity
                            , feqn_rhs    = defn' }) }

renameImplicit :: (in_thing -> RnM out_thing)
               -> HsImplicitBndrs GhcRn in_thing
               -> RnM (HsImplicitBndrs DocNameI out_thing)
renameImplicit rn_thing (HsIB { hsib_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsIB { hsib_body = thing'
                      , hsib_ext = noExtField }) }

renameWc :: (in_thing -> RnM out_thing)
         -> HsWildCardBndrs GhcRn in_thing
         -> RnM (HsWildCardBndrs DocNameI out_thing)
renameWc rn_thing (HsWC { hswc_body = thing })
  = do { thing' <- rn_thing thing
       ; return (HsWC { hswc_body = thing'
                      , hswc_ext = noExtField }) }

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
