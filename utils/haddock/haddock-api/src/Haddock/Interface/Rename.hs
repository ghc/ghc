{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Interface.Rename
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Interface.Rename (renameInterface) where

import Control.Applicative ()
import Control.DeepSeq (force)
import Control.Monad hiding (mapM)
import Control.Monad.Reader
import Control.Monad.Writer.CPS
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Traversable (mapM)
import GHC hiding (NoLink)
import GHC.Builtin.Types (eqTyCon_RDR, tupleDataConName, tupleTyConName)
import GHC.Types.Basic (Boxity (..), TopLevelFlag (..), TupleSort (..))
import GHC.Types.Name
import GHC.Types.Name.Reader (RdrName (Exact))
import Prelude hiding (mapM)

import Haddock.Backends.Hoogle (ppExportD)
import Haddock.GhcUtils
import Haddock.Types

-- | Traverse docstrings and ASTs in the Haddock interface, renaming 'Name' to
-- 'DocName'.
--
-- What this really boils down to is: for each 'Name', figure out which of the
-- modules that export the name is the preferred place to link to.
--
-- The renamed output gets written into fields in the Haddock interface record
-- that were previously left empty.
renameInterface
  :: DynFlags
  -- ^ GHC session dyn flags
  -> Map.Map (Maybe String) (Set.Set String)
  -- ^ Ignored symbols. A map from module names to unqualified names. Module
  -- 'Just M' mapping to name 'f' means that link warnings should not be
  -- generated for occurances of specifically 'M.f'. Module 'Nothing' mapping to
  -- name 'f' means that link warnings should not be generated for any 'f'.
  -> LinkEnv
  -- ^ Link environment. A map from 'Name' to 'Module', where name 'n' maps to
  -- module 'M' if 'M' is the preferred link destination for name 'n'.
  -> Bool
  -- ^ Are warnings enabled?
  -> Bool
  -- ^ Is Hoogle output enabled?
  -> Interface
  -- ^ The interface we are renaming.
  -> Ghc Interface
  -- ^ The renamed interface. Note that there is nothing really special about
  -- this being in the 'Ghc' monad. This could very easily be any 'MonadIO' or
  -- even pure, depending on the link warnings are reported.
renameInterface dflags ignoreSet renamingEnv warnings hoogle iface = do
  let (iface', warnedNames) =
        runRnM
          dflags
          mdl
          localLinkEnv
          warnName
          (hoogle && not (OptHide `elem` ifaceOptions iface))
          (renameInterfaceRn iface)
  reportMissingLinks mdl warnedNames
  return iface'
  where
    -- The current module
    mdl :: Module
    mdl = ifaceMod iface

    -- The local link environment, where every name exported by this module is
    -- mapped to the module itself, and everything else comes from the global
    -- renaming env
    localLinkEnv :: LinkEnv
    localLinkEnv = foldr f renamingEnv (ifaceVisibleExports iface)
      where
        f name !env = Map.insert name mdl env

    -- The function used to determine whether we should warn about a name
    -- which we do not find in the renaming environment
    warnName name =
      -- Warnings must be enabled
      warnings
        -- Current module must not be hidden from Haddock
        && not (OptHide `elem` ifaceOptions iface)
        -- Must be an external name that is not built-in syntax, not a type
        -- variable, and not '~'
        && isExternalName name
        && not (isBuiltInSyntax name)
        && not (isTyVarName name)
        && Exact name /= eqTyCon_RDR
        -- Must not be in the set of ignored symbols for the module or the
        -- unqualified ignored symbols
        && not (getOccString name `Set.member` ignoreSet')
      where
        -- The set of ignored symbols within the module this name is located
        -- in unioned with the set of globally ignored symbols
        ignoreSet' :: Set.Set String
        ignoreSet' =
          Set.union
            (Map.findWithDefault Set.empty (Just $ modString name) ignoreSet)
            (Map.findWithDefault Set.empty Nothing ignoreSet)

        modString :: Name -> String
        modString = moduleString . nameModule

-- | Output warning messages indicating that the renamer could not find link
-- destinations for the names in the given set as they occur in the given
-- module.
reportMissingLinks :: Module -> Set.Set Name -> Ghc ()
reportMissingLinks mdl names
  | Set.null names = return ()
  | otherwise =
      liftIO $ do
        putStrLn $ "Warning: " ++ moduleString mdl ++ ": could not find link destinations for: "
        traverse_ (putStrLn . ("\t- " ++) . qualifiedName) names
  where
    qualifiedName :: Name -> String
    qualifiedName name = moduleString (nameModule name) ++ "." ++ getOccString name

--------------------------------------------------------------------------------
-- Monad for renaming
--------------------------------------------------------------------------------

-- | A renaming monad which provides 'MonadReader' access to a renaming
-- environment, and 'MonadWriter' access to a 'Set' of names for which link
-- warnings should be generated, based on the renaming environment.
newtype RnM a = RnM {unRnM :: ReaderT RnMEnv (Writer (Set.Set Name)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader RnMEnv, MonadWriter (Set.Set Name))

-- | The renaming monad environment. Stores the linking environment (mapping
-- names to modules), the link warning predicate, and the current module.
data RnMEnv = RnMEnv
  { rnLinkEnv :: LinkEnv
  -- ^ The linking environment (map from names to modules)
  , rnWarnName :: (Name -> Bool)
  -- ^ Link warning predicate (whether failing to find a link destination
  -- for a given name should result in a warning)
  , rnModuleString :: String
  -- ^ The current module
  , rnHoogleOutput :: Bool
  -- ^ Should Hoogle output be generated for this module?
  , rnDynFlags :: DynFlags
  -- ^ GHC Session DynFlags, necessary for Hoogle output generation
  }

-- | Run the renamer action in a renaming environment built using the given
-- module, link env, and link warning predicate. Returns the renamed value along
-- with a set of 'Name's that were not renamed and should be warned for (i.e.
-- they satisfied the link warning predicate).
runRnM :: DynFlags -> Module -> LinkEnv -> (Name -> Bool) -> Bool -> RnM a -> (a, Set.Set Name)
runRnM dflags mdl linkEnv warnName hoogleOutput rn =
  runWriter $ runReaderT (unRnM rn) rnEnv
  where
    rnEnv :: RnMEnv
    rnEnv =
      RnMEnv
        { rnLinkEnv = linkEnv
        , rnWarnName = warnName
        , rnModuleString = moduleString mdl
        , rnHoogleOutput = hoogleOutput
        , rnDynFlags = dflags
        }

--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------

-- | Rename an `Interface` in the renaming environment.
renameInterfaceRn :: Interface -> RnM Interface
renameInterfaceRn iface = do
  exportItems <- renameExportItems (ifaceExportItems iface)
  orphans <- mapM renameDocInstance (ifaceOrphanInstances iface)
  finalModDoc <- renameDocumentation (ifaceDoc iface)
  pure $!
    iface
      { ifaceRnDoc = finalModDoc
      , -- The un-renamed export items are not used after renaming
        ifaceRnExportItems = exportItems
      , ifaceExportItems = []
      , -- The un-renamed orphan instances are not used after renaming
        ifaceRnOrphanInstances = orphans
      , ifaceOrphanInstances = []
      }

-- | Lookup a 'Name' in the renaming environment.
lookupRn :: Name -> RnM DocName
lookupRn name = RnM $ do
  linkEnv <- asks rnLinkEnv
  case Map.lookup name linkEnv of
    Nothing -> return $ Undocumented name
    Just mdl -> return $ Documented name mdl

-- | Rename a 'Name' in the renaming environment. This is very similar to
-- 'lookupRn', but tracks any names not found in the renaming environment if the
-- `rnWarnName` predicate is true.
renameName :: Name -> RnM DocName
renameName name = do
  warnName <- asks rnWarnName
  docName <- lookupRn name
  case docName of
    Undocumented _ -> do
      when (warnName name) $
        tell $
          Set.singleton name
      return docName
    _ -> return docName

-- | Rename a located 'Name' in the current renaming environment.
renameNameL :: GenLocated l Name -> RnM (GenLocated l DocName)
renameNameL = mapM renameName

-- | Rename a list of export items in the current renaming environment.
renameExportItems :: [ExportItem GhcRn] -> RnM [ExportItem DocNameI]
renameExportItems = mapM renameExportItem

-- | Rename an 'ExportItem' in the current renaming environment.
renameExportItem :: ExportItem GhcRn -> RnM (ExportItem DocNameI)
renameExportItem item = case item of
  ExportModule mdl -> return (ExportModule mdl)
  ExportGroup lev id_ doc -> do
    doc' <- renameDoc doc
    return (ExportGroup lev id_ doc')
  ExportDecl ed@(ExportD decl pats doc subs instances fixities splice) -> do
    -- If Hoogle output should be generated, generate it
    RnMEnv{..} <- ask
    let !hoogleOut =
          force $
            if rnHoogleOutput
              then
                -- Since Hoogle is line based, we want to avoid breaking long lines.
                let dflags = rnDynFlags{pprCols = maxBound}
                 in ppExportD dflags ed
              else []

    decl' <- renameLDecl decl
    pats' <- renamePats pats
    doc' <- renameDocForDecl doc
    subs' <- mapM renameSub subs
    instances' <- forM instances renameDocInstance
    fixities' <- forM fixities $ \(name, fixity) -> do
      name' <- lookupRn name
      return (name', fixity)

    return $
      ExportDecl
        RnExportD
          { rnExpDExpD = ExportD decl' pats' doc' subs' instances' fixities' splice
          , rnExpDHoogle = hoogleOut
          }
  ExportNoDecl x subs -> do
    x' <- lookupRn x
    subs' <- mapM lookupRn subs
    return (ExportNoDecl x' subs')
  ExportDoc doc -> do
    doc' <- renameDoc doc
    return (ExportDoc doc')

renameDocForDecl :: DocForDecl Name -> RnM (DocForDecl DocName)
renameDocForDecl (doc, fnArgsDoc) =
  (,) <$> renameDocumentation doc <*> renameFnArgsDoc fnArgsDoc

renameDocumentation :: Documentation Name -> RnM (Documentation DocName)
renameDocumentation (Documentation mDoc mWarning) =
  Documentation <$> mapM renameDoc mDoc <*> mapM renameDoc mWarning

renameLDocHsSyn :: Located (WithHsDocIdentifiers HsDocString a) -> RnM (Located (WithHsDocIdentifiers HsDocString b))
renameLDocHsSyn (L l doc) = return (L l (WithHsDocIdentifiers (hsDocString doc) []))

renameDoc :: Traversable t => t (Wrap Name) -> RnM (t (Wrap DocName))
renameDoc = traverse (traverse renameName)

renameFnArgsDoc :: FnArgsDoc Name -> RnM (FnArgsDoc DocName)
renameFnArgsDoc = mapM renameDoc

renameLType :: LHsType GhcRn -> RnM (LHsType DocNameI)
renameLType = mapM renameType

renameLTypeArg :: LHsTypeArg GhcRn -> RnM (LHsTypeArg DocNameI)
renameLTypeArg (HsValArg _ ty) = do
  ty' <- renameLType ty
  return $ HsValArg noExtField ty'
renameLTypeArg (HsTypeArg _ ki) = do
  ki' <- renameLKind ki
  return $ HsTypeArg noExtField ki'
renameLTypeArg (HsArgPar _) = return $ HsArgPar noExtField

renameLSigType :: LHsSigType GhcRn -> RnM (LHsSigType DocNameI)
renameLSigType = mapM renameSigType

renameLSigWcType :: LHsSigWcType GhcRn -> RnM (LHsSigWcType DocNameI)
renameLSigWcType = renameWc renameLSigType

renameLKind :: LHsKind GhcRn -> RnM (LHsKind DocNameI)
renameLKind = renameLType

renameMaybeLKind :: Maybe (LHsKind GhcRn) -> RnM (Maybe (LHsKind DocNameI))
renameMaybeLKind = traverse renameLKind

renameFamilyResultSig :: LFamilyResultSig GhcRn -> RnM (LFamilyResultSig DocNameI)
renameFamilyResultSig (L loc (NoSig _)) =
  return (L loc (NoSig noExtField))
renameFamilyResultSig (L loc (KindSig _ ki)) =
  do
    ki' <- renameLKind ki
    return (L loc (KindSig noExtField ki'))
renameFamilyResultSig (L loc (TyVarSig _ bndr)) =
  do
    bndr' <- renameLTyVarBndr return bndr
    return (L loc (TyVarSig noExtField bndr'))

renameInjectivityAnn :: LInjectivityAnn GhcRn -> RnM (LInjectivityAnn DocNameI)
renameInjectivityAnn (L loc (InjectivityAnn _ lhs rhs)) =
  do
    lhs' <- renameNameL lhs
    rhs' <- mapM renameNameL rhs
    return (L loc (InjectivityAnn noExtField lhs' rhs'))

renameMaybeInjectivityAnn
  :: Maybe (LInjectivityAnn GhcRn)
  -> RnM (Maybe (LInjectivityAnn DocNameI))
renameMaybeInjectivityAnn = traverse renameInjectivityAnn

renameArrow :: HsArrow GhcRn -> RnM (HsArrow DocNameI)
renameArrow (HsUnrestrictedArrow _) = return (HsUnrestrictedArrow noExtField)
renameArrow (HsLinearArrow _) = return (HsLinearArrow noExtField)
renameArrow (HsExplicitMult _ p) = HsExplicitMult noExtField <$> renameLType p

renameType :: HsType GhcRn -> RnM (HsType DocNameI)
renameType t = case t of
  HsForAllTy{hst_tele = tele, hst_body = ltype} -> do
    tele' <- renameHsForAllTelescope tele
    ltype' <- renameLType ltype
    return
      ( HsForAllTy
          { hst_xforall = noAnn
          , hst_tele = tele'
          , hst_body = ltype'
          }
      )
  HsQualTy{hst_ctxt = lcontext, hst_body = ltype} -> do
    lcontext' <- renameLContext lcontext
    ltype' <- renameLType ltype
    return (HsQualTy{hst_xqual = noAnn, hst_ctxt = lcontext', hst_body = ltype'})
  HsTyVar _ ip (L l n) -> return . HsTyVar noAnn ip . L l =<< renameName n
  HsBangTy _ b ltype -> return . HsBangTy noAnn b =<< renameLType ltype
  HsStarTy _ isUni -> return (HsStarTy noAnn isUni)
  HsAppTy _ a b -> do
    a' <- renameLType a
    b' <- renameLType b
    return (HsAppTy noAnn a' b')
  HsAppKindTy _ a b -> do
    a' <- renameLType a
    b' <- renameLKind b
    return (HsAppKindTy noAnn a' b')
  HsFunTy _ w a b -> do
    a' <- renameLType a
    b' <- renameLType b
    w' <- renameArrow w
    return (HsFunTy noAnn w' a' b')
  HsListTy _ ty -> return . (HsListTy noAnn) =<< renameLType ty
  HsIParamTy _ n ty -> liftM (HsIParamTy noAnn n) (renameLType ty)
  -- Special-case unary boxed tuples so that they are pretty-printed as
  -- `Solo x`, not `(x)`
  HsTupleTy _ HsBoxedOrConstraintTuple [ty] -> do
    name <- renameName (tupleTyConName BoxedTuple 1)
    let lhs = noLocA $ HsTyVar noAnn NotPromoted (noLocA name)
    rhs <- renameLType ty
    return (HsAppTy noAnn lhs rhs)
  HsTupleTy _ b ts -> return . HsTupleTy noAnn b =<< mapM renameLType ts
  HsSumTy _ ts -> HsSumTy noAnn <$> mapM renameLType ts
  HsOpTy _ prom a (L loc op) b -> do
    op' <- renameName op
    a' <- renameLType a
    b' <- renameLType b
    return (HsOpTy noAnn prom a' (L loc op') b')
  HsParTy _ ty -> return . (HsParTy noAnn) =<< renameLType ty
  HsKindSig _ ty k -> do
    ty' <- renameLType ty
    k' <- renameLKind k
    return (HsKindSig noAnn ty' k')
  HsDocTy _ ty doc -> do
    ty' <- renameLType ty
    doc' <- renameLDocHsSyn doc
    return (HsDocTy noAnn ty' doc')
  HsTyLit _ x -> return (HsTyLit noAnn (renameTyLit x))
  HsRecTy _ a -> HsRecTy noAnn <$> mapM renameConDeclFieldField a
  XHsType a -> pure (XHsType a)
  HsExplicitListTy _ a b -> HsExplicitListTy noAnn a <$> mapM renameLType b
  -- Special-case unary boxed tuples so that they are pretty-printed as
  -- `'MkSolo x`, not `'(x)`
  HsExplicitTupleTy _ [ty] -> do
    name <- renameName (tupleDataConName Boxed 1)
    let lhs = noLocA $ HsTyVar noAnn IsPromoted (noLocA name)
    rhs <- renameLType ty
    return (HsAppTy noAnn lhs rhs)
  HsExplicitTupleTy _ b -> HsExplicitTupleTy noAnn <$> mapM renameLType b
  HsSpliceTy (HsUntypedSpliceTop _ st) _ -> renameType (unLoc st)
  HsSpliceTy (HsUntypedSpliceNested _) _ -> error "renameType: not an top level type splice"
  HsWildCardTy _ -> pure (HsWildCardTy noAnn)

renameTyLit :: HsTyLit GhcRn -> HsTyLit DocNameI
renameTyLit t = case t of
  HsNumTy _ v -> HsNumTy noExtField v
  HsStrTy _ v -> HsStrTy noExtField v
  HsCharTy _ v -> HsCharTy noExtField v

renameSigType :: HsSigType GhcRn -> RnM (HsSigType DocNameI)
renameSigType (HsSig{sig_bndrs = bndrs, sig_body = body}) = do
  bndrs' <- renameOuterTyVarBndrs bndrs
  body' <- renameLType body
  pure $ HsSig{sig_ext = noExtField, sig_bndrs = bndrs', sig_body = body'}

renameLHsQTyVars :: LHsQTyVars GhcRn -> RnM (LHsQTyVars DocNameI)
renameLHsQTyVars (HsQTvs{hsq_explicit = tvs}) =
  do
    tvs' <- mapM (renameLTyVarBndr renameHsBndrVis) tvs
    return
      ( HsQTvs
          { hsq_ext = noExtField
          , hsq_explicit = tvs'
          }
      )

renameHsBndrVis :: HsBndrVis GhcRn -> RnM (HsBndrVis DocNameI)
renameHsBndrVis (HsBndrRequired _) = return (HsBndrRequired noExtField)
renameHsBndrVis (HsBndrInvisible at) = return (HsBndrInvisible at)

renameHsForAllTelescope :: HsForAllTelescope GhcRn -> RnM (HsForAllTelescope DocNameI)
renameHsForAllTelescope tele = case tele of
  HsForAllVis _ bndrs -> do
    bndrs' <- mapM (renameLTyVarBndr return) bndrs
    pure $ HsForAllVis noExtField bndrs'
  HsForAllInvis _ bndrs -> do
    bndrs' <- mapM (renameLTyVarBndr return) bndrs
    pure $ HsForAllInvis noExtField bndrs'

renameLTyVarBndr :: (flag -> RnM flag') -> LHsTyVarBndr flag GhcRn -> RnM (LHsTyVarBndr flag' DocNameI)
renameLTyVarBndr rn_flag (L loc (UserTyVar _ fl (L l n))) =
  do
    fl' <- rn_flag fl
    n' <- renameName n
    return (L loc (UserTyVar noExtField fl' (L l n')))
renameLTyVarBndr rn_flag (L loc (KindedTyVar _ fl (L lv n) kind)) =
  do
    fl' <- rn_flag fl
    n' <- renameName n
    kind' <- renameLKind kind
    return (L loc (KindedTyVar noExtField fl' (L lv n') kind'))

renameLContext :: LocatedC [LHsType GhcRn] -> RnM (LocatedC [LHsType DocNameI])
renameLContext (L loc context) = do
  context' <- mapM renameLType context
  return (L loc context')

renameInstHead :: InstHead GhcRn -> RnM (InstHead DocNameI)
renameInstHead InstHead{..} = do
  cname <- renameName ihdClsName
  types <- mapM renameType ihdTypes
  itype <- case ihdInstType of
    ClassInst{..} ->
      ClassInst
        <$> mapM renameType clsiCtx
        <*> renameLHsQTyVars clsiTyVars
        <*> mapM renameSig clsiSigs
        <*> mapM renameDocInstance clsiAssocTys
    TypeInst ts -> TypeInst <$> traverse renameType ts
    DataInst dd -> DataInst <$> renameTyClD dd
  return
    InstHead
      { ihdClsName = cname
      , ihdTypes = types
      , ihdInstType = itype
      }

renameLDecl :: LHsDecl GhcRn -> RnM (LHsDecl DocNameI)
renameLDecl (L loc d) = return . L loc =<< renameDecl d

renamePats :: [(HsDecl GhcRn, DocForDecl Name)] -> RnM [(HsDecl DocNameI, DocForDecl DocName)]
renamePats =
  mapM
    ( \(d, doc) -> do
        d' <- renameDecl d
        doc' <- renameDocForDecl doc
        return (d', doc')
    )

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

renameLThing :: (a GhcRn -> RnM (a DocNameI)) -> LocatedAn an (a GhcRn) -> RnM (Located (a DocNameI))
renameLThing fn (L loc x) = return . L (locA loc) =<< fn x

renameTyClD :: TyClDecl GhcRn -> RnM (TyClDecl DocNameI)
renameTyClD d = case d of
  --  TyFamily flav lname ltyvars kind tckind -> do
  FamDecl{tcdFam = decl} -> do
    decl' <- renameFamilyDecl decl
    return (FamDecl{tcdFExt = noExtField, tcdFam = decl'})
  SynDecl{tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdRhs = rhs} -> do
    lname' <- renameNameL lname
    tyvars' <- renameLHsQTyVars tyvars
    rhs' <- renameLType rhs
    return
      ( SynDecl
          { tcdSExt = noExtField
          , tcdLName = lname'
          , tcdTyVars = tyvars'
          , tcdFixity = fixity
          , tcdRhs = rhs'
          }
      )
  DataDecl{tcdLName = lname, tcdTyVars = tyvars, tcdFixity = fixity, tcdDataDefn = defn} -> do
    lname' <- renameNameL lname
    tyvars' <- renameLHsQTyVars tyvars
    defn' <- renameDataDefn defn
    return
      ( DataDecl
          { tcdDExt = noExtField
          , tcdLName = lname'
          , tcdTyVars = tyvars'
          , tcdFixity = fixity
          , tcdDataDefn = defn'
          }
      )
  ClassDecl
    { tcdCtxt = lcontext
    , tcdLName = lname
    , tcdTyVars = ltyvars
    , tcdFixity = fixity
    , tcdFDs = lfundeps
    , tcdSigs = lsigs
    , tcdATs = ats
    , tcdATDefs = at_defs
    } -> do
      lcontext' <- traverse renameLContext lcontext
      lname' <- renameNameL lname
      ltyvars' <- renameLHsQTyVars ltyvars
      lfundeps' <- mapM renameLFunDep lfundeps
      lsigs' <- mapM renameLSig lsigs
      ats' <- mapM (renameLThing renameFamilyDecl) ats
      at_defs' <- mapM (mapM renameTyFamDefltD) at_defs
      -- we don't need the default methods or the already collected doc entities
      return
        ( ClassDecl
            { tcdCExt = noExtField
            , tcdCtxt = lcontext'
            , tcdLName = lname'
            , tcdTyVars = ltyvars'
            , tcdFixity = fixity
            , tcdFDs = lfundeps'
            , tcdSigs = lsigs'
            , tcdMeths = []
            , tcdATs = ats'
            , tcdATDefs = at_defs'
            , tcdDocs = []
            }
        )
  where
    renameLFunDep :: LHsFunDep GhcRn -> RnM (LHsFunDep DocNameI)
    renameLFunDep (L loc (FunDep _ xs ys)) = do
      xs' <- mapM renameName (map unLoc xs)
      ys' <- mapM renameName (map unLoc ys)
      return (L (locA loc) (FunDep noExtField (map noLocA xs') (map noLocA ys')))

    renameLSig (L loc sig) = return . L (locA loc) =<< renameSig sig

renameFamilyDecl :: FamilyDecl GhcRn -> RnM (FamilyDecl DocNameI)
renameFamilyDecl
  ( FamilyDecl
      { fdInfo = info
      , fdLName = lname
      , fdTyVars = ltyvars
      , fdFixity = fixity
      , fdResultSig = result
      , fdInjectivityAnn = injectivity
      }
    ) = do
    info' <- renameFamilyInfo info
    lname' <- renameNameL lname
    ltyvars' <- renameLHsQTyVars ltyvars
    result' <- renameFamilyResultSig result
    injectivity' <- renameMaybeInjectivityAnn injectivity
    return
      ( FamilyDecl
          { fdExt = noExtField
          , fdInfo = info'
          , fdTopLevel = TopLevel
          , fdLName = lname'
          , fdTyVars = ltyvars'
          , fdFixity = fixity
          , fdResultSig = result'
          , fdInjectivityAnn = injectivity'
          }
      )

renameFamilyInfo :: FamilyInfo GhcRn -> RnM (FamilyInfo DocNameI)
renameFamilyInfo DataFamily = return DataFamily
renameFamilyInfo OpenTypeFamily = return OpenTypeFamily
renameFamilyInfo (ClosedTypeFamily eqns) =
  do
    eqns' <- mapM (mapM (mapM renameTyFamInstEqn)) eqns
    return $ ClosedTypeFamily eqns'

renameDataDefn :: HsDataDefn GhcRn -> RnM (HsDataDefn DocNameI)
renameDataDefn
  ( HsDataDefn
      { dd_ctxt = lcontext
      , dd_cType = cType
      , dd_kindSig = k
      , dd_cons = cons
      }
    ) = do
    lcontext' <- traverse renameLContext lcontext
    k' <- renameMaybeLKind k
    cons' <- mapM (mapMA renameCon) cons
    -- I don't think we need the derivings, so we return Nothing
    return
      ( HsDataDefn
          { dd_ext = noExtField
          , dd_ctxt = lcontext'
          , dd_cType = cType
          , dd_kindSig = k'
          , dd_cons = cons'
          , dd_derivs = []
          }
      )

renameCon :: ConDecl GhcRn -> RnM (ConDecl DocNameI)
renameCon
  decl@( ConDeclH98
          { con_name = lname
          , con_ex_tvs = ltyvars
          , con_mb_cxt = lcontext
          , con_args = details
          , con_doc = mbldoc
          , con_forall = forall_
          }
        ) = do
    lname' <- renameNameL lname
    ltyvars' <- mapM (renameLTyVarBndr return) ltyvars
    lcontext' <- traverse renameLContext lcontext
    details' <- renameH98Details details
    mbldoc' <- mapM (renameLDocHsSyn) mbldoc
    return
      ( decl
          { con_ext = noExtField
          , con_name = lname'
          , con_ex_tvs = ltyvars'
          , con_mb_cxt = lcontext'
          , con_forall = forall_ -- Remove when #18311 is fixed
          , con_args = details'
          , con_doc = mbldoc'
          }
      )
renameCon
  ConDeclGADT
    { con_names = lnames
    , con_bndrs = bndrs
    , con_mb_cxt = lcontext
    , con_g_args = details
    , con_res_ty = res_ty
    , con_doc = mbldoc
    } = do
    lnames' <- mapM renameNameL lnames
    bndrs' <- mapM renameOuterTyVarBndrs bndrs
    lcontext' <- traverse renameLContext lcontext
    details' <- renameGADTDetails details
    res_ty' <- renameLType res_ty
    mbldoc' <- mapM renameLDocHsSyn mbldoc
    return
      ( ConDeclGADT
          { con_g_ext = noExtField
          , con_names = lnames'
          , con_bndrs = bndrs'
          , con_mb_cxt = lcontext'
          , con_g_args = details'
          , con_res_ty = res_ty'
          , con_doc = mbldoc'
          }
      )

renameHsScaled
  :: HsScaled GhcRn (LHsType GhcRn)
  -> RnM (HsScaled DocNameI (LHsType DocNameI))
renameHsScaled (HsScaled w ty) = HsScaled <$> renameArrow w <*> renameLType ty

renameH98Details
  :: HsConDeclH98Details GhcRn
  -> RnM (HsConDeclH98Details DocNameI)
renameH98Details (RecCon (L l fields)) = do
  fields' <- mapM renameConDeclFieldField fields
  return (RecCon (L (locA l) fields'))
renameH98Details (PrefixCon ts ps) = PrefixCon ts <$> mapM renameHsScaled ps
renameH98Details (InfixCon a b) = do
  a' <- renameHsScaled a
  b' <- renameHsScaled b
  return (InfixCon a' b')

renameGADTDetails
  :: HsConDeclGADTDetails GhcRn
  -> RnM (HsConDeclGADTDetails DocNameI)
renameGADTDetails (RecConGADT _ (L l fields)) = do
  fields' <- mapM renameConDeclFieldField fields
  return (RecConGADT noExtField (L (locA l) fields'))
renameGADTDetails (PrefixConGADT _ ps) = PrefixConGADT noExtField <$> mapM renameHsScaled ps

renameConDeclFieldField :: LConDeclField GhcRn -> RnM (LConDeclField DocNameI)
renameConDeclFieldField (L l (ConDeclField _ names t doc)) = do
  names' <- mapM renameLFieldOcc names
  t' <- renameLType t
  doc' <- mapM renameLDocHsSyn doc
  return $ L (locA l) (ConDeclField noExtField names' t' doc')

renameLFieldOcc :: LFieldOcc GhcRn -> RnM (LFieldOcc DocNameI)
renameLFieldOcc (L l (FieldOcc sel lbl)) = do
  sel' <- renameName sel
  return $ L l (FieldOcc sel' lbl)

renameSig :: Sig GhcRn -> RnM (Sig DocNameI)
renameSig sig = case sig of
  TypeSig _ lnames ltype -> do
    lnames' <- mapM renameNameL lnames
    ltype' <- renameLSigWcType ltype
    return (TypeSig noExtField lnames' ltype')
  ClassOpSig _ is_default lnames sig_ty -> do
    lnames' <- mapM renameNameL lnames
    ltype' <- renameLSigType sig_ty
    return (ClassOpSig noExtField is_default lnames' ltype')
  PatSynSig _ lnames sig_ty -> do
    lnames' <- mapM renameNameL lnames
    sig_ty' <- renameLSigType sig_ty
    return $ PatSynSig noExtField lnames' sig_ty'
  FixSig _ (FixitySig _ lnames fixity) -> do
    lnames' <- mapM renameNameL lnames
    return $ FixSig noExtField (FixitySig noExtField lnames' fixity)
  MinimalSig _ (L l s) -> do
    s' <- traverse (traverse lookupRn) s
    return $ MinimalSig noExtField (L l s')
  -- we have filtered out all other kinds of signatures in Interface.Create
  _ -> error "expected TypeSig"

renameForD :: ForeignDecl GhcRn -> RnM (ForeignDecl DocNameI)
renameForD (ForeignImport _ lname ltype x) = do
  lname' <- renameNameL lname
  ltype' <- renameLSigType ltype
  return (ForeignImport noExtField lname' ltype' (renameForI x))
renameForD (ForeignExport _ lname ltype x) = do
  lname' <- renameNameL lname
  ltype' <- renameLSigType ltype
  return (ForeignExport noExtField lname' ltype' (renameForE x))

renameForI :: ForeignImport GhcRn -> ForeignImport DocNameI
renameForI (CImport _ cconv safety mHeader spec) = CImport noExtField cconv safety mHeader spec

renameForE :: ForeignExport GhcRn -> ForeignExport DocNameI
renameForE (CExport _ spec) = CExport noExtField spec

renameInstD :: InstDecl GhcRn -> RnM (InstDecl DocNameI)
renameInstD (ClsInstD{cid_inst = d}) = do
  d' <- renameClsInstD d
  return (ClsInstD{cid_d_ext = noExtField, cid_inst = d'})
renameInstD (TyFamInstD{tfid_inst = d}) = do
  d' <- renameTyFamInstD d
  return (TyFamInstD{tfid_ext = noExtField, tfid_inst = d'})
renameInstD (DataFamInstD{dfid_inst = d}) = do
  d' <- renameDataFamInstD d
  return (DataFamInstD{dfid_ext = noExtField, dfid_inst = d'})

renameDerivD :: DerivDecl GhcRn -> RnM (DerivDecl DocNameI)
renameDerivD
  ( DerivDecl
      { deriv_type = ty
      , deriv_strategy = strat
      , deriv_overlap_mode = omode
      }
    ) = do
    ty' <- renameLSigWcType ty
    strat' <- mapM (mapM renameDerivStrategy) strat
    return
      ( DerivDecl
          { deriv_ext = noExtField
          , deriv_type = ty'
          , deriv_strategy = strat'
          , deriv_overlap_mode = omode
          }
      )

renameDerivStrategy :: DerivStrategy GhcRn -> RnM (DerivStrategy DocNameI)
renameDerivStrategy (StockStrategy a) = pure (StockStrategy a)
renameDerivStrategy (AnyclassStrategy a) = pure (AnyclassStrategy a)
renameDerivStrategy (NewtypeStrategy a) = pure (NewtypeStrategy a)
renameDerivStrategy (ViaStrategy ty) = ViaStrategy <$> renameLSigType ty

renameClsInstD :: ClsInstDecl GhcRn -> RnM (ClsInstDecl DocNameI)
renameClsInstD
  ( ClsInstDecl
      { cid_overlap_mode = omode
      , cid_poly_ty = ltype
      , cid_tyfam_insts = lATs
      , cid_datafam_insts = lADTs
      }
    ) = do
    ltype' <- renameLSigType ltype
    lATs' <- mapM (mapM renameTyFamInstD) lATs
    lADTs' <- mapM (mapM renameDataFamInstD) lADTs
    return
      ( ClsInstDecl
          { cid_ext = noExtField
          , cid_overlap_mode = omode
          , cid_poly_ty = ltype'
          , cid_binds = []
          , cid_sigs = []
          , cid_tyfam_insts = lATs'
          , cid_datafam_insts = lADTs'
          }
      )

renameTyFamInstD :: TyFamInstDecl GhcRn -> RnM (TyFamInstDecl DocNameI)
renameTyFamInstD (TyFamInstDecl{tfid_eqn = eqn}) =
  do
    eqn' <- renameTyFamInstEqn eqn
    return (TyFamInstDecl{tfid_xtn = noExtField, tfid_eqn = eqn'})

renameTyFamInstEqn :: TyFamInstEqn GhcRn -> RnM (TyFamInstEqn DocNameI)
renameTyFamInstEqn
  ( FamEqn
      { feqn_tycon = tc
      , feqn_bndrs = bndrs
      , feqn_pats = pats
      , feqn_fixity = fixity
      , feqn_rhs = rhs
      }
    ) =
    do
      tc' <- renameNameL tc
      bndrs' <- renameOuterTyVarBndrs bndrs
      pats' <- mapM renameLTypeArg pats
      rhs' <- renameLType rhs
      return
        ( FamEqn
            { feqn_ext = noExtField
            , feqn_tycon = tc'
            , feqn_bndrs = bndrs'
            , feqn_pats = pats'
            , feqn_fixity = fixity
            , feqn_rhs = rhs'
            }
        )

renameTyFamDefltD :: TyFamDefltDecl GhcRn -> RnM (TyFamDefltDecl DocNameI)
renameTyFamDefltD = renameTyFamInstD

renameDataFamInstD :: DataFamInstDecl GhcRn -> RnM (DataFamInstDecl DocNameI)
renameDataFamInstD (DataFamInstDecl{dfid_eqn = eqn}) =
  do
    eqn' <- rename_data_fam_eqn eqn
    return (DataFamInstDecl{dfid_eqn = eqn'})
  where
    rename_data_fam_eqn
      :: FamEqn GhcRn (HsDataDefn GhcRn)
      -> RnM (FamEqn DocNameI (HsDataDefn DocNameI))
    rename_data_fam_eqn
      ( FamEqn
          { feqn_tycon = tc
          , feqn_bndrs = bndrs
          , feqn_pats = pats
          , feqn_fixity = fixity
          , feqn_rhs = defn
          }
        ) =
        do
          tc' <- renameNameL tc
          bndrs' <- renameOuterTyVarBndrs bndrs
          pats' <- mapM renameLTypeArg pats
          defn' <- renameDataDefn defn
          return
            ( FamEqn
                { feqn_ext = noExtField
                , feqn_tycon = tc'
                , feqn_bndrs = bndrs'
                , feqn_pats = pats'
                , feqn_fixity = fixity
                , feqn_rhs = defn'
                }
            )

renameOuterTyVarBndrs
  :: HsOuterTyVarBndrs flag GhcRn
  -> RnM (HsOuterTyVarBndrs flag DocNameI)
renameOuterTyVarBndrs (HsOuterImplicit{}) =
  pure $ HsOuterImplicit{hso_ximplicit = noExtField}
renameOuterTyVarBndrs (HsOuterExplicit{hso_bndrs = exp_bndrs}) =
  HsOuterExplicit noExtField <$> mapM (renameLTyVarBndr return) exp_bndrs

renameWc
  :: (in_thing -> RnM out_thing)
  -> HsWildCardBndrs GhcRn in_thing
  -> RnM (HsWildCardBndrs DocNameI out_thing)
renameWc rn_thing (HsWC{hswc_body = thing}) =
  do
    thing' <- rn_thing thing
    return
      ( HsWC
          { hswc_body = thing'
          , hswc_ext = noExtField
          }
      )

renameDocInstance :: DocInstance GhcRn -> RnM (DocInstance DocNameI)
renameDocInstance (inst, idoc, L l n, m) = do
  inst' <- renameInstHead inst
  n' <- renameName n
  idoc' <- mapM renameDoc idoc
  return (inst', idoc', L l n', m)

renameSub :: (Name, DocForDecl Name) -> RnM (DocName, DocForDecl DocName)
renameSub (n, doc) = do
  n' <- renameName n
  doc' <- renameDocForDecl doc
  return (n', doc')
