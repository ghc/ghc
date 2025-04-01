{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- For the HasLoc instances

{-
Main functions for .hie file generation
-}

module GHC.Iface.Ext.Ast ( mkHieFile, mkHieFileWithSource, getCompressedAsts, enrichHie) where

import GHC.Utils.Outputable(ppr)

import GHC.Prelude hiding ( head, init, last, tail )

import GHC.Types.Avail            ( Avails )
import GHC.Data.Bag               ( Bag, bagToList )
import GHC.Types.Basic
import GHC.Data.BooleanFormula
import GHC.Core.Class             ( className, classSCSelIds )
import GHC.Core.ConLike           ( conLikeName )
import GHC.Core.FVs
import GHC.Core.DataCon           ( dataConNonlinearType )
import GHC.Types.FieldLabel
import GHC.Hs
import GHC.Hs.Syn.Type
import GHC.Utils.Monad            ( concatMapM, MonadIO(liftIO) )
import GHC.Types.Id               ( isDataConId_maybe )
import GHC.Types.Name             ( Name, nameSrcSpan, nameUnique, wiredInNameTyThing_maybe, getName )
import GHC.Types.Name.Env         ( NameEnv, emptyNameEnv, extendNameEnv, lookupNameEnv )
import GHC.Types.Name.Reader      ( RecFieldInfo(..), WithUserRdr(..) )
import GHC.Types.SrcLoc
import GHC.Core.Type              ( Type )
import GHC.Core.TyCon             ( TyCon, tyConClass_maybe )
import GHC.Core.Predicate
import GHC.Core.InstEnv
import GHC.Tc.Types
import GHC.Tc.Types.Evidence
import GHC.Types.Var              ( Id, Var, EvId, varName, varType, varUnique )
import GHC.Types.Var.Env
import GHC.Builtin.Uniques
import GHC.Iface.Make             ( mkIfaceExports )
import GHC.Utils.Panic
import GHC.Data.Maybe
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Data.Pair

import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils

import GHC.Unit.Module            ( ml_hs_file )
import GHC.Unit.Module.ModSummary

import qualified Data.Array as A
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Data                  ( Data, Typeable )
import Data.Foldable              ( toList )
import Data.Functor.Identity      ( Identity(..) )
import Data.List.NonEmpty         ( NonEmpty(..), nonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Void                  ( Void, absurd )
import Control.Monad              ( forM_ )
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class  ( lift )
import Control.Applicative        ( (<|>) )
import GHC.Types.TypeEnv          ( TypeEnv )
import Control.Arrow              ( second )
import Data.Traversable           ( mapAccumR )

{- Note [Updating HieAst for changes in the GHC AST]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When updating the code in this file for changes in the GHC AST, you
need to pay attention to the following things:

1) Symbols (Names/Vars/Modules) in the following categories:

   a) Symbols that appear in the source file that directly correspond to
   something the user typed
   b) Symbols that don't appear in the source, but should be in some sense
   "visible" to a user, particularly via IDE tooling or the like. This
   includes things like the names introduced by RecordWildcards (We record
   all the names introduced by a (..) in HIE files), and will include implicit
   parameters and evidence variables after one of my pending MRs lands.

2) Subtrees that may contain such symbols, or correspond to a SrcSpan in
   the file. This includes all `Located` things

For 1), you need to call `toHie` for one of the following instances

instance ToHie (Context (Located Name)) where ...
instance ToHie (Context (Located Var)) where ...
instance ToHie (IEContext (Located ModuleName)) where ...

`Context` is a data type that looks like:

data Context a = C ContextInfo a -- Used for names and bindings

`ContextInfo` is defined in `GHC.Iface.Ext.Types`, and looks like

data ContextInfo
  = Use                -- ^ regular variable
  | MatchBind
  | IEThing IEType     -- ^ import/export
  | TyDecl
  -- | Value binding
  | ValBind
      BindType     -- ^ whether or not the binding is in an instance
      Scope        -- ^ scope over which the value is bound
      (Maybe Span) -- ^ span of entire binding
  ...

It is used to annotate symbols in the .hie files with some extra information on
the context in which they occur and should be fairly self explanatory. You need
to select one that looks appropriate for the symbol usage. In very rare cases,
you might need to extend this sum type if none of the cases seem appropriate.

So, given a `Located Name` that is just being "used", and not defined at a
particular location, you would do the following:

   toHie $ C Use located_name

If you select one that corresponds to a binding site, you will need to
provide a `Scope` and a `Span` for your binding. Both of these are basically
`SrcSpans`.

The `SrcSpan` in the `Scope` is supposed to span over the part of the source
where the symbol can be legally allowed to occur. For more details on how to
calculate this, see Note [Capturing Scopes and other non local information]
in GHC.Iface.Ext.Ast.

The binding `Span` is supposed to be the span of the entire binding for
the name.

For a function definition `foo`:

foo x = x + y
  where y = x^2

The binding `Span` is the span of the entire function definition from `foo x`
to `x^2`.  For a class definition, this is the span of the entire class, and
so on.  If this isn't well defined for your bit of syntax (like a variable
bound by a lambda), then you can just supply a `Nothing`

There is a test that checks that all symbols in the resulting HIE file
occur inside their stated `Scope`. This can be turned on by passing the
-fvalidate-ide-info flag to ghc along with -fwrite-ide-info to generate the
.hie file.

You may also want to provide a test in testsuite/test/hiefile that includes
a file containing your new construction, and tests that the calculated scope
is valid (by using -fvalidate-ide-info)

For subtrees in the AST that may contain symbols, the procedure is fairly
straightforward.  If you are extending the GHC AST, you will need to provide a
`ToHie` instance for any new types you may have introduced in the AST.

Here is an extract from the `ToHie` instance for (LHsExpr (GhcPass p)):

  toHie e@(L mspan oexpr) = concatM $ getTypeNode e : case oexpr of
      HsVar _ (L _ var) ->
        [ toHie $ C Use (L mspan var)
             -- Patch up var location since typechecker removes it
        ]
      ...
      HsApp _ a b ->
        [ toHie a
        , toHie b
        ]

If your subtree is `Located` or has a `SrcSpan` available, the output list
should contain a HieAst `Node` corresponding to the subtree. You can use
either `makeNode` or `getTypeNode` for this purpose, depending on whether it
makes sense to assign a `Type` to the subtree. After this, you just need
to concatenate the result of calling `toHie` on all subexpressions and
appropriately annotated symbols contained in the subtree.

The code above from the ToHie instance of `LhsExpr (GhcPass p)` is supposed
to work for both the renamed and typechecked source. `getTypeNode` is from
the `HasType` class defined in this file, and it has different instances
for `GhcTc` and `GhcRn` that allow it to access the type of the expression
when given a typechecked AST:

class Data a => HasType a where
  getTypeNode :: a -> HieM [HieAST Type]
instance HasType (LHsExpr GhcTc) where
  getTypeNode e@(L spn e') = ... -- Actually get the type for this expression
instance HasType (LHsExpr GhcRn) where
  getTypeNode (L spn e) = makeNode e spn -- Fallback to a regular `makeNode` without recording the type

If your subtree doesn't have a span available, you can omit the `makeNode`
call and just recurse directly in to the subexpressions.

-}

-- These synonyms match those defined in compiler/GHC.hs
type RenamedSource     = ( HsGroup GhcRn, [LImportDecl GhcRn]
                         , Maybe [(LIE GhcRn, Avails)]
                         , Maybe (LHsDoc GhcRn)
                         , Maybe (XRec GhcRn ModuleName) )
type TypecheckedSource = LHsBinds GhcTc


{- Note [Name Remapping]
   ~~~~~~~~~~~~~~~~~~~~~
The Typechecker introduces new names for mono names in AbsBinds.
We don't care about the distinction between mono and poly bindings,
so we replace all occurrences of the mono name with the poly name.
-}
type VarMap a = DVarEnv (Var,a)
data HieState = HieState
  { name_remapping :: NameEnv Id
  , unlocated_ev_binds :: VarMap (S.Set ContextInfo)
  -- These contain evidence bindings that we don't have a location for
  -- These are placed at the top level Node in the HieAST after everything
  -- else has been generated
  -- This includes things like top level evidence bindings.
  , type_env :: TypeEnv
  -- tcg_type_env from TcGblEnv contains the type environment for the module
  , entity_infos :: NameEntityInfo
  -- ^ Information about entities in the module
  }

addUnlocatedEvBind :: Var -> ContextInfo -> HieM ()
addUnlocatedEvBind var ci = do
  let go (a,b) (_,c) = (a,S.union b c)
  lift $ modify' $ \s ->
    s { unlocated_ev_binds =
          extendDVarEnv_C go (unlocated_ev_binds s)
                          var (var,S.singleton ci)
      }

getUnlocatedEvBinds :: FastString -> HieM (NodeIdentifiers Type,[HieAST Type])
getUnlocatedEvBinds file = do
  binds <- lift $ gets unlocated_ev_binds
  org <- ask
  let elts = dVarEnvElts binds

      mkNodeInfo (n,ci) = (Right (varName n), IdentifierDetails (Just $ varType n) ci)

      go e@(v,_) (xs,ys) = case nameSrcSpan $ varName v of
        RealSrcSpan spn _
          | srcSpanFile spn == file ->
            let node = Node (mkSourcedNodeInfo org ni) spn []
                ni = NodeInfo mempty [] $ M.fromList [mkNodeInfo e]
              in (xs,node:ys)
        _ -> (mkNodeInfo e : xs,ys)

      (nis,asts) = foldr go ([],[]) elts

  pure $ (M.fromList nis, asts)

lookupAndInsertEntityName :: Name -> HieM ()
lookupAndInsertEntityName name = do
  m <- lift $ gets type_env
  let tyThing = lookupNameEnv m name <|> wiredInNameTyThing_maybe name
  insertEntityInfo name $ maybe (nameEntityInfo name) tyThingEntityInfo tyThing

-- | Insert entity information for an identifier
insertEntityInfo :: Name -> S.Set EntityInfo -> HieM ()
insertEntityInfo ident info = do
  lift $ modify' $ \s ->
    s { entity_infos = M.insertWith S.union ident info (entity_infos s) }

initState :: HieState
initState = HieState emptyNameEnv emptyDVarEnv mempty mempty

class ModifyState a where -- See Note [Name Remapping]
  addSubstitution :: a -> a -> HieState -> HieState

instance ModifyState Name where
  addSubstitution _ _ hs = hs

instance ModifyState Id where
  addSubstitution mono poly hs =
    hs{name_remapping = extendNameEnv (name_remapping hs) (varName mono) poly}

modifyState :: [ABExport] -> HieState -> HieState
modifyState = foldr go id
  where
    go ABE{abe_poly=poly,abe_mono=mono} f
      = addSubstitution mono poly . f

type HieM = ReaderT NodeOrigin (State HieState)

-- | Construct an 'HieFile' from the outputs of the typechecker.
mkHieFile :: MonadIO m
          => ModSummary
          -> TcGblEnv
          -> RenamedSource -> m HieFile
mkHieFile ms ts rs = do
  let src_file = expectJust (ml_hs_file $ ms_location ms)
  src <- liftIO $ BS.readFile src_file
  pure $ mkHieFileWithSource src_file src ms ts rs

-- | Construct an 'HieFile' from the outputs of the typechecker but don't
-- read the source file again from disk.
mkHieFileWithSource :: FilePath
                    -> BS.ByteString
                    -> ModSummary
                    -> TcGblEnv
                    -> RenamedSource -> HieFile
mkHieFileWithSource src_file src ms ts rs =
  let tc_binds = tcg_binds ts
      top_ev_binds = tcg_ev_binds ts
      insts = tcg_insts ts
      tte = tcg_type_env ts
      tcs = tcg_tcs ts
      (asts',arr,entityInfos) = getCompressedAsts tc_binds rs top_ev_binds insts tcs tte in
  HieFile
      { hie_hs_file = src_file
      , hie_module = ms_mod ms
      , hie_types = arr
      , hie_asts = asts'
      -- mkIfaceExports sorts the AvailInfos for stability
      , hie_exports = mkIfaceExports (tcg_exports ts)
      , hie_hs_src = src
      , hie_entity_infos = entityInfos
      }

getCompressedAsts :: TypecheckedSource -> RenamedSource -> Bag EvBind -> [ClsInst] -> [TyCon] -> TypeEnv
  -> (HieASTs TypeIndex, A.Array TypeIndex HieTypeFlat, NameEntityInfo)
getCompressedAsts ts rs top_ev_binds insts tcs tte =
  let (asts, infos) = enrichHie ts rs top_ev_binds insts tcs tte
      add c (a, b) = (a,b,c)
  in add infos $ compressTypes asts

enrichHie :: TypecheckedSource -> RenamedSource -> Bag EvBind -> [ClsInst] -> [TyCon] -> TypeEnv
  -> (HieASTs Type, NameEntityInfo)
enrichHie ts (hsGrp, imports, exports, docs, modName) ev_bs insts tcs tte =
  second entity_infos $ runIdentity $ flip runStateT initState{type_env=tte} $ flip runReaderT SourceInfo $ do
    modName <- toHie (IEC Export <$> modName)
    tasts <- toHie $ fmap (BC RegularBind ModuleScope) ts
    rasts <- processGrp hsGrp
    imps <- toHie $ filter (not . ideclImplicit . ideclExt . unLoc) imports
    exps <- toHie $ fmap (map $ IEC Export . fst) exports
    docs <- toHie docs
    -- Add Instance bindings
    forM_ insts $ \i ->
      addUnlocatedEvBind (is_dfun i) (EvidenceVarBind (EvInstBind False (is_cls_nm i)) ModuleScope Nothing)
    -- Add class parent bindings
    forM_ tcs $ \tc ->
      case tyConClass_maybe tc of
        Nothing -> pure ()
        Just c -> forM_ (classSCSelIds c) $ \v ->
          addUnlocatedEvBind v (EvidenceVarBind (EvInstBind True (className c)) ModuleScope Nothing)
    let spanFile file children = case nonEmpty children of
          Nothing -> realSrcLocSpan (mkRealSrcLoc file 1 1)
          Just children -> mkRealSrcSpan
              (realSrcSpanStart $ nodeSpan (NE.head children))
              (realSrcSpanEnd   $ nodeSpan (NE.last children))

        flat_asts = concat
          [ modName
          , tasts
          , rasts
          , imps
          , exps
          , docs
          ]

        modulify (HiePath file) xs' = do

          top_ev_asts :: [HieAST Type] <- do
            let
              l :: SrcSpanAnnA
              l = noAnnSrcSpan (RealSrcSpan (realSrcLocSpan $ mkRealSrcLoc file 1 1) Strict.Nothing)
            toHie $ EvBindContext ModuleScope Nothing
                  $ L l (EvBinds ev_bs)

          (uloc_evs,more_ev_asts) <- getUnlocatedEvBinds file

          let xs = mergeSortAsts $ xs' ++ top_ev_asts ++ more_ev_asts
              span = spanFile file xs

              moduleInfo = SourcedNodeInfo
                             $ M.singleton SourceInfo
                               $ (simpleNodeInfo "Module" "Module")
                                  {nodeIdentifiers = uloc_evs}

              moduleNode = Node moduleInfo span []

          case mergeSortAsts $ moduleNode : xs of
            [x] -> return x
            xs -> panicDoc "enrichHie: mergeSortAsts retur:ed more than one result" (ppr $ map nodeSpan xs)

    asts' <- sequence
          $ M.mapWithKey modulify
          $ M.fromListWith (++)
          $ map (\x -> (HiePath (srcSpanFile (nodeSpan x)),[x])) flat_asts

    let asts = HieASTs $ resolveTyVarScopes asts'
    return asts

processGrp :: HsGroup GhcRn -> HieM [HieAST Type]
processGrp grp = concatM
      [ toHie $ fmap (RS ModuleScope ) hs_valds grp
      , toHie $ hs_splcds grp
      , toHie $ hs_tyclds grp
      , toHie $ hs_derivds grp
      , toHie $ hs_fixds grp
      , toHie $ hs_defds grp
      , toHie $ hs_fords grp
      , toHie $ hs_warnds grp
      , toHie $ hs_annds grp
      , toHie $ hs_ruleds grp
      , toHie $ hs_docs grp
      ]

getRealSpanA :: EpAnn ann -> Maybe Span
getRealSpanA la = getRealSpan (locA la)

getRealSpan :: SrcSpan -> Maybe Span
getRealSpan (RealSrcSpan sp _) = Just sp
getRealSpan _ = Nothing

grhss_span :: (Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p)))) ~ EpAnn NoEpAnns)
           => GRHSs (GhcPass p) (LocatedA (body (GhcPass p))) -> SrcSpan
grhss_span (GRHSs _ xs bs) = foldl' combineSrcSpans (spanHsLocaLBinds bs) (NE.map getLocA xs)

bindingsOnly :: [Context Name] -> HieM [HieAST a]
bindingsOnly [] = pure []
bindingsOnly (C c n : xs) = do
  org <- ask
  rest <- bindingsOnly xs
  lookupAndInsertEntityName n
  pure $ case nameSrcSpan n of
    RealSrcSpan span _ -> Node (mkSourcedNodeInfo org nodeinfo) span [] : rest
      where nodeinfo = NodeInfo S.empty [] (M.singleton (Right n) info)
            info = mempty{identInfo = S.singleton c}
    _ -> rest

concatM :: Monad m => [m [a]] -> m [a]
concatM xs = concat <$> sequence xs

{- Note [Capturing Scopes and other non local information]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
toHie is a local transformation, but scopes of bindings cannot be known locally,
hence we have to push the relevant info down into the binding nodes.
We use the following types (*Context and *Scoped) to wrap things and
carry the required info
(Maybe Span) always carries the span of the entire binding, including rhs
-}
data Context a = C ContextInfo a -- Used for names and bindings

data RContext a = RC RecFieldContext a
data RFContext a = RFC RecFieldContext (Maybe Span) a
-- ^ context for record fields

data IEContext a = IEC IEType a
-- ^ context for imports/exports

data BindContext a = BC BindType Scope a
-- ^ context for imports/exports

data PatSynFieldContext a = PSC (Maybe Span) a
-- ^ context for pattern synonym fields.

data SigContext a = SC SigInfo a
-- ^ context for type signatures

data SigInfo = SI SigType (Maybe Span)

data SigType = BindSig | ClassSig | InstSig

data EvBindContext a = EvBindContext Scope (Maybe Span) a

data RScoped a = RS Scope a
-- ^ Scope spans over everything to the right of a, (mostly) not
-- including a itself
-- (Includes a in a few special cases like recursive do bindings) or
-- let/where bindings

-- | Pattern scope
data PScoped a = PS (Maybe Span)
                    Scope       -- ^ use site of the pattern
                    Scope       -- ^ pattern to the right of a, not including a
                    a
  deriving (Data) -- Pattern Scope

{- Note [TyVar Scopes]
   ~~~~~~~~~~~~~~~~~~~
Due to -XScopedTypeVariables, type variables can be in scope quite far from
their original binding. We resolve the scope of these type variables
in a separate pass
-}
data TScoped a = TS TyVarScope a -- TyVarScope

data TVScoped a = TVS TyVarScope Scope a -- TyVarScope
-- ^ First scope remains constant
-- Second scope is used to build up the scope of a tyvar over
-- things to its right, ala RScoped

-- | Each element scopes over the elements to the right
listScopes :: Traversable f => Scope -> f (LocatedA a) -> f (RScoped (LocatedA a))
listScopes = fmap snd . mapAccumR (\ (scope :: Scope) pat -> let scope' = combineScopes scope $ mkScope $ getLocA pat in (scope', RS scope pat))

-- | 'listScopes' specialised to 'PScoped' things
patScopes
  :: Traversable f
  => Maybe Span
  -> Scope
  -> Scope
  -> f (LPat (GhcPass p))
  -> f (PScoped (LPat (GhcPass p)))
patScopes rsp useScope patScope =
    fmap (\(RS sc a) -> PS rsp useScope sc a) . listScopes patScope

-- | 'listScopes' specialised to 'TVScoped' things
tvScopes
  :: TyVarScope
  -> Scope
  -> [LHsTyVarBndr flag (GhcPass a)]
  -> [TVScoped (LHsTyVarBndr flag (GhcPass a))]
tvScopes tvScope rhsScope xs =
  map (\(RS sc a)-> TVS tvScope sc a) $ listScopes rhsScope xs

{- Note [Scoping Rules for SigPat]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Explicitly quantified variables in pattern type signatures are not
brought into scope in the rhs, but implicitly quantified variables
are (HsWC and HsIB).
This is unlike other signatures, where explicitly quantified variables
are brought into the RHS Scope
For example
foo :: forall a. ...;
foo = ... -- a is in scope here

bar (x :: forall a. a -> a) = ... -- a is not in scope here
--   ^ a is in scope here (pattern body)

bax (x :: a) = ... -- a is in scope here

This case in handled in the instance for HsPatSigType
-}

instance HasLoc thing => HasLoc (PScoped thing) where
  getHasLoc (PS _ _ _ a) = getHasLoc a

instance HasLoc a => HasLoc (DataDefnCons a) where
  getHasLoc = getHasLocList . toList

instance (HasLoc a, HiePass p) => HasLoc (FamEqn (GhcPass p) a) where
  getHasLoc (FamEqn _ a outer_bndrs b _ c) = case outer_bndrs of
    HsOuterImplicit{} ->
      foldl1' combineSrcSpans (getHasLoc a :| getHasLocList b : getHasLoc c : [])
    HsOuterExplicit{hso_bndrs = tvs} ->
      foldl1' combineSrcSpans (getHasLoc a :| getHasLocList tvs : getHasLocList b : getHasLoc c : [])

instance (HasLoc tm, HasLoc ty) => HasLoc (HsArg (GhcPass p) tm ty) where
  getHasLoc (HsValArg _ tm) = getHasLoc tm
  getHasLoc (HsTypeArg _ ty) = getHasLoc ty
  getHasLoc (HsArgPar sp)  = sp

instance HasLoc (HsDataDefn GhcRn) where
  getHasLoc def@(HsDataDefn{}) = getHasLoc $ dd_cons def
    -- Only used for data family instances, so we only need rhs
    -- Most probably the rest will be unhelpful anyway

-- | The main worker class
-- See Note [Updating HieAst for changes in the GHC AST] for more information
-- on how to add/modify instances for this.
class ToHie a where
  toHie :: a -> HieM [HieAST Type]

-- | Used to collect type info
class HasType a where
  getTypeNode :: a -> HieM [HieAST Type]

instance ToHie Void where
  toHie v = absurd v

instance (ToHie a) => ToHie [a] where
  toHie = concatMapM toHie

instance (ToHie a) => ToHie (NonEmpty a) where
  toHie = concatMapM toHie

instance (ToHie a) => ToHie (DataDefnCons a) where
  toHie = concatMapM toHie

instance (ToHie a) => ToHie (Bag a) where
  toHie = toHie . bagToList

instance (ToHie a) => ToHie (Maybe a) where
  toHie = maybe (pure []) toHie

instance ToHie (IEContext (LocatedA ModuleName)) where
  toHie (IEC c (L (EpAnn (EpaSpan (RealSrcSpan span _)) _ _) mname)) = do
      org <- ask
      pure [Node (mkSourcedNodeInfo org $ NodeInfo S.empty [] idents) span []]
    where details = mempty{identInfo = S.singleton (IEThing c)}
          idents = M.singleton (Left mname) details
  toHie _ = pure []

instance ToHie (Context (Located a)) => ToHie (Context (LocatedN a)) where
  toHie (C c (L l a)) = toHie (C c (L (locA l) a))

instance ToHie (Context (Located a)) => ToHie (Context (LocatedA a)) where
  toHie (C c (L l a)) = toHie (C c (L (locA l) a))

instance ToHie (Context (Located Var)) where
  toHie c = case c of
      C context (L (RealSrcSpan span _) name')
        | varUnique name' == mkBuiltinUnique 1 -> pure []
          -- `mkOneRecordSelector` makes a field var using this unique, which we ignore
        | otherwise -> do
          m <- lift $ gets name_remapping
          org <- ask
          let name = case lookupNameEnv m (varName name') of
                Just var -> var
                Nothing-> name'
              ty = case isDataConId_maybe name' of
                      Nothing -> varType name'
                      Just dc -> dataConNonlinearType dc
          -- insert the entity info for the name into the entity_infos map
          insertEntityInfo (varName name) $ idEntityInfo name
          insertEntityInfo (varName name') $ idEntityInfo name'
          pure
            [Node
              (mkSourcedNodeInfo org $ NodeInfo S.empty [] $
                M.singleton (Right $ varName name)
                            (IdentifierDetails (Just ty)
                                               (S.singleton context)))
              span
              []]
      C (EvidenceVarBind i _ sp)  (L _ name) -> do
        addUnlocatedEvBind name (EvidenceVarBind i ModuleScope sp)
        pure []
      _ -> pure []

instance ToHie (Context (Located Name)) where
  toHie c = case c of
      C context (L (RealSrcSpan span _) name')
        | nameUnique name' == mkBuiltinUnique 1 -> pure []
          -- `mkOneRecordSelector` makes a field var using this unique, which we ignore
        | otherwise -> do
          m <- lift $ gets name_remapping
          org <- ask
          let name = case lookupNameEnv m name' of
                Just var -> varName var
                Nothing -> name'
          -- insert the entity info for the name into the entity_infos map
          lookupAndInsertEntityName name
          lookupAndInsertEntityName name'
          pure
            [Node
              (mkSourcedNodeInfo org $ NodeInfo S.empty [] $
                M.singleton (Right name)
                            (IdentifierDetails Nothing
                                               (S.singleton context)))
              span
              []]
      _ -> pure []

instance ToHie (Context (Located (WithUserRdr Name))) where
  toHie (C c (L l (WithUserRdr _ n))) = toHie $ C c (L l n)

evVarsOfTermList :: EvTerm -> [EvId]
evVarsOfTermList (EvExpr e)         = exprSomeFreeVarsList isEvVar e
evVarsOfTermList (EvTypeable _ ev)  =
  case ev of
    EvTypeableTyCon _ e   -> concatMap evVarsOfTermList e
    EvTypeableTyApp e1 e2 -> concatMap evVarsOfTermList [e1,e2]
    EvTypeableTrFun e1 e2 e3 -> concatMap evVarsOfTermList [e1,e2,e3]
    EvTypeableTyLit e     -> evVarsOfTermList e
evVarsOfTermList (EvFun{}) = []

instance ToHie (EvBindContext (LocatedA TcEvBinds)) where
  toHie (EvBindContext sc sp (L span (EvBinds bs)))
    = concatMapM go $ bagToList bs
    where
      go evbind = do
          let evDeps = evVarsOfTermList $ eb_rhs evbind
              depNames = EvBindDeps $ map varName evDeps
          concatM $
            [ toHie (C (EvidenceVarBind (EvLetBind depNames) (combineScopes sc (mkScope span)) sp)
                                        (L span $ eb_lhs evbind))
            , toHie $ map (C EvidenceVarUse . L span) $ evDeps
            ]
  toHie _ = pure []

instance ToHie (LocatedA HsWrapper) where
  toHie (L osp wrap)
    = case wrap of
        (WpLet bs)      -> toHie $ EvBindContext (mkScope osp) (getRealSpanA osp) (L osp bs)
        (WpCompose a b) -> concatM $
          [toHie (L osp a), toHie (L osp b)]
        (WpFun a b _)   -> concatM $
          [toHie (L osp a), toHie (L osp b)]
        (WpEvLam a) ->
          toHie $ C (EvidenceVarBind EvWrapperBind (mkScope osp) (getRealSpanA osp))
                $ L osp a
        (WpEvApp a) ->
          concatMapM (toHie . C EvidenceVarUse . L osp) $ evVarsOfTermList a
        _               -> pure []

instance HiePass p => HasType (LocatedA (HsBind (GhcPass p))) where
  getTypeNode (L spn bind) =
    case hiePass @p of
      HieRn -> makeNode bind (locA spn)
      HieTc ->  case bind of
        FunBind{fun_id = name} -> makeTypeNode bind (locA spn) (varType $ unLoc name)
        _ -> makeNode bind (locA spn)

instance HiePass p => HasType (LocatedA (Pat (GhcPass p))) where
  getTypeNode (L spn pat) =
    case hiePass @p of
      HieRn -> makeNodeA pat spn
      HieTc -> makeTypeNodeA pat spn (hsPatType pat)

-- | This instance tries to construct 'HieAST' nodes which include the type of
-- the expression. It is not yet possible to do this efficiently for all
-- expression forms, so we skip filling in the type for those inputs.
--
-- See Note [Computing the type of every node in the tree]
instance HiePass p => HasType (LocatedA (HsExpr (GhcPass p))) where
  getTypeNode (L spn e) =
    case hiePass @p of
      HieRn -> fallback
      HieTc -> case computeType e of
          Just ty -> makeTypeNodeA e spn ty
          Nothing -> fallback
    where
      fallback :: HieM [HieAST Type]
      fallback = makeNodeA e spn

      -- Skip computing the type of some expressions for performance reasons.
      --
      -- See impact on Haddock output (esp. missing type annotations or links)
      -- before skipping more kinds of expressions. See impact on Haddock
      -- performance before computing the types of more expressions.
      --
      -- See Note [Computing the type of every node in the tree]
      computeType :: HsExpr GhcTc -> Maybe Type
      computeType e = case e of
        HsApp{} -> Nothing
        HsAppType{} -> Nothing
        NegApp{} -> Nothing
        HsPar _ e -> computeLType e
        ExplicitTuple{} -> Nothing
        HsIf _ _ t f -> computeLType t <|> computeLType f
        HsLet _ _ body -> computeLType body
        RecordCon con_expr _ _ -> computeType con_expr
        ExprWithTySig _ e _ -> computeLType e
        HsPragE _ _ e -> computeLType e
        XExpr (ExpandedThingTc thing e)
          | OrigExpr (HsGetField{}) <- thing -- for record-dot-syntax
          -> Just (hsExprType e)
          | otherwise -> computeType e
        XExpr (HsTick _ e) -> computeLType e
        XExpr (HsBinTick _ _ e) -> computeLType e
        e -> Just (hsExprType e)

      computeLType :: LHsExpr GhcTc -> Maybe Type
      computeLType (L _ e) = computeType e

{- Note [Computing the type of every node in the tree]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC.Iface.Ext.Ast we decorate every node in the AST with its
type, computed by `hsExprType` applied to that node.  So it's
important that `hsExprType` takes roughly constant time per node.
There are three cases to consider:

1. For many nodes (e.g. HsVar, HsDo, HsCase) it is easy to get their
   type -- e.g. it is stored in the node, or in sub-node thereof.

2. For some nodes (e.g. HsPar, HsTick, HsIf) the type of the node is
   the type of a child, so we can recurse, fast.  We don't expect the
   nesting to be very deep, so while this is theoretically non-linear,
   we don't expect it to be a problem in practice.

3. A very few nodes (e.g. HsApp) are more troublesome because we need to
   take the type of a child, and then do some non-trivial processing.
   To be conservative on computation, we decline to decorate these
   nodes, using `fallback` instead.

The function `computeType e` returns `Just t` if we can find the type
of `e` cheaply, and `Nothing` otherwise.  The base `Nothing` cases
are the troublesome ones in (3) above. Hopefully we can ultimately
get rid of them all.

See #16233

-}

data HiePassEv p where
  HieRn :: HiePassEv 'Renamed
  HieTc :: HiePassEv 'Typechecked

class ( HiePass (NoGhcTcPass p)
      , NoGhcTcPass p ~ 'Renamed
      , ModifyState (IdGhcP p)
      , Data (GRHS  (GhcPass p) (LocatedA (HsExpr (GhcPass p))))
      , Data (Match (GhcPass p) (LocatedA (HsExpr (GhcPass p))))
      , Data (Match (GhcPass p) (LocatedA (HsCmd  (GhcPass p))))
      , Data (Stmt  (GhcPass p) (LocatedA (HsExpr (GhcPass p))))
      , Data (Stmt  (GhcPass p) (LocatedA (HsCmd  (GhcPass p))))
      , Data (HsExpr (GhcPass p))
      , Data (HsCmd  (GhcPass p))
      , Data (HsCmdTop (GhcPass p))
      , Data (GRHS (GhcPass p) (LocatedA (HsCmd (GhcPass p))))
      , Data (HsUntypedSplice (GhcPass p))
      , Data (HsLocalBinds (GhcPass p))
      , Data (FieldOcc (GhcPass p))
      , Data (HsTupArg (GhcPass p))
      , Data (IPBind (GhcPass p))
      , ToHie (Context (Located (IdGhcP p)))
      , ToHie (Context (Located (IdOccGhcP p)))
      , Anno (IdGhcP p) ~ SrcSpanAnnN
      , Anno (IdOccGhcP p) ~ SrcSpanAnnN
      , Typeable p
      )
      => HiePass p where
  hiePass :: HiePassEv p

instance HiePass 'Renamed where
  hiePass = HieRn
instance HiePass 'Typechecked where
  hiePass = HieTc

instance ToHie (Context (Located NoExtField)) where
  toHie _ = pure []

type AnnoBody p body
  = ( Anno (Match (GhcPass p) (LocatedA (body (GhcPass p))))
                   ~ SrcSpanAnnA
    , Anno [LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))]
                   ~ SrcSpanAnnLW
    , Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                   ~ EpAnn NoEpAnns
    , Anno (StmtLR (GhcPass p) (GhcPass p) (LocatedA (body (GhcPass p)))) ~ SrcSpanAnnA

    , Data (body (GhcPass p))
    , Data (Match (GhcPass p) (LocatedA (body (GhcPass p))))
    , Data (GRHS  (GhcPass p) (LocatedA (body (GhcPass p))))
    , Data (Stmt  (GhcPass p) (LocatedA (body (GhcPass p))))
    )

instance HiePass p => ToHie (BindContext (LocatedA (HsBind (GhcPass p)))) where
  toHie (BC context scope b@(L span bind)) =
    concatM $ getTypeNode b : case bind of
      FunBind{fun_id = name, fun_matches = matches, fun_ext = ext} ->
        [ toHie $ C (ValBind context scope $ getRealSpanA span) name
        , toHie matches
        , case hiePass @p of
            HieTc | (wrap, _) <- ext -> toHie $ L span wrap
            _ -> pure []
        ]
      PatBind{pat_lhs = lhs, pat_rhs = rhs} ->
        [ toHie $ PS (getRealSpan (locA span)) scope NoScope lhs
        , toHie rhs
        ]
      VarBind{var_rhs = expr} ->
        [ toHie expr
        ]
      XHsBindsLR ext -> case hiePass @p of
        HieTc
          | AbsBinds{ abs_exports = xs, abs_binds = binds
                    , abs_ev_binds = ev_binds
                    , abs_ev_vars = ev_vars } <- ext
          ->
            [  lift (modify (modifyState xs)) >> -- Note [Name Remapping]
                    (toHie $ fmap (BC context scope) binds)
            , toHie $ map (L span . abe_wrap) xs
            , toHie $
                map (EvBindContext (mkScope span) (getRealSpanA span)
                    . L span) ev_binds
            , toHie $
                map (C (EvidenceVarBind EvSigBind
                                        (mkScope span)
                                        (getRealSpanA span))
                    . L span) ev_vars
            ]
      PatSynBind _ psb ->
        [ toHie $ L (locA span) psb -- PatSynBinds only occur at the top level
        ]

instance ( HiePass p
         , AnnoBody p body
         , ToHie (LocatedA (body (GhcPass p)))
         ) => ToHie (MatchGroup (GhcPass p) (LocatedA (body (GhcPass p)))) where
  toHie mg = case mg of
    MG{ mg_alts = (L span alts) } ->
      local (setOrigin origin) $ concatM
        [ locOnly (locA span)
        , toHie alts
        ]
    where origin = case hiePass @p of
             HieRn -> mg_ext mg
             HieTc -> mg_origin $ mg_ext mg

setOrigin :: Origin -> NodeOrigin -> NodeOrigin
setOrigin FromSource _ = SourceInfo
setOrigin (Generated {}) _ = GeneratedInfo

instance HiePass p => ToHie (Located (PatSynBind (GhcPass p) (GhcPass p))) where
    toHie (L sp psb) = concatM $ case psb of
      PSB{psb_id=var, psb_args=dets, psb_def=pat, psb_dir=dir} ->
        [ toHie $ C (Decl PatSynDec $ getRealSpan sp) var
        , toHie $ toBind dets
        , toHie $ PS Nothing lhsScope patScope pat
        , toHie dir
        ]
        where
          lhsScope = combineScopes varScope detScope
          varScope = mkScope var
          patScope = mkScope $ getLoc pat
          detScope = case dets of
            (PrefixCon args) -> foldr combineScopes NoScope $ map mkScope args
            (InfixCon a b) -> combineScopes (mkScope a) (mkScope b)
            (RecCon r) -> foldr go NoScope r
          go (RecordPatSynField (a :: FieldOcc (GhcPass p)) b) c = combineScopes c
            $ combineScopes (mkScope (foLabel a)) (mkScope b)
          detSpan = case detScope of
            LocalScope a -> Just a
            _ -> Nothing
          toBind (PrefixCon args) = PrefixCon $ map (C Use) args
          toBind (InfixCon a b) = InfixCon (C Use a) (C Use b)
          toBind (RecCon r) = RecCon $ map (PSC detSpan) r

instance ToHie a => ToHie (WithUserRdr a) where
  toHie (WithUserRdr _ a) = toHie a

instance HiePass p => ToHie (HsPatSynDir (GhcPass p)) where
  toHie dir = case dir of
    ExplicitBidirectional mg -> toHie mg
    _ -> pure []

instance ( HiePass p
         , Data (body (GhcPass p))
         , AnnoBody p body
         , ToHie (LocatedA (body (GhcPass p)))
         ) => ToHie (LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))) where
  toHie (L span m ) = concatM $ makeNodeA m span : case m of
    Match{m_ctxt=mctx, m_pats = L _ pats, m_grhss = grhss } ->
      [ toHieHsMatchContext @p mctx
      , let rhsScope = mkScope $ grhss_span grhss
          in toHie $ patScopes Nothing rhsScope NoScope pats
      , toHie grhss
      ]

toHieHsMatchContext :: forall p. HiePass p => HsMatchContext (LIdP (NoGhcTc (GhcPass p)))
                                           -> HieM [HieAST Type]
toHieHsMatchContext ctxt
  = case ctxt of
      FunRhs{mc_fun=name} -> toHie $ C MatchBind (get_name name)
      StmtCtxt a          -> toHieHsStmtContext @p a
      _                   -> pure []
  where
      -- See a paragraph about Haddock in #20415.
    get_name :: LIdP (NoGhcTc (GhcPass p)) -> LocatedN Name
    get_name name = case hiePass @p of
                      HieRn -> name
                      HieTc -> name

toHieHsStmtContext :: forall p. HiePass p => HsStmtContext (LIdP (NoGhcTc (GhcPass p)))
                                          -> HieM [HieAST Type]
toHieHsStmtContext ctxt
  = case ctxt of
      PatGuard a      -> toHieHsMatchContext @p a
      ParStmtCtxt a   -> toHieHsStmtContext  @p a
      TransStmtCtxt a -> toHieHsStmtContext  @p a
      _               -> pure []

instance HiePass p => ToHie (PScoped (LocatedA (Pat (GhcPass p)))) where
  toHie (PS rsp scope pscope lpat@(L ospan opat)) =
    concatM $ getTypeNode lpat : case opat of
      OrPat _ pats ->
        map (toHie . PS rsp scope pscope) (NE.toList pats)
      WildPat _ ->
        []
      VarPat _ lname ->
        [ toHie $ C (PatternBind scope pscope rsp) lname
        ]
      LazyPat _ p ->
        [ toHie $ PS rsp scope pscope p
        ]
      AsPat _ lname pat ->
        [ toHie $ C (PatternBind scope
                                 (combineScopes (mkScope pat) pscope)
                                 rsp)
                    lname
        , toHie $ PS rsp scope pscope pat
        ]
      ParPat _ pat ->
        [ toHie $ PS rsp scope pscope pat
        ]
      BangPat _ pat ->
        [ toHie $ PS rsp scope pscope pat
        ]
      ListPat _ pats ->
        [ toHie $ patScopes rsp scope pscope pats
        ]
      TuplePat _ pats _ ->
        [ toHie $ patScopes rsp scope pscope pats
        ]
      SumPat _ pat _ _ ->
        [ toHie $ PS rsp scope pscope pat
        ]
      ConPat {pat_con = con, pat_args = dets, pat_con_ext = ext} ->
        case hiePass @p of
          HieTc ->
            [ toHie $ C Use $ fmap conLikeName con
            , toHie $ contextify dets
            , let ev_binds = cpt_binds ext
                  ev_vars = cpt_dicts ext
                  wrap = cpt_wrap ext
                  evscope = mkScope ospan `combineScopes` scope `combineScopes` pscope
                 in concatM [ toHie $ EvBindContext scope rsp $ L ospan ev_binds
                            , toHie $ L ospan wrap
                            , toHie $ map (C (EvidenceVarBind EvPatternBind evscope rsp)
                                          . L ospan) ev_vars
                            ]
            ]
          HieRn ->
            [ toHie $ C Use (fmap getName con)
            , toHie $ contextify dets
            ]
      ViewPat _ expr pat ->
        [ toHie expr
        , toHie $ PS rsp scope pscope pat
        ]
      SplicePat _ sp ->
        [ toHie $ L ospan sp
        ]
      LitPat _ _ ->
        []
      NPat _ (L loc lit) _ eq ->
        [ toHie $ L (l2l loc :: SrcSpanAnnA) lit
        , toHieSyntax (L ospan eq)
        ]
      NPlusKPat _ n (L loc lit) _ ord _ ->
        [ toHie $ C (PatternBind scope pscope rsp) n
        , toHie $ L (l2l loc :: SrcSpanAnnA) lit
        , toHieSyntax (L ospan ord)
        ]
      SigPat _ pat sig ->
        [ toHie $ PS rsp scope pscope pat
        , case hiePass @p of
            HieTc ->
              let cscope = mkScope pat in
                toHie $ TS (ResolvedScopes [cscope, scope, pscope])
                           sig
            HieRn -> pure []
        ]
      EmbTyPat _ tp ->
        [ toHie $ TS (ResolvedScopes [scope, pscope]) tp
        ]
      InvisPat _ tp ->
        [ toHie $ TS (ResolvedScopes [scope, pscope]) tp
        ]
      XPat e ->
        case hiePass @p of
          HieRn -> case e of
            HsPatExpanded _ p -> [ toHie $ PS rsp scope pscope (L ospan p) ]
          HieTc -> case e of
            CoPat wrap pat _ ->
              [ toHie $ L ospan wrap
              , toHie $ PS rsp scope pscope $ (L ospan pat)
              ]
            ExpansionPat _ p -> [ toHie $ PS rsp scope pscope (L ospan p) ]
    where
      contextify :: a ~ LPat (GhcPass p) => HsConDetails a (HsRecFields (GhcPass p) a)
                 -> HsConDetails (PScoped a) (RContext (HsRecFields (GhcPass p) (PScoped a)))
      contextify (PrefixCon args) =
        PrefixCon (patScopes rsp scope pscope args)
      contextify (InfixCon a b) = InfixCon a' b'
        where Pair a' b' = patScopes rsp scope pscope (Pair a b)
      contextify (RecCon r) = RecCon $ RC RecFieldMatch $ contextify_rec r
      contextify_rec (HsRecFields x fds a) = HsRecFields x (map go scoped_fds) a
        where
          go :: RScoped (LocatedA (HsFieldBind id a1))
                      -> LocatedA (HsFieldBind id (PScoped a1)) -- AZ
          go (RS fscope (L spn (HsFieldBind x lbl pat pun))) =
            L spn $ HsFieldBind x lbl (PS rsp scope fscope pat) pun
          scoped_fds = listScopes pscope fds

toHieSyntax :: forall p. HiePass p => LocatedA (SyntaxExpr (GhcPass p)) -> HieM [HieAST Type]
toHieSyntax s = local (const GeneratedInfo) $ case hiePass @p of
  HieRn -> toHie s
  HieTc -> toHie s

instance ToHie (LocatedA SyntaxExprRn) where
  toHie (L mspan (SyntaxExprRn expr)) = toHie (L mspan expr)
  toHie (L _ NoSyntaxExprRn) = pure []

instance ToHie (LocatedA SyntaxExprTc) where
  toHie (L mspan (SyntaxExprTc expr w1 w2)) = concatM
      [ toHie (L mspan expr)
      , concatMapM (toHie . L mspan) w1
      , toHie (L mspan w2)
      ]
  toHie (L _ NoSyntaxExprTc) = pure []

instance ToHie (TScoped (HsPatSigType GhcRn)) where
  toHie (TS sc (HsPS (HsPSRn wcs tvs) body@(L span _))) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) (wcs++tvs)
      , toHie body
      ]
  -- See Note [Scoping Rules for SigPat]

instance ToHie (TScoped (HsTyPat GhcRn)) where
  toHie (TS sc (HsTP (HsTPRn wcs imp_tvs exp_tvs) body@(L span _))) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) (wcs ++ imp_tvs ++ exp_tvs)
      , toHie body
      ]

instance ( ToHie (LocatedA (body (GhcPass p)))
         , HiePass p
         , AnnoBody p body
         ) => ToHie (GRHSs (GhcPass p) (LocatedA (body (GhcPass p)))) where
  toHie grhs = concatM $ case grhs of
    GRHSs _ grhss binds ->
     [ toHie grhss
     , toHie $ RS (mkScope $ grhss_span grhs) binds
     ]

instance ( ToHie (LocatedA (body (GhcPass p)))
         , HiePass p
         , AnnoBody p body
         ) => ToHie (LocatedAn NoEpAnns (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))) where
  toHie (L span g) = concatM $ makeNodeA g span : case g of
    GRHS _ guards body ->
      [ toHie $ listScopes (mkScope body) guards
      , toHie body
      ]

{-
Note [Source locations for implicit function calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While calls to e.g. 'fromString' with -XOverloadedStrings do not actually
appear in the source code, giving their HsWrapper the location of the
overloaded bit of syntax that triggered them is useful for assigning
their type class evidence uses to the right location in the HIE AST.
Without this, we only get type class instance information under the
expected top-level node if the type had to be inferred. (#23540)

We currently handle the following constructors with this in mind,
all largely in the renamer as their locations are normally inherited by
the typechecker:

  * HsOverLit, where we assign the SrcSpan of the overloaded literal
    to ol_from_fun.
  * HsDo, where we give the SrcSpan of the entire do block to each
    ApplicativeStmt.
  * Expanded (via ExpandedThingRn) ExplicitList{}, where we give the SrcSpan of the original
    list expression to the 'fromListN' call.

In order for the implicit function calls to not be confused for actual
occurrences of functions in the source code, most of this extra information
is put under 'GeneratedInfo'.
-}

whenPostTc :: forall p t m. (HiePass p, Applicative t, Monoid m) => ((p ~ 'Typechecked) => t m) -> t m
whenPostTc a = case hiePass @p of
  HieTc -> a
  HieRn -> pure mempty

-- | Helper function for a common pattern where we are only interested in
-- implicit evidence information: runs only post-typecheck and marks the
-- current 'NodeOrigin' as generated.
whenPostTcGen :: forall p. HiePass p => ((p ~ 'Typechecked) => HieM [HieAST Type]) -> HieM [HieAST Type]
whenPostTcGen a = local (const GeneratedInfo) $ whenPostTc @p a

instance HiePass p => ToHie (LocatedA (HsOverLit (GhcPass p))) where
  toHie (L span (OverLit x _)) = whenPostTcGen @p $ concatM $ case x of
      OverLitTc _ witness _ ->
        [ toHie (L span witness)
        ]
      -- See Note [Source locations for implicit function calls]

instance HiePass p => ToHie (LocatedA (HsExpr (GhcPass p))) where
  toHie e@(L mspan oexpr) = concatM $ getTypeNode e : case oexpr of
      HsVar _ (L _ var) ->
        [ toHie $ C Use (L mspan var)
             -- Patch up var location since typechecker removes it
        ]
      HsOverLabel {} -> []
      HsIPVar _ _ -> []
      HsOverLit _ o ->
        [ toHie (L mspan o)
        ]
      HsLit _ _ -> []
      HsLam _ _ mg ->
        [ toHie mg
        ]
      HsApp _ a b ->
        [ toHie a
        , toHie b
        ]
      HsAppType _ expr sig ->
        [ toHie expr
        , toHie $ TS (ResolvedScopes []) sig
        ]
      OpApp _ a b c ->
        [ toHie a
        , toHie b
        , toHie c
        ]
      NegApp _ a _ ->
        [ toHie a
        ]
      HsPar _ a ->
        [ toHie a
        ]
      SectionL _ a b ->
        [ toHie a
        , toHie b
        ]
      SectionR _ a b ->
        [ toHie a
        , toHie b
        ]
      ExplicitTuple _ args _ ->
        [ toHie args
        ]
      ExplicitSum _ _ _ expr ->
        [ toHie expr
        ]
      HsCase _ expr matches ->
        [ toHie expr
        , toHie matches
        ]
      HsIf _ a b c ->
        [ toHie a
        , toHie b
        , toHie c
        ]
      HsMultiIf _ grhss ->
        [ toHie grhss
        ]
      HsLet _ binds expr ->
        [ toHie $ RS (mkScope expr) binds
        , toHie expr
        ]
      HsDo _ _ (L ispan stmts) ->
        [ locOnly (locA ispan)
        , toHie $ listScopes NoScope stmts
        ]
      ExplicitList _ exprs ->
        [ toHie exprs
        ]
      RecordCon { rcon_con = con, rcon_flds = binds} ->
        [ toHie $ C Use $ con_name
        , toHie $ RC RecFieldAssign $ binds
        ]
        where
          con_name :: LocatedN Name
          con_name = case hiePass @p of       -- Like ConPat
                       HieRn -> fmap getName con
                       HieTc -> fmap conLikeName con
      RecordUpd { rupd_expr = expr
                , rupd_flds = RegularRecUpdFields { recUpdFields = upds } }->
        [ toHie expr
        , case hiePass @p of
          HieRn -> toHie $ map (RC RecFieldAssign) upds
          HieTc -> toHie $ map (RC RecFieldAssign) upds
        ]
      RecordUpd { rupd_expr = expr
                , rupd_flds = OverloadedRecUpdFields {} }->
        [ toHie expr
        ]
      ExprWithTySig _ expr sig ->
        [ toHie expr
        , toHie $ TS (ResolvedScopes [mkScope expr]) sig
        ]
      ArithSeq enum _ info ->
        [ toHie info
        , whenPostTcGen @p $ toHie (L mspan enum)
        ]
      HsPragE _ _ expr ->
        [ toHie expr
        ]
      HsProc _ pat cmdtop ->
        [ toHie $ PS Nothing (mkScope cmdtop) NoScope pat
        , toHie cmdtop
        ]
      HsStatic _ expr ->
        [ toHie expr
        ]
      HsEmbTy _ ty ->
        [ toHie $ TS (ResolvedScopes []) ty
        ]
      HsQual _ ctx body ->
        [ toHie ctx
        , toHie body
        ]
      HsForAll x tele body -> case hiePass @p of
        HieRn ->
          [ toHieForAllTele tele (getLocA body)
          ]
        HieTc -> dataConCantHappen x
      HsFunArr x mult arg res -> case hiePass @p of
        HieRn ->
          [ toHie (multAnnToHsExpr mult)
          , toHie arg
          , toHie res
          ]
        HieTc -> dataConCantHappen x
      HsTypedBracket xbracket b -> case hiePass @p of
        HieRn ->
          [ toHie b
          ]
        HieTc | HsBracketTc _ _ _ p <- xbracket ->
          [ toHie b
          , toHie p
          ]
      HsUntypedBracket xbracket b -> case hiePass @p of
        HieRn ->
            [ toHie b
            , toHie xbracket
            ]
        HieTc | HsBracketTc q _ _ p <- xbracket ->
          [ toHie q
          , toHie p
          ]
      HsTypedSplice _ x ->
        [ toHie x
        ]
      HsUntypedSplice _ x ->
        [ toHie $ L mspan x
        ]
      HsGetField {} -> []
      HsProjection {} -> []
      HsHole _ -> [] -- there is a hole here, but that causes trouble
      XExpr x -> case hiePass @p of
        HieTc -> case x of
          WrapExpr w a
            -> [ toHie $ L mspan a
               , toHie (L mspan w) ]
          ExpandedThingTc _ e
            -> [ toHie (L mspan e) ]
          ConLikeTc con _ _
            -> [ toHie $ C Use $ L mspan $ conLikeName con ]
          HsTick _ expr
            -> [ toHie expr
               ]
          HsBinTick _ _ expr
            -> [ toHie expr
               ]
          HsRecSelTc fld
            -> [ toHie $ RFC RecFieldOcc Nothing (L mspan fld)
               ]
        HieRn -> case x of
          HsRecSelRn fld
            -> [ toHie $ RFC RecFieldOcc Nothing (L mspan fld)
               ]
          _ -> []

-- NOTE: no longer have the location
instance HiePass p => ToHie (HsTupArg (GhcPass p)) where
  toHie arg = concatM $ case arg of
    Present _ expr ->
      [ toHie expr
      ]
    Missing _ -> []

instance ( ToHie (LocatedA (body (GhcPass p)))
         , AnnoBody p body
         , HiePass p
         ) => ToHie (RScoped (LocatedA (Stmt (GhcPass p) (LocatedA (body (GhcPass p)))))) where
  toHie (RS scope (L span stmt)) = concatM $ node : case stmt of
      LastStmt _ body _ _ ->
        [ toHie body
        ]
      BindStmt monad pat body ->
        [ toHie $ PS (getRealSpan $ getLocA body) scope NoScope pat
        , toHie body
        , whenPostTcGen @p $
            toHieSyntax $ L span (xbstc_bindOp monad)
        ]
      BodyStmt _ body monad alternative ->
        [ toHie body
        , whenPostTc @p $
            concatMapM (toHieSyntax . L span) [monad, alternative]
        ]
      LetStmt _ binds ->
        [ toHie $ RS scope binds
        ]
      ParStmt _ parstmts _ _ ->
        [ concatMapM (\(ParStmtBlock _ stmts _ _) ->
                          toHie $ listScopes NoScope stmts)
                     parstmts
        ]
      TransStmt {trS_stmts = stmts, trS_using = using, trS_by = by} ->
        [ toHie $ listScopes scope stmts
        , toHie using
        , toHie by
        ]
      RecStmt {recS_stmts = L _ stmts} ->
        [ toHie $ map (RS $ combineScopes scope (mkScope (locA span))) stmts
        ]
      XStmtLR x -> case hiePass @p of
        HieRn -> extApplicativeStmt x
        HieTc -> extApplicativeStmt x
    where
      node = case hiePass @p of
        HieTc -> makeNodeA stmt span
        HieRn -> makeNodeA stmt span
      extApplicativeStmt :: ApplicativeStmt (GhcPass p) (GhcPass p) -> [ReaderT NodeOrigin (State HieState) [HieAST Type]]
      extApplicativeStmt (ApplicativeStmt _ stmts _) =
        [ concatMapM (toHie . RS scope . snd) stmts
        , let applicative_or_functor = map fst stmts
           in whenPostTcGen @p $
                concatMapM (toHieSyntax . L span) applicative_or_functor
        ]

instance HiePass p => ToHie (RScoped (HsLocalBinds (GhcPass p))) where
  toHie (RS scope binds) = concatM $ makeNode binds (spanHsLocaLBinds binds) : case binds of
      EmptyLocalBinds _ -> []
      HsIPBinds _ ipbinds -> case ipbinds of
        IPBinds evbinds xs -> let sc = combineScopes scope $ scopeHsLocaLBinds binds
                                  sp :: SrcSpanAnnA
                                  sp = noAnnSrcSpan $ spanHsLocaLBinds binds in
          [
            case hiePass @p of
              HieTc -> toHie $ EvBindContext sc (getRealSpan $ locA sp) $ L sp evbinds
              HieRn -> pure []
          , toHie $ map (RS sc) xs
          ]
      HsValBinds _ valBinds ->
        [
          toHie $ RS (combineScopes scope (scopeHsLocaLBinds binds))
                      valBinds
        ]

scopeHsLocaLBinds :: HsLocalBinds (GhcPass p) -> Scope
scopeHsLocaLBinds (HsValBinds _ (ValBinds _ bs sigs))
  = foldr combineScopes NoScope (bsScope ++ sigsScope)
  where
    bsScope :: [Scope]
    bsScope = map (mkScope . getLoc) bs
    sigsScope :: [Scope]
    sigsScope = map (mkScope . getLocA) sigs
scopeHsLocaLBinds (HsValBinds _ (XValBindsLR (NValBinds bs sigs)))
  = foldr combineScopes NoScope (bsScope ++ sigsScope)
  where
    bsScope :: [Scope]
    bsScope = map (mkScope . getLoc) $ concatMap snd bs
    sigsScope :: [Scope]
    sigsScope = map (mkScope . getLocA) sigs

scopeHsLocaLBinds (HsIPBinds _ (IPBinds _ bs))
  = foldr combineScopes NoScope (map (mkScope . getLoc) bs)
scopeHsLocaLBinds (EmptyLocalBinds _) = NoScope

instance HiePass p => ToHie (RScoped (LocatedA (IPBind (GhcPass p)))) where
  toHie (RS scope (L sp bind@(IPBind v _ expr))) = concatM $ makeNodeA bind sp : case hiePass @p of
    HieRn -> [toHie expr]
    HieTc -> [ toHie $ C (EvidenceVarBind EvImplicitBind scope (getRealSpanA sp))
                       $ L sp v
             , toHie expr
             ]

instance HiePass p => ToHie (RScoped (HsValBindsLR (GhcPass p) (GhcPass p))) where
  toHie (RS sc v) = concatM $ case v of
    ValBinds _ binds sigs ->
      [ toHie $ fmap (BC RegularBind sc) binds
      , toHie $ fmap (SC (SI BindSig Nothing)) sigs
      ]
    XValBindsLR x -> [ toHie $ RS sc x ]

instance HiePass p => ToHie (RScoped (NHsValBindsLR (GhcPass p))) where
  toHie (RS sc (NValBinds binds sigs)) = concatM $
    [ toHie (concatMap (map (BC RegularBind sc) . snd) binds)
    , toHie $ fmap (SC (SI BindSig Nothing)) sigs
    ]

instance ( ToHie arg , HasLoc arg , Data arg
         , HiePass p ) => ToHie (RContext (HsRecFields (GhcPass p) arg)) where
  toHie (RC c (HsRecFields _ fields _)) = toHie $ map (RC c) fields

instance ( ToHie (RFContext label)
         , ToHie arg, HasLoc arg, Data arg
         , Data label
         ) => ToHie (RContext (LocatedA (HsFieldBind label arg))) where
  toHie (RC c (L span recfld)) = concatM $ makeNode recfld (locA span) : case recfld of
    HsFieldBind _ label expr _ ->
      [ toHie $ RFC c (getRealSpan $ getHasLoc expr) label
      , toHie expr
      ]

instance HiePass p => ToHie (RFContext (LocatedA (FieldOcc (GhcPass p)))) where
  toHie (RFC c rhs (L nspan f)) = concatM $
    case hiePass @p of
      HieRn ->
        case f of
          FieldOcc _ fld ->
            [toHie $ C (RecField c rhs) (L (locA nspan) $ unLoc fld)]
      HieTc ->
        case f of
          FieldOcc _ fld ->
            [toHie $ C (RecField c rhs) (L (locA nspan) $ unLoc fld)]


instance HiePass p => ToHie (RScoped (ApplicativeArg (GhcPass p))) where
  toHie (RS sc (ApplicativeArgOne _ pat expr _)) = concatM
    [ toHie $ PS Nothing sc NoScope pat
    , toHie expr
    ]
  toHie (RS sc (ApplicativeArgMany _ stmts _ pat _)) = concatM
    [ toHie $ listScopes NoScope stmts
    , toHie $ PS Nothing sc NoScope pat
    ]

instance (ToHie arg, ToHie rec) => ToHie (HsConDetails arg rec) where
  toHie (PrefixCon args) = toHie args
  toHie (RecCon rec) = toHie rec
  toHie (InfixCon a b) = concatM [ toHie a, toHie b]

instance ToHie (HsConDeclGADTDetails GhcRn) where
  toHie (PrefixConGADT _ args) = toHie args
  toHie (RecConGADT _ rec) = toHie rec

instance HiePass p => ToHie (LocatedAn NoEpAnns (HsCmdTop (GhcPass p))) where
  toHie (L span top) = concatM $ makeNodeA top span : case top of
    HsCmdTop _ cmd ->
      [ toHie cmd
      ]

instance HiePass p => ToHie (LocatedA (HsCmd (GhcPass p))) where
  toHie (L span cmd) = concatM $ makeNodeA cmd span : case cmd of
      HsCmdArrApp _ a b _ _ ->
        [ toHie a
        , toHie b
        ]
      HsCmdArrForm _ a _ cmdtops ->
        [ toHie a
        , toHie cmdtops
        ]
      HsCmdApp _ a b ->
        [ toHie a
        , toHie b
        ]
      HsCmdPar _ a ->
        [ toHie a
        ]
      HsCmdCase _ expr alts ->
        [ toHie expr
        , toHie alts
        ]
      HsCmdLam _ _ alts ->
        [ toHie alts
        ]
      HsCmdIf _ _ a b c ->
        [ toHie a
        , toHie b
        , toHie c
        ]
      HsCmdLet _ binds cmd' ->
        [ toHie $ RS (mkScope cmd') binds
        , toHie cmd'
        ]
      HsCmdDo _ (L ispan stmts) ->
        [ locOnly (locA ispan)
        , toHie $ listScopes NoScope stmts
        ]
      XCmd _ -> []

instance ToHie (TyClGroup GhcRn) where
  toHie TyClGroup{ group_tyclds = classes
                 , group_roles  = roles
                 , group_kisigs = sigs
                 , group_instds = instances } =
    concatM
    [ toHie classes
    , toHie sigs
    , toHie roles
    , toHie instances
    ]

instance ToHie (LocatedA (TyClDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      FamDecl {tcdFam = fdecl} ->
        [ toHie ((L span fdecl) :: LFamilyDecl GhcRn)
        ]
      SynDecl {tcdLName = name, tcdTyVars = vars, tcdRhs = typ} ->
        [ toHie $ C (Decl SynDec $ getRealSpanA span) name
        , toHie $ TS (ResolvedScopes [mkScope $ getLocA typ]) vars
        , toHie typ
        ]
      DataDecl {tcdLName = name, tcdTyVars = vars, tcdDataDefn = defn} ->
        [ toHie $ C (Decl DataDec $ getRealSpanA span) name
        , toHie $ TS (ResolvedScopes [quant_scope, rhs_scope]) vars
        , toHie defn
        ]
        where
          quant_scope = mkScope $ fromMaybe (noLocA []) $ dd_ctxt defn
          rhs_scope = sig_sc `combineScopes` con_sc `combineScopes` deriv_sc
          sig_sc = maybe NoScope mkScope $ dd_kindSig defn
          con_sc = foldr combineScopes NoScope $ mkScope <$> dd_cons defn
          deriv_sc = foldr combineScopes NoScope $ mkScope <$> dd_derivs defn
      ClassDecl { tcdCtxt = context
                , tcdLName = name
                , tcdTyVars = vars
                , tcdFDs = deps
                , tcdSigs = sigs
                , tcdMeths = meths
                , tcdATs = typs
                , tcdATDefs = deftyps
                } ->
        [ toHie $ C (Decl ClassDec $ getRealSpanA span) name
        , toHie context
        , toHie $ TS (ResolvedScopes [context_scope, rhs_scope]) vars
        , toHie deps
        , toHie $ map (SC $ SI ClassSig $ getRealSpanA span) sigs
        , toHie $ fmap (BC InstanceBind ModuleScope) meths
        , toHie typs
        , concatMapM (locOnly . getLocA) deftyps
        , toHie deftyps
        ]
        where
          context_scope = mkScope $ fromMaybe (noLocA []) context
          rhs_scope = foldl1' combineScopes $ NE.map mkScope
            ( getHasLocList deps :| getHasLocList sigs : getHasLocList meths : getHasLocList typs : getHasLocList deftyps : [])

instance ToHie (LocatedA (FamilyDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      FamilyDecl _ info _ name vars _ sig inj ->
        [ toHie $ C (Decl FamDec $ getRealSpanA span) name
        , toHie $ TS (ResolvedScopes [rhsSpan]) vars
        , toHie info
        , toHie $ RS injSpan sig
        , toHie inj
        ]
        where
          rhsSpan = sigSpan `combineScopes` injSpan
          sigSpan = mkScope $ getLocA sig
          injSpan = maybe NoScope (mkScope . getLocA) inj

instance ToHie (FamilyInfo GhcRn) where
  toHie (ClosedTypeFamily (Just eqns)) = concatM $
    [ concatMapM (locOnly . getLocA) eqns
    , toHie $ map go eqns
    ]
    where
      go (L l ib) = TS (ResolvedScopes [mkScope l]) ib
  toHie _ = pure []

instance ToHie (RScoped (LocatedAn NoEpAnns (FamilyResultSig GhcRn))) where
  toHie (RS sc (L span sig)) = concatM $ makeNodeA sig span : case sig of
      NoSig _ ->
        []
      KindSig _ k ->
        [ toHie k
        ]
      TyVarSig _ bndr ->
        [ toHie $ TVS (ResolvedScopes [sc]) NoScope bndr
        ]

instance ToHie (LocatedA (FunDep GhcRn)) where
  toHie (L span fd@(FunDep _ lhs rhs)) = concatM $
    [ makeNode fd (locA span)
    , toHie $ map (C Use) lhs
    , toHie $ map (C Use) rhs
    ]


instance ToHie (TScoped (FamEqn GhcRn (HsDataDefn GhcRn))) where
  toHie (TS _ f) = toHie f

instance ToHie (TScoped (FamEqn GhcRn (LocatedA (HsType GhcRn)))) where
  toHie (TS _ f) = toHie f

instance (ToHie rhs, HasLoc rhs)
    => ToHie (FamEqn GhcRn rhs) where
  toHie fe@(FamEqn _ var outer_bndrs pats _ rhs) = concatM $
    [ toHie $ C (Decl InstDec $ getRealSpan $ getHasLoc fe) var
    , toHie $ TVS (ResolvedScopes []) scope outer_bndrs
    , toHie pats
    , toHie rhs
    ]
    where scope = combineScopes patsScope rhsScope
          patsScope = mkScope (getHasLocList pats)
          rhsScope = mkScope (getHasLoc rhs)

instance ToHie (LocatedAn NoEpAnns (InjectivityAnn GhcRn)) where
  toHie (L span ann) = concatM $ makeNodeA ann span : case ann of
      InjectivityAnn _ lhs rhs ->
        [ toHie $ C Use lhs
        , toHie $ map (C Use) rhs
        ]

instance ToHie (HsDataDefn GhcRn) where
  toHie (HsDataDefn _ ctx _ mkind cons derivs) = concatM
    [ toHie ctx
    , toHie mkind
    , toHie cons
    , toHie derivs
    ]

instance ToHie (Located [LocatedAn NoEpAnns (HsDerivingClause GhcRn)]) where
  toHie (L span clauses) = concatM
    [ locOnly span
    , toHie clauses
    ]

instance ToHie (LocatedAn NoEpAnns (HsDerivingClause GhcRn)) where
  toHie (L span cl) = concatM $ makeNodeA cl span : case cl of
      HsDerivingClause _ strat dct ->
        [ toHie (RS (mkScope dct) <$> strat)
        , toHie dct
        ]

instance ToHie (LocatedC (DerivClauseTys GhcRn)) where
  toHie (L span dct) = concatM $ makeNodeA dct span : case dct of
      DctSingle _ ty -> [ toHie $ TS (ResolvedScopes []) ty ]
      DctMulti _ tys -> [ toHie $ map (TS (ResolvedScopes [])) tys ]

instance ToHie (RScoped (LocatedAn NoEpAnns (DerivStrategy GhcRn))) where
  toHie (RS sc (L span strat)) = concatM $ makeNodeA strat span : case strat of
      StockStrategy _ -> []
      AnyclassStrategy _ -> []
      NewtypeStrategy _ -> []
      ViaStrategy s -> [ toHie (TS (ResolvedScopes [sc]) s) ]

instance ToHie (LocatedP OverlapMode) where
  toHie (L span _) = locOnly (locA span)

instance ToHie (LocatedA (ConDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl (locA span) : case decl of
      ConDeclGADT { con_names = names, con_bndrs = L outer_bndrs_loc outer_bndrs
                  , con_mb_cxt = ctx, con_g_args = args, con_res_ty = typ
                  , con_doc = doc} ->
        [ toHie $ C (Decl ConDec $ getRealSpanA span) <$> names
        , case outer_bndrs of
            HsOuterImplicit{hso_ximplicit = imp_vars} ->
              bindingsOnly $ map (C $ TyVarBind (mkScope outer_bndrs_loc) resScope)
                             imp_vars
            HsOuterExplicit{hso_bndrs = exp_bndrs} ->
              toHie $ tvScopes resScope NoScope exp_bndrs
        , toHie ctx
        , toHie args
        , toHie typ
        , toHie doc
        ]
        where
          rhsScope = combineScopes argsScope tyScope
          ctxScope = maybe NoScope mkScope ctx
          argsScope = case args of
            PrefixConGADT _ xs -> scaled_args_scope xs
            RecConGADT _ x     -> mkScope x
          tyScope = mkScope typ
          resScope = ResolvedScopes [ctxScope, rhsScope]
      ConDeclH98 { con_name = name, con_ex_tvs = qvars
                 , con_mb_cxt = ctx, con_args = dets
                 , con_doc = doc} ->
        [ toHie $ C (Decl ConDec $ getRealSpan (locA span)) name
        , toHie $ tvScopes (ResolvedScopes []) rhsScope qvars
        , toHie ctx
        , toHie dets
        , toHie doc
        ]
        where
          rhsScope = combineScopes ctxScope argsScope
          ctxScope = maybe NoScope mkScope ctx
          argsScope = case dets of
            PrefixCon xs -> scaled_args_scope xs
            InfixCon a b -> scaled_args_scope [a, b]
            RecCon x     -> mkScope x
    where scaled_args_scope :: [HsConDeclField GhcRn] -> Scope
          scaled_args_scope = foldr combineScopes NoScope . map (mkScope . cdf_type)

instance ToHie (LocatedL [LocatedA (HsConDeclRecField GhcRn)]) where
  toHie (L span decls) = concatM $
    [ locOnly (locA span)
    , toHie decls
    ]

instance ToHie (HsConDeclField GhcRn) where
  toHie (CDF { cdf_multiplicity, cdf_type, cdf_doc }) = concatM
    [ toHie (multAnnToHsType cdf_multiplicity)
    , toHie cdf_type
    , toHie cdf_doc
    ]

instance ToHie (TScoped (HsWildCardBndrs GhcRn (LocatedA (HsSigType GhcRn)))) where
  toHie (TS sc (HsWC names a)) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) names
      , toHie $ TS sc a
      ]
    where span = getHasLoc a

instance ToHie (TScoped (HsWildCardBndrs GhcRn (LocatedA (HsType GhcRn)))) where
  toHie (TS sc (HsWC names a)) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) names
      , toHie a
      ]
    where span = getHasLoc a

instance ToHie (LocatedA (StandaloneKindSig GhcRn)) where
  toHie (L sp sig) = concatM [makeNodeA sig sp, toHie sig]

instance ToHie (StandaloneKindSig GhcRn) where
  toHie sig = concatM $ case sig of
    StandaloneKindSig _ name typ ->
      [ toHie $ C TyDecl name
      , toHie $ TS (ResolvedScopes []) typ
      ]

instance HiePass p => ToHie (SigContext (LocatedA (Sig (GhcPass p)))) where
  toHie (SC (SI styp msp) (L sp sig)) =
    case hiePass @p of
      HieTc -> pure []
      HieRn -> concatM $ makeNodeA sig sp : case sig of
        TypeSig _ names typ ->
          [ toHie $ map (C TyDecl) names
          , toHie $ TS (UnresolvedScope (map unLoc names) Nothing) typ
          ]
        PatSynSig _ names typ ->
          [ toHie $ map (C TyDecl) names
          , toHie $ TS (UnresolvedScope (map unLoc names) Nothing) typ
          ]
        ClassOpSig _ _ names typ ->
          [ case styp of
              ClassSig -> toHie $ map (C $ ClassTyDecl $ getRealSpanA sp) names
              _  -> toHie $ map (C $ TyDecl) names
          , toHie $ TS (UnresolvedScope (map unLoc names) msp) typ
          ]
        FixSig _ fsig ->
          [ toHie $ L sp fsig
          ]
        InlineSig _ name _ ->
          [ toHie $ (C Use) name
          ]
        SpecSig _ name typs _ ->
          [ toHie $ (C Use) name
          , toHie $ map (TS (ResolvedScopes [])) typs
          ]
        SpecSigE _ bndrs spec_e _ ->
          [ toHieRuleBndrs (locA sp) (mkScope spec_e) bndrs
          , toHie spec_e
          ]
        SpecInstSig _ typ ->
          [ toHie $ TS (ResolvedScopes []) typ
          ]
        MinimalSig _ form ->
          [ toHie form
          ]
        SCCFunSig _ name mtxt ->
          [ toHie $ (C Use) name
          , maybe (pure []) (locOnly . getLocA) mtxt
          ]
        CompleteMatchSig _ names typ ->
          [ toHie $ map (C Use) names
          , toHie $ fmap (C Use) typ
          ]
        XSig _ -> []

instance ToHie (TScoped (LocatedA (HsSigType GhcRn))) where
  toHie (TS tsc (L span t@HsSig{sig_bndrs=bndrs,sig_body=body})) = concatM $ makeNodeA t span :
      [ toHie (TVS tsc (mkScope span) bndrs)
      , toHie body
      ]

-- Check this
instance Data flag => ToHie (TVScoped (HsOuterTyVarBndrs flag GhcRn)) where
  toHie (TVS tsc sc bndrs) = case bndrs of
    HsOuterImplicit xs -> bindingsOnly $ map (C $ TyVarBind sc tsc) xs
    HsOuterExplicit _ xs -> toHie $ tvScopes tsc sc xs

toHieForAllTele ::  HsForAllTelescope GhcRn -> SrcSpan -> HieM [HieAST Type]
toHieForAllTele (HsForAllVis { hsf_vis_bndrs = bndrs }) loc =
  toHie $ tvScopes (ResolvedScopes []) (mkScope loc) bndrs
toHieForAllTele (HsForAllInvis { hsf_invis_bndrs = bndrs }) loc =
  toHie $ tvScopes (ResolvedScopes []) (mkScope loc) bndrs

instance ToHie (LocatedA (HsType GhcRn)) where
  toHie (L span t) = concatM $ makeNode t (locA span) : case t of
      HsForAllTy _ tele body ->
        [ toHieForAllTele tele (getLocA body)
        , toHie body
        ]
      HsQualTy _ ctx body ->
        [ toHie ctx
        , toHie body
        ]
      HsTyVar _ _ var ->
        [ toHie $ C Use var
        ]
      HsAppTy _ a b ->
        [ toHie a
        , toHie b
        ]
      HsAppKindTy _ ty ki ->
        [ toHie ty
        , toHie ki
        ]
      HsFunTy _ w a b ->
        [ toHie (multAnnToHsType w)
        , toHie a
        , toHie b
        ]
      HsListTy _ a ->
        [ toHie a
        ]
      HsTupleTy _ _ tys ->
        [ toHie tys
        ]
      HsSumTy _ tys ->
        [ toHie tys
        ]
      HsOpTy _ _prom a op b ->
        [ toHie a
        , toHie $ C Use op
        , toHie b
        ]
      HsParTy _ a ->
        [ toHie a
        ]
      HsIParamTy _ ip ty ->
        [ toHie ip
        , toHie ty
        ]
      HsKindSig _ a b ->
        [ toHie a
        , toHie b
        ]
      HsSpliceTy _ a ->
        [ toHie $ L span a
        ]
      HsDocTy _ a doc ->
        [ toHie a
        , toHie doc
        ]
      HsExplicitListTy _ _ tys ->
        [ toHie tys
        ]
      HsExplicitTupleTy _ _ tys ->
        [ toHie tys
        ]
      HsTyLit _ _ -> []
      HsWildCardTy _ -> []
      HsStarTy _ _ -> []
      XHsType _ -> []

instance (ToHie tm, ToHie ty) => ToHie (HsArg (GhcPass p) tm ty) where
  toHie (HsValArg _ tm) = toHie tm
  toHie (HsTypeArg _ ty) = toHie ty
  toHie (HsArgPar sp) = locOnly sp

instance Data flag => ToHie (TVScoped (LocatedA (HsTyVarBndr flag GhcRn))) where
  toHie (TVS tsc sc (L span bndr)) =
    concatM $ makeNodeA bndr span : (name' ++ kind')
    where
      name' = case hsBndrVar bndr of
        HsBndrWildCard _ -> []
        HsBndrVar _ tv   -> [toHie $ C (TyVarBind sc tsc) tv]
      kind' = case hsBndrKind bndr of
        HsBndrNoKind _ -> []
        HsBndrKind _ k -> [toHie k]

instance ToHie (TScoped (LHsQTyVars GhcRn)) where
  toHie (TS sc (HsQTvs implicits vars)) = concatM $
    [ bindingsOnly bindings
    , toHie $ tvScopes sc NoScope vars
    ]
    where
      varLoc = getHasLocList vars
      bindings = map (C $ TyVarBind (mkScope varLoc) sc) implicits

instance ToHie (LocatedC [LocatedA (HsType GhcRn)]) where
  toHie (L span tys) = concatM $
      [ locOnly (locA span)
      , toHie tys
      ]

instance HiePass p => ToHie (LocatedC [LocatedA (HsExpr (GhcPass p))]) where
  toHie (L span exprs) = concatM $
    [ locOnly (locA span)
    , toHie exprs
    ]

instance ToHie (LocatedA (HsConDeclRecField GhcRn)) where
  toHie (L span field) = concatM $ makeNode field (locA span) : case field of
      HsConDeclRecField _ fields typ ->
        [ toHie $ map (RFC RecFieldDecl (getRealSpan $ getHasLoc $ cdf_type typ)) fields
        , toHie typ
        ]

instance ToHie (LHsExpr a) => ToHie (ArithSeqInfo a) where
  toHie (From expr) = toHie expr
  toHie (FromThen a b) = concatM $
    [ toHie a
    , toHie b
    ]
  toHie (FromTo a b) = concatM $
    [ toHie a
    , toHie b
    ]
  toHie (FromThenTo a b c) = concatM $
    [ toHie a
    , toHie b
    , toHie c
    ]

instance ToHie (LocatedA (SpliceDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      SpliceDecl _ splice _ ->
        [ toHie splice
        ]

instance ToHie (HsQuote GhcRn) where
  toHie (ExpBr _ e)  = toHie e
  toHie (PatBr _ b)  = toHie (PS Nothing NoScope NoScope b)
  toHie (DecBrL {} ) = pure []
  toHie (DecBrG _ decls) = processGrp decls
  toHie (TypBr _ ty) = toHie ty
  toHie (VarBr {} )  = pure []

instance ToHie PendingRnSplice where
  toHie (PendingRnSplice _ _ e) = toHie e

instance ToHie PendingTcSplice where
  toHie (PendingTcSplice _ e) = toHie e

instance (HiePass p, Data (IdGhcP p))
  => ToHie (GenLocated SrcSpanAnnL (BooleanFormula (GhcPass p))) where
    toHie (L span form) =  concatM $ makeNode form (locA span) : case form of
      Var a ->
        [ toHie $ C Use a
        ]
      And forms ->
        [ toHie forms
        ]
      Or forms ->
        [ toHie forms
        ]
      Parens f ->
        [ toHie f
        ]

instance ToHie (LocatedAn NoEpAnns HsIPName) where
  toHie (L span e) = makeNodeA e span

instance HiePass p => ToHie (LocatedA (HsUntypedSplice (GhcPass p))) where
  toHie (L span sp) = concatM $ makeNodeA sp span : case sp of
      HsUntypedSpliceExpr _ expr ->
        [ toHie expr
        ]
      HsQuasiQuote _ _ ispanFs ->
        [ locOnly (getLocA ispanFs)
        ]

instance ToHie (LocatedA (RoleAnnotDecl GhcRn)) where
  toHie (L span annot) = concatM $ makeNodeA annot span : case annot of
      RoleAnnotDecl _ var roles ->
        [ toHie $ C Use var
        , concatMapM (locOnly . getLocA) roles
        ]

instance ToHie (LocatedA (InstDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      ClsInstD _ d ->
        [ toHie $ L span d
        ]
      DataFamInstD _ d ->
        [ toHie $ L span d
        ]
      TyFamInstD _ d ->
        [ toHie $ L span d
        ]

instance ToHie (LocatedA (ClsInstDecl GhcRn)) where
  toHie (L span decl) = concatM
    [ toHie $ TS (ResolvedScopes [mkScope span]) $ cid_poly_ty decl
    , toHie $ fmap (BC InstanceBind ModuleScope) $ cid_binds decl
    , toHie $ map (SC $ SI InstSig $ getRealSpanA span) $ cid_sigs decl
    , concatMapM (locOnly . getLocA) $ cid_tyfam_insts decl
    , toHie $ cid_tyfam_insts decl
    , concatMapM (locOnly . getLocA) $ cid_datafam_insts decl
    , toHie $ cid_datafam_insts decl
    , toHie $ cid_overlap_mode decl
    ]

instance ToHie (LocatedA (DataFamInstDecl GhcRn)) where
  toHie (L sp (DataFamInstDecl d)) = toHie $ TS (ResolvedScopes [mkScope sp]) d

instance ToHie (LocatedA (TyFamInstDecl GhcRn)) where
  toHie (L sp (TyFamInstDecl _ d)) = toHie $ TS (ResolvedScopes [mkScope sp]) d


instance HiePass p => ToHie (Context (FieldOcc (GhcPass p))) where
  toHie (C c (FieldOcc _ l)) = toHie (C c l)

instance HiePass p => ToHie (PatSynFieldContext (RecordPatSynField (GhcPass p))) where
  toHie (PSC sp (RecordPatSynField a b)) = concatM $
    [ toHie $ C (RecField RecFieldDecl sp) a
    , toHie $ C Use b
    ]

instance ToHie (LocatedA (DerivDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      DerivDecl _ typ strat overlap ->
        [ toHie $ TS (ResolvedScopes []) typ
        , toHie $ (RS (mkScope span) <$> strat)
        , toHie overlap
        ]

instance ToHie (LocatedA (FixitySig GhcRn)) where
  toHie (L span sig) = concatM $ makeNodeA sig span : case sig of
      FixitySig _ vars _ ->
        [ toHie $ map (C Use) vars
        ]

instance ToHie (LocatedA (DefaultDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      DefaultDecl _ cl typs ->
        [ maybe (pure []) (toHie . C Use) cl
        , toHie typs
        ]

instance ToHie (LocatedA (ForeignDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      ForeignImport {fd_name = name, fd_sig_ty = sig, fd_fi = fi} ->
        [ toHie $ C (ValBind RegularBind ModuleScope $ getRealSpanA span) name
        , toHie $ TS (ResolvedScopes []) sig
        , toHie fi
        ]
      ForeignExport {fd_name = name, fd_sig_ty = sig, fd_fe = fe} ->
        [ toHie $ C Use name
        , toHie $ TS (ResolvedScopes []) sig
        , toHie fe
        ]

instance ToHie (ForeignImport GhcRn) where
  toHie (CImport (L c _) (L a _) (L b _) _ _) = concatM $
    [ locOnlyE a
    , locOnlyE b
    , locOnlyE c
    ]

instance ToHie (ForeignExport GhcRn) where
  toHie (CExport (L b _) (L a _)) = concatM $
    [ locOnlyE a
    , locOnlyE b
    ]

instance ToHie (LocatedA (WarnDecls GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      Warnings _ warnings ->
        [ toHie warnings
        ]

instance ToHie (LocatedA (WarnDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl (locA span) : case decl of
      Warning _ vars _ ->
        [ toHie $ map (C Use) vars
        ]

instance ToHie (LocatedA (AnnDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      HsAnnotation _ prov expr ->
        [ toHie prov
        , toHie expr
        ]

instance ToHie (AnnProvenance GhcRn) where
  toHie (ValueAnnProvenance a) = toHie $ C Use a
  toHie (TypeAnnProvenance a) = toHie $ C Use a
  toHie ModuleAnnProvenance = pure []

instance ToHie (LocatedA (RuleDecls GhcRn)) where
  toHie (L span decl) = concatM $ makeNodeA decl span : case decl of
      HsRules _ rules ->
        [ toHie rules
        ]

instance ToHie (LocatedA (RuleDecl GhcRn)) where
  toHie (L span r@(HsRule { rd_name = rname, rd_bndrs = bndrs
                          , rd_lhs = exprA, rd_rhs = exprB }))
    = concatM
        [ makeNodeA r span
        , locOnly $ getLocA rname
        , toHieRuleBndrs (locA span) scope bndrs
        , toHie exprA
        , toHie exprB
        ]
    where
      scope = mkScope exprA `combineScopes` mkScope exprB

toHieRuleBndrs :: SrcSpan -> Scope -> RuleBndrs GhcRn -> HieM [HieAST Type]
toHieRuleBndrs span body_sc (RuleBndrs { rb_tyvs = tybndrs, rb_tmvs = bndrs })
    = concatM [ toHie $ fmap (tvScopes (ResolvedScopes []) full_sc) tybndrs
              , toHie $ map (RS $ mkScope (locA span)) bndrs ]
    where
      full_sc = bndrs_sc `combineScopes` body_sc
      bndrs_sc = maybe NoScope mkScope (listToMaybe bndrs)

instance ToHie (RScoped (LocatedAn NoEpAnns (RuleBndr GhcRn))) where
  toHie (RS sc (L span bndr)) = concatM $ makeNodeA bndr span : case bndr of
      RuleBndr _ var ->
        [ toHie $ C (ValBind RegularBind sc Nothing) var
        ]
      RuleBndrSig _ var typ ->
        [ toHie $ C (ValBind RegularBind sc Nothing) var
        , toHie $ TS (ResolvedScopes [sc]) typ
        ]

instance ToHie (LocatedA (ImportDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl (locA span) : case decl of
      ImportDecl { ideclName = name, ideclAs = as, ideclImportList = hidden } ->
        [ toHie $ IEC Import name
        , toHie $ fmap (IEC ImportAs) as
        , maybe (pure []) goIE hidden
        ]
    where
      goIE (hiding, (L sp liens)) = concatM $
        [ locOnly (locA sp)
        , toHie $ map (IEC c) liens
        ]
        where
         -- ROMES:TODO: I notice some overlap here with Iface types, eventually
         -- we could join these
         c = case hiding of
               Exactly -> Import
               EverythingBut -> ImportHiding


instance ToHie (IEContext (LocatedA (IE GhcRn))) where
  toHie (IEC c (L span ie)) = concatM $ makeNode ie (locA span) : case ie of
      IEVar _ n _ ->
        [ toHie $ IEC c n
        ]
      IEThingAbs _ n _ ->
        [ toHie $ IEC c n
        ]
      IEThingAll _ n _ ->
        [ toHie $ IEC c n
        ]
      IEThingWith _ n _ ns _ ->
        [ toHie $ IEC c n
        , toHie $ map (IEC c) ns
        ]
      IEModuleContents _ n ->
        [ toHie $ IEC c n
        ]
      IEGroup _ _ d -> [toHie d]
      IEDoc _ d -> [toHie d]
      IEDocNamed _ _ -> []

instance ToHie (IEContext (LocatedA (IEWrappedName GhcRn))) where
  toHie (IEC c (L span iewn)) = concatM $ makeNodeA iewn span : case iewn of
      IEDefault _ (L l p) ->
        [ toHie $ C (IEThing c) (L l p)
        ]
      IEName _ (L l n) ->
        [ toHie $ C (IEThing c) (L l n)
        ]
      IEPattern _ (L l p) ->
        [ toHie $ C (IEThing c) (L l p)
        ]
      IEType _ (L l n) ->
        [ toHie $ C (IEThing c) (L l n)
        ]

instance ToHie (IEContext (Located RecFieldInfo)) where
  toHie (IEC c (L span info)) = concatM
      [ makeNode info span
      , toHie $ C (IEThing c) $ L span (flSelector $ recFieldLabel info)
      ]

instance ToHie (LocatedA (DocDecl GhcRn)) where
  toHie (L span d) = concatM $ makeNodeA d span : case d of
    DocCommentNext d -> [ toHie d ]
    DocCommentPrev d -> [ toHie d ]
    DocCommentNamed _ d -> [ toHie d ]
    DocGroup _ d -> [ toHie d ]

instance ToHie (LHsDoc GhcRn) where
  toHie (L span d@(WithHsDocIdentifiers _ ids)) =
    concatM $ makeNode d span : [toHie $ map (C Use) ids]
