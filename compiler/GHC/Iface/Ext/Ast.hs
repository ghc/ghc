{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
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

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
Main functions for .hie file generation
-}

#include "HsVersions.h"

module GHC.Iface.Ext.Ast ( mkHieFile, mkHieFileWithSource, getCompressedAsts, enrichHie) where

import GHC.Utils.Outputable(ppr)

import GHC.Prelude

import GHC.Types.Avail            ( Avails )
import GHC.Data.Bag               ( Bag, bagToList )
import GHC.Types.Basic
import GHC.Data.BooleanFormula
import GHC.Core.Class             ( FunDep, className, classSCSelIds )
import GHC.Core.Utils             ( exprType )
import GHC.Core.ConLike           ( conLikeName, ConLike(RealDataCon) )
import GHC.Core.TyCon             ( TyCon, tyConClass_maybe )
import GHC.Core.FVs
import GHC.Core.DataCon           ( dataConNonlinearType )
import GHC.HsToCore               ( deSugarExpr )
import GHC.Types.FieldLabel
import GHC.Hs
import GHC.Driver.Env
import GHC.Utils.Monad            ( concatMapM, liftIO )
import GHC.Types.Id               ( isDataConId_maybe )
import GHC.Types.Name             ( Name, nameSrcSpan, nameUnique )
import GHC.Types.Name.Env         ( NameEnv, emptyNameEnv, extendNameEnv, lookupNameEnv )
import GHC.Types.SrcLoc
import GHC.Tc.Utils.Zonk          ( hsLitType, hsPatType )
import GHC.Core.Type              ( mkVisFunTys, Type )
import GHC.Core.Predicate
import GHC.Core.InstEnv
import GHC.Builtin.Types          ( mkListTy, mkSumTy )
import GHC.Tc.Types
import GHC.Tc.Types.Evidence
import GHC.Types.Var              ( Id, Var, EvId, varName, setVarName, varType, varUnique )
import GHC.Types.Var.Env
import GHC.Builtin.Uniques
import GHC.Iface.Make             ( mkIfaceExports )
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Data.FastString

import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils

import GHC.Unit.Module            ( ModuleName, ml_hs_file )
import GHC.Unit.Module.ModSummary

import qualified Data.Array as A
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Data                  ( Data, Typeable )
import Data.Void                  ( Void, absurd )
import Control.Monad              ( forM_ )
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class  ( lift )

{- Note [Updating HieAst for changes in the GHC AST]

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

Here are is an extract from the `ToHie` instance for (LHsExpr (GhcPass p)):

  toHie e@(L mspan oexpr) = concatM $ getTypeNode e : case oexpr of
      HsVar _ (L _ var) ->
        [ toHie $ C Use (L mspan var)
             -- Patch up var location since typechecker removes it
        ]
      HsConLikeOut _ con ->
        [ toHie $ C Use $ L mspan $ conLikeName con
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
                         , Maybe LHsDocString )
type TypecheckedSource = LHsBinds GhcTc


{- Note [Name Remapping]
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

initState :: HieState
initState = HieState emptyNameEnv emptyDVarEnv

class ModifyState a where -- See Note [Name Remapping]
  addSubstitution :: a -> a -> HieState -> HieState

instance ModifyState Name where
  addSubstitution _ _ hs = hs

instance ModifyState Id where
  addSubstitution mono poly hs =
    hs{name_remapping = extendNameEnv (name_remapping hs) (varName mono) poly}

modifyState :: ModifyState (IdP p) => [ABExport p] -> HieState -> HieState
modifyState = foldr go id
  where
    go ABE{abe_poly=poly,abe_mono=mono} f
      = addSubstitution mono poly . f
    go _ f = f

type HieM = ReaderT NodeOrigin (StateT HieState Hsc)

-- | Construct an 'HieFile' from the outputs of the typechecker.
mkHieFile :: ModSummary
          -> TcGblEnv
          -> RenamedSource -> Hsc HieFile
mkHieFile ms ts rs = do
  let src_file = expectJust "mkHieFile" (ml_hs_file $ ms_location ms)
  src <- liftIO $ BS.readFile src_file
  mkHieFileWithSource src_file src ms ts rs

-- | Construct an 'HieFile' from the outputs of the typechecker but don't
-- read the source file again from disk.
mkHieFileWithSource :: FilePath
                    -> BS.ByteString
                    -> ModSummary
                    -> TcGblEnv
                    -> RenamedSource -> Hsc HieFile
mkHieFileWithSource src_file src ms ts rs = do
  let tc_binds = tcg_binds ts
      top_ev_binds = tcg_ev_binds ts
      insts = tcg_insts ts
      tcs = tcg_tcs ts
  (asts', arr) <- getCompressedAsts tc_binds rs top_ev_binds insts tcs
  return $ HieFile
      { hie_hs_file = src_file
      , hie_module = ms_mod ms
      , hie_types = arr
      , hie_asts = asts'
      -- mkIfaceExports sorts the AvailInfos for stability
      , hie_exports = mkIfaceExports (tcg_exports ts)
      , hie_hs_src = src
      }

getCompressedAsts :: TypecheckedSource -> RenamedSource -> Bag EvBind -> [ClsInst] -> [TyCon]
  -> Hsc (HieASTs TypeIndex, A.Array TypeIndex HieTypeFlat)
getCompressedAsts ts rs top_ev_binds insts tcs = do
  asts <- enrichHie ts rs top_ev_binds insts tcs
  return $ compressTypes asts

enrichHie :: TypecheckedSource -> RenamedSource -> Bag EvBind -> [ClsInst] -> [TyCon]
  -> Hsc (HieASTs Type)
enrichHie ts (hsGrp, imports, exports, _) ev_bs insts tcs =
  flip evalStateT initState $ flip runReaderT SourceInfo $ do
    tasts <- toHie $ fmap (BC RegularBind ModuleScope) ts
    rasts <- processGrp hsGrp
    imps <- toHie $ filter (not . ideclImplicit . unLoc) imports
    exps <- toHie $ fmap (map $ IEC Export . fst) exports
    -- Add Instance bindings
    forM_ insts $ \i ->
      addUnlocatedEvBind (is_dfun i) (EvidenceVarBind (EvInstBind False (is_cls_nm i)) ModuleScope Nothing)
    -- Add class parent bindings
    forM_ tcs $ \tc ->
      case tyConClass_maybe tc of
        Nothing -> pure ()
        Just c -> forM_ (classSCSelIds c) $ \v ->
          addUnlocatedEvBind v (EvidenceVarBind (EvInstBind True (className c)) ModuleScope Nothing)
    let spanFile file children = case children of
          [] -> realSrcLocSpan (mkRealSrcLoc file 1 1)
          _ -> mkRealSrcSpan (realSrcSpanStart $ nodeSpan $ head children)
                             (realSrcSpanEnd   $ nodeSpan $ last children)

        flat_asts = concat
          [ tasts
          , rasts
          , imps
          , exps
          ]

        modulify (HiePath file) xs' = do

          top_ev_asts <-
            toHie $ EvBindContext ModuleScope Nothing
                  $ L (RealSrcSpan (realSrcLocSpan $ mkRealSrcLoc file 1 1) Nothing)
                  $ EvBinds ev_bs

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
  where
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
      ]

getRealSpan :: SrcSpan -> Maybe Span
getRealSpan (RealSrcSpan sp _) = Just sp
getRealSpan _ = Nothing

grhss_span :: GRHSs (GhcPass p) body -> SrcSpan
grhss_span (GRHSs _ xs bs) = foldl' combineSrcSpans (getLoc bs) (map getLoc xs)

bindingsOnly :: [Context Name] -> HieM [HieAST a]
bindingsOnly [] = pure []
bindingsOnly (C c n : xs) = do
  org <- ask
  rest <- bindingsOnly xs
  pure $ case nameSrcSpan n of
    RealSrcSpan span _ -> Node (mkSourcedNodeInfo org nodeinfo) span [] : rest
      where nodeinfo = NodeInfo S.empty [] (M.singleton (Right n) info)
            info = mempty{identInfo = S.singleton c}
    _ -> rest

concatM :: Monad m => [m [a]] -> m [a]
concatM xs = concat <$> sequence xs

{- Note [Capturing Scopes and other non local information]
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
  deriving (Typeable, Data) -- Pattern Scope

{- Note [TyVar Scopes]
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
listScopes :: Scope -> [Located a] -> [RScoped (Located a)]
listScopes _ [] = []
listScopes rhsScope [pat] = [RS rhsScope pat]
listScopes rhsScope (pat : pats) = RS sc pat : pats'
  where
    pats'@((RS scope p):_) = listScopes rhsScope pats
    sc = combineScopes scope $ mkScope $ getLoc p

-- | 'listScopes' specialised to 'PScoped' things
patScopes
  :: Maybe Span
  -> Scope
  -> Scope
  -> [LPat (GhcPass p)]
  -> [PScoped (LPat (GhcPass p))]
patScopes rsp useScope patScope xs =
  map (\(RS sc a) -> PS rsp useScope sc a) $
    listScopes patScope xs

-- | 'listScopes' specialised to 'HsPatSigType'
tScopes
  :: Scope
  -> Scope
  -> [HsPatSigType (GhcPass a)]
  -> [TScoped (HsPatSigType (GhcPass a))]
tScopes scope rhsScope xs =
  map (\(RS sc a) -> TS (ResolvedScopes [scope, sc]) (unLoc a)) $
    listScopes rhsScope (map (\hsps -> L (getLoc $ hsps_body hsps) hsps) xs)
  -- We make the HsPatSigType into a Located one by using the location of the underlying LHsType.
  -- We then strip off the redundant location information afterward, and take the union of the given scope and those to the right when forming the TS.

-- | 'listScopes' specialised to 'TVScoped' things
tvScopes
  :: TyVarScope
  -> Scope
  -> [LHsTyVarBndr flag (GhcPass a)]
  -> [TVScoped (LHsTyVarBndr flag (GhcPass a))]
tvScopes tvScope rhsScope xs =
  map (\(RS sc a)-> TVS tvScope sc a) $ listScopes rhsScope xs

{- Note [Scoping Rules for SigPat]
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

class HasLoc a where
  -- ^ conveniently calculate locations for things without locations attached
  loc :: a -> SrcSpan

instance HasLoc thing => HasLoc (PScoped thing) where
  loc (PS _ _ _ a) = loc a

instance HasLoc (Located a) where
  loc (L l _) = l

instance HasLoc a => HasLoc [a] where
  loc [] = noSrcSpan
  loc xs = foldl1' combineSrcSpans $ map loc xs

instance HasLoc a => HasLoc (FamEqn (GhcPass s) a) where
  loc (FamEqn _ a outer_bndrs b _ c) = case outer_bndrs of
    HsOuterImplicit{} ->
      foldl1' combineSrcSpans [loc a, loc b, loc c]
    HsOuterExplicit{hso_bndrs = tvs} ->
      foldl1' combineSrcSpans [loc a, loc tvs, loc b, loc c]

instance (HasLoc tm, HasLoc ty) => HasLoc (HsArg tm ty) where
  loc (HsValArg tm) = loc tm
  loc (HsTypeArg _ ty) = loc ty
  loc (HsArgPar sp)  = sp

instance HasLoc (HsDataDefn GhcRn) where
  loc def@(HsDataDefn{}) = loc $ dd_cons def
    -- Only used for data family instances, so we only need rhs
    -- Most probably the rest will be unhelpful anyway

{- Note [Real DataCon Name]
The typechecker substitutes the conLikeWrapId for the name, but we don't want
this showing up in the hieFile, so we replace the name in the Id with the
original datacon name
See also Note [Data Constructor Naming]
-}
class HasRealDataConName p where
  getRealDataCon :: XRecordCon p -> Located (IdP p) -> Located (IdP p)

instance HasRealDataConName GhcRn where
  getRealDataCon _ n = n
instance HasRealDataConName GhcTc where
  getRealDataCon RecordConTc{rcon_con_like = con} (L sp var) =
    L sp (setVarName var (conLikeName con))

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

instance (ToHie a) => ToHie (Bag a) where
  toHie = toHie . bagToList

instance (ToHie a) => ToHie (Maybe a) where
  toHie = maybe (pure []) toHie

instance ToHie (IEContext (Located ModuleName)) where
  toHie (IEC c (L (RealSrcSpan span _) mname)) = do
      org <- ask
      pure $ [Node (mkSourcedNodeInfo org $ NodeInfo S.empty [] idents) span []]
    where details = mempty{identInfo = S.singleton (IEThing c)}
          idents = M.singleton (Left mname) details
  toHie _ = pure []

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
          pure
            [Node
              (mkSourcedNodeInfo org $ NodeInfo S.empty [] $
                M.singleton (Right name)
                            (IdentifierDetails Nothing
                                               (S.singleton context)))
              span
              []]
      _ -> pure []

evVarsOfTermList :: EvTerm -> [EvId]
evVarsOfTermList (EvExpr e)         = exprSomeFreeVarsList isEvVar e
evVarsOfTermList (EvTypeable _ ev)  =
  case ev of
    EvTypeableTyCon _ e   -> concatMap evVarsOfTermList e
    EvTypeableTyApp e1 e2 -> concatMap evVarsOfTermList [e1,e2]
    EvTypeableTrFun e1 e2 e3 -> concatMap evVarsOfTermList [e1,e2,e3]
    EvTypeableTyLit e     -> evVarsOfTermList e
evVarsOfTermList (EvFun{}) = []

instance ToHie (EvBindContext (Located TcEvBinds)) where
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

instance ToHie (Located HsWrapper) where
  toHie (L osp wrap)
    = case wrap of
        (WpLet bs)      -> toHie $ EvBindContext (mkScope osp) (getRealSpan osp) (L osp bs)
        (WpCompose a b) -> concatM $
          [toHie (L osp a), toHie (L osp b)]
        (WpFun a b _ _) -> concatM $
          [toHie (L osp a), toHie (L osp b)]
        (WpEvLam a) ->
          toHie $ C (EvidenceVarBind EvWrapperBind (mkScope osp) (getRealSpan osp))
                $ L osp a
        (WpEvApp a) ->
          concatMapM (toHie . C EvidenceVarUse . L osp) $ evVarsOfTermList a
        _               -> pure []

instance HiePass p => HasType (Located (HsBind (GhcPass p))) where
  getTypeNode (L spn bind) =
    case hiePass @p of
      HieRn -> makeNode bind spn
      HieTc ->  case bind of
        FunBind{fun_id = name} -> makeTypeNode bind spn (varType $ unLoc name)
        _ -> makeNode bind spn

instance HiePass p => HasType (Located (Pat (GhcPass p))) where
  getTypeNode (L spn pat) =
    case hiePass @p of
      HieRn -> makeNode pat spn
      HieTc -> makeTypeNode pat spn (hsPatType pat)

-- | This instance tries to construct 'HieAST' nodes which include the type of
-- the expression. It is not yet possible to do this efficiently for all
-- expression forms, so we skip filling in the type for those inputs.
--
-- 'HsApp', for example, doesn't have any type information available directly on
-- the node. Our next recourse would be to desugar it into a 'CoreExpr' then
-- query the type of that. Yet both the desugaring call and the type query both
-- involve recursive calls to the function and argument! This is particularly
-- problematic when you realize that the HIE traversal will eventually visit
-- those nodes too and ask for their types again.
--
-- Since the above is quite costly, we just skip cases where computing the
-- expression's type is going to be expensive.
--
-- See #16233
instance HiePass p => HasType (Located (HsExpr (GhcPass p))) where
  getTypeNode e@(L spn e') =
    case hiePass @p of
      HieRn -> makeNode e' spn
      HieTc ->
        -- Some expression forms have their type immediately available
        let tyOpt = case e' of
              HsLit _ l -> Just (hsLitType l)
              HsOverLit _ o -> Just (overLitType o)

              HsConLikeOut _ (RealDataCon con) -> Just (dataConNonlinearType con)

              HsLam     _ (MG { mg_ext = groupTy }) -> Just (matchGroupType groupTy)
              HsLamCase _ (MG { mg_ext = groupTy }) -> Just (matchGroupType groupTy)
              HsCase _  _ (MG { mg_ext = groupTy }) -> Just (mg_res_ty groupTy)

              ExplicitList  ty _ _   -> Just (mkListTy ty)
              ExplicitSum   ty _ _ _ -> Just (mkSumTy ty)
              HsDo          ty _ _   -> Just ty
              HsMultiIf     ty _     -> Just ty

              _ -> Nothing

        in
        case tyOpt of
          Just t -> makeTypeNode e' spn t
          Nothing
            | skipDesugaring e' -> fallback
            | otherwise -> do
                hs_env <- lift $ lift $ Hsc $ \e w -> return (e,w)
                (_,mbe) <- liftIO $ deSugarExpr hs_env e
                maybe fallback (makeTypeNode e' spn . exprType) mbe
        where
          fallback = makeNode e' spn

          matchGroupType :: MatchGroupTc -> Type
          matchGroupType (MatchGroupTc args res) = mkVisFunTys args res

          -- | Skip desugaring of these expressions for performance reasons.
          --
          -- See impact on Haddock output (esp. missing type annotations or links)
          -- before marking more things here as 'False'. See impact on Haddock
          -- performance before marking more things as 'True'.
          skipDesugaring :: HsExpr GhcTc -> Bool
          skipDesugaring e = case e of
            HsVar{}             -> False
            HsUnboundVar{}      -> False
            HsConLikeOut{}      -> False
            HsRecFld{}          -> False
            HsOverLabel{}       -> False
            HsIPVar{}           -> False
            XExpr (WrapExpr {}) -> False
            _                   -> True

data HiePassEv p where
  HieRn :: HiePassEv 'Renamed
  HieTc :: HiePassEv 'Typechecked

class ( IsPass p
      , HiePass (NoGhcTcPass p)
      , ModifyState (IdGhcP p)
      , Data (GRHS (GhcPass p) (Located (HsExpr (GhcPass p))))
      , Data (HsExpr (GhcPass p))
      , Data (HsCmd (GhcPass p))
      , Data (AmbiguousFieldOcc (GhcPass p))
      , Data (HsCmdTop (GhcPass p))
      , Data (GRHS (GhcPass p) (Located (HsCmd (GhcPass p))))
      , Data (HsSplice (GhcPass p))
      , Data (HsLocalBinds (GhcPass p))
      , Data (FieldOcc (GhcPass p))
      , Data (HsTupArg (GhcPass p))
      , Data (IPBind (GhcPass p))
      , ToHie (Context (Located (IdGhcP p)))
      , ToHie (Context (Located (XUnboundVar (GhcPass p))))
      , ToHie (RFContext (Located (AmbiguousFieldOcc (GhcPass p))))
      , ToHie (RFContext (Located (FieldOcc (GhcPass p))))
      , ToHie (TScoped (LHsWcType (GhcPass (NoGhcTcPass p))))
      , ToHie (TScoped (LHsSigWcType (GhcPass (NoGhcTcPass p))))
      , HasRealDataConName (GhcPass p)
      )
      => HiePass p where
  hiePass :: HiePassEv p

instance HiePass 'Renamed where
  hiePass = HieRn
instance HiePass 'Typechecked where
  hiePass = HieTc

instance ToHie (Context (Located NoExtField)) where
  toHie _ = pure []

instance HiePass p => ToHie (BindContext (Located (HsBind (GhcPass p)))) where
  toHie (BC context scope b@(L span bind)) =
    concatM $ getTypeNode b : case bind of
      FunBind{fun_id = name, fun_matches = matches, fun_ext = wrap} ->
        [ toHie $ C (ValBind context scope $ getRealSpan span) name
        , toHie matches
        , case hiePass @p of
            HieTc -> toHie $ L span wrap
            _ -> pure []
        ]
      PatBind{pat_lhs = lhs, pat_rhs = rhs} ->
        [ toHie $ PS (getRealSpan span) scope NoScope lhs
        , toHie rhs
        ]
      VarBind{var_rhs = expr} ->
        [ toHie expr
        ]
      AbsBinds{ abs_exports = xs, abs_binds = binds
              , abs_ev_binds = ev_binds
              , abs_ev_vars = ev_vars } ->
        [  lift (modify (modifyState xs)) >> -- Note [Name Remapping]
                (toHie $ fmap (BC context scope) binds)
        , toHie $ map (L span . abe_wrap) xs
        , toHie $
            map (EvBindContext (mkScope span) (getRealSpan span)
                . L span) ev_binds
        , toHie $
            map (C (EvidenceVarBind EvSigBind
                                    (mkScope span)
                                    (getRealSpan span))
                . L span) ev_vars
        ]
      PatSynBind _ psb ->
        [ toHie $ L span psb -- PatSynBinds only occur at the top level
        ]

instance ( HiePass p
         , ToHie (Located body)
         , Data body
         ) => ToHie (MatchGroup (GhcPass p) (Located body)) where
  toHie mg = case mg of
    MG{ mg_alts = (L span alts) , mg_origin = origin} ->
      local (setOrigin origin) $ concatM
        [ locOnly span
        , toHie alts
        ]

setOrigin :: Origin -> NodeOrigin -> NodeOrigin
setOrigin FromSource _ = SourceInfo
setOrigin Generated _ = GeneratedInfo

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
          varScope = mkLScope var
          patScope = mkScope $ getLoc pat
          detScope = case dets of
            (PrefixCon _ args) -> foldr combineScopes NoScope $ map mkLScope args
            (InfixCon a b) -> combineScopes (mkLScope a) (mkLScope b)
            (RecCon r) -> foldr go NoScope r
          go (RecordPatSynField a b) c = combineScopes c
            $ combineScopes (mkLScope (rdrNameFieldOcc a)) (mkLScope b)
          detSpan = case detScope of
            LocalScope a -> Just a
            _ -> Nothing
          toBind (PrefixCon ts args) = ASSERT(null ts) PrefixCon ts $ map (C Use) args
          toBind (InfixCon a b) = InfixCon (C Use a) (C Use b)
          toBind (RecCon r) = RecCon $ map (PSC detSpan) r

instance HiePass p => ToHie (HsPatSynDir (GhcPass p)) where
  toHie dir = case dir of
    ExplicitBidirectional mg -> toHie mg
    _ -> pure []

instance ( HiePass p
         , Data body
         , ToHie (Located body)
         ) => ToHie (Located (Match (GhcPass p) (Located body))) where
  toHie (L span m ) = concatM $ node : case m of
    Match{m_ctxt=mctx, m_pats = pats, m_grhss =  grhss } ->
      [ toHie mctx
      , let rhsScope = mkScope $ grhss_span grhss
          in toHie $ patScopes Nothing rhsScope NoScope pats
      , toHie grhss
      ]
    where
      node = case hiePass @p of
        HieTc -> makeNode m span
        HieRn -> makeNode m span

instance HiePass p => ToHie (HsMatchContext (GhcPass p)) where
  toHie (FunRhs{mc_fun=name}) = toHie $ C MatchBind name
  toHie (StmtCtxt a) = toHie a
  toHie _ = pure []

instance HiePass p => ToHie (HsStmtContext (GhcPass p)) where
  toHie (PatGuard a) = toHie a
  toHie (ParStmtCtxt a) = toHie a
  toHie (TransStmtCtxt a) = toHie a
  toHie _ = pure []

instance HiePass p => ToHie (PScoped (Located (Pat (GhcPass p)))) where
  toHie (PS rsp scope pscope lpat@(L ospan opat)) =
    concatM $ getTypeNode lpat : case opat of
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
                                 (combineScopes (mkLScope pat) pscope)
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
            [ toHie $ C Use con
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
      NPat _ _ _ _ ->
        []
      NPlusKPat _ n _ _ _ _ ->
        [ toHie $ C (PatternBind scope pscope rsp) n
        ]
      SigPat _ pat sig ->
        [ toHie $ PS rsp scope pscope pat
        , case hiePass @p of
            HieTc ->
              let cscope = mkLScope pat in
                toHie $ TS (ResolvedScopes [cscope, scope, pscope])
                           sig
            HieRn -> pure []
        ]
      XPat e ->
        case hiePass @p of
          HieTc ->
            let CoPat wrap pat _ = e
              in [ toHie $ L ospan wrap
                 , toHie $ PS rsp scope pscope $ (L ospan pat)
                 ]
#if __GLASGOW_HASKELL__ < 811
          HieRn -> []
#endif
    where
      contextify :: a ~ LPat (GhcPass p) => HsConDetails (HsPatSigType (NoGhcTc (GhcPass p))) a (HsRecFields (GhcPass p) a)
                 -> HsConDetails (TScoped (HsPatSigType (NoGhcTc (GhcPass p)))) (PScoped a) (RContext (HsRecFields (GhcPass p) (PScoped a)))
      contextify (PrefixCon tyargs args) = PrefixCon (tScopes scope argscope tyargs) (patScopes rsp scope pscope args)
        where argscope = foldr combineScopes NoScope $ map mkLScope args
      contextify (InfixCon a b) = InfixCon a' b'
        where [a', b'] = patScopes rsp scope pscope [a,b]
      contextify (RecCon r) = RecCon $ RC RecFieldMatch $ contextify_rec r
      contextify_rec (HsRecFields fds a) = HsRecFields (map go scoped_fds) a
        where
          go (RS fscope (L spn (HsRecField lbl pat pun))) =
            L spn $ HsRecField lbl (PS rsp scope fscope pat) pun
          scoped_fds = listScopes pscope fds

instance ToHie (TScoped (HsPatSigType GhcRn)) where
  toHie (TS sc (HsPS (HsPSRn wcs tvs) body@(L span _))) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) (wcs++tvs)
      , toHie body
      ]
  -- See Note [Scoping Rules for SigPat]

instance ( ToHie (Located body)
         , HiePass p
         , Data body
         ) => ToHie (GRHSs (GhcPass p) (Located body)) where
  toHie grhs = concatM $ case grhs of
    GRHSs _ grhss binds ->
     [ toHie grhss
     , toHie $ RS (mkScope $ grhss_span grhs) binds
     ]

instance ( ToHie (Located body)
         , HiePass a
         , Data body
         ) => ToHie (Located (GRHS (GhcPass a) (Located body))) where
  toHie (L span g) = concatM $ node : case g of
    GRHS _ guards body ->
      [ toHie $ listScopes (mkLScope body) guards
      , toHie body
      ]
    where
      node = case hiePass @a of
        HieRn -> makeNode g span
        HieTc -> makeNode g span

instance HiePass p => ToHie (Located (HsExpr (GhcPass p))) where
  toHie e@(L mspan oexpr) = concatM $ getTypeNode e : case oexpr of
      HsVar _ (L _ var) ->
        [ toHie $ C Use (L mspan var)
             -- Patch up var location since typechecker removes it
        ]
      HsUnboundVar var _ ->
        [ toHie $ C Use (L mspan var) ]
      HsConLikeOut _ con ->
        [ toHie $ C Use $ L mspan $ conLikeName con
        ]
      HsRecFld _ fld ->
        [ toHie $ RFC RecFieldOcc Nothing (L mspan fld)
        ]
      HsOverLabel _ _ _ -> []
      HsIPVar _ _ -> []
      HsOverLit _ _ -> []
      HsLit _ _ -> []
      HsLam _ mg ->
        [ toHie mg
        ]
      HsLamCase _ mg ->
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
        [ toHie $ RS (mkLScope expr) binds
        , toHie expr
        ]
      HsDo _ _ (L ispan stmts) ->
        [ locOnly ispan
        , toHie $ listScopes NoScope stmts
        ]
      ExplicitList _ _ exprs ->
        [ toHie exprs
        ]
      RecordCon {rcon_ext = mrealcon, rcon_con_name = name, rcon_flds = binds} ->
        [ toHie $ C Use (getRealDataCon @(GhcPass p) mrealcon name)
            -- See Note [Real DataCon Name]
        , toHie $ RC RecFieldAssign $ binds
        ]
      RecordUpd {rupd_expr = expr, rupd_flds = upds}->
        [ toHie expr
        , toHie $ map (RC RecFieldAssign) upds
        ]
      ExprWithTySig _ expr sig ->
        [ toHie expr
        , toHie $ TS (ResolvedScopes [mkLScope expr]) sig
        ]
      ArithSeq _ _ info ->
        [ toHie info
        ]
      HsPragE _ _ expr ->
        [ toHie expr
        ]
      HsProc _ pat cmdtop ->
        [ toHie $ PS Nothing (mkLScope cmdtop) NoScope pat
        , toHie cmdtop
        ]
      HsStatic _ expr ->
        [ toHie expr
        ]
      HsTick _ _ expr ->
        [ toHie expr
        ]
      HsBinTick _ _ _ expr ->
        [ toHie expr
        ]
      HsBracket _ b ->
        [ toHie b
        ]
      HsRnBracketOut _ b p ->
        [ toHie b
        , toHie p
        ]
      HsTcBracketOut _ _wrap b p ->
        [ toHie b
        , toHie p
        ]
      HsSpliceE _ x ->
        [ toHie $ L mspan x
        ]
      XExpr x
        | GhcTc <- ghcPass @p
        , WrapExpr (HsWrap w a) <- x
        -> [ toHie $ L mspan a
           , toHie (L mspan w)
           ]
        | GhcTc <- ghcPass @p
        , ExpansionExpr (HsExpanded _ b) <- x
        -> [ toHie (L mspan b)
           ]
        | otherwise -> []

instance HiePass p => ToHie (Located (HsTupArg (GhcPass p))) where
  toHie (L span arg) = concatM $ makeNode arg span : case arg of
    Present _ expr ->
      [ toHie expr
      ]
    Missing _ -> []

instance ( ToHie (Located body)
         , Data body
         , HiePass p
         ) => ToHie (RScoped (Located (Stmt (GhcPass p) (Located body)))) where
  toHie (RS scope (L span stmt)) = concatM $ node : case stmt of
      LastStmt _ body _ _ ->
        [ toHie body
        ]
      BindStmt _ pat body ->
        [ toHie $ PS (getRealSpan $ getLoc body) scope NoScope pat
        , toHie body
        ]
      ApplicativeStmt _ stmts _ ->
        [ concatMapM (toHie . RS scope . snd) stmts
        ]
      BodyStmt _ body _ _ ->
        [ toHie body
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
      RecStmt {recS_stmts = stmts} ->
        [ toHie $ map (RS $ combineScopes scope (mkScope span)) stmts
        ]
    where
      node = case hiePass @p of
        HieTc -> makeNode stmt span
        HieRn -> makeNode stmt span

instance HiePass p => ToHie (RScoped (Located (HsLocalBinds (GhcPass p)))) where
  toHie (RS scope (L sp binds)) = concatM $ makeNode binds sp : case binds of
      EmptyLocalBinds _ -> []
      HsIPBinds _ ipbinds -> case ipbinds of
        IPBinds evbinds xs -> let sc = combineScopes scope $ mkScope sp in
          [ case hiePass @p of
              HieTc -> toHie $ EvBindContext sc (getRealSpan sp) $ L sp evbinds
              HieRn -> pure []
          , toHie $ map (RS sc) xs
          ]
      HsValBinds _ valBinds ->
        [ toHie $ RS (combineScopes scope $ mkScope sp)
                      valBinds
        ]

instance HiePass p => ToHie (RScoped (Located (IPBind (GhcPass p)))) where
  toHie (RS scope (L sp bind)) = concatM $ makeNode bind sp : case bind of
    IPBind _ (Left _) expr -> [toHie expr]
    IPBind _ (Right v) expr ->
      [ toHie $ C (EvidenceVarBind EvImplicitBind scope (getRealSpan sp))
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
    [ toHie (concatMap (map (BC RegularBind sc) . bagToList . snd) binds)
    , toHie $ fmap (SC (SI BindSig Nothing)) sigs
    ]

instance ( ToHie arg , HasLoc arg , Data arg
         , HiePass p ) => ToHie (RContext (HsRecFields (GhcPass p) arg)) where
  toHie (RC c (HsRecFields fields _)) = toHie $ map (RC c) fields

instance ( ToHie (RFContext (Located label))
         , ToHie arg , HasLoc arg , Data arg
         , Data label
         ) => ToHie (RContext (LHsRecField' label arg)) where
  toHie (RC c (L span recfld)) = concatM $ makeNode recfld span : case recfld of
    HsRecField label expr _ ->
      [ toHie $ RFC c (getRealSpan $ loc expr) label
      , toHie expr
      ]

instance ToHie (RFContext (Located (FieldOcc GhcRn))) where
  toHie (RFC c rhs (L nspan f)) = concatM $ case f of
    FieldOcc name _ ->
      [ toHie $ C (RecField c rhs) (L nspan name)
      ]

instance ToHie (RFContext (Located (FieldOcc GhcTc))) where
  toHie (RFC c rhs (L nspan f)) = concatM $ case f of
    FieldOcc var _ ->
      [ toHie $ C (RecField c rhs) (L nspan var)
      ]

instance ToHie (RFContext (Located (AmbiguousFieldOcc GhcRn))) where
  toHie (RFC c rhs (L nspan afo)) = concatM $ case afo of
    Unambiguous name _ ->
      [ toHie $ C (RecField c rhs) $ L nspan name
      ]
    Ambiguous _name _ ->
      [ ]

instance ToHie (RFContext (Located (AmbiguousFieldOcc GhcTc))) where
  toHie (RFC c rhs (L nspan afo)) = concatM $ case afo of
    Unambiguous var _ ->
      [ toHie $ C (RecField c rhs) (L nspan var)
      ]
    Ambiguous var _ ->
      [ toHie $ C (RecField c rhs) (L nspan var)
      ]

instance HiePass p => ToHie (RScoped (ApplicativeArg (GhcPass p))) where
  toHie (RS sc (ApplicativeArgOne _ pat expr _)) = concatM
    [ toHie $ PS Nothing sc NoScope pat
    , toHie expr
    ]
  toHie (RS sc (ApplicativeArgMany _ stmts _ pat _)) = concatM
    [ toHie $ listScopes NoScope stmts
    , toHie $ PS Nothing sc NoScope pat
    ]

instance (ToHie tyarg, ToHie arg, ToHie rec) => ToHie (HsConDetails tyarg arg rec) where
  toHie (PrefixCon tyargs args) = concatM [ toHie tyargs, toHie args ]
  toHie (RecCon rec) = toHie rec
  toHie (InfixCon a b) = concatM [ toHie a, toHie b]

instance ToHie (HsConDeclGADTDetails GhcRn) where
  toHie (PrefixConGADT args) = toHie args
  toHie (RecConGADT rec) = toHie rec

instance HiePass p => ToHie (Located (HsCmdTop (GhcPass p))) where
  toHie (L span top) = concatM $ makeNode top span : case top of
    HsCmdTop _ cmd ->
      [ toHie cmd
      ]

instance HiePass p => ToHie (Located (HsCmd (GhcPass p))) where
  toHie (L span cmd) = concatM $ makeNode cmd span : case cmd of
      HsCmdArrApp _ a b _ _ ->
        [ toHie a
        , toHie b
        ]
      HsCmdArrForm _ a _ _ cmdtops ->
        [ toHie a
        , toHie cmdtops
        ]
      HsCmdApp _ a b ->
        [ toHie a
        , toHie b
        ]
      HsCmdLam _ mg ->
        [ toHie mg
        ]
      HsCmdPar _ a ->
        [ toHie a
        ]
      HsCmdCase _ expr alts ->
        [ toHie expr
        , toHie alts
        ]
      HsCmdLamCase _ alts ->
        [ toHie alts
        ]
      HsCmdIf _ _ a b c ->
        [ toHie a
        , toHie b
        , toHie c
        ]
      HsCmdLet _ binds cmd' ->
        [ toHie $ RS (mkLScope cmd') binds
        , toHie cmd'
        ]
      HsCmdDo _ (L ispan stmts) ->
        [ locOnly ispan
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

instance ToHie (Located (TyClDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      FamDecl {tcdFam = fdecl} ->
        [ toHie (L span fdecl)
        ]
      SynDecl {tcdLName = name, tcdTyVars = vars, tcdRhs = typ} ->
        [ toHie $ C (Decl SynDec $ getRealSpan span) name
        , toHie $ TS (ResolvedScopes [mkScope $ getLoc typ]) vars
        , toHie typ
        ]
      DataDecl {tcdLName = name, tcdTyVars = vars, tcdDataDefn = defn} ->
        [ toHie $ C (Decl DataDec $ getRealSpan span) name
        , toHie $ TS (ResolvedScopes [quant_scope, rhs_scope]) vars
        , toHie defn
        ]
        where
          quant_scope = mkLScope $ dd_ctxt defn
          rhs_scope = sig_sc `combineScopes` con_sc `combineScopes` deriv_sc
          sig_sc = maybe NoScope mkLScope $ dd_kindSig defn
          con_sc = foldr combineScopes NoScope $ map mkLScope $ dd_cons defn
          deriv_sc = mkLScope $ dd_derivs defn
      ClassDecl { tcdCtxt = context
                , tcdLName = name
                , tcdTyVars = vars
                , tcdFDs = deps
                , tcdSigs = sigs
                , tcdMeths = meths
                , tcdATs = typs
                , tcdATDefs = deftyps
                } ->
        [ toHie $ C (Decl ClassDec $ getRealSpan span) name
        , toHie context
        , toHie $ TS (ResolvedScopes [context_scope, rhs_scope]) vars
        , toHie deps
        , toHie $ map (SC $ SI ClassSig $ getRealSpan span) sigs
        , toHie $ fmap (BC InstanceBind ModuleScope) meths
        , toHie typs
        , concatMapM (locOnly . getLoc) deftyps
        , toHie deftyps
        ]
        where
          context_scope = mkLScope context
          rhs_scope = foldl1' combineScopes $ map mkScope
            [ loc deps, loc sigs, loc (bagToList meths), loc typs, loc deftyps]

instance ToHie (Located (FamilyDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      FamilyDecl _ info name vars _ sig inj ->
        [ toHie $ C (Decl FamDec $ getRealSpan span) name
        , toHie $ TS (ResolvedScopes [rhsSpan]) vars
        , toHie info
        , toHie $ RS injSpan sig
        , toHie inj
        ]
        where
          rhsSpan = sigSpan `combineScopes` injSpan
          sigSpan = mkScope $ getLoc sig
          injSpan = maybe NoScope (mkScope . getLoc) inj

instance ToHie (FamilyInfo GhcRn) where
  toHie (ClosedTypeFamily (Just eqns)) = concatM $
    [ concatMapM (locOnly . getLoc) eqns
    , toHie $ map go eqns
    ]
    where
      go (L l ib) = TS (ResolvedScopes [mkScope l]) ib
  toHie _ = pure []

instance ToHie (RScoped (Located (FamilyResultSig GhcRn))) where
  toHie (RS sc (L span sig)) = concatM $ makeNode sig span : case sig of
      NoSig _ ->
        []
      KindSig _ k ->
        [ toHie k
        ]
      TyVarSig _ bndr ->
        [ toHie $ TVS (ResolvedScopes [sc]) NoScope bndr
        ]

instance ToHie (Located (FunDep (Located Name))) where
  toHie (L span fd@(lhs, rhs)) = concatM $
    [ makeNode fd span
    , toHie $ map (C Use) lhs
    , toHie $ map (C Use) rhs
    ]

instance (ToHie rhs, HasLoc rhs)
    => ToHie (TScoped (FamEqn GhcRn rhs)) where
  toHie (TS _ f) = toHie f

instance (ToHie rhs, HasLoc rhs)
    => ToHie (FamEqn GhcRn rhs) where
  toHie fe@(FamEqn _ var outer_bndrs pats _ rhs) = concatM $
    [ toHie $ C (Decl InstDec $ getRealSpan $ loc fe) var
    , toHie $ TVS (ResolvedScopes []) scope outer_bndrs
    , toHie pats
    , toHie rhs
    ]
    where scope = combineScopes patsScope rhsScope
          patsScope = mkScope (loc pats)
          rhsScope = mkScope (loc rhs)

instance ToHie (Located (InjectivityAnn GhcRn)) where
  toHie (L span ann) = concatM $ makeNode ann span : case ann of
      InjectivityAnn lhs rhs ->
        [ toHie $ C Use lhs
        , toHie $ map (C Use) rhs
        ]

instance ToHie (HsDataDefn GhcRn) where
  toHie (HsDataDefn _ _ ctx _ mkind cons derivs) = concatM
    [ toHie ctx
    , toHie mkind
    , toHie cons
    , toHie derivs
    ]

instance ToHie (Located [Located (HsDerivingClause GhcRn)]) where
  toHie (L span clauses) = concatM
    [ locOnly span
    , toHie clauses
    ]

instance ToHie (Located (HsDerivingClause GhcRn)) where
  toHie (L span cl) = concatM $ makeNode cl span : case cl of
      HsDerivingClause _ strat dct ->
        [ toHie strat
        , toHie dct
        ]

instance ToHie (Located (DerivClauseTys GhcRn)) where
  toHie (L span dct) = concatM $ makeNode dct span : case dct of
      DctSingle _ ty -> [ toHie $ TS (ResolvedScopes []) ty ]
      DctMulti _ tys -> [ toHie $ map (TS (ResolvedScopes [])) tys ]

instance ToHie (Located (DerivStrategy GhcRn)) where
  toHie (L span strat) = concatM $ makeNode strat span : case strat of
      StockStrategy -> []
      AnyclassStrategy -> []
      NewtypeStrategy -> []
      ViaStrategy s -> [ toHie (TS (ResolvedScopes []) s) ]

instance ToHie (Located OverlapMode) where
  toHie (L span _) = locOnly span

instance ToHie a => ToHie (HsScaled GhcRn a) where
  toHie (HsScaled w t) = concatM [toHie (arrowToHsType w), toHie t]

instance ToHie (Located (ConDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      ConDeclGADT { con_names = names, con_bndrs = L outer_bndrs_loc outer_bndrs
                  , con_mb_cxt = ctx, con_g_args = args, con_res_ty = typ } ->
        [ toHie $ map (C (Decl ConDec $ getRealSpan span)) names
        , case outer_bndrs of
            HsOuterImplicit{hso_ximplicit = imp_vars} ->
              bindingsOnly $ map (C $ TyVarBind (mkScope outer_bndrs_loc) resScope)
                             imp_vars
            HsOuterExplicit{hso_bndrs = exp_bndrs} ->
              toHie $ tvScopes resScope NoScope exp_bndrs
        , toHie ctx
        , toHie args
        , toHie typ
        ]
        where
          rhsScope = combineScopes argsScope tyScope
          ctxScope = maybe NoScope mkLScope ctx
          argsScope = case args of
            PrefixConGADT xs -> scaled_args_scope xs
            RecConGADT x     -> mkLScope x
          tyScope = mkLScope typ
          resScope = ResolvedScopes [ctxScope, rhsScope]
      ConDeclH98 { con_name = name, con_ex_tvs = qvars
                 , con_mb_cxt = ctx, con_args = dets } ->
        [ toHie $ C (Decl ConDec $ getRealSpan span) name
        , toHie $ tvScopes (ResolvedScopes []) rhsScope qvars
        , toHie ctx
        , toHie dets
        ]
        where
          rhsScope = combineScopes ctxScope argsScope
          ctxScope = maybe NoScope mkLScope ctx
          argsScope = case dets of
            PrefixCon _ xs -> scaled_args_scope xs
            InfixCon a b   -> scaled_args_scope [a, b]
            RecCon x       -> mkLScope x
    where scaled_args_scope :: [HsScaled GhcRn (LHsType GhcRn)] -> Scope
          scaled_args_scope = foldr combineScopes NoScope . map (mkLScope . hsScaledThing)

instance ToHie (Located [Located (ConDeclField GhcRn)]) where
  toHie (L span decls) = concatM $
    [ locOnly span
    , toHie decls
    ]

instance ToHie (TScoped (HsWildCardBndrs GhcRn (Located (HsSigType GhcRn)))) where
  toHie (TS sc (HsWC names a)) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) names
      , toHie $ TS sc a
      ]
    where span = loc a

instance ToHie (TScoped (HsWildCardBndrs GhcRn (Located (HsType GhcRn)))) where
  toHie (TS sc (HsWC names a)) = concatM $
      [ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) names
      , toHie a
      ]
    where span = loc a

instance ToHie (Located (StandaloneKindSig GhcRn)) where
  toHie (L sp sig) = concatM [makeNode sig sp, toHie sig]

instance ToHie (StandaloneKindSig GhcRn) where
  toHie sig = concatM $ case sig of
    StandaloneKindSig _ name typ ->
      [ toHie $ C TyDecl name
      , toHie $ TS (ResolvedScopes []) typ
      ]

instance HiePass p => ToHie (SigContext (Located (Sig (GhcPass p)))) where
  toHie (SC (SI styp msp) (L sp sig)) =
    case hiePass @p of
      HieTc -> pure []
      HieRn -> concatM $ makeNode sig sp : case sig of
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
              ClassSig -> toHie $ map (C $ ClassTyDecl $ getRealSpan sp) names
              _  -> toHie $ map (C $ TyDecl) names
          , toHie $ TS (UnresolvedScope (map unLoc names) msp) typ
          ]
        IdSig _ _ -> []
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
        SpecInstSig _ _ typ ->
          [ toHie $ TS (ResolvedScopes []) typ
          ]
        MinimalSig _ _ form ->
          [ toHie form
          ]
        SCCFunSig _ _ name mtxt ->
          [ toHie $ (C Use) name
          , maybe (pure []) (locOnly . getLoc) mtxt
          ]
        CompleteMatchSig _ _ (L ispan names) typ ->
          [ locOnly ispan
          , toHie $ map (C Use) names
          , toHie $ fmap (C Use) typ
          ]

instance ToHie (TScoped (Located (HsSigType GhcRn))) where
  toHie (TS tsc (L span t@HsSig{sig_bndrs=bndrs,sig_body=body})) = concatM $ makeNode t span :
      [ toHie (TVS tsc (mkScope span) bndrs)
      , toHie body
      ]

instance Data flag => ToHie (TVScoped (HsOuterTyVarBndrs flag GhcRn)) where
  toHie (TVS tsc sc bndrs) = case bndrs of
    HsOuterImplicit xs -> bindingsOnly $ map (C $ TyVarBind sc tsc) xs
    HsOuterExplicit _ xs -> toHie $ tvScopes tsc sc xs

instance ToHie (Located (HsType GhcRn)) where
  toHie (L span t) = concatM $ makeNode t span : case t of
      HsForAllTy _ tele body ->
        let scope = mkScope $ getLoc body in
        [ case tele of
            HsForAllVis { hsf_vis_bndrs = bndrs } ->
              toHie $ tvScopes (ResolvedScopes []) scope bndrs
            HsForAllInvis { hsf_invis_bndrs = bndrs } ->
              toHie $ tvScopes (ResolvedScopes []) scope bndrs
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
        [ toHie (arrowToHsType w)
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
      HsOpTy _ a op b ->
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
      HsDocTy _ a _ ->
        [ toHie a
        ]
      HsBangTy _ _ ty ->
        [ toHie ty
        ]
      HsRecTy _ fields ->
        [ toHie fields
        ]
      HsExplicitListTy _ _ tys ->
        [ toHie tys
        ]
      HsExplicitTupleTy _ tys ->
        [ toHie tys
        ]
      HsTyLit _ _ -> []
      HsWildCardTy _ -> []
      HsStarTy _ _ -> []
      XHsType _ -> []

instance (ToHie tm, ToHie ty) => ToHie (HsArg tm ty) where
  toHie (HsValArg tm) = toHie tm
  toHie (HsTypeArg _ ty) = toHie ty
  toHie (HsArgPar sp) = locOnly sp

instance Data flag => ToHie (TVScoped (Located (HsTyVarBndr flag GhcRn))) where
  toHie (TVS tsc sc (L span bndr)) = concatM $ makeNode bndr span : case bndr of
      UserTyVar _ _ var ->
        [ toHie $ C (TyVarBind sc tsc) var
        ]
      KindedTyVar _ _ var kind ->
        [ toHie $ C (TyVarBind sc tsc) var
        , toHie kind
        ]

instance ToHie (TScoped (LHsQTyVars GhcRn)) where
  toHie (TS sc (HsQTvs implicits vars)) = concatM $
    [ bindingsOnly bindings
    , toHie $ tvScopes sc NoScope vars
    ]
    where
      varLoc = loc vars
      bindings = map (C $ TyVarBind (mkScope varLoc) sc) implicits

instance ToHie (Located [Located (HsType GhcRn)]) where
  toHie (L span tys) = concatM $
      [ locOnly span
      , toHie tys
      ]

instance ToHie (Located (ConDeclField GhcRn)) where
  toHie (L span field) = concatM $ makeNode field span : case field of
      ConDeclField _ fields typ _ ->
        [ toHie $ map (RFC RecFieldDecl (getRealSpan $ loc typ)) fields
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

instance ToHie (Located (SpliceDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      SpliceDecl _ splice _ ->
        [ toHie splice
        ]

instance ToHie (HsBracket a) where
  toHie _ = pure []

instance ToHie PendingRnSplice where
  toHie _ = pure []

instance ToHie PendingTcSplice where
  toHie _ = pure []

instance ToHie (LBooleanFormula (Located Name)) where
  toHie (L span form) = concatM $ makeNode form span : case form of
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

instance ToHie (Located HsIPName) where
  toHie (L span e) = makeNode e span

instance HiePass p => ToHie (Located (HsSplice (GhcPass p))) where
  toHie (L span sp) = concatM $ makeNode sp span : case sp of
      HsTypedSplice _ _ _ expr ->
        [ toHie expr
        ]
      HsUntypedSplice _ _ _ expr ->
        [ toHie expr
        ]
      HsQuasiQuote _ _ _ ispan _ ->
        [ locOnly ispan
        ]
      HsSpliced _ _ _ ->
        []
      XSplice x -> case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
                     GhcPs -> noExtCon x
                     GhcRn -> noExtCon x
#endif
                     GhcTc -> case x of
                                HsSplicedT _ -> []

instance ToHie (Located (RoleAnnotDecl GhcRn)) where
  toHie (L span annot) = concatM $ makeNode annot span : case annot of
      RoleAnnotDecl _ var roles ->
        [ toHie $ C Use var
        , concatMapM (locOnly . getLoc) roles
        ]

instance ToHie (Located (InstDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      ClsInstD _ d ->
        [ toHie $ L span d
        ]
      DataFamInstD _ d ->
        [ toHie $ L span d
        ]
      TyFamInstD _ d ->
        [ toHie $ L span d
        ]

instance ToHie (Located (ClsInstDecl GhcRn)) where
  toHie (L span decl) = concatM
    [ toHie $ TS (ResolvedScopes [mkScope span]) $ cid_poly_ty decl
    , toHie $ fmap (BC InstanceBind ModuleScope) $ cid_binds decl
    , toHie $ map (SC $ SI InstSig $ getRealSpan span) $ cid_sigs decl
    , concatMapM (locOnly . getLoc) $ cid_tyfam_insts decl
    , toHie $ cid_tyfam_insts decl
    , concatMapM (locOnly . getLoc) $ cid_datafam_insts decl
    , toHie $ cid_datafam_insts decl
    , toHie $ cid_overlap_mode decl
    ]

instance ToHie (Located (DataFamInstDecl GhcRn)) where
  toHie (L sp (DataFamInstDecl d)) = toHie $ TS (ResolvedScopes [mkScope sp]) d

instance ToHie (Located (TyFamInstDecl GhcRn)) where
  toHie (L sp (TyFamInstDecl d)) = toHie $ TS (ResolvedScopes [mkScope sp]) d

instance HiePass p => ToHie (Context (FieldOcc (GhcPass p))) where
  toHie (C c (FieldOcc n (L l _))) = case hiePass @p of
    HieTc -> toHie (C c (L l n))
    HieRn -> toHie (C c (L l n))

instance HiePass p => ToHie (PatSynFieldContext (RecordPatSynField (GhcPass p))) where
  toHie (PSC sp (RecordPatSynField a b)) = concatM $
    [ toHie $ C (RecField RecFieldDecl sp) a
    , toHie $ C Use b
    ]

instance ToHie (Located (DerivDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      DerivDecl _ typ strat overlap ->
        [ toHie $ TS (ResolvedScopes []) typ
        , toHie strat
        , toHie overlap
        ]

instance ToHie (Located (FixitySig GhcRn)) where
  toHie (L span sig) = concatM $ makeNode sig span : case sig of
      FixitySig _ vars _ ->
        [ toHie $ map (C Use) vars
        ]

instance ToHie (Located (DefaultDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      DefaultDecl _ typs ->
        [ toHie typs
        ]

instance ToHie (Located (ForeignDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      ForeignImport {fd_name = name, fd_sig_ty = sig, fd_fi = fi} ->
        [ toHie $ C (ValBind RegularBind ModuleScope $ getRealSpan span) name
        , toHie $ TS (ResolvedScopes []) sig
        , toHie fi
        ]
      ForeignExport {fd_name = name, fd_sig_ty = sig, fd_fe = fe} ->
        [ toHie $ C Use name
        , toHie $ TS (ResolvedScopes []) sig
        , toHie fe
        ]

instance ToHie ForeignImport where
  toHie (CImport (L a _) (L b _) _ _ (L c _)) = concatM $
    [ locOnly a
    , locOnly b
    , locOnly c
    ]

instance ToHie ForeignExport where
  toHie (CExport (L a _) (L b _)) = concatM $
    [ locOnly a
    , locOnly b
    ]

instance ToHie (Located (WarnDecls GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      Warnings _ _ warnings ->
        [ toHie warnings
        ]

instance ToHie (Located (WarnDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      Warning _ vars _ ->
        [ toHie $ map (C Use) vars
        ]

instance ToHie (Located (AnnDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      HsAnnotation _ _ prov expr ->
        [ toHie prov
        , toHie expr
        ]

instance ToHie (Context (Located a)) => ToHie (AnnProvenance a) where
  toHie (ValueAnnProvenance a) = toHie $ C Use a
  toHie (TypeAnnProvenance a) = toHie $ C Use a
  toHie ModuleAnnProvenance = pure []

instance ToHie (Located (RuleDecls GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      HsRules _ _ rules ->
        [ toHie rules
        ]

instance ToHie (Located (RuleDecl GhcRn)) where
  toHie (L span r@(HsRule _ rname _ tybndrs bndrs exprA exprB)) = concatM
        [ makeNode r span
        , locOnly $ getLoc rname
        , toHie $ fmap (tvScopes (ResolvedScopes []) scope) tybndrs
        , toHie $ map (RS $ mkScope span) bndrs
        , toHie exprA
        , toHie exprB
        ]
    where scope = bndrs_sc `combineScopes` exprA_sc `combineScopes` exprB_sc
          bndrs_sc = maybe NoScope mkLScope (listToMaybe bndrs)
          exprA_sc = mkLScope exprA
          exprB_sc = mkLScope exprB

instance ToHie (RScoped (Located (RuleBndr GhcRn))) where
  toHie (RS sc (L span bndr)) = concatM $ makeNode bndr span : case bndr of
      RuleBndr _ var ->
        [ toHie $ C (ValBind RegularBind sc Nothing) var
        ]
      RuleBndrSig _ var typ ->
        [ toHie $ C (ValBind RegularBind sc Nothing) var
        , toHie $ TS (ResolvedScopes [sc]) typ
        ]

instance ToHie (Located (ImportDecl GhcRn)) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      ImportDecl { ideclName = name, ideclAs = as, ideclHiding = hidden } ->
        [ toHie $ IEC Import name
        , toHie $ fmap (IEC ImportAs) as
        , maybe (pure []) goIE hidden
        ]
    where
      goIE (hiding, (L sp liens)) = concatM $
        [ locOnly sp
        , toHie $ map (IEC c) liens
        ]
        where
         c = if hiding then ImportHiding else Import

instance ToHie (IEContext (Located (IE GhcRn))) where
  toHie (IEC c (L span ie)) = concatM $ makeNode ie span : case ie of
      IEVar _ n ->
        [ toHie $ IEC c n
        ]
      IEThingAbs _ n ->
        [ toHie $ IEC c n
        ]
      IEThingAll _ n ->
        [ toHie $ IEC c n
        ]
      IEThingWith flds n _ ns ->
        [ toHie $ IEC c n
        , toHie $ map (IEC c) ns
        , toHie $ map (IEC c) flds
        ]
      IEModuleContents _ n ->
        [ toHie $ IEC c n
        ]
      IEGroup _ _ _ -> []
      IEDoc _ _ -> []
      IEDocNamed _ _ -> []

instance ToHie (IEContext (LIEWrappedName Name)) where
  toHie (IEC c (L span iewn)) = concatM $ makeNode iewn span : case iewn of
      IEName n ->
        [ toHie $ C (IEThing c) n
        ]
      IEPattern p ->
        [ toHie $ C (IEThing c) p
        ]
      IEType n ->
        [ toHie $ C (IEThing c) n
        ]

instance ToHie (IEContext (Located FieldLabel)) where
  toHie (IEC c (L span lbl)) = concatM $ makeNode lbl span : case lbl of
      FieldLabel _ _ n ->
        [ toHie $ C (IEThing c) $ L span n
        ]
