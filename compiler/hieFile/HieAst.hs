{-
Main functions for .hie file generation
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module HieAst ( mkHieFile ) where

import GhcPrelude

import Avail                      ( Avails )
import Bag                        ( Bag, bagToList )
import BasicTypes
import BooleanFormula
import Class                      ( FunDep )
import CoreUtils                  ( exprType )
import ConLike                    ( conLikeName )
import Desugar                    ( deSugarExpr )
import FieldLabel
import HsSyn
import HscTypes
import Module                     ( ModuleName, ml_hs_file )
import MonadUtils                 ( concatMapM, liftIO )
import Name                       ( Name, nameSrcSpan, setNameLoc )
import SrcLoc
import TcHsSyn                    ( hsLitType, hsPatType )
import Type                       ( mkFunTys, Type )
import TysWiredIn                 ( mkListTy, mkSumTy )
import Var                        ( Id, Var, setVarName, varName, varType )
import TcRnTypes
import MkIface                    ( mkIfaceExports )

import HieTypes
import HieUtils

import qualified Data.Array as A
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Data                  ( Data, Typeable )
import Data.List                  ( foldl1' )
import Data.Maybe                 ( listToMaybe )
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class  ( lift )

-- These synonyms match those defined in main/GHC.hs
type RenamedSource     = ( HsGroup GhcRn, [LImportDecl GhcRn]
                         , Maybe [(LIE GhcRn, Avails)]
                         , Maybe LHsDocString )
type TypecheckedSource = LHsBinds GhcTc


{- Note [Name Remapping]
The Typechecker introduces new names for mono names in AbsBinds.
We don't care about the distinction between mono and poly bindings,
so we replace all occurrences of the mono name with the poly name.
-}
newtype HieState = HieState
  { name_remapping :: M.Map Name Id
  }

initState :: HieState
initState = HieState M.empty

class ModifyState a where -- See Note [Name Remapping]
  addSubstitution :: a -> a -> HieState -> HieState

instance ModifyState Name where
  addSubstitution _ _ hs = hs

instance ModifyState Id where
  addSubstitution mono poly hs =
    hs{name_remapping = M.insert (varName mono) poly (name_remapping hs)}

modifyState :: ModifyState (IdP p) => [ABExport p] -> HieState -> HieState
modifyState = foldr go id
  where
    go ABE{abe_poly=poly,abe_mono=mono} f = addSubstitution mono poly . f
    go _ f = f

type HieM = ReaderT HieState Hsc

-- | Construct an 'HieFile' from the outputs of the typechecker.
mkHieFile :: ModSummary
          -> TcGblEnv
          -> RenamedSource -> Hsc HieFile
mkHieFile ms ts rs = do
  let tc_binds = tcg_binds ts
  (asts', arr) <- getCompressedAsts tc_binds rs
  let Just src_file = ml_hs_file $ ms_location ms
  src <- liftIO $ BS.readFile src_file
  return $ HieFile
      { hie_hs_file = src_file
      , hie_module = ms_mod ms
      , hie_types = arr
      , hie_asts = asts'
      -- mkIfaceExports sorts the AvailInfos for stability
      , hie_exports = mkIfaceExports (tcg_exports ts)
      , hie_hs_src = src
      }

getCompressedAsts :: TypecheckedSource -> RenamedSource
  -> Hsc (HieASTs TypeIndex, A.Array TypeIndex HieTypeFlat)
getCompressedAsts ts rs = do
  asts <- enrichHie ts rs
  return $ compressTypes asts

enrichHie :: TypecheckedSource -> RenamedSource -> Hsc (HieASTs Type)
enrichHie ts (hsGrp, imports, exports, _) = flip runReaderT initState $ do
    tasts <- toHie $ fmap (BC RegularBind ModuleScope) ts
    rasts <- processGrp hsGrp
    imps <- toHie $ filter (not . ideclImplicit . unLoc) imports
    exps <- toHie $ fmap (map $ IEC Export . fst) exports
    let spanFile children = case children of
          [] -> mkRealSrcSpan (mkRealSrcLoc "" 1 1) (mkRealSrcLoc "" 1 1)
          _ -> mkRealSrcSpan (realSrcSpanStart $ nodeSpan $ head children)
                             (realSrcSpanEnd   $ nodeSpan $ last children)

        modulify xs =
          Node (simpleNodeInfo "Module" "Module") (spanFile xs) xs

        asts = HieASTs
          $ resolveTyVarScopes
          $ M.map (modulify . mergeSortAsts)
          $ M.fromListWith (++)
          $ map (\x -> (srcSpanFile (nodeSpan x),[x])) flat_asts

        flat_asts = concat
          [ tasts
          , rasts
          , imps
          , exps
          ]
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
getRealSpan (RealSrcSpan sp) = Just sp
getRealSpan _ = Nothing

grhss_span :: GRHSs p body -> SrcSpan
grhss_span (GRHSs _ xs bs) = foldl' combineSrcSpans (getLoc bs) (map getLoc xs)
grhss_span (XGRHSs _) = error "XGRHS has no span"

bindingsOnly :: [Context Name] -> [HieAST a]
bindingsOnly [] = []
bindingsOnly (C c n : xs) = case nameSrcSpan n of
  RealSrcSpan span -> Node nodeinfo span [] : bindingsOnly xs
    where nodeinfo = NodeInfo S.empty [] (M.singleton (Right n) info)
          info = mempty{identInfo = S.singleton c}
  _ -> bindingsOnly xs

concatM :: Monad m => [m [a]] -> m [a]
concatM xs = concat <$> sequence xs

{- Note [Capturing Scopes and other non local information]
toHie is a local tranformation, but scopes of bindings cannot be known locally,
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
  map (\(RS sc a) -> PS rsp useScope sc (unLoc a)) $
    listScopes patScope (map dL xs)

-- | 'listScopes' specialised to 'TVScoped' things
tvScopes
  :: TyVarScope
  -> Scope
  -> [LHsTyVarBndr a]
  -> [TVScoped (LHsTyVarBndr a)]
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
Because of HsWC and HsIB pass on their scope to their children
we must wrap the LHsType in pattern signatures in a
Shielded explictly, so that the HsWC/HsIB scope is not passed
on the the LHsType
-}

data Shielded a = SH Scope a -- Ignores its TScope, uses its own scope instead

type family ProtectedSig a where
  ProtectedSig GhcRn = HsWildCardBndrs GhcRn (HsImplicitBndrs
                                                GhcRn
                                                (Shielded (LHsType GhcRn)))
  ProtectedSig GhcTc = NoExt

class ProtectSig a where
  protectSig :: Scope -> LHsSigWcType (NoGhcTc a) -> ProtectedSig a

instance (HasLoc a) => HasLoc (Shielded a) where
  loc (SH _ a) = loc a

instance (ToHie (TScoped a)) => ToHie (TScoped (Shielded a)) where
  toHie (TS _ (SH sc a)) = toHie (TS (ResolvedScopes [sc]) a)

instance ProtectSig GhcTc where
  protectSig _ _ = NoExt

instance ProtectSig GhcRn where
  protectSig sc (HsWC a (HsIB b sig)) =
    HsWC a (HsIB b (SH sc sig))
  protectSig _ _ = error "protectSig not given HsWC (HsIB)"

class HasLoc a where
  -- ^ defined so that HsImplicitBndrs and HsWildCardBndrs can
  -- know what their implicit bindings are scoping over
  loc :: a -> SrcSpan

instance HasLoc thing => HasLoc (TScoped thing) where
  loc (TS _ a) = loc a

instance HasLoc thing => HasLoc (PScoped thing) where
  loc (PS _ _ _ a) = loc a

instance HasLoc (LHsQTyVars GhcRn) where
  loc (HsQTvs _ vs) = loc vs
  loc _ = noSrcSpan

instance HasLoc thing => HasLoc (HsImplicitBndrs a thing) where
  loc (HsIB _ a) = loc a
  loc _ = noSrcSpan

instance HasLoc thing => HasLoc (HsWildCardBndrs a thing) where
  loc (HsWC _ a) = loc a
  loc _ = noSrcSpan

instance HasLoc (Located a) where
  loc (L l _) = l

instance HasLoc a => HasLoc [a] where
  loc [] = noSrcSpan
  loc xs = foldl1' combineSrcSpans $ map loc xs

instance (HasLoc a, HasLoc b) => HasLoc (FamEqn s a b) where
  loc (FamEqn _ a Nothing b _ c) = foldl1' combineSrcSpans [loc a, loc b, loc c]
  loc (FamEqn _ a (Just tvs) b _ c) = foldl1' combineSrcSpans
                                              [loc a, loc tvs, loc b, loc c]
  loc _ = noSrcSpan
instance (HasLoc tm, HasLoc ty) => HasLoc (HsArg tm ty) where
  loc (HsValArg tm) = loc tm
  loc (HsTypeArg _ ty) = loc ty
  loc (HsArgPar sp)  = sp

instance HasLoc (HsDataDefn GhcRn) where
  loc def@(HsDataDefn{}) = loc $ dd_cons def
    -- Only used for data family instances, so we only need rhs
    -- Most probably the rest will be unhelpful anyway
  loc _ = noSrcSpan

instance HasLoc (Pat (GhcPass a)) where
  loc (dL -> L l _) = l

-- | The main worker class
class ToHie a where
  toHie :: a -> HieM [HieAST Type]

-- | Used to collect type info
class Data a => HasType a where
  getTypeNode :: a -> HieM [HieAST Type]

instance (ToHie a) => ToHie [a] where
  toHie = concatMapM toHie

instance (ToHie a) => ToHie (Bag a) where
  toHie = toHie . bagToList

instance (ToHie a) => ToHie (Maybe a) where
  toHie = maybe (pure []) toHie

instance ToHie (Context (Located NoExt)) where
  toHie _ = pure []

instance ToHie (TScoped NoExt) where
  toHie _ = pure []

instance ToHie (IEContext (Located ModuleName)) where
  toHie (IEC c (L (RealSrcSpan span) mname)) =
      pure $ [Node (NodeInfo S.empty [] idents) span []]
    where details = mempty{identInfo = S.singleton (IEThing c)}
          idents = M.singleton (Left mname) details
  toHie _ = pure []

instance ToHie (Context (Located Var)) where
  toHie c = case c of
      C context (L (RealSrcSpan span) name')
        -> do
        m <- asks name_remapping
        let name = M.findWithDefault name' (varName name') m
        pure
          [Node
            (NodeInfo S.empty [] $
              M.singleton (Right $ varName name)
                          (IdentifierDetails (Just $ varType name')
                                             (S.singleton context)))
            span
            []]
      _ -> pure []

instance ToHie (Context (Located Name)) where
  toHie c = case c of
      C context (L (RealSrcSpan span) name') -> do
        m <- asks name_remapping
        let name = case M.lookup name' m of
              Just var -> varName var
              Nothing -> name'
        pure
          [Node
            (NodeInfo S.empty [] $
              M.singleton (Right name)
                          (IdentifierDetails Nothing
                                             (S.singleton context)))
            span
            []]
      _ -> pure []

-- | Dummy instances - never called
instance ToHie (TScoped (LHsSigWcType GhcTc)) where
  toHie _ = pure []
instance ToHie (TScoped (LHsWcType GhcTc)) where
  toHie _ = pure []
instance ToHie (SigContext (LSig GhcTc)) where
  toHie _ = pure []
instance ToHie (TScoped Type) where
  toHie _ = pure []

instance HasType (LHsBind GhcRn) where
  getTypeNode (L spn bind) = makeNode bind spn

instance HasType (LHsBind GhcTc) where
  getTypeNode (L spn bind) = case bind of
      FunBind{fun_id = name} -> makeTypeNode bind spn (varType $ unLoc name)
      _ -> makeNode bind spn

instance HasType (LPat GhcRn) where
  getTypeNode (dL -> L spn pat) = makeNode pat spn

instance HasType (LPat GhcTc) where
  getTypeNode (dL -> L spn opat) = makeTypeNode opat spn (hsPatType opat)

instance HasType (LHsExpr GhcRn) where
  getTypeNode (L spn e) = makeNode e spn

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
instance HasType (LHsExpr GhcTc) where
  getTypeNode e@(L spn e') = lift $
    -- Some expression forms have their type immediately available
    let tyOpt = case e' of
          HsLit _ l -> Just (hsLitType l)
          HsOverLit _ o -> Just (overLitType o)

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
      _ | skipDesugaring e' -> fallback
        | otherwise -> do
            hs_env <- Hsc $ \e w -> return (e,w)
            (_,mbe) <- liftIO $ deSugarExpr hs_env e
            maybe fallback (makeTypeNode e' spn . exprType) mbe
    where
      fallback = makeNode e' spn

      matchGroupType :: MatchGroupTc -> Type
      matchGroupType (MatchGroupTc args res) = mkFunTys args res

      -- | Skip desugaring of these expressions for performance reasons.
      --
      -- See impact on Haddock output (esp. missing type annotations or links)
      -- before marking more things here as 'False'. See impact on Haddock
      -- performance before marking more things as 'True'.
      skipDesugaring :: HsExpr a -> Bool
      skipDesugaring e = case e of
        HsVar{}        -> False
        HsUnboundVar{} -> False
        HsConLikeOut{} -> False
        HsRecFld{}     -> False
        HsOverLabel{}  -> False
        HsIPVar{}      -> False
        HsWrap{}       -> False
        _              -> True

instance ( ToHie (Context (Located (IdP a)))
         , ToHie (MatchGroup a (LHsExpr a))
         , ToHie (PScoped (LPat a))
         , ToHie (GRHSs a (LHsExpr a))
         , ToHie (LHsExpr a)
         , ToHie (Located (PatSynBind a a))
         , HasType (LHsBind a)
         , ModifyState (IdP a)
         , Data (HsBind a)
         ) => ToHie (BindContext (LHsBind a)) where
  toHie (BC context scope b@(L span bind)) =
    concatM $ getTypeNode b : case bind of
      FunBind{fun_id = name, fun_matches = matches} ->
        [ toHie $ C (ValBind context scope $ getRealSpan span) name
        , toHie matches
        ]
      PatBind{pat_lhs = lhs, pat_rhs = rhs} ->
        [ toHie $ PS (getRealSpan span) scope NoScope lhs
        , toHie rhs
        ]
      VarBind{var_rhs = expr} ->
        [ toHie expr
        ]
      AbsBinds{abs_exports = xs, abs_binds = binds} ->
        [ local (modifyState xs) $ -- Note [Name Remapping]
            toHie $ fmap (BC context scope) binds
        ]
      PatSynBind _ psb ->
        [ toHie $ L span psb -- PatSynBinds only occur at the top level
        ]
      XHsBindsLR _ -> []

instance ( ToHie (LMatch a body)
         ) => ToHie (MatchGroup a body) where
  toHie mg = concatM $ case mg of
    MG{ mg_alts = (L span alts) , mg_origin = FromSource } ->
      [ pure $ locOnly span
      , toHie alts
      ]
    MG{} -> []
    XMatchGroup _ -> []

instance ( ToHie (Context (Located (IdP a)))
         , ToHie (PScoped (LPat a))
         , ToHie (HsPatSynDir a)
         ) => ToHie (Located (PatSynBind a a)) where
    toHie (L sp psb) = concatM $ case psb of
      PSB{psb_id=var, psb_args=dets, psb_def=pat, psb_dir=dir} ->
        [ toHie $ C (Decl PatSynDec $ getRealSpan sp) var
        , toHie $ toBind dets
        , toHie $ PS Nothing lhsScope NoScope pat
        , toHie dir
        ]
        where
          lhsScope = combineScopes varScope detScope
          varScope = mkLScope var
          detScope = case dets of
            (PrefixCon args) -> foldr combineScopes NoScope $ map mkLScope args
            (InfixCon a b) -> combineScopes (mkLScope a) (mkLScope b)
            (RecCon r) -> foldr go NoScope r
          go (RecordPatSynField a b) c = combineScopes c
            $ combineScopes (mkLScope a) (mkLScope b)
          detSpan = case detScope of
            LocalScope a -> Just a
            _ -> Nothing
          toBind (PrefixCon args) = PrefixCon $ map (C Use) args
          toBind (InfixCon a b) = InfixCon (C Use a) (C Use b)
          toBind (RecCon r) = RecCon $ map (PSC detSpan) r
      XPatSynBind _ -> []

instance ( ToHie (MatchGroup a (LHsExpr a))
         ) => ToHie (HsPatSynDir a) where
  toHie dir = case dir of
    ExplicitBidirectional mg -> toHie mg
    _ -> pure []

instance ( a ~ GhcPass p
         , ToHie body
         , ToHie (HsMatchContext (NameOrRdrName (IdP a)))
         , ToHie (PScoped (LPat a))
         , ToHie (GRHSs a body)
         , Data (Match a body)
         ) => ToHie (LMatch (GhcPass p) body) where
  toHie (L span m ) = concatM $ makeNode m span : case m of
    Match{m_ctxt=mctx, m_pats = pats, m_grhss =  grhss } ->
      [ toHie mctx
      , let rhsScope = mkScope $ grhss_span grhss
          in toHie $ patScopes Nothing rhsScope NoScope pats
      , toHie grhss
      ]
    XMatch _ -> []

instance ( ToHie (Context (Located a))
         ) => ToHie (HsMatchContext a) where
  toHie (FunRhs{mc_fun=name}) = toHie $ C MatchBind name
  toHie (StmtCtxt a) = toHie a
  toHie _ = pure []

instance ( ToHie (HsMatchContext a)
         ) => ToHie (HsStmtContext a) where
  toHie (PatGuard a) = toHie a
  toHie (ParStmtCtxt a) = toHie a
  toHie (TransStmtCtxt a) = toHie a
  toHie _ = pure []

instance ( a ~ GhcPass p
         , ToHie (Context (Located (IdP a)))
         , ToHie (RContext (HsRecFields a (PScoped (LPat a))))
         , ToHie (LHsExpr a)
         , ToHie (TScoped (LHsSigWcType a))
         , ProtectSig a
         , ToHie (TScoped (ProtectedSig a))
         , HasType (LPat a)
         , Data (HsSplice a)
         ) => ToHie (PScoped (LPat (GhcPass p))) where
  toHie (PS rsp scope pscope lpat@(dL -> L ospan opat)) =
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
                                 (combineScopes (mkLScope (dL pat)) pscope)
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
      ConPatIn c dets ->
        [ toHie $ C Use c
        , toHie $ contextify dets
        ]
      ConPatOut {pat_con = con, pat_args = dets}->
        [ toHie $ C Use $ fmap conLikeName con
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
        , let cscope = mkLScope (dL pat) in
            toHie $ TS (ResolvedScopes [cscope, scope, pscope])
                       (protectSig @a cscope sig)
              -- See Note [Scoping Rules for SigPat]
        ]
      CoPat _ _ _ _ ->
        []
      XPat _ -> []
    where
      contextify (PrefixCon args) = PrefixCon $ patScopes rsp scope pscope args
      contextify (InfixCon a b) = InfixCon a' b'
        where [a', b'] = patScopes rsp scope pscope [a,b]
      contextify (RecCon r) = RecCon $ RC RecFieldMatch $ contextify_rec r
      contextify_rec (HsRecFields fds a) = HsRecFields (map go scoped_fds) a
        where
          go (RS fscope (L spn (HsRecField lbl pat pun))) =
            L spn $ HsRecField lbl (PS rsp scope fscope pat) pun
          scoped_fds = listScopes pscope fds

instance ( ToHie body
         , ToHie (LGRHS a body)
         , ToHie (RScoped (LHsLocalBinds a))
         ) => ToHie (GRHSs a body) where
  toHie grhs = concatM $ case grhs of
    GRHSs _ grhss binds ->
     [ toHie grhss
     , toHie $ RS (mkScope $ grhss_span grhs) binds
     ]
    XGRHSs _ -> []

instance ( ToHie (Located body)
         , ToHie (RScoped (GuardLStmt a))
         , Data (GRHS a (Located body))
         ) => ToHie (LGRHS a (Located body)) where
  toHie (L span g) = concatM $ makeNode g span : case g of
    GRHS _ guards body ->
      [ toHie $ listScopes (mkLScope body) guards
      , toHie body
      ]
    XGRHS _ -> []

instance ( a ~ GhcPass p
         , ToHie (Context (Located (IdP a)))
         , HasType (LHsExpr a)
         , ToHie (PScoped (LPat a))
         , ToHie (MatchGroup a (LHsExpr a))
         , ToHie (LGRHS a (LHsExpr a))
         , ToHie (RContext (HsRecordBinds a))
         , ToHie (RFContext (Located (AmbiguousFieldOcc a)))
         , ToHie (ArithSeqInfo a)
         , ToHie (LHsCmdTop a)
         , ToHie (RScoped (GuardLStmt a))
         , ToHie (RScoped (LHsLocalBinds a))
         , ToHie (TScoped (LHsWcType (NoGhcTc a)))
         , ToHie (TScoped (LHsSigWcType (NoGhcTc a)))
         , Data (HsExpr a)
         , Data (HsSplice a)
         , Data (HsTupArg a)
         , Data (AmbiguousFieldOcc a)
         ) => ToHie (LHsExpr (GhcPass p)) where
  toHie e@(L mspan oexpr) = concatM $ getTypeNode e : case oexpr of
      HsVar _ (L _ var) ->
        [ toHie $ C Use (L mspan var)
             -- Patch up var location since typechecker removes it
        ]
      HsUnboundVar _ _ ->
        []
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
      HsIf _ _ a b c ->
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
        [ pure $ locOnly ispan
        , toHie $ listScopes NoScope stmts
        ]
      ExplicitList _ _ exprs ->
        [ toHie exprs
        ]
      RecordCon {rcon_con_name = name, rcon_flds = binds}->
        [ toHie $ C Use name
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
      HsSCC _ _ _ expr ->
        [ toHie expr
        ]
      HsCoreAnn _ _ _ expr ->
        [ toHie expr
        ]
      HsProc _ pat cmdtop ->
        [ toHie $ PS Nothing (mkLScope cmdtop) NoScope pat
        , toHie cmdtop
        ]
      HsStatic _ expr ->
        [ toHie expr
        ]
      HsArrApp _ a b _ _ ->
        [ toHie a
        , toHie b
        ]
      HsArrForm _ expr _ cmds ->
        [ toHie expr
        , toHie cmds
        ]
      HsTick _ _ expr ->
        [ toHie expr
        ]
      HsBinTick _ _ _ expr ->
        [ toHie expr
        ]
      HsTickPragma _ _ _ _ expr ->
        [ toHie expr
        ]
      HsWrap _ _ a ->
        [ toHie $ L mspan a
        ]
      HsBracket _ b ->
        [ toHie b
        ]
      HsRnBracketOut _ b p ->
        [ toHie b
        , toHie p
        ]
      HsTcBracketOut _ b p ->
        [ toHie b
        , toHie p
        ]
      HsSpliceE _ x ->
        [ toHie $ L mspan x
        ]
      EWildPat _ -> []
      EAsPat _ a b ->
        [ toHie $ C Use a
        , toHie b
        ]
      EViewPat _ a b ->
        [ toHie a
        , toHie b
        ]
      ELazyPat _ a ->
        [ toHie a
        ]
      XExpr _ -> []

instance ( a ~ GhcPass p
         , ToHie (LHsExpr a)
         , Data (HsTupArg a)
         ) => ToHie (LHsTupArg (GhcPass p)) where
  toHie (L span arg) = concatM $ makeNode arg span : case arg of
    Present _ expr ->
      [ toHie expr
      ]
    Missing _ -> []
    XTupArg _ -> []

instance ( a ~ GhcPass p
         , ToHie (PScoped (LPat a))
         , ToHie (LHsExpr a)
         , ToHie (SigContext (LSig a))
         , ToHie (RScoped (LHsLocalBinds a))
         , ToHie (RScoped (ApplicativeArg a))
         , ToHie (Located body)
         , Data (StmtLR a a (Located body))
         , Data (StmtLR a a (Located (HsExpr a)))
         ) => ToHie (RScoped (LStmt (GhcPass p) (Located body))) where
  toHie (RS scope (L span stmt)) = concatM $ makeNode stmt span : case stmt of
      LastStmt _ body _ _ ->
        [ toHie body
        ]
      BindStmt _ pat body _ _ ->
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
      XStmtLR _ -> []

instance ( ToHie (LHsExpr a)
         , ToHie (PScoped (LPat a))
         , ToHie (BindContext (LHsBind a))
         , ToHie (SigContext (LSig a))
         , ToHie (RScoped (HsValBindsLR a a))
         , Data (HsLocalBinds a)
         ) => ToHie (RScoped (LHsLocalBinds a)) where
  toHie (RS scope (L sp binds)) = concatM $ makeNode binds sp : case binds of
      EmptyLocalBinds _ -> []
      HsIPBinds _ _ -> []
      HsValBinds _ valBinds ->
        [ toHie $ RS (combineScopes scope $ mkScope sp)
                      valBinds
        ]
      XHsLocalBindsLR _ -> []

instance ( ToHie (BindContext (LHsBind a))
         , ToHie (SigContext (LSig a))
         , ToHie (RScoped (XXValBindsLR a a))
         ) => ToHie (RScoped (HsValBindsLR a a)) where
  toHie (RS sc v) = concatM $ case v of
    ValBinds _ binds sigs ->
      [ toHie $ fmap (BC RegularBind sc) binds
      , toHie $ fmap (SC (SI BindSig Nothing)) sigs
      ]
    XValBindsLR x -> [ toHie $ RS sc x ]

instance ToHie (RScoped (NHsValBindsLR GhcTc)) where
  toHie (RS sc (NValBinds binds sigs)) = concatM $
    [ toHie (concatMap (map (BC RegularBind sc) . bagToList . snd) binds)
    , toHie $ fmap (SC (SI BindSig Nothing)) sigs
    ]
instance ToHie (RScoped (NHsValBindsLR GhcRn)) where
  toHie (RS sc (NValBinds binds sigs)) = concatM $
    [ toHie (concatMap (map (BC RegularBind sc) . bagToList . snd) binds)
    , toHie $ fmap (SC (SI BindSig Nothing)) sigs
    ]

instance ( ToHie (RContext (LHsRecField a arg))
         ) => ToHie (RContext (HsRecFields a arg)) where
  toHie (RC c (HsRecFields fields _)) = toHie $ map (RC c) fields

instance ( ToHie (RFContext (Located label))
         , ToHie arg
         , HasLoc arg
         , Data label
         , Data arg
         ) => ToHie (RContext (LHsRecField' label arg)) where
  toHie (RC c (L span recfld)) = concatM $ makeNode recfld span : case recfld of
    HsRecField label expr _ ->
      [ toHie $ RFC c (getRealSpan $ loc expr) label
      , toHie expr
      ]

removeDefSrcSpan :: Name -> Name
removeDefSrcSpan n = setNameLoc n noSrcSpan

instance ToHie (RFContext (LFieldOcc GhcRn)) where
  toHie (RFC c rhs (L nspan f)) = concatM $ case f of
    FieldOcc name _ ->
      [ toHie $ C (RecField c rhs) (L nspan $ removeDefSrcSpan name)
      ]
    XFieldOcc _ -> []

instance ToHie (RFContext (LFieldOcc GhcTc)) where
  toHie (RFC c rhs (L nspan f)) = concatM $ case f of
    FieldOcc var _ ->
      let var' = setVarName var (removeDefSrcSpan $ varName var)
      in [ toHie $ C (RecField c rhs) (L nspan var')
         ]
    XFieldOcc _ -> []

instance ToHie (RFContext (Located (AmbiguousFieldOcc GhcRn))) where
  toHie (RFC c rhs (L nspan afo)) = concatM $ case afo of
    Unambiguous name _ ->
      [ toHie $ C (RecField c rhs) $ L nspan $ removeDefSrcSpan name
      ]
    Ambiguous _name _ ->
      [ ]
    XAmbiguousFieldOcc _ -> []

instance ToHie (RFContext (Located (AmbiguousFieldOcc GhcTc))) where
  toHie (RFC c rhs (L nspan afo)) = concatM $ case afo of
    Unambiguous var _ ->
      let var' = setVarName var (removeDefSrcSpan $ varName var)
      in [ toHie $ C (RecField c rhs) (L nspan var')
         ]
    Ambiguous var _ ->
      let var' = setVarName var (removeDefSrcSpan $ varName var)
      in [ toHie $ C (RecField c rhs) (L nspan var')
         ]
    XAmbiguousFieldOcc _ -> []

instance ( a ~ GhcPass p
         , ToHie (PScoped (LPat a))
         , ToHie (BindContext (LHsBind a))
         , ToHie (LHsExpr a)
         , ToHie (SigContext (LSig a))
         , ToHie (RScoped (HsValBindsLR a a))
         , Data (StmtLR a a (Located (HsExpr a)))
         , Data (HsLocalBinds a)
         ) => ToHie (RScoped (ApplicativeArg (GhcPass p))) where
  toHie (RS sc (ApplicativeArgOne _ pat expr _)) = concatM
    [ toHie $ PS Nothing sc NoScope pat
    , toHie expr
    ]
  toHie (RS sc (ApplicativeArgMany _ stmts _ pat)) = concatM
    [ toHie $ listScopes NoScope stmts
    , toHie $ PS Nothing sc NoScope pat
    ]
  toHie (RS _ (XApplicativeArg _)) = pure []

instance (ToHie arg, ToHie rec) => ToHie (HsConDetails arg rec) where
  toHie (PrefixCon args) = toHie args
  toHie (RecCon rec) = toHie rec
  toHie (InfixCon a b) = concatM [ toHie a, toHie b]

instance ( ToHie (LHsCmd a)
         , Data  (HsCmdTop a)
         ) => ToHie (LHsCmdTop a) where
  toHie (L span top) = concatM $ makeNode top span : case top of
    HsCmdTop _ cmd ->
      [ toHie cmd
      ]
    XCmdTop _ -> []

instance ( a ~ GhcPass p
         , ToHie (PScoped (LPat a))
         , ToHie (BindContext (LHsBind a))
         , ToHie (LHsExpr a)
         , ToHie (MatchGroup a (LHsCmd a))
         , ToHie (SigContext (LSig a))
         , ToHie (RScoped (HsValBindsLR a a))
         , Data (HsCmd a)
         , Data (HsCmdTop a)
         , Data (StmtLR a a (Located (HsCmd a)))
         , Data (HsLocalBinds a)
         , Data (StmtLR a a (Located (HsExpr a)))
         ) => ToHie (LHsCmd (GhcPass p)) where
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
        [ pure $ locOnly ispan
        , toHie $ listScopes NoScope stmts
        ]
      HsCmdWrap _ _ _ -> []
      XCmd _ -> []

instance ToHie (TyClGroup GhcRn) where
  toHie (TyClGroup _ classes roles instances) = concatM
    [ toHie classes
    , toHie roles
    , toHie instances
    ]
  toHie (XTyClGroup _) = pure []

instance ToHie (LTyClDecl GhcRn) where
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
        , concatMapM (pure . locOnly . getLoc) deftyps
        , toHie $ map (go . unLoc) deftyps
        ]
        where
          context_scope = mkLScope context
          rhs_scope = foldl1' combineScopes $ map mkScope
            [ loc deps, loc sigs, loc (bagToList meths), loc typs, loc deftyps]

          go :: TyFamDefltEqn GhcRn
             -> FamEqn GhcRn (TScoped (LHsQTyVars GhcRn)) (LHsType GhcRn)
          go (FamEqn a var bndrs pat b rhs) =
             FamEqn a var bndrs (TS (ResolvedScopes [mkLScope rhs]) pat) b rhs
          go (XFamEqn NoExt) = XFamEqn NoExt
      XTyClDecl _ -> []

instance ToHie (LFamilyDecl GhcRn) where
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
      XFamilyDecl _ -> []

instance ToHie (FamilyInfo GhcRn) where
  toHie (ClosedTypeFamily (Just eqns)) = concatM $
    [ concatMapM (pure . locOnly . getLoc) eqns
    , toHie $ map go eqns
    ]
    where
      go (L l ib) = TS (ResolvedScopes [mkScope l]) ib
  toHie _ = pure []

instance ToHie (RScoped (LFamilyResultSig GhcRn)) where
  toHie (RS sc (L span sig)) = concatM $ makeNode sig span : case sig of
      NoSig _ ->
        []
      KindSig _ k ->
        [ toHie k
        ]
      TyVarSig _ bndr ->
        [ toHie $ TVS (ResolvedScopes [sc]) NoScope bndr
        ]
      XFamilyResultSig _ -> []

instance ToHie (Located (FunDep (Located Name))) where
  toHie (L span fd@(lhs, rhs)) = concatM $
    [ makeNode fd span
    , toHie $ map (C Use) lhs
    , toHie $ map (C Use) rhs
    ]

instance (ToHie pats, ToHie rhs, HasLoc pats, HasLoc rhs)
    => ToHie (TScoped (FamEqn GhcRn pats rhs)) where
  toHie (TS _ f) = toHie f

instance ( ToHie pats
         , ToHie rhs
         , HasLoc pats
         , HasLoc rhs
         ) => ToHie (FamEqn GhcRn pats rhs) where
  toHie fe@(FamEqn _ var tybndrs pats _ rhs) = concatM $
    [ toHie $ C (Decl InstDec $ getRealSpan $ loc fe) var
    , toHie $ fmap (tvScopes (ResolvedScopes []) scope) tybndrs
    , toHie pats
    , toHie rhs
    ]
    where scope = combineScopes patsScope rhsScope
          patsScope = mkScope (loc pats)
          rhsScope = mkScope (loc rhs)
  toHie (XFamEqn _) = pure []

instance ToHie (LInjectivityAnn GhcRn) where
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
  toHie (XHsDataDefn _) = pure []

instance ToHie (HsDeriving GhcRn) where
  toHie (L span clauses) = concatM
    [ pure $ locOnly span
    , toHie clauses
    ]

instance ToHie (LHsDerivingClause GhcRn) where
  toHie (L span cl) = concatM $ makeNode cl span : case cl of
      HsDerivingClause _ strat (L ispan tys) ->
        [ toHie strat
        , pure $ locOnly ispan
        , toHie $ map (TS (ResolvedScopes [])) tys
        ]
      XHsDerivingClause _ -> []

instance ToHie (Located (DerivStrategy GhcRn)) where
  toHie (L span strat) = concatM $ makeNode strat span : case strat of
      StockStrategy -> []
      AnyclassStrategy -> []
      NewtypeStrategy -> []
      ViaStrategy s -> [ toHie $ TS (ResolvedScopes []) s ]

instance ToHie (Located OverlapMode) where
  toHie (L span _) = pure $ locOnly span

instance ToHie (LConDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      ConDeclGADT { con_names = names, con_qvars = qvars
                  , con_mb_cxt = ctx, con_args = args, con_res_ty = typ } ->
        [ toHie $ map (C (Decl ConDec $ getRealSpan span)) names
        , toHie $ TS (ResolvedScopes [ctxScope, rhsScope]) qvars
        , toHie ctx
        , toHie args
        , toHie typ
        ]
        where
          rhsScope = combineScopes argsScope tyScope
          ctxScope = maybe NoScope mkLScope ctx
          argsScope = condecl_scope args
          tyScope = mkLScope typ
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
          argsScope = condecl_scope dets
      XConDecl _ -> []
    where condecl_scope args = case args of
            PrefixCon xs -> foldr combineScopes NoScope $ map mkLScope xs
            InfixCon a b -> combineScopes (mkLScope a) (mkLScope b)
            RecCon x -> mkLScope x

instance ToHie (Located [LConDeclField GhcRn]) where
  toHie (L span decls) = concatM $
    [ pure $ locOnly span
    , toHie decls
    ]

instance ( HasLoc thing
         , ToHie (TScoped thing)
         ) => ToHie (TScoped (HsImplicitBndrs GhcRn thing)) where
  toHie (TS sc (HsIB ibrn a)) = concatM $
      [ pure $ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) ibrn
      , toHie $ TS sc a
      ]
    where span = loc a
  toHie (TS _ (XHsImplicitBndrs _)) = pure []

instance ( HasLoc thing
         , ToHie (TScoped thing)
         ) => ToHie (TScoped (HsWildCardBndrs GhcRn thing)) where
  toHie (TS sc (HsWC names a)) = concatM $
      [ pure $ bindingsOnly $ map (C $ TyVarBind (mkScope span) sc) names
      , toHie $ TS sc a
      ]
    where span = loc a
  toHie (TS _ (XHsWildCardBndrs _)) = pure []

instance ToHie (SigContext (LSig GhcRn)) where
  toHie (SC (SI styp msp) (L sp sig)) = concatM $ makeNode sig sp : case sig of
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
        , pure $ maybe [] (locOnly . getLoc) mtxt
        ]
      CompleteMatchSig _ _ (L ispan names) typ ->
        [ pure $ locOnly ispan
        , toHie $ map (C Use) names
        , toHie $ fmap (C Use) typ
        ]
      XSig _ -> []

instance ToHie (LHsType GhcRn) where
  toHie x = toHie $ TS (ResolvedScopes []) x

instance ToHie (TScoped (LHsType GhcRn)) where
  toHie (TS tsc (L span t)) = concatM $ makeNode t span : case t of
      HsForAllTy _ bndrs body ->
        [ toHie $ tvScopes tsc (mkScope $ getLoc body) bndrs
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
        , toHie $ TS (ResolvedScopes []) ki
        ]
      HsFunTy _ a b ->
        [ toHie a
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
  toHie (HsArgPar sp) = pure $ locOnly sp

instance ToHie (TVScoped (LHsTyVarBndr GhcRn)) where
  toHie (TVS tsc sc (L span bndr)) = concatM $ makeNode bndr span : case bndr of
      UserTyVar _ var ->
        [ toHie $ C (TyVarBind sc tsc) var
        ]
      KindedTyVar _ var kind ->
        [ toHie $ C (TyVarBind sc tsc) var
        , toHie kind
        ]
      XTyVarBndr _ -> []

instance ToHie (TScoped (LHsQTyVars GhcRn)) where
  toHie (TS sc (HsQTvs (HsQTvsRn implicits _) vars)) = concatM $
    [ pure $ bindingsOnly bindings
    , toHie $ tvScopes sc NoScope vars
    ]
    where
      varLoc = loc vars
      bindings = map (C $ TyVarBind (mkScope varLoc) sc) implicits
  toHie (TS _ (XLHsQTyVars _)) = pure []

instance ToHie (LHsContext GhcRn) where
  toHie (L span tys) = concatM $
      [ pure $ locOnly span
      , toHie tys
      ]

instance ToHie (LConDeclField GhcRn) where
  toHie (L span field) = concatM $ makeNode field span : case field of
      ConDeclField _ fields typ _ ->
        [ toHie $ map (RFC RecFieldDecl (getRealSpan $ loc typ)) fields
        , toHie typ
        ]
      XConDeclField _ -> []

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

instance ToHie (LSpliceDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      SpliceDecl _ splice _ ->
        [ toHie splice
        ]
      XSpliceDecl _ -> []

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

instance ( ToHie (LHsExpr a)
         , Data (HsSplice a)
         ) => ToHie (Located (HsSplice a)) where
  toHie (L span sp) = concatM $ makeNode sp span : case sp of
      HsTypedSplice _ _ _ expr ->
        [ toHie expr
        ]
      HsUntypedSplice _ _ _ expr ->
        [ toHie expr
        ]
      HsQuasiQuote _ _ _ ispan _ ->
        [ pure $ locOnly ispan
        ]
      HsSpliced _ _ _ ->
        []
      HsSplicedT _ ->
        []
      XSplice _ -> []

instance ToHie (LRoleAnnotDecl GhcRn) where
  toHie (L span annot) = concatM $ makeNode annot span : case annot of
      RoleAnnotDecl _ var roles ->
        [ toHie $ C Use var
        , concatMapM (pure . locOnly . getLoc) roles
        ]
      XRoleAnnotDecl _ -> []

instance ToHie (LInstDecl GhcRn) where
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
      XInstDecl _ -> []

instance ToHie (LClsInstDecl GhcRn) where
  toHie (L span decl) = concatM
    [ toHie $ TS (ResolvedScopes [mkScope span]) $ cid_poly_ty decl
    , toHie $ fmap (BC InstanceBind ModuleScope) $ cid_binds decl
    , toHie $ map (SC $ SI InstSig $ getRealSpan span) $ cid_sigs decl
    , pure $ concatMap (locOnly . getLoc) $ cid_tyfam_insts decl
    , toHie $ cid_tyfam_insts decl
    , pure $ concatMap (locOnly . getLoc) $ cid_datafam_insts decl
    , toHie $ cid_datafam_insts decl
    , toHie $ cid_overlap_mode decl
    ]

instance ToHie (LDataFamInstDecl GhcRn) where
  toHie (L sp (DataFamInstDecl d)) = toHie $ TS (ResolvedScopes [mkScope sp]) d

instance ToHie (LTyFamInstDecl GhcRn) where
  toHie (L sp (TyFamInstDecl d)) = toHie $ TS (ResolvedScopes [mkScope sp]) d

instance ToHie (Context a)
         => ToHie (PatSynFieldContext (RecordPatSynField a)) where
  toHie (PSC sp (RecordPatSynField a b)) = concatM $
    [ toHie $ C (RecField RecFieldDecl sp) a
    , toHie $ C Use b
    ]

instance ToHie (LDerivDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      DerivDecl _ typ strat overlap ->
        [ toHie $ TS (ResolvedScopes []) typ
        , toHie strat
        , toHie overlap
        ]
      XDerivDecl _ -> []

instance ToHie (LFixitySig GhcRn) where
  toHie (L span sig) = concatM $ makeNode sig span : case sig of
      FixitySig _ vars _ ->
        [ toHie $ map (C Use) vars
        ]
      XFixitySig _ -> []

instance ToHie (LDefaultDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      DefaultDecl _ typs ->
        [ toHie typs
        ]
      XDefaultDecl _ -> []

instance ToHie (LForeignDecl GhcRn) where
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
      XForeignDecl _ -> []

instance ToHie ForeignImport where
  toHie (CImport (L a _) (L b _) _ _ (L c _)) = pure $ concat $
    [ locOnly a
    , locOnly b
    , locOnly c
    ]

instance ToHie ForeignExport where
  toHie (CExport (L a _) (L b _)) = pure $ concat $
    [ locOnly a
    , locOnly b
    ]

instance ToHie (LWarnDecls GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      Warnings _ _ warnings ->
        [ toHie warnings
        ]
      XWarnDecls _ -> []

instance ToHie (LWarnDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      Warning _ vars _ ->
        [ toHie $ map (C Use) vars
        ]
      XWarnDecl _ -> []

instance ToHie (LAnnDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      HsAnnotation _ _ prov expr ->
        [ toHie prov
        , toHie expr
        ]
      XAnnDecl _ -> []

instance ToHie (Context (Located a)) => ToHie (AnnProvenance a) where
  toHie (ValueAnnProvenance a) = toHie $ C Use a
  toHie (TypeAnnProvenance a) = toHie $ C Use a
  toHie ModuleAnnProvenance = pure []

instance ToHie (LRuleDecls GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      HsRules _ _ rules ->
        [ toHie rules
        ]
      XRuleDecls _ -> []

instance ToHie (LRuleDecl GhcRn) where
  toHie (L _ (XRuleDecl _)) = pure []
  toHie (L span r@(HsRule _ rname _ tybndrs bndrs exprA exprB)) = concatM
        [ makeNode r span
        , pure $ locOnly $ getLoc rname
        , toHie $ fmap (tvScopes (ResolvedScopes []) scope) tybndrs
        , toHie $ map (RS $ mkScope span) bndrs
        , toHie exprA
        , toHie exprB
        ]
    where scope = bndrs_sc `combineScopes` exprA_sc `combineScopes` exprB_sc
          bndrs_sc = maybe NoScope mkLScope (listToMaybe bndrs)
          exprA_sc = mkLScope exprA
          exprB_sc = mkLScope exprB

instance ToHie (RScoped (LRuleBndr GhcRn)) where
  toHie (RS sc (L span bndr)) = concatM $ makeNode bndr span : case bndr of
      RuleBndr _ var ->
        [ toHie $ C (ValBind RegularBind sc Nothing) var
        ]
      RuleBndrSig _ var typ ->
        [ toHie $ C (ValBind RegularBind sc Nothing) var
        , toHie $ TS (ResolvedScopes [sc]) typ
        ]
      XRuleBndr _ -> []

instance ToHie (LImportDecl GhcRn) where
  toHie (L span decl) = concatM $ makeNode decl span : case decl of
      ImportDecl { ideclName = name, ideclAs = as, ideclHiding = hidden } ->
        [ toHie $ IEC Import name
        , toHie $ fmap (IEC ImportAs) as
        , maybe (pure []) goIE hidden
        ]
      XImportDecl _ -> []
    where
      goIE (hiding, (L sp liens)) = concatM $
        [ pure $ locOnly sp
        , toHie $ map (IEC c) liens
        ]
        where
         c = if hiding then ImportHiding else Import

instance ToHie (IEContext (LIE GhcRn)) where
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
      IEThingWith _ n _ ns flds ->
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
      XIE _ -> []

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

instance ToHie (IEContext (Located (FieldLbl Name))) where
  toHie (IEC c (L span lbl)) = concatM $ makeNode lbl span : case lbl of
      FieldLabel _ _ n ->
        [ toHie $ C (IEThing c) $ L span n
        ]
