
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Desugaring step of the
-- [Lower Your Guards paper](https://dl.acm.org/doi/abs/10.1145/3408989).
--
-- Desugars Haskell source syntax into guard tree variants Pm*.
-- In terms of the paper, this module is concerned with Sections 3.1, Figure 4,
-- in particular.
module GHC.HsToCore.Pmc.Desugar (
      desugarPatBind, desugarGRHSs, desugarMatches, desugarEmptyCase
    ) where

import GHC.Prelude

import GHC.HsToCore.Pmc.Types
import GHC.HsToCore.Pmc.Utils
import GHC.Core (Expr(Var,App))
import GHC.Data.FastString (unpackFS, lengthFS)
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session
import GHC.Hs
import GHC.Tc.Utils.Zonk (shortCutLit)
import GHC.Types.Id
import GHC.Core.ConLike
import GHC.Types.Name
import GHC.Builtin.Types
import GHC.Builtin.Names (rationalTyConName)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Core.DataCon
import GHC.Types.Var (EvVar)
import GHC.Core.Coercion
import GHC.Tc.Types.Evidence (HsWrapper(..), isIdHsWrapper)
import {-# SOURCE #-} GHC.HsToCore.Expr (dsExpr, dsLExpr, dsSyntaxExpr)
import {-# SOURCE #-} GHC.HsToCore.Binds (dsHsWrapper)
import GHC.HsToCore.Utils (isTrueLHsExpr, selectMatchVar)
import GHC.HsToCore.Match.Literal (dsLit, dsOverLit)
import GHC.HsToCore.Monad
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Data.Maybe
import qualified GHC.LanguageExtensions as LangExt
import GHC.Utils.Monad (concatMapM)
import GHC.Types.SourceText (FractionalLit(..))
import Control.Monad (zipWithM)
import Data.List (elemIndex)
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE

-- import GHC.Driver.Ppr

-- | Smart constructor that eliminates trivial lets
mkPmLetVar :: Id -> Id -> [PmGrd]
mkPmLetVar x y | x == y = []
mkPmLetVar x y          = [PmLet x (Var y)]

-- | ADT constructor pattern => no existentials, no local constraints
vanillaConGrd :: Id -> DataCon -> [Id] -> PmGrd
vanillaConGrd scrut con arg_ids =
  PmCon { pm_id = scrut, pm_con_con = PmAltConLike (RealDataCon con)
        , pm_con_tvs = [], pm_con_dicts = [], pm_con_args = arg_ids }

-- | Creates a '[PmGrd]' refining a match var of list type to a list,
-- where list fields are matched against the incoming tagged '[PmGrd]'s.
-- For example:
--   @mkListGrds "a" "[(x, True <- x),(y, !y)]"@
-- to
--   @"[(x:b) <- a, True <- x, (y:c) <- b, !y, [] <- c]"@
-- where @b@ and @c@ are freshly allocated in @mkListGrds@ and @a@ is the match
-- variable.
mkListGrds :: Id -> [(Id, [PmGrd])] -> DsM [PmGrd]
-- See Note [Order of guards matter] for why we need to intertwine guards
-- on list elements.
mkListGrds a []                  = pure [vanillaConGrd a nilDataCon []]
mkListGrds a ((x, head_grds):xs) = do
  b <- mkPmId (idType a)
  tail_grds <- mkListGrds b xs
  pure $ vanillaConGrd a consDataCon [x, b] : head_grds ++ tail_grds

-- | Create a '[PmGrd]' refining a match variable to a 'PmLit'.
mkPmLitGrds :: Id -> PmLit -> DsM [PmGrd]
mkPmLitGrds x (PmLit _ (PmLitString s)) = do
  -- We desugar String literals to list literals for better overlap reasoning.
  -- It's a little unfortunate we do this here rather than in
  -- 'GHC.HsToCore.Pmc.Solver.trySolve' and
  -- 'GHC.HsToCore.Pmc.Solver.addRefutableAltCon', but it's so much simpler
  -- here. See Note [Representation of Strings in TmState] in
  -- GHC.HsToCore.Pmc.Solver
  vars <- traverse mkPmId (take (lengthFS s) (repeat charTy))
  let mk_char_lit y c = mkPmLitGrds y (PmLit charTy (PmLitChar c))
  char_grdss <- zipWithM mk_char_lit vars (unpackFS s)
  mkListGrds x (zip vars char_grdss)
mkPmLitGrds x lit = do
  let grd = PmCon { pm_id = x
                  , pm_con_con = PmAltLit lit
                  , pm_con_tvs = []
                  , pm_con_dicts = []
                  , pm_con_args = [] }
  pure [grd]

-- | @desugarPat _ x pat@ transforms @pat@ into a '[PmGrd]', where
-- the variable representing the match is @x@.
desugarPat :: Id -> Pat GhcTc -> DsM [PmGrd]
desugarPat x pat = case pat of
  WildPat  _ty -> pure []
  VarPat _ y   -> pure (mkPmLetVar (unLoc y) x)
  ParPat _ _ p _ -> desugarLPat x p
  LazyPat _ _  -> pure [] -- like a wildcard
  BangPat _ p@(L l p') ->
    -- Add the bang in front of the list, because it will happen before any
    -- nested stuff.
    (PmBang x pm_loc :) <$> desugarLPat x p
      where pm_loc = Just (SrcInfo (L (locA l) (ppr p')))

  -- (x@pat)   ==>   Desugar pat with x as match var and handle impedance
  --                 mismatch with incoming match var
  AsPat _ (L _ y) p -> (mkPmLetVar y x ++) <$> desugarLPat y p

  SigPat _ p _ty -> desugarLPat x p

  XPat ext -> case ext of

    ExpansionPat orig expansion -> do
      dflags <- getDynFlags
      case orig of
        -- We add special logic for overloaded list patterns. When:
        --   - a ViewPat is the expansion of a ListPat,
        --   - RebindableSyntax is off,
        --   - the type of the pattern is the built-in list type,
        -- then we assume that the view function, 'toList', is the identity.
        -- This improves pattern-match overload checks, as this will allow
        -- the pattern match checker to directly inspect the inner pattern.
        -- See #14547, and Note [Desugaring overloaded list patterns] (Wrinkle).
        ListPat {}
          | ViewPat arg_ty _lexpr pat <- expansion
          , not (xopt LangExt.RebindableSyntax dflags)
          , Just _ <- splitListTyConApp_maybe arg_ty
          -> desugarLPat x pat

        _ -> desugarPat x expansion

    -- See Note [Desugar CoPats]
    -- Generally the translation is
    -- pat |> co   ===>   let y = x |> co, pat <- y  where y is a match var of pat
    CoPat wrapper p _ty
      | isIdHsWrapper wrapper                   -> desugarPat x p
      | WpCast co <-  wrapper, isReflexiveCo co -> desugarPat x p
      | otherwise -> do
          (y, grds) <- desugarPatV p
          wrap_rhs_y <- dsHsWrapper wrapper
          pure (PmLet y (wrap_rhs_y (Var x)) : grds)

  -- (n + k)  ===>   let b = x >= k, True <- b, let n = x-k
  NPlusKPat _pat_ty (L _ n) k1 k2 ge minus -> do
    b <- mkPmId boolTy
    let grd_b = vanillaConGrd b trueDataCon []
    [ke1, ke2] <- traverse dsOverLit [unLoc k1, k2]
    rhs_b <- dsSyntaxExpr ge    [Var x, ke1]
    rhs_n <- dsSyntaxExpr minus [Var x, ke2]
    pure [PmLet b rhs_b, grd_b, PmLet n rhs_n]

  -- (fun -> pat)   ===>   let y = fun x, pat <- y where y is a match var of pat
  ViewPat _arg_ty lexpr pat -> do
    (y, grds) <- desugarLPatV pat
    fun <- dsLExpr lexpr
    pure $ PmLet y (App fun (Var x)) : grds

  -- list
  ListPat _ ps ->
    desugarListPat x ps

  ConPat { pat_con     = L _ con
         , pat_args    = ps
         , pat_con_ext = ConPatTc
           { cpt_arg_tys = arg_tys
           , cpt_tvs     = ex_tvs
           , cpt_dicts   = dicts
           }
         } ->
    desugarConPatOut x con arg_tys ex_tvs dicts ps

  NPat ty (L _ olit) mb_neg _ -> do
    -- See Note [Literal short cut] in "GHC.HsToCore.Match.Literal"
    -- We inline the Literal short cut for @ty@ here, because @ty@ is more
    -- precise than the field of OverLitTc, which is all that dsOverLit (which
    -- normally does the literal short cut) can look at. Also @ty@ matches the
    -- type of the scrutinee, so info on both pattern and scrutinee (for which
    -- short cutting in dsOverLit works properly) is overloaded iff either is.
    dflags <- getDynFlags
    let platform = targetPlatform dflags
    pm_lit <- case olit of
      OverLit{ ol_val = val, ol_ext = OverLitTc { ol_rebindable = rebindable } }
        | not rebindable
        , Just expr <- shortCutLit platform val ty
        -> coreExprAsPmLit <$> dsExpr expr
        | not rebindable
        , (HsFractional f) <- val
        , negates <- if fl_neg f then 1 else 0
        -> do
            rat_tc <- dsLookupTyCon rationalTyConName
            let rat_ty = mkTyConTy rat_tc
            return $ Just $ PmLit rat_ty (PmLitOverRat negates f)
        | otherwise
        -> do
           dsLit <- dsOverLit olit
           let !pmLit = coreExprAsPmLit dsLit :: Maybe PmLit
          --  pprTraceM "desugarPat"
          --     (
          --       text "val" <+> ppr val $$
          --       text "witness" <+> ppr (ol_witness olit) $$
          --       text "dsLit" <+> ppr dsLit $$
          --       text "asPmLit" <+> ppr pmLit
          --     )
           return pmLit

    let lit = case pm_lit of
          Just l -> l
          Nothing -> pprPanic "failed to detect OverLit" (ppr olit)
    let lit' = case mb_neg of
          Just _  -> expectJust "failed to negate lit" (negatePmLit lit)
          Nothing -> lit
    mkPmLitGrds x lit'

  LitPat _ lit -> do
    core_expr <- dsLit (convertLit lit)
    let lit = expectJust "failed to detect Lit" (coreExprAsPmLit core_expr)
    mkPmLitGrds x lit

  TuplePat _tys pats boxity -> do
    (vars, grdss) <- mapAndUnzipM desugarLPatV pats
    let tuple_con = tupleDataCon boxity (length vars)
    pure $ vanillaConGrd x tuple_con vars : concat grdss

  SumPat _ty p alt arity -> do
    (y, grds) <- desugarLPatV p
    let sum_con = sumDataCon alt arity
    -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
    pure $ vanillaConGrd x sum_con [y] : grds

  SplicePat {} -> panic "Check.desugarPat: SplicePat"

-- | 'desugarPat', but also select and return a new match var.
desugarPatV :: Pat GhcTc -> DsM (Id, [PmGrd])
desugarPatV pat = do
  x <- selectMatchVar Many pat
  grds <- desugarPat x pat
  pure (x, grds)

desugarLPat :: Id -> LPat GhcTc -> DsM [PmGrd]
desugarLPat x = desugarPat x . unLoc

desugarLMatchPat :: Id -> LMatchPat GhcTc -> DsM [PmGrd]
desugarLMatchPat x (L _ (VisPat _ pat)) = desugarLPat x pat
desugarLMatchPat _ _                    = panic "we don't have other patterns yet"

-- | 'desugarLPat', but also select and return a new match var.
desugarLPatV :: LPat GhcTc -> DsM (Id, [PmGrd])
desugarLPatV = desugarPatV . unLoc

-- | @desugarListPat _ x [p1, ..., pn]@ is basically
--   @desugarConPatOut _ x $(mkListConPatOuts [p1, ..., pn]>@ without ever
-- constructing the 'ConPatOut's.
desugarListPat :: Id -> [LPat GhcTc] -> DsM [PmGrd]
desugarListPat x pats = do
  vars_and_grdss <- traverse desugarLPatV pats
  mkListGrds x vars_and_grdss

-- | Desugar a constructor pattern
desugarConPatOut :: Id -> ConLike -> [Type] -> [TyVar]
                 -> [EvVar] -> HsConPatDetails GhcTc -> DsM [PmGrd]
desugarConPatOut x con univ_tys ex_tvs dicts = \case
    PrefixCon _ ps               -> go_field_pats (zip [0..] ps)
    InfixCon  p1 p2              -> go_field_pats (zip [0..] [p1,p2])
    RecCon    (HsRecFields fs _) -> go_field_pats (rec_field_ps fs)
  where
    -- The actual argument types (instantiated)
    arg_tys     = map scaledThing $ conLikeInstOrigArgTys con (univ_tys ++ mkTyVarTys ex_tvs)

    -- Extract record field patterns tagged by field index from a list of
    -- LHsRecField
    rec_field_ps fs = map (tagged_pat . unLoc) fs
      where
        tagged_pat f = (lbl_to_index (getName (hsRecFieldId f)), hfbRHS f)
        -- Unfortunately the label info is empty when the DataCon wasn't defined
        -- with record field labels, hence we desugar to field index.
        orig_lbls        = map flSelector $ conLikeFieldLabels con
        lbl_to_index lbl = expectJust "lbl_to_index" $ elemIndex lbl orig_lbls

    go_field_pats tagged_pats = do
      -- The fields that appear might not be in the correct order. So
      --   1. Do the PmCon match
      --   2. Then pattern match on the fields in the order given by the first
      --      field of @tagged_pats@.
      -- See Note [Field match order for RecCon]

      -- Desugar the mentioned field patterns. We're doing this first to get
      -- the Ids for pm_con_args and bring them in order afterwards.
      let trans_pat (n, pat) = do
            (var, pvec) <- desugarLPatV pat
            pure ((n, var), pvec)
      (tagged_vars, arg_grdss) <- mapAndUnzipM trans_pat tagged_pats

      let get_pat_id n ty = case lookup n tagged_vars of
            Just var -> pure var
            Nothing  -> mkPmId ty

      -- 1. the constructor pattern match itself
      arg_ids <- zipWithM get_pat_id [0..] arg_tys
      let con_grd = PmCon x (PmAltConLike con) ex_tvs dicts arg_ids

      -- 2. guards from field selector patterns
      let arg_grds = concat arg_grdss

      -- tracePm "ConPatOut" (ppr x $$ ppr con $$ ppr arg_ids)
      pure (con_grd : arg_grds)

desugarPatBind :: SrcSpan -> Id -> Pat GhcTc -> DsM (PmPatBind Pre)
-- See 'GrdPatBind' for how this simply repurposes GrdGRHS.
desugarPatBind loc var pat =
  PmPatBind . flip PmGRHS (SrcInfo (L loc (ppr pat))) . GrdVec <$> desugarPat var pat

desugarEmptyCase :: Id -> DsM PmEmptyCase
desugarEmptyCase var = pure PmEmptyCase { pe_var = var }

-- | Desugar the non-empty 'Match'es of a 'MatchGroup'.
desugarMatches :: [Id] -> NonEmpty (LMatch GhcTc (LHsExpr GhcTc))
               -> DsM (PmMatchGroup Pre)
desugarMatches vars matches =
  PmMatchGroup <$> traverse (desugarMatch vars) matches

-- Desugar a single match
desugarMatch :: [Id] -> LMatch GhcTc (LHsExpr GhcTc) -> DsM (PmMatch Pre)
desugarMatch vars (L match_loc (Match { m_pats = pats, m_grhss = grhss })) = do
  pats'  <- concat <$> zipWithM desugarLMatchPat vars pats
  grhss' <- desugarGRHSs (locA match_loc) (sep (map ppr pats)) grhss
  -- tracePm "desugarMatch" (vcat [ppr pats, ppr pats', ppr grhss'])
  return PmMatch { pm_pats = GrdVec pats', pm_grhss = grhss' }

desugarGRHSs :: SrcSpan -> SDoc -> GRHSs GhcTc (LHsExpr GhcTc) -> DsM (PmGRHSs Pre)
desugarGRHSs match_loc pp_pats grhss = do
  lcls <- desugarLocalBinds (grhssLocalBinds grhss)
  grhss' <- traverse (desugarLGRHS match_loc pp_pats)
              . expectJust "desugarGRHSs"
              . NE.nonEmpty
              $ grhssGRHSs grhss
  return PmGRHSs { pgs_lcls = GrdVec lcls, pgs_grhss = grhss' }

-- | Desugar a guarded right-hand side to a single 'GrdTree'
desugarLGRHS :: SrcSpan -> SDoc -> LGRHS GhcTc (LHsExpr GhcTc) -> DsM (PmGRHS Pre)
desugarLGRHS match_loc pp_pats (L _loc (GRHS _ gs _)) = do
  -- _loc points to the match separator (ie =, ->) that comes after the guards.
  -- Hence we have to pass in the match_loc, which we use in case that the RHS
  -- is unguarded.
  -- pp_pats is the space-separated pattern of the current Match this
  -- GRHS belongs to, so the @A B x@ part in @A B x | 0 <- x@.
  let rhs_info = case gs of
        []              -> L match_loc      pp_pats
        (L grd_loc _):_ -> L (locA grd_loc) (pp_pats <+> vbar <+> interpp'SP gs)
  grds <- concatMapM (desugarGuard . unLoc) gs
  pure PmGRHS { pg_grds = GrdVec grds, pg_rhs = SrcInfo rhs_info }

-- | Desugar a guard statement to a '[PmGrd]'
desugarGuard :: GuardStmt GhcTc -> DsM [PmGrd]
desugarGuard guard = case guard of
  BodyStmt _   e _ _ -> desugarBoolGuard e
  LetStmt  _   binds -> desugarLocalBinds binds
  BindStmt _ p e     -> desugarBind p e
  LastStmt        {} -> panic "desugarGuard LastStmt"
  ParStmt         {} -> panic "desugarGuard ParStmt"
  TransStmt       {} -> panic "desugarGuard TransStmt"
  RecStmt         {} -> panic "desugarGuard RecStmt"
  ApplicativeStmt {} -> panic "desugarGuard ApplicativeLastStmt"

-- | Desugar local bindings to a bunch of 'PmLet' guards.
-- Deals only with simple @let@ or @where@ bindings without any polymorphism,
-- recursion, pattern bindings etc.
-- See Note [Long-distance information for HsLocalBinds].
desugarLocalBinds :: HsLocalBinds GhcTc -> DsM [PmGrd]
desugarLocalBinds (HsValBinds _ (XValBindsLR (NValBinds binds _))) =
  concatMapM (concatMapM go . bagToList) (map snd binds)
  where
    go :: LHsBind GhcTc -> DsM [PmGrd]
    go (L _ FunBind{fun_id = L _ x, fun_matches = mg})
      -- See Note [Long-distance information for HsLocalBinds] for why this
      -- pattern match is so very specific.
      | L _ [L _ Match{m_pats = [], m_grhss = grhss}] <- mg_alts mg
      , GRHSs{grhssGRHSs = [L _ (GRHS _ _grds rhs)]} <- grhss = do
          core_rhs <- dsLExpr rhs
          return [PmLet x core_rhs]
    go (L _ AbsBinds{ abs_tvs = [], abs_ev_vars = []
                    , abs_exports=exports, abs_binds = binds }) = do
      -- Typechecked HsLocalBinds are wrapped in AbsBinds, which carry
      -- renamings. See Note [Long-distance information for HsLocalBinds]
      -- for the details.
      let go_export :: ABExport GhcTc -> Maybe PmGrd
          go_export ABE{abe_poly = x, abe_mono = y, abe_wrap = wrap}
            | isIdHsWrapper wrap
            = assertPpr (idType x `eqType` idType y)
                        (ppr x $$ ppr (idType x) $$ ppr y $$ ppr (idType y)) $
              Just $ PmLet x (Var y)
            | otherwise
            = Nothing
      let exps = mapMaybe go_export exports
      bs <- concatMapM go (bagToList binds)
      return (exps ++ bs)
    go _ = return []
desugarLocalBinds _binds = return []

-- | Desugar a pattern guard
--   @pat <- e ==>  let x = e;  <guards for pat <- x>@
desugarBind :: LPat GhcTc -> LHsExpr GhcTc -> DsM [PmGrd]
desugarBind p e = dsLExpr e >>= \case
  Var y
    | Nothing <- isDataConId_maybe y
    -- RHS is a variable, so that will allow us to omit the let
    -> desugarLPat y p
  rhs -> do
    (x, grds) <- desugarLPatV p
    pure (PmLet x rhs : grds)

-- | Desugar a boolean guard
--   @e ==>  let x = e; True <- x@
desugarBoolGuard :: LHsExpr GhcTc -> DsM [PmGrd]
desugarBoolGuard e
  | isJust (isTrueLHsExpr e) = return []
    -- The formal thing to do would be to generate (True <- True)
    -- but it is trivial to solve so instead we give back an empty
    -- [PmGrd] for efficiency
  | otherwise = dsLExpr e >>= \case
      Var y
        | Nothing <- isDataConId_maybe y
        -- Omit the let by matching on y
        -> pure [vanillaConGrd y trueDataCon []]
      rhs -> do
        x <- mkPmId boolTy
        pure [PmLet x rhs, vanillaConGrd x trueDataCon []]

{- Note [Field match order for RecCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The order for RecCon field patterns actually determines evaluation order of
the pattern match. For example:

  data T = T { a :: Char, b :: Int }
  f :: T -> ()
  f T{ b = 42, a = 'a' } = ()

Then @f (T (error "a") (error "b"))@ errors out with "b" because it is mentioned
first in the pattern match.

This means we can't just desugar the pattern match to
@[T a b <- x, 'a' <- a, 42 <- b]@. Instead we have to force them in the
right order: @[T a b <- x, 42 <- b, 'a' <- a]@.

Note [Order of guards matters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Similar to Note [Field match order for RecCon], the order in which the guards
for a pattern match appear matter. Consider a situation similar to T5117:

  f (0:_)  = ()
  f (0:[]) = ()

The latter clause is clearly redundant. Yet if we desugar the second clause as

  [x:xs' <- xs, [] <- xs', 0 <- x]

We will say that the second clause only has an inaccessible RHS. That's because
we force the tail of the list before comparing its head! So the correct
translation would have been

  [x:xs' <- xs, 0 <- x, [] <- xs']

And we have to take in the guards on list cells into @mkListGrds@.

Note [Desugar CoPats]
~~~~~~~~~~~~~~~~~~~~~~~
The pattern match checker did not know how to handle coerced patterns
`CoPat` efficiently, which gave rise to #11276. The original approach
desugared `CoPat`s:

    pat |> co    ===>    x (pat <- (x |> co))

Why did we do this seemingly unnecessary expansion in the first place?
The reason is that the type of @pat |> co@ (which is the type of the value
abstraction we match against) might be different than that of @pat@. Data
instances such as @Sing (a :: Bool)@ are a good example of this: If we would
just drop the coercion, we'd get a type error when matching @pat@ against its
value abstraction, with the result being that pmIsSatisfiable decides that every
possible data constructor fitting @pat@ is rejected as uninhabitated, leading to
a lot of false warnings.

But we can check whether the coercion is a hole or if it is just refl, in
which case we can drop it.

Note [Long-distance information for HsLocalBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#18626)

  f :: Int -> ()
  f x | y = ()
    where
      y = True

  x :: ()
  x | let y = True, y = ()

Both definitions are exhaustive, but to make the necessary long-distance
connection from @y@'s binding to its use site in a guard, we have to collect
'PmLet' guards for the 'HsLocalBinds' which contain @y@'s definitions.

In principle, we are only interested in desugaring local binds that are
'FunBind's, that

  * Have no pattern matches. If @y@ above had any patterns, it would be a
    function and we can't reason about them anyway.
  * Have singleton match group with a single GRHS.
    Otherwise, what expression to pick in the generated guard @let y = <rhs>@?

It turns out that desugaring type-checked local binds in this way is a bit
more complex than expected: Apparently, all bindings are wrapped in 'AbsBinds'
Nfter type-checking. See Note [AbsBinds] in "GHC.Hs.Binds".

We make sure that there is no polymorphism in the way by checking that there
are no 'abs_tvs' or 'abs_ev_vars' (we don't reason about
@y :: forall a. Eq a => ...@) and that the exports carry no 'HsWrapper's. In
this case, the exports are a simple renaming substitution that we can capture
with 'PmLet'. Ultimately we'll hit those renamed 'FunBind's, though, which is
the whole point.

The place to store the 'PmLet' guards for @where@ clauses (which are per
'GRHSs') is as a field of 'PmGRHSs'. For plain @let@ guards as in the guards of
@x@, we can simply add them to the 'pg_grds' field of 'PmGRHS'.
-}
