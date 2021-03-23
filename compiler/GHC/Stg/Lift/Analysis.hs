{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Provides the heuristics for when it's beneficial to lambda lift bindings.
-- Most significantly, this employs a cost model to estimate impact on heap
-- allocations, by looking at an STG expression's 'Skeleton'.
module GHC.Stg.Lift.Analysis (
    -- * #when# When to lift
    -- $when

    -- * #clogro# Estimating closure growth
    -- $clogro

    -- * AST annotation
    Skeleton(..), BinderInfo(..), binderInfoBndr,
    LlStgBinding, LlStgExpr, LlStgRhs, LlStgAlt, tagSkeletonTopBind,
    -- * Lifting decision
    goodToLift,
    closureGrowth -- Exported just for the docs
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile

import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Driver.Session
import GHC.Types.Id
import GHC.Runtime.Heap.Layout ( WordOff )
import GHC.Stg.Syntax
import qualified GHC.StgToCmm.ArgRep  as StgToCmm.ArgRep
import qualified GHC.StgToCmm.Closure as StgToCmm.Closure
import qualified GHC.StgToCmm.Layout  as StgToCmm.Layout
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Types.Var.Set
import GHC.Data.STuple

import Data.Maybe ( mapMaybe )

-- Note [When to lift]
-- ~~~~~~~~~~~~~~~~~~~
-- $when
-- The analysis proceeds in two steps:
--
--   1. It tags the syntax tree with analysis information in the form of
--      'BinderInfo' at each binder and 'Skeleton's at each let-binding
--      by 'tagSkeletonTopBind' and friends.
--   2. The resulting syntax tree is treated by the "GHC.Stg.Lift"
--      module, calling out to 'goodToLift' to decide if a binding is worthwhile
--      to lift.
--      'goodToLift' consults argument occurrence information in 'BinderInfo'
--      and estimates 'closureGrowth', for which it needs the 'Skeleton'.
--
-- So the annotations from 'tagSkeletonTopBind' ultimately fuel 'goodToLift',
-- which employs a number of heuristics to identify and exclude lambda lifting
-- opportunities deemed non-beneficial:
--
--  [Top-level bindings] can't be lifted.
--  [Thunks] and data constructors shouldn't be lifted in order not to destroy
--    sharing.
--  [Argument occurrences] #arg_occs# of binders prohibit them to be lifted.
--    Doing the lift would re-introduce the very allocation at call sites that
--    we tried to get rid off in the first place. We capture analysis
--    information in 'BinderInfo'. Note that we also consider a nullary
--    application as argument occurrence, because it would turn into an n-ary
--    partial application created by a generic apply function. This occurs in
--    CPS-heavy code like the CS benchmark.
--  [Join points] should not be lifted, simply because there's no reduction in
--    allocation to be had.
--  [Abstracting over join points] destroys join points, because they end up as
--    arguments to the lifted function.
--  [Abstracting over known local functions] turns a known call into an unknown
--    call (e.g. some @stg_ap_*@), which is generally slower. Can be turned off
--    with @-fstg-lift-lams-known@.
--  [Calling convention] Don't lift when the resulting function would have a
--    higher arity than available argument registers for the calling convention.
--    Can be influenced with @-fstg-lift-(non)rec-args(-any)@.
--  [Closure growth] introduced when former free variables have to be available
--    at call sites may actually lead to an increase in overall allocations
--  resulting from a lift. Estimating closure growth is described in
--  "GHC.Stg.Lift.Analysis#clogro" and is what most of this module is ultimately
--  concerned with.
--
-- There's a <https://gitlab.haskell.org/ghc/ghc/wikis/late-lam-lift wiki page> with
-- some more background and history.

-- Note [Estimating closure growth]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- $clogro
-- We estimate closure growth by abstracting the syntax tree into a 'Skeleton',
-- capturing only syntactic details relevant to 'closureGrowth', such as
--
--   * 'ClosureSk', representing closure allocation.
--   * 'RhsSk', representing a RHS of a binding and how many times it's called
--     by an appropriate 'Card'.
--   * 'AltSk', 'BothSk' and 'NilSk' for choice, sequence and empty element.
--
-- This abstraction is mostly so that the main analysis function 'closureGrowth'
-- can stay simple and focused. Also, skeletons tend to be much smaller than
-- the syntax tree they abstract, so it makes sense to construct them once and
-- and operate on them instead of the actual syntax tree.
--
-- A more detailed treatment of computing closure growth, including examples,
-- can be found in the paper referenced from the
-- <https://gitlab.haskell.org/ghc/ghc/wikis/late-lam-lift wiki page>.

llTrace :: String -> SDoc -> a -> a
llTrace _ _ c = c
-- llTrace a b c = pprTrace a b c

type instance BinderP      'LiftLams = BinderInfo
type instance XRhsClosure  'LiftLams = DIdSet
type instance XLet         'LiftLams = Skeleton
type instance XLetNoEscape 'LiftLams = Skeleton


-- | Captures details of the syntax tree relevant to the cost model, such as
-- closures, multi-shot lambdas and case expressions.
data Skeleton
  = ClosureSk !Id !DIdSet {- ^ free vars -} !Skeleton
  | RhsSk !Card {- ^ how often the RHS was entered -} !Skeleton
  | AltSk !Skeleton !Skeleton
  | BothSk !Skeleton !Skeleton
  | NilSk

bothSk :: Skeleton -> Skeleton -> Skeleton
bothSk NilSk b = b
bothSk a NilSk = a
bothSk a b     = BothSk a b

altSk :: Skeleton -> Skeleton -> Skeleton
altSk NilSk b = b
altSk a NilSk = a
altSk a b     = AltSk a b

rhsSk :: Card -> Skeleton -> Skeleton
rhsSk _        NilSk = NilSk
rhsSk body_dmd skel  = RhsSk body_dmd skel

-- | The type used in binder positions in 'GenStgExpr's.
data BinderInfo
  = BindsClosure !Id !Bool -- ^ Let(-no-escape)-bound thing with a flag
                           --   indicating whether it occurs as an argument
                           --   or in a nullary application
                           --   (see "GHC.Stg.Lift.Analysis#arg_occs").
  | BoringBinder !Id       -- ^ Every other kind of binder

-- | Gets the bound 'Id' out a 'BinderInfo'.
binderInfoBndr :: BinderInfo -> Id
binderInfoBndr (BoringBinder bndr)   = bndr
binderInfoBndr (BindsClosure bndr _) = bndr

-- | Returns 'Nothing' for 'BoringBinder's and 'Just' the flag indicating
-- occurrences as argument or in a nullary applications otherwise.
binderInfoOccursAsArg :: BinderInfo -> Maybe Bool
binderInfoOccursAsArg BoringBinder{}     = Nothing
binderInfoOccursAsArg (BindsClosure _ b) = Just b

instance Outputable Skeleton where
  ppr NilSk = text ""
  ppr (AltSk l r) = vcat
    [ text "{ " <+> ppr l
    , text "ALT"
    , text "  " <+> ppr r
    , text "}"
    ]
  ppr (BothSk l r) = ppr l $$ ppr r
  ppr (ClosureSk f fvs body) = ppr f <+> ppr fvs $$ nest 2 (ppr body)
  ppr (RhsSk card body) = hcat
    [ lambda
    , ppr card
    , dot
    , ppr body
    ]

instance Outputable BinderInfo where
  ppr = ppr . binderInfoBndr

instance OutputableBndr BinderInfo where
  pprBndr b = pprBndr b . binderInfoBndr
  pprPrefixOcc = pprPrefixOcc . binderInfoBndr
  pprInfixOcc = pprInfixOcc . binderInfoBndr
  bndrIsJoin_maybe = bndrIsJoin_maybe . binderInfoBndr

mkArgOccs :: [StgArg] -> IdSet
mkArgOccs = mkVarSet . mapMaybe stg_arg_var
  where
    stg_arg_var (StgVarArg occ) = Just occ
    stg_arg_var _               = Nothing

-- | Tags every binder with its 'BinderInfo' and let bindings with their
-- 'Skeleton's.
tagSkeletonTopBind :: CgStgBinding -> LlStgBinding
-- NilSk is OK when tagging top-level bindings. Also, top-level things are never
-- lambda-lifted, so no need to track their argument occurrences. They can also
-- never be let-no-escapes (thus we pass False).
tagSkeletonTopBind bind = bind'
  where
    (_, _, _, bind') = tagSkeletonBinding False NilSk emptyVarSet bind

-- | Tags binders of an 'StgExpr' with its 'BinderInfo' and let bindings with
-- their 'Skeleton's. Additionally, returns its 'Skeleton' and the set of binder
-- occurrences in argument and nullary application position
-- (cf. "GHC.Stg.Lift.Analysis#arg_occs").
tagSkeletonExpr :: CgStgExpr -> (Skeleton, IdSet, LlStgExpr)
tagSkeletonExpr (StgLit lit)
  = (NilSk, emptyVarSet, StgLit lit)
tagSkeletonExpr (StgConApp con mn args tys)
  = (NilSk, mkArgOccs args, StgConApp con mn args tys)
tagSkeletonExpr (StgOpApp op args ty)
  = (NilSk, mkArgOccs args, StgOpApp op args ty)
tagSkeletonExpr (StgApp f args)
  = (NilSk, arg_occs, StgApp f args)
  where
    arg_occs
      -- This checks for nullary applications, which we treat the same as
      -- argument occurrences, see "GHC.Stg.Lift.Analysis#arg_occs".
      | null args = unitVarSet f
      | otherwise = mkArgOccs args
tagSkeletonExpr (StgCase scrut bndr ty alts)
  = (skel, arg_occs, StgCase scrut' bndr' ty alts')
  where
    (scrut_skel, scrut_arg_occs, scrut') = tagSkeletonExpr scrut
    (alt_skels, alt_arg_occss, alts') = mapAndUnzip3 tagSkeletonAlt alts
    skel = bothSk scrut_skel (foldr altSk NilSk alt_skels)
    arg_occs = unionVarSets (scrut_arg_occs:alt_arg_occss) `delVarSet` bndr
    bndr' = BoringBinder bndr
tagSkeletonExpr (StgTick t e)
  = (skel, arg_occs, StgTick t e')
  where
    (skel, arg_occs, e') = tagSkeletonExpr e
tagSkeletonExpr (StgLet _ bind body) = tagSkeletonLet False body bind
tagSkeletonExpr (StgLetNoEscape _ bind body) = tagSkeletonLet True body bind

mkLet :: Bool -> Skeleton -> LlStgBinding -> LlStgExpr -> LlStgExpr
mkLet True = StgLetNoEscape
mkLet _    = StgLet

tagSkeletonLet
  :: Bool
  -- ^ Is the binding a let-no-escape?
  -> CgStgExpr
  -- ^ Let body
  -> CgStgBinding
  -- ^ Binding group
  -> (Skeleton, IdSet, LlStgExpr)
  -- ^ RHS skeletons, argument occurrences and annotated binding
tagSkeletonLet is_lne body bind
  = (let_skel, arg_occs, mkLet is_lne scope bind' body')
  where
    (body_skel, body_arg_occs, body') = tagSkeletonExpr body
    (let_skel, arg_occs, scope, bind')
      = tagSkeletonBinding is_lne body_skel body_arg_occs bind

tagSkeletonBinding
  :: Bool
  -- ^ Is the binding a let-no-escape?
  -> Skeleton
  -- ^ Let body skeleton
  -> IdSet
  -- ^ Argument occurrences in the body
  -> CgStgBinding
  -- ^ Binding group
  -> (Skeleton, IdSet, Skeleton, LlStgBinding)
  -- ^ Let skeleton, argument occurrences, scope skeleton of binding and
  --   the annotated binding
tagSkeletonBinding is_lne body_skel body_arg_occs (StgNonRec bndr rhs)
  = (let_skel, arg_occs, scope, bind')
  where
    (rhs_skel, rhs_arg_occs, rhs') = tagSkeletonRhs bndr rhs
    arg_occs = (body_arg_occs `unionVarSet` rhs_arg_occs) `delVarSet` bndr
    bind_skel
      | is_lne    = rhs_skel -- no closure is allocated for let-no-escapes
      | otherwise = ClosureSk bndr (freeVarsOfRhs rhs) rhs_skel
    let_skel = bothSk body_skel bind_skel
    occurs_as_arg = bndr `elemVarSet` body_arg_occs
    -- Compared to the recursive case, this exploits the fact that @bndr@ is
    -- never free in @rhs@.
    scope = body_skel
    bind' = StgNonRec (BindsClosure bndr occurs_as_arg) rhs'
tagSkeletonBinding is_lne body_skel body_arg_occs (StgRec pairs)
  = (let_skel, arg_occs, scope, StgRec pairs')
  where
    (bndrs, _) = unzip pairs
    -- Local recursive STG bindings also regard the defined binders as free
    -- vars. We want to delete those for our cost model, as these are known
    -- calls anyway when we add them to the same top-level recursive group as
    -- the top-level binding currently being analysed.
    skel_occs_rhss' = map (uncurry tagSkeletonRhs) pairs
    rhss_arg_occs = map sndOf3 skel_occs_rhss'
    scope_occs = unionVarSets (body_arg_occs:rhss_arg_occs)
    arg_occs = scope_occs `delVarSetList` bndrs
    -- @skel_rhss@ aren't yet wrapped in closures. We'll do that in a moment,
    -- but we also need the un-wrapped skeletons for calculating the @scope@
    -- of the group, as the outer closures don't contribute to closure growth
    -- when we lift this specific binding.
    scope = foldr (bothSk . fstOf3) body_skel skel_occs_rhss'
    -- Now we can build the actual Skeleton for the expression just by
    -- iterating over each bind pair.
    (bind_skels, pairs') = unzip (zipWith single_bind bndrs skel_occs_rhss')
    let_skel = foldr bothSk body_skel bind_skels
    single_bind bndr (skel_rhs, _, rhs') = (bind_skel, (bndr', rhs'))
      where
        -- Here, we finally add the closure around each @skel_rhs@.
        bind_skel
          | is_lne    = skel_rhs -- no closure is allocated for let-no-escapes
          | otherwise = ClosureSk bndr fvs skel_rhs
        fvs = freeVarsOfRhs rhs' `dVarSetMinusVarSet` mkVarSet bndrs
        bndr' = BindsClosure bndr (bndr `elemVarSet` scope_occs)

tagSkeletonRhs :: Id -> CgStgRhs -> (Skeleton, IdSet, LlStgRhs)
tagSkeletonRhs _ (StgRhsCon ccs dc mn ts args)
  = (NilSk, mkArgOccs args, StgRhsCon ccs dc mn ts args)
tagSkeletonRhs bndr (StgRhsClosure fvs ccs upd bndrs body)
  = (rhs_skel, body_arg_occs, StgRhsClosure fvs ccs upd bndrs' body')
  where
    bndrs' = map BoringBinder bndrs
    (body_skel, body_arg_occs, body') = tagSkeletonExpr body
    rhs_skel = rhsSk (rhsCard bndr) body_skel

-- | How many times will the lambda body of the RHS bound to the given
-- identifier be evaluated, relative to its defining context? This function
-- computes the answer in form of a 'Card'.
rhsCard :: Id -> Card
rhsCard bndr
  | is_thunk  = oneifyCard n
  | otherwise = sFst $ peelManyCalls (idArity bndr) cd
  where
    is_thunk = idArity bndr == 0
    -- Let's pray idDemandInfo is still OK after unarise...
    n :* cd = idDemandInfo bndr

tagSkeletonAlt :: CgStgAlt -> (Skeleton, IdSet, LlStgAlt)
tagSkeletonAlt (con, bndrs, rhs)
  = (alt_skel, arg_occs, (con, map BoringBinder bndrs, rhs'))
  where
    (alt_skel, alt_arg_occs, rhs') = tagSkeletonExpr rhs
    arg_occs = alt_arg_occs `delVarSetList` bndrs

-- | Combines several heuristics to decide whether to lambda-lift a given
-- @let@-binding to top-level. See "GHC.Stg.Lift.Analysis#when" for details.
goodToLift
  :: DynFlags
  -> TopLevelFlag
  -> RecFlag
  -> (DIdSet -> DIdSet) -- ^ An expander function, turning 'InId's into
                        -- 'OutId's. See 'GHC.Stg.Lift.Monad.liftedIdsExpander'.
  -> [(BinderInfo, LlStgRhs)]
  -> Skeleton
  -> Maybe DIdSet       -- ^ @Just abs_ids@ <=> This binding is beneficial to
                        -- lift and @abs_ids@ are the variables it would
                        -- abstract over
goodToLift dflags top_lvl rec_flag expander pairs scope = decide
  [ ("top-level", isTopLevel top_lvl) -- keep in sync with Note [When to lift]
  , ("memoized", any_memoized)
  , ("argument occurrences", arg_occs)
  , ("join point", is_join_point)
  , ("abstracts join points", abstracts_join_ids)
  , ("abstracts known local function", abstracts_known_local_fun)
  , ("args spill on stack", args_spill_on_stack)
  , ("increases allocation", inc_allocs)
  ] where
      profile  = targetProfile dflags
      platform = profilePlatform profile
      decide deciders
        | not (fancy_or deciders)
        = llTrace "stgLiftLams:lifting"
                  (ppr bndrs <+> ppr abs_ids $$
                   ppr allocs $$
                   ppr scope) $
          Just abs_ids
        | otherwise
        = Nothing
      ppr_deciders = vcat . map (text . fst) . filter snd
      fancy_or deciders
        = llTrace "stgLiftLams:goodToLift" (ppr bndrs $$ ppr_deciders deciders) $
          any snd deciders

      bndrs = map (binderInfoBndr . fst) pairs
      bndrs_set = mkVarSet bndrs
      rhss = map snd pairs

      -- First objective: Calculate @abs_ids@, e.g. the former free variables
      -- the lifted binding would abstract over. We have to merge the free
      -- variables of all RHS to get the set of variables that will have to be
      -- passed through parameters.
      fvs = unionDVarSets (map freeVarsOfRhs rhss)
      -- To lift the binding to top-level, we want to delete the lifted binders
      -- themselves from the free var set. Local let bindings track recursive
      -- occurrences in their free variable set. We neither want to apply our
      -- cost model to them (see 'tagSkeletonRhs'), nor pass them as parameters
      -- when lifted, as these are known calls. We call the resulting set the
      -- identifiers we abstract over, thus @abs_ids@. These are all 'OutId's.
      -- We will save the set in 'LiftM.e_expansions' for each of the variables
      -- if we perform the lift.
      abs_ids = expander (delDVarSetList fvs bndrs)

      -- We don't lift updatable thunks or constructors
      any_memoized = any is_memoized_rhs rhss
      is_memoized_rhs StgRhsCon{} = True
      is_memoized_rhs (StgRhsClosure _ _ upd _ _) = isUpdatable upd

      -- Don't lift binders occurring as arguments. This would result in complex
      -- argument expressions which would have to be given a name, reintroducing
      -- the very allocation at each call site that we wanted to get rid off in
      -- the first place.
      arg_occs = or (mapMaybe (binderInfoOccursAsArg . fst) pairs)

      -- These don't allocate anyway.
      is_join_point = any isJoinId bndrs

      -- Abstracting over join points/let-no-escapes spoils them.
      abstracts_join_ids = any isJoinId (dVarSetElems abs_ids)

      -- Abstracting over known local functions that aren't floated themselves
      -- turns a known, fast call into an unknown, slow call:
      --
      --    let f x = ...
      --        g y = ... f x ... -- this was a known call
      --    in g 4
      --
      -- After lifting @g@, but not @f@:
      --
      --    l_g f y = ... f y ... -- this is now an unknown call
      --    let f x = ...
      --    in l_g f 4
      --
      -- We can abuse the results of arity analysis for this:
      -- idArity f > 0 ==> known
      known_fun id = idArity id > 0
      abstracts_known_local_fun
        = not (liftLamsKnown dflags) && any known_fun (dVarSetElems abs_ids)

      -- Number of arguments of a RHS in the current binding group if we decide
      -- to lift it
      n_args
        = length
        . StgToCmm.Closure.nonVoidIds -- void parameters don't appear in Cmm
        . (dVarSetElems abs_ids ++)
        . rhsLambdaBndrs
      max_n_args
        | isRec rec_flag = liftLamsRecArgs dflags
        | otherwise      = liftLamsNonRecArgs dflags
      -- We have 5 hardware registers on x86_64 to pass arguments in. Any excess
      -- args are passed on the stack, which means slow memory accesses
      args_spill_on_stack
        | Just n <- max_n_args = maximum (map n_args rhss) > n
        | otherwise = False

      -- We only perform the lift if allocations didn't increase.
      -- Note that @clo_growth@ will be 'infinity' if there was positive growth
      -- under a multi-shot lambda.
      -- Also, abstracting over LNEs is unacceptable. LNEs might return
      -- unlifted tuples, which idClosureFootprint can't cope with.
      inc_allocs = abstracts_join_ids || allocs > 0
      allocs = clo_growth + mkIntWithInf (negate closuresSize)
      -- We calculate and then add up the size of each binding's closure.
      -- GHC does not currently share closure environments, and we either lift
      -- the entire recursive binding group or none of it.
      closuresSize = sum $ flip map rhss $ \rhs ->
        closureSize profile
        . dVarSetElems
        . expander
        . flip dVarSetMinusVarSet bndrs_set
        $ freeVarsOfRhs rhs
      clo_growth = closureGrowth expander (idClosureFootprint platform) bndrs_set abs_ids scope

rhsLambdaBndrs :: LlStgRhs -> [Id]
rhsLambdaBndrs StgRhsCon{} = []
rhsLambdaBndrs (StgRhsClosure _ _ _ bndrs _) = map binderInfoBndr bndrs

-- | The size in words of a function closure closing over the given 'Id's,
-- including the header.
closureSize :: Profile -> [Id] -> WordOff
closureSize profile ids = words + pc_STD_HDR_SIZE (platformConstants (profilePlatform profile))
  -- We go through sTD_HDR_SIZE rather than fixedHdrSizeW so that we don't
  -- optimise differently when profiling is enabled.
  where
    (words, _, _)
      -- Functions have a StdHeader (as opposed to ThunkHeader).
      = StgToCmm.Layout.mkVirtHeapOffsets profile StgToCmm.Layout.StdHeader
      . StgToCmm.Closure.addIdReps
      . StgToCmm.Closure.nonVoidIds
      $ ids

-- | The number of words a single 'Id' adds to a closure's size.
-- Note that this can't handle unboxed tuples (which may still be present in
-- let-no-escapes, even after Unarise), in which case
-- @'GHC.StgToCmm.Closure.idPrimRep'@ will crash.
idClosureFootprint:: Platform -> Id -> WordOff
idClosureFootprint platform
  = StgToCmm.ArgRep.argRepSizeW platform
  . StgToCmm.ArgRep.idArgRep platform

-- | @closureGrowth expander sizer f fvs@ computes the closure growth in words
-- as a result of lifting @f@ to top-level. If there was any growing closure
-- under a multi-shot lambda, the result will be 'infinity'.
-- Also see "GHC.Stg.Lift.Analysis#clogro".
closureGrowth
  :: (DIdSet -> DIdSet)
  -- ^ Expands outer free ids that were lifted to their free vars
  -> (Id -> Int)
  -- ^ Computes the closure footprint of an identifier
  -> IdSet
  -- ^ Binding group for which lifting is to be decided
  -> DIdSet
  -- ^ Free vars of the whole binding group prior to lifting it. These must be
  --   available at call sites if we decide to lift the binding group.
  -> Skeleton
  -- ^ Abstraction of the scope of the function
  -> IntWithInf
  -- ^ Closure growth. 'infinity' indicates there was growth under a
  --   (multi-shot) lambda.
closureGrowth expander sizer group abs_ids = go
  where
    go NilSk = 0
    go (BothSk a b) = go a + go b
    go (AltSk a b) = max (go a) (go b)
    go (ClosureSk _ clo_fvs rhs)
      -- If no binder of the @group@ occurs free in the closure, the lifting
      -- won't have any effect on it and we can omit the recursive call.
      | n_occs == 0 = 0
      -- Otherwise, we account the cost of allocating the closure and add it to
      -- the closure growth of its RHS.
      | otherwise   = mkIntWithInf cost + go rhs
      where
        n_occs = sizeDVarSet (clo_fvs' `dVarSetIntersectVarSet` group)
        -- What we close over considering prior lifting decisions
        clo_fvs' = expander clo_fvs
        -- Variables that would additionally occur free in the closure body if
        -- we lift @f@
        newbies = abs_ids `minusDVarSet` clo_fvs'
        -- Lifting @f@ removes @f@ from the closure but adds all @newbies@
        cost = nonDetStrictFoldDVarSet (\id size -> sizer id + size) 0 newbies - n_occs
        -- Using a non-deterministic fold is OK here because addition is commutative.
    go (RhsSk n body)
      -- The conservative assumption would be that
      --   1. Every RHS with positive growth would be called multiple times,
      --      modulo thunks.
      --   2. Every RHS with negative growth wouldn't be called at all.
      --
      -- In the first case, we'd have to return 'infinity', while in the
      -- second case, we'd have to return 0. But we can do far better
      -- considering information from the demand analyser, which provides us
      -- with conservative estimates on minimum and maximum evaluation
      -- cardinality. The @body_dmd@ part of 'RhsSk' is the result of
      -- 'rhsCard' and accurately captures the cardinality of the RHSs body
      -- relative to its defining context.
      | isAbs n      = 0
      | cg <= 0      = if isStrict n then cg else 0
      | isUsedOnce n = cg
      | otherwise    = infinity
      where
        cg = go body
