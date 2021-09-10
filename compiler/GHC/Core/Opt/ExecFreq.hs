-- | Analyses concerned with how often some part of an expression is executed.
--
-- Currently, we only estimate relative frequency of case alternatives, but we
-- may estimate static profiles for whole functions in the future (taking into
-- account loops, etc.). The basic ideas are borrowed from the imperative world:
--
--   [1] Branch prediction for free. Ball and Larus, 1993.
--       https://dl.acm.org/doi/abs/10.1145/173262.155119
--
--   [2] Static branch frequency and program profile analysis. Wu and Larus, 1994.
--       https://dl.acm.org/doi/10.1145/192724.192725
--
-- See Note [Estimating CoreAlt frequencies] for implementation details.
--
module GHC.Core.Opt.ExecFreq
  ( estimateAltFreqs
  )
where

import GHC.Prelude
import GHC.Core
import GHC.Core.Utils
import GHC.Core.Ppr
import GHC.Types.Basic
import GHC.Types.Literal
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Builtin.PrimOps
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Utils.Outputable
import GHC.Utils.Trace
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn, groupBy, nub)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

{- Note [Estimating CoreAlt frequencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very useful for a compiler to know when one case alternative is taken
much more often than another. For example, consider

  sum n = case n of
    0 -> 0 :: Int
    _ -> n + sum (n-1)

Here, for the vast majority of inputs, control flow enters the second
alternative, because

  1. Most Ints aren't equal to 0
  2. The second alternative contains a recursive call, indicating a loop of some
     sort. Most loops loop more often than once, hence it's a plausible estimate
     that we will not yet exit it.

Equipped with that knowledge, the compiler can optimise 'sum' favoring
transformations that improve the second branch, even if that means the first
branch gets a bit slower. Some examples:

  * The Simplifier could decide not to inline in the cold branch, having more
    code size to spend inlining in the second branch.
  * The first branch may need some value boxed, but the second one doesn't. It
    may be more efficient to unbox the value and re-allocate the box in the
    first branch.
  * Similarly, we could give 'sum' the CPR property, thereby unboxing its result
    and destroying the sharing of the (then floated out) 0 CAF.
  * We may float out a binding from the second alternative, even if that means
    more allocation if we take the first alternative
  * (Feel free to add more...)

How do we know which alternatives are hotter/colder than others?

This problem has long been solved in the imperative world, where there are
always just 2 alternatives in each branch decision. We'll build on

  [1] Branch prediction for free. Ball and Larus, 1993.
      https://dl.acm.org/doi/abs/10.1145/173262.155119
  [2] Static branch frequency and program profile analysis. Wu and Larus, 1994.
      https://dl.acm.org/doi/10.1145/192724.192725
  [3] Corpus-based static branch prediction. Calder et al., 1995.
      https://dl.acm.org/doi/10.1145/223428.207118

[1] comes up with a set of useful branch heuristics and measures their
effectiveness. [2] builds on the branch heuristics of [1] and contributes how to
fuse evidence from different heuristics (implemented as 'fuseHeuristic').

We implement a couple of different Heuristics, inspired by [1]:

  1. Opcode Heuristic (OH) captures intuition like "most Ints aren't equal to a
     single constant", but goes a bit beyond that. See 'opcodeHeuristic'.
  2. Loop Branch Heuristic (LBH, see 'loopBranchHeuristic') captures the
     intuition that recursive alts are taken more often than non-recursive ones.
  3. Call Heuristic (CH) says that alts with external calls are rather cold.
     See 'callHeuristic'.
  4. Store Heuristic (SH) says that alts with stores are cold. We extend this to
     general side-effects. See 'storeHeuristic'.
  5. Return Heuristic (RH) says that alts that quickly return are cold. We
     interpret this to mean that paths are cold when they don't jump to a
     (shared) join point. See 'returnHeuristic'.

It's a bit questionable if and how (3) to (5) apply to functional programs.
They might give false indications if regarded in isolation, but in concert with
other heuristics, they might be worth it. The occurrence info that is necessary
to support (3) to (5) is a bit complicated to characterise, so they are
deactivated for now.

Current weights of how much we can trust each individual heuristic are just
rough guesses based on the numbers reported in [1] and [2]. We should eventually
derive these weights by profiling information, training our machine learning
algorithm, so to speak. That is exactly what [3] does.
-}

{-
************************************************************************
*                                                                      *
           Env+Usage: Gathering occurrence info
*                                                                      *
************************************************************************
-}

data Env = E
  { e_level :: !Int
  , e_rec_bndrs :: !(IdEnv Int)
  }

emptyEnv :: Env
emptyEnv = E { e_level = 0, e_rec_bndrs = emptyVarEnv }

delBndrEnv :: Env -> Id -> Env
delBndrEnv env bndr = delBndrsEnv env [bndr]

delBndrsEnv :: Env -> [Id] -> Env
delBndrsEnv env bndrs
  = env { e_rec_bndrs = delVarEnvList (e_rec_bndrs env) bndrs }

enterRec :: Env -> [Id] -> Env
enterRec env bndrs
  = env { e_level = e_level env + 1
        , e_rec_bndrs = extendVarEnvList (e_rec_bndrs env) pairs }
  where
    pairs = zip bndrs (repeat (e_level env))

data Usage = U
  { u_uses        :: !IdSet
  , u_lvls        :: !(IntMap IdSet)
  , u_exit_path   :: !Bool
  , u_ext_calls   :: !Bool
  , u_side_effect :: !Bool
  }

instance Outputable Usage where
  ppr (U uses lvls exit_path ext_calls side_effect) = char 'U' <> braces (fcat
    [        text "uses=", ppr uses
    , comma, text "lvls=", ppr lvls
    , if exit_path then comma <> text "exit_path" else empty
    , if ext_calls then comma <> text "ext_calls" else empty
    , if side_effect then comma <> text "side_effect" else empty
    ])

emptyUsage :: Usage
emptyUsage = U
  { u_uses = emptyVarSet
  , u_lvls = IntMap.empty
  , u_exit_path = False
  , u_ext_calls = False
  , u_side_effect = False }

singleUsage :: Env -> Id -> Usage
singleUsage env b
  | isGlobalId b, idArity b > 0, not $ isPrimOpId b, not $ isDataConWorkId b, not $ isDataConWrapId b
  = emptyUsage { u_ext_calls = True }
  | Just op <- isPrimOpId_maybe b, not $ primOpOkForSideEffects op
  = emptyUsage { u_side_effect = True }
  | isLocalId b
  = emptyUsage { u_lvls = lvls, u_uses = unitVarSet b, u_exit_path = not (isJoinId b) && idArity b == 0 }
  | otherwise   = emptyUsage
  where
    lvls = case lookupVarEnv (e_rec_bndrs env) b of
      Just lvl -> IntMap.singleton lvl (unitVarSet b)
      Nothing  -> IntMap.empty

leaveScope :: Usage -> [Id] -> Usage
leaveScope u bndrs
  = u { u_uses = delVarSetList (u_uses u) bndrs }

leaveRec :: Int -> Usage -> [Id] -> Usage
leaveRec lvl u bndrs
  = u { u_uses = delVarSetList (u_uses u) bndrs
      , u_lvls = IntMap.alter f lvl (u_lvls u) }
  where
    f Nothing = Nothing
    f (Just set)
      | let set' = delVarSetList set bndrs
      , not $ isEmptyVarSet set'
      = Just set'
      | otherwise
      = Nothing

lubUsages :: [Usage] -> Usage
lubUsages = foldl' f emptyUsage{u_exit_path = True}
  where
    f u1 u2 = U { u_uses = unionVarSet (u_uses u1) (u_uses u2)
                , u_lvls = IntMap.unionWith unionVarSet (u_lvls u1) (u_lvls u2)
                , u_exit_path = u_exit_path u1 && u_exit_path u2
                , u_ext_calls = u_ext_calls u1 || u_ext_calls u2
                , u_side_effect = u_side_effect u1 || u_side_effect u2 }

thenUsage :: Usage -> Usage -> Usage
thenUsage u1 u2 = U { u_uses = unionVarSet (u_uses u1) (u_uses u2)
                    , u_lvls = IntMap.unionWith unionVarSet (u_lvls u1) (u_lvls u2)
                    , u_exit_path = u_exit_path u2 -- IMPORTANT difference to lub
                    , u_ext_calls = u_ext_calls u1 || u_ext_calls u2
                    , u_side_effect = u_side_effect u1 || u_side_effect u2 }

{-
************************************************************************
*                                                                      *
           Main analysis traversal
*                                                                      *
************************************************************************
-}

-- | See Note [Estimating CoreAlt frequencies].
estimateAltFreqs :: CoreProgram -> CoreProgram
estimateAltFreqs = go emptyEnv
  where
    go _   []     = []
    go env (b:bs) = b' : go env' bs
      where
        (env', _, b') = analBind env b

analBind :: Env -> CoreBind -> (Env, Usage, CoreBind)
analBind env (NonRec b rhs) = (delBndrEnv env b, usg, NonRec b rhs')
  where
    (usg, rhs') = analExpr env rhs
analBind env (Rec pairs) = (env', usg', Rec pairs')
  where
    (bs, rhss)    = unzip pairs
    env'          = enterRec env bs
    (usgs, rhss') = mapAndUnzip (analExpr env') rhss
    pairs'        = zip bs rhss'
    usg'          = lubUsages usgs

analExpr :: Env -> CoreExpr -> (Usage, CoreExpr)
analExpr env e = case e of
  Coercion{} -> (emptyUsage, e)
  Type{}     -> (emptyUsage, e)
  Lit{}      -> (emptyUsage, e)
  (Var v)    -> (singleUsage env v, e)
  Cast e co | (usg, e') <- analExpr env e -> (usg, Cast e' co)
  Tick t e  | (usg, e') <- analExpr env e -> (usg, Tick t e')
  Lam b e   | (usg, e') <- analExpr (delBndrEnv env b) e -> (leaveScope usg [b], Lam b e')
  App f a
    | (usg_f, f') <- analExpr env f
    , (usg_a, a') <- analExpr env a
    -> (usg_a `thenUsage` usg_f, App f' a')
  Let bind e
    | (env', usg_bs, bind') <- analBind env bind
    , (usg_e, e') <- analExpr env' e
    , let leave = case bind of Rec{} -> leaveRec (e_level env); _ -> leaveScope
    -> (leave (usg_bs `thenUsage` usg_e) (bindersOf bind), Let bind' e')
  Case scrut b ty alts
    | (usg_scrut, scrut') <- analExpr env scrut
    , let usg_w_alts = map (analAlt (delBndrEnv env b)) alts
    , (usg_alts, alts') <- applyHeuristic heuristic scrut usg_w_alts
    -> (leaveScope (usg_scrut `thenUsage` lubUsages usg_alts) [b], Case scrut' b ty alts')
  where
    -- heuristic = traceHeuristic "combined" $ fuseHeuristics $ NonEmpty.fromList
    heuristic = fuseHeuristics $ NonEmpty.fromList
      [ loopBranchHeuristic
      , opcodeHeuristic
      , ignoreHeuristic $ traceHeuristic "CH" $ callHeuristic
      , ignoreHeuristic $ traceHeuristic "SH" $ storeHeuristic
      , ignoreHeuristic $ traceHeuristic "RH" $ returnHeuristic
      ]

analAlt :: Env -> CoreAlt -> (Usage, CoreAlt)
analAlt env (Alt con freq bs rhs) = (leaveScope usg bs, Alt con freq bs rhs')
  where
    (usg, rhs') = analExpr (delBndrsEnv env bs) rhs

{-
************************************************************************
*                                                                      *
           Branch heuristics
*                                                                      *
************************************************************************
-}

-- | A Branch/Alt prediction heuristic. See Note [Estimating CoreAlt frequencies].
type Heuristic = CoreExpr -> [(AltCon, [Var], Usage)] -> Maybe [Freq]

-- | Associative. So 'Heuristic' is a Semigroup via this operation.
-- This operation is Dempster's rule of combination for the very narrow use case
-- of ours, where all masses of non-singleton sets are 0.
--
-- See Note [Estimating CoreAlt frequencies].
fuseHeuristic :: Heuristic -> Heuristic -> Heuristic
fuseHeuristic a b scrut alts = a scrut alts <+> b scrut alts
  where
    f1      <+> Nothing = f1
    Nothing <+> f2      = f2
    Just f1 <+> Just f2 = Just $! normaliseFreqs joint -- Dampster's rule of combination
      where
        joint = zipWithEqual "fuseHeuristic" (*) f1 f2

fuseHeuristics :: NonEmpty Heuristic -> Heuristic
fuseHeuristics = foldr1 fuseHeuristic

traceHeuristic :: String -> Heuristic -> Heuristic
traceHeuristic descr heur scrut alts = pprTrace descr (pprCoreExpr scrut $$ ppr alts $$ ppr freqs) freqs
  where
    freqs = heur scrut alts
_ = traceHeuristic -- suppress unused warning

ignoreHeuristic :: Heuristic -> Heuristic
ignoreHeuristic _heur _scrut _alts = Nothing

applyHeuristic :: Heuristic -> CoreExpr -> [(Usage, CoreAlt)] -> ([Usage], [CoreAlt])
applyHeuristic heur scrut usg_w_alts = (usgs, alts')
  where
    heur_alts = [ (con, bs, usg) | (usg, Alt con _ bs _) <- usg_w_alts ]
    freqs = heur scrut heur_alts `orElse` uniformFreqs (length alts)
    (usgs, alts) = unzip usg_w_alts
    alts' = zipWith (\(Alt con _ bs rhs) freq -> Alt con freq bs rhs) alts freqs

-- | Returns 'True' if the given predicate is neither all 'True' or all 'False'
-- on the elements of the list.
discriminates :: (a -> Bool) -> [a] -> Bool
discriminates p xs
  | [_all_true_or_false] <- nub (map p xs) = False -- NB: we shortcircuit on the 2nd distinct element!
  | otherwise                              = True

-- | This is the Loop Branch Heuristic from [1] and [2].
--
-- "Predict as taken an edge back to a loop’s head. Predict as not taken an
-- edge exiting a loop."
--
-- Our back edges are calls to recursive functions, the nesting levels of which
-- we track in 'u_lvls'.
loopBranchHeuristic :: Heuristic
loopBranchHeuristic _scrut alts
  | applies   = Just $! freqs
  | otherwise = Nothing
  where
    max_back_lvl usg = case IntMap.lookupMax (u_lvls usg) of
      Nothing       -> -1 -- lower than all other levels
      Just (lvl, _) -> lvl
    max_back_lvls = map (\(con, _, usg) -> (max_back_lvl usg, con)) alts
    has_back_edge = (>= 0) . fst
    applies = discriminates has_back_edge max_back_lvls
    sorted_back_lvls = sortOn fst max_back_lvls
    lvl_batches = groupBy (\a b -> fst a == fst b) sorted_back_lvls
    lvl_factor = 4 -- Factor by which we favor higher-level back edges
    batches_w_counts = zip lvl_batches [ lvl_factor^i | i <- [0::Int, 1 ..] ]
      -- Example:
      --   * alt  A   has back_lvl -1 (e.g., no back edge)
      --   * alts B,D have back_lvl 2 (e.g., continue outermost loops)
      --   * alt  C   has back_lvl 14 (e.g., continues the innermost loop)
      -- Then we'd get lvl_batches of [(-1, [A]), (2, [B,D]), (14, [C])]
      -- and we'd get assoc counts of [     1   ,     4,          16   ],
      -- so increasing with lvl_factor 4.
      -- And then we simply re-align with the original alts and normalise to get
      --   alts  [(A,_,_), (B,_,_), (C,_,_), (D,_,_) ]
      --   freqs [  0.4  ,   0.16 ,   0.64 ,   0.16  ]
    total_count = sum $ map (\(grp, c) -> c * length grp) batches_w_counts
    cons_freqs = [ (con, Freq (fromIntegral c / fromIntegral total_count))
                 | (grp, c) <- batches_w_counts
                 , (_, con) <- grp ]
    freqs = [ expectJust "con not present" $ lookup con cons_freqs | (con, _, _) <- alts ]

-- | @usgHeuristic p yes no@ is a 'Heuristic' that applies whenever `p` is a
-- discriminating predicate on the case alternatives, weighing alternatives
-- that satisfy `p` with integer weights `yes` and those that don't with `no`.
usgHeuristic :: (Usage -> Bool) -> Int -> Int -> Heuristic
usgHeuristic p yes no _scrut alts
  | applies   = Just $! freqs
  | otherwise = Nothing
  where
    applies = discriminates (\(_, _, usg) -> p usg) alts
    freqs = absToRelFreqs [ if p usg then yes else no | (_, _, usg) <- alts ]

-- | This is the Return Heuristic from [1] and [2].
--
-- "Predict a successor that contains a store instruction and does not
-- post-dominate will not be taken."
--
-- We interpret "store" as primops with side-effects here. Also we don't only
-- look into direct successors, but the whole alternative
-- (for practical reasons). It doesn't work well, as exit paths tend to have no
-- side-effects and thus will win from this heuristic.
returnHeuristic :: Heuristic
returnHeuristic = usgHeuristic u_exit_path 1 4

-- | This is the Call Heuristic from [1] and [2].
--
-- "Predict a successor that contains a store instruction and does not
-- post-dominate will not be taken."
--
-- We interpret "store" as primops with side-effects here. Also we don't only
-- look into direct successors, but the whole alternative
-- (for practical reasons). It doesn't work well, as exit paths tend to have no
-- side-effects and thus will win from this heuristic.
callHeuristic :: Heuristic
callHeuristic = usgHeuristic u_ext_calls 1 4

-- | This is the Store Heuristic from [1] and [2].
--
-- "Predict a successor that contains a store instruction and does not
-- post-dominate will not be taken."
--
-- We interpret "store" as primops with side-effects here. Also we don't only
-- look into direct successors, but the whole alternative
-- (for practical reasons). It doesn't work well, as exit paths tend to have no
-- side-effects and thus will win from this heuristic.
storeHeuristic :: Heuristic
storeHeuristic = usgHeuristic u_side_effect 1 4

predictSpecificAlt :: (AltCon -> Bool) -> [(AltCon, [Var], Usage)] -> [Freq]
predictSpecificAlt p = absToRelFreqs . snd . foldr go (False, [])
  where
    high_freq = 5
    low_freq = 1
    go (con, _, _) (found_it, abs_freqs) = case con of
      DEFAULT | not found_it -> (True,    high_freq:abs_freqs)
                  -- DEFAULT always comes first in alts if it exists
                  -- Thus, it will be the last thing `go` encounters.
                  -- If we haven't found a matching AltCon so far, we pick DEFAULT
      _       | p con        -> (True,    high_freq:abs_freqs)
              | otherwise    -> (found_it, low_freq:abs_freqs)

predictZero, predictOne, predictDefault :: [(AltCon, [Var], Usage)] -> [Freq]

predictZero = predictSpecificAlt p
  where
    p (LitAlt l) = isZeroLit l
    p _          = False

predictOne = predictSpecificAlt p
  where
    p (LitAlt l) = isOneLit l
    p _          = False

predictDefault = predictSpecificAlt p
  where
    p DEFAULT = True
    p _       = False

-- | This is the Opcode Heuristic from [1] and [2].
--
-- "Predict that a comparison of an integer [for us: general literal] for less
-- than zero, less than or equal to zero, or equal to a constant, will fail."
--
-- We extend the heuristic slightly to deal with multiple literal alts, in which
-- we predict the DEFAULT alt. Predicting the `< 0` and `<= 0` cases as False
-- is a choice we might want to revisit; the original paper did it based on the
-- use of negative error codes that are prevalent in C.
opcodeHeuristic :: Heuristic
opcodeHeuristic scrut alts
  | not $ any lit_alt alts = Nothing
  | [_, _] <- alts   = case putLitRight <$> isComparisonApp_maybe scrut of
      -- putLitRight arranges it such that the Literal is the right operand
      Just (cmp, _, Lit r_lit)
        | isZeroLit r_lit -> Just $! case cmp of
              Lt -> predictZero alts
              Le -> predictZero alts
              Eq -> predictZero alts
              Gt -> predictOne alts
              Ge -> predictOne alts
              Ne -> predictOne alts
        | Eq <- cmp -> Just $! predictZero alts
        | Ne <- cmp -> Just $! predictOne alts
      _ -> Just $! predictDefault alts
   | otherwise             = Just $! predictDefault alts
  where
    lit_alt (LitAlt{}, _, _) = True
    lit_alt _                = False

putLitRight :: (Comparison, CoreExpr, CoreExpr) -> (Comparison, CoreExpr, CoreExpr)
putLitRight (cmp, l@Lit{}, r) = (flipComparison cmp, r, l)
putLitRight orig              = orig
