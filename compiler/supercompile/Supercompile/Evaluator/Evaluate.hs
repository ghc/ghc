module Supercompile.Evaluator.Evaluate (normalise, step, gc, shouldExposeUnfolding) where

#include "HsVersions.h"

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.GHC (termToCoreExpr)
import Supercompile.StaticFlags
import Supercompile.Utilities

import qualified Data.Map as M

import qualified CoreSyn as CoreSyn
import CoreUnfold
import DynFlags (DynFlag(..), defaultDynFlags, dopt_set)
import Coercion (liftCoSubstWith, coercionKind, isReflCo, mkUnsafeCo)
import TyCon
import Type
import PrelRules
import Id
import Module
import Name (nameModule_maybe)
import IdInfo (isShortableIdInfo)
import DataCon
import Pair
import BasicTypes
import Demand (splitStrictSig, isBotRes)


-- FIXME: this doesn't really work very well if the Answers are indirections, which is a common case!
-- However, this is not so serious a problem since I started spotting manifest primops applications and
-- avoiding going through the wrapper to compile them.
evaluatePrim :: InScopeSet -> Tag -> PrimOp -> [Type] -> [Coerced Answer] -> Maybe (Anned (Coerced Answer))
evaluatePrim iss tg pop tys args = do
    args' <- fmap (map CoreSyn.Type tys ++) $ mapM to args
    (res:_) <- return [res | CoreSyn.BuiltinRule { CoreSyn.ru_nargs = nargs, CoreSyn.ru_try = f }
                          <- primOpRules pop (error "evaluatePrim: dummy primop name")
                      , nargs == length args
                      , Just res <- [f (const CoreSyn.NoUnfolding) args']]
    fmap (annedCoercedAnswer tg) $ fro res
  where
    to :: Coerced Answer -> Maybe CoreSyn.CoreExpr
    to (mb_co, (rn, v)) = fmap coerce $ case v of
        Literal l          -> Just (CoreSyn.Lit l)
        Coercion co        -> Just (CoreSyn.Coercion co)
        Data dc tys cos xs -> Just (CoreSyn.Var (dataConWrapId dc) `CoreSyn.mkTyApps` map (renameType iss rn) tys `CoreSyn.mkCoApps` cos `CoreSyn.mkVarApps` map (renameId rn) xs)
        _                  -> Nothing
      where
        -- It is quite important that we don't wrap things with spurious refl coercions when it comes
        -- to RULEs, because the default constant-folding rules don't trigger if there are too many coercions.
        -- Luckily, the coercion within a CastBy is guaranteed to be non-refl.
        coerce | Just co <- castByCo mb_co
               = (`CoreSyn.Cast` co)
               | otherwise
               = id

    fro :: CoreSyn.CoreExpr -> Maybe (Coerced Answer)
    fro (CoreSyn.Cast e co)   = fmap (\(mb_co', in_v) -> (castBy (maybe co (\co' -> mkTransCo iss co' co) (castByCo mb_co')) tg, in_v)) $ fro e
    fro (CoreSyn.Lit l)       = Just (Uncast, (emptyRenaming, Literal l))
    fro (CoreSyn.Coercion co) = Just (Uncast, (mkIdentityRenaming (tyCoVarsOfCo co), Coercion co))
    fro e = do (dc, univ_tys, e_args0) <- exprIsConApp_maybe (const CoreSyn.NoUnfolding) e
               case newTyConCo_maybe (dataConTyCon dc) of
                 Just co_axiom -> let [e_arg0] = e_args0 in fro (e_arg0 `CoreSyn.Cast` mkAxInstCo co_axiom univ_tys)
                 Nothing -> do
                   let (ex_tys, e_args1) = takeWhileJust toType_maybe     e_args0
                       (cos,    e_args2) = takeWhileJust toCoercion_maybe e_args1
                       (xs,     e_args3) = takeWhileJust toVar_maybe      e_args2
                   [] <- return e_args3
                   return (Uncast, (renamedValue (Data dc (univ_tys ++ ex_tys) cos xs)))
      where toType_maybe (CoreSyn.Type ty) = Just ty
            toType_maybe _                 = Nothing
            
            toCoercion_maybe (CoreSyn.Coercion co) = Just co
            toCoercion_maybe _                     = Nothing
            
            toVar_maybe (CoreSyn.Var x) = Just x
            toVar_maybe _               = Nothing

type ContextSummary = (Bool, [ArgSummary], CallCtxt)

summariseContext :: PureHeap -> Stack -> ContextSummary
summariseContext h k = trainCarFoldr go (True, [], BoringCtxt) k
  where go kf (lone_variable, arg_infos, cont_info) = case tagee kf of
          TyApply _         -> (lone_variable,           arg_infos, cont_info)
          CoApply _         -> (False, TrivArg         : arg_infos, ValAppCtxt)
          Apply x'          -> (False, summariseArg x' : arg_infos, ValAppCtxt)
          Scrutinise _ _ _  -> (True,                           [], CaseCtxt)
          PrimApply _ _ _ _ -> (True,                           [], ArgCtxt False)
          StrictLet _ _     -> (True,                           [], ArgCtxt False)
          Update _          -> (True,                           [], BoringCtxt)
          CastIt _          -> (lone_variable,           arg_infos, cont_info)

        summariseArg x' = case M.lookup x' h of
          Just hb | Just (_, e) <- heapBindingTerm hb -> if termIsValue e then ValueArg else NonTrivArg
          _ -> TrivArg

-- Can't use callSiteInline because it never inlines stuff with DFunUnfolding!
ghcHeuristics :: Id -> AnnedTerm {- try not to pull on this, it does a lot of work -}
              -> ContextSummary -> Bool
ghcHeuristics x e (lone_variable, arg_infos, cont_info)
  | isStrongLoopBreaker (idOccInfo x)
  = False -- NB: have to check this (not just use idUnfolding) because we might consider "e"
  | otherwise
  = (try (realIdUnfolding x) `mplus` try answer_unf) `orElse` trce (text "No unfolding") False
  where
    try unf = case unf of
      CoreSyn.CoreUnfolding { CoreSyn.uf_is_top = is_top, CoreSyn.uf_is_work_free = is_work_free, CoreSyn.uf_expandable = expandable
                            , CoreSyn.uf_arity = arity, CoreSyn.uf_guidance = guidance }
                               -> trce_fail (ppr (CoreSyn.uf_tmpl unf)) $
                                  Just $ tryUnfolding dflags1 x lone_variable 
                                                      arg_infos cont_info is_top 
                                                      is_work_free expandable
                                                      arity guidance
      -- GHC actually only looks through DFunUnfoldings in exprIsConApp_maybe,
      -- so I'll do this rough heuristic instead:
      CoreSyn.DFunUnfolding {} -> trce_fail (text "Unsaturated dictionary unfolding") $
                                  Just $ length arg_infos >= idArity x
      CoreSyn.NoUnfolding      -> Nothing
      CoreSyn.OtherCon {}      -> Nothing

    dflags0 = defaultDynFlags (error "ghcHeuristics: Settings in DynFlags used!")
    -- Set these two flags so that we get information about failed inlinings:
    dflags1 | tRACE     = dopt_set (dopt_set dflags0 Opt_D_verbose_core2core) Opt_D_dump_inlinings
            | otherwise = dflags0

    trce_fail :: SDoc -> Maybe Bool -> Maybe Bool
    --trce_fail doc (Just False) = trce doc (Just False)
    trce_fail _   mb_x         = mb_x

    trce :: SDoc -> a -> a
    trce | tRACE     = pprTrace ("Considering inlining: " ++ showSDoc (ppr x))
         | otherwise = flip const

    -- NB: I'm not particularly happy that I may have to make up a whole new unfolding at ever
    -- occurrence site, but GHC makes it hard to do otherwise because any binding with a non-stable
    -- Unfolding pinned to it gets the Unfolding zapped by GHC's renamer
    answer_unf = mkUnfolding CoreSyn.InlineRhs False False (termToCoreExpr (annedTermToTerm e))

-- | Non-expansive simplification we can do everywhere safely
--
-- Normalisation only ever releases deeds: it is *never* a net consumer of deeds. So normalisation
-- will never be impeded by a lack of deeds.
normalise :: UnnormalisedState -> State
normalise = snd . step' True . Right

-- | Possibly non-normalising simplification we can only do if we are allowed to by a termination test
--
-- Unlike normalisation, stepping may be a net consumer of deeds and thus be impeded by a lack of them.
step :: State -> Maybe State
step s = guard reduced >> return result
  where (reduced, result) = step' False $ Left s

step' :: Bool -> Either State UnnormalisedState -> (Bool, State) -- The flag indicates whether we managed to reduce any steps *at all*
step' normalising ei_state = {-# SCC "step'" #-}
    -- pprTrace "step'" (either (pPrintFullState quietStatePrettiness) (pPrintFullUnnormalisedState quietStatePrettiness) ei_state) $
    (\res@(_reduced, stepped_state) -> let _deeds = either releaseStateDeed releaseUnnormalisedStateDeed ei_state
                                           _doc = either (pPrintFullState quietStatePrettiness) (pPrintFullUnnormalisedState quietStatePrettiness) ei_state
                                           _fvs = either stateFreeVars unnormalisedStateFreeVars ei_state in
                                       ASSERT2(not dEEDS || noChange _deeds (releaseStateDeed stepped_state),
                                               hang (text "step': deeds lost or gained:") 2 (_doc $$ pPrintFullState quietStatePrettiness stepped_state))
                                       ASSERT2(subVarSet (stateFreeVars stepped_state) _fvs,
                                               text "step': FVs" $$ hang (text "Before:") 2 (pPrint _fvs $$ _doc) $$
                                                                    hang (text "After:")  2 (pPrint (stateFreeVars stepped_state) $$ pPrintFullState quietStatePrettiness stepped_state))
                                       -- traceRender (text "normalising" $$ nest 2 (pPrintFullUnnormalisedState state) $$ text "to" $$ nest 2 (pPrintFullState stepped_state)) $
                                       res) $
    go_entry ei_state
  where
    go_entry :: Either State UnnormalisedState -> (Bool, State)
    go_entry (Left (deeds, heap, k, anned_qa)) = case annee anned_qa of
      Question _ -> go_question (deeds, heap, k, fmap (\(Question x') -> x') anned_qa)
      Answer   _ -> go_answer   (deeds, heap, k, fmap (\(Answer a) -> a)     anned_qa)
    go_entry (Right state) = go state

    go :: UnnormalisedState -> (Bool, State)
    go (deeds, heap@(Heap h ids), k, (rn, e)) = case annee e of
        Var x            -> go_question (deeds, heap, k, fmap (\(rn, Var _)   -> renameId rn x) (renameAnned (rn, e)))
        Value v          -> go_answer   (deeds, heap, k, fmap (\(rn, Value _) -> (rn, v))       (renameAnned (rn, e)))
        TyApp e ty       -> go (deeds, heap,        Tagged tg (TyApply (renameType ids rn ty))                                   `Car` k, (rn, e))
        CoApp e co       -> go (deeds, heap,        Tagged tg (CoApply (renameCoercion ids rn co))                               `Car` k, (rn, e))
        App e x          -> go (deeds, heap,        Tagged tg (Apply (renameId rn x))                                            `Car` k, (rn, e))
        PrimOp pop tys es
          | (e:es) <- es -> go (deeds, heap,        Tagged tg (PrimApply pop (map (renameType ids rn) tys) [] (map ((,) rn) es)) `Car` k, (rn, e))
          | otherwise    -> pprPanic "step': nullary primops unsupported" (ppr pop)
        Case e x ty alts -> go (deeds, Heap h ids', Tagged tg (Scrutinise x' (renameType ids rn ty) (rn', alts))                 `Car` k, (rn, e))
          where (ids', rn', x') = renameNonRecBinder ids rn x
        Cast e co        -> go (deeds', heap,                                                                                         k', (rn, e))
          where (deeds', k') = appendCast deeds ids tg (renameCoercion ids rn co) k
        Let x e1 e2
          | isUnLiftedType (idType x) -> go (deeds,                  Heap h                                       ids', Tagged tg (StrictLet x' (rn', e2)) `Car` k, in_e1)
          | otherwise                 -> go (deeds `releaseDeeds` 1, Heap (M.insert x' (internallyBound in_e1) h) ids',                                          k, (rn', e2))
          where (ids', rn', (x', in_e1)) = renameNonRecBound ids rn (x, e1)
        LetRec xes e     -> go (deeds `releaseDeeds` 1, Heap (h `M.union` M.fromList [(x', internallyBound in_e) | (x', in_e) <- xes']) ids', k, (rn', e))
          where (ids', rn', xes') = renameBounds ids rn xes
      where tg = annedTag e

    appendCast deeds ids tg co' k
      | isReflCo final_co' = (deeds' `releaseDeeds` 1,                              k')
      | otherwise          = (deeds',                  Tagged tg (CastIt final_co') `Car` k')
      where
        -- Maintain the invariant that there are no adjacent casts in the stack
        (deeds', final_co', k') = case k of
          Tagged _old_tg (CastIt old_co') `Car` k' -> (deeds `releaseDeeds` 1, mkTransCo ids co' old_co', k')
          _                                        -> (deeds,                  co',                       k)

    -- TODO: in all the code below, I'm no longer sure whether we preserve deeds or not. In particular, the CastBy cases
    -- are a worry. But since the deeds stuff is probably on the way out I'm not trying to fix it right now.

    go_question (deeds, h, k, anned_x) = maybe (False, (deeds, h, k, fmap Question anned_x)) ((,) True) $ force  deeds h k (annedTag anned_x) (annee anned_x)
    go_answer   (deeds, h, k, anned_a) = maybe (False, (deeds, h, k, fmap Answer anned_a))   ((,) True) $ unwind deeds h k (annedTag anned_a) (annee anned_a)

    -- Deal with a variable at the top of the stack
    -- Might have to claim deeds if inlining a non-value non-internally-bound thing here
    -- FIXME: look inside unfoldings
    force :: Deeds -> Heap -> Stack -> Tag -> Out Var -> Maybe State
    force deeds (Heap h ids) k tg x'
      -- Try to trim the stack if the Id is guaranteed to bottom out after a certain number of arguments
      -- This is really amazingly important because so many case branches bottom out in at least one branch,
      -- and we can save supercompiling big case nests if we trim them out eagerly.
      --
      -- The supercompiled size of Bernouilli decreased from 19193 to 16173 with this change.
      -- FIXME: do this another way? (with freevars)
      | Just (ds, res_d) <- fmap splitStrictSig $ idStrictness_maybe x'
      , isBotRes res_d
      , Just (h_extra, k) <- trimUnreachable (length ds) (idType x') k
      = Just (deeds, Heap (h `M.union` h_extra) ids, k, fmap (\(Var x') -> Question x') (annedTerm tg (Var x'))) -- Kind of a hacky way to get an Anned Question!
      | otherwise = do
        -- NB: it doesn't matter if the thing we look up is superinlinable because we'll actually only need to check then when we come to
        -- look it up as a *value*. In any case, the vast majority of SUPERINLINABLE things will be values, so we'll never get here.
        hb <- M.lookup x' h
        in_e <- heapBindingTerm hb
        case termToCastAnswer ids in_e of
          -- Consider the case of non-internal bindings. We want to get at the values of them, but since we can't create updates we can
          -- only do so if they are already values. This is perhaps weird, but continuing using unwind' is exactly the right thing to do
          Just anned_cast_a -> do
            let deeds0 = releaseDeeds deeds 1 -- We can release the deed for the reference itself
            deeds1 <- if howBound hb == InternallyBound
                       then return deeds0
                       else claimDeeds deeds0 (annedSize anned_cast_a)
            unwind' deeds1 (Heap h ids) k (Just (x', (mkIdentityRenaming (unitVarSet x'), annedTerm tg (Var x'))))
                                          (annedToTagged anned_cast_a)
                                          (if dUPLICATE_VALUES_EVALUATOR then in_e else (mkIdentityRenaming (unitVarSet x'), fmap Var $ annedVar tg x'))
          Nothing -> do
            -- NB: we MUST NOT create update frames for non-concrete bindings!! This has bitten me in the past, and it is seriously confusing. 
            guard (howBound hb == InternallyBound)
            -- Avoid creating consecutive update frames: implements "stack squeezing" to maintain stack invariants
            -- NB: suprisingly this is not broken, even if there are cycles in the heap:
            --     <x |-> y, y |-> x | x |>
            -- --> <         y |-> x | y | update x>
            -- --> <         y |-> x | x | update x>
            --     The reason is that squeezing adds a heap binding that just points into an update frame bound thing,
            --     and once a binder is on the stack it won't be turned into a heap binder until we have got a value,
            --     so inlining the squeezed heap binding will either just get us stuck immediately or get to a value
            --
            -- NB: squeezing will discard any flag marking x' as superinlinable. However, I think this is totally OK:
            -- the update frame already on the stack (which is preserved by squeezing) contains the name by which the *user*
            -- tried to access the function, and it is in keeping with the rest of GHC (where the inlinability you see is
            -- based on the label you wrote in your own code) that that is the relevant flag.
            return $ normalise $ case (fst (peelUpdateStack k)) of
              Nothing                        -> (deeds, Heap (M.delete x' h)                            ids, Tagged tg (Update x') `Car` k, in_e)
              Just (cast_by, Tagged tg_y y') -> (deeds, Heap (M.insert x' (internallyBound in_e_ref) h) ids,                             k, in_e)
                where in_e_ref = mkVarCastBy tg_y y' cast_by

    -- TODO: this function totally ignores deeds
    trimUnreachable :: Int   -- Number of value arguments needed before evaluation bottoms out
                    -> Type  -- Type of the possibly-bottoming thing in the hole
                    -> Stack -- Stack consuming the hole
                    -> Maybe (PureHeap, -- Heap bindings arising from any update frames we trimmed off
                              Stack)    -- Trimmed stack (strictly "less" than the input one -- not necessarily shorter since we will replace e.g. a trailing Scrutinise with a Cast)
    trimUnreachable = go
      where
        -- Ran out of stack: even if n == 0 we don't want to
        -- trim the stack in these cases because we musn't return
        -- Just if the tail of the stack is already trivial: doing
        -- so would risk non-termination
        go _ _       (Loco _)                           = Nothing
        go _ _       (Tagged _ (CastIt _) `Car` Loco _) = Nothing
        -- Got some non-trivial stack that is unreachable due to bottomness: kill it (remembering to bind any updated stuff)
        go 0 hole_ty k@(Tagged cast_tg _ `Car` _) = Just $
          trainFoldl' (\(!hole_ty,    !h) (Tagged tg kf) -> (stackFrameType' kf hole_ty, case kf of Update x' -> M.insert x' (internallyBound (renamedTerm (annedTerm tg (Var x')))) h; _ -> h))
                      (\(!overall_ty, !h) gen -> (h, (if hole_ty `eqType` overall_ty then id else (Tagged cast_tg (CastIt (mkUnsafeCo hole_ty overall_ty)) `Car`)) $ Loco gen)) (hole_ty, M.empty) k
        -- Haven't yet reached a bottom, but we might get enough arguments to reach
        -- one in the future, so keep going
        go n hole_ty (kf `Car` k) = mb_n' >>= \n' -> liftM (second (kf `Car`)) $ go n' (stackFrameType kf hole_ty) k
          where mb_n' = case tagee kf of
                          TyApply _         -> Just n
                          CoApply _         -> Just (n - 1)
                          Apply _           -> Just (n - 1)
                          Scrutinise _ _ _  -> Nothing
                          PrimApply _ _ _ _ -> Nothing
                          StrictLet _ _     -> Nothing
                          Update _          -> Just n
                          CastIt _          -> Just n

    -- Deal with a value at the top of the stack
    unwind :: Deeds -> Heap -> Stack -> Tag -> Answer -> Maybe State
    unwind deeds heap k tg_a a = unwind' deeds heap k Nothing (Tagged tg_a (Uncast, a)) (taggedAnswerToInAnnedTerm (Tagged tg_a a))

    unwind' :: Deeds -> Heap -> Stack
            -> Maybe (Var,             -- Variable name of thing in focus, for normalisation check/SUPERINLINABLE purposes
                      In AnnedTerm)    -- Version of the focus that should go into *focus* (useful if we are reducing <x = v | x | update y >, we want x in the focus in next step, not y!)
            -> Tagged (Coerced Answer) -- "Meaning" of the focus, in value terms
            -> In AnnedTerm            -- Version of the focus that should go into heap (may just be a variable reference to a value)
            -> Maybe State
    unwind' deeds heap@(Heap h ids) k mb_x' (Tagged tg_a cast_a@(mb_co, (rn, v))) in_e_heap = case k of
     Loco _ -> Nothing
     Car kf k -> case tagee kf of
        TyApply ty'                    ->                  tyApply    (deeds `releaseDeeds` 1)          ty'
        CoApply co'                    ->                  coApply    (deeds `releaseDeeds` 1)          co'
        Apply x2'                      ->                  apply      deeds                    (tag kf) x2'
        Scrutinise x' ty' in_alts      -> fmap normalise $ scrutinise (deeds `releaseDeeds` 1)          x' ty' in_alts
        PrimApply pop tys' in_vs in_es -> fmap normalise $ primop     deeds                    (tag kf) pop tys' in_vs in_es
        StrictLet x' in_e2             -> fmap normalise $ strictLet  (deeds `releaseDeeds` 1)          x' in_e2
        -- NB: since there are never two adjacent casts on the stack, our reference expressions will always have at most 1 coercion
        CastIt co'                     -> unwind' deeds heap k (fmap (second cast_e) mb_x') (castAnswer ids (tag kf) co' (Tagged tg_a cast_a)) (cast_e in_e_heap) -- TODO: can potentially release deeds from cast_a double-cast here
          where cast_e in_e = (mkInScopeIdentityRenaming ids, annedTerm (tag kf) (renameIn (renameAnnedTerm ids) in_e `Cast` co'))
        Update x'                      -> do
            -- If duplicating values, we ensure normalisation by not executing updates
            guard (not normalising || not dUPLICATE_VALUES_EVALUATOR)
            return $ normalise (deeds', Heap (M.insert x' (internallyBound in_e_heap) h) ids, k, in_e')
          where Just deeds' = claimDeeds (deeds `releaseDeeds` castAnswerSize cast_a) (annedTermSize (snd in_e_heap))
                in_e' = case mb_x' of
                          Nothing              -> (mkIdentityRenaming (unitVarSet x'), fmap Var $ annedVar (tag kf) x')
                          Just (_, in_e_focus) -> in_e_focus
      where
        -- Should check this is not Nothing before using the "meaning" a
        checkShouldExposeUnfolding = case mb_x' of
                     -- FIXME: if we eliminate "dead" update frames, we should return True here or else WAY TOO MUCH will be treated as superinlinable
                     Nothing -> Just True
                     -- We have to check this here (not just when preparing unfoldings) because
                     -- we would like to also exclude local recursive loops, not just top-level ones
                     Just (x', _) -> case shouldExposeUnfolding x' of
                       Right super  -> Just super
                       Left why_not -> pprTrace "Unavailable:" (ppr x' <+> parens (text why_not)) $
                                       fail why_not

        -- NB: assumes that ei_state has the same size as what we would return from "step" if the dereferencing fails.
        -- At the time of writing, this invariant holds (since turning terms into equivalent stack frames doesn't change size)
        checkLambdaish :: UnnormalisedState -> Maybe State
        checkLambdaish s'_unnormalised
          -- If not duplicating values, we ensure normalisation by not executing applications to non-explicit-lambdas
          | normalising, isJust mb_x', not dUPLICATE_VALUES_EVALUATOR = Nothing
          | otherwise = do
            super <- checkShouldExposeUnfolding
            let s' = normalise s'_unnormalised
            -- NB: you might think it would be OK to inline even when *normalising*, as long as the inlining
            -- makes the term strictly smaller. This is very tempting, but we would have to check that the size
            -- measure decreased by the inlining is the same as the size measure needed to prove normalisation of
            -- the reduction system, and I'm too lazy to do that right now.
            --
            -- NB: it is hard to get a size-decrease check like this to work well. Consider:
            --  bindIO getArgs (\x -> e)
            -- This has 2 applications and one lambda (I've omitted all casts, since they cost 0). If we inline we get:
            --  \s -> case getArgs s of (# x, s #) -> e s
            -- This has 2 applications, one lambda, and ONE CASE EXPRESSION. So things must have got worse! But bindIO with
            -- one known argument is almost a canonical example of the kind of thing we *would* like to inline, and the example
            -- above assumes perfect reduction: note in particular that we've beta-reduced the (\x -> e) away entirely (and
            -- done so within a case branch), so to even get this far we need "deep normalisation" *and* inlining used-once lams
            -- as part of this check.
            --
            -- Perhaps if we had deep normalisation + GC we could get these results by penalising heap allocation heavily?
            -- If so we must remember to do it for heap bindings *and* letrecs.
            let k_summary = summariseContext h (Car kf k)
            guard $ case () of
              _ -- If the lambda is marked SUPERINLINABLE, always inline it
                | super
                -> True
                -- If inlining gets us to a value, accept it. This is a bit ad-hoc for two reasons:
                --  1. We might reach a value now, and then later apply two more arguments to effectively unconditionally inline a full application
                --  2. We might need to do another round of inlining to actually expose a value e.g. g in (f = \x y -> e; g = \x -> f x; h = g x)
                -- However, this is important so that speculation is able to turn partial applications into explicit values
                | (_, _, k, qa) <- s'
                , Answer _ <- annee qa
                , Just _ <- isCastStack_maybe k -- Might be a trailing cast
                -> True
                -- If the result is actually smaller, accept it (this catches manifest values)
                -- NB: garbage collect before comparison in case we inlined an internallyBound thing
                -- into its only use site, which is a very important case to catch!
                -- TODO: should gc the unnormalised state as well.. but risks non-termination?
                | stateSize (gc s') <= either (stateSize . gc) unnormalisedStateSize ei_state
                -> True
                -- It might still be OK to get larger if GHC's inlining heuristics say we should
                | Just (x', _) <- mb_x'
                , ghcHeuristics x' (annedTerm tg_a (coercedAnswerToAnnedTerm' ids cast_a)) k_summary -- NB: the tag is irrelevant
                -> True
                -- Otherwise, we don't want to beta-reduce
                | otherwise
                -> pprTrace "Unwanted:" (pPrint (coercedAnswerToAnnedTerm' ids cast_a)) False
            (case mb_x' of Just (x', _) -> pprTrace "Inlining" (ppr x'); Nothing -> id) $
              return s'

        tyApply :: Deeds -> Out Type -> Maybe State
        tyApply deeds0 ty' = do
          TyLambda x e_body <- return v
          checkLambdaish $
            let deeds1 = deeds0 `releaseDeeds` 1 -- Release deed associated with the lambda (but not its body)
                (deeds2, k') = case mb_co of Uncast            -> (deeds1, k)
                                             CastBy co' _tg_co -> appendCast deeds1 ids tg_a (co' `mk_inst` ty') k
            in (deeds2, heap, k', (insertTypeSubst rn x ty', e_body))
          where mk_inst = mkInstCo ids

        coApply :: Deeds -> Out Coercion -> Maybe State
        coApply deeds0 apply_co' = do
          Lambda x e_body <- return v
          checkLambdaish $
            let deeds1 = deeds0 `releaseDeeds` 1 -- Release deed associated with the lambda (but not its body)
            in case mb_co of
                 Uncast            -> (deeds1, heap, k,  (insertCoercionSubst rn x apply_co',      e_body))
                 CastBy co' _tg_co -> (deeds2, heap, k', (insertCoercionSubst rn x cast_apply_co', e_body))
                   where -- Implements the special case of beta-reduction of cast lambda where the argument is an explicit coercion value.
                         -- You can derive this rule from the rules in "Practical aspects of evidence-based compilation" by combining:
                         --  1. TPush, to move the co' from the lambda to the argument and result (arg_co' and res_co')
                         --  2. The rules in Figure 5, to replace a cast of a coercion value with a simple coercion value
                         --  3. The fact that nth commutes with sym to clean up the result (can be proven from Figure 4)
                         (arg_co', res_co') = (mkNthCo 0 co', mkNthCo 1 co')
                         (arg_from_co', arg_to_co') = (mkNthCo 0 arg_co', mkNthCo 1 arg_co')
                         cast_apply_co' = arg_from_co' `mk_trans` apply_co' `mk_trans` mk_sym arg_to_co'
                         mk_trans = mkTransCo ids
                         mk_sym = mkSymCo ids
                         -- Maintain the no-adjacent-casts invariant
                         (deeds2, k') = appendCast deeds1 ids tg_a res_co' k

        apply :: Deeds -> Tag -> Out Var -> Maybe State
        apply deeds0 tg_kf x' = do
          Lambda x e_body <- return v
          checkLambdaish $ case mb_co of
              Uncast -> (deeds1, Heap h ids, k, (insertIdRenaming rn x x', e_body))
                where deeds1 = deeds0 `releaseDeeds` 2 -- Release deed associated with the lambda (but not its body), AND that from the stack frame
              CastBy co' _tg_co -> (deeds1, Heap (M.insert y' (internallyBound (renamedTerm e_arg)) h) ids', k', (rn', e_body))
                where (ids', rn', y') = renameNonRecBinder ids rn (x `setIdType` arg_co_from_ty')
                      Pair arg_co_from_ty' _arg_co_to_ty' = coercionKind arg_co'
                      (arg_co', res_co') = (mkNthCo 0 co', mkNthCo 1 co')
                      e_arg = annedTerm tg_a (annedTerm tg_kf (Var x') `Cast` mkSymCo ids arg_co')
                      (deeds1, k') = appendCast deeds0 ids tg_a res_co' k -- Might release a deed if the final body coercion is refl

        -- TODO: use checkShouldExposeUnfolding
        scrutinise :: Deeds -> Out Var -> Out Type -> In [AnnedAlt] -> Maybe UnnormalisedState
        scrutinise deeds_init wild' _ty' (rn_alts, alts)
           -- Literals are easy -- we can make the simplifying assumption that the types of literals are
           -- always simple TyCons without any universally quantified type variables.
          | Literal l <- v
          , case mb_co_kind of Nothing -> True; Just (_, _, Pair from_ty' to_ty') -> from_ty' `eqType` to_ty' -- NB: should never see refl here!
          , (deeds2, alt_e):_ <- [(deeds1 `releaseDeeds` annedAltsSize rest, (rn_alts, alt_e)) | ((LiteralAlt alt_l, alt_e), rest) <- bagContexts alts, alt_l == l]
          = Just (deeds2, Heap h1 ids, k, alt_e)
          
           -- Data is a big stinking mess! I hate you, KPush rule.
          | Data dc tys cos xs <- v
           -- a) Ensure that the coercion on the data (if any) lets us do the reduction, and determine
           --    the appropriate coercions to use (if any) on each value argument to the DataCon
          , Just mb_dc_cos <- case mb_co_kind of
                                    Nothing -> return Nothing
                                    Just (co', tg_co, Pair from_ty' to_ty') -> do
                                      (from_tc, _from_tc_arg_tys') <- splitTyConApp_maybe from_ty'
                                      (to_tc,   _to_tc_arg_tys')   <- splitTyConApp_maybe to_ty'
                                      guard $ from_tc == to_tc
                                      return $ Just $
                                        let -- Substantially copied from CoreUnfold.exprIsConApp_maybe:
                                            tc_arity       = tyConArity from_tc
                                            dc_univ_tyvars = dataConUnivTyVars dc
                                            dc_ex_tyvars   = dataConExTyVars dc
                                            arg_tys        = dataConRepArgTys dc
                                        
                                            -- Make the "theta" from Fig 3 of the paper
                                            (_univ_tys, ex_tys) = splitAt tc_arity tys
                                            gammas = decomposeCo tc_arity co'
                                            theta_subst = ASSERT2(length dc_univ_tyvars == tc_arity,      ppr dc $$ ppr dc_univ_tyvars $$ ppr tc_arity)
                                                          ASSERT2(length dc_ex_tyvars   == length ex_tys, ppr dc $$ ppr dc_ex_tyvars $$ ppr (length ex_tys))
                                                          liftCoSubstWith (dc_univ_tyvars ++ dc_ex_tyvars)
                                                                          (gammas         ++ map mkReflCo ex_tys)
                                        in map (\arg_ty -> (theta_subst arg_ty, tg_co)) arg_tys -- Use tag from the original coercion everywhere
           -- b) Identify the first appropriate branch of the case and reduce -- apply the discovered coercions if necessary
          , (deeds3, h', ids', alt_e):_ <- [ res
                                           | ((DataAlt alt_dc alt_as alt_qs alt_xs, alt_e), rest) <- bagContexts alts
                                           , alt_dc == dc
                                           , let tys' = map (renameType ids rn) tys
                                                 cos' = map (renameCoercion ids rn) cos
                                                 xs' = map (renameId rn) xs
                                                 rn_alts' = insertTypeSubsts rn_alts (alt_as `zip` tys')
                                                 deeds2 = deeds1 `releaseDeeds` annedAltsSize rest
                                           , Just res <- [do (deeds3, h', ids', rn_alts') <- case mb_dc_cos of
                                                               Nothing     -> return (deeds2, h1, ids, insertIdRenamings (insertCoercionSubsts rn_alts' (alt_qs `zip` cos')) (alt_xs `zip` xs'))
                                                               Just dc_cos -> foldM (\(deeds, h, ids, rn_alts) (uncast_e_arg', alt_y, (dc_co, tg_co)) ->
                                                                                        let Pair _dc_co_from_ty' dc_co_to_ty' = coercionKind dc_co -- TODO: use to_tc_arg_tys' from above?
                                                                                            (ids', rn_alts', y') = renameNonRecBinder ids rn_alts (alt_y `setIdType` dc_co_to_ty')
                                                                                            e_arg = annedTerm tg_co $ annedTerm tg_a uncast_e_arg' `Cast` dc_co
                                                                                        in fmap (\deeds' -> (deeds', M.insert y' (internallyBound (renamedTerm e_arg)) h, ids', rn_alts')) $ claimDeeds deeds (annedSize e_arg))
                                                                                    (deeds2, h1, ids, rn_alts') (zip3 (map (Value . Coercion) cos' ++ map Var xs') (alt_qs ++ alt_xs) dc_cos)
                                                             return (deeds3, h', ids', (rn_alts', alt_e))]
                                           ]
          = Just (deeds3, Heap h' ids', k, alt_e)
          
           -- Thank god, default alternatives are trivial:
          | (deeds2, alt_e):_ <- [(deeds1 `releaseDeeds` annedAltsSize rest, (rn_alts, alt_e)) | ((DefaultAlt, alt_e), rest) <- bagContexts alts]
          = Just (deeds2, Heap h1 ids, k, alt_e)
          
           -- This can legitimately occur, e.g. when supercompiling (if x then (case x of False -> 1) else 2)
          | otherwise
          = Nothing
          where mb_co_kind = case mb_co of
                               Uncast          -> Nothing
                               CastBy co tg_co -> Just (co, tg_co, coercionKind co)
                deeds0 = deeds_init `releaseDeeds` castAnswerSize cast_a
                (deeds1, h1) | isDeadBinder wild' = (deeds0,                         h)
                             | otherwise          = (deeds0', M.insert wild' wild_hb h)
                               where wild_hb = internallyBound in_e_heap
                                     Just deeds0' = claimDeeds deeds0 (annedTermSize (snd in_e_heap))
                                     -- NB: we add the *non-dereferenced* value to the heap for a case wildcard, because anything else may duplicate allocation

        -- NB: this actually duplicates the answer "a" into the answers field of the PrimApply, even if that "a" is update-bound
        -- This isn't perhaps in the spirit of the rest of the evaluator, but it probably doesn't matter at all in practice.
        -- TODO: use checkShouldExposeUnfolding?
        primop :: Deeds -> Tag -> PrimOp -> [Out Type] -> [Anned (Coerced Answer)] -> [In AnnedTerm] -> Maybe UnnormalisedState
        primop deeds tg_kf pop tys' anned_as [] = do
            guard eVALUATE_PRIMOPS -- NB: this is not faithful to paper 1 because we still turn primop expressions into
                                   -- stack frames.. this is bad because it will impede good specilations (without smart generalisation)
            let tg_kf' = tg_kf { tagOccurrences = if oCCURRENCE_GENERALISATION then tagOccurrences tg_kf + sum (map tagOccurrences (tg_a : map annedTag anned_as)) else 1 }
            anned_cast_a' <- evaluatePrim ids tg_kf' pop tys' (map annee anned_as ++ [cast_a])
            deeds <- claimDeeds (deeds `releaseDeeds` (sum (map annedSize anned_as) + castAnswerSize cast_a + 1)) (annedSize anned_cast_a') -- I don't think this can ever fail
            return (deeds, heap, k, taggedCastAnswerToInAnnedTerm ids (annedToTagged anned_cast_a'))
        primop deeds tg_kf pop tys' anned_as in_es = case in_es of
            (in_e:in_es) -> Just (deeds, heap, Tagged tg_kf (PrimApply pop tys' (anned_as ++ [annedCoercedAnswer tg_a cast_a]) in_es) `Car` k, in_e)
            []           -> Nothing

        strictLet :: Deeds -> Out Var -> In AnnedTerm -> Maybe UnnormalisedState
        strictLet deeds x' in_e2 = Just (deeds', Heap (M.insert x' (internallyBound in_e_heap) h) ids, k, in_e2)
          where Just deeds' = claimDeeds (deeds `releaseDeeds` castAnswerSize cast_a) (annedTermSize (snd in_e_heap))


-- We don't want to expose an unfolding if it would not be inlineable in the initial phase.
-- This gives normal RULES more of a chance to fire.
--
-- NB: this only controls whether a particular definition is available for inlining at all.
-- For the inlining to actualy happen we also make a check at the call-site that considers
-- whether it is benefical to inline the definition into the particular context we find.
--
-- NB: a place where these heuristics really hurt is:
--   {-# INLINE [0] foldr #-}
--   foldr k z = go
--             where
--               go []     = z
--               go (y:ys) = y `k` go ys
--
-- Although foldr gets inlined by the supercompiler, "go" is a local recursive loop which
-- doesn't get inlined at all. See also:
--  {-# SUPERINLINABLE sum #-}
--  sum     l       = sum' l 0
--    where
--      sum' []     a = a
--      sum' (x:xs) a = sum' xs (a+x)
--
-- A possible solution is to mark all those Ids syntactically contained within a SUPERINLINABLE
-- unfolding as SUPERINLINABLE if they are not explicitly INLINE/NOINLINE. We can do this when
-- we construct the unfoldings in the first place.
shouldExposeUnfolding :: Id -> Either String Superinlinable
shouldExposeUnfolding x = case inl_inline inl_prag of
    -- FIXME: God help my soul
    _ | Just mod <- nameModule_maybe (idName x)
      , moduleName mod `elem` map mkModuleName ["Data.Complex", "GHC.List"]
      -> Right True
    -- NB: we don't check the activation on INLINE things because so many activations
    -- are used to ensure that e.g. RULE-based fusion works properly, and NOINLINE will
    -- generally impede supercompiler-directed fusion.
    --
    -- Our philosophy: if it is *ever* inlinable (in any phase), expose it
    Inline                                 -> Right True -- Don't check for size increase at all if marked INLINE
    Inlinable super
      | only_if_superinlinable, not super  -> Left "INLINEABLE but not SUPERINLINABLE"
      | otherwise                          -> Right super
    NoInline
      | isNeverActive (inl_act inl_prag)   -> Left "unconditional NONLINE"
      | only_if_superinlinable             -> Left "conditional NOINLINE, not SUPERINLINABLE"
    EmptyInlineSpec 
      | only_if_superinlinable             -> Left "not SUPERINLINABLE"
    _                                      -> Right False
  where inl_prag = idInlinePragma x
        -- EXPERIMENT: only respect the SUPERINLINABLE distinction on *loop breakers*
        -- The motivation is that we don't really want to go around annotating (GHC.Base.>>=),
        -- bindIO, etc etc as SUPERINLINABLE.
        only_if_superinlinable = case sUPERINLINABILITY of
          ForRecursion  -> isStrongLoopBreaker (idOccInfo x)
          ForEverything -> True
          ForNothing    -> False


-- We used to garbage-collect in the evaluator, when we executed the rule for update frames. This had two benefits:
--  1) We don't have to actually update the heap or even claim a new deed
--  2) We make the supercompiler less likely to terminate, because GCing so tends to reduce TagBag sizes
--
-- However, this caused problems with speculation: to prevent incorrectly garbage collecting bindings from the invisible "enclosing"
-- heap when we speculated one of the bindings from the heap, we had to pass around an extra "live set" of parts of the heap that might
-- be referred to later on. Furthermore:
--  * Finding FVs when executing every update step was a bit expensive (though they were memoized on each of the State components)
--  * This didn't GC cycles (i.e. don't consider stuff from the Heap that was only referred to by the thing being removed as "GC roots")
--  * It didn't seem to make any difference to the benchmark numbers anyway
--
-- You might think a good alternative approach is to:
-- 1. Drop dead update frames in transitiveInline (which is anyway responsible for ensuring there is no dead stuff in the stack)
-- 2. "Squeeze" just before the matcher: this shorts out indirections-to-indirections and does update-frame stack squeezing.
--    You might also think that it would be cool to just do this in normalisation, but then when normalising during specualation the enclosing
--    context wouldn't get final_rned :-(
--
-- HOWEVER. That doesn't work properly because normalisation itself can introduce dead bindings - i.e. in order to be guaranteed to
-- catch all the junk we have to GC normalised bindings, not the pre-normalised ones that transitiveInline sees. So instead I did
-- both points 1 and 2 right just before we go to the matcher.
--
-- HOWEVER. Simon suggested something that made me realise that actually we could do squeezing of consecutive update frames and
-- indirection chains in the evaluator (and thus the normaliser) itself, which is even cooler. Thus all that is left to do in the
-- GC is to make a "global" analysis that drops stuff that is definitely dead. We *still* want to run this just before the matcher because
-- although dead heap bindings don't bother it, it would be confused by dead update frames.
--
-- TODO: have the garbage collector collapse (let x = True in x) to (True) -- but note that this requires onceness analysis
gc :: State -> State
gc _state@(deeds0, Heap h ids, k, in_e)
  = {-# SCC "gc" #-}
    ASSERT2(stateUncoveredVars gced_state `subVarSet` stateUncoveredVars _state, ppr (stateUncoveredVars gced_state, PrettyDoc (pPrintFullState quietStatePrettiness _state), PrettyDoc (pPrintFullState quietStatePrettiness gced_state)))
    gced_state -- We do not insist that *no* variables are uncovered because when used from the speculator this may not be true
  where
    gced_state = (deeds2, Heap h' ids, k', in_e)
    
    -- We have to use stateAllFreeVars here rather than stateFreeVars because in order to safely prune the live stack we need
    -- variables bound by k to be part of the live set if they occur within in_e or the rest of the k
    live0 = stateAllFreeVars (deeds0, Heap M.empty ids, k, in_e)
    (deeds1, h', live1) = inlineLiveHeap deeds0 h live0
    -- Collecting dead update frames doesn't make any new heap bindings dead since they don't refer to anything
    (deeds2, k') | False     = pruneLiveStack deeds1 k live1
                 | otherwise = (deeds1, k) -- FIXME: turned this off for now because it means that the resulting term might not be normalised (!!!)
                                           -- NB: if you change this check out checkShouldExposeUnfolding as well
    
    inlineLiveHeap :: Deeds -> PureHeap -> FreeVars -> (Deeds, PureHeap, FreeVars)
    inlineLiveHeap deeds h live = (foldr (flip releaseHeapBindingDeeds . snd) deeds h_dead_kvs, h_live, live')
      where
        (h_dead_kvs, h_live, live') = heap_worker (M.toAscList h) M.empty live
        
        -- This is just like Split.transitiveInline, but simpler since it never has to worry about running out of deeds:
        heap_worker :: [(Var, HeapBinding)]   -- Possibly-dead heap as sorted list. NB: not a PureHeap map because creating..
                    -> PureHeap -> FreeVars   -- ..the map on every iteration showed up as 6% of SC allocations!
                    -> ([(Var, HeapBinding)], -- Dead heap
                        PureHeap, FreeVars)
        heap_worker h_pending h_output live
          = if live == live'
            then (h_pending', h_output', live')
            else heap_worker h_pending' h_output' live'
          where 
            (h_pending', h_output', live') = foldr consider_inlining ([], h_output, live) h_pending
        
            -- NB: It's important that type variables become live after inlining a binding, or we won't
            -- necessarily lambda-abstract over all the free type variables of a h-function
            consider_inlining (x', hb) (h_pending_kvs, h_output, live)
              | x' `elemVarSet` live = (h_pending_kvs,            M.insert x' hb h_output, live `unionVarSet` heapBindingFreeVars hb `unionVarSet` varBndrFreeVars x')
              | otherwise            = ((x', hb) : h_pending_kvs, h_output,                live)
    
    -- NB: doing this is cool yet also dangerous at the same time. What if we have:
    --  {-# NOINLINE foo #-}
    --  foo = \x -> e
    --
    --  root = case foo 100 of \Delta
    --
    -- After normalisation + GCing (including dropping dead update frames) we will basically get:
    --  case (\x -> e) 100 of \Delta
    --
    -- So this is really bad because we have lost the NOINLINE information!
    -- Of course, this is also sometimes cool because it turns non-normalising beta-reductions into manifestly normalising ones.
    --
    -- My compromise is to allow dumping only those binders with "shortable" IdInfo, where shortability
    -- is a notion stolen from GHCs simplifier.
    --
    -- TODO: perhaps this same check should be applied in the Update frame compressor, though that would destroy some stack invariants
    pruneLiveStack :: Deeds -> Stack -> FreeVars -> (Deeds, Stack)
    pruneLiveStack init_deeds k live = trainFoldr (\kf (deeds, k_live) -> if (case tagee kf of Update x' | isShortableIdInfo (idInfo x') -> x' `elemVarSet` live; _ -> True)
                                                                          then (deeds, kf `Car` k_live)
                                                                          else (deeds `releaseStackFrameDeeds` kf, k_live))
                                                  (\gen deeds -> (deeds, Loco gen)) init_deeds k
