module Supercompile.Evaluator.Evaluate (normalise, step) where

#include "HsVersions.h"

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.StaticFlags
import Supercompile.Utilities

import qualified Data.Map as M

import qualified CoreSyn as CoreSyn
import Coercion
import TyCon
import Type
import PrelRules
import Id
import DataCon
import Pair


evaluatePrim :: InScopeSet -> Tag -> PrimOp -> [Type] -> [Answer] -> Maybe (Anned Answer)
evaluatePrim iss tg pop tys args = do
    args' <- fmap (map CoreSyn.Type tys ++) $ mapM to args
    (res:_) <- return [res | CoreSyn.BuiltinRule { CoreSyn.ru_nargs = nargs, CoreSyn.ru_try = f }
                          <- primOpRules pop (error "evaluatePrim: dummy primop name")
                      , nargs == length args
                      , Just res <- [f (const CoreSyn.NoUnfolding) args']]
    fmap (annedAnswer tg) $ fro res
  where
    to :: Answer -> Maybe CoreSyn.CoreExpr
    to (mb_co, (rn, v)) = fmap (maybe id (flip CoreSyn.Cast . fst) mb_co) $ case v of
        Literal l          -> Just (CoreSyn.Lit l)
        Coercion co        -> Just (CoreSyn.Coercion co)
        Data dc tys cos xs -> Just (CoreSyn.Var (dataConWrapId dc) `CoreSyn.mkTyApps` map (renameType iss rn) tys `CoreSyn.mkCoApps` cos `CoreSyn.mkVarApps` map (renameId rn) xs)
        _                  -> Nothing
    
    fro :: CoreSyn.CoreExpr -> Maybe Answer
    fro (CoreSyn.Cast e co)   = fmap (\(mb_co', in_v) -> (Just (maybe co (\(co', _) -> co' `mkTransCo` co) mb_co', tg), in_v)) $ fro e
    fro (CoreSyn.Lit l)       = Just (Nothing, (emptyRenaming, Literal l))
    fro (CoreSyn.Coercion co) = Just (Nothing, (mkIdentityRenaming (tyCoVarsOfCo co), Coercion co))
    fro e | CoreSyn.Var f <- e_fun, Just dc <- isDataConId_maybe f, [] <- e_args3 = Just (Nothing, (renamedValue (Data dc tys cos xs)))
          | otherwise = Nothing
      where (e_fun, e_args0) = CoreSyn.collectArgs e
            (tys, e_args1) = takeWhileJust toType_maybe     e_args0
            (cos, e_args2) = takeWhileJust toCoercion_maybe e_args1
            (xs,  e_args3) = takeWhileJust toVar_maybe      e_args2
            
            toType_maybe (CoreSyn.Type ty) = Just ty
            toType_maybe _                 = Nothing
            
            toCoercion_maybe (CoreSyn.Coercion co) = Just co
            toCoercion_maybe _                     = Nothing
            
            toVar_maybe (CoreSyn.Var x) = Just x
            toVar_maybe _               = Nothing

castAnswer :: Answer -> Maybe (Out Coercion, Tag) -> Answer
castAnswer (mb_co, in_v) mb_co' = (plusMaybe (\(co, _tg) (co', tg') -> (co `mkTransCo` co', tg')) mb_co mb_co', in_v)


-- | Non-expansive simplification we can do everywhere safely
--
-- Normalisation only ever releases deeds: it is *never* a net consumer of deeds. So normalisation
-- will never be impeded by a lack of deeds.
normalise :: UnnormalisedState -> State
normalise = snd . step' True

-- | Possibly non-normalising simplification we can only do if we are allowed to by a termination test
--
-- Unlike normalisation, stepping may be a net consumer of deeds and thus be impeded by a lack of them.
step :: State -> Maybe State
step s = guard reduced >> return result
  where (reduced, result) = step' False $ denormalise s

step' :: Bool -> UnnormalisedState -> (Bool, State) -- The flag indicates whether we managed to reduce any steps *at all*
step' normalising state =
    (\res@(_reduced, stepped_state) -> ASSERT2(noChange (releaseUnnormalisedStateDeed state) (releaseStateDeed stepped_state),
                                               hang (text "step': deeds lost or gained:") 2 (pPrintFullUnnormalisedState state $$ pPrintFullState stepped_state))
                                       ASSERT2(subVarSet (stateFreeVars stepped_state) (unnormalisedStateFreeVars state),
                                               text "step': FVs" $$ hang (text "Before:") 2 (pPrint (unnormalisedStateFreeVars state) $$ pPrintFullUnnormalisedState state) $$
                                                                    hang (text "After:")  2 (pPrint (stateFreeVars stepped_state) $$ pPrintFullState stepped_state))
                                       -- traceRender (text "normalising" $$ nest 2 (pPrintFullUnnormalisedState state) $$ text "to" $$ nest 2 (pPrintFullState stepped_state)) $
                                       res) $
    go state
  where
    go :: (Deeds, Heap, Stack, In AnnedTerm) -> (Bool, State)
    go (deeds, heap@(Heap h ids), k, (rn, e)) 
     | Just anned_a <- termToAnswer ids (rn, e) = go_answer (deeds, heap, k, anned_a)
     | otherwise = case annee e of
        Var x            -> go_question (deeds, heap, k, fmap (\(rn, Var _) -> renameId rn x) (renameAnned (rn, e)))
        Value v          -> pprPanic "step': values are always answers" (ppr v)
        TyApp e ty       -> go (deeds, heap,        Tagged tg (TyApply (renameType ids rn ty))                                   : k, (rn, e))
        CoApp e co       -> go (deeds, heap,        Tagged tg (CoApply (renameCoercion ids rn co))                               : k, (rn, e))
        App e x          -> go (deeds, heap,        Tagged tg (Apply (renameId rn x))                                            : k, (rn, e))
        PrimOp pop tys es
          | (e:es) <- es -> go (deeds, heap,        Tagged tg (PrimApply pop (map (renameType ids rn) tys) [] (map ((,) rn) es)) : k, (rn, e))
          | otherwise    -> pprPanic "step': nullary primops unsupported" (ppr pop)
        Case e x ty alts -> go (deeds, Heap h ids', Tagged tg (Scrutinise x' (renameType ids rn ty) (rn', alts))                 : k, (rn, e))
          where (ids', rn', x') = renameNonRecBinder ids rn x
        Cast e co        -> go (deeds, heap,        Tagged tg (CastIt (renameCoercion ids rn co))                                : k, (rn, e))
        Let x e1 e2
          | isUnLiftedType (idType x) -> go (deeds,     Heap h                                       ids', Tagged tg (StrictLet x' (rn', e2)) : k, in_e1)
          | otherwise                 -> go (deeds + 1, Heap (M.insert x' (internallyBound in_e1) h) ids',                                      k, (rn', e2))
          where (ids', rn', (x', in_e1)) = renameNonRecBound ids rn (x, e1)
        LetRec xes e     -> go (deeds + 1, Heap (h `M.union` M.fromList [(x', internallyBound in_e) | (x', in_e) <- xes']) ids', k, (rn', e))
          where (ids', rn', xes') = renameBounds ids rn xes
      where tg = annedTag e

    go_question (deeds, h, k, anned_x) = maybe (False, (deeds, h, k, fmap Question anned_x)) (\s -> (True, normalise s)) $ force  deeds h k (annedTag anned_x) (annee anned_x)
    go_answer   (deeds, h, k, anned_a) = maybe (False, (deeds, h, k, fmap Answer anned_a))   (\s -> (True, normalise s)) $ unwind deeds h k (annedTag anned_a) (annee anned_a)

    prepareAnswer :: Deeds
                  -> Out Var -- ^ Name to which the value is bound
                  -> Answer  -- ^ Bound value, which we have *exactly* 1 deed for already that is not recorded in the Deeds itself
                  -> Maybe (Deeds, Answer) -- Outgoing deeds have that 1 latent deed included in them, and we have claimed deeds for the outgoing value
    prepareAnswer deeds x' a
      | dUPLICATE_VALUES_EVALUATOR = fmap (flip (,) a) $ claimDeeds (deeds + 1) (answerSize' a)
       -- Avoid creating indirections to indirections: implements indirection compression
      | (_, (_, Indirect _)) <- a  = return (deeds, a)
      | otherwise                  = return (deeds, (Nothing, (mkIdentityRenaming (unitVarSet x'), Indirect x')))

    -- We have not yet claimed deeds for the result of this function
    lookupAnswer :: Heap -> Out Var -> Maybe (Anned Answer)
    lookupAnswer (Heap h ids) x' = do
        hb <- M.lookup x' h
        case heapBindingTerm hb of
          Just in_e -> termToAnswer ids in_e -- FIXME: it would be cooler if we could exploit cheap non-values in unfoldings as well..
          Nothing   -> Nothing
    
    -- Deal with a variable at the top of the stack
    -- Might have to claim deeds if inlining a non-value non-internally-bound thing here
    -- FIXME: look inside unfoldings
    force :: Deeds -> Heap -> Stack -> Tag -> Out Var -> Maybe UnnormalisedState
    force deeds (Heap h ids) k tg x'
      -- NB: inlining values is non-normalising if dUPLICATE_VALUES_EVALUATOR is on (since doing things the long way would involve executing an update frame)
      | not (dUPLICATE_VALUES_EVALUATOR && normalising)
      , Just anned_a <- lookupAnswer (Heap h ids) x' -- NB: don't unwind *immediately* because we want that changing a Var into a Value in an empty stack is seen as a reduction 'step'
      = do { (deeds, a) <- prepareAnswer deeds x' (annee anned_a); return $ denormalise (deeds, Heap h ids, k, fmap Answer $ annedAnswer (annedTag anned_a) a) }
      | otherwise = do
        hb <- M.lookup x' h
        -- NB: we MUST NOT create update frames for non-concrete bindings!! This has bitten me in the past, and it is seriously confusing. 
        guard (howBound hb == InternallyBound)
        in_e <- heapBindingTerm hb
        return $ case k of
             -- Avoid creating consecutive update frames: implements "stack squeezing"
            kf : _ | Update y' <- tagee kf -> (deeds, Heap (M.insert x' (internallyBound (mkIdentityRenaming (unitVarSet y'), annedTerm (tag kf) (Var y'))) h) ids,                         k, in_e)
            _                              -> (deeds, Heap (M.delete x' h)                                                                                     ids, Tagged tg (Update x') : k, in_e)

    -- Deal with a value at the top of the stack
    unwind :: Deeds -> Heap -> Stack -> Tag -> Answer -> Maybe UnnormalisedState
    unwind deeds h k tg_v in_v = uncons k >>= \(kf, k) -> case tagee kf of
        TyApply ty'                    -> tyApply    (deeds + 1)          h k      in_v ty'
        CoApply co'                    -> coApply    (deeds + 1)          h k      in_v co'
        Apply x2'                      -> apply      deeds       (tag kf) h k      in_v x2'
        Scrutinise x' ty' in_alts      -> scrutinise (deeds + 1)          h k tg_v in_v x' ty' in_alts
        PrimApply pop tys' in_vs in_es -> primop     deeds       (tag kf) h k tg_v pop tys' in_vs in_v in_es
        StrictLet x' in_e2             -> strictLet  (deeds + 1)          h k tg_v in_v x' in_e2
        CastIt co'                     -> cast       deeds       (tag kf) h k      in_v co'
        Update x'
          | normalising, dUPLICATE_VALUES_EVALUATOR -> Nothing -- If duplicating values, we ensure normalisation by not executing updates
          | otherwise                               -> update deeds h k tg_v x' in_v
      where
        -- When derereferencing an indirection, it is important that the resulting value is not stored anywhere. The reasons are:
        --  1) That would cause allocation to be duplicated if we residualised immediately afterwards, because the value would still be in the heap
        --  2) It would cause a violation of the deeds invariant because *syntax* would be duplicate
        --  3) It feels a bit weird because it might turn phantom stuff into real stuff
        --
        -- Indirections do not change the deeds story much (at all). You have to pay a deed per indirection, which is released
        -- whenever the indirection dies in the process of evaluation (e.g. in the function position of an application). The deeds
        -- that the indirection "points to" are not affected by any of this. The exception is if we *retain* any subcomponent
        -- of the dereferenced thing - in this case we have to be sure to claim some deeds for that subcomponent. For example, if we
        -- dereference to get a lambda in our function application we had better claim deeds for the body.
        dereference :: Heap -> Answer -> Answer
        dereference h (mb_co, (rn, Indirect x)) | Just anned_a <- lookupAnswer h (renameId rn x) = dereference h (annee anned_a `castAnswer` mb_co)
        dereference _ a = a
    
        deferenceLambdaish :: Heap -> Answer -> Maybe Answer
        deferenceLambdaish h a@(_, (_, v))
          | normalising, not dUPLICATE_VALUES_EVALUATOR, Indirect _ <- v = Nothing -- If not duplicating values, we ensure normalisation by not executing applications to non-explicit-lambdas
          | otherwise = Just (dereference h a)
    
        tyApply :: Deeds -> Heap -> Stack -> Answer -> Out Type -> Maybe UnnormalisedState
        tyApply deeds h k in_v@(_, (_, v)) ty' = do
            (mb_co, (rn, TyLambda x e_body)) <- deferenceLambdaish h in_v
            fmap (\deeds -> (deeds, h, case mb_co of Nothing -> k; Just (co', tg_co) -> Tagged tg_co (CastIt (co' `mkInstCo` ty')) : k, (insertTypeSubst rn x ty', e_body))) $
                 claimDeeds (deeds + annedValueSize' v) (annedSize e_body)

        coApply :: Deeds -> Heap -> Stack -> Answer -> Out Coercion -> Maybe UnnormalisedState
        coApply deeds h k in_v@(_, (_, v)) apply_co' = do
            (mb_co, (rn, Lambda x e_body)) <- deferenceLambdaish h in_v
            flip fmap (claimDeeds (deeds + annedValueSize' v) (annedSize e_body)) $ \deeds -> case mb_co of
                Nothing           -> (deeds, h, k,                                 (insertCoercionSubst rn x apply_co', e_body))
                Just (co', tg_co) -> (deeds, h, Tagged tg_co (CastIt res_co') : k, (insertCoercionSubst rn x cast_apply_co', e_body))
                  where -- Implements the special case of beta-reduction of cast lambda where the argument is an explicit coercion value.
                        -- You can derive this rule from the rules in "Practical aspects of evidence-based compilation" by combining:
                        --  1. TPush, to move the co' from the lambda to the argument and result (arg_co' and res_co')
                        --  2. The rules in Figure 5, to replace a cast of a coercion value with a simple coercion value
                        --  3. The fact that nth commutes with sym to clean up the result (can be proven from Figure 4)
                        [arg_co', res_co'] = decomposeCo 2 co'
                        [arg_from_co', arg_to_co'] = decomposeCo 2 arg_co'
                        cast_apply_co' = arg_from_co' `mkTransCo` apply_co' `mkTransCo` mkSymCo arg_to_co'

        apply :: Deeds -> Tag -> Heap -> Stack -> Answer -> Out Var -> Maybe UnnormalisedState
        apply deeds tg_v (Heap h ids) k in_v@(_, (_, v)) x' = do
            (mb_co, (rn, Lambda x e_body)) <- deferenceLambdaish (Heap h ids) in_v
            case mb_co of
              Nothing -> fmap (\deeds -> (deeds, Heap h ids, k, (insertIdRenaming rn x x', e_body))) $
                              claimDeeds (deeds + 1 + annedValueSize' v) (annedSize e_body)
              Just (co', tg_co) -> fmap (\deeds -> (deeds, Heap (M.insert y' (internallyBound (renamedTerm e_arg)) h) ids', Tagged tg_co (CastIt res_co') : k, (rn', e_body))) $
                                        claimDeeds (deeds + 1 + annedValueSize' v) (annedSize e_arg + annedSize e_body)
                where (ids', rn', y') = renameNonRecBinder ids rn (x `setIdType` arg_co_from_ty')
                      Pair arg_co_from_ty' _arg_co_to_ty' = coercionKind arg_co'
                      [arg_co', res_co'] = decomposeCo 2 co'
                      e_arg = annedTerm tg_co (annedTerm tg_v (Var x') `Cast` mkSymCo arg_co')

        scrutinise :: Deeds -> Heap -> Stack -> Tag -> Answer -> Out Var -> Out Type -> In [AnnedAlt] -> Maybe UnnormalisedState
        scrutinise deeds0 (Heap h0 ids) k tg_v (mb_co_v, (rn_v, v)) wild' _ty' (rn_alts, alts)
           -- Literals are easy -- we can make the simplifying assumption that the types of literals are
           -- always simple TyCons without any universally quantified type variables.
          | Literal l <- v_deref
          , case mb_co_deref_kind of Nothing -> True; Just (_, _, Pair from_ty' to_ty') -> from_ty' `eqType` to_ty'
          , (deeds2, alt_e):_ <- [(deeds1 + annedAltsSize rest, (rn_alts, alt_e)) | ((LiteralAlt alt_l, alt_e), rest) <- bagContexts alts, alt_l == l]
          = Just (deeds2, Heap h1 ids, k, alt_e)
          
           -- Data is a big stinking mess! I hate you, KPush rule.
          | Data dc tys cos xs <- v_deref
           -- a) Ensure that the coercion on the data (if any) lets us do the reduction, and determine
           --    the appropriate coercions to use (if any) on each value argument to the DataCon
          , Just mb_dc_cos <- case mb_co_deref_kind of
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
                                            gammas = decomposeCo tc_arity co'
                                            theta_subst = liftCoSubstWith (dc_univ_tyvars ++ dc_ex_tyvars)
                                                                          (gammas         ++ map mkReflCo tys)
                                        in map (\arg_ty -> (theta_subst arg_ty, tg_co)) arg_tys -- Use tag from the original coercion everywhere
           -- b) Identify the first appropriate branch of the case and reduce -- apply the discovered coercions if necessary
          , (deeds3, h', ids', alt_e):_ <- [ res
                                           | ((DataAlt alt_dc alt_as alt_qs alt_xs, alt_e), rest) <- bagContexts alts
                                           , alt_dc == dc
                                           , let tys' = map (renameType ids rn_v_deref) tys
                                                 cos' = map (renameCoercion ids rn_v_deref) cos
                                                 xs' = map (renameId rn_v_deref) xs
                                                 rn_alts' = insertTypeSubsts rn_alts (alt_as `zip` tys')
                                                 deeds2 = deeds1 + annedAltsSize rest
                                           , Just res <- [do (deeds3, h', ids', rn_alts') <- case mb_dc_cos of
                                                               Nothing     -> return (deeds2, h1, ids, insertIdRenamings (insertCoercionSubsts rn_alts' (alt_qs `zip` cos')) (alt_xs `zip` xs'))
                                                               Just dc_cos -> foldM (\(deeds, h, ids, rn_alts) (uncast_e_arg', alt_y, (dc_co, tg_co)) ->
                                                                                        let Pair _dc_co_from_ty' dc_co_to_ty' = coercionKind dc_co -- TODO: use to_tc_arg_tys' from above?
                                                                                            (ids', rn_alts', y') = renameNonRecBinder ids rn_alts (alt_y `setIdType` dc_co_to_ty')
                                                                                            e_arg = annedTerm tg_co $ annedTerm tg_v uncast_e_arg' `Cast` dc_co
                                                                                        in fmap (\deeds' -> (deeds', M.insert y' (internallyBound (renamedTerm e_arg)) h, ids', rn_alts')) $ claimDeeds deeds (annedSize e_arg))
                                                                                    (deeds2, h1, ids, rn_alts') (zip3 (map (Value . Coercion) cos' ++ map Var xs') (alt_qs ++ alt_xs) dc_cos)
                                                             return (deeds3, h', ids', (rn_alts', alt_e))]
                                           ]
          = Just (deeds3, Heap h' ids', k, alt_e)
          
           -- Thank god, default alternatives are trivial:
          | (deeds2, alt_e):_ <- [(deeds1 + annedAltsSize rest, (rn_alts, alt_e)) | ((DefaultAlt, alt_e), rest) <- bagContexts alts]
          = Just (deeds2, Heap h1 ids, k, alt_e)
          
           -- This can legitimately occur, e.g. when supercompiling (if x then (case x of False -> 1) else 2)
          | otherwise
          = Nothing
          where (mb_co_deref, (rn_v_deref, v_deref)) = dereference (Heap h0 ids) (mb_co_v, (rn_v, v))
                mb_co_deref_kind = fmap (\(co, tg_co) -> (co, tg_co, coercionKind co)) mb_co_deref
                (deeds1, h1) | isDeadBinder wild' = (deeds0 + annedValueSize' v + maybe 0 (const 1) mb_co_v, h0)
                             | otherwise          = (deeds0,                                                 M.insert wild' wild_hb h0)
                               where wild_hb = case mb_co_v of
                                                 Nothing      -> internallyBound (rn_v, annedTerm tg_v (Value v))
                                                 Just co_tg_v -> internallyBound (renamedTerm $ annedTerm tg_v (castValueToAnnedTerm' co_tg_v (renameAnnedValue' ids rn_v v)))
                               -- NB: we add the *non-dereferenced* value to the heap for a case wildcard, because anything else may duplicate allocation

        primop :: Deeds -> Tag -> Heap -> Stack -> Tag -> PrimOp -> [Out Type] -> [Anned Answer] -> Answer -> [In AnnedTerm] -> Maybe UnnormalisedState
        primop deeds tg_kf heap@(Heap _ ids) k tg_a pop tys' anned_as a [] = do
            guard eVALUATE_PRIMOPS -- NB: this is not faithful to paper 1 because we still turn primop expressions into
                                   -- stack frames.. this is bad because it will impede good specilations (without smart generalisation)
            let as' = map (dereference h) $ map annee anned_as ++ [a]
                tg_kf' = tg_kf { tagOccurrences = if oCCURRENCE_GENERALISATION then tagOccurrences tg_kf + sum (map tagOccurrences (tg_a : map annedTag anned_as)) else 1 }
            a' <- evaluatePrim ids tg_kf' pop tys' as'
            deeds <- claimDeeds (deeds + sum (map annedSize anned_as) + answerSize' a + 1) (annedSize a') -- I don't think this can ever fail
            return (denormalise (deeds, heap, k, fmap Answer a'))
        primop deeds tg_kf h k tg_a pop tys' anned_as a in_es = case in_es of
            (in_e:in_es) -> Just (deeds, h, Tagged tg_kf (PrimApply pop tys' (anned_as ++ [annedAnswer tg_a a]) in_es) : k, in_e)
            []           -> Nothing

        strictLet :: Deeds -> Heap -> Stack -> Tag -> Answer -> Out Var -> In AnnedTerm -> Maybe UnnormalisedState
        strictLet deeds (Heap h ids) k tg_a a x' in_e2 = Just (deeds, Heap (M.insert x' (internallyBound (annedAnswerToInAnnedTerm ids (annedAnswer tg_a a))) h) ids, k, in_e2)

        cast :: Deeds -> Tag -> Heap -> Stack -> Answer -> Coercion -> Maybe UnnormalisedState
        cast deeds tg_kf (Heap h ids) k (mb_co, in_v) co' = Just (deeds', Heap h ids, k, annedAnswerToInAnnedTerm ids (annedAnswer tg_kf ans'))
          where (deeds', ans') = case mb_co of
                    Nothing           -> (deeds,     (Just (co',                tg_kf), in_v))
                    Just (co, _tg_co) -> (deeds + 1, (Just (co `mkTransCo` co', tg_kf), in_v))

        update :: Deeds -> Heap -> Stack -> Tag -> Out Var -> Answer -> Maybe UnnormalisedState
        update deeds (Heap h ids) k tg_a x' a = do
            (deeds', prepared_in_v) <- case prepareAnswer deeds x' a of
                Nothing                      -> pprTrace "update-deeds:" (pPrint x') Nothing
                Just (deeds', prepared_in_v) -> Just (deeds', prepared_in_v)
            return (deeds', Heap (M.insert x' (internallyBound (annedAnswerToInAnnedTerm ids (annedAnswer tg_a a))) h) ids, k, annedAnswerToInAnnedTerm ids (annedAnswer tg_a prepared_in_v))
