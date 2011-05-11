{-# LANGUAGE PatternGuards #-}
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

import qualified Data.Set as S
import qualified Data.Map as M

import qualified CoreSyn as CoreSyn
import Coercion
import PrelRules
import Id
import DataCon


evaluatePrim :: Tag -> PrimOp -> [Answer] -> Maybe (Anned Answer)
evaluatePrim tg pop args = do
    args' <- mapM to args
    (res:_) <- return [res | CoreSyn.BuiltinRule { CoreSyn.ru_nargs = nargs, CoreSyn.ru_try = f }
                          <- primOpRules pop (error "evaluatePrim: dummy primop name")
                      , nargs == length args
                      , Just res <- [f (const CoreSyn.NoUnfolding) args']]
    fmap (annedAnswer tg) $ fro res
  where
    to :: Answer -> Maybe CoreSyn.CoreExpr
    to (mb_co, (rn, v)) = fmap (maybe id (flip CoreSyn.Cast . fst) mb_co) $ case v of
        Literal l  -> Just (CoreSyn.Lit l)
        Data dc xs -> Just (CoreSyn.Var (dataConWrapId dc) `CoreSyn.mkVarApps` map (rename rn) xs)
        _          -> Nothing
    
    fro :: CoreSyn.CoreExpr -> Maybe Answer
    fro (CoreSyn.Cast e co) = fmap (\(mb_co', in_v) -> (Just (maybe co (\(co', _) -> co' `mkTransCo` co) mb_co', tg), in_v)) $ fro e
    fro (CoreSyn.Lit l)     = Just (Nothing, (emptyRenaming, Literal l))
    fro e | CoreSyn.Var f <- e, Just dc <- isDataConId_maybe f = fmap (\xs -> (Nothing, (mkIdentityRenaming (mkVarSet xs), Data dc xs))) $ mapM toVar_maybe es
          | otherwise = Nothing
      where (e, es) = CoreSyn.collectArgs e
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
    (\res@(_reduced, stepped_state) -> ASSERT2(noChange (releaseStateDeed state) (releaseStateDeed stepped_state),
                                               hang (text "step': deeds lost or gained:") 2 (pPrintFullUnnormalisedState state $$ pPrintFullState stepped_state))
                                       ASSERT2(S.isSubsetOf (stateFreeVars stepped_state) (stateFreeVars state),
                                               text "step': FVs" $$ pPrint (stateFreeVars state) $$ pPrintFullUnnormalisedState state $$ pPrint (stateFreeVars stepped_state) $$ pPrintFullState stepped_state)
                                       -- traceRender (text "normalising" $$ nest 2 (pPrintFullUnnormalisedState state) $$ text "to" $$ nest 2 (pPrintFullState stepped_state)) $
                                       res) $
    go state
  where
    go :: (Deeds, Heap, Stack, In AnnedTerm) -> (Bool, State)
    go (deeds, h@(Heap _ ids), k, (rn, e)) 
     | Just anned_a <- termToAnswer ids (rn, e) = go_answer (deeds, h, k, anned_a)
     | otherwise = case annee e of
        Var x             -> go_question (deeds, h, k, fmap (const (rename rn x)) e)
        TyApp e ty        -> go (deeds, h, Tagged tg (TyApply (renameType ids rn ty))                             : k, (rn, e))
        App e x           -> go (deeds, h, Tagged tg (Apply (rename rn x))                                        : k, (rn, e))
        PrimOp pop (e:es) -> go (deeds, h, Tagged tg (PrimApply pop [] (map ((,) rn) es))                         : k, (rn, e))
        Case e x ty alts  -> go (deeds, h, Tagged tg (Scrutinise (rename rn x) (renameType ids rn ty) (rn, alts)) : k, (rn, e))
        Cast e co         -> go (deeds, h, Tagged tg (CastIt (renameCoercion ids rn co))                          : k, (rn, e))
        LetRec xes e      -> go (allocate (deeds + 1) h k (rn, (xes, e)))
        _                 -> panic "reduced" (text "Impossible expression" $$ ppr1 e)
      where tg = annedTag e

    go_question (deeds, h, k, anned_x) = maybe (False, (deeds, h, k, fmap Question anned_x)) (\s -> (True, normalise s)) $ force  deeds h k (annedTag anned_x) (annee anned_x)
    go_answer   (deeds, h, k, anned_a) = maybe (False, (deeds, h, k, fmap Answer anned_a))   (\s -> (True, normalise s)) $ unwind deeds h k (annedTag anned_a) (annee anned_a)

    allocate :: Deeds -> Heap -> Stack -> In ([(Var, AnnedTerm)], AnnedTerm) -> UnnormalisedState
    allocate deeds (Heap h ids) k (rn, (xes, e)) = (deeds, Heap (h `M.union` M.fromList [(x', internallyBound in_e) | (x', in_e) <- xes']) ids', k, (rn', e))
      where (ids', rn', xes') = renameBounds ids rn xes

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
            _                              -> (deeds, Heap (M.delete x' h)                                                                          ids, Tagged tg (Update x') : k, in_e)

    -- Deal with a value at the top of the stack
    unwind :: Deeds -> Heap -> Stack -> Tag -> Answer -> Maybe UnnormalisedState
    unwind deeds h k tg_v in_v = uncons k >>= \(kf, k) -> case tagee kf of
        TyApply ty'               -> tyApply    (deeds + 1)          h k      in_v ty'
        Apply x2'                 -> apply      deeds       (tag kf) h k      in_v x2'
        Scrutinise x' ty' in_alts -> scrutinise (deeds + 1)          h k tg_v in_v x' ty' in_alts
        PrimApply pop in_vs in_es -> primop     deeds       (tag kf) h k tg_v pop in_vs in_v in_es
        CastIt co'                -> cast       deeds       (tag kf) h k      in_v co'
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
        dereference h (mb_co, (rn, Indirect x)) | Just anned_a <- lookupAnswer h (rename rn x) = dereference h (annee anned_a `castAnswer` mb_co)
        dereference _ a = a
    
        deferenceLambdaish :: Heap -> Answer -> Maybe Answer
        deferenceLambdaish h a@(_, (_, v))
          | normalising, not dUPLICATE_VALUES_EVALUATOR, Indirect _ <- v = Nothing -- If not duplicating values, we ensure normalisation by not executing applications to non-explicit-lambdas
          | otherwise = Just (dereference h a)
    
        tyApply :: Deeds -> Heap -> Stack -> Answer -> Out Type -> Maybe UnnormalisedState
        tyApply deeds h k in_v@(_, (_, v)) ty' = do
            (mb_co, (rn, TyLambda x e_body)) <- deferenceLambdaish h in_v
            fmap (\deeds -> (deeds, h, case mb_co of Nothing -> k; Just (co', tg_co) -> Tagged tg_co (Coerce (co' `mkInstCo` ty')) : k, (insertTypeSubst rn x ty', e_body))) $
                 claimDeeds (deeds + annedValueSize' v) (annedSize e_body)

        apply :: Deeds -> Tag -> Heap -> Stack -> Answer -> Out Var -> Maybe UnnormalisedState
        apply deeds tg_v (Heap h ids) k in_v@(_, (_, v)) x' = do
            (mb_co, (rn, Lambda x e_body)) <- deferenceLambdaish (Heap h ids) in_v
            case mb_co of
              Nothing -> fmap (\deeds -> (deeds, Heap h ids, k, (insertRenaming rn x x', e_body))) $
                              claimDeeds (deeds + 1 + annedValueSize' v) (annedSize e_body)
              Just (co', tg_co) -> fmap (\deeds -> (deeds, Heap (M.insert y' e_arg h) ids', Tagged tg_co (Cast res_co') : k)) $
                                        claimDeeds (deeds + 1 + annedValueSize' v) (annedSize e_arg + annedSize e_body)
                where (ids', rn', [y']) = renameNonRecBinders ids rn [x `setVarType` arg_co_from_ty']) $
                      (arg_co_from_ty', _arg_co_to_ty') = coercionKind arg_co'
                      [arg_co', res_co'] = decomposeCo 2 co'
                      e_arg = annedTerm tg_co (annedTerm tg_v (Var x') `Cast` mkSymCo arg_co')

        scrutinise :: Deeds -> Heap -> Stack -> Tag -> Answer -> Out Var -> Out Type -> In [AnnedAlt] -> Maybe UnnormalisedState
        scrutinise deeds (Heap h ids) k tg_v (rn_v, v) x' ty' (rn_alts, alts)
          | Literal l <- v_deref
          , (alt_e, rest):_ <- [((rn_alts, alt_e), rest) | ((LiteralAlt alt_l, alt_e), rest) <- bagContexts alts, alt_l == l]
          = Just (deeds + annedValueSize' v + annedAltsSize rest, Heap h ids, k, alt_e)
          | Data dc xs <- v_deref
          , (alt_e, rest):_ <- [((insertRenamings (alt_xs `zip` map (rename rn_v_deref) xs) rn_alts, alt_e), rest) | ((DataAlt alt_dc alt_xs, alt_e), rest) <- bagContexts alts, alt_dc == dc]
          = Just (deeds + annedValueSize' v + annedAltsSize rest, Heap h ids, k, alt_e)
          | ((mb_alt_x, alt_e), rest):_ <- [((mb_alt_x, alt_e), rest) | ((DefaultAlt mb_alt_x, alt_e), rest) <- bagContexts alts]
          = Just $ case mb_alt_x of
                     Nothing    -> (deeds + annedValueSize' v + annedAltsSize rest, Heap h                                                                            ids,  k, (rn_alts,  alt_e))
                     Just alt_x -> (deeds +                     annedAltsSize rest, Heap (M.insert alt_x' (internallyBound (rn_v, annedTerm tg_v $ Value v)) h) ids', k, (rn_alts', alt_e))
                       where (ids', rn_alts', [alt_x']) = renameNonRecBinders ids rn_alts [alt_x]
                             -- NB: we add the *non-dereferenced* value to the heap in a default branch with variable, because anything else may duplicate allocation
          | otherwise
          = Nothing -- This can legitimately occur, e.g. when supercompiling (if x then (case x of False -> 1) else 2)
          where (mb_co_deref, (rn_v_deref, v_deref)) = dereference (Heap h ids) (rn_v, v)

        primop :: Deeds -> Tag -> Heap -> Stack -> Tag -> PrimOp -> [Anned Answer] -> Answer -> [In AnnedTerm] -> Maybe UnnormalisedState
        primop deeds tg_kf h k tg_a pop anned_as a [] = do
            guard eVALUATE_PRIMOPS -- NB: this is not faithful to paper 1 because we still turn primop expressions into
                                   -- stack frames.. this is bad because it will impede good specilations (without smart generalisation)
            let as' = map (dereference h) $ map annee anned_as ++ [a]
                tg_kf' = tg_kf { tagOccurrences = if oCCURRENCE_GENERALISATION then tagOccurrences tg_kf + sum (map tagOccurrences (tg_a : map annedTag anned_as)) else 1 }
            a' <- evaluatePrim tg_kf' pop as'
            deeds <- claimDeeds (deeds + sum (map annedSize anned_as) + answerSize' a + 1) (annedSize a') -- I don't think this can ever fail
            return (denormalise (deeds, h, k, fmap Answer a'))
        primop deeds tg_kf h k tg_a pop anned_as a (in_e:in_es) = Just (deeds, h, Tagged tg_kf (PrimApply pop (anned_as ++ [annedAnswer tg_a a]) in_es) : k, in_e)

        cast :: Deeds -> Tag -> Heap -> Stack -> Answer -> Coercion -> Maybe UnnormalisedState
        cast deeds tg_kf (Heap h ids) k (mb_co, in_v) co' = Just (deeds', Heap h ids, k, annedAnswerToAnnedTerm ids (annedAnswer tg_kf ans'))
          where (deeds', ans') = case mb_co of
                    Nothing          -> (deeds,     (Just (co',                tg_kf), in_v))
                    Just (co, tg_co) -> (deeds + 1, (Just (co `mkTransCo` co', tg_kf), in_v))

        update :: Deeds -> Heap -> Stack -> Tag -> Out Var -> Answer -> Maybe UnnormalisedState
        update deeds (Heap h ids) k tg_a x' a = do
            (deeds', prepared_in_v) <- case prepareAnswer deeds x' a of
                Nothing                      -> pprTrace "update-deeds:" (pPrint x') Nothing
                Just (deeds', prepared_in_v) -> Just (deeds', prepared_in_v)
            return (deeds', Heap (M.insert x' (internallyBound (annedAnswerToAnnedTerm ids (annedAnswer tg_a a))) h) ids, k, annedAnswerToAnnedTerm ids (annedAnswer tg_a prepared_in_v))
