{-# LANGUAGE PatternGuards #-}
module Supercompile.Evaluator.Evaluate (normalise, step) where

#include "HsVersions.h"

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.FreeVars
--import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.StaticFlags
import Supercompile.Utilities

import qualified Data.Set as S
import qualified Data.Map as M

import qualified CoreSyn as CoreSyn
import PrelRules
import Id
import DataCon


evaluatePrim :: PrimOp -> [AnnedValue] -> Maybe AnnedValue
evaluatePrim pop args = do
    args' <- mapM to args
    (res:_) <- [res | CoreSyn.BuiltinRule { CoreSyn.ru_nargs = nargs, CoreSyn.ru_try = f }
                        <- primOpRules pop (error "evaluatePrim: dummy primop name")
                    , nargs == length args
                    , Just res <- [f (const CoreSyn.NoUnfolding) args']]
    return res
  where
    to :: AnnedValue -> Maybe CoreSyn.CoreExpr
    to (mb_co, rv) = fmap (maybe id (flip CoreSyn.Cast) mb_co) $ case rv of
        Literal l  -> Just (CoreSyn.Lit l)
        Data dc xs -> Just (CoreSyn.Var (dataConWrapId dc) `CoreSyn.mkVarApps` xs)
        _          -> Nothing
    
    fro :: CoreSyn.CoreExpr -> Maybe Coercion -> Maybe AnnedValue
    fro (CoreSyn.Cast e co) = fmap (\(mb_co', rv) -> (Just $ maybe co (`mkTransCoercion` co) mb_co', rv)) $ fro e
    fro (CoreSyn.Lit l)     = Just (Literal l)
    fro e | CoreSyn.Var f <- e, Just dc <- isDataConId_maybe f = fmap (Data dc) $ mapM toVar_maybe es
          | otherwise = Nothing
      where (e, es) = CoreSyn.collectArgs e
            toVar_maybe (CoreSyn.Var x) = Just x
            toVar_maybe _               = Nothing


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
    (\res@(_reduced, state') -> ASSERT(noChange (releaseStateDeed state) (releaseStateDeed state'), 
                                       hang (text "step': deeds lost or gained:") 2 (pPrint state $$ pPrint state'))
                                ASSERT(stateFreeVars state' `S.isSubsetOf` stateFreeVars state,
                                       text "step': FVs" $$ pPrint (stateFreeVars state) $$ pPrint state $$ pPrint (stateFreeVars state') $$ pPrint state')
                                -- traceRender (text "normalising" $$ nest 2 (pPrintFullUnnormalisedState state) $$ text "to" $$ nest 2 (pPrintFullState state')) $
                                res) $
    go state
  where
    go (deeds, h@(Heap _ ids), k, (rn, e)) = case annee e of
        Var x             -> maybe (False, (deeds, h, k, (rn, fmap (const (Question x)) e))) (\s -> (True, normalise s)) $ force  deeds h k tg (rename rn x);
        Value v           -> maybe (False, (deeds, h, k, (rn, fmap (const (Answer v)) e)))   (\s -> (True, normalise s)) $ unwind deeds h k tg (rn, v)
        TyApp e ty        -> go (deeds, h, Tagged tg (TyApply (renameType ids rn ty))  : k, (rn, e))
        App e x           -> go (deeds, h, Tagged tg (Apply (rename rn x))             : k, (rn, e))
        PrimOp pop []     -> panic "reduced" (text "Nullary primop" <+> pPrint pop <+> text "in input")
        PrimOp pop (e:es) -> go (deeds, h, Tagged tg (PrimApply pop [] (map (rn,) es)) : k, (rn, e))
        Case e alts       -> go (deeds, h, Tagged tg (Scrutinise (rn, alts))           : k, (rn, e))
        Cast e co         -> go (deeds, h, Tagged tg (CastIt (renameType ids rn co))   : k, (rn, e))
        LetRec xes e      -> go (allocate (deeds + 1) h k (rn, (xes, e)))
      where tg = annedTag e

    allocate :: Deeds -> Heap -> Stack -> In ([(Var, AnnedTerm)], AnnedTerm) -> UnnormalisedState
    allocate deeds (Heap h ids) k (rn, (xes, e)) = (deeds, Heap (h `M.union` M.fromList [(x', internallyBound in_e) | (x', in_e) <- xes']) ids', k, (rn', e))
      where (ids', rn', xes') = renameBounds (\_ x' -> x') ids rn xes

    prepareValue :: Deeds
                 -> Out Var       -- ^ Name to which the value is bound
                 -> In AnnedValue -- ^ Bound value, which we have *exactly* 1 deed for already that is not recorded in the Deeds itself
                 -> Maybe (Deeds, In AnnedValue) -- Outgoing deeds have that 1 latent deed included in them, and we have claimed deeds for the outgoing value
    prepareValue deeds x' in_v@(_, v)
      | dUPLICATE_VALUES_EVALUATOR = fmap (,in_v) $ claimDeeds (deeds + 1) (annedValueSize' v)
       -- Avoid creating indirections to indirections: implements indirection compression
      | Indirect _ <- v            = return (deeds, in_v)
      | otherwise                  = return (deeds, (mkIdentityRenaming [x'], Indirect x'))

    -- We have not yet claimed deeds for the result of this function
    lookupValue :: Heap -> Out Var -> Maybe (In (Anned AnnedValue))
    lookupValue (Heap h _) x' = do
        hb <- M.lookup x' h
        case heapBindingTerm hb of
          Just  (rn, anned_e) -> fmap (rn,) $ termToValue anned_e -- FIXME: it would be cooler if we could exploit cheap non-values in unfoldings as well..
          Nothing             -> Nothing
    
    -- Deal with a variable at the top of the stack
    -- Might have to claim deeds if inlining a non-value non-internally-bound thing here
    force deeds (Heap h ids) k tg x'
      -- NB: inlining values is non-normalising if dUPLICATE_VALUES_EVALUATOR is on (since doing things the long way would involve executing an update frame)
      | not (dUPLICATE_VALUES_EVALUATOR && normalising)
      , Just (rn, anned_v) <- lookupValue (Heap h ids) x' -- NB: don't unwind *immediately* because we want that changing a Var into a Value in an empty stack is seen as a reduction 'step'
      = do { (deeds, (rn, v)) <- prepareValue deeds x' (rn, annee anned_v); return (deeds, Heap h ids, k, (rn, annedTerm (annedTag anned_v) (Value v))) }
      | otherwise = do
        hb <- M.lookup x' h
        -- NB: we MUST NOT create update frames for non-concrete bindings!! This has bitten me in the past, and it is seriously confusing. 
        guard (howBound hb == InternallyBound)
        in_e <- heapBindingTerm hb
        return $ case k of
             -- Avoid creating consecutive update frames: implements "stack squeezing"
            kf : _ | Update y' <- tagee kf -> (deeds, Heap (M.insert x' (internallyBound (mkIdentityRenaming [y'], annedTerm (tag kf) (Var y'))) h) ids,                         k, in_e)
            _                              -> (deeds, Heap (M.delete x' h)                                                                          ids, Tagged tg (Update x') : k, in_e)

    -- Deal with a value at the top of the stack
    unwind :: Deeds -> Heap -> Stack -> Tag -> In AnnedValue -> Maybe UnnormalisedState
    unwind deeds h k tg_v in_v = uncons k >>= \(kf, k) -> case tagee kf of
        TyApply ty'               -> tyApply    (deeds + 1)          h k      in_v ty'
        Apply x2'                 -> apply      (deeds + 1)          h k      in_v x2'
        Scrutinise in_alts        -> scrutinise (deeds + 1)          h k tg_v in_v in_alts
        PrimApply pop in_vs in_es -> primop     deeds       (tag kf) h k tg_v pop in_vs in_v in_es
        CastIt co'                -> cast       (deeds + 1)          h k      in_v co'
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
        dereference :: Heap -> In AnnedValue -> In AnnedValue
        dereference h (rn, Indirect x) | Just (rn', anned_v') <- lookupValue h (safeRename "dereference" rn x) = dereference h (rn', annee anned_v')
        dereference _ in_v = in_v
    
        deferenceLambdaish :: Heap -> In AnnedValue -> Maybe (In AnnedValue)
        deferenceLambdaish h in_v@(_, v)
          | normalising, not dUPLICATE_VALUES_EVALUATOR, Indirect _ <- v = Nothing -- If not duplicating values, we ensure normalisation by not executing applications to non-explicit-lambdas
          | otherwise = Just (dereference h in_v)
    
        tyApply :: Deeds -> Heap -> Stack -> In AnnedValue -> Out Type -> Maybe UnnormalisedState
        tyApply deeds h k in_v@(_, v) ty' = do
            (rn, TyLambda x e_body) <- deferenceLambdaish h in_v
            fmap (\deeds -> (deeds, h, k, (insertTypeSubst x ty' rn, e_body))) $ claimDeeds (deeds + annedValueSize' v) (annedSize e_body)

        apply :: Deeds -> Heap -> Stack -> In AnnedValue -> Out Var -> Maybe UnnormalisedState
        apply deeds h k in_v@(_, v) x' = do
            (rn, Lambda x e_body) <- deferenceLambdaish h in_v
            fmap (\deeds -> (deeds, h, k, (insertRenaming x x' rn, e_body))) $ claimDeeds (deeds + annedValueSize' v) (annedSize e_body)

        scrutinise :: Deeds -> Heap -> Stack -> Tag -> In AnnedValue -> In [AnnedAlt] -> Maybe UnnormalisedState
        scrutinise deeds (Heap h ids) k tg_v (rn_v, v)  (rn_alts, alts)
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
                       where (ids', rn_alts', alt_x') = renameBinder ids rn_alts alt_x
                             -- NB: we add the *non-dereferenced* value to the heap in a default branch with variable, because anything else may duplicate allocation
          | otherwise
          = Nothing -- This can legitimately occur, e.g. when supercompiling (if x then (case x of False -> 1) else 2)
          where (rn_v_deref, v_deref) = dereference (Heap h ids) (rn_v, v)

        primop :: Deeds -> Tag -> Heap -> Stack -> Tag -> PrimOp -> [In (Anned AnnedValue)] -> In AnnedValue -> [In AnnedTerm] -> Maybe UnnormalisedState
        primop deeds tg_kf h k tg_v2 pop [(rn_v1, anned_v1)] (rn_v2, v2) []
          | not eVALUATE_PRIMOPS -- NB: this is not faithful to paper 1 because we still turn primop expressions into stack frames.. this is bad because it will impede good specilations (without smart generalisation)
          = Nothing
          | (_, Literal (Int l1)) <- dereference h (rn_v1, annee anned_v1)
          , (_, Literal (Int l2)) <- dereference h (rn_v2, v2)
          , Just v <- f pop l1 l2
          -- , traceRender ("primop", length k, tagOccurrences tg_kf + tagOccurrences tg_v2 + tagOccurrences (annedTag anned_v1)) $ True
          , let e' = annedTerm (tg_kf { tagOccurrences = if oCCURRENCE_GENERALISATION then tagOccurrences tg_kf + tagOccurrences tg_v2 + tagOccurrences (annedTag anned_v1) else 1 }) (Value v)
          , Just deeds <- claimDeeds (deeds + annedSize anned_v1 + annedValueSize' v2 + 1) (annedSize e') -- I don't think this can ever fail
          = Just (deeds, h, k, (emptyRenaming, e'))
          | otherwise
          = Nothing -- Can occur legitimately if some of the arguments of the primop are just indirections to nothing or irreducible (division by zero?)
          where f pop = case pop of Add -> retInt (+); Subtract -> retInt (-);
                                    Multiply -> retInt (*); Divide -> \l1 l2 -> guard (l2 /= 0) >> retInt div l1 l2; Modulo -> retInt mod;
                                    Equal -> retBool (==); LessThan -> retBool (<); LessThanEqual -> retBool (<=)
                retInt  pop l1 l2 = Just $ Literal (Int (pop l1 l2))
                retBool pop l1 l2 = Just $ if pop l1 l2 then Data trueDataCon [] else Data falseDataCon []
        primop deeds tg_kf h k tg_v pop in_vs (rn, v) (in_e:in_es) = Just (deeds, h, Tagged tg_kf (PrimApply pop (in_vs ++ [(rn, annedValue tg_v v)]) in_es) : k, in_e)
        primop _     _     _ _ _    _   _     _       _            = Nothing -- I don't think this can occur legitimately

        cast :: Deeds -> Heap -> Stack -> In AnnedValue -> Coercion -> Maybe UnnormalisedState
        cast deeds h k (rn, (mb_co, rv)) co' = 

        update :: Deeds -> Heap -> Stack -> Tag -> Out Var -> In AnnedValue -> Maybe UnnormalisedState
        update deeds (Heap h ids) k tg_v x' in_v@(rn, v) = do
            (deeds', prepared_in_v) <- case prepareValue deeds x' in_v of
                Nothing                      -> trace (render (text "update-deeds:" <+> pPrint x')) Nothing
                Just (deeds', prepared_in_v) -> Just (deeds', prepared_in_v)
            return (deeds', Heap (M.insert x' (internallyBound (rn, annedTerm tg_v (Value v))) h) ids, k, second (annedTerm tg_v . Value) prepared_in_v)
