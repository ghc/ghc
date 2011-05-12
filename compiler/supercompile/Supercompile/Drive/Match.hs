{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Supercompile.Drive.Match (match) where

#include "HsVersions.h"

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Syntax

import Supercompile.Utilities hiding (guard)

import qualified Data.Map as M
import qualified Data.Set as S


--newtype Match a = Match { unMatch :: Either String a }
newtype Match a = Match { unMatch :: Maybe a }

instance Functor Match where
    fmap = liftM

instance Monad Match where
    return = Match . return
    mx >>= fxmy = Match $ unMatch mx >>= (unMatch . fxmy)
    --fail s = Match $ Left s
    fail s = Match $ fail s

guard :: String -> Bool -> Match ()
guard _   True  = return ()
guard msg False = fail msg

runMatch :: Match a -> Maybe a
-- runMatch (Match (Right x))  = Just x
-- runMatch (Match (Left msg)) = trace ("match " ++ msg) Nothing
runMatch = unMatch

matchInScopeSet :: (a -> FreeVars) -> a -> a -> InScopeSet
matchInScopeSet f x y = mkInScopeSet (f x `unionVarSet` f y)

-- instance MonadPlus Match where
--     mzero = fail "mzero"
--     mx1 `mplus` mx2 = Match $ unMatch mx1 `mplus` unMatch mx2


match :: State -- ^ Tieback semantics
      -> State -- ^ This semantics
      -> Maybe Renaming -- ^ Renaming from left to right
match (_deeds_l, Heap h_l _, k_l, qa_l) (_deeds_r, Heap h_r _, k_r, qa_r) = -- (\res -> traceRender ("match", M.keysSet h_l, residualiseDriveState (Heap h_l prettyIdSupply, k_l, in_e_l), M.keysSet h_r, residualiseDriveState (Heap h_r prettyIdSupply, k_r, in_e_r), res) res) $
  runMatch $ do
    free_eqs1 <- matchAnned (matchQA (qaFreeVars qa_l qa_r)) qa_l qa_r
    (bound_eqs, free_eqs2) <- matchEC k_l k_r
    matchHeap h_l h_r (bound_eqs, free_eqs1 ++ free_eqs2)

matchAnned :: (In a -> In a -> b)
           -> In (Anned a) -> In (Anned a) -> b
matchAnned f (rn_l, e_l) (rn_r, e_r) = f (rn_l, annee e_l) (rn_r, annee e_r)

matchQA :: InScopeSet -> QA -> QA -> Match [(Var, Var)]
matchQA _   (Question x_l') (Question x_r') = return [matchVar x_l' x_r']
matchQA ids (Answer in_v_l) (Answer in_v_r) = matchInValue ids in_v_l in_v_r
matchQA _ _ _ = fail "matchQA"

matchInTerm :: InScopeSet -> In AnnedTerm -> In AnnedTerm -> Match [(Var, Var)]
matchInTerm ids = matchAnned (matchInTerm' ids)

matchInTerm' :: InScopeSet -> In (TermF Anned) -> In (TermF Anned) -> Match [(Var, Var)]
matchInTerm' _   (rn_l, Var x_l)           (rn_r, Var x_r)           = return [matchInVar (rn_l, x_l) (rn_r, x_r)]
matchInTerm' ids (rn_l, Value v_l)         (rn_r, Value v_r)         = matchInValue ids (rn_l, v_l) (rn_r, v_r)
matchInTerm' ids (rn_l, App e_l x_l)       (rn_r, App e_r x_r)       = matchInTerm ids (rn_l, e_l) (rn_r, e_r) >>= \eqs -> return (matchInVar (rn_l, x_l) (rn_r, x_r) : eqs)
matchInTerm' ids (rn_l, PrimOp pop_l es_l) (rn_r, PrimOp pop_r es_r) = guard "matchInTerm: primop" (pop_l == pop_r) >> matchInList (matchInTerm ids) (rn_l, es_l) (rn_r, es_r)
matchInTerm' ids (rn_l, Case e_l alts_l)   (rn_r, Case e_r alts_r)   = liftM2 (++) (matchInTerm ids (rn_l, e_l) (rn_r, e_r)) (matchInAlts ids (rn_l, alts_l) (rn_r, alts_r))
matchInTerm' ids (rn_l, LetRec xes_l e_l)  (rn_r, LetRec xes_r e_r)  = matchInTerm ids'' (rn_l', e_l) (rn_r', e_r) >>= \eqs -> matchLetRecs ids'' eqs xes_l' xes_r'
  where (ids',  rn_l', xes_l') = renameBounds (\_ x' -> x') ids  rn_l xes_l
        (ids'', rn_r', xes_r') = renameBounds (\_ x' -> x') ids' rn_r xes_r
matchInTerm' _ _ _ = fail "matchInTerm'"

matchInValue :: InScopeSet -> In AnnedValue -> In AnnedValue -> Match [(Var, Var)]
matchInValue _   (rn_l, Indirect x_l)   (rn_r, Indirect x_r)   = return [matchInVar (rn_l, x_l) (rn_r, x_r)]
matchInValue ids (rn_l, Lambda x_l e_l) (rn_r, Lambda x_r e_r) = matchInTerm ids'' (rn_l', e_l) (rn_r', e_r) >>= \eqs -> matchRigidBinders [(x_l', x_r')] eqs
  where (ids',  rn_l', [x_l']) = renameNonRecBinders ids  rn_l [x_l]
        (ids'', rn_r', [x_r']) = renameNonRecBinders ids' rn_r [x_r]
matchInValue _   (rn_l, Data dc_l xs_l) (rn_r, Data dc_r xs_r) = guard "matchInValue: datacon" (dc_l == dc_r) >> matchInVars (rn_l, xs_l) (rn_r, xs_r)
matchInValue _   (_,    Literal l_l)    (_,    Literal l_r)    = guard "matchInValue: literal" (l_l == l_r) >> return []
matchInValue _ _ _ = fail "matchInValue"

matchInAlts :: InScopeSet -> In [AnnedAlt] -> In [AnnedAlt] -> Match [(Var, Var)]
matchInAlts ids (rn_l, alts_l) (rn_r, alts_r) = fmap concat $ zipWithEqualM (matchInAlt ids) (map ((,) rn_l) alts_l) (map ((,) rn_r) alts_r)

matchInAlt :: InScopeSet -> In AnnedAlt -> In AnnedAlt -> Match [(Var, Var)]
matchInAlt ids (rn_l, (alt_con_l, alt_e_l)) (rn_r, (alt_con_r, alt_e_r)) = matchAltCon alt_con_l' alt_con_r' >>= \binders -> matchInTerm ids'' (rn_l', alt_e_l) (rn_r', alt_e_r) >>= \eqs -> matchRigidBinders binders eqs
  where (ids',  rn_l', alt_con_l') = renameAltCon ids  rn_l alt_con_l
        (ids'', rn_r', alt_con_r') = renameAltCon ids' rn_r alt_con_r

matchAltCon :: AltCon -> AltCon -> Match [(Var, Var)]
matchAltCon (DataAlt dc_l xs_l) (DataAlt dc_r xs_r) = guard "matchAltCon: datacon" (dc_l == dc_r) >> return (xs_l `zip` xs_r)
matchAltCon (LiteralAlt l_l)    (LiteralAlt l_r)    = guard "matchAltCon: literal" (l_l == l_r) >> return []
matchAltCon (DefaultAlt mb_x_l) (DefaultAlt mb_x_r) = matchMaybe matchVar mb_x_l mb_x_r
matchAltCon _ _ = fail "matchAltCon"

matchVar :: Out Var -> Out Var -> (Var, Var)
matchVar x_l' x_r' = (x_l', x_r')

matchInVar :: In Var -> In Var -> (Var, Var)
matchInVar (rn_l, x_l) (rn_r, x_r) = (rename rn_l x_l, rename rn_r x_r)

matchInVars :: In [Var] -> In [Var] -> Match [(Var, Var)]
matchInVars = matchInList (\x_l' x_r' -> return [matchInVar x_l' x_r'])

matchInList :: (In a -> In a -> Match [(Var, Var)])
            -> In [a] -> In [a] -> Match [(Var, Var)]
matchInList match (rn_l, xs_l) (rn_r, xs_r) = fmap concat $ zipWithEqualM match (map ((,) rn_l) xs_l) (map ((,) rn_r) xs_r)

matchList :: (a -> a -> Match [(Var, Var)])
          -> [a] -> [a] -> Match [(Var, Var)]
matchList match xs_l xs_r = fmap concat (zipWithEqualM match xs_l xs_r)

matchMaybe :: (a -> a -> (Var, Var))
           -> Maybe a -> Maybe a -> Match [(Var, Var)]
matchMaybe _ Nothing    Nothing    = return []
matchMaybe f (Just x_l) (Just x_r) = return [f x_l x_r]
matchMaybe _ _ _ = fail "matchMaybe"

matchEC :: Stack -> Stack -> Match ([(Var, Var)], [(Var, Var)])
matchEC k_l k_r = fmap combine $ zipWithEqualM matchECFrame k_l k_r
  where combine = (concat *** concat) . unzip

matchECFrame :: Tagged StackFrame -> Tagged StackFrame -> Match ([(Var, Var)], [(Var, Var)])
matchECFrame kf_l kf_r = matchECFrame' (tagee kf_l) (tagee kf_r)

matchECFrame' :: StackFrame -> StackFrame -> Match ([(Var, Var)], [(Var, Var)])
matchECFrame' (Apply x_l')                      (Apply x_r')                      = return ([], [matchVar x_l' x_r'])
matchECFrame' (Scrutinise in_alts_l)            (Scrutinise in_alts_r)            = fmap ((,) []) $ matchInAlts (matchInScopeSet (inFreeVars annedAltsFreeVars) in_alts_l in_alts_r) in_alts_l in_alts_r
matchECFrame' (PrimApply pop_l in_vs_l in_es_l) (PrimApply pop_r in_vs_r in_es_r) = fmap ((,) []) $ guard "matchECFrame': primop" (pop_l == pop_r) >> liftM2 (++) (matchList (\in_v_l in_v_r -> matchAnned (matchInValue (matchInScopeSet (inFreeVars annedValueFreeVars) in_v_l in_v_r)) in_v_l in_v_r) in_vs_l in_vs_r) (matchList (\in_e_l in_e_r -> matchInTerm (mkInScopeSet (inFreeVars annedTermFreeVars) in_e_l in_e_r) in_e_l in_e_r) in_es_l in_es_r)
matchECFrame' (Update x_l')                     (Update x_r')                     = return ([matchVar x_l' x_r'], [])
matchECFrame' _ _ = fail "matchECFrame'"

matchRigidBinders :: [(Var, Var)] -> [(Var, Var)] -> Match [(Var, Var)]
matchRigidBinders bound_eqs eqs = do
    occursCheck bound_eqs eqs
    return $ filter (`notElem` bound_eqs) eqs

-- The occurs check is trigged by one of these two situations:
--   x |-> Just y_l;  (update y_l)<x> `match` x |-> Just free; (update y_r)<x>   Can't instantiate y_l with free since its not a template var
--   x |-> Just tmpl; (update y_l)<x> `match` x |-> Just y_r;  (update y_r)<x>   Can't instantiate tmpl with y_r since y_r is bound locally
occursCheck :: [(Var, Var)] -> [(Var, Var)] -> Match ()
occursCheck bound_eqs eqs = guard "occursCheck" $ not $ any (\(x_l, x_r) -> any (\(bound_x_l, bound_x_r) -> (x_l == bound_x_l) /= (x_r == bound_x_r)) bound_eqs) eqs

-- NB: if there are dead bindings in the left PureHeap then the output Renaming will not contain a renaming for their binders.
matchHeap :: PureHeap -> PureHeap -> ([(Var, Var)], [(Var, Var)]) -> Match Renaming
matchHeap init_h_l init_h_r (bound_eqs, free_eqs) = do
    -- 1) Find the initial matching by simply recursively matching used bindings from the Left
    --    heap against those from the Right heap (if any)
    eqs <- matchEnvironmentExact (matchInScopeSet (snd . pureHeapVars) init_h_l init_h_r) bound_eqs free_eqs init_h_l init_h_r
    -- 2) Perhaps we violate the occurs check?
    occursCheck bound_eqs eqs
    -- 3) If the left side var was free, we might have assumed two different corresponding rights for it. This is not necessarily a problem:
    --      a |-> True; ()<(a, a)> `match` c |-> True; d |-> True; ()<(c, d)>
    --      a |-> True; ()<(a, a)> `match` c |-> True; d |-> c; ()<(c, d)>
    -- However, I'm going to reject this for now (simpler).
    safeMkRenaming eqs

--- Returns a renaming from the list only if the list maps a "left" variable to a unique "right" variable
safeMkRenaming :: [(Var, Var)] -> Match Renaming
safeMkRenaming eqs = guard "safeMkRenaming" (all (\(x_l, x_r) -> rename rn x_l == x_r) eqs) >> return rn
  where rn = mkRenaming eqs


matchLetRecs :: InScopeSet -> [(Var, Var)] -> [(Var, In AnnedTerm)] -> [(Var, In AnnedTerm)] -> Match [(Var, Var)]
matchLetRecs ids'' eqs xes_l' xes_r' = matchEnvironmentExact ids'' [] eqs (M.fromList [(x', internallyBound in_e) | (x', in_e) <- xes_l']) (M.fromList [(x', internallyBound in_e) | (x', in_e) <- xes_r'])

matchEnvironmentExact :: InScopeSet -> [(Var, Var)] -> [(Var, Var)] -> PureHeap -> PureHeap -> Match [(Var, Var)]
matchEnvironmentExact ids bound_eqs free_eqs init_h_l init_h_r = do
    -- 1) Find the initial matching by simply recursively matching used bindings from the Left
    --    heap against those from the Right heap (if any).
    eqs <- matchEnvironment ids bound_eqs free_eqs init_h_l init_h_r
    -- 2) The outgoing equalities should only relate x_l's that are not bound by init_h_l
    --    because we don't want the local bound variables I've generated from the initial InScopeSet "leaking" upwards.
    --    (I think this reason is now redundant, but actually we still need to make sure that we only output equalities
    --     on *free variables* of the two heaps, not any bound members).
    --    NB: Because some variables may be bound by update frames in the stack, we need to filter out those too...
    let (bound_xs_l, bound_xs_r) = unzip bound_eqs
        internally_bound_l = fst (pureHeapVars init_h_l) InternallyBound `S.union` S.fromList bound_xs_l
    eqs <- --traceRender ("matchEnvironmentExact", eqs, bound_eqs, init_h_l, init_h_r) $
           return $ filter (\(x_l, _x_r) -> x_l `S.notMember` internally_bound_l) eqs
    -- 3) Now the problem is that there might be some bindings in the Right heap that are referred
    --    to by eqs. We want an exact match, so we can't allow that.
    --
    -- We *could* do away with this check, but then we might might match e.g. a LambdaBound var on the left against a InternallyBound
    -- one on the right. At the moment I'm not able to actually synthesize the required unfolding in the caller (TODO), so I must prevent this.
    let internally_bound_r = fst (pureHeapVars init_h_r) InternallyBound `S.union` S.fromList bound_xs_r
    guard "matchEnvironmentExact" $ all (\(_x_l, x_r) -> x_r `S.notMember` internally_bound_r) eqs
    -- 4) We now know that all of the variables bound by both init_h_l and init_h_r are not mentioned
    --    in the outgoing equalities, which is what we want for an exact match.
    --     NB: We use this function when matching letrecs, so don't necessarily want to build a renaming immediately
    return eqs

matchEnvironment :: InScopeSet -> [(Var, Var)] -> [(Var, Var)] -> PureHeap -> PureHeap -> Match [(Var, Var)]
matchEnvironment ids bound_eqs free_eqs h_l h_r = matchLoop bound_eqs free_eqs S.empty S.empty
  where
    -- NB: must respect work-sharing for non-values
    --  x |-> e1, y |-> e1; (x, y) `match` x |-> e1; (x, x) == Nothing
    --  x |-> e1; (x, x) `match` x |-> e1; y |-> e1; (x, y) == Nothing (though this is more questionable, it seems a consistent choice)
    -- NB: treat equal values as equal regardless of duplication
    --  x |-> v, y |-> v; (x, y) `match` x |-> v; (x, x) /= Nothing
    -- TODO: look through variables on both sides
    --  x |-> e1; (x, x) `match` x |-> e1; y |-> x `match` (x, y) /= Nothing
    --  x |-> e1, y |-> x; (x, y) `match` x |-> e1 `match` (x, x) /= Nothing
    --
    -- It used to be important to allow instantiatation of a dynamic variable with a static *variable*.
    -- This was so because if we didn't tie back to a situation where all that had changed was that one more
    -- variable was static, we would immediately whistle because the tagbags would be the same.
    --
    -- In the new world, we record staticness as phantom heap bindings, so this just doesn't figure in at all.
    -- We can account for staticness using the standard generalisation mechanism, and there is no need for the
    -- matcher to have hacks like that (though we still have to be careful about how we match phantoms).
    
    markUsed x' (_, e) used = if isCheap (annee e) then used else S.insert x' used
    
    matchLoop known [] _ _ = return known
    matchLoop known ((x_l, x_r):free_eqs) used_l used_r
       -- Perhaps we have already assumed this equality is true?
      | (x_l, x_r) `elem` known = matchLoop known free_eqs used_l used_r
      | otherwise = case (M.lookup x_l h_l, M.lookup x_r h_r) of
           -- If matching an internal let, it is possible that variables occur free. Insist that free-ness matches:
          (Nothing, Nothing) -> go [] used_l used_r
          (Just _, Nothing) -> fail "matchLoop: L-R freeness"
          (Nothing, Just _) -> fail "matchLoop: R-L freeness"
          (Just hb_l, Just hb_r) -> case ((howBound &&& heapBindingTerm) hb_l, (howBound &&& heapBindingTerm) hb_r) of
               -- If the template provably doesn't use this heap binding, we can match it against anything at all
              ((InternallyBound, Nothing), _) -> matchLoop known free_eqs used_l used_r
               -- If the template internalises a binding of this form, check that the matchable semantics is the same.
               -- If the matchable doesn't have a corresponding binding tieback is impossible because we have less info this time.
              ((InternallyBound, Just in_e_l), (_how_r, mb_in_e_r)) -> case mb_in_e_r of
                  Just in_e_r | x_l `S.notMember` used_l, x_r `S.notMember` used_r -> matchInTerm ids in_e_l in_e_r >>= \extra_free_eqs -> go extra_free_eqs (markUsed x_l in_e_l used_l) (markUsed x_r in_e_r used_r)
                  _ -> fail "matchLoop: InternallyBound"
               -- If the template has no information but exposes a lambda, we can rename to tie back.
               -- If there is a corresponding binding in the matchable we can't tieback because we have more info this time.
               --
               -- NB: this may cause us to instantiate a lambda-bound var with one that is presently let-bound. The alternative
               -- (almost certainly an sc-stop) is worse, though... Doing this really matters; see for example the Bernouilli benchmark.
               -- FIXME: give let-bound nothings tags and generalise to get the same effect?
              ((LambdaBound, Nothing), (_how_r, mb_in_e_r)) -> case mb_in_e_r of
                  Nothing -> (if _how_r == LetBound then pprTrace "Downgrading" (ppr x_l <+> ppr x_r) else id) $
                             go [] used_l used_r
                  Just _ -> fail "matchLoop: uninformative LambdaBound"
               -- If the template has an unfolding, we must do lookthrough
              ((LambdaBound, Just in_e_l), (_how_r, mb_in_e_r)) -> case mb_in_e_r of
                  Just in_e_r | x_l `S.notMember` used_l, x_r `S.notMember` used_r -> matchInTerm ids in_e_l in_e_r >>= \extra_free_eqs -> go extra_free_eqs (markUsed x_l in_e_l used_l) (markUsed x_r in_e_r used_r)
                  _ -> fail "matchLoop: informative LambdaBound"
               -- We assume no-shadowing, so if two names are the same they must refer to the same thing
               -- NB: because I include this case, we may not include a renaming for some lambda-bound variables in the final knowns
               --
               -- Interestingly, doing this matching here also improves matching in the case where a previous state had a more-or-less
               -- evaluated version of this heap binding in place. We "know" that we can match them since they originated from the same
               -- heap binding, even though evaluation may have changed their shape.
               --
               -- Of course, we still need to match the FVs on both sides. For example, the LHS could be {x |-> Just y} with the RHS
               -- {x |-> Just y, y |-> True} -- better not tie back in this situation, so we validate that the y bindings still match.
               -- This also ensures that the outgoing knowns can be used to build a renaming that includes the RHS of these bindings.
               --
               -- OK, I don't think this is safe in the case where either side is not LetBound. The reason is that we might have:
               --     D[(let x = e1 in x, let x = e2 in x)]
               -- ==> (D[let x = e1 in x], D[let x = e2 in x])
               --
               -- Which floats to:
               --     let h0 = D[let x = e1 in x]
               --     in in (h0, D[let x = e2 in x])
               --
               -- We better not tieback the second tuple component to h0 on the basis that the two x binders match!
               -- They are only guaranteed to match if the are **Let bound**, because in that case those binders must have been
               -- created by a common ancestor and hence we can just match the uniques to determine whether the binders are the "same".
               -- It is NOT safe to do this is both/either sides are LambdaBound, because we have no guarantee of a common ancestor in that case.
              ((_how_l, mb_in_e_l), (_how_r, mb_in_e_r)) | x_l == x_r -> case (mb_in_e_l, mb_in_e_r) of
                  (Nothing,     Nothing)     -> go [] used_l used_r
                  (Just in_e_l, Just in_e_r) -> ASSERT2(inFreeVars annedTermFreeVars in_e_r `S.isSubsetOf` inFreeVars annedTermFreeVars in_e_l, text "match" <+> ppr (x_l, _how_l, in_e_l, x_r, _how_r, in_e_r))
                                                go [(x, x) | x <- S.toList (inFreeVars annedTermFreeVars in_e_l)] (markUsed x_l in_e_l used_l) (markUsed x_r in_e_r used_r)
                  _                          -> fail "matchLoop: insane LetBounds"
               -- If the template doesn't lambda abstract, we can't rename. Only tieback if we have an exact *name* match.
               --
               -- You might think that we could do better than this if both the LHS and RHS had unfoldings, by matching them.
               -- However, this is very dangerous because we don't want to match the template {x |-> let {Just y}, y |-> lam {}}
               -- against the matchable {x' |-> let {Just y'}, y' |-> lam {}}, since the template may still be able to reach y via the binding
               -- for x (we renamed the lambda-abstracted y to y' so there is nothing to fear from there).
               --
               -- NB: we can treat this *almost* exactly like the LambdaBound+unfolding case now since we have the invariant that LetBound things never
               -- refer to LambdaBound things. *However* we anticipate that doing so would almost always fail to tieback, so we elect to just stick with
               -- the "cheap-but-inaccurate" name-matching heuristic.
              ((LetBound, _), _) -> fail "matchLoop: LetBound"
      where go extra_free_eqs = matchLoop ((x_l, x_r) : known) (extra_free_eqs ++ free_eqs)
