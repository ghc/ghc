module Supercompile (supercompileProgram, supercompileProgramSelective) where

-- FIXME: I need to document the basis on which I push down unlifted heap bindings (they are all values, IIRC)
-- TODO:
--  * Why does the supercompiler not match as much as it should? (e.g. Interpreter, UInterpreter)
--  * We should probably claimStep when supercompiling the RHS of an explicit lambda bound by a recursive "let".
--    Reason: it is tantamount to inlining the body one time. Note that we don't care about non-lambdas (we don't
--    pay for inlining them) or non-values (we don't put a copy of a non-value in the heap along with the RHS).
--
--    If there isn't enough left, what do we do?? Obvious answer: lambda abstract over the function name.
--    Better(?) answer: add it as a let-bound Nothing to the heap, so the resulting h-function is trapped by the residual letrec...

-- TODO: pre-transforming (case e1 of y { C z -> e2[z] }) to case (case e1 of y { C z -> z }) of z -> e2[z]
-- might help us replace CPR more because even if we generalise away the e2[z] we potentially keep the unboxing.
-- Probably can't/shouldn't do this if the wildcard binder y is used in the RHS.

import Supercompile.GHC
import Supercompile.Utilities
import qualified Supercompile.Core.Syntax as S
import qualified Supercompile.Core.FreeVars as S
import qualified Supercompile.Drive.Process1 as S ()
import qualified Supercompile.Drive.Process2 as S ()
import qualified Supercompile.Drive.Process3 as S

import BasicTypes (InlinePragma(..), InlineSpec(..), isActiveIn)
import CoreSyn
import CoreFVs    (exprFreeVars)
import CoreUtils  (exprType)
import Coercion   (isCoVar, mkCoVarCo, mkAxInstCo)
import DataCon    (dataConAllTyVars, dataConRepArgTys, dataConInstOrigArgTys, dataConTyCon, dataConWorkId)
import VarSet
import VarEnv
import Name       (localiseName, mkSystemName)
import OccName    (mkVarOcc)
import Var        (Var, varUnique, varName, setVarName)
import Id
import FastString (fsLit)
import PrelNames  (undefinedName)
import PrimOp     (primOpSig)
import TcType     (tcSplitDFunTy)
import Type       (mkTyVarTy, mkForAllTy, mkFunTys)
import TysPrim    (alphaTyVar, argAlphaTyVar)
import TyCon      (newTyConCo_maybe)

import qualified Data.Map as M

-- Split input bindings into two lists:
--  1) CoreBinds binding variables with at least one binder marked by the predicate,
--     and any CoreBinds that those CoreBinds transitively refer to
--  2) The remaining CoreBinds. These may refer to those CoreBinds but are not referred
--     to *by* them
--
-- As a bonus, it returns the free variables of the bindings in the second list.
--
-- NB: assumes no-shadowing at the top level. I don't want to have to rename stuff to
-- commute CoreBinds...
partitionBinds :: (Id -> Bool) -> [CoreBind] -> ([CoreBind], [CoreBind], S.FreeVars)
partitionBinds should_sc initial_binds = go initial_inside [(b, unionVarSets (map S.idFreeVars (bindersOf b) ++ map exprFreeVars (rhssOfBind b))) | b <- initial_undecided]
  where
    (initial_inside, initial_undecided) = partition (any should_sc . bindersOf) initial_binds
    
    go :: [CoreBind] -> [(CoreBind, S.FreeVars)] -> ([CoreBind], [CoreBind], S.FreeVars)
    go inside undecided
        | null inside' = (inside, map fst undecided, unionVarSets (map snd undecided))
        | otherwise    = first3 (inside ++) $ go (map fst inside') undecided'
      where
        -- Move anything inside that refers to a binding that was moved inside last round
        (inside', undecided') = partition (\(_, fvs) -> inside_bs `intersectsVarSet` fvs) undecided
        inside_bs = mkVarSet [x | b <- inside, x <- bindersOf b]

coreBindsToCoreTerm :: (Id -> Bool) -> [CoreBind] -> (CoreExpr, Var -> [CoreBind])
coreBindsToCoreTerm should_sc binds
  = (mkLets internal_sc_binds (mkChurchVarTup sc_internal_xs),
     \y -> [NonRec x (mkChurchTupleSelector sc_internal_xs internal_x (Var y)) | (x, internal_x) <- sc_xs_internal_xs] ++ dont_sc_binds)
  where
    -- We put all the sc_binds into a local let, and use Church-encoded tuples to bind back to the top level the names of
    -- any of those sc_binds that are either exported *or* in the free variables of something from dont_sc_binds.
    -- Making that list as small as possible allows the supercompiler to determine that more things are used linearly.
    --
    -- We used to use a standard "big tuple" to do the binding-back, but this breaks down if we need to include
    -- some variables of unlifted type (of kind #) or a dictionary (of kind Constraint) since the type arguments of
    -- the (,..,) tycon must be of kind *.
    (sc_binds, dont_sc_binds, dont_sc_binds_fvs) = partitionBinds should_sc binds
    
    -- This is a sweet hack. Most of the top-level binders will be External names. It is a Bad Idea to locally-bind
    -- an External name, because several Externals with the same name but different uniques will generate clashing
    -- C labels at code-generation time (the unique is not included in the label).
    --
    -- To work around this, we deexternalise the variables at the *local binding sites* we are about to create.
    -- Note that we leave the *use sites* totally intact: we rely on the fact that a) variables are compared only by
    -- unique and b) the internality of these names will be carried down on the next simplifier run, so this works.
    -- The ice is thin, though!
    sc_xs = bindersOfBinds sc_binds
    internal_sc_binds = [case bind of NonRec x e -> NonRec (localiseInternaliseId x) e
                                      Rec xes    -> Rec (map (first localiseInternaliseId) xes)
                        | bind <- sc_binds]
    -- Decide which things we should export from the supercompiled term using a Church tuple.
    -- We need to export to the top level of the module those bindings that are *any* of:
    --   1. Are exported by the module itself
    --   2. Are free variables of the non-supercompiled bindings
    --   3. Are free variables of the var binder for another top-level-exported thing
    go exported exported' undecided
       | null exported' = exported
       | otherwise      = go (exported' ++ exported) exported'' undecided'
      where (exported'', undecided') = partition (\(x, _) -> x `elemVarSet` exported_xs') undecided
            exported_xs' = unionVarSets (map (S.idFreeVars . fst) exported')
    sc_xs_internal_xs = uncurry (go []) (partition (\(x, _) -> isExportedId x || x `elemVarSet` dont_sc_binds_fvs) (sc_xs `zip` bindersOfBinds internal_sc_binds))
    sc_internal_xs = map snd sc_xs_internal_xs
    localiseInternaliseId x = setIdNotExported (x `setVarName` localiseName (varName x))
     -- If we don't mark these Ids as not exported then we get lots of residual top-level bindings of the form x = y

mkChurchVarTup :: [Id] -> CoreExpr
mkChurchVarTup []             = Var (mkVanillaGlobal undefinedName (mkForAllTy alphaTyVar (mkTyVarTy alphaTyVar)))
mkChurchVarTup xs@(first_x:_) = Lam argAlphaTyVar $ Lam k $ Var k `mkVarApps` xs
  where iss = mkInScopeSet (mkVarSet xs)
        -- Gin up a name for the continuation argument from spit, glue, and an exhaustive set of shadowed names
        --
        -- FIXME: We have to lie and use argAlphaTyVar here because we want to instantiate it with types of
        -- kind * and of kind Constraint.
        --
        -- The lie is that instantiation with types of kind # would be very bad!! Luckily this never happens
        -- since the top level can never bind an unlifted value.
        k = uniqAway iss $ mkLocalId (mkSystemName (varUnique first_x) (mkVarOcc "k"))
                                     (map idType xs `mkFunTys` mkTyVarTy argAlphaTyVar)

mkChurchTupleSelector :: [Var] -> Var -> CoreExpr -> CoreExpr
mkChurchTupleSelector xs want_x tup_e = tup_e `App` Type (idType want_x) `App` mkLams xs (Var want_x)

termUnfoldings :: S.Term -> [(Var, S.Term)]
termUnfoldings e = go (S.termFreeVars e) emptyVarSet []
  where
    go new_fvs all_fvs all_xes
      | isEmptyVarSet added_fvs = all_xes -- FIXME: varBndrFreeVars?
      | otherwise               = go (unionVarSets (map (S.termFreeVars . snd) added_xes)) (all_fvs `unionVarSet` added_fvs) (added_xes ++ all_xes)
      where added_fvs = new_fvs `minusVarSet` all_fvs
            added_xes = [ (x, e)
                        | x <- varSetElems added_fvs
                        , Just e <- [varUnfolding x]]

    varUnfolding x
      | Just pop <- isPrimOpId_maybe x     = Just $ primOpUnfolding pop
      | Just dc <- isDataConWorkId_maybe x = Just $ dataUnfolding dc
      | not (shouldExposeUnfolding x)      = Nothing
      | otherwise                          = case realIdUnfolding x of
        NoUnfolding                   -> Nothing
        OtherCon _                    -> Nothing
        DFunUnfolding _ dc es         -> Just $ runParseM $ coreExprToTerm $ mkLams as $ mkLams xs $ Var (dataConWorkId dc) `mkTyApps` cls_tys `mkApps` [(e `mkTyApps` map mkTyVarTy as) `mkVarApps` xs | e <- es]
         where (as, theta, _cls, cls_tys) = tcSplitDFunTy (idType x)
               xs = zipWith (mkSysLocal (fsLit "x")) bv_uniques theta
        CoreUnfolding { uf_tmpl = e } -> Just $ runParseM $ coreExprToTerm e
         -- NB: it's OK if the unfolding is a non-value, as the evaluator won't inline LetBound non-values
    
    -- We don't want to expose an unfolding if it would not be inlineable in the initial phase.
    -- This gives normal RULES more of a chance to fire.
    shouldExposeUnfolding x = case inl_inline inl_prag of
        Inline          -> True
        Inlinable       -> True
        NoInline        -> isActiveIn 2 (inl_act inl_prag)
        EmptyInlineSpec -> True
      where inl_prag = idInlinePragma x
    
    primOpUnfolding pop = S.tyLambdas as $ S.lambdas xs $ S.primOp pop (map mkTyVarTy as) (map S.var xs)
      where (as, arg_tys, _res_ty, _arity, _strictness) = primOpSig pop
            xs = zipWith (mkSysLocal (fsLit "x")) bv_uniques arg_tys
    
    dataUnfolding dc
      | Just co_axiom <- newTyConCo_maybe (dataConTyCon dc)
      , let [x] = xs
      = S.tyLambdas as $ S.lambdas [x] $ S.var x `S.cast` mkAxInstCo co_axiom (map mkTyVarTy as)
      | otherwise
      = S.tyLambdas as $ S.lambdas xs $ S.value (S.Data dc (map mkTyVarTy as) (map mkCoVarCo qs) ys)
      where as = dataConAllTyVars dc
            arg_tys = dataConRepArgTys dc
            xs = zipWith (mkSysLocal (fsLit "x")) bv_uniques arg_tys
            (qs, ys) = span isCoVar xs

    -- It doesn't matter if we reuse Uniques here because by construction they can't shadow other uses of the anfUniqSupply'
    bv_uniques = uniqsFromSupply anfUniqSupply'

supercompile :: CoreExpr -> IO CoreExpr
supercompile e = -- liftM (termToCoreExpr . snd) $
                 return $ termToCoreExpr $
                 S.supercompile (M.fromList unfs) e'
  where unfs = termUnfoldings e'
        e' = runParseM (coreExprToTerm e)

supercompileProgram :: [CoreBind] -> IO [CoreBind]
supercompileProgram = supercompileProgramSelective (const True)

supercompileProgramSelective :: (Id -> Bool) -> [CoreBind] -> IO [CoreBind]
supercompileProgramSelective should_sc binds = liftM (\e' -> NonRec x e' : rebuild x) (supercompile e)
  where x = mkSysLocal (fsLit "sc") topUnique (exprType e)
        (e, rebuild) = coreBindsToCoreTerm should_sc binds
