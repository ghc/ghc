module Supercompile (supercompileProgram, supercompileProgramSelective) where

#include "HsVersions.h"

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
import Supercompile.StaticFlags
import qualified Supercompile.Core.Syntax as S
import qualified Supercompile.Core.FreeVars as S
import qualified Supercompile.Drive.Process1 as S ()
import qualified Supercompile.Drive.Process2 as S ()
import qualified Supercompile.Drive.Process3 as S

import BasicTypes (InlinePragma(..), InlineSpec(..), isActiveIn, TupleSort(..), isStrongLoopBreaker)
import CoreSyn
import CoreFVs    (exprFreeVars)
import CoreUtils  (exprType)
import MkCore     (mkWildValBinder)
import Coercion   (isCoVar, mkCoVarCo, mkAxInstCo)
import DataCon    (dataConAllTyVars, dataConRepArgTys, dataConTyCon, dataConWorkId)
import VarSet
import Id
import MkId       (realWorldPrimId)
import FastString (fsLit)
import PrimOp     (primOpSig)
import TcType     (tcSplitDFunTy)
import Type       (mkTyVarTy, mkTyConApp)
import TysPrim    (realWorldStatePrimTy)
import TysWiredIn (tupleTyCon, tupleCon)
import TyCon      (newTyConCo_maybe)

import qualified Data.Map as M

type FlatCoreBinds = [(Id, CoreExpr)]

-- Split input bindings into two lists:
--  1) CoreBinds binding variables with at least one binder marked by the predicate,
--     and any CoreBinds that those CoreBinds transitively refer to
--  2) The remaining CoreBinds. These may refer to those CoreBinds but are not referred
--     to *by* them
--
-- NB: assumes no-shadowing at the top level. I don't want to have to rename stuff to
-- commute CoreBinds...
partitionBinds :: (Id -> Bool) -> FlatCoreBinds -> (FlatCoreBinds, FlatCoreBinds)
partitionBinds should_sc initial_binds = go initial_inside initial_undecided
  where
    (initial_inside, initial_undecided) = partition (should_sc . fst) initial_binds
    
    go :: FlatCoreBinds -> FlatCoreBinds -> (FlatCoreBinds, FlatCoreBinds)
    go inside undecided
        | null inside' = (inside, undecided)
        | otherwise    = first (inside ++) $ go inside' undecided'
      where
        -- Move anything inside that is referred to by a binding that was moved inside last round
        inside_fvs = coreBindsFVs inside
        (inside', undecided') = partition (\(x, _) -> x `elemVarSet` inside_fvs) undecided

coreBindsFVs :: FlatCoreBinds -> S.FreeVars
coreBindsFVs bs = unionVarSets [S.idFreeVars x `unionVarSet` exprFreeVars e | (x, e) <- bs]

coreBindsToCoreTerm :: (Id -> Bool) -> FlatCoreBinds -> (CoreExpr, Var -> FlatCoreBinds)
coreBindsToCoreTerm should_sc binds
  = --pprTrace "coreBindsToCoreTerm" (ppr (bindersOfBinds binds, bindersOfBinds sc_binds, bindersOfBinds dont_sc_binds, dont_sc_binds_fvs, sc_xs_internal_xs, sc_xs, bindersOfBinds internal_sc_binds)) $
    (Let (Rec internal_sc_binds) (mkLiftedVarTup sc_internal_xs),
     \y -> [(x, mkLiftedTupleSelector sc_internal_xs internal_x (Var y)) | (x, internal_x) <- sc_xs_internal_xs] ++ dont_sc_binds)
  where
    -- We put all the sc_binds into a local let, and use unboxed tuples to bind back to the top level the names of
    -- any of those sc_binds that are either exported *or* in the free variables of something from dont_sc_binds.
    -- Making that list as small as possible allows the supercompiler to determine that more things are used linearly.
    --
    -- We used to use a standard "big tuple" to do the binding-back, but this breaks down if we need to include
    -- some variables of unlifted type (of kind #) or a dictionary (of kind Constraint) since the type arguments of
    -- the (,..,) tycon must be of kind *. The unlifted case isn't important (they can't occur at top level), but
    -- the Constraint case is a killer.
    --
    -- Then I tried to use a church-encoded tuple to do that, where the tuple (x, y, z) is encoded as
    -- /\(a :: ArgTypeKind). \(k :: x_ty -> y_ty -> z_ty -> a). k x y z
    -- And selected from by applying an appropriate type argument and continuation. Unfortunately, this type polymorphism,
    -- while permitted by the type system, is an illusion: abstraction over types of kinds other than * only works
    -- if the type abstractions all are beta-reduced away before code generation.
    --
    -- Another problem with this second approach is that GHC's inlining heuristics didn't tend to inline very
    -- large encoded tuples even with explicit continutaion args, because the contination binder didn't get a
    -- large enough discount.
    --
    -- My third attempt just encodes it as an unboxed tuple, which we contrive to buind at the top level by abstracting
    -- it over a useless arg of void representation.
    (sc_binds, dont_sc_binds) = partitionBinds should_sc binds -- FIXME: experiment with just taking annotated binds, and relying on unfoldings for the rest (problematic for non-values, though!)
    dont_sc_binds_fvs = coreBindsFVs dont_sc_binds
    
    -- We should zap fragile information on the Ids' we use within the tuple selector. The reasons are:
    --  1. They may be mutually inter-referring, and the binders of a "case" are not simultaneously brought into scope
    --  2. For some reason, GHC seems to have trouble optimising (let x = y in x) to (y) if x has an unfolding.
    zappedBindersOfBinds = map (zapFragileIdInfo . fst)

    -- This is a sweet hack. Most of the top-level binders will be External names. It is a Bad Idea to locally-bind
    -- an External name, because several Externals with the same name but different uniques will generate clashing
    -- C labels at code-generation time (the unique is not included in the label).
    --
    -- To work around this, we deexternalise the variables at the *local binding sites* we are about to create.
    -- Note that we leave the *use sites* totally intact: we rely on the fact that a) variables are compared only by
    -- unique and b) the internality of these names will be carried down on the next simplifier run, so this works.
    -- The ice is thin, though!
    sc_xs = zappedBindersOfBinds sc_binds
    -- NB: if we don't mark these Ids as not exported then we get lots of residual top-level bindings of the form x = y
    internal_sc_binds = map (first localiseId) sc_binds
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
    sc_xs_internal_xs = uncurry (go []) (partition (\(x, _) -> isExportedId x || x `elemVarSet` dont_sc_binds_fvs) (sc_xs `zip` zappedBindersOfBinds internal_sc_binds))
    sc_internal_xs = map snd sc_xs_internal_xs


-- NB: I can't see any GHC code that prevents nullary unboxed tuples, but I'm not actually sure they work
-- (note that in particular they get the same OccName as the unary versions).
mkLiftedVarTup :: [Id] -> CoreExpr
mkLiftedVarTup xs = Lam (mkWildValBinder realWorldStatePrimTy) $ Var (dataConWorkId (tupleCon UnboxedTuple (length xs))) `mkTyApps` map idType xs `mkVarApps` xs

mkLiftedTupleSelector :: [Id] -> Id -> CoreExpr -> CoreExpr
mkLiftedTupleSelector xs want_x tup_e
  = Case (tup_e `App` Var realWorldPrimId) (mkWildValBinder (mkTyConApp (tupleTyCon UnboxedTuple n) (map idType xs))) (idType want_x)
         [(DataAlt (tupleCon UnboxedTuple n), xs, Var want_x)]
  where n = length xs

termUnfoldings :: S.Term -> [(Var, S.Term)]
termUnfoldings e = go (S.termFreeVars e) emptyVarSet [] []
  where
    -- FIXME: varBndrFreeVars?
    go new_fvs all_fvs all_xwhy_nots all_xes
      | isEmptyVarSet added_fvs = pprTrace "termUnfoldings" (vcat [hang (text why_not <> text ":") 2 (vcat (map ppr xs)) | (why_not, xs) <- groups snd fst all_xwhy_nots]) $
                                  all_xes
      | otherwise               = go (unionVarSets (map (S.termFreeVars . snd) added_xes)) (all_fvs `unionVarSet` added_fvs)
                                     (added_xwhy_nots ++ all_xwhy_nots) (added_xes ++ all_xes)
      where added_fvs = new_fvs `minusVarSet` all_fvs
            (added_xwhy_nots, added_xes)
              = foldVarSet (\x (xwhy_nots, xes) -> case varUnfolding x of
                                                     Left why_not -> ((x, why_not):xwhy_nots,        xes)
                                                     Right e      -> (             xwhy_nots, (x, e):xes))
                           ([], []) added_fvs

    varUnfolding x
      | Just pop <- isPrimOpId_maybe x          = Right $ primOpUnfolding pop
      | Just dc <- isDataConWorkId_maybe x      = Right $ dataUnfolding dc
      | Just why_not <- shouldExposeUnfolding x = Left why_not
      | otherwise                               = case realIdUnfolding x of
        NoUnfolding                   -> Left "no unfolding"
        OtherCon _                    -> Left "no positive unfolding"
        DFunUnfolding _ dc es         -> Right $ runParseM us2 $ coreExprToTerm $ mkLams as $ mkLams xs $ Var (dataConWorkId dc) `mkTyApps` cls_tys `mkApps` [(e `mkTyApps` map mkTyVarTy as) `mkVarApps` xs | e <- es]
         where (as, theta, _cls, cls_tys) = tcSplitDFunTy (idType x)
               xs = zipWith (mkSysLocal (fsLit "x")) bv_uniques theta
        CoreUnfolding { uf_tmpl = e } -> Right $ runParseM us2 $ coreExprToTerm e
         -- NB: it's OK if the unfolding is a non-value, as the evaluator won't inline LetBound non-values
    
    -- We don't want to expose an unfolding if it would not be inlineable in the initial phase.
    -- This gives normal RULES more of a chance to fire.
    --
    -- NB: at this point we have already done prepareTerm, so used local bindings will be pushed into "e"
    -- already. Thus all this code basically only affects imported functions. In this way, we exactly match
    -- the behaviour of GHC's current Specialise pass, which:
    --  * Exhaustively specialises *locally defined* functions on their dictionary arguments
    --  * Specialises those *imported* functions that are marked Inlineable
    shouldExposeUnfolding x = case inl_inline inl_prag of
        Inlinable super
          | only_if_superinlinable, not super  -> Just "INLINEABLE but not SUPERINLINABLE"
        NoInline
          | isActiveIn 2 (inl_act inl_prag)    -> Just "NONLINE"
          | only_if_superinlinable             -> Just "(inactive) NOINLINE, not SUPERINLINABLE"
        EmptyInlineSpec 
          | only_if_superinlinable             -> Just "not SUPERINLINABLE"
        _                                      -> Nothing
      where inl_prag = idInlinePragma x
            -- EXPERIMENT: only respect the SUPERINLINABLE distinction on *loop breakers*
            -- The motivation is that we don't really want to go around annotating (GHC.Base.>>=),
            -- bindIO, etc etc as SUPERINLINABLE.
            only_if_superinlinable | sUPERINLINABLE_ONLY = isStrongLoopBreaker (idOccInfo x)
                                   | otherwise           = False
    
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

    -- We need a UniqSupply so we can generate Uniques for datacon/primop/user unfoldings. It doesn't really matter
    -- that the binders we generate here may shadow things above, but we have to be careful with our use of anfUniqSupply'
    -- when we call runParseM to deal with user unfoldings. The reason is that coreExprToTerm assumes that no free variables
    -- of the CoreExpr have Uniques generated by the unique supply it is passed.
    --
    -- This meant that when runParseM used to unconditionally use anfUniqSupply' for Uniques we had a stupid bug, because
    -- we were also using the uniques from anfUniqueSupply to generate lambda-binders in the DFunUnfolding case.
    -- All we needed to do to fix this was to make sure we split off a seperate UniqSupply for generating the lambda-bindings
    -- than we pass down to runParseM. 
    (us1, us2) = splitUniqSupply anfUniqSupply'
    bv_uniques = uniqsFromSupply us1

supercompile :: CoreExpr -> IO CoreExpr
supercompile e = -- liftM (termToCoreExpr . snd) $
                 return $ termToCoreExpr $
                 S.supercompile (M.fromList unfs) e'
  where unfs = termUnfoldings e'
        e' = runParseM anfUniqSupply' (coreExprToTerm e)

supercompileProgram :: [CoreBind] -> IO [CoreBind]
supercompileProgram binds = supercompileProgramSelective selector binds
  where selector | any idSupercompilePragma (bindersOfBinds binds) = idSupercompilePragma
                 | otherwise                                       = const True

supercompileProgramSelective :: (Id -> Bool) -> [CoreBind] -> IO [CoreBind]
supercompileProgramSelective should_sc binds = liftM (\e' -> [Rec $ (x, e') : rebuild x]) (supercompile e)
  where x = mkSysLocal (fsLit "sc") topUnique (exprType e)
        -- NB: we assume no-shadowing at top level, which is probably reasonable
        flat_binds = flattenBinds binds
        (e, rebuild) = ASSERT(length (nub (map fst flat_binds)) == length flat_binds)
                       coreBindsToCoreTerm should_sc flat_binds
