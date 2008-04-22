%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

%************************************************************************

		Static Argument Transformation pass

%************************************************************************

May be seen as removing invariants from loops:
Arguments of recursive functions that do not change in recursive
calls are removed from the recursion, which is done locally
and only passes the arguments which effectively change.

Example:
map = /\ ab -> \f -> \xs -> case xs of
                 []       -> []
                 (a:b) -> f a : map f b

as map is recursively called with the same argument f (unmodified)
we transform it to

map = /\ ab -> \f -> \xs -> let map' ys = case ys of
                       []     -> []
                       (a:b) -> f a : map' b
                in map' xs

Notice that for a compiler that uses lambda lifting this is
useless as map' will be transformed back to what map was.

We could possibly do the same for big lambdas, but we don't as
they will eventually be removed in later stages of the compiler,
therefore there is no penalty in keeping them.

We only apply the SAT when the number of static args is > 2. This
produces few bad cases.  See
	should_transform 
in saTransform.

Here are the headline nofib results:
                  Size    Allocs   Runtime
Min             +0.0%    -13.7%    -21.4%
Max             +0.1%     +0.0%     +5.4%
Geometric Mean  +0.0%     -0.2%     -6.9%

The previous patch, to fix polymorphic floatout demand signatures, is
essential to make this work well!


\begin{code}

module SAT ( doStaticArgs ) where

import DynFlags
import Var
import VarEnv
import CoreSyn
import CoreLint
import Type
import TcType
import Id
import UniqSupply
import Unique
import Util

import Data.List
import Panic
import FastString

#include "HsVersions.h"
\end{code}

\begin{code}
doStaticArgs :: DynFlags -> UniqSupply -> [CoreBind] -> IO [CoreBind]
doStaticArgs dflags us binds = do
    showPass dflags "Static argument"
    let binds' = snd $ mapAccumL sat_bind_threaded_us us binds
    endPass dflags "Static argument" Opt_D_verbose_core2core binds'
  where
    sat_bind_threaded_us us bind = 
        let (us1, us2) = splitUniqSupply us 
        in (us1, runSAT (satBind bind) us2)
\end{code}
\begin{code}
-- We don't bother to SAT recursive groups since it can lead
-- to massive code expansion: see Andre Santos' thesis for details.
-- This means we only apply the actual SAT to Rec groups of one element,
-- but we want to recurse into the others anyway to discover other binds
satBind :: CoreBind -> SatM CoreBind
satBind (NonRec binder expr) = do
    expr' <- satExpr expr
    return (NonRec binder expr')
satBind (Rec [(binder, rhs)]) = do
    insSAEnvFromBinding binder rhs
    rhs' <- satExpr rhs
    saTransform binder rhs'
satBind (Rec pairs) = do
    let (binders, rhss) = unzip pairs
    rhss' <- mapM satExpr rhss
    return (Rec (zipEqual "satBind" binders rhss'))
\end{code}
\begin{code}
emptySATInfo :: Id -> Maybe (Id, SATInfo)
emptySATInfo v = Just (v, ([], []))

satExpr :: CoreExpr -> SatM CoreExpr
satExpr var@(Var v) = do
    updSAEnv (emptySATInfo v)
    return var

satExpr lit@(Lit _) = do
    return lit

satExpr (Lam binders body) = do
    body' <- satExpr body
    return (Lam binders body')

satExpr app@(App _ _) = do
    getAppArgs app

satExpr (Case expr bndr ty alts) = do
    expr' <- satExpr expr
    alts' <- mapM satAlt alts
    return (Case expr' bndr ty alts')
  where
    satAlt (con, bndrs, expr) = do
        expr' <- satExpr expr
        return (con, bndrs, expr')

satExpr (Let bind body) = do
    body' <- satExpr body
    bind' <- satBind bind
    return (Let bind' body')

satExpr (Note note expr) = do
    expr' <- satExpr expr
    return (Note note expr')

satExpr ty@(Type _) = do
    return ty

satExpr (Cast expr coercion) = do
    expr' <- satExpr expr
    return (Cast expr' coercion)
\end{code}

\begin{code}
getAppArgs :: CoreExpr -> SatM CoreExpr
getAppArgs app = do
    (app', result) <- get app
    updSAEnv result
    return app'
  where
    get :: CoreExpr -> SatM (CoreExpr, Maybe (Id, SATInfo))
    get (App e (Type ty)) = do
        (e', result) <- get e
        return
            (App e' (Type ty),
            case result of
                Nothing            -> Nothing
                Just (v, (tv, lv)) -> Just (v, (tv ++ [Static ty], lv)))

    get (App e a) = do
        (e', result) <- get e
        a' <- satExpr a
        
        let si = case a' of
                    Var v -> Static v
                    _     -> NotStatic
        return
            (App e' a',
            case result of
                Just (v, (tv, lv))  -> Just (v, (tv, lv ++ [si]))
                Nothing             -> Nothing)

    get var@(Var v) = do
        return (var, emptySATInfo v)

    get e = do
        e' <- satExpr e
        return (e', Nothing)
\end{code}

%************************************************************************

	Environment

%************************************************************************

\begin{code}
data SATEnv = SatEnv { idSATInfo :: IdEnv SATInfo }

emptyEnv :: SATEnv
emptyEnv = SatEnv { idSATInfo = emptyVarEnv }

type SATInfo = ([Staticness Type], [Staticness Id])

data Staticness a = Static a | NotStatic

delOneFromSAEnv :: Id -> SatM ()
delOneFromSAEnv v = modifyEnv $ \env -> env { idSATInfo = delVarEnv (idSATInfo env) v }

updSAEnv :: Maybe (Id, SATInfo) -> SatM ()
updSAEnv Nothing = do
    return ()
updSAEnv (Just (b, (tyargs, args))) = do
    r <- getSATInfo b
    case r of
      Nothing               -> return ()
      Just (tyargs', args') -> do
          delOneFromSAEnv b
          insSAEnv b (checkArgs (eqWith coreEqType) tyargs tyargs',
                      checkArgs (eqWith (==)) args args')
  where eqWith _  NotStatic  NotStatic  = True
        eqWith eq (Static x) (Static y) = x `eq` y
        eqWith _  _          _          = False

checkArgs :: (Staticness a -> Staticness a -> Bool) -> [Staticness a] -> [Staticness a] -> [Staticness a]
checkArgs _  as [] = notStatics (length as)
checkArgs _  [] as = notStatics (length as)
checkArgs eq (a:as) (a':as') | a `eq` a' = a:checkArgs eq as as'
checkArgs eq (_:as) (_:as') = NotStatic:checkArgs eq as as'

notStatics :: Int -> [Staticness a]
notStatics n = nOfThem n NotStatic

insSAEnv :: Id -> SATInfo -> SatM ()
insSAEnv b info = modifyEnv $ \env -> env { idSATInfo = extendVarEnv (idSATInfo env) b info }

insSAEnvFromBinding :: Id -> CoreExpr -> SatM ()
insSAEnvFromBinding bndr e = insSAEnv bndr (getArgLists e)
\end{code}

%************************************************************************

	Static Argument Transformation Monad

%************************************************************************

Two items of state to thread around: a UniqueSupply and a SATEnv.

\begin{code}
newtype SatM result
  = SatM (UniqSupply -> SATEnv -> (result, SATEnv))

instance Monad SatM where
    (>>=) = thenSAT
    (>>) = thenSAT_
    return = returnSAT

runSAT :: SatM a -> UniqSupply -> a
runSAT (SatM f) us = fst $ f us emptyEnv

thenSAT :: SatM a -> (a -> SatM b) -> SatM b
thenSAT (SatM m) k
  = SatM $ \us env -> 
    case splitUniqSupply us    of { (s1, s2) ->
    case m s1 env              of { (m_result, menv) ->
    case k m_result            of { (SatM k') ->
    k' s2 menv }}}

thenSAT_ :: SatM a -> SatM b -> SatM b
thenSAT_ (SatM m) (SatM k)
  = SatM $ \us env ->
    case splitUniqSupply us    of { (s1, s2) ->
    case m s1 env               of { (_, menv) ->
    k s2 menv }}

returnSAT :: a -> SatM a
returnSAT v = withEnv $ \env -> (v, env)

modifyEnv :: (SATEnv -> SATEnv) -> SatM ()
modifyEnv f = SatM $ \_ env -> ((), f env)

withEnv :: (SATEnv -> (b, SATEnv)) -> SatM b
withEnv f = SatM $ \_ env -> f env

projectFromEnv :: (SATEnv -> a) -> SatM a
projectFromEnv f = withEnv (\env -> (f env, env))
\end{code}

%************************************************************************

		Utility Functions

%************************************************************************

\begin{code}
getSATInfo :: Id -> SatM (Maybe SATInfo)
getSATInfo var = projectFromEnv $ \env -> lookupVarEnv (idSATInfo env) var

newSATName :: Id -> Type -> SatM Id
newSATName _ ty
  = SatM $ \us env -> (mkSysLocal (fsLit "$sat") (uniqFromSupply us) ty, env)

getArgLists :: CoreExpr -> ([Staticness Type], [Staticness Id])
getArgLists expr
  = let
    (tvs, lambda_bounds, _) = collectTyAndValBinders expr
    in
    ([ Static (mkTyVarTy tv) | tv <- tvs ],
     [ Static v              | v <- lambda_bounds ])

\end{code}

We implement saTransform using shadowing of binders, that is
we transform
map = \f as -> case as of
         [] -> []
         (a':as') -> let x = f a'
                 y = map f as'
                 in x:y
to
map = \f as -> let map = \f as -> map' as
           in let rec map' = \as -> case as of
                      [] -> []
                      (a':as') -> let x = f a'
                              y = map f as'
                              in x:y
          in map' as

the inner map should get inlined and eliminated.

\begin{code}
saTransform :: Id -> CoreExpr -> SatM CoreBind
saTransform binder rhs = do
    r <- getSATInfo binder
    case r of
      Just (tyargs, args) | should_transform args
        -> do
            -- In order to get strictness information on this new binder
            -- we need to make sure this stage happens >before< the analysis
            binder' <- newSATName binder (mkSATLamTy tyargs args)
            new_rhs <- mkNewRhs binder binder' args rhs
            return (NonRec binder new_rhs)
      _ -> return (Rec [(binder, rhs)])
  where
    should_transform args
      = staticArgsLength > 1		-- THIS IS THE DECISION POINT
      where staticArgsLength = length (filter isStatic args)
    
    mkNewRhs binder binder' args rhs = let
        non_static_args :: [Id]
        non_static_args = get_nsa args rhs_val_binders
          where
            get_nsa :: [Staticness a] -> [a] -> [a]
            get_nsa [] _ = []
            get_nsa _ [] = []
            get_nsa (NotStatic:args) (v:as) = v:get_nsa args as
            get_nsa (_:args)         (_:as) =   get_nsa args as

        -- To do the transformation, the game plan is to:
        -- 1. Create a small nonrecursive RHS that takes the
        --    original arguments to the function but discards
        --    the ones that are static and makes a call to the
        --    SATed version with the remainder. We intend that
        --    this will be inlined later, removing the overhead
        -- 2. Bind this nonrecursive RHS over the original body
        --    WITH THE SAME UNIQUE as the original body so that
        --    any recursive calls to the original now go via
        --    the small wrapper
        -- 3. Rebind the original function to a new one which contains
        --    our SATed function and just makes a call to it:
        --    we call the thing making this call the local body

        local_body = mkApps (Var binder') [Var a | a <- non_static_args]

        nonrec_rhs = mkOrigLam local_body

        -- HACK! The following is a fake SysLocal binder with
        --  *the same* unique as binder.
        -- the reason for this is the following:
        -- this binder *will* get inlined but if it happen to be
        -- a top level binder it is never removed as dead code,
        -- therefore we have to remove that information (of it being
        -- top-level or exported somehow.)
        -- A better fix is to use binder directly but with the TopLevel
        -- tag (or Exported tag) modified.
        fake_binder = mkSysLocal (fsLit "sat")
                (getUnique binder)
                (idType binder)
        rec_body = mkLams non_static_args
                   (Let (NonRec fake_binder nonrec_rhs) {-in-} rhs_body)
        in return (mkOrigLam (Let (Rec [(binder', rec_body)]) {-in-} local_body))
      where
        (rhs_binders, rhs_body) = collectBinders rhs
        rhs_val_binders = filter isId rhs_binders
        
        mkOrigLam = mkLams rhs_binders

    mkSATLamTy tyargs args
      = substTy (mk_inst_tyenv tyargs tv_tmpl)
                (mkSigmaTy tv_tmpl' theta_tys' tau_ty')
      where
          -- get type info for the local function:
          (tv_tmpl, theta_tys, tau_ty) = (tcSplitSigmaTy . idType) binder
          (reg_arg_tys, res_type)      = splitFunTys tau_ty

          -- now, we drop the ones that are
          -- static, that is, the ones we will not pass to the local function
          tv_tmpl'     = dropStatics tyargs tv_tmpl

          -- Extract the args that correspond to the theta tys (e.g. dictionaries) and argument tys (normal values)
          (args1, args2) = splitAtList theta_tys args
          theta_tys'     = dropStatics args1 theta_tys
          reg_arg_tys'   = dropStatics args2 reg_arg_tys

          -- Piece the function type back together from our static-filtered components
          tau_ty'        = mkFunTys reg_arg_tys' res_type

          mk_inst_tyenv :: [Staticness Type] -> [TyVar] -> TvSubst
          mk_inst_tyenv []              _      = emptyTvSubst
          mk_inst_tyenv (Static s:args) (t:ts) = extendTvSubst (mk_inst_tyenv args ts) t s
          mk_inst_tyenv (_:args)        (_:ts) = mk_inst_tyenv args ts
          mk_inst_tyenv _               _      = panic "mk_inst_tyenv"

dropStatics :: [Staticness a] -> [b] -> [b]
dropStatics [] t = t
dropStatics (Static _:args) (_:ts) = dropStatics args ts
dropStatics (_:args)        (t:ts) = t:dropStatics args ts
dropStatics _               _      = panic "dropStatics"

isStatic :: Staticness a -> Bool
isStatic NotStatic = False
isStatic _         = True
\end{code}
