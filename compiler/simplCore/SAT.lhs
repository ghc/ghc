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
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details


module SAT ( doStaticArgs ) where

import Var
import CoreSyn
import CoreUtils
import Type
import Coercion
import Id
import Name
import VarEnv
import UniqSupply
import Util
import UniqFM
import VarSet
import Unique
import UniqSet
import Outputable

import Data.List
import FastString

#include "HsVersions.h"
\end{code}

\begin{code}
doStaticArgs :: UniqSupply -> CoreProgram -> CoreProgram
doStaticArgs us binds = snd $ mapAccumL sat_bind_threaded_us us binds
  where
    sat_bind_threaded_us us bind =
        let (us1, us2) = splitUniqSupply us
        in (us1, fst $ runSAT us2 (satBind bind emptyUniqSet))
\end{code}
\begin{code}
-- We don't bother to SAT recursive groups since it can lead
-- to massive code expansion: see Andre Santos' thesis for details.
-- This means we only apply the actual SAT to Rec groups of one element,
-- but we want to recurse into the others anyway to discover other binds
satBind :: CoreBind -> IdSet -> SatM (CoreBind, IdSATInfo)
satBind (NonRec binder expr) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (NonRec binder expr', finalizeApp expr_app sat_info_expr)
satBind (Rec [(binder, rhs)]) interesting_ids = do
    let interesting_ids' = interesting_ids `addOneToUniqSet` binder
        (rhs_binders, rhs_body) = collectBinders rhs
    (rhs_body', sat_info_rhs_body) <- satTopLevelExpr rhs_body interesting_ids'
    let sat_info_rhs_from_args = unitVarEnv binder (bindersToSATInfo rhs_binders)
        sat_info_rhs' = mergeIdSATInfo sat_info_rhs_from_args sat_info_rhs_body
        
        shadowing = binder `elementOfUniqSet` interesting_ids
        sat_info_rhs'' = if shadowing
                        then sat_info_rhs' `delFromUFM` binder -- For safety
                        else sat_info_rhs'
    
    bind' <- saTransformMaybe binder (lookupUFM sat_info_rhs' binder) 
			      rhs_binders rhs_body'
    return (bind', sat_info_rhs'')
satBind (Rec pairs) interesting_ids = do
    let (binders, rhss) = unzip pairs
    rhss_SATed <- mapM (\e -> satTopLevelExpr e interesting_ids) rhss
    let (rhss', sat_info_rhss') = unzip rhss_SATed
    return (Rec (zipEqual "satBind" binders rhss'), mergeIdSATInfos sat_info_rhss')
\end{code}
\begin{code}
data App = VarApp Id | TypeApp Type | CoApp Coercion
data Staticness a = Static a | NotStatic

type IdAppInfo = (Id, SATInfo)

type SATInfo = [Staticness App]
type IdSATInfo = IdEnv SATInfo
emptyIdSATInfo :: IdSATInfo
emptyIdSATInfo = emptyUFM

{-
pprIdSATInfo id_sat_info = vcat (map pprIdAndSATInfo (Map.toList id_sat_info))
  where pprIdAndSATInfo (v, sat_info) = hang (ppr v <> colon) 4 (pprSATInfo sat_info)
-}

pprSATInfo :: SATInfo -> SDoc
pprSATInfo staticness = hcat $ map pprStaticness staticness

pprStaticness :: Staticness App -> SDoc
pprStaticness (Static (VarApp _))  = ptext (sLit "SV") 
pprStaticness (Static (TypeApp _)) = ptext (sLit "ST") 
pprStaticness (Static (CoApp _))   = ptext (sLit "SC")
pprStaticness NotStatic            = ptext (sLit "NS")


mergeSATInfo :: SATInfo -> SATInfo -> SATInfo
mergeSATInfo [] _  = []
mergeSATInfo _  [] = []
mergeSATInfo (NotStatic:statics) (_:apps) = NotStatic : mergeSATInfo statics apps
mergeSATInfo (_:statics) (NotStatic:apps) = NotStatic : mergeSATInfo statics apps
mergeSATInfo ((Static (VarApp v)):statics)  ((Static (VarApp v')):apps)  = (if v == v' then Static (VarApp v) else NotStatic) : mergeSATInfo statics apps
mergeSATInfo ((Static (TypeApp t)):statics) ((Static (TypeApp t')):apps) = (if t `eqType` t' then Static (TypeApp t) else NotStatic) : mergeSATInfo statics apps
mergeSATInfo ((Static (CoApp c)):statics) ((Static (CoApp c')):apps)     = (if c `coreEqCoercion` c' then Static (CoApp c) else NotStatic) : mergeSATInfo statics apps
mergeSATInfo l  r  = pprPanic "mergeSATInfo" $ ptext (sLit "Left:") <> pprSATInfo l <> ptext (sLit ", ")
                                            <> ptext (sLit "Right:") <> pprSATInfo r

mergeIdSATInfo :: IdSATInfo -> IdSATInfo -> IdSATInfo
mergeIdSATInfo = plusUFM_C mergeSATInfo

mergeIdSATInfos :: [IdSATInfo] -> IdSATInfo
mergeIdSATInfos = foldl' mergeIdSATInfo emptyIdSATInfo

bindersToSATInfo :: [Id] -> SATInfo
bindersToSATInfo vs = map (Static . binderToApp) vs
    where binderToApp v | isId v    = VarApp v
                        | isTyVar v = TypeApp $ mkTyVarTy v
                        | otherwise = CoApp $ mkCoVarCo v

finalizeApp :: Maybe IdAppInfo -> IdSATInfo -> IdSATInfo
finalizeApp Nothing id_sat_info = id_sat_info
finalizeApp (Just (v, sat_info')) id_sat_info = 
    let sat_info'' = case lookupUFM id_sat_info v of
                        Nothing -> sat_info'
                        Just sat_info -> mergeSATInfo sat_info sat_info'
    in extendVarEnv id_sat_info v sat_info''
\end{code}
\begin{code}
satTopLevelExpr :: CoreExpr -> IdSet -> SatM (CoreExpr, IdSATInfo)
satTopLevelExpr expr interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (expr', finalizeApp expr_app sat_info_expr)

satExpr :: CoreExpr -> IdSet -> SatM (CoreExpr, IdSATInfo, Maybe IdAppInfo)
satExpr var@(Var v) interesting_ids = do
    let app_info = if v `elementOfUniqSet` interesting_ids
                   then Just (v, [])
                   else Nothing
    return (var, emptyIdSATInfo, app_info)

satExpr lit@(Lit _) _ = do
    return (lit, emptyIdSATInfo, Nothing)

satExpr (Lam binders body) interesting_ids = do
    (body', sat_info, this_app) <- satExpr body interesting_ids
    return (Lam binders body', finalizeApp this_app sat_info, Nothing)

satExpr (App fn arg) interesting_ids = do
    (fn', sat_info_fn, fn_app) <- satExpr fn interesting_ids
    let satRemainder = boring fn' sat_info_fn
    case fn_app of
        Nothing -> satRemainder Nothing
        Just (fn_id, fn_app_info) ->
            -- TODO: remove this use of append somehow (use a data structure with O(1) append but a left-to-right kind of interface)
            let satRemainderWithStaticness arg_staticness = satRemainder $ Just (fn_id, fn_app_info ++ [arg_staticness])
            in case arg of
                Type t     -> satRemainderWithStaticness $ Static (TypeApp t)
                Coercion c -> satRemainderWithStaticness $ Static (CoApp c)
                Var v      -> satRemainderWithStaticness $ Static (VarApp v)
                _          -> satRemainderWithStaticness $ NotStatic
  where
    boring :: CoreExpr -> IdSATInfo -> Maybe IdAppInfo -> SatM (CoreExpr, IdSATInfo, Maybe IdAppInfo)
    boring fn' sat_info_fn app_info = 
        do (arg', sat_info_arg, arg_app) <- satExpr arg interesting_ids
           let sat_info_arg' = finalizeApp arg_app sat_info_arg
               sat_info = mergeIdSATInfo sat_info_fn sat_info_arg'
           return (App fn' arg', sat_info, app_info)

satExpr (Case expr bndr ty alts) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    let sat_info_expr' = finalizeApp expr_app sat_info_expr
    
    zipped_alts' <- mapM satAlt alts
    let (alts', sat_infos_alts) = unzip zipped_alts'
    return (Case expr' bndr ty alts', mergeIdSATInfo sat_info_expr' (mergeIdSATInfos sat_infos_alts), Nothing)
  where
    satAlt (con, bndrs, expr) = do
        (expr', sat_info_expr) <- satTopLevelExpr expr interesting_ids
        return ((con, bndrs, expr'), sat_info_expr)

satExpr (Let bind body) interesting_ids = do
    (body', sat_info_body, body_app) <- satExpr body interesting_ids
    (bind', sat_info_bind) <- satBind bind interesting_ids
    return (Let bind' body', mergeIdSATInfo sat_info_body sat_info_bind, body_app)

satExpr (Tick tickish expr) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (Tick tickish expr', sat_info_expr, expr_app)

satExpr ty@(Type _) _ = do
    return (ty, emptyIdSATInfo, Nothing)
    
satExpr co@(Coercion _) _ = do
    return (co, emptyIdSATInfo, Nothing)

satExpr (Cast expr coercion) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (Cast expr' coercion, sat_info_expr, expr_app)
\end{code}

%************************************************************************

                Static Argument Transformation Monad

%************************************************************************

\begin{code}
type SatM result = UniqSM result

runSAT :: UniqSupply -> SatM a -> a
runSAT = initUs_

newUnique :: SatM Unique
newUnique = getUniqueUs
\end{code}


%************************************************************************

                Static Argument Transformation Monad

%************************************************************************

To do the transformation, the game plan is to:

1. Create a small nonrecursive RHS that takes the
   original arguments to the function but discards
   the ones that are static and makes a call to the
   SATed version with the remainder. We intend that
   this will be inlined later, removing the overhead

2. Bind this nonrecursive RHS over the original body
   WITH THE SAME UNIQUE as the original body so that
   any recursive calls to the original now go via
   the small wrapper

3. Rebind the original function to a new one which contains
   our SATed function and just makes a call to it:
   we call the thing making this call the local body

Example: transform this

    map :: forall a b. (a->b) -> [a] -> [b]
    map = /\ab. \(f:a->b) (as:[a]) -> body[map]
to
    map :: forall a b. (a->b) -> [a] -> [b]
    map = /\ab. \(f:a->b) (as:[a]) ->
         letrec map' :: [a] -> [b]
		    -- The "worker function
                map' = \(as:[a]) -> 
			 let map :: forall a' b'. (a -> b) -> [a] -> [b]
				-- The "shadow function
			     map = /\a'b'. \(f':(a->b) (as:[a]).
				   map' as
	                 in body[map]
         in map' as

Note [Shadow binding]
~~~~~~~~~~~~~~~~~~~~~
The calls to the inner map inside body[map] should get inlined
by the local re-binding of 'map'.  We call this the "shadow binding".

But we can't use the original binder 'map' unchanged, because
it might be exported, in which case the shadow binding won't be
discarded as dead code after it is inlined.

So we use a hack: we make a new SysLocal binder with the *same* unique
as binder.  (Another alternative would be to reset the export flag.)

Note [Binder type capture]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that in the inner map (the "shadow function"), the static arguments
are discarded -- it's as if they were underscores.  Instead, mentions
of these arguments (notably in the types of dynamic arguments) are bound
by the *outer* lambdas of the main function.  So we must make up fresh
names for the static arguments so that they do not capture variables 
mentioned in the types of dynamic args.  

In the map example, the shadow function must clone the static type
argument a,b, giving a',b', to ensure that in the \(as:[a]), the 'a'
is bound by the outer forall.  We clone f' too for consistency, but
that doesn't matter either way because static Id arguments aren't 
mentioned in the shadow binding at all.

If we don't we get something like this:

[Exported]
[Arity 3]
GHC.Base.until =
  \ (@ a_aiK)
    (p_a6T :: a_aiK -> GHC.Types.Bool)
    (f_a6V :: a_aiK -> a_aiK)
    (x_a6X :: a_aiK) ->
    letrec {
      sat_worker_s1aU :: a_aiK -> a_aiK
      []
      sat_worker_s1aU =
        \ (x_a6X :: a_aiK) ->
          let {
            sat_shadow_r17 :: forall a_a3O.
                              (a_a3O -> GHC.Types.Bool) -> (a_a3O -> a_a3O) -> a_a3O -> a_a3O
            []
            sat_shadow_r17 =
              \ (@ a_aiK)
                (p_a6T :: a_aiK -> GHC.Types.Bool)
                (f_a6V :: a_aiK -> a_aiK)
                (x_a6X :: a_aiK) ->
                sat_worker_s1aU x_a6X } in
          case p_a6T x_a6X of wild_X3y [ALWAYS Dead Nothing] {
            GHC.Types.False -> GHC.Base.until @ a_aiK p_a6T f_a6V (f_a6V x_a6X);
            GHC.Types.True -> x_a6X
          }; } in
    sat_worker_s1aU x_a6X
    
Where sat_shadow has captured the type variables of x_a6X etc as it has a a_aiK 
type argument. This is bad because it means the application sat_worker_s1aU x_a6X
is not well typed.

\begin{code}
saTransformMaybe :: Id -> Maybe SATInfo -> [Id] -> CoreExpr -> SatM CoreBind
saTransformMaybe binder maybe_arg_staticness rhs_binders rhs_body
  | Just arg_staticness <- maybe_arg_staticness
  , should_transform arg_staticness
  = saTransform binder arg_staticness rhs_binders rhs_body
  | otherwise
  = return (Rec [(binder, mkLams rhs_binders rhs_body)])
  where 
    should_transform staticness = n_static_args > 1 -- THIS IS THE DECISION POINT
      where
	n_static_args = length (filter isStaticValue staticness)

saTransform :: Id -> SATInfo -> [Id] -> CoreExpr -> SatM CoreBind
saTransform binder arg_staticness rhs_binders rhs_body
  = do	{ shadow_lam_bndrs <- mapM clone binders_w_staticness
	; uniq	 	   <- newUnique
	; return (NonRec binder (mk_new_rhs uniq shadow_lam_bndrs)) }
  where
    -- Running example: foldr
    -- foldr \alpha \beta c n xs = e, for some e
    -- arg_staticness = [Static TypeApp, Static TypeApp, Static VarApp, Static VarApp, NonStatic]
    -- rhs_binders = [\alpha, \beta, c, n, xs]
    -- rhs_body = e
    
    binders_w_staticness = rhs_binders `zip` (arg_staticness ++ repeat NotStatic)
					-- Any extra args are assumed NotStatic

    non_static_args :: [Var]
	    -- non_static_args = [xs]
	    -- rhs_binders_without_type_capture = [\alpha', \beta', c, n, xs]
    non_static_args = [v | (v, NotStatic) <- binders_w_staticness]

    clone (bndr, NotStatic) = return bndr
    clone (bndr, _        ) = do { uniq <- newUnique
				 ; return (setVarUnique bndr uniq) }

    -- new_rhs = \alpha beta c n xs -> 
    --           let sat_worker = \xs -> let sat_shadow = \alpha' beta' c n xs -> 
    -- 					     sat_worker xs 
    --                                   in e
    --           in sat_worker xs
    mk_new_rhs uniq shadow_lam_bndrs 
	= mkLams rhs_binders $ 
	  Let (Rec [(rec_body_bndr, rec_body)]) 
	  local_body
	where
	  local_body = mkVarApps (Var rec_body_bndr) non_static_args

	  rec_body = mkLams non_static_args $
                     Let (NonRec shadow_bndr shadow_rhs) rhs_body

	    -- See Note [Binder type capture]
	  shadow_rhs = mkLams shadow_lam_bndrs local_body
	    -- nonrec_rhs = \alpha' beta' c n xs -> sat_worker xs

	  rec_body_bndr = mkSysLocal (fsLit "sat_worker") uniq (exprType rec_body)
	    -- rec_body_bndr = sat_worker
    
	    -- See Note [Shadow binding]; make a SysLocal
	  shadow_bndr = mkSysLocal (occNameFS (getOccName binder)) 
				   (idUnique binder)
				   (exprType shadow_rhs)

isStaticValue :: Staticness App -> Bool
isStaticValue (Static (VarApp _)) = True
isStaticValue _                   = False

\end{code}
