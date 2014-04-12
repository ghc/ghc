%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Desugaring arrow commands

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module DsArrows ( dsProcExpr ) where

#include "HsVersions.h"

import Match
import DsUtils
import DsMonad

import HsSyn	hiding (collectPatBinders, collectPatsBinders, collectLStmtsBinders, collectLStmtBinders, collectStmtBinders )
import TcHsSyn

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types (newtypes etc), and sometimes not
--     So WATCH OUT; check each use of split*Ty functions.
-- Sigh.  This is a pain.

import {-# SOURCE #-} DsExpr ( dsExpr, dsLExpr, dsLocalBinds )

import TcType
import TcEvidence
import CoreSyn
import CoreFVs
import CoreUtils
import MkCore
import DsBinds (dsHsWrapper)

import Name
import Var
import Id
import DataCon
import TysWiredIn
import BasicTypes
import PrelNames
import Outputable
import Bag
import VarSet
import SrcLoc
import ListSetOps( assocDefault )
import FastString
import Data.List
\end{code}

\begin{code}
data DsCmdEnv = DsCmdEnv {
	arr_id, compose_id, first_id, app_id, choice_id, loop_id :: CoreExpr
    }

mkCmdEnv :: CmdSyntaxTable Id -> DsM ([CoreBind], DsCmdEnv)
-- See Note [CmdSyntaxTable] in HsExpr
mkCmdEnv tc_meths
  = do { (meth_binds, prs) <- mapAndUnzipM mk_bind tc_meths
       ; return (meth_binds, DsCmdEnv {
               arr_id     = Var (find_meth prs arrAName),
               compose_id = Var (find_meth prs composeAName),
               first_id   = Var (find_meth prs firstAName),
               app_id     = Var (find_meth prs appAName),
               choice_id  = Var (find_meth prs choiceAName),
               loop_id    = Var (find_meth prs loopAName)
             }) }
  where
    mk_bind (std_name, expr)
      = do { rhs <- dsExpr expr
           ; id <- newSysLocalDs (exprType rhs)
           ; return (NonRec id rhs, (std_name, id)) }
 
    find_meth prs std_name
      = assocDefault (mk_panic std_name) prs std_name
    mk_panic std_name = pprPanic "mkCmdEnv" (ptext (sLit "Not found:") <+> ppr std_name)

-- arr :: forall b c. (b -> c) -> a b c
do_arr :: DsCmdEnv -> Type -> Type -> CoreExpr -> CoreExpr
do_arr ids b_ty c_ty f = mkApps (arr_id ids) [Type b_ty, Type c_ty, f]

-- (>>>) :: forall b c d. a b c -> a c d -> a b d
do_compose :: DsCmdEnv -> Type -> Type -> Type ->
		CoreExpr -> CoreExpr -> CoreExpr
do_compose ids b_ty c_ty d_ty f g
  = mkApps (compose_id ids) [Type b_ty, Type c_ty, Type d_ty, f, g]

-- first :: forall b c d. a b c -> a (b,d) (c,d)
do_first :: DsCmdEnv -> Type -> Type -> Type -> CoreExpr -> CoreExpr
do_first ids b_ty c_ty d_ty f
  = mkApps (first_id ids) [Type b_ty, Type c_ty, Type d_ty, f]

-- app :: forall b c. a (a b c, b) c
do_app :: DsCmdEnv -> Type -> Type -> CoreExpr
do_app ids b_ty c_ty = mkApps (app_id ids) [Type b_ty, Type c_ty]

-- (|||) :: forall b d c. a b d -> a c d -> a (Either b c) d
-- note the swapping of d and c
do_choice :: DsCmdEnv -> Type -> Type -> Type ->
		CoreExpr -> CoreExpr -> CoreExpr
do_choice ids b_ty c_ty d_ty f g
  = mkApps (choice_id ids) [Type b_ty, Type d_ty, Type c_ty, f, g]

-- loop :: forall b d c. a (b,d) (c,d) -> a b c
-- note the swapping of d and c
do_loop :: DsCmdEnv -> Type -> Type -> Type -> CoreExpr -> CoreExpr
do_loop ids b_ty c_ty d_ty f
  = mkApps (loop_id ids) [Type b_ty, Type d_ty, Type c_ty, f]

-- premap :: forall b c d. (b -> c) -> a c d -> a b d
-- premap f g = arr f >>> g
do_premap :: DsCmdEnv -> Type -> Type -> Type ->
		CoreExpr -> CoreExpr -> CoreExpr
do_premap ids b_ty c_ty d_ty f g
   = do_compose ids b_ty c_ty d_ty (do_arr ids b_ty c_ty f) g

mkFailExpr :: HsMatchContext Id -> Type -> DsM CoreExpr
mkFailExpr ctxt ty
  = mkErrorAppDs pAT_ERROR_ID ty (matchContextErrString ctxt)

-- construct CoreExpr for \ (a :: a_ty, b :: b_ty) -> a
mkFstExpr :: Type -> Type -> DsM CoreExpr
mkFstExpr a_ty b_ty = do
    a_var <- newSysLocalDs a_ty
    b_var <- newSysLocalDs b_ty
    pair_var <- newSysLocalDs (mkCorePairTy a_ty b_ty)
    return (Lam pair_var
               (coreCasePair pair_var a_var b_var (Var a_var)))

-- construct CoreExpr for \ (a :: a_ty, b :: b_ty) -> b
mkSndExpr :: Type -> Type -> DsM CoreExpr
mkSndExpr a_ty b_ty = do
    a_var <- newSysLocalDs a_ty
    b_var <- newSysLocalDs b_ty
    pair_var <- newSysLocalDs (mkCorePairTy a_ty b_ty)
    return (Lam pair_var
               (coreCasePair pair_var a_var b_var (Var b_var)))
\end{code}

Build case analysis of a tuple.  This cannot be done in the DsM monad,
because the list of variables is typically not yet defined.

\begin{code}
-- coreCaseTuple [u1..] v [x1..xn] body
--	= case v of v { (x1, .., xn) -> body }
-- But the matching may be nested if the tuple is very big

coreCaseTuple :: UniqSupply -> Id -> [Id] -> CoreExpr -> CoreExpr
coreCaseTuple uniqs scrut_var vars body
  = mkTupleCase uniqs vars body scrut_var (Var scrut_var)

coreCasePair :: Id -> Id -> Id -> CoreExpr -> CoreExpr
coreCasePair scrut_var var1 var2 body
  = Case (Var scrut_var) scrut_var (exprType body)
         [(DataAlt (tupleCon BoxedTuple 2), [var1, var2], body)]
\end{code}

\begin{code}
mkCorePairTy :: Type -> Type -> Type
mkCorePairTy t1 t2 = mkBoxedTupleTy [t1, t2]

mkCorePairExpr :: CoreExpr -> CoreExpr -> CoreExpr
mkCorePairExpr e1 e2 = mkCoreTup [e1, e2]

mkCoreUnitExpr :: CoreExpr
mkCoreUnitExpr = mkCoreTup []
\end{code}

The input is divided into a local environment, which is a flat tuple
(unless it's too big), and a stack, which is a right-nested pair.
In general, the input has the form

	((x1,...,xn), (s1,...(sk,())...))

where xi are the environment values, and si the ones on the stack,
with s1 being the "top", the first one to be matched with a lambda.

\begin{code}
envStackType :: [Id] -> Type -> Type
envStackType ids stack_ty = mkCorePairTy (mkBigCoreVarTupTy ids) stack_ty

-- splitTypeAt n (t1,... (tn,t)...) = ([t1, ..., tn], t)
splitTypeAt :: Int -> Type -> ([Type], Type)
splitTypeAt n ty
  | n == 0 = ([], ty)
  | otherwise = case tcTyConAppArgs ty of
      [t, ty'] -> let (ts, ty_r) = splitTypeAt (n-1) ty' in (t:ts, ty_r)
      _ -> pprPanic "splitTypeAt" (ppr ty)

----------------------------------------------
--		buildEnvStack
--
--	((x1,...,xn),stk)

buildEnvStack :: [Id] -> Id -> CoreExpr
buildEnvStack env_ids stack_id
  = mkCorePairExpr (mkBigCoreVarTup env_ids) (Var stack_id)

----------------------------------------------
-- 		matchEnvStack
--
--	\ ((x1,...,xn),stk) -> body
--	=>
--	\ pair ->
--	case pair of (tup,stk) ->
--	case tup of (x1,...,xn) ->
--	body

matchEnvStack	:: [Id] 	-- x1..xn
		-> Id	 	-- stk
		-> CoreExpr 	-- e
		-> DsM CoreExpr
matchEnvStack env_ids stack_id body = do
    uniqs <- newUniqueSupply
    tup_var <- newSysLocalDs (mkBigCoreVarTupTy env_ids)
    let match_env = coreCaseTuple uniqs tup_var env_ids body
    pair_id <- newSysLocalDs (mkCorePairTy (idType tup_var) (idType stack_id))
    return (Lam pair_id (coreCasePair pair_id tup_var stack_id match_env))

----------------------------------------------
-- 		matchEnv
--
--	\ (x1,...,xn) -> body
--	=>
--	\ tup ->
--	case tup of (x1,...,xn) ->
--	body

matchEnv :: [Id] 	-- x1..xn
	 -> CoreExpr 	-- e
	 -> DsM CoreExpr
matchEnv env_ids body = do
    uniqs <- newUniqueSupply
    tup_id <- newSysLocalDs (mkBigCoreVarTupTy env_ids)
    return (Lam tup_id (coreCaseTuple uniqs tup_id env_ids body))

----------------------------------------------
--		matchVarStack
--
--	case (x1, ...(xn, s)...) -> e
--	=>
--	case z0 of (x1,z1) ->
--	case zn-1 of (xn,s) ->
--	e
matchVarStack :: [Id] -> Id -> CoreExpr -> DsM (Id, CoreExpr)
matchVarStack [] stack_id body = return (stack_id, body)
matchVarStack (param_id:param_ids) stack_id body = do
    (tail_id, tail_code) <- matchVarStack param_ids stack_id body
    pair_id <- newSysLocalDs (mkCorePairTy (idType param_id) (idType tail_id))
    return (pair_id, coreCasePair pair_id param_id tail_id tail_code)
\end{code}

\begin{code}
mkHsEnvStackExpr :: [Id] -> Id -> LHsExpr Id
mkHsEnvStackExpr env_ids stack_id
  = mkLHsTupleExpr [mkLHsVarTuple env_ids, nlHsVar stack_id]
\end{code}

Translation of arrow abstraction

\begin{code}

-- D; xs |-a c : () --> t'  	---> c'
-- --------------------------
-- D |- proc p -> c :: a t t'	---> premap (\ p -> ((xs),())) c'
--
--		where (xs) is the tuple of variables bound by p

dsProcExpr
	:: LPat Id
	-> LHsCmdTop Id
	-> DsM CoreExpr
dsProcExpr pat (L _ (HsCmdTop cmd _unitTy cmd_ty ids)) = do
    (meth_binds, meth_ids) <- mkCmdEnv ids
    let locals = mkVarSet (collectPatBinders pat)
    (core_cmd, _free_vars, env_ids) <- dsfixCmd meth_ids locals unitTy cmd_ty cmd
    let env_ty = mkBigCoreVarTupTy env_ids
    let env_stk_ty = mkCorePairTy env_ty unitTy
    let env_stk_expr = mkCorePairExpr (mkBigCoreVarTup env_ids) mkCoreUnitExpr
    fail_expr <- mkFailExpr ProcExpr env_stk_ty
    var <- selectSimpleMatchVarL pat
    match_code <- matchSimply (Var var) ProcExpr pat env_stk_expr fail_expr
    let pat_ty = hsLPatType pat
        proc_code = do_premap meth_ids pat_ty env_stk_ty cmd_ty
                    (Lam var match_code)
                    core_cmd
    return (mkLets meth_binds proc_code)
\end{code}

Translation of a command judgement of the form

	D; xs |-a c : stk --> t

to an expression e such that

	D |- e :: a (xs, stk) t

\begin{code}
dsLCmd :: DsCmdEnv -> IdSet -> Type -> Type -> LHsCmd Id -> [Id]
       -> DsM (CoreExpr, IdSet)
dsLCmd ids local_vars stk_ty res_ty cmd env_ids
  = dsCmd ids local_vars stk_ty res_ty (unLoc cmd) env_ids

dsCmd   :: DsCmdEnv		-- arrow combinators
	-> IdSet		-- set of local vars available to this command
	-> Type			-- type of the stack (right-nested tuple)
	-> Type			-- return type of the command
	-> HsCmd Id		-- command to desugar
	-> [Id]			-- list of vars in the input to this command
				-- This is typically fed back,
				-- so don't pull on it too early
	-> DsM (CoreExpr,	-- desugared expression
		IdSet)		-- subset of local vars that occur free

-- D |- fun :: a t1 t2
-- D, xs |- arg :: t1
-- -----------------------------
-- D; xs |-a fun -< arg : stk --> t2
--
--		---> premap (\ ((xs), _stk) -> arg) fun

dsCmd ids local_vars stack_ty res_ty
        (HsCmdArrApp arrow arg arrow_ty HsFirstOrderApp _)
        env_ids = do
    let
        (a_arg_ty, _res_ty') = tcSplitAppTy arrow_ty
        (_a_ty, arg_ty) = tcSplitAppTy a_arg_ty
    core_arrow <- dsLExpr arrow
    core_arg   <- dsLExpr arg
    stack_id   <- newSysLocalDs stack_ty
    core_make_arg <- matchEnvStack env_ids stack_id core_arg
    return (do_premap ids
              (envStackType env_ids stack_ty)
              arg_ty
              res_ty
              core_make_arg
              core_arrow,
            exprFreeIds core_arg `intersectVarSet` local_vars)

-- D, xs |- fun :: a t1 t2
-- D, xs |- arg :: t1
-- ------------------------------
-- D; xs |-a fun -<< arg : stk --> t2
--
--		---> premap (\ ((xs), _stk) -> (fun, arg)) app

dsCmd ids local_vars stack_ty res_ty
        (HsCmdArrApp arrow arg arrow_ty HsHigherOrderApp _)
        env_ids = do
    let
        (a_arg_ty, _res_ty') = tcSplitAppTy arrow_ty
        (_a_ty, arg_ty) = tcSplitAppTy a_arg_ty
    
    core_arrow <- dsLExpr arrow
    core_arg   <- dsLExpr arg
    stack_id   <- newSysLocalDs stack_ty
    core_make_pair <- matchEnvStack env_ids stack_id
          (mkCorePairExpr core_arrow core_arg)

    return (do_premap ids
              (envStackType env_ids stack_ty)
              (mkCorePairTy arrow_ty arg_ty)
              res_ty
              core_make_pair
              (do_app ids arg_ty res_ty),
            (exprFreeIds core_arrow `unionVarSet` exprFreeIds core_arg)
              `intersectVarSet` local_vars)

-- D; ys |-a cmd : (t,stk) --> t'
-- D, xs |-  exp :: t
-- ------------------------
-- D; xs |-a cmd exp : stk --> t'
--
--		---> premap (\ ((xs),stk) -> ((ys),(e,stk))) cmd

dsCmd ids local_vars stack_ty res_ty (HsCmdApp cmd arg) env_ids = do
    core_arg <- dsLExpr arg
    let
        arg_ty = exprType core_arg
        stack_ty' = mkCorePairTy arg_ty stack_ty
    (core_cmd, free_vars, env_ids')
             <- dsfixCmd ids local_vars stack_ty' res_ty cmd
    stack_id <- newSysLocalDs stack_ty
    arg_id <- newSysLocalDs arg_ty
    -- push the argument expression onto the stack
    let
	stack' = mkCorePairExpr (Var arg_id) (Var stack_id)
        core_body = bindNonRec arg_id core_arg
			(mkCorePairExpr (mkBigCoreVarTup env_ids') stack')

    -- match the environment and stack against the input
    core_map <- matchEnvStack env_ids stack_id core_body
    return (do_premap ids
                      (envStackType env_ids stack_ty)
                      (envStackType env_ids' stack_ty')
                      res_ty
                      core_map
                      core_cmd,
            free_vars `unionVarSet`
              (exprFreeIds core_arg `intersectVarSet` local_vars))

-- D; ys |-a cmd : stk t'
-- -----------------------------------------------
-- D; xs |-a \ p1 ... pk -> cmd : (t1,...(tk,stk)...) t'
--
--		---> premap (\ ((xs), (p1, ... (pk,stk)...)) -> ((ys),stk)) cmd

dsCmd ids local_vars stack_ty res_ty
        (HsCmdLam (MG { mg_alts = [L _ (Match pats _ (GRHSs [L _ (GRHS [] body)] _ ))] }))
        env_ids = do
    let
        pat_vars = mkVarSet (collectPatsBinders pats)
        local_vars' = pat_vars `unionVarSet` local_vars
	(pat_tys, stack_ty') = splitTypeAt (length pats) stack_ty
    (core_body, free_vars, env_ids') <- dsfixCmd ids local_vars' stack_ty' res_ty body
    param_ids <- mapM newSysLocalDs pat_tys
    stack_id' <- newSysLocalDs stack_ty'

    -- the expression is built from the inside out, so the actions
    -- are presented in reverse order

    let
        -- build a new environment, plus what's left of the stack
        core_expr = buildEnvStack env_ids' stack_id'
        in_ty = envStackType env_ids stack_ty
        in_ty' = envStackType env_ids' stack_ty'
    
    fail_expr <- mkFailExpr LambdaExpr in_ty'
    -- match the patterns against the parameters
    match_code <- matchSimplys (map Var param_ids) LambdaExpr pats core_expr fail_expr
    -- match the parameters against the top of the old stack
    (stack_id, param_code) <- matchVarStack param_ids stack_id' match_code
    -- match the old environment and stack against the input
    select_code <- matchEnvStack env_ids stack_id param_code
    return (do_premap ids in_ty in_ty' res_ty select_code core_body,
            free_vars `minusVarSet` pat_vars)

dsCmd ids local_vars stack_ty res_ty (HsCmdPar cmd) env_ids
  = dsLCmd ids local_vars stack_ty res_ty cmd env_ids

-- D, xs |- e :: Bool
-- D; xs1 |-a c1 : stk --> t
-- D; xs2 |-a c2 : stk --> t
-- ----------------------------------------
-- D; xs |-a if e then c1 else c2 : stk --> t
--
--		---> premap (\ ((xs),stk) ->
--			 if e then Left ((xs1),stk) else Right ((xs2),stk))
--		       (c1 ||| c2)

dsCmd ids local_vars stack_ty res_ty (HsCmdIf mb_fun cond then_cmd else_cmd)
        env_ids = do
    core_cond <- dsLExpr cond
    (core_then, fvs_then, then_ids) <- dsfixCmd ids local_vars stack_ty res_ty then_cmd
    (core_else, fvs_else, else_ids) <- dsfixCmd ids local_vars stack_ty res_ty else_cmd
    stack_id   <- newSysLocalDs stack_ty
    either_con <- dsLookupTyCon eitherTyConName
    left_con   <- dsLookupDataCon leftDataConName
    right_con  <- dsLookupDataCon rightDataConName

    let mk_left_expr ty1 ty2 e = mkConApp left_con [Type ty1, Type ty2, e]
        mk_right_expr ty1 ty2 e = mkConApp right_con [Type ty1, Type ty2, e]

        in_ty = envStackType env_ids stack_ty
        then_ty = envStackType then_ids stack_ty
        else_ty = envStackType else_ids stack_ty
        sum_ty = mkTyConApp either_con [then_ty, else_ty]
        fvs_cond = exprFreeIds core_cond `intersectVarSet` local_vars
        
        core_left  = mk_left_expr  then_ty else_ty (buildEnvStack then_ids stack_id)
        core_right = mk_right_expr then_ty else_ty (buildEnvStack else_ids stack_id)

    core_if <- case mb_fun of 
       Just fun -> do { core_fun <- dsExpr fun
                      ; matchEnvStack env_ids stack_id $
                        mkCoreApps core_fun [core_cond, core_left, core_right] }
       Nothing  -> matchEnvStack env_ids stack_id $
                   mkIfThenElse core_cond core_left core_right

    return (do_premap ids in_ty sum_ty res_ty
                core_if
                (do_choice ids then_ty else_ty res_ty core_then core_else),
        fvs_cond `unionVarSet` fvs_then `unionVarSet` fvs_else)
\end{code}

Case commands are treated in much the same way as if commands
(see above) except that there are more alternatives.  For example

	case e of { p1 -> c1; p2 -> c2; p3 -> c3 }

is translated to

	premap (\ ((xs)*ts) -> case e of
		p1 -> (Left (Left (xs1)*ts))
		p2 -> Left ((Right (xs2)*ts))
		p3 -> Right ((xs3)*ts))
	((c1 ||| c2) ||| c3)

The idea is to extract the commands from the case, build a balanced tree
of choices, and replace the commands with expressions that build tagged
tuples, obtaining a case expression that can be desugared normally.
To build all this, we use triples describing segments of the list of
case bodies, containing the following fields:
 * a list of expressions of the form (Left|Right)* ((xs)*ts), to be put
   into the case replacing the commands
 * a sum type that is the common type of these expressions, and also the
   input type of the arrow
 * a CoreExpr for an arrow built by combining the translated command
   bodies with |||.

\begin{code}
dsCmd ids local_vars stack_ty res_ty 
      (HsCmdCase exp (MG { mg_alts = matches, mg_arg_tys = arg_tys, mg_origin = origin }))
      env_ids = do
    stack_id <- newSysLocalDs stack_ty

    -- Extract and desugar the leaf commands in the case, building tuple
    -- expressions that will (after tagging) replace these leaves

    let
        leaves = concatMap leavesMatch matches
        make_branch (leaf, bound_vars) = do
            (core_leaf, _fvs, leaf_ids) <-
                  dsfixCmd ids (bound_vars `unionVarSet` local_vars) stack_ty res_ty leaf
            return ([mkHsEnvStackExpr leaf_ids stack_id],
                    envStackType leaf_ids stack_ty,
                    core_leaf)
    
    branches <- mapM make_branch leaves
    either_con <- dsLookupTyCon eitherTyConName
    left_con <- dsLookupDataCon leftDataConName
    right_con <- dsLookupDataCon rightDataConName
    let
        left_id  = HsVar (dataConWrapId left_con)
        right_id = HsVar (dataConWrapId right_con)
        left_expr  ty1 ty2 e = noLoc $ HsApp (noLoc $ HsWrap (mkWpTyApps [ty1, ty2]) left_id ) e
        right_expr ty1 ty2 e = noLoc $ HsApp (noLoc $ HsWrap (mkWpTyApps [ty1, ty2]) right_id) e

        -- Prefix each tuple with a distinct series of Left's and Right's,
        -- in a balanced way, keeping track of the types.

        merge_branches (builds1, in_ty1, core_exp1)
                       (builds2, in_ty2, core_exp2)
          = (map (left_expr in_ty1 in_ty2) builds1 ++
                map (right_expr in_ty1 in_ty2) builds2,
             mkTyConApp either_con [in_ty1, in_ty2],
             do_choice ids in_ty1 in_ty2 res_ty core_exp1 core_exp2)
        (leaves', sum_ty, core_choices) = foldb merge_branches branches

        -- Replace the commands in the case with these tagged tuples,
        -- yielding a HsExpr Id we can feed to dsExpr.

        (_, matches') = mapAccumL (replaceLeavesMatch res_ty) leaves' matches
        in_ty = envStackType env_ids stack_ty

    core_body <- dsExpr (HsCase exp (MG { mg_alts = matches', mg_arg_tys = arg_tys
                                        , mg_res_ty = sum_ty, mg_origin = origin }))
        -- Note that we replace the HsCase result type by sum_ty,
        -- which is the type of matches'

    core_matches <- matchEnvStack env_ids stack_id core_body
    return (do_premap ids in_ty sum_ty res_ty core_matches core_choices,
            exprFreeIds core_body  `intersectVarSet` local_vars)

-- D; ys |-a cmd : stk --> t
-- ----------------------------------
-- D; xs |-a let binds in cmd : stk --> t
--
--		---> premap (\ ((xs),stk) -> let binds in ((ys),stk)) c

dsCmd ids local_vars stack_ty res_ty (HsCmdLet binds body) env_ids = do
    let
        defined_vars = mkVarSet (collectLocalBinders binds)
        local_vars' = defined_vars `unionVarSet` local_vars
    
    (core_body, _free_vars, env_ids') <- dsfixCmd ids local_vars' stack_ty res_ty body
    stack_id <- newSysLocalDs stack_ty
    -- build a new environment, plus the stack, using the let bindings
    core_binds <- dsLocalBinds binds (buildEnvStack env_ids' stack_id)
    -- match the old environment and stack against the input
    core_map <- matchEnvStack env_ids stack_id core_binds
    return (do_premap ids
                        (envStackType env_ids stack_ty)
                        (envStackType env_ids' stack_ty)
                        res_ty
                        core_map
                        core_body,
        exprFreeIds core_binds `intersectVarSet` local_vars)

-- D; xs |-a ss : t
-- ----------------------------------
-- D; xs |-a do { ss } : () --> t
--
--		---> premap (\ (env,stk) -> env) c

dsCmd ids local_vars stack_ty res_ty (HsCmdDo stmts _) env_ids = do
    (core_stmts, env_ids') <- dsCmdDo ids local_vars res_ty stmts env_ids
    let env_ty = mkBigCoreVarTupTy env_ids
    core_fst <- mkFstExpr env_ty stack_ty
    return (do_premap ids
		(mkCorePairTy env_ty stack_ty)
		env_ty
		res_ty
		core_fst
		core_stmts,
	env_ids')

-- D |- e :: forall e. a1 (e,stk1) t1 -> ... an (e,stkn) tn -> a (e,stk) t
-- D; xs |-a ci :: stki --> ti
-- -----------------------------------
-- D; xs |-a (|e c1 ... cn|) :: stk --> t	---> e [t_xs] c1 ... cn

dsCmd _ids local_vars _stack_ty _res_ty (HsCmdArrForm op _ args) env_ids = do
    let env_ty = mkBigCoreVarTupTy env_ids
    core_op <- dsLExpr op
    (core_args, fv_sets) <- mapAndUnzipM (dsTrimCmdArg local_vars env_ids) args
    return (mkApps (App core_op (Type env_ty)) core_args,
            unionVarSets fv_sets)

dsCmd ids local_vars stack_ty res_ty (HsCmdCast coercion cmd) env_ids = do
    (core_cmd, env_ids') <- dsCmd ids local_vars stack_ty res_ty cmd env_ids
    wrapped_cmd <- dsHsWrapper (mkWpCast coercion) core_cmd
    return (wrapped_cmd, env_ids')

dsCmd _ _ _ _ _ c = pprPanic "dsCmd" (ppr c)

-- D; ys |-a c : stk --> t	(ys <= xs)
-- ---------------------
-- D; xs |-a c : stk --> t	---> premap (\ ((xs),stk) -> ((ys),stk)) c

dsTrimCmdArg
	:: IdSet		-- set of local vars available to this command
	-> [Id]			-- list of vars in the input to this command
	-> LHsCmdTop Id		-- command argument to desugar
	-> DsM (CoreExpr,	-- desugared expression
		IdSet)		-- subset of local vars that occur free
dsTrimCmdArg local_vars env_ids (L _ (HsCmdTop cmd stack_ty cmd_ty ids)) = do
    (meth_binds, meth_ids) <- mkCmdEnv ids
    (core_cmd, free_vars, env_ids') <- dsfixCmd meth_ids local_vars stack_ty cmd_ty cmd
    stack_id <- newSysLocalDs stack_ty
    trim_code <- matchEnvStack env_ids stack_id (buildEnvStack env_ids' stack_id)
    let
        in_ty = envStackType env_ids stack_ty
        in_ty' = envStackType env_ids' stack_ty
        arg_code = if env_ids' == env_ids then core_cmd else
                do_premap meth_ids in_ty in_ty' cmd_ty trim_code core_cmd
    return (mkLets meth_binds arg_code, free_vars)

-- Given D; xs |-a c : stk --> t, builds c with xs fed back.
-- Typically needs to be prefixed with arr (\(p, stk) -> ((xs),stk))

dsfixCmd
	:: DsCmdEnv		-- arrow combinators
	-> IdSet		-- set of local vars available to this command
	-> Type			-- type of the stack (right-nested tuple)
	-> Type			-- return type of the command
	-> LHsCmd Id		-- command to desugar
	-> DsM (CoreExpr,	-- desugared expression
		IdSet,		-- subset of local vars that occur free
		[Id])		-- the same local vars as a list, fed back
dsfixCmd ids local_vars stk_ty cmd_ty cmd
  = trimInput (dsLCmd ids local_vars stk_ty cmd_ty cmd)

-- Feed back the list of local variables actually used a command,
-- for use as the input tuple of the generated arrow.

trimInput
	:: ([Id] -> DsM (CoreExpr, IdSet))
	-> DsM (CoreExpr,	-- desugared expression
		IdSet,		-- subset of local vars that occur free
		[Id])		-- same local vars as a list, fed back to
				-- the inner function to form the tuple of
				-- inputs to the arrow.
trimInput build_arrow
  = fixDs (\ ~(_,_,env_ids) -> do
        (core_cmd, free_vars) <- build_arrow env_ids
        return (core_cmd, free_vars, varSetElems free_vars))

\end{code}

Translation of command judgements of the form

	D |-a do { ss } : t

\begin{code}

dsCmdDo :: DsCmdEnv		-- arrow combinators
	-> IdSet		-- set of local vars available to this statement
	-> Type			-- return type of the statement
	-> [CmdLStmt Id]        -- statements to desugar
	-> [Id]			-- list of vars in the input to this statement
				-- This is typically fed back,
				-- so don't pull on it too early
	-> DsM (CoreExpr,	-- desugared expression
		IdSet)		-- subset of local vars that occur free

dsCmdDo _ _ _ [] _ = panic "dsCmdDo"

-- D; xs |-a c : () --> t
-- --------------------------
-- D; xs |-a do { c } : t
--
--		---> premap (\ (xs) -> ((xs), ())) c

dsCmdDo ids local_vars res_ty [L _ (LastStmt body _)] env_ids = do
    (core_body, env_ids') <- dsLCmd ids local_vars unitTy res_ty body env_ids
    let env_ty = mkBigCoreVarTupTy env_ids
    env_var <- newSysLocalDs env_ty
    let core_map = Lam env_var (mkCorePairExpr (Var env_var) mkCoreUnitExpr)
    return (do_premap ids
                        env_ty
			(mkCorePairTy env_ty unitTy)
                        res_ty
                        core_map
                        core_body,
	env_ids')

dsCmdDo ids local_vars res_ty (stmt:stmts) env_ids = do
    let
        bound_vars = mkVarSet (collectLStmtBinders stmt)
        local_vars' = bound_vars `unionVarSet` local_vars
    (core_stmts, _, env_ids') <- trimInput (dsCmdDo ids local_vars' res_ty stmts)
    (core_stmt, fv_stmt) <- dsCmdLStmt ids local_vars env_ids' stmt env_ids
    return (do_compose ids
                (mkBigCoreVarTupTy env_ids)
                (mkBigCoreVarTupTy env_ids')
                res_ty
                core_stmt
                core_stmts,
              fv_stmt)

\end{code}
A statement maps one local environment to another, and is represented
as an arrow from one tuple type to another.  A statement sequence is
translated to a composition of such arrows.
\begin{code}
dsCmdLStmt :: DsCmdEnv -> IdSet -> [Id] -> CmdLStmt Id -> [Id]
           -> DsM (CoreExpr, IdSet)
dsCmdLStmt ids local_vars out_ids cmd env_ids
  = dsCmdStmt ids local_vars out_ids (unLoc cmd) env_ids

dsCmdStmt
	:: DsCmdEnv		-- arrow combinators
	-> IdSet		-- set of local vars available to this statement
	-> [Id]			-- list of vars in the output of this statement
	-> CmdStmt Id           -- statement to desugar
	-> [Id]			-- list of vars in the input to this statement
				-- This is typically fed back,
				-- so don't pull on it too early
	-> DsM (CoreExpr,	-- desugared expression
		IdSet)		-- subset of local vars that occur free

-- D; xs1 |-a c : () --> t
-- D; xs' |-a do { ss } : t'
-- ------------------------------
-- D; xs  |-a do { c; ss } : t'
--
--		---> premap (\ ((xs)) -> (((xs1),()),(xs')))
--			(first c >>> arr snd) >>> ss

dsCmdStmt ids local_vars out_ids (BodyStmt cmd _ _ c_ty) env_ids = do
    (core_cmd, fv_cmd, env_ids1) <- dsfixCmd ids local_vars unitTy c_ty cmd
    core_mux <- matchEnv env_ids
        (mkCorePairExpr
	    (mkCorePairExpr (mkBigCoreVarTup env_ids1) mkCoreUnitExpr)
	    (mkBigCoreVarTup out_ids))
    let
	in_ty = mkBigCoreVarTupTy env_ids
	in_ty1 = mkCorePairTy (mkBigCoreVarTupTy env_ids1) unitTy
	out_ty = mkBigCoreVarTupTy out_ids
	before_c_ty = mkCorePairTy in_ty1 out_ty
	after_c_ty = mkCorePairTy c_ty out_ty
    snd_fn <- mkSndExpr c_ty out_ty
    return (do_premap ids in_ty before_c_ty out_ty core_mux $
		do_compose ids before_c_ty after_c_ty out_ty
			(do_first ids in_ty1 c_ty out_ty core_cmd) $
		do_arr ids after_c_ty out_ty snd_fn,
	      extendVarSetList fv_cmd out_ids)

-- D; xs1 |-a c : () --> t
-- D; xs' |-a do { ss } : t'		xs2 = xs' - defs(p)
-- -----------------------------------
-- D; xs  |-a do { p <- c; ss } : t'
--
--		---> premap (\ (xs) -> (((xs1),()),(xs2)))
--			(first c >>> arr (\ (p, (xs2)) -> (xs'))) >>> ss
--
-- It would be simpler and more consistent to do this using second,
-- but that's likely to be defined in terms of first.

dsCmdStmt ids local_vars out_ids (BindStmt pat cmd _ _) env_ids = do
    (core_cmd, fv_cmd, env_ids1) <- dsfixCmd ids local_vars unitTy (hsLPatType pat) cmd
    let
	pat_ty = hsLPatType pat
	pat_vars = mkVarSet (collectPatBinders pat)
	env_ids2 = varSetElems (mkVarSet out_ids `minusVarSet` pat_vars)
	env_ty2 = mkBigCoreVarTupTy env_ids2

    -- multiplexing function
    --		\ (xs) -> (((xs1),()),(xs2))

    core_mux <- matchEnv env_ids
        (mkCorePairExpr
	    (mkCorePairExpr (mkBigCoreVarTup env_ids1) mkCoreUnitExpr)
	    (mkBigCoreVarTup env_ids2))

    -- projection function
    --		\ (p, (xs2)) -> (zs)

    env_id <- newSysLocalDs env_ty2
    uniqs <- newUniqueSupply
    let
	after_c_ty = mkCorePairTy pat_ty env_ty2
	out_ty = mkBigCoreVarTupTy out_ids
	body_expr = coreCaseTuple uniqs env_id env_ids2 (mkBigCoreVarTup out_ids)
    
    fail_expr <- mkFailExpr (StmtCtxt DoExpr) out_ty
    pat_id    <- selectSimpleMatchVarL pat
    match_code <- matchSimply (Var pat_id) (StmtCtxt DoExpr) pat body_expr fail_expr
    pair_id   <- newSysLocalDs after_c_ty
    let
	proj_expr = Lam pair_id (coreCasePair pair_id pat_id env_id match_code)

    -- put it all together
    let
	in_ty = mkBigCoreVarTupTy env_ids
	in_ty1 = mkCorePairTy (mkBigCoreVarTupTy env_ids1) unitTy
	in_ty2 = mkBigCoreVarTupTy env_ids2
	before_c_ty = mkCorePairTy in_ty1 in_ty2
    return (do_premap ids in_ty before_c_ty out_ty core_mux $
		do_compose ids before_c_ty after_c_ty out_ty
			(do_first ids in_ty1 pat_ty in_ty2 core_cmd) $
		do_arr ids after_c_ty out_ty proj_expr,
	      fv_cmd `unionVarSet` (mkVarSet out_ids `minusVarSet` pat_vars))

-- D; xs' |-a do { ss } : t
-- --------------------------------------
-- D; xs  |-a do { let binds; ss } : t
--
--		---> arr (\ (xs) -> let binds in (xs')) >>> ss

dsCmdStmt ids local_vars out_ids (LetStmt binds) env_ids = do
    -- build a new environment using the let bindings
    core_binds <- dsLocalBinds binds (mkBigCoreVarTup out_ids)
    -- match the old environment against the input
    core_map <- matchEnv env_ids core_binds
    return (do_arr ids
			(mkBigCoreVarTupTy env_ids)
			(mkBigCoreVarTupTy out_ids)
			core_map,
	    exprFreeIds core_binds `intersectVarSet` local_vars)

-- D; ys  |-a do { ss; returnA -< ((xs1), (ys2)) } : ...
-- D; xs' |-a do { ss' } : t
-- ------------------------------------
-- D; xs  |-a do { rec ss; ss' } : t
--
--			xs1 = xs' /\ defs(ss)
--			xs2 = xs' - defs(ss)
--			ys1 = ys - defs(ss)
--			ys2 = ys /\ defs(ss)
--
--		---> arr (\(xs) -> ((ys1),(xs2))) >>>
--			first (loop (arr (\((ys1),~(ys2)) -> (ys)) >>> ss)) >>>
--			arr (\((xs1),(xs2)) -> (xs')) >>> ss'

dsCmdStmt ids local_vars out_ids
        (RecStmt { recS_stmts = stmts
                 , recS_later_ids = later_ids, recS_rec_ids = rec_ids
                 , recS_later_rets = later_rets, recS_rec_rets = rec_rets })
        env_ids = do
    let
        env2_id_set = mkVarSet out_ids `minusVarSet` mkVarSet later_ids
        env2_ids = varSetElems env2_id_set
        env2_ty = mkBigCoreVarTupTy env2_ids

    -- post_loop_fn = \((later_ids),(env2_ids)) -> (out_ids)

    uniqs <- newUniqueSupply
    env2_id <- newSysLocalDs env2_ty
    let
        later_ty = mkBigCoreVarTupTy later_ids
        post_pair_ty = mkCorePairTy later_ty env2_ty
        post_loop_body = coreCaseTuple uniqs env2_id env2_ids (mkBigCoreVarTup out_ids)

    post_loop_fn <- matchEnvStack later_ids env2_id post_loop_body

    --- loop (...)

    (core_loop, env1_id_set, env1_ids)
               <- dsRecCmd ids local_vars stmts later_ids later_rets rec_ids rec_rets

    -- pre_loop_fn = \(env_ids) -> ((env1_ids),(env2_ids))

    let
        env1_ty = mkBigCoreVarTupTy env1_ids
        pre_pair_ty = mkCorePairTy env1_ty env2_ty
        pre_loop_body = mkCorePairExpr (mkBigCoreVarTup env1_ids)
                                        (mkBigCoreVarTup env2_ids)

    pre_loop_fn <- matchEnv env_ids pre_loop_body

    -- arr pre_loop_fn >>> first (loop (...)) >>> arr post_loop_fn

    let
        env_ty = mkBigCoreVarTupTy env_ids
        out_ty = mkBigCoreVarTupTy out_ids
        core_body = do_premap ids env_ty pre_pair_ty out_ty
                pre_loop_fn
                (do_compose ids pre_pair_ty post_pair_ty out_ty
                        (do_first ids env1_ty later_ty env2_ty
                                core_loop)
                        (do_arr ids post_pair_ty out_ty
                                post_loop_fn))

    return (core_body, env1_id_set `unionVarSet` env2_id_set)

dsCmdStmt _ _ _ _ s = pprPanic "dsCmdStmt" (ppr s)

--	loop (premap (\ ((env1_ids), ~(rec_ids)) -> (env_ids))
--	      (ss >>> arr (\ (out_ids) -> ((later_rets),(rec_rets))))) >>>

dsRecCmd
        :: DsCmdEnv		-- arrow combinators
        -> IdSet		-- set of local vars available to this statement
        -> [CmdLStmt Id]        -- list of statements inside the RecCmd
        -> [Id]			-- list of vars defined here and used later
        -> [HsExpr Id]		-- expressions corresponding to later_ids
        -> [Id]			-- list of vars fed back through the loop
        -> [HsExpr Id]		-- expressions corresponding to rec_ids
        -> DsM (CoreExpr,	-- desugared statement
                IdSet,		-- subset of local vars that occur free
                [Id])		-- same local vars as a list

dsRecCmd ids local_vars stmts later_ids later_rets rec_ids rec_rets = do
    let
        later_id_set = mkVarSet later_ids
        rec_id_set = mkVarSet rec_ids
        local_vars' = rec_id_set `unionVarSet` later_id_set `unionVarSet` local_vars

    -- mk_pair_fn = \ (out_ids) -> ((later_rets),(rec_rets))

    core_later_rets <- mapM dsExpr later_rets
    core_rec_rets <- mapM dsExpr rec_rets
    let
        -- possibly polymorphic version of vars of later_ids and rec_ids
        out_ids = varSetElems (unionVarSets (map exprFreeIds (core_later_rets ++ core_rec_rets)))
        out_ty = mkBigCoreVarTupTy out_ids

        later_tuple = mkBigCoreTup core_later_rets
        later_ty = mkBigCoreVarTupTy later_ids

        rec_tuple = mkBigCoreTup core_rec_rets
        rec_ty = mkBigCoreVarTupTy rec_ids

        out_pair = mkCorePairExpr later_tuple rec_tuple
        out_pair_ty = mkCorePairTy later_ty rec_ty

    mk_pair_fn <- matchEnv out_ids out_pair

    -- ss

    (core_stmts, fv_stmts, env_ids) <- dsfixCmdStmts ids local_vars' out_ids stmts

    -- squash_pair_fn = \ ((env1_ids), ~(rec_ids)) -> (env_ids)

    rec_id <- newSysLocalDs rec_ty
    let
        env1_id_set = fv_stmts `minusVarSet` rec_id_set
        env1_ids = varSetElems env1_id_set
        env1_ty = mkBigCoreVarTupTy env1_ids
        in_pair_ty = mkCorePairTy env1_ty rec_ty
        core_body = mkBigCoreTup (map selectVar env_ids)
          where
            selectVar v
                | v `elemVarSet` rec_id_set
                  = mkTupleSelector rec_ids v rec_id (Var rec_id)
                | otherwise = Var v

    squash_pair_fn <- matchEnvStack env1_ids rec_id core_body

    -- loop (premap squash_pair_fn (ss >>> arr mk_pair_fn))

    let
        env_ty = mkBigCoreVarTupTy env_ids
        core_loop = do_loop ids env1_ty later_ty rec_ty
                (do_premap ids in_pair_ty env_ty out_pair_ty
                        squash_pair_fn
                        (do_compose ids env_ty out_ty out_pair_ty
                                core_stmts
                                (do_arr ids out_ty out_pair_ty mk_pair_fn)))

    return (core_loop, env1_id_set, env1_ids)

\end{code}
A sequence of statements (as in a rec) is desugared to an arrow between
two environments (no stack)
\begin{code}

dsfixCmdStmts
	:: DsCmdEnv		-- arrow combinators
	-> IdSet		-- set of local vars available to this statement
	-> [Id]			-- output vars of these statements
	-> [CmdLStmt Id]        -- statements to desugar
	-> DsM (CoreExpr,	-- desugared expression
		IdSet,		-- subset of local vars that occur free
		[Id])		-- same local vars as a list

dsfixCmdStmts ids local_vars out_ids stmts
  = trimInput (dsCmdStmts ids local_vars out_ids stmts)

dsCmdStmts
	:: DsCmdEnv		-- arrow combinators
	-> IdSet		-- set of local vars available to this statement
	-> [Id]			-- output vars of these statements
	-> [CmdLStmt Id]        -- statements to desugar
	-> [Id]			-- list of vars in the input to these statements
	-> DsM (CoreExpr,	-- desugared expression
		IdSet)		-- subset of local vars that occur free

dsCmdStmts ids local_vars out_ids [stmt] env_ids
  = dsCmdLStmt ids local_vars out_ids stmt env_ids

dsCmdStmts ids local_vars out_ids (stmt:stmts) env_ids = do
    let
        bound_vars = mkVarSet (collectLStmtBinders stmt)
        local_vars' = bound_vars `unionVarSet` local_vars
    (core_stmts, _fv_stmts, env_ids') <- dsfixCmdStmts ids local_vars' out_ids stmts
    (core_stmt, fv_stmt) <- dsCmdLStmt ids local_vars env_ids' stmt env_ids
    return (do_compose ids
                (mkBigCoreVarTupTy env_ids)
                (mkBigCoreVarTupTy env_ids')
                (mkBigCoreVarTupTy out_ids)
                core_stmt
                core_stmts,
              fv_stmt)

dsCmdStmts _ _ _ [] _ = panic "dsCmdStmts []"
\end{code}

Match a list of expressions against a list of patterns, left-to-right.

\begin{code}
matchSimplys :: [CoreExpr]              -- Scrutinees
	     -> HsMatchContext Name	-- Match kind
	     -> [LPat Id]         	-- Patterns they should match
	     -> CoreExpr                -- Return this if they all match
	     -> CoreExpr                -- Return this if they don't
	     -> DsM CoreExpr
matchSimplys [] _ctxt [] result_expr _fail_expr = return result_expr
matchSimplys (exp:exps) ctxt (pat:pats) result_expr fail_expr = do
    match_code <- matchSimplys exps ctxt pats result_expr fail_expr
    matchSimply exp ctxt pat match_code fail_expr
matchSimplys _ _ _ _ _ = panic "matchSimplys"
\end{code}

List of leaf expressions, with set of variables bound in each

\begin{code}
leavesMatch :: LMatch Id (Located (body Id)) -> [(Located (body Id), IdSet)]
leavesMatch (L _ (Match pats _ (GRHSs grhss binds)))
  = let
	defined_vars = mkVarSet (collectPatsBinders pats)
			`unionVarSet`
		       mkVarSet (collectLocalBinders binds)
    in
    [(body, 
      mkVarSet (collectLStmtsBinders stmts) 
	`unionVarSet` defined_vars) 
    | L _ (GRHS stmts body) <- grhss]
\end{code}

Replace the leaf commands in a match

\begin{code}
replaceLeavesMatch
        :: Type                                 -- new result type
        -> [Located (body' Id)]                 -- replacement leaf expressions of that type
        -> LMatch Id (Located (body Id))        -- the matches of a case command
        -> ([Located (body' Id)],               -- remaining leaf expressions
            LMatch Id (Located (body' Id)))     -- updated match
replaceLeavesMatch _res_ty leaves (L loc (Match pat mt (GRHSs grhss binds)))
  = let
	(leaves', grhss') = mapAccumL replaceLeavesGRHS leaves grhss
    in
    (leaves', L loc (Match pat mt (GRHSs grhss' binds)))

replaceLeavesGRHS
        :: [Located (body' Id)]                 -- replacement leaf expressions of that type
        -> LGRHS Id (Located (body Id))         -- rhss of a case command
        -> ([Located (body' Id)],               -- remaining leaf expressions
            LGRHS Id (Located (body' Id)))      -- updated GRHS
replaceLeavesGRHS (leaf:leaves) (L loc (GRHS stmts _))
  = (leaves, L loc (GRHS stmts leaf))
replaceLeavesGRHS [] _ = panic "replaceLeavesGRHS []"
\end{code}

Balanced fold of a non-empty list.

\begin{code}
foldb :: (a -> a -> a) -> [a] -> a
foldb _ [] = error "foldb of empty list"
foldb _ [x] = x
foldb f xs = foldb f (fold_pairs xs)
  where
    fold_pairs [] = []
    fold_pairs [x] = [x]
    fold_pairs (x1:x2:xs) = f x1 x2:fold_pairs xs
\end{code}

Note [Dictionary binders in ConPatOut] See also same Note in HsUtils
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following functions to collect value variables from patterns are
copied from HsUtils, with one change: we also collect the dictionary
bindings (pat_binds) from ConPatOut.  We need them for cases like

h :: Arrow a => Int -> a (Int,Int) Int
h x = proc (y,z) -> case compare x y of
                GT -> returnA -< z+x

The type checker turns the case into

                case compare x y of
                  GT { p77 = plusInt } -> returnA -< p77 z x

Here p77 is a local binding for the (+) operation.

See comments in HsUtils for why the other version does not include
these bindings.

\begin{code}
collectPatBinders :: LPat Id -> [Id]
collectPatBinders pat = collectl pat []

collectPatsBinders :: [LPat Id] -> [Id]
collectPatsBinders pats = foldr collectl [] pats

---------------------
collectl :: LPat Id -> [Id] -> [Id]
-- See Note [Dictionary binders in ConPatOut]
collectl (L _ pat) bndrs
  = go pat
  where
    go (VarPat var)               = var : bndrs
    go (WildPat _)                = bndrs
    go (LazyPat pat)              = collectl pat bndrs
    go (BangPat pat)              = collectl pat bndrs
    go (AsPat (L _ a) pat)        = a : collectl pat bndrs
    go (ParPat  pat)              = collectl pat bndrs

    go (ListPat pats _ _)         = foldr collectl bndrs pats
    go (PArrPat pats _)           = foldr collectl bndrs pats
    go (TuplePat pats _ _)        = foldr collectl bndrs pats

    go (ConPatIn _ ps)            = foldr collectl bndrs (hsConPatArgs ps)
    go (ConPatOut {pat_args=ps, pat_binds=ds}) =
                                    collectEvBinders ds
                                    ++ foldr collectl bndrs (hsConPatArgs ps)
    go (LitPat _)                 = bndrs
    go (NPat _ _ _)               = bndrs
    go (NPlusKPat (L _ n) _ _ _)  = n : bndrs

    go (SigPatIn pat _)           = collectl pat bndrs
    go (SigPatOut pat _)          = collectl pat bndrs
    go (CoPat _ pat _)            = collectl (noLoc pat) bndrs
    go (ViewPat _ pat _)          = collectl pat bndrs
    go p@(SplicePat {})           = pprPanic "collectl/go" (ppr p)
    go p@(QuasiQuotePat {})       = pprPanic "collectl/go" (ppr p)

collectEvBinders :: TcEvBinds -> [Id]
collectEvBinders (EvBinds bs)   = foldrBag add_ev_bndr [] bs
collectEvBinders (TcEvBinds {}) = panic "ToDo: collectEvBinders"

add_ev_bndr :: EvBind -> [Id] -> [Id]
add_ev_bndr (EvBind b _) bs | isId b    = b:bs
                            | otherwise = bs
  -- A worry: what about coercion variable binders??

collectLStmtsBinders :: [LStmt Id body] -> [Id]
collectLStmtsBinders = concatMap collectLStmtBinders

collectLStmtBinders :: LStmt Id body -> [Id]
collectLStmtBinders = collectStmtBinders . unLoc

collectStmtBinders :: Stmt Id body -> [Id]
collectStmtBinders (BindStmt pat _ _ _) = collectPatBinders pat
collectStmtBinders (LetStmt binds)      = collectLocalBinders binds
collectStmtBinders (BodyStmt {})        = []
collectStmtBinders (LastStmt {})        = []
collectStmtBinders (ParStmt xs _ _)     = collectLStmtsBinders
                                        $ [ s | ParStmtBlock ss _ _ <- xs, s <- ss]
collectStmtBinders (TransStmt { trS_stmts = stmts }) = collectLStmtsBinders stmts
collectStmtBinders (RecStmt { recS_later_ids = later_ids }) = later_ids

\end{code}
