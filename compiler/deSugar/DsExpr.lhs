%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Desugaring exporessions.

\begin{code}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module DsExpr ( dsExpr, dsLExpr, dsLocalBinds, dsValBinds, dsLit ) where

#include "HsVersions.h"

import Match
import MatchLit
import DsBinds
import DsGRHSs
import DsListComp
import DsUtils
import DsArrows
import DsMonad
import Name
import NameEnv

#ifdef GHCI
	-- Template Haskell stuff iff bootstrapped
import DsMeta
#endif

import HsSyn

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import TcType
import Type
import Coercion
import CoreSyn
import CoreUtils
import CoreFVs
import MkCore

import DynFlags
import StaticFlags
import CostCentre
import Id
import Var
import VarSet
import DataCon
import TysWiredIn
import BasicTypes
import PrelNames
import Maybes
import SrcLoc
import Util
import Bag
import Outputable
import FastString

import Control.Monad
\end{code}


%************************************************************************
%*									*
		dsLocalBinds, dsValBinds
%*									*
%************************************************************************

\begin{code}
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr
dsLocalBinds EmptyLocalBinds	body = return body
dsLocalBinds (HsValBinds binds) body = dsValBinds binds body
dsLocalBinds (HsIPBinds binds)  body = dsIPBinds  binds body

-------------------------
dsValBinds :: HsValBinds Id -> CoreExpr -> DsM CoreExpr
dsValBinds (ValBindsOut binds _) body = foldrM ds_val_bind body binds

-------------------------
dsIPBinds :: HsIPBinds Id -> CoreExpr -> DsM CoreExpr
dsIPBinds (IPBinds ip_binds ev_binds) body
  = do	{ ds_ev_binds <- dsTcEvBinds ev_binds
	; let inner = wrapDsEvBinds ds_ev_binds body
		-- The dict bindings may not be in 
		-- dependency order; hence Rec
	; foldrM ds_ip_bind inner ip_binds }
  where
    ds_ip_bind (L _ (IPBind n e)) body
      = do e' <- dsLExpr e
           return (Let (NonRec (ipNameName n) e') body)

-------------------------
ds_val_bind :: (RecFlag, LHsBinds Id) -> CoreExpr -> DsM CoreExpr
-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE and SPECIALISE pragmas...
ds_val_bind (NonRecursive, hsbinds) body
  | [L loc bind] <- bagToList hsbinds,
	-- Non-recursive, non-overloaded bindings only come in ones
	-- ToDo: in some bizarre case it's conceivable that there
	--       could be dict binds in the 'binds'.  (See the notes
	--	 below.  Then pattern-match would fail.  Urk.)
    strictMatchOnly bind
  = putSrcSpanDs loc (dsStrictBind bind body)

-- Ordinary case for bindings; none should be unlifted
ds_val_bind (_is_rec, binds) body
  = do	{ prs <- dsLHsBinds binds
	; ASSERT2( not (any (isUnLiftedType . idType . fst) prs), ppr _is_rec $$ ppr binds )
	  case prs of
            [] -> return body
            _  -> return (Let (Rec prs) body) }
	-- Use a Rec regardless of is_rec. 
	-- Why? Because it allows the binds to be all
	-- mixed up, which is what happens in one rare case
	-- Namely, for an AbsBind with no tyvars and no dicts,
	-- 	   but which does have dictionary bindings.
	-- See notes with TcSimplify.inferLoop [NO TYVARS]
	-- It turned out that wrapping a Rec here was the easiest solution
	--
	-- NB The previous case dealt with unlifted bindings, so we
	--    only have to deal with lifted ones now; so Rec is ok

------------------
dsStrictBind :: HsBind Id -> CoreExpr -> DsM CoreExpr
dsStrictBind (AbsBinds { abs_tvs = [], abs_ev_vars = []
               , abs_exports = exports
               , abs_ev_binds = ev_binds
               , abs_binds = binds }) body
  = do { ds_ev_binds <- dsTcEvBinds ev_binds
       ; let body1 = foldr bind_export body exports
             bind_export (_, g, l, _) b = bindNonRec g (Var l) b
       ; body2 <- foldlBagM (\body bind -> dsStrictBind (unLoc bind) body) 
                            body1 binds 
       ; return (wrapDsEvBinds ds_ev_binds body2) }

dsStrictBind (FunBind { fun_id = L _ fun, fun_matches = matches, fun_co_fn = co_fn 
	              , fun_tick = tick, fun_infix = inf }) body
		-- Can't be a bang pattern (that looks like a PatBind)
		-- so must be simply unboxed
  = do { (args, rhs) <- matchWrapper (FunRhs (idName fun ) inf) matches
       ; MASSERT( null args ) -- Functions aren't lifted
       ; MASSERT( isIdHsWrapper co_fn )
       ; rhs' <- mkOptTickBox tick rhs
       ; return (bindNonRec fun rhs' body) }

dsStrictBind (PatBind {pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty }) body
  = 	-- let C x# y# = rhs in body
	-- ==> case rhs of C x# y# -> body
    do { rhs <- dsGuarded grhss ty
       ; let upat = unLoc pat
             eqn = EqnInfo { eqn_pats = [upat], 
                             eqn_rhs = cantFailMatchResult body }
       ; var    <- selectMatchVar upat
       ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
       ; return (scrungleMatch var rhs result) }

dsStrictBind bind body = pprPanic "dsLet: unlifted" (ppr bind $$ ppr body)

----------------------
strictMatchOnly :: HsBind Id -> Bool
strictMatchOnly (AbsBinds { abs_binds = binds })
  = anyBag (strictMatchOnly . unLoc) binds
strictMatchOnly (PatBind { pat_lhs = lpat, pat_rhs_ty = ty })
  =  isUnboxedTupleType ty 
  || isBangLPat lpat   
  || any (isUnLiftedType . idType) (collectPatBinders lpat)
strictMatchOnly (FunBind { fun_id = L _ id })
  = isUnLiftedType (idType id)
strictMatchOnly _ = False -- I hope!  Checked immediately by caller in fact

scrungleMatch :: Id -> CoreExpr -> CoreExpr -> CoreExpr
-- Returns something like (let var = scrut in body)
-- but if var is an unboxed-tuple type, it inlines it in a fragile way
-- Special case to handle unboxed tuple patterns; they can't appear nested
-- The idea is that 
--	case e of (# p1, p2 #) -> rhs
-- should desugar to
--	case e of (# x1, x2 #) -> ... match p1, p2 ...
-- NOT
--	let x = e in case x of ....
--
-- But there may be a big 
--	let fail = ... in case e of ...
-- wrapping the whole case, which complicates matters slightly
-- It all seems a bit fragile.  Test is dsrun013.

scrungleMatch var scrut body
  | isUnboxedTupleType (idType var) = scrungle body
  | otherwise			    = bindNonRec var scrut body
  where
    scrungle (Case (Var x) bndr ty alts)
		    | x == var = Case scrut bndr ty alts
    scrungle (Let binds body)  = Let binds (scrungle body)
    scrungle other = panic ("scrungleMatch: tuple pattern:\n" ++ showSDoc (ppr other))

\end{code}

%************************************************************************
%*									*
\subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}
%*									*
%************************************************************************

\begin{code}
dsLExpr :: LHsExpr Id -> DsM CoreExpr

dsLExpr (L loc e) = putSrcSpanDs loc $ dsExpr e

dsExpr :: HsExpr Id -> DsM CoreExpr
dsExpr (HsPar e) 	      = dsLExpr e
dsExpr (ExprWithTySigOut e _) = dsLExpr e
dsExpr (HsVar var)     	      = return (Var var)
dsExpr (HsIPVar ip)    	      = return (Var (ipNameName ip))
dsExpr (HsLit lit)     	      = dsLit lit
dsExpr (HsOverLit lit) 	      = dsOverLit lit

dsExpr (HsWrap co_fn e)
  = do { co_fn' <- dsHsWrapper co_fn
       ; e' <- dsExpr e
       ; warn_id <- doptDs Opt_WarnIdentities
       ; when warn_id $ warnAboutIdentities e' co_fn'
       ; return (co_fn' e') }

dsExpr (NegApp expr neg_expr) 
  = App <$> dsExpr neg_expr <*> dsLExpr expr

dsExpr (HsLam a_Match)
  = uncurry mkLams <$> matchWrapper LambdaExpr a_Match

dsExpr (HsApp fun arg)
  = mkCoreAppDs <$> dsLExpr fun <*>  dsLExpr arg
\end{code}

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
	(expr op)
\end{verbatim}
to
\begin{verbatim}
	\x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
	map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
	let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.

\begin{code}
dsExpr (OpApp e1 op _ e2)
  = -- for the type of y, we need the type of op's 2nd argument
    mkCoreAppsDs <$> dsLExpr op <*> mapM dsLExpr [e1, e2]
    
dsExpr (SectionL expr op)	-- Desugar (e !) to ((!) e)
  = mkCoreAppDs <$> dsLExpr op <*> dsLExpr expr

-- dsLExpr (SectionR op expr)	-- \ x -> op x expr
dsExpr (SectionR op expr) = do
    core_op <- dsLExpr op
    -- for the type of x, we need the type of op's 2nd argument
    let (x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
        -- See comment with SectionL
    y_core <- dsLExpr expr
    x_id <- newSysLocalDs x_ty
    y_id <- newSysLocalDs y_ty
    return (bindNonRec y_id y_core $
            Lam x_id (mkCoreAppsDs core_op [Var x_id, Var y_id]))

dsExpr (ExplicitTuple tup_args boxity)
  = do { let go (lam_vars, args) (Missing ty)
                    -- For every missing expression, we need
		    -- another lambda in the desugaring.
               = do { lam_var <- newSysLocalDs ty
                    ; return (lam_var : lam_vars, Var lam_var : args) }
	     go (lam_vars, args) (Present expr)
     	            -- Expressions that are present don't generate
                    -- lambdas, just arguments.
               = do { core_expr <- dsLExpr expr
                    ; return (lam_vars, core_expr : args) }

       ; (lam_vars, args) <- foldM go ([], []) (reverse tup_args)
		-- The reverse is because foldM goes left-to-right

       ; return $ mkCoreLams lam_vars $ 
                  mkConApp (tupleCon boxity (length tup_args))
                           (map (Type . exprType) args ++ args) }

dsExpr (HsSCC cc expr) = do
    mod_name <- getModuleDs
    Note (SCC (mkUserCC cc mod_name)) <$> dsLExpr expr

dsExpr (HsCoreAnn fs expr)
  = Note (CoreNote $ unpackFS fs) <$> dsLExpr expr

dsExpr (HsCase discrim matches@(MatchGroup _ rhs_ty)) 
  | isEmptyMatchGroup matches	-- A Core 'case' is always non-empty
  = 		      		-- So desugar empty HsCase to error call
    mkErrorAppDs pAT_ERROR_ID (funResultTy rhs_ty) (ptext (sLit "case"))

  | otherwise
  = do { core_discrim <- dsLExpr discrim
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt matches
       ; return (scrungleMatch discrim_var core_discrim matching_code) }

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
dsExpr (HsLet binds body) = do
    body' <- dsLExpr body
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo ListComp stmts body result_ty)
  =	-- Special case for list comprehensions
    dsListComp stmts body elt_ty
  where
    [elt_ty] = tcTyConAppArgs result_ty

dsExpr (HsDo DoExpr stmts body result_ty)
  = dsDo stmts body result_ty

dsExpr (HsDo GhciStmt stmts body result_ty)
  = dsDo stmts body result_ty

dsExpr (HsDo MDoExpr stmts body result_ty)
  = dsDo stmts body result_ty

dsExpr (HsDo PArrComp stmts body result_ty)
  =	-- Special case for array comprehensions
    dsPArrComp (map unLoc stmts) body elt_ty
  where
    [elt_ty] = tcTyConAppArgs result_ty

dsExpr (HsIf mb_fun guard_expr then_expr else_expr)
  = do { pred <- dsLExpr guard_expr
       ; b1 <- dsLExpr then_expr
       ; b2 <- dsLExpr else_expr
       ; case mb_fun of
           Just fun -> do { core_fun <- dsExpr fun
                          ; return (mkCoreApps core_fun [pred,b1,b2]) }
           Nothing  -> return $ mkIfThenElse pred b1 b2 }
\end{code}


\noindent
\underline{\bf Various data construction things}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitList elt_ty xs) 
  = dsExplicitList elt_ty xs

-- We desugar [:x1, ..., xn:] as
--   singletonP x1 +:+ ... +:+ singletonP xn
--
dsExpr (ExplicitPArr ty []) = do
    emptyP <- dsLookupDPHId emptyPName
    return (Var emptyP `App` Type ty)
dsExpr (ExplicitPArr ty xs) = do
    singletonP <- dsLookupDPHId singletonPName
    appP       <- dsLookupDPHId appPName
    xs'        <- mapM dsLExpr xs
    return . foldr1 (binary appP) $ map (unary singletonP) xs'
  where
    unary  fn x   = mkApps (Var fn) [Type ty, x]
    binary fn x y = mkApps (Var fn) [Type ty, x, y]

dsExpr (ArithSeq expr (From from))
  = App <$> dsExpr expr <*> dsLExpr from

dsExpr (ArithSeq expr (FromTo from to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, to]

dsExpr (ArithSeq expr (FromThen from thn))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn]

dsExpr (ArithSeq expr (FromThenTo from thn to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn, to]

dsExpr (PArrSeq expr (FromTo from to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, to]

dsExpr (PArrSeq expr (FromThenTo from thn to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn, to]

dsExpr (PArrSeq _ _)
  = panic "DsExpr.dsExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer and typechecker
    -- shouldn't have let it through
\end{code}

\noindent
\underline{\bf Record construction and update}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
	T { op2 = e }
==>
	let err = /\a -> recConErr a 
	T (recConErr t1 "M.lhs/230/op1") 
	  e 
	  (recConErr t1 "M.lhs/230/op3")
\end{verbatim}
@recConErr@ then converts its arugment string into a proper message
before printing it as
\begin{verbatim}
	M.lhs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.

\begin{code}
dsExpr (RecordCon (L _ data_con_id) con_expr rbinds) = do
    con_expr' <- dsExpr con_expr
    let
        (arg_tys, _) = tcSplitFunTys (exprType con_expr')
        -- A newtype in the corner should be opaque; 
        -- hence TcType.tcSplitFunTys

        mk_arg (arg_ty, lbl)    -- Selector id has the field label as its name
          = case findField (rec_flds rbinds) lbl of
              (rhs:rhss) -> ASSERT( null rhss )
                            dsLExpr rhs
              []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (ppr lbl)
        unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty empty

        labels = dataConFieldLabels (idDataCon data_con_id)
        -- The data_con_id is guaranteed to be the wrapper id of the constructor
    
    con_args <- if null labels
                then mapM unlabelled_bottom arg_tys
                else mapM mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels)
    
    return (mkApps con_expr' con_args)
\end{code}

Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
	data T = T1 {op1, op2, op3 :: Int}
	       | T2 {op4, op2 :: Int}
	       | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
	r { op2 = e }
===>
	let op2 = e in
	case r of
	  T1 op1 _ op3 -> T1 op1 op2 op3
	  T2 op4 _     -> T2 op4 op2
	  other	       -> recUpdError "M.lhs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

Note [Update for GADTs]
~~~~~~~~~~~~~~~~~~~~~~~
Consider 
   data T a b where
     T1 { f1 :: a } :: T a Int

Then the wrapper function for T1 has type 
   $WT1 :: a -> T a Int
But if x::T a b, then
   x { f1 = v } :: T a b   (not T a Int!)
So we need to cast (T a Int) to (T a b).  Sigh.

\begin{code}
dsExpr expr@(RecordUpd record_expr (HsRecFields { rec_flds = fields })
		       cons_to_upd in_inst_tys out_inst_tys)
  | null fields
  = dsLExpr record_expr
  | otherwise
  = ASSERT2( notNull cons_to_upd, ppr expr )

    do	{ record_expr' <- dsLExpr record_expr
	; field_binds' <- mapM ds_field fields
	; let upd_fld_env :: NameEnv Id	-- Maps field name to the LocalId of the field binding
	      upd_fld_env = mkNameEnv [(f,l) | (f,l,_) <- field_binds']

	-- It's important to generate the match with matchWrapper,
	-- and the right hand sides with applications of the wrapper Id
	-- so that everything works when we are doing fancy unboxing on the
	-- constructor aguments.
	; alts <- mapM (mk_alt upd_fld_env) cons_to_upd
	; ([discrim_var], matching_code) 
		<- matchWrapper RecUpd (MatchGroup alts in_out_ty)

	; return (add_field_binds field_binds' $
		  bindNonRec discrim_var record_expr' matching_code) }
  where
    ds_field :: HsRecField Id (LHsExpr Id) -> DsM (Name, Id, CoreExpr)
      -- Clone the Id in the HsRecField, because its Name is that
      -- of the record selector, and we must not make that a lcoal binder
      -- else we shadow other uses of the record selector
      -- Hence 'lcl_id'.  Cf Trac #2735
    ds_field rec_field = do { rhs <- dsLExpr (hsRecFieldArg rec_field)
    	     	       	    ; let fld_id = unLoc (hsRecFieldId rec_field)
    	     	       	    ; lcl_id <- newSysLocalDs (idType fld_id)
			    ; return (idName fld_id, lcl_id, rhs) }

    add_field_binds [] expr = expr
    add_field_binds ((_,b,r):bs) expr = bindNonRec b r (add_field_binds bs expr)

	-- Awkwardly, for families, the match goes 
	-- from instance type to family type
    tycon     = dataConTyCon (head cons_to_upd)
    in_ty     = mkTyConApp tycon in_inst_tys
    in_out_ty = mkFunTy in_ty (mkFamilyTyConApp tycon out_inst_tys)

    mk_alt upd_fld_env con
      = do { let (univ_tvs, ex_tvs, eq_spec, 
		  eq_theta, dict_theta, arg_tys, _) = dataConFullSig con
		 subst = mkTopTvSubst (univ_tvs `zip` in_inst_tys)

		-- I'm not bothering to clone the ex_tvs
	   ; eqs_vars   <- mapM newPredVarDs (substTheta subst (eqSpecPreds eq_spec))
	   ; theta_vars <- mapM newPredVarDs (substTheta subst (eq_theta ++ dict_theta))
	   ; arg_ids    <- newSysLocalsDs (substTys subst arg_tys)
	   ; let val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
    					 (dataConFieldLabels con) arg_ids
                 mk_val_arg field_name pat_arg_id 
                     = nlHsVar (lookupNameEnv upd_fld_env field_name `orElse` pat_arg_id)
		 inst_con = noLoc $ HsWrap wrap (HsVar (dataConWrapId con))
			-- Reconstruct with the WrapId so that unpacking happens
		 wrap = mkWpEvVarApps theta_vars          `WpCompose` 
			mkWpTyApps    (mkTyVarTys ex_tvs) `WpCompose`
			mkWpTyApps [ty | (tv, ty) <- univ_tvs `zip` out_inst_tys
				       , isNothing (lookupTyVar wrap_subst tv) ]
    	         rhs = foldl (\a b -> nlHsApp a b) inst_con val_args

			-- Tediously wrap the application in a cast
			-- Note [Update for GADTs]
		 wrapped_rhs | null eq_spec = rhs
			     | otherwise    = mkLHsWrap (WpCast wrap_co) rhs
		 wrap_co = mkTyConApp tycon [ lookup tv ty 
					    | (tv,ty) <- univ_tvs `zip` out_inst_tys]
		 lookup univ_tv ty = case lookupTyVar wrap_subst univ_tv of
					Just ty' -> ty'
					Nothing  -> ty
		 wrap_subst = mkTopTvSubst [ (tv,mkSymCoercion (mkTyVarTy co_var))
					   | ((tv,_),co_var) <- eq_spec `zip` eqs_vars ]
		 
    	         pat = noLoc $ ConPatOut { pat_con = noLoc con, pat_tvs = ex_tvs
					 , pat_dicts = eqs_vars ++ theta_vars
					 , pat_binds = emptyTcEvBinds
					 , pat_args = PrefixCon $ map nlVarPat arg_ids
					 , pat_ty = in_ty }
	   ; return (mkSimpleMatch [pat] wrapped_rhs) }

\end{code}

Here is where we desugar the Template Haskell brackets and escapes

\begin{code}
-- Template Haskell stuff

#ifdef GHCI	/* Only if bootstrapping */
dsExpr (HsBracketOut x ps) = dsBracket x ps
dsExpr (HsSpliceE s)       = pprPanic "dsExpr:splice" (ppr s)
#endif

-- Arrow notation extension
dsExpr (HsProc pat cmd) = dsProcExpr pat cmd
\end{code}

Hpc Support 

\begin{code}
dsExpr (HsTick ix vars e) = do
  e' <- dsLExpr e
  mkTickBox ix vars e'

-- There is a problem here. The then and else branches
-- have no free variables, so they are open to lifting.
-- We need someway of stopping this.
-- This will make no difference to binary coverage
-- (did you go here: YES or NO), but will effect accurate
-- tick counting.

dsExpr (HsBinTick ixT ixF e) = do
  e2 <- dsLExpr e
  do { ASSERT(exprType e2 `coreEqType` boolTy)
       mkBinaryTickBox ixT ixF e2
     }
\end{code}

\begin{code}

-- HsSyn constructs that just shouldn't be here:
dsExpr (ExprWithTySig _ _)  = panic "dsExpr:ExprWithTySig"


findField :: [HsRecField Id arg] -> Name -> [arg]
findField rbinds lbl 
  = [rhs | HsRecField { hsRecFieldId = id, hsRecFieldArg = rhs } <- rbinds 
	 , lbl == idName (unLoc id) ]
\end{code}

%--------------------------------------------------------------------

Note [Desugaring explicit lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Explicit lists are desugared in a cleverer way to prevent some
fruitless allocations.  Essentially, whenever we see a list literal
[x_1, ..., x_n] we:

1. Find the tail of the list that can be allocated statically (say
   [x_k, ..., x_n]) by later stages and ensure we desugar that
   normally: this makes sure that we don't cause a code size increase
   by having the cons in that expression fused (see later) and hence
   being unable to statically allocate any more

2. For the prefix of the list which cannot be allocated statically,
   say [x_1, ..., x_(k-1)], we turn it into an expression involving
   build so that if we find any foldrs over it it will fuse away
   entirely!
   
   So in this example we will desugar to:
   build (\c n -> x_1 `c` x_2 `c` .... `c` foldr c n [x_k, ..., x_n]
   
   If fusion fails to occur then build will get inlined and (since we
   defined a RULE for foldr (:) []) we will get back exactly the
   normal desugaring for an explicit list.

This optimisation can be worth a lot: up to 25% of the total
allocation in some nofib programs. Specifically

        Program           Size    Allocs   Runtime  CompTime
        rewrite          +0.0%    -26.3%      0.02     -1.8%
           ansi          -0.3%    -13.8%      0.00     +0.0%
           lift          +0.0%     -8.7%      0.00     -2.3%

Of course, if rules aren't turned on then there is pretty much no
point doing this fancy stuff, and it may even be harmful.

=======>  Note by SLPJ Dec 08.

I'm unconvinced that we should *ever* generate a build for an explicit
list.  See the comments in GHC.Base about the foldr/cons rule, which 
points out that (foldr k z [a,b,c]) may generate *much* less code than
(a `k` b `k` c `k` z).

Furthermore generating builds messes up the LHS of RULES. 
Example: the foldr/single rule in GHC.Base
   foldr k z [x] = ...
We do not want to generate a build invocation on the LHS of this RULE!

We fix this by disabling rules in rule LHSs, and testing that
flag here; see Note [Desugaring RULE left hand sides] in Desugar

To test this I've added a (static) flag -fsimple-list-literals, which
makes all list literals be generated via the simple route.  


\begin{code}
dsExplicitList :: PostTcType -> [LHsExpr Id] -> DsM CoreExpr
-- See Note [Desugaring explicit lists]
dsExplicitList elt_ty xs
  = do { dflags <- getDOptsDs
       ; xs' <- mapM dsLExpr xs
       ; let (dynamic_prefix, static_suffix) = spanTail is_static xs'
       ; if opt_SimpleListLiterals 	       		-- -fsimple-list-literals
         || not (dopt Opt_EnableRewriteRules dflags)	-- Rewrite rules off
	    	-- Don't generate a build if there are no rules to eliminate it!
		-- See Note [Desugaring RULE left hand sides] in Desugar
         || null dynamic_prefix   -- Avoid build (\c n. foldr c n xs)!
         then return $ mkListExpr elt_ty xs'
         else mkBuildExpr elt_ty (mkSplitExplicitList dynamic_prefix static_suffix) }
  where
    is_static :: CoreExpr -> Bool
    is_static e = all is_static_var (varSetElems (exprFreeVars e))

    is_static_var :: Var -> Bool
    is_static_var v 
      | isId v = isExternalName (idName v)  -- Top-level things are given external names
      | otherwise = False                   -- Type variables

    mkSplitExplicitList prefix suffix (c, _) (n, n_ty)
      = do { let suffix' = mkListExpr elt_ty suffix
           ; folded_suffix <- mkFoldrExpr elt_ty n_ty (Var c) (Var n) suffix'
           ; return (foldr (App . App (Var c)) folded_suffix prefix) }

spanTail :: (a -> Bool) -> [a] -> ([a], [a])
spanTail f xs = (reverse rejected, reverse satisfying)
    where (satisfying, rejected) = span f $ reverse xs
\end{code}

Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in DsListComp).  Basically does the translation given in the
Haskell 98 report:

\begin{code}
dsDo	:: [LStmt Id]
	-> LHsExpr Id
	-> Type			-- Type of the whole expression
	-> DsM CoreExpr

dsDo stmts body result_ty
  = goL stmts
  where
    -- result_ty must be of the form (m b)
    (m_ty, _b_ty) = tcSplitAppTy result_ty

    goL [] = dsLExpr body
    goL ((L loc stmt):lstmts) = putSrcSpanDs loc (go loc stmt lstmts)
  
    go _ (ExprStmt rhs then_expr _) stmts
      = do { rhs2 <- dsLExpr rhs
           ; case tcSplitAppTy_maybe (exprType rhs2) of
                Just (container_ty, returning_ty) -> warnDiscardedDoBindings rhs container_ty returning_ty
                _                                 -> return ()
           ; then_expr2 <- dsExpr then_expr
	   ; rest <- goL stmts
	   ; return (mkApps then_expr2 [rhs2, rest]) }
    
    go _ (LetStmt binds) stmts
      = do { rest <- goL stmts
	   ; dsLocalBinds binds rest }

    go _ (BindStmt pat rhs bind_op fail_op) stmts
      = do  { body     <- goL stmts
            ; rhs'     <- dsLExpr rhs
	    ; bind_op' <- dsExpr bind_op
	    ; var   <- selectSimpleMatchVarL pat
	    ; let bind_ty = exprType bind_op' 	-- rhs -> (pat -> res1) -> res2
	    	  res1_ty = funResultTy (funArgTy (funResultTy bind_ty))
	    ; match <- matchSinglePat (Var var) (StmtCtxt DoExpr) pat
    	    			      res1_ty (cantFailMatchResult body)
	    ; match_code <- handle_failure pat match fail_op
	    ; return (mkApps bind_op' [rhs', Lam var match_code]) }
    
    go loc (RecStmt { recS_stmts = rec_stmts, recS_later_ids = later_ids
                    , recS_rec_ids = rec_ids, recS_ret_fn = return_op
                    , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op
                    , recS_rec_rets = rec_rets }) stmts
      = ASSERT( length rec_ids > 0 )
        goL (new_bind_stmt : stmts)
      where
        -- returnE <- dsExpr return_id
        -- mfixE <- dsExpr mfix_id
        new_bind_stmt = L loc $ BindStmt (mkLHsPatTup later_pats) mfix_app
                                         bind_op 
                                         noSyntaxExpr  -- Tuple cannot fail

        tup_ids      = rec_ids ++ filterOut (`elem` rec_ids) later_ids
        rec_tup_pats = map nlVarPat tup_ids
        later_pats   = rec_tup_pats
        rets         = map noLoc rec_rets

        mfix_app   = nlHsApp (noLoc mfix_op) mfix_arg
        mfix_arg   = noLoc $ HsLam (MatchGroup [mkSimpleMatch [mfix_pat] body]
                                             (mkFunTy tup_ty body_ty))
        mfix_pat   = noLoc $ LazyPat $ mkLHsPatTup rec_tup_pats
        body       = noLoc $ HsDo DoExpr rec_stmts return_app body_ty
        return_app = nlHsApp (noLoc return_op) (mkLHsTupleExpr rets)
	body_ty    = mkAppTy m_ty tup_ty
        tup_ty     = mkBoxedTupleTy (map idType tup_ids) -- Deals with singleton case

handle_failure :: LPat Id -> MatchResult -> SyntaxExpr Id -> DsM CoreExpr
    -- In a do expression, pattern-match failure just calls
    -- the monadic 'fail' rather than throwing an exception
handle_failure pat match fail_op
  | matchCanFail match
  = do { fail_op' <- dsExpr fail_op
       ; fail_msg <- mkStringExpr (mk_fail_msg pat)
       ; extractMatchResult match (App fail_op' fail_msg) }
  | otherwise
  = extractMatchResult match (error "It can't fail")

mk_fail_msg :: Located e -> String
mk_fail_msg pat = "Pattern match failure in do expression at " ++ 
		  showSDoc (ppr (getLoc pat))
\end{code}

Translation for RecStmt's: 
-----------------------------
We turn (RecStmt [v1,..vn] stmts) into:
  
  (v1,..,vn) <- mfix (\~(v1,..vn). do stmts
				      return (v1,..vn))

\begin{code}
{-
dsMDo   :: HsStmtContext Name
        -> [(Name,Id)]
	-> [LStmt Id]
	-> LHsExpr Id
	-> Type			-- Type of the whole expression
	-> DsM CoreExpr

dsMDo ctxt tbl stmts body result_ty
  = goL stmts
  where
    goL [] = dsLExpr body
    goL ((L loc stmt):lstmts) = putSrcSpanDs loc (go loc stmt lstmts)
  
    (m_ty, b_ty) = tcSplitAppTy result_ty	-- result_ty must be of the form (m b)
    return_id = lookupEvidence tbl returnMName
    bind_id   = lookupEvidence tbl bindMName
    then_id   = lookupEvidence tbl thenMName
    fail_id   = lookupEvidence tbl failMName

    go _ (LetStmt binds) stmts
      = do { rest <- goL stmts
	   ; dsLocalBinds binds rest }

    go _ (ExprStmt rhs then_expr rhs_ty) stmts
      = do { rhs2 <- dsLExpr rhs
	   ; warnDiscardedDoBindings rhs m_ty rhs_ty
           ; then_expr2 <- dsExpr then_expr
           ; rest <- goL stmts
           ; return (mkApps then_expr2 [rhs2, rest]) }
    
    go _ (BindStmt pat rhs bind_op _) stmts
      = do { body     <- goL stmts
           ; rhs'     <- dsLExpr rhs
           ; bind_op' <- dsExpr bind_op
           ; var   <- selectSimpleMatchVarL pat
	   ; match <- matchSinglePat (Var var) (StmtCtxt ctxt) pat
                                     result_ty (cantFailMatchResult body)
           ; match_code <- handle_failure pat match fail_op
           ; return (mkApps bind_op [rhs', Lam var match_code]) }
    
    go loc (RecStmt { recS_stmts = rec_stmts, recS_later_ids = later_ids
                    , recS_rec_ids = rec_ids, recS_rec_rets = rec_rets
                    , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op }) stmts
      = ASSERT( length rec_ids > 0 )
        ASSERT( length rec_ids == length rec_rets )
        ASSERT( isEmptyTcEvBinds _ev_binds )
        pprTrace "dsMDo" (ppr later_ids) $
	 goL (new_bind_stmt : stmts)
      where
        new_bind_stmt = L loc $ BindStmt (mk_tup_pat later_pats) mfix_app
                                         bind_op noSyntaxExpr
	
		-- Remove the later_ids that appear (without fancy coercions) 
		-- in rec_rets, because there's no need to knot-tie them separately
		-- See Note [RecStmt] in HsExpr
	later_ids'   = filter (`notElem` mono_rec_ids) later_ids
	mono_rec_ids = [ id | HsVar id <- rec_rets ]
    
        mfix_app = nlHsApp (noLoc mfix_op) mfix_arg
	mfix_arg = noLoc $ HsLam (MatchGroup [mkSimpleMatch [mfix_pat] body]
					     (mkFunTy tup_ty body_ty))

	-- The rec_tup_pat must bind the rec_ids only; remember that the 
	-- 	trimmed_laters may share the same Names
	-- Meanwhile, the later_pats must bind the later_vars
	rec_tup_pats = map mk_wild_pat later_ids' ++ map nlVarPat rec_ids
	later_pats   = map nlVarPat    later_ids' ++ map mk_later_pat rec_ids
	rets         = map nlHsVar     later_ids' ++ map noLoc rec_rets

	mfix_pat = noLoc $ LazyPat $ mk_tup_pat rec_tup_pats
	body     = noLoc $ HsDo ctxt rec_stmts return_app body_ty
	body_ty = mkAppTy m_ty tup_ty
	tup_ty  = mkBoxedTupleTy (map idType (later_ids' ++ rec_ids))  -- Deals with singleton case

        return_app  = nlHsApp (noLoc return_op) (mkLHsTupleExpr rets)

	mk_wild_pat :: Id -> LPat Id 
   	mk_wild_pat v = noLoc $ WildPat $ idType v

	mk_later_pat :: Id -> LPat Id
	mk_later_pat v | v `elem` later_ids' = mk_wild_pat v
		       | otherwise	     = nlVarPat v

 	mk_tup_pat :: [LPat Id] -> LPat Id
  	mk_tup_pat [p] = p
	mk_tup_pat ps  = noLoc $ mkVanillaTuplePat ps Boxed
-}
\end{code}


%************************************************************************
%*									*
                 Warning about identities
%*									*
%************************************************************************

Warn about functions that convert between one type and another
when the to- and from- types are the same.  Then it's probably
(albeit not definitely) the identity
\begin{code}
warnAboutIdentities :: CoreExpr -> (CoreExpr -> CoreExpr) -> DsM ()
warnAboutIdentities (Var v) co_fn
  | idName v `elem` conversionNames
  , let fun_ty = exprType (co_fn (Var v))
  , Just (arg_ty, res_ty) <- splitFunTy_maybe fun_ty
  , arg_ty `tcEqType` res_ty  -- So we are converting  ty -> ty
  = warnDs (vcat [ ptext (sLit "Call of") <+> ppr v <+> dcolon <+> ppr fun_ty
                 , nest 2 $ ptext (sLit "can probably be omitted")
                 , parens (ptext (sLit "Use -fno-warn-identities to suppress this messsage)"))
           ])
warnAboutIdentities _ _ = return ()

conversionNames :: [Name]
conversionNames
  = [ toIntegerName, toRationalName
    , fromIntegralName, realToFracName ]
 -- We can't easily add fromIntegerName, fromRationalName,
 -- becuase they are generated by literals
\end{code}

%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
-- Warn about certain types of values discarded in monadic bindings (#3263)
warnDiscardedDoBindings :: LHsExpr Id -> Type -> Type -> DsM ()
warnDiscardedDoBindings rhs container_ty returning_ty = do {
          -- Warn about discarding non-() things in 'monadic' binding
        ; warn_unused <- doptDs Opt_WarnUnusedDoBind
        ; if warn_unused && not (returning_ty `tcEqType` unitTy)
           then warnDs (unusedMonadBind rhs returning_ty)
           else do {
          -- Warn about discarding m a things in 'monadic' binding of the same type,
          -- but only if we didn't already warn due to Opt_WarnUnusedDoBind
        ; warn_wrong <- doptDs Opt_WarnWrongDoBind
        ; case tcSplitAppTy_maybe returning_ty of
                  Just (returning_container_ty, _) -> when (warn_wrong && container_ty `tcEqType` returning_container_ty) $
                                                            warnDs (wrongMonadBind rhs returning_ty)
                  _ -> return () } }

unusedMonadBind :: LHsExpr Id -> Type -> SDoc
unusedMonadBind rhs returning_ty
  = ptext (sLit "A do-notation statement discarded a result of type") <+> ppr returning_ty <> dot $$
    ptext (sLit "Suppress this warning by saying \"_ <- ") <> ppr rhs <> ptext (sLit "\",") $$
    ptext (sLit "or by using the flag -fno-warn-unused-do-bind")

wrongMonadBind :: LHsExpr Id -> Type -> SDoc
wrongMonadBind rhs returning_ty
  = ptext (sLit "A do-notation statement discarded a result of type") <+> ppr returning_ty <> dot $$
    ptext (sLit "Suppress this warning by saying \"_ <- ") <> ppr rhs <> ptext (sLit "\",") $$
    ptext (sLit "or by using the flag -fno-warn-wrong-do-bind")
\end{code}
