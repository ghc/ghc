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

#ifdef GHCI
import PrelNames
	-- Template Haskell stuff iff bootstrapped
import DsMeta
#endif

import HsSyn
import TcHsSyn

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import TcType
import Type
import CoreSyn
import CoreUtils
import MkCore

import DynFlags
import CostCentre
import Id
import PrelInfo
import DataCon
import TysWiredIn
import BasicTypes
import PrelNames
import SrcLoc
import Util
import Bag
import Outputable
import FastString
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
dsIPBinds (IPBinds ip_binds dict_binds) body
  = do	{ prs <- dsLHsBinds dict_binds
	; let inner = Let (Rec prs) body
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
  | [L _ (AbsBinds [] [] exports binds)] <- bagToList hsbinds,
    (L loc bind : null_binds) <- bagToList binds,
    isBangHsBind bind
    || isUnboxedTupleBind bind
    || or [isUnLiftedType (idType g) | (_, g, _, _) <- exports]
  = let
      body_w_exports		      = foldr bind_export body exports
      bind_export (tvs, g, l, _) body = ASSERT( null tvs )
				        bindNonRec g (Var l) body
    in
    ASSERT (null null_binds)
	-- Non-recursive, non-overloaded bindings only come in ones
	-- ToDo: in some bizarre case it's conceivable that there
	--       could be dict binds in the 'binds'.  (See the notes
	--	 below.  Then pattern-match would fail.  Urk.)
    putSrcSpanDs loc	$
    case bind of
      FunBind { fun_id = L _ fun, fun_matches = matches, fun_co_fn = co_fn, 
		fun_tick = tick, fun_infix = inf }
        -> do (args, rhs) <- matchWrapper (FunRhs (idName fun ) inf) matches
              MASSERT( null args ) -- Functions aren't lifted
              MASSERT( isIdHsWrapper co_fn )
              rhs' <- mkOptTickBox tick rhs
              return (bindNonRec fun rhs' body_w_exports)

      PatBind {pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty }
	-> 	-- let C x# y# = rhs in body
		-- ==> case rhs of C x# y# -> body
	   putSrcSpanDs loc			$
           do { rhs <- dsGuarded grhss ty
              ; let upat = unLoc pat
                    eqn = EqnInfo { eqn_pats = [upat], 
                                    eqn_rhs = cantFailMatchResult body_w_exports }
              ; var    <- selectMatchVar upat
              ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
              ; return (scrungleMatch var rhs result) }

      _ -> pprPanic "dsLet: unlifted" (pprLHsBinds hsbinds $$ ppr body)


-- Ordinary case for bindings; none should be unlifted
ds_val_bind (_is_rec, binds) body
  = do	{ prs <- dsLHsBinds binds
	; ASSERT( not (any (isUnLiftedType . idType . fst) prs) )
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

isUnboxedTupleBind :: HsBind Id -> Bool
isUnboxedTupleBind (PatBind { pat_rhs_ty = ty }) = isUnboxedTupleType ty
isUnboxedTupleBind _                             = False

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
dsExpr (HsWrap co_fn e)       = dsCoercion co_fn (dsExpr e)

dsExpr (NegApp expr neg_expr) 
  = App <$> dsExpr neg_expr <*> dsLExpr expr

dsExpr (HsLam a_Match)
  = uncurry mkLams <$> matchWrapper LambdaExpr a_Match

dsExpr (HsApp fun arg)
  = mkCoreApp <$> dsLExpr fun <*>  dsLExpr arg
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
    mkCoreApps <$> dsLExpr op <*> mapM dsLExpr [e1, e2]
    
dsExpr (SectionL expr op)	-- Desugar (e !) to ((!) e)
  = mkCoreApp <$> dsLExpr op <*> dsLExpr expr

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
            Lam x_id (mkCoreApps core_op [Var x_id, Var y_id]))

dsExpr (HsSCC cc expr) = do
    mod_name <- getModuleDs
    Note (SCC (mkUserCC cc mod_name)) <$> dsLExpr expr


-- hdaume: core annotation

dsExpr (HsCoreAnn fs expr)
  = Note (CoreNote $ unpackFS fs) <$> dsLExpr expr

dsExpr (HsCase discrim matches@(MatchGroup _ rhs_ty)) 
  | isEmptyMatchGroup matches	-- A Core 'case' is always non-empty
  = 		      		-- So desugar empty HsCase to error call
    mkErrorAppDs pAT_ERROR_ID (funResultTy rhs_ty) "case"

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

dsExpr (HsDo (MDoExpr tbl) stmts body result_ty)
  = dsMDo tbl stmts body result_ty

dsExpr (HsDo PArrComp stmts body result_ty)
  =	-- Special case for array comprehensions
    dsPArrComp (map unLoc stmts) body elt_ty
  where
    [elt_ty] = tcTyConAppArgs result_ty

dsExpr (HsIf guard_expr then_expr else_expr)
  = mkIfThenElse <$> dsLExpr guard_expr <*> dsLExpr then_expr <*> dsLExpr else_expr
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
    emptyP <- dsLookupGlobalId emptyPName
    return (Var emptyP `App` Type ty)
dsExpr (ExplicitPArr ty xs) = do
    singletonP <- dsLookupGlobalId singletonPName
    appP       <- dsLookupGlobalId appPName
    xs'        <- mapM dsLExpr xs
    return . foldr1 (binary appP) $ map (unary singletonP) xs'
  where
    unary  fn x   = mkApps (Var fn) [Type ty, x]
    binary fn x y = mkApps (Var fn) [Type ty, x, y]

dsExpr (ExplicitTuple expr_list boxity) = do
    core_exprs <- mapM dsLExpr expr_list
    return (mkConApp (tupleCon boxity (length expr_list))
                  (map (Type .  exprType) core_exprs ++ core_exprs))

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
              []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (showSDoc (ppr lbl))
        unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty ""

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

\begin{code}
dsExpr expr@(RecordUpd record_expr (HsRecFields { rec_flds = fields })
		       cons_to_upd in_inst_tys out_inst_tys)
  | null fields
  = dsLExpr record_expr
  | otherwise
  =	-- Record stuff doesn't work for existentials
	-- The type checker checks for this, but we need 
	-- worry only about the constructors that are to be updated
    ASSERT2( notNull cons_to_upd && all isVanillaDataCon cons_to_upd, ppr expr )

    do	{ record_expr' <- dsLExpr record_expr
	; let	-- Awkwardly, for families, the match goes 
		-- from instance type to family type
		tycon     = dataConTyCon (head cons_to_upd)
		in_ty     = mkTyConApp tycon in_inst_tys
		in_out_ty = mkFunTy in_ty
				    (mkFamilyTyConApp tycon out_inst_tys)

		mk_val_arg field old_arg_id 
		  = case findField fields field  of
		      (rhs:rest) -> ASSERT(null rest) rhs
		      []	 -> nlHsVar old_arg_id

		mk_alt con
	 	  = ASSERT( isVanillaDataCon con )
		    do 	{ arg_ids <- newSysLocalsDs (dataConInstOrigArgTys con in_inst_tys)
			-- This call to dataConInstOrigArgTys won't work for existentials
			-- but existentials don't have record types anyway
			; let val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
						(dataConFieldLabels con) arg_ids
			      rhs = foldl (\a b -> nlHsApp a b)
				          (nlHsTyApp (dataConWrapId con) out_inst_tys)
				          val_args
			      pat = mkPrefixConPat con (map nlVarPat arg_ids) in_ty

			; return (mkSimpleMatch [pat] rhs) }

	-- It's important to generate the match with matchWrapper,
	-- and the right hand sides with applications of the wrapper Id
	-- so that everything works when we are doing fancy unboxing on the
	-- constructor aguments.
	; alts <- mapM mk_alt cons_to_upd
	; ([discrim_var], matching_code) <- matchWrapper RecUpd (MatchGroup alts in_out_ty)

	; return (bindNonRec discrim_var record_expr' matching_code) }
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
\begin{code}

dsExplicitList :: PostTcType -> [LHsExpr Id] -> DsM CoreExpr
-- See Note [Desugaring explicit lists]
dsExplicitList elt_ty xs = do
    dflags <- getDOptsDs
    xs' <- mapM dsLExpr xs
    if not (dopt Opt_EnableRewriteRules dflags)
        then return $ mkListExpr elt_ty xs'
        else mkBuildExpr elt_ty (mkSplitExplicitList (thisPackage dflags) xs')
  where
    mkSplitExplicitList this_package xs' (c, _) (n, n_ty) = do
        let (dynamic_prefix, static_suffix) = spanTail (rhsIsStatic this_package) xs'
            static_suffix' = mkListExpr elt_ty static_suffix
        
        folded_static_suffix <- mkFoldrExpr elt_ty n_ty (Var c) (Var n) static_suffix'
        let build_body = foldr (App . App (Var c)) folded_static_suffix dynamic_prefix
        return build_body

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

dsDo stmts body _result_ty
  = go (map unLoc stmts)
  where
    go [] = dsLExpr body
    
    go (ExprStmt rhs then_expr _ : stmts)
      = do { rhs2 <- dsLExpr rhs
	   ; then_expr2 <- dsExpr then_expr
	   ; rest <- go stmts
	   ; return (mkApps then_expr2 [rhs2, rest]) }
    
    go (LetStmt binds : stmts)
      = do { rest <- go stmts
	   ; dsLocalBinds binds rest }

    go (BindStmt pat rhs bind_op fail_op : stmts)
      = 
       do  { body     <- go stmts
           ; rhs'     <- dsLExpr rhs
	   ; bind_op' <- dsExpr bind_op
	   ; var   <- selectSimpleMatchVarL pat
	   ; let bind_ty = exprType bind_op' 	-- rhs -> (pat -> res1) -> res2
		 res1_ty = funResultTy (funArgTy (funResultTy bind_ty))
	   ; match <- matchSinglePat (Var var) (StmtCtxt DoExpr) pat
    				     res1_ty (cantFailMatchResult body)
	   ; match_code <- handle_failure pat match fail_op
	   ; return (mkApps bind_op' [rhs', Lam var match_code]) }
    
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
dsMDo	:: PostTcTable
	-> [LStmt Id]
	-> LHsExpr Id
	-> Type			-- Type of the whole expression
	-> DsM CoreExpr

dsMDo tbl stmts body result_ty
  = go (map unLoc stmts)
  where
    (m_ty, b_ty) = tcSplitAppTy result_ty	-- result_ty must be of the form (m b)
    mfix_id   = lookupEvidence tbl mfixName
    return_id = lookupEvidence tbl returnMName
    bind_id   = lookupEvidence tbl bindMName
    then_id   = lookupEvidence tbl thenMName
    fail_id   = lookupEvidence tbl failMName
    ctxt      = MDoExpr tbl

    go [] = dsLExpr body
    
    go (LetStmt binds : stmts)
      = do { rest <- go stmts
	   ; dsLocalBinds binds rest }

    go (ExprStmt rhs _ rhs_ty : stmts)
      = do { rhs2 <- dsLExpr rhs
	   ; rest <- go stmts
	   ; return (mkApps (Var then_id) [Type rhs_ty, Type b_ty, rhs2, rest]) }
    
    go (BindStmt pat rhs _ _ : stmts)
      = do { body  <- go stmts
	   ; var   <- selectSimpleMatchVarL pat
	   ; match <- matchSinglePat (Var var) (StmtCtxt ctxt) pat
    				  result_ty (cantFailMatchResult body)
	   ; fail_msg   <- mkStringExpr (mk_fail_msg pat)
	   ; let fail_expr = mkApps (Var fail_id) [Type b_ty, fail_msg]
	   ; match_code <- extractMatchResult match fail_expr

	   ; rhs'       <- dsLExpr rhs
	   ; return (mkApps (Var bind_id) [Type (hsLPatType pat), Type b_ty, 
					     rhs', Lam var match_code]) }
    
    go (RecStmt rec_stmts later_ids rec_ids rec_rets binds : stmts)
      = ASSERT( length rec_ids > 0 )
        ASSERT( length rec_ids == length rec_rets )
	go (new_bind_stmt : let_stmt : stmts)
      where
        new_bind_stmt = mkBindStmt (mk_tup_pat later_pats) mfix_app
	let_stmt = LetStmt (HsValBinds (ValBindsOut [(Recursive, binds)] []))

	
		-- Remove the later_ids that appear (without fancy coercions) 
		-- in rec_rets, because there's no need to knot-tie them separately
		-- See Note [RecStmt] in HsExpr
	later_ids'   = filter (`notElem` mono_rec_ids) later_ids
	mono_rec_ids = [ id | HsVar id <- rec_rets ]
    
	mfix_app = nlHsApp (nlHsTyApp mfix_id [tup_ty]) mfix_arg
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
	tup_ty  = mkCoreTupTy (map idType (later_ids' ++ rec_ids))
		  -- mkCoreTupTy deals with singleton case

	return_app  = nlHsApp (nlHsTyApp return_id [tup_ty]) 
			      (mk_ret_tup rets)

	mk_wild_pat :: Id -> LPat Id 
   	mk_wild_pat v = noLoc $ WildPat $ idType v

	mk_later_pat :: Id -> LPat Id
	mk_later_pat v | v `elem` later_ids' = mk_wild_pat v
		       | otherwise	     = nlVarPat v

 	mk_tup_pat :: [LPat Id] -> LPat Id
  	mk_tup_pat [p] = p
	mk_tup_pat ps  = noLoc $ mkVanillaTuplePat ps Boxed

	mk_ret_tup :: [LHsExpr Id] -> LHsExpr Id
	mk_ret_tup [r] = r
	mk_ret_tup rs  = noLoc $ ExplicitTuple rs Boxed
\end{code}
