%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Desugaring list comprehensions, monad comprehensions and array comprehensions

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module DsListComp ( dsListComp, dsPArrComp, dsMonadComp ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr ( dsExpr, dsLExpr, dsLocalBinds )

import HsSyn
import TcHsSyn
import CoreSyn
import MkCore

import DsMonad		-- the monadery used in the desugarer
import DsUtils

import DynFlags
import CoreUtils
import Id
import Type
import TysWiredIn
import Match
import PrelNames
import SrcLoc
import Outputable
import FastString
import TcType
\end{code}

List comprehensions may be desugared in one of two ways: ``ordinary''
(as you would expect if you read SLPJ's book) and ``with foldr/build
turned on'' (if you read Gill {\em et al.}'s paper on the subject).

There will be at least one ``qualifier'' in the input.

\begin{code}
dsListComp :: [LStmt Id] 
	   -> Type		-- Type of entire list 
	   -> DsM CoreExpr
dsListComp lquals res_ty = do 
    dflags <- getDOptsDs
    let quals = map unLoc lquals
        [elt_ty] = tcTyConAppArgs res_ty
    
    if not (dopt Opt_EnableRewriteRules dflags) || dopt Opt_IgnoreInterfacePragmas dflags
       -- Either rules are switched off, or we are ignoring what there are;
       -- Either way foldr/build won't happen, so use the more efficient
       -- Wadler-style desugaring
       || isParallelComp quals
       -- Foldr-style desugaring can't handle parallel list comprehensions
        then deListComp quals (mkNilExpr elt_ty)
        else mkBuildExpr elt_ty (\(c, _) (n, _) -> dfListComp c n quals) 
             -- Foldr/build should be enabled, so desugar 
             -- into foldrs and builds

  where 
    -- We must test for ParStmt anywhere, not just at the head, because an extension
    -- to list comprehensions would be to add brackets to specify the associativity
    -- of qualifier lists. This is really easy to do by adding extra ParStmts into the
    -- mix of possibly a single element in length, so we do this to leave the possibility open
    isParallelComp = any isParallelStmt
  
    isParallelStmt (ParStmt _ _ _ _) = True
    isParallelStmt _                 = False
    
    
-- This function lets you desugar a inner list comprehension and a list of the binders
-- of that comprehension that we need in the outer comprehension into such an expression
-- and the type of the elements that it outputs (tuples of binders)
dsInnerListComp :: ([LStmt Id], [Id]) -> DsM (CoreExpr, Type)
dsInnerListComp (stmts, bndrs) = do
  = do { expr <- dsListComp (stmts ++ [noLoc $ mkLastStmt (mkBigLHsVarTup bndrs)]) 
                           bndrs_tuple_type
       ; return (expr, bndrs_tuple_type) }
  where
    bndrs_tuple_type = mkBigCoreVarTupTy bndrs
        
-- This function factors out commonality between the desugaring strategies for TransformStmt.
-- Given such a statement it gives you back an expression representing how to compute the transformed
-- list and the tuple that you need to bind from that list in order to proceed with your desugaring
dsTransformStmt :: Stmt Id -> DsM (CoreExpr, LPat Id)
dsTransformStmt (TransformStmt stmts binders usingExpr maybeByExpr _ _)
 = do { (expr, binders_tuple_type) <- dsInnerListComp (stmts, binders)
      ; usingExpr' <- dsLExpr usingExpr
    
      ; using_args <-
          case maybeByExpr of
            Nothing -> return [expr]
            Just byExpr -> do
                byExpr' <- dsLExpr byExpr
                
                us <- newUniqueSupply
                [tuple_binder] <- newSysLocalsDs [binders_tuple_type]
                let byExprWrapper = mkTupleCase us binders byExpr' tuple_binder (Var tuple_binder)
                
                return [Lam tuple_binder byExprWrapper, expr]

      ; let inner_list_expr = mkApps usingExpr' ((Type binders_tuple_type) : using_args)
            pat = mkBigLHsVarPatTup binders
      ; return (inner_list_expr, pat) }
    
-- This function factors out commonality between the desugaring strategies for GroupStmt.
-- Given such a statement it gives you back an expression representing how to compute the transformed
-- list and the tuple that you need to bind from that list in order to proceed with your desugaring
dsGroupStmt :: Stmt Id -> DsM (CoreExpr, LPat Id)
dsGroupStmt (GroupStmt stmts binderMap by using _ _ _) = do
    let (fromBinders, toBinders) = unzip binderMap
        
        fromBindersTypes = map idType fromBinders
        toBindersTypes = map idType toBinders
        
        toBindersTupleType = mkBigCoreTupTy toBindersTypes
    
    -- Desugar an inner comprehension which outputs a list of tuples of the "from" binders
    (expr, from_tup_ty) <- dsInnerListComp (stmts, fromBinders)
    
    -- Work out what arguments should be supplied to that expression: i.e. is an extraction
    -- function required? If so, create that desugared function and add to arguments
    usingExpr' <- dsLExpr (either id noLoc using)
    usingArgs <- case by of
                   Nothing   -> return [expr]
 		   Just by_e -> do { by_e' <- dsLExpr by_e
                                   ; us <- newUniqueSupply
                                   ; [from_tup_id] <- newSysLocalsDs [from_tup_ty]
                                   ; let by_wrap = mkTupleCase us fromBinders by_e' 
                                                   from_tup_id (Var from_tup_id)
                                   ; return [Lam from_tup_id by_wrap, expr] }
    
    -- Create an unzip function for the appropriate arity and element types and find "map"
    (unzip_fn, unzip_rhs) <- mkUnzipBind fromBindersTypes
    map_id <- dsLookupGlobalId mapName

    -- Generate the expressions to build the grouped list
    let -- First we apply the grouping function to the inner list
        inner_list_expr = mkApps usingExpr' ((Type from_tup_ty) : usingArgs)
        -- Then we map our "unzip" across it to turn the lists of tuples into tuples of lists
        -- We make sure we instantiate the type variable "a" to be a list of "from" tuples and
        -- the "b" to be a tuple of "to" lists!
        unzipped_inner_list_expr = mkApps (Var map_id) 
            [Type (mkListTy from_tup_ty), Type toBindersTupleType, Var unzip_fn, inner_list_expr]
        -- Then finally we bind the unzip function around that expression
        bound_unzipped_inner_list_expr = Let (Rec [(unzip_fn, unzip_rhs)]) unzipped_inner_list_expr
    
    -- Build a pattern that ensures the consumer binds into the NEW binders, which hold lists rather than single values
    let pat = mkBigLHsVarPatTup toBinders
    return (bound_unzipped_inner_list_expr, pat)
    
\end{code}

%************************************************************************
%*									*
\subsection[DsListComp-ordinary]{Ordinary desugaring of list comprehensions}
%*									*
%************************************************************************

Just as in Phil's chapter~7 in SLPJ, using the rules for
optimally-compiled list comprehensions.  This is what Kevin followed
as well, and I quite happily do the same.  The TQ translation scheme
transforms a list of qualifiers (either boolean expressions or
generators) into a single expression which implements the list
comprehension.  Because we are generating 2nd-order polymorphic
lambda-calculus, calls to NIL and CONS must be applied to a type
argument, as well as their usual value arguments.
\begin{verbatim}
TE << [ e | qs ] >>  =  TQ << [ e | qs ] ++ Nil (typeOf e) >>

(Rule C)
TQ << [ e | ] ++ L >> = Cons (typeOf e) TE <<e>> TE <<L>>

(Rule B)
TQ << [ e | b , qs ] ++ L >> =
    if TE << b >> then TQ << [ e | qs ] ++ L >> else TE << L >>

(Rule A')
TQ << [ e | p <- L1, qs ]  ++  L2 >> =
  letrec
    h = \ u1 ->
    	  case u1 of
	    []        ->  TE << L2 >>
	    (u2 : u3) ->
		  (( \ TE << p >> -> ( TQ << [e | qs]  ++  (h u3) >> )) u2)
		    [] (h u3)
  in
    h ( TE << L1 >> )

"h", "u1", "u2", and "u3" are new variables.
\end{verbatim}

@deListComp@ is the TQ translation scheme.  Roughly speaking, @dsExpr@
is the TE translation scheme.  Note that we carry around the @L@ list
already desugared.  @dsListComp@ does the top TE rule mentioned above.

To the above, we add an additional rule to deal with parallel list
comprehensions.  The translation goes roughly as follows:
     [ e | p1 <- e11, let v1 = e12, p2 <- e13
         | q1 <- e21, let v2 = e22, q2 <- e23]
     =>
     [ e | ((x1, .., xn), (y1, ..., ym)) <-
               zip [(x1,..,xn) | p1 <- e11, let v1 = e12, p2 <- e13]
                   [(y1,..,ym) | q1 <- e21, let v2 = e22, q2 <- e23]]
where (x1, .., xn) are the variables bound in p1, v1, p2
      (y1, .., ym) are the variables bound in q1, v2, q2

In the translation below, the ParStmt branch translates each parallel branch
into a sub-comprehension, and desugars each independently.  The resulting lists
are fed to a zip function, we create a binding for all the variables bound in all
the comprehensions, and then we hand things off the the desugarer for bindings.
The zip function is generated here a) because it's small, and b) because then we
don't have to deal with arbitrary limits on the number of zip functions in the
prelude, nor which library the zip function came from.
The introduced tuples are Boxed, but only because I couldn't get it to work
with the Unboxed variety.

\begin{code}

deListComp :: [Stmt Id] -> CoreExpr -> DsM CoreExpr

deListComp [] _ = panic "deListComp"

deListComp (LastStmt body _ : quals) list 
  =     -- Figure 7.4, SLPJ, p 135, rule C above
    ASSERT( null quals )
    do { core_body <- dsLExpr body
       ; return (mkConsExpr (exprType core_body) core_body list) }

	-- Non-last: must be a guard
deListComp (ExprStmt guard _ _ _ : quals) list = do  -- rule B above
    core_guard <- dsLExpr guard
    core_rest <- deListComp quals list
    return (mkIfThenElse core_guard core_rest list)

-- [e | let B, qs] = let B in [e | qs]
deListComp (LetStmt binds : quals) list = do
    core_rest <- deListComp quals list
    dsLocalBinds binds core_rest

deListComp (stmt@(TransformStmt {}) : quals) list = do
    (inner_list_expr, pat) <- dsTransformStmt stmt
    deBindComp pat inner_list_expr quals list

deListComp (stmt@(GroupStmt {}) : quals) list = do
    (inner_list_expr, pat) <- dsGroupStmt stmt
    deBindComp pat inner_list_expr quals list

deListComp (BindStmt pat list1 _ _ : quals) core_list2 = do -- rule A' above
    core_list1 <- dsLExpr list1
    deBindComp pat core_list1 quals core_list2

deListComp (ParStmt stmtss_w_bndrs _ _ _ : quals) list
  = do
    exps_and_qual_tys <- mapM dsInnerListComp stmtss_w_bndrs
    let (exps, qual_tys) = unzip exps_and_qual_tys
    
    (zip_fn, zip_rhs) <- mkZipBind qual_tys

	-- Deal with [e | pat <- zip l1 .. ln] in example above
    deBindComp pat (Let (Rec [(zip_fn, zip_rhs)]) (mkApps (Var zip_fn) exps)) 
		   quals list

  where 
	bndrs_s = map snd stmtss_w_bndrs

	-- pat is the pattern ((x1,..,xn), (y1,..,ym)) in the example above
	pat  = mkBigLHsPatTup pats
	pats = map mkBigLHsVarPatTup bndrs_s
\end{code}


\begin{code}
deBindComp :: OutPat Id
           -> CoreExpr
           -> [Stmt Id]
           -> CoreExpr
           -> DsM (Expr Id)
deBindComp pat core_list1 quals core_list2 = do
    let
        u3_ty@u1_ty = exprType core_list1	-- two names, same thing

        -- u1_ty is a [alpha] type, and u2_ty = alpha
        u2_ty = hsLPatType pat

        res_ty = exprType core_list2
        h_ty   = u1_ty `mkFunTy` res_ty
        
    [h, u1, u2, u3] <- newSysLocalsDs [h_ty, u1_ty, u2_ty, u3_ty]

    -- the "fail" value ...
    let
        core_fail   = App (Var h) (Var u3)
        letrec_body = App (Var h) core_list1
        
    rest_expr <- deListComp quals core_fail
    core_match <- matchSimply (Var u2) (StmtCtxt ListComp) pat rest_expr core_fail	
    
    let
        rhs = Lam u1 $
	      Case (Var u1) u1 res_ty
		   [(DataAlt nilDataCon,  [], 	    core_list2),
		    (DataAlt consDataCon, [u2, u3], core_match)]
			-- Increasing order of tag
            
    return (Let (Rec [(h, rhs)]) letrec_body)
\end{code}

%************************************************************************
%*									*
\subsection[DsListComp-foldr-build]{Foldr/Build desugaring of list comprehensions}
%*									*
%************************************************************************

@dfListComp@ are the rules used with foldr/build turned on:

\begin{verbatim}
TE[ e | ]            c n = c e n
TE[ e | b , q ]      c n = if b then TE[ e | q ] c n else n
TE[ e | p <- l , q ] c n = let 
				f = \ x b -> case x of
						  p -> TE[ e | q ] c b
						  _ -> b
			   in
			   foldr f n l
\end{verbatim}

\begin{code}
dfListComp :: Id -> Id -- 'c' and 'n'
        -> [Stmt Id]   -- the rest of the qual's
        -> DsM CoreExpr

dfListComp _ _ [] = panic "dfListComp"

dfListComp c_id n_id (LastStmt body _ : quals) 
  = ASSERT( null quals )
    do { core_body <- dsLExpr body
       ; return (mkApps (Var c_id) [core_body, Var n_id]) }

	-- Non-last: must be a guard
dfListComp c_id n_id (ExprStmt guard _ _ _  : quals) = do
    core_guard <- dsLExpr guard
    core_rest <- dfListComp c_id n_id quals
    return (mkIfThenElse core_guard core_rest (Var n_id))

dfListComp c_id n_id (LetStmt binds : quals) = do
    -- new in 1.3, local bindings
    core_rest <- dfListComp c_id n_id quals
    dsLocalBinds binds core_rest

dfListComp c_id n_id (stmt@(TransformStmt {}) : quals) = do
    (inner_list_expr, pat) <- dsTransformStmt stmt
    -- Anyway, we bind the newly transformed list via the generic binding function
    dfBindComp c_id n_id (pat, inner_list_expr) quals 

dfListComp c_id n_id (stmt@(GroupStmt {}) : quals) = do
    (inner_list_expr, pat) <- dsGroupStmt stmt
    -- Anyway, we bind the newly grouped list via the generic binding function
    dfBindComp c_id n_id (pat, inner_list_expr) quals 
    
dfListComp c_id n_id (BindStmt pat list1 _ _ : quals) = do
    -- evaluate the two lists
    core_list1 <- dsLExpr list1
    
    -- Do the rest of the work in the generic binding builder
    dfBindComp c_id n_id (pat, core_list1) quals
               
dfBindComp :: Id -> Id	        -- 'c' and 'n'
       -> (LPat Id, CoreExpr)
	   -> [Stmt Id] 	        -- the rest of the qual's
	   -> DsM CoreExpr
dfBindComp c_id n_id (pat, core_list1) quals = do
    -- find the required type
    let x_ty   = hsLPatType pat
        b_ty   = idType n_id

    -- create some new local id's
    [b, x] <- newSysLocalsDs [b_ty, x_ty]

    -- build rest of the comprehesion
    core_rest <- dfListComp c_id b quals

    -- build the pattern match
    core_expr <- matchSimply (Var x) (StmtCtxt ListComp)
		pat core_rest (Var b)

    -- now build the outermost foldr, and return
    mkFoldrExpr x_ty b_ty (mkLams [x, b] core_expr) (Var n_id) core_list1
\end{code}

%************************************************************************
%*									*
\subsection[DsFunGeneration]{Generation of zip/unzip functions for use in desugaring}
%*									*
%************************************************************************

\begin{code}

mkZipBind :: [Type] -> DsM (Id, CoreExpr)
-- mkZipBind [t1, t2] 
-- = (zip, \as1:[t1] as2:[t2] 
--	   -> case as1 of 
--		[] -> []
--		(a1:as'1) -> case as2 of
--				[] -> []
--				(a2:as'2) -> (a1, a2) : zip as'1 as'2)]

mkZipBind elt_tys = do
    ass  <- mapM newSysLocalDs  elt_list_tys
    as'  <- mapM newSysLocalDs  elt_tys
    as's <- mapM newSysLocalDs  elt_list_tys
    
    zip_fn <- newSysLocalDs zip_fn_ty
    
    let inner_rhs = mkConsExpr elt_tuple_ty 
			(mkBigCoreVarTup as')
			(mkVarApps (Var zip_fn) as's)
        zip_body  = foldr mk_case inner_rhs (zip3 ass as' as's)
    
    return (zip_fn, mkLams ass zip_body)
  where
    elt_list_tys      = map mkListTy elt_tys
    elt_tuple_ty      = mkBigCoreTupTy elt_tys
    elt_tuple_list_ty = mkListTy elt_tuple_ty
    
    zip_fn_ty         = mkFunTys elt_list_tys elt_tuple_list_ty

    mk_case (as, a', as') rest
	  = Case (Var as) as elt_tuple_list_ty
		  [(DataAlt nilDataCon,  [],        mkNilExpr elt_tuple_ty),
		   (DataAlt consDataCon, [a', as'], rest)]
			-- Increasing order of tag
            
            
mkUnzipBind :: [Type] -> DsM (Id, CoreExpr)
-- mkUnzipBind [t1, t2] 
-- = (unzip, \ys :: [(t1, t2)] -> foldr (\ax :: (t1, t2) axs :: ([t1], [t2])
--     -> case ax of
--      (x1, x2) -> case axs of
--                (xs1, xs2) -> (x1 : xs1, x2 : xs2))
--      ([], [])
--      ys)
-- 
-- We use foldr here in all cases, even if rules are turned off, because we may as well!
mkUnzipBind elt_tys = do
    ax  <- newSysLocalDs elt_tuple_ty
    axs <- newSysLocalDs elt_list_tuple_ty
    ys  <- newSysLocalDs elt_tuple_list_ty
    xs  <- mapM newSysLocalDs elt_tys
    xss <- mapM newSysLocalDs elt_list_tys
    
    unzip_fn <- newSysLocalDs unzip_fn_ty

    [us1, us2] <- sequence [newUniqueSupply, newUniqueSupply]

    let nil_tuple = mkBigCoreTup (map mkNilExpr elt_tys)
        
        concat_expressions = map mkConcatExpression (zip3 elt_tys (map Var xs) (map Var xss))
        tupled_concat_expression = mkBigCoreTup concat_expressions
        
        folder_body_inner_case = mkTupleCase us1 xss tupled_concat_expression axs (Var axs)
        folder_body_outer_case = mkTupleCase us2 xs folder_body_inner_case ax (Var ax)
        folder_body = mkLams [ax, axs] folder_body_outer_case
        
    unzip_body <- mkFoldrExpr elt_tuple_ty elt_list_tuple_ty folder_body nil_tuple (Var ys)
    return (unzip_fn, mkLams [ys] unzip_body)
  where
    elt_tuple_ty       = mkBigCoreTupTy elt_tys
    elt_tuple_list_ty  = mkListTy elt_tuple_ty
    elt_list_tys       = map mkListTy elt_tys
    elt_list_tuple_ty  = mkBigCoreTupTy elt_list_tys
    
    unzip_fn_ty        = elt_tuple_list_ty `mkFunTy` elt_list_tuple_ty
            
    mkConcatExpression (list_element_ty, head, tail) = mkConsExpr list_element_ty head tail
\end{code}

%************************************************************************
%*									*
\subsection[DsPArrComp]{Desugaring of array comprehensions}
%*									*
%************************************************************************

\begin{code}

-- entry point for desugaring a parallel array comprehension
--
--   [:e | qss:] = <<[:e | qss:]>> () [:():]
--
dsPArrComp :: [Stmt Id] 
            -> DsM CoreExpr

-- Special case for parallel comprehension
dsPArrComp (ParStmt qss _ _ _ : quals) = dePArrParComp qss quals

-- Special case for simple generators:
--
--  <<[:e' | p <- e, qs:]>> = <<[: e' | qs :]>> p e
--
-- if matching again p cannot fail, or else
--
--  <<[:e' | p <- e, qs:]>> = 
--    <<[:e' | qs:]>> p (filterP (\x -> case x of {p -> True; _ -> False}) e)
--
dsPArrComp (BindStmt p e _ _ : qs) = do
    filterP <- dsLookupDPHId filterPName
    ce <- dsLExpr e
    let ety'ce  = parrElemType ce
        false   = Var falseDataConId
        true    = Var trueDataConId
    v <- newSysLocalDs ety'ce
    pred <- matchSimply (Var v) (StmtCtxt PArrComp) p true false
    let gen | isIrrefutableHsPat p = ce
            | otherwise            = mkApps (Var filterP) [Type ety'ce, mkLams [v] pred, ce]
    dePArrComp qs p gen

dsPArrComp qs = do -- no ParStmt in `qs'
    sglP <- dsLookupDPHId singletonPName
    let unitArray = mkApps (Var sglP) [Type unitTy, mkCoreTup []]
    dePArrComp qs (noLoc $ WildPat unitTy) unitArray



-- the work horse
--
dePArrComp :: [Stmt Id] 
	   -> LPat Id		-- the current generator pattern
	   -> CoreExpr		-- the current generator expression
	   -> DsM CoreExpr

dePArrComp [] _ _ = panic "dePArrComp"

--
--  <<[:e' | :]>> pa ea = mapP (\pa -> e') ea
--
dePArrComp (LastStmt e' _ : quals) pa cea
  = ASSERT( null quals )
    do { mapP <- dsLookupDPHId mapPName
       ; let ty = parrElemType cea
       ; (clam, ty'e') <- deLambda ty pa e'
       ; return $ mkApps (Var mapP) [Type ty, Type ty'e', clam, cea] }
--
--  <<[:e' | b, qs:]>> pa ea = <<[:e' | qs:]>> pa (filterP (\pa -> b) ea)
--
dePArrComp (ExprStmt b _ _ _ : qs) pa cea = do
    filterP <- dsLookupDPHId filterPName
    let ty = parrElemType cea
    (clam,_) <- deLambda ty pa b
    dePArrComp qs pa (mkApps (Var filterP) [Type ty, clam, cea])

--
--  <<[:e' | p <- e, qs:]>> pa ea =
--    let ef = \pa -> e
--    in
--    <<[:e' | qs:]>> (pa, p) (crossMap ea ef)
--
-- if matching again p cannot fail, or else
--
--  <<[:e' | p <- e, qs:]>> pa ea = 
--    let ef = \pa -> filterP (\x -> case x of {p -> True; _ -> False}) e
--    in
--    <<[:e' | qs:]>> (pa, p) (crossMapP ea ef)
--
dePArrComp (BindStmt p e _ _ : qs) pa cea = do
    filterP <- dsLookupDPHId filterPName
    crossMapP <- dsLookupDPHId crossMapPName
    ce <- dsLExpr e
    let ety'cea = parrElemType cea
        ety'ce  = parrElemType ce
        false   = Var falseDataConId
        true    = Var trueDataConId
    v <- newSysLocalDs ety'ce
    pred <- matchSimply (Var v) (StmtCtxt PArrComp) p true false
    let cef | isIrrefutableHsPat p = ce
            | otherwise            = mkApps (Var filterP) [Type ety'ce, mkLams [v] pred, ce]
    (clam, _) <- mkLambda ety'cea pa cef
    let ety'cef = ety'ce		    -- filter doesn't change the element type
        pa'     = mkLHsPatTup [pa, p]

    dePArrComp qs pa' (mkApps (Var crossMapP) 
                                 [Type ety'cea, Type ety'cef, cea, clam])
--
--  <<[:e' | let ds, qs:]>> pa ea = 
--    <<[:e' | qs:]>> (pa, (x_1, ..., x_n)) 
--		      (mapP (\v@pa -> let ds in (v, (x_1, ..., x_n))) ea)
--  where
--    {x_1, ..., x_n} = DV (ds)		-- Defined Variables
--
dePArrComp (LetStmt ds : qs) pa cea = do
    mapP <- dsLookupDPHId mapPName
    let xs     = collectLocalBinders ds
        ty'cea = parrElemType cea
    v <- newSysLocalDs ty'cea
    clet <- dsLocalBinds ds (mkCoreTup (map Var xs))
    let'v <- newSysLocalDs (exprType clet)
    let projBody = mkCoreLet (NonRec let'v clet) $ 
                   mkCoreTup [Var v, Var let'v]
        errTy    = exprType projBody
        errMsg   = ptext (sLit "DsListComp.dePArrComp: internal error!")
    cerr <- mkErrorAppDs pAT_ERROR_ID errTy errMsg
    ccase <- matchSimply (Var v) (StmtCtxt PArrComp) pa projBody cerr
    let pa'    = mkLHsPatTup [pa, mkLHsPatTup (map nlVarPat xs)]
        proj   = mkLams [v] ccase
    dePArrComp qs pa' (mkApps (Var mapP) 
                                   [Type ty'cea, Type errTy, proj, cea])
--
-- The parser guarantees that parallel comprehensions can only appear as
-- singeltons qualifier lists, which we already special case in the caller.
-- So, encountering one here is a bug.
--
dePArrComp (ParStmt _ _ _ _ : _) _ _ = 
  panic "DsListComp.dePArrComp: malformed comprehension AST"

--  <<[:e' | qs | qss:]>> pa ea = 
--    <<[:e' | qss:]>> (pa, (x_1, ..., x_n)) 
--		       (zipP ea <<[:(x_1, ..., x_n) | qs:]>>)
--    where
--      {x_1, ..., x_n} = DV (qs)
--
dePArrParComp :: [([LStmt Id], [Id])] -> [Stmt Id] -> DsM CoreExpr
dePArrParComp qss quals = do
    (pQss, ceQss) <- deParStmt qss
    dePArrComp quals pQss ceQss
  where
    deParStmt []             =
      -- empty parallel statement lists have no source representation
      panic "DsListComp.dePArrComp: Empty parallel list comprehension"
    deParStmt ((qs, xs):qss) = do        -- first statement
      let res_expr = mkLHsVarTuple xs
      cqs <- dsPArrComp (map unLoc qs ++ [mkLastStmt res_expr])
      parStmts qss (mkLHsVarPatTup xs) cqs
    ---
    parStmts []             pa cea = return (pa, cea)
    parStmts ((qs, xs):qss) pa cea = do  -- subsequent statements (zip'ed)
      zipP <- dsLookupDPHId zipPName
      let pa'      = mkLHsPatTup [pa, mkLHsVarPatTup xs]
          ty'cea   = parrElemType cea
          res_expr = mkLHsVarTuple xs
      cqs <- dsPArrComp (map unLoc qs ++ [mkLastStmt res_expr])
      let ty'cqs = parrElemType cqs
          cea'   = mkApps (Var zipP) [Type ty'cea, Type ty'cqs, cea, cqs]
      parStmts qss pa' cea'

-- generate Core corresponding to `\p -> e'
--
deLambda :: Type			-- type of the argument
	  -> LPat Id			-- argument pattern
	  -> LHsExpr Id			-- body
	  -> DsM (CoreExpr, Type)
deLambda ty p e =
    mkLambda ty p =<< dsLExpr e

-- generate Core for a lambda pattern match, where the body is already in Core
--
mkLambda :: Type			-- type of the argument
	 -> LPat Id			-- argument pattern
	 -> CoreExpr			-- desugared body
	 -> DsM (CoreExpr, Type)
mkLambda ty p ce = do
    v <- newSysLocalDs ty
    let errMsg = ptext (sLit "DsListComp.deLambda: internal error!")
        ce'ty  = exprType ce
    cerr <- mkErrorAppDs pAT_ERROR_ID ce'ty errMsg
    res <- matchSimply (Var v) (StmtCtxt PArrComp) p ce cerr
    return (mkLams [v] res, ce'ty)

-- obtain the element type of the parallel array produced by the given Core
-- expression
--
parrElemType   :: CoreExpr -> Type
parrElemType e  = 
  case splitTyConApp_maybe (exprType e) of
    Just (tycon, [ty]) | tycon == parrTyCon -> ty
    _							  -> panic
      "DsListComp.parrElemType: not a parallel array type"
\end{code}

Translation for monad comprehensions

\begin{code}

-- | Keep the "context" of a monad comprehension in a small data type to avoid
-- some boilerplate...
data DsMonadComp = DsMonadComp
    { mc_return :: Either (SyntaxExpr Id) (Expr CoreBndr)
    , mc_body   :: LHsExpr Id
    , mc_m_ty   :: Type
    }

--
-- Entry point for monad comprehension desugaring
--
dsMonadComp :: [LStmt Id]       -- the statements
            -> Type             -- the final type
            -> DsM CoreExpr
dsMonadComp stmts res_ty
  = dsMcStmts stmts (DsMonadComp (Left return_op) body m_ty)
  where
    (m_ty, _) = tcSplitAppTy res_ty


dsMcStmts :: [LStmt Id]
          -> DsMonadComp
          -> DsM CoreExpr

-- No statements left for desugaring. Desugar the body after calling "return"
-- on it.
dsMcStmts [] DsMonadComp { mc_return, mc_body }
  = case mc_return of
         Left ret   -> dsLExpr $ noLoc ret `nlHsApp` mc_body
         Right ret' -> do
             { body' <- dsLExpr mc_body
             ; return $ mkApps ret' [body'] }

-- Otherwise desugar each statement step by step
dsMcStmts ((L loc stmt) : lstmts) mc
  = putSrcSpanDs loc (dsMcStmt stmt lstmts mc)


dsMcStmt :: Stmt Id -> [LStmt Id] -> DsM CoreExpr

dsMcStmt (LastStmt body ret_op) stmts
  = ASSERT( null stmts )
    do { body' <- dsLExpr body
       ; ret_op' <- dsExpr ret_op
       ; return (App ret_op' body') }

--   [ .. | let binds, stmts ]
dsMcStmt (LetStmt binds) stmts 
  = do { rest <- dsMcStmts stmts
       ; dsLocalBinds binds rest }

--   [ .. | a <- m, stmts ]
dsMcStmt (BindStmt pat rhs bind_op fail_op) stmts
  = do { rhs' <- dsLExpr rhs
       ; dsMcBindStmt pat rhs' bind_op fail_op stmts }

-- Apply `guard` to the `exp` expression
--
--   [ .. | exp, stmts ]
--
dsMcStmt (ExprStmt exp then_exp guard_exp _) stmts 
  = do { exp'       <- dsLExpr exp
       ; guard_exp' <- dsExpr guard_exp
       ; then_exp'  <- dsExpr then_exp
       ; rest       <- dsMcStmts stmts
       ; return $ mkApps then_exp' [ mkApps guard_exp' [exp']
                                   , rest ] }

-- Transform statements desugar like this:
--
--   [ .. | qs, then f by e ]  ->  f (\q_v -> e) [| qs |]
--
-- where [| qs |] is the desugared inner monad comprehenion generated by the
-- statements `qs`.
dsMcStmt (TransformStmt stmts binders usingExpr maybeByExpr return_op bind_op) stmts_rest
  = do { expr <- dsInnerMonadComp stmts binders return_op
       ; let binders_tup_type = mkBigCoreTupTy $ map idType binders
       ; usingExpr' <- dsLExpr usingExpr
       ; using_args <- case maybeByExpr of
            Nothing -> return [expr]
            Just byExpr -> do
                byExpr' <- dsLExpr byExpr
                us <- newUniqueSupply
                tup_binder <- newSysLocalDs binders_tup_type
                let byExprWrapper = mkTupleCase us binders byExpr' tup_binder (Var tup_binder)
                return [Lam tup_binder byExprWrapper, expr]

       ; let pat = mkBigLHsVarPatTup binders
             rhs = mkApps usingExpr' ((Type binders_tup_type) : using_args)

       ; dsMcBindStmt pat rhs bind_op noSyntaxExpr stmts_rest }

-- Group statements desugar like this:
--
--   [| (q, then group by e using f); rest |]
--   --->  f {qt} (\qv -> e) [| q; return qv |] >>= \ n_tup -> 
--         case unzip n_tup of qv -> [| rest |]
--
-- where   variables (v1:t1, ..., vk:tk) are bound by q
--         qv = (v1, ..., vk)
--         qt = (t1, ..., tk)
--         (>>=) :: m2 a -> (a -> m3 b) -> m3 b
--         f :: forall a. (a -> t) -> m1 a -> m2 (n a)
--         n_tup :: n qt
--         unzip :: n qt -> (n t1, ..., n tk)    (needs Functor n)
--
--   [| q, then group by e using f |]  ->  (f (\q_v -> e) [| q |]) >>= (return . (unzip q_v))
--
-- which is equal to
--
--   [| q, then group by e using f |]  ->  liftM (unzip q_v) (f (\q_v -> e) [| q |])
--
-- where unzip is of the form
--
--   unzip :: n (a,b,c,..) -> (n a,n b,n c,..)
--   unzip m_tuple = ( fmap selN1 m_tuple
--                   , fmap selN2 m_tuple
--                   , .. )
--     where selN1 (a,b,c,..) = a
--           selN2 (a,b,c,..) = b
--           ..
--
dsMcStmt (GroupStmt stmts binderMap by using return_op bind_op fmap_op) stmts_rest
  = do { let (fromBinders, toBinders) = unzip binderMap
             fromBindersTypes         = map idType fromBinders		-- Types ty
             fromBindersTupleTy       = mkBigCoreTupTy fromBindersTypes
             toBindersTypes           = map idType toBinders		-- Types (n ty)
             toBindersTupleTy         = mkBigCoreTupTy toBindersTypes

       -- Desugar an inner comprehension which outputs a list of tuples of the "from" binders
       ; expr <- dsInnerMonadComp stmts fromBinders return_op

       -- Work out what arguments should be supplied to that expression: i.e. is an extraction
       -- function required? If so, create that desugared function and add to arguments
       ; usingExpr' <- dsLExpr (either id noLoc using)
       ; usingArgs <- case by of
                        Nothing   -> return [expr]
                        Just by_e -> do { by_e' <- dsLExpr by_e
                                        ; lam <- matchTuple fromBinders by_e'
                                        ; return [lam, expr] }

       -- Create an unzip function for the appropriate arity and element types
       ; fmap_op' <- dsExpr fmap_op
       ; (unzip_fn, unzip_rhs) <- mkMcUnzipM fmap_op' m_ty fromBindersTypes

       -- Generate the expressions to build the grouped list
       -- Build a pattern that ensures the consumer binds into the NEW binders, 
       -- which hold monads rather than single values
       ; bind_op' <- dsExpr bind_op
       ; let bind_ty = exprType bind_op'    -- m2 (n (a,b,c)) -> (n (a,b,c) -> r1) -> r2
             n_tup_ty = funArgTy $ funArgTy $ funResultTy bind_ty

       ; body      <- dsMcStmts stmts_rest
       ; n_tup_var <- newSysLocalDs n_tup_ty
       ; tup_n_var <- newSysLocalDs (mkBigCoreVarTupTy toBinders)
       ; us        <- newUniqueSupply
       ; let unzip_n_tup = Let (Rec [(unzip_fn, unzip_rhs)]) $
                           App (Var unzip_fn) (Var n_tup_var)
	     -- unzip_n_tup :: (n a, n b, n c)
             body' = mkTupleCase us toBinders body unzip_n_tup (Var tup_n_var)
		   
       ; return (mkApps bind_op' [rhs', Lam n_tup_var body']) }

-- Parallel statements. Use `Control.Monad.Zip.mzip` to zip parallel
-- statements, for example:
--
--   [ body | qs1 | qs2 | qs3 ]
--     ->  [ body | (bndrs1, (bndrs2, bndrs3)) 
--                     <- [bndrs1 | qs1] `mzip` ([bndrs2 | qs2] `mzip` [bndrs3 | qs3]) ]
--
-- where `mzip` has type
--   mzip :: forall a b. m a -> m b -> m (a,b)
-- NB: we need a polymorphic mzip because we call it several times

dsMcStmt (ParStmt pairs mzip_op bind_op return_op) stmts_rest
 = do  { exps <- mapM ds_inner pairs
       ; let qual_tys = map (mkBigCoreVarTupTy . snd) pairs
       ; mzip_op' <- dsExpr mzip_op
       ; (zip_fn, zip_rhs) <- mkMcZipM mzip_op' (mc_m_ty mc) qual_tys

       ; let -- The pattern variables
             vars = map (mkBigLHsVarPatTup . snd) pairs
             -- Pattern with tuples of variables
             -- [v1,v2,v3]  =>  (v1, (v2, v3))
             pat = foldr (\tn tm -> mkBigLHsPatTup [tn, tm]) (last vars) (init vars)
             rhs = Let (Rec [(zip_fn, zip_rhs)]) (mkApps (Var zip_fn) exps)

       ; dsMcBindStmt pat rhs bind_op noSyntaxExpr stmts_rest }
  where
    ds_inner (stmts, bndrs) = dsInnerMonadComp stmts bndrs mono_ret_op
       where 
         mono_ret_op = HsWrap (WpTyApp (mkBigCoreVarTupTy bndrs)) return_op

dsMcStmt stmt _ = pprPanic "dsMcStmt: unexpected stmt" (ppr stmt)


matchTuple :: [Id] -> CoreExpr -> DsM CoreExpr
-- (matchTuple [a,b,c] body)
--       returns the Core term
--  \x. case x of (a,b,c) -> body 
matchTuple ids body
  = do { us <- newUniqueSupply
       ; tup_id <- newSysLocalDs (mkBigLHsVarPatTup ids)
       ; return (Lam tup_id $ mkTupleCase us ids body tup_id (Var tup_id)) }


-- general `rhs' >>= \pat -> stmts` desugaring where `rhs'` is already a
-- desugared `CoreExpr`
dsMcBindStmt :: LPat Id
             -> CoreExpr        -- ^ the desugared rhs of the bind statement
             -> SyntaxExpr Id
             -> SyntaxExpr Id
             -> [LStmt Id]
             -> DsM CoreExpr
dsMcBindStmt pat rhs' bind_op fail_op stmts
  = do  { body     <- dsMcStmts stmts 
        ; bind_op' <- dsExpr bind_op
        ; var      <- selectSimpleMatchVarL pat
        ; let bind_ty = exprType bind_op' 	-- rhs -> (pat -> res1) -> res2
              res1_ty = funResultTy (funArgTy (funResultTy bind_ty))
        ; match <- matchSinglePat (Var var) (StmtCtxt DoExpr) pat
                                  res1_ty (cantFailMatchResult body)
        ; match_code <- handle_failure pat match fail_op
        ; return (mkApps bind_op' [rhs', Lam var match_code]) }

  where
    -- In a monad comprehension expression, pattern-match failure just calls
    -- the monadic `fail` rather than throwing an exception
    handle_failure pat match fail_op
      | matchCanFail match
        = do { fail_op' <- dsExpr fail_op
             ; fail_msg <- mkStringExpr (mk_fail_msg pat)
             ; extractMatchResult match (App fail_op' fail_msg) }
      | otherwise
        = extractMatchResult match (error "It can't fail") 

    mk_fail_msg :: Located e -> String
    mk_fail_msg pat = "Pattern match failure in monad comprehension at " ++ 
                      showSDoc (ppr (getLoc pat))

-- Desugar nested monad comprehensions, for example in `then..` constructs
--    dsInnerMonadComp quals [a,b,c] ret_op
-- returns the desugaring of 
--       [ (a,b,c) | quals ]

dsInnerMonadComp :: [LStmt Id]
                 -> [Id]	-- Return a tuple of these variables
                 -> LHsExpr Id	-- The monomorphic "return" operator
                 -> DsM CoreExpr
dsInnerMonadComp stmts bndrs ret_op
  = dsMcStmts (stmts ++ [noLoc (ReturnStmt (mkBigLHsVarTup bndrs) ret_op)])

-- The `unzip` function for `GroupStmt` in a monad comprehensions
--
--   unzip :: m (a,b,..) -> (m a,m b,..)
--   unzip m_tuple = ( liftM selN1 m_tuple
--                   , liftM selN2 m_tuple
--                   , .. )
--
--   mkMcUnzipM m [t1, t2]
--     = (unzip_fn, \ys :: m (t1, t2) ->
--         ( liftM (selN1 :: (t1, t2) -> t1) ys
--         , liftM (selN2 :: (t1, t2) -> t2) ys
--         ))
--
mkMcUnzipM :: CoreExpr
           -> Type                      -- m
           -> [Type]                    -- [a,b,c,..]
           -> DsM (Id, CoreExpr)
mkMcUnzipM liftM_op m_ty elt_tys
  = do  { ys    <- newSysLocalDs monad_tuple_ty
        ; xs    <- mapM newSysLocalDs elt_tys
        ; scrut <- newSysLocalDs tuple_tys

        ; unzip_fn <- newSysLocalDs unzip_fn_ty

        ; let -- Select one Id from our tuple
              selectExpr n = mkLams [scrut] $ mkTupleSelector xs (xs !! n) scrut (Var scrut)
              -- Apply 'selectVar' and 'ys' to 'liftM'
              tupleElem n = mkApps liftM_op
                                   -- Types (m is figured out by the type checker):
                                   -- liftM :: forall a b. (a -> b) -> m a -> m b
                                   [ Type tuple_tys, Type (elt_tys !! n)
                                   -- Arguments:
                                   , selectExpr n, Var ys ]
              -- The final expression with the big tuple
              unzip_body = mkBigCoreTup [ tupleElem n | n <- [0..length elt_tys - 1] ]

        ; return (unzip_fn, mkLams [ys] unzip_body) }
  where monad_tys       = map (m_ty `mkAppTy`) elt_tys                  -- [m a,m b,m c,..]
        tuple_monad_tys = mkBigCoreTupTy monad_tys                      -- (m a,m b,m c,..)
        tuple_tys       = mkBigCoreTupTy elt_tys                        -- (a,b,c,..)
        monad_tuple_ty  = m_ty `mkAppTy` tuple_tys                      -- m (a,b,c,..)
        unzip_fn_ty     = monad_tuple_ty `mkFunTy` tuple_monad_tys      -- m (a,b,c,..) -> (m a,m b,m c,..)

-- Generate the `mzip` function for `ParStmt` in monad comprehensions, for
-- example:
--
--   mzip :: m t1
--        -> (m t2 -> m t3 -> m (t2, t3))
--        -> m (t1, (t2, t3))
--
--   mkMcZipM m [t1, t2, t3]
--     = (zip_fn, \(q1::t1) (q2::t2) (q3::t3) ->
--         mzip q1 (mzip q2 q3))
--
mkMcZipM :: CoreExpr
         -> Type
         -> [Type]
         -> DsM (Id, CoreExpr)

mkMcZipM mzip_op m_ty tys@(_:_:_) -- min. 2 types
 = do  { (ids, t1, tuple_ty, zip_body) <- loop tys
       ; zip_fn <- newSysLocalDs $
                       (m_ty `mkAppTy` t1)
                       `mkFunTy`
                       (m_ty `mkAppTy` tuple_ty)
                       `mkFunTy`
                       (m_ty `mkAppTy` mkBigCoreTupTy [t1, tuple_ty])
       ; return (zip_fn, mkLams ids zip_body) }

 where 
       -- loop :: [Type] -> DsM ([Id], Type, [Type], CoreExpr)
       loop [t1, t2] = do -- last run of the `loop`
           { ids@[a,b] <- newSysLocalsDs (map (m_ty `mkAppTy`) [t1,t2])
           ; let zip_body = mkApps mzip_op [ Type t1, Type t2 , Var a, Var b ]
           ; return (ids, t1, t2, zip_body) }

       loop (t1:tr) = do
           { -- Get ty, ids etc from the "inner" zip
             (ids', t1', t2', zip_body') <- loop tr

           ; a <- newSysLocalDs $ m_ty `mkAppTy` t1
           ; let tuple_ty' = mkBigCoreTupTy [t1', t2']
                 zip_body = mkApps mzip_op [ Type t1, Type tuple_ty', Var a, zip_body' ]
           ; return ((a:ids'), t1, tuple_ty', zip_body) }

-- This case should never happen:
mkMcZipM _ _ tys = pprPanic "mkMcZipM: unexpected argument" (ppr tys)

\end{code}
