%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Desugaring list comprehensions, monad comprehensions and array comprehensions

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}

module DsListComp ( dsListComp, dsPArrComp, dsMonadComp ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr ( dsExpr, dsLExpr, dsLocalBinds )

import HsSyn
import TcHsSyn
import CoreSyn
import MkCore

import DsMonad          -- the monadery used in the desugarer
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
import ListSetOps( getNth )
import Util
\end{code}

List comprehensions may be desugared in one of two ways: ``ordinary''
(as you would expect if you read SLPJ's book) and ``with foldr/build
turned on'' (if you read Gill {\em et al.}'s paper on the subject).

There will be at least one ``qualifier'' in the input.

\begin{code}
dsListComp :: [ExprLStmt Id]
           -> Type              -- Type of entire list
           -> DsM CoreExpr
dsListComp lquals res_ty = do
    dflags <- getDynFlags
    let quals = map unLoc lquals
        elt_ty = case tcTyConAppArgs res_ty of
                   [elt_ty] -> elt_ty
                   _ -> pprPanic "dsListComp" (ppr res_ty $$ ppr lquals)

    if not (gopt Opt_EnableRewriteRules dflags) || gopt Opt_IgnoreInterfacePragmas dflags
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

    isParallelStmt (ParStmt {}) = True
    isParallelStmt _            = False


-- This function lets you desugar a inner list comprehension and a list of the binders
-- of that comprehension that we need in the outer comprehension into such an expression
-- and the type of the elements that it outputs (tuples of binders)
dsInnerListComp :: (ParStmtBlock Id Id) -> DsM (CoreExpr, Type)
dsInnerListComp (ParStmtBlock stmts bndrs _)
  = do { expr <- dsListComp (stmts ++ [noLoc $ mkLastStmt (mkBigLHsVarTup bndrs)])
                            (mkListTy bndrs_tuple_type)
       ; return (expr, bndrs_tuple_type) }
  where
    bndrs_tuple_type = mkBigCoreVarTupTy bndrs

-- This function factors out commonality between the desugaring strategies for GroupStmt.
-- Given such a statement it gives you back an expression representing how to compute the transformed
-- list and the tuple that you need to bind from that list in order to proceed with your desugaring
dsTransStmt :: ExprStmt Id -> DsM (CoreExpr, LPat Id)
dsTransStmt (TransStmt { trS_form = form, trS_stmts = stmts, trS_bndrs = binderMap
                       , trS_by = by, trS_using = using }) = do
    let (from_bndrs, to_bndrs) = unzip binderMap
        from_bndrs_tys  = map idType from_bndrs
        to_bndrs_tys    = map idType to_bndrs
        to_bndrs_tup_ty = mkBigCoreTupTy to_bndrs_tys

    -- Desugar an inner comprehension which outputs a list of tuples of the "from" binders
    (expr, from_tup_ty) <- dsInnerListComp (ParStmtBlock stmts from_bndrs noSyntaxExpr)

    -- Work out what arguments should be supplied to that expression: i.e. is an extraction
    -- function required? If so, create that desugared function and add to arguments
    usingExpr' <- dsLExpr using
    usingArgs <- case by of
                   Nothing   -> return [expr]
                   Just by_e -> do { by_e' <- dsLExpr by_e
                                   ; lam <- matchTuple from_bndrs by_e'
                                   ; return [lam, expr] }

    -- Create an unzip function for the appropriate arity and element types and find "map"
    unzip_stuff <- mkUnzipBind form from_bndrs_tys
    map_id <- dsLookupGlobalId mapName

    -- Generate the expressions to build the grouped list
    let -- First we apply the grouping function to the inner list
        inner_list_expr = mkApps usingExpr' usingArgs
        -- Then we map our "unzip" across it to turn the lists of tuples into tuples of lists
        -- We make sure we instantiate the type variable "a" to be a list of "from" tuples and
        -- the "b" to be a tuple of "to" lists!
        -- Then finally we bind the unzip function around that expression
        bound_unzipped_inner_list_expr
          = case unzip_stuff of
              Nothing -> inner_list_expr
              Just (unzip_fn, unzip_rhs) -> Let (Rec [(unzip_fn, unzip_rhs)]) $
                                            mkApps (Var map_id) $
                                            [ Type (mkListTy from_tup_ty)
                                            , Type to_bndrs_tup_ty
                                            , Var unzip_fn
                                            , inner_list_expr]

    -- Build a pattern that ensures the consumer binds into the NEW binders,
    -- which hold lists rather than single values
    let pat = mkBigLHsVarPatTup to_bndrs
    return (bound_unzipped_inner_list_expr, pat)

dsTransStmt _ = panic "dsTransStmt: Not given a TransStmt"
\end{code}

%************************************************************************
%*                                                                      *
\subsection[DsListComp-ordinary]{Ordinary desugaring of list comprehensions}
%*                                                                      *
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

deListComp :: [ExprStmt Id] -> CoreExpr -> DsM CoreExpr

deListComp [] _ = panic "deListComp"

deListComp (LastStmt body _ : quals) list
  =     -- Figure 7.4, SLPJ, p 135, rule C above
    ASSERT( null quals )
    do { core_body <- dsLExpr body
       ; return (mkConsExpr (exprType core_body) core_body list) }

        -- Non-last: must be a guard
deListComp (BodyStmt guard _ _ _ : quals) list = do  -- rule B above
    core_guard <- dsLExpr guard
    core_rest <- deListComp quals list
    return (mkIfThenElse core_guard core_rest list)

-- [e | let B, qs] = let B in [e | qs]
deListComp (LetStmt binds : quals) list = do
    core_rest <- deListComp quals list
    dsLocalBinds binds core_rest

deListComp (stmt@(TransStmt {}) : quals) list = do
    (inner_list_expr, pat) <- dsTransStmt stmt
    deBindComp pat inner_list_expr quals list

deListComp (BindStmt pat list1 _ _ : quals) core_list2 = do -- rule A' above
    core_list1 <- dsLExpr list1
    deBindComp pat core_list1 quals core_list2

deListComp (ParStmt stmtss_w_bndrs _ _ : quals) list
  = do { exps_and_qual_tys <- mapM dsInnerListComp stmtss_w_bndrs
       ; let (exps, qual_tys) = unzip exps_and_qual_tys

       ; (zip_fn, zip_rhs) <- mkZipBind qual_tys

        -- Deal with [e | pat <- zip l1 .. ln] in example above
       ; deBindComp pat (Let (Rec [(zip_fn, zip_rhs)]) (mkApps (Var zip_fn) exps))
                    quals list }
  where
        bndrs_s = [bs | ParStmtBlock _ bs _ <- stmtss_w_bndrs]

        -- pat is the pattern ((x1,..,xn), (y1,..,ym)) in the example above
        pat  = mkBigLHsPatTup pats
        pats = map mkBigLHsVarPatTup bndrs_s

deListComp (RecStmt {} : _) _ = panic "deListComp RecStmt"
\end{code}


\begin{code}
deBindComp :: OutPat Id
           -> CoreExpr
           -> [ExprStmt Id]
           -> CoreExpr
           -> DsM (Expr Id)
deBindComp pat core_list1 quals core_list2 = do
    let
        u3_ty@u1_ty = exprType core_list1       -- two names, same thing

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
                   [(DataAlt nilDataCon,  [],       core_list2),
                    (DataAlt consDataCon, [u2, u3], core_match)]
                        -- Increasing order of tag

    return (Let (Rec [(h, rhs)]) letrec_body)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[DsListComp-foldr-build]{Foldr/Build desugaring of list comprehensions}
%*                                                                      *
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
dfListComp :: Id -> Id      -- 'c' and 'n'
        -> [ExprStmt Id]    -- the rest of the qual's
        -> DsM CoreExpr

dfListComp _ _ [] = panic "dfListComp"

dfListComp c_id n_id (LastStmt body _ : quals)
  = ASSERT( null quals )
    do { core_body <- dsLExpr body
       ; return (mkApps (Var c_id) [core_body, Var n_id]) }

        -- Non-last: must be a guard
dfListComp c_id n_id (BodyStmt guard _ _ _  : quals) = do
    core_guard <- dsLExpr guard
    core_rest <- dfListComp c_id n_id quals
    return (mkIfThenElse core_guard core_rest (Var n_id))

dfListComp c_id n_id (LetStmt binds : quals) = do
    -- new in 1.3, local bindings
    core_rest <- dfListComp c_id n_id quals
    dsLocalBinds binds core_rest

dfListComp c_id n_id (stmt@(TransStmt {}) : quals) = do
    (inner_list_expr, pat) <- dsTransStmt stmt
    -- Anyway, we bind the newly grouped list via the generic binding function
    dfBindComp c_id n_id (pat, inner_list_expr) quals

dfListComp c_id n_id (BindStmt pat list1 _ _ : quals) = do
    -- evaluate the two lists
    core_list1 <- dsLExpr list1

    -- Do the rest of the work in the generic binding builder
    dfBindComp c_id n_id (pat, core_list1) quals

dfListComp _ _ (ParStmt {} : _) = panic "dfListComp ParStmt"
dfListComp _ _ (RecStmt {} : _) = panic "dfListComp RecStmt"

dfBindComp :: Id -> Id          -- 'c' and 'n'
           -> (LPat Id, CoreExpr)
           -> [ExprStmt Id]     -- the rest of the qual's
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
%*                                                                      *
\subsection[DsFunGeneration]{Generation of zip/unzip functions for use in desugaring}
%*                                                                      *
%************************************************************************

\begin{code}

mkZipBind :: [Type] -> DsM (Id, CoreExpr)
-- mkZipBind [t1, t2]
-- = (zip, \as1:[t1] as2:[t2]
--         -> case as1 of
--              [] -> []
--              (a1:as'1) -> case as2 of
--                              [] -> []
--                              (a2:as'2) -> (a1, a2) : zip as'1 as'2)]

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


mkUnzipBind :: TransForm -> [Type] -> DsM (Maybe (Id, CoreExpr))
-- mkUnzipBind [t1, t2]
-- = (unzip, \ys :: [(t1, t2)] -> foldr (\ax :: (t1, t2) axs :: ([t1], [t2])
--     -> case ax of
--      (x1, x2) -> case axs of
--                (xs1, xs2) -> (x1 : xs1, x2 : xs2))
--      ([], [])
--      ys)
--
-- We use foldr here in all cases, even if rules are turned off, because we may as well!
mkUnzipBind ThenForm _
 = return Nothing    -- No unzipping for ThenForm
mkUnzipBind _ elt_tys
  = do { ax  <- newSysLocalDs elt_tuple_ty
       ; axs <- newSysLocalDs elt_list_tuple_ty
       ; ys  <- newSysLocalDs elt_tuple_list_ty
       ; xs  <- mapM newSysLocalDs elt_tys
       ; xss <- mapM newSysLocalDs elt_list_tys

       ; unzip_fn <- newSysLocalDs unzip_fn_ty

       ; [us1, us2] <- sequence [newUniqueSupply, newUniqueSupply]

       ; let nil_tuple = mkBigCoreTup (map mkNilExpr elt_tys)
             concat_expressions = map mkConcatExpression (zip3 elt_tys (map Var xs) (map Var xss))
             tupled_concat_expression = mkBigCoreTup concat_expressions

             folder_body_inner_case = mkTupleCase us1 xss tupled_concat_expression axs (Var axs)
             folder_body_outer_case = mkTupleCase us2 xs folder_body_inner_case ax (Var ax)
             folder_body = mkLams [ax, axs] folder_body_outer_case

       ; unzip_body <- mkFoldrExpr elt_tuple_ty elt_list_tuple_ty folder_body nil_tuple (Var ys)
       ; return (Just (unzip_fn, mkLams [ys] unzip_body)) }
  where
    elt_tuple_ty       = mkBigCoreTupTy elt_tys
    elt_tuple_list_ty  = mkListTy elt_tuple_ty
    elt_list_tys       = map mkListTy elt_tys
    elt_list_tuple_ty  = mkBigCoreTupTy elt_list_tys

    unzip_fn_ty        = elt_tuple_list_ty `mkFunTy` elt_list_tuple_ty

    mkConcatExpression (list_element_ty, head, tail) = mkConsExpr list_element_ty head tail
\end{code}

%************************************************************************
%*                                                                      *
\subsection[DsPArrComp]{Desugaring of array comprehensions}
%*                                                                      *
%************************************************************************

\begin{code}

-- entry point for desugaring a parallel array comprehension
--
--   [:e | qss:] = <<[:e | qss:]>> () [:():]
--
dsPArrComp :: [ExprStmt Id]
            -> DsM CoreExpr

-- Special case for parallel comprehension
dsPArrComp (ParStmt qss _ _ : quals) = dePArrParComp qss quals

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
    filterP <- dsDPHBuiltin filterPVar
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
    sglP <- dsDPHBuiltin singletonPVar
    let unitArray = mkApps (Var sglP) [Type unitTy, mkCoreTup []]
    dePArrComp qs (noLoc $ WildPat unitTy) unitArray



-- the work horse
--
dePArrComp :: [ExprStmt Id]
           -> LPat Id           -- the current generator pattern
           -> CoreExpr          -- the current generator expression
           -> DsM CoreExpr

dePArrComp [] _ _ = panic "dePArrComp"

--
--  <<[:e' | :]>> pa ea = mapP (\pa -> e') ea
--
dePArrComp (LastStmt e' _ : quals) pa cea
  = ASSERT( null quals )
    do { mapP <- dsDPHBuiltin mapPVar
       ; let ty = parrElemType cea
       ; (clam, ty'e') <- deLambda ty pa e'
       ; return $ mkApps (Var mapP) [Type ty, Type ty'e', clam, cea] }
--
--  <<[:e' | b, qs:]>> pa ea = <<[:e' | qs:]>> pa (filterP (\pa -> b) ea)
--
dePArrComp (BodyStmt b _ _ _ : qs) pa cea = do
    filterP <- dsDPHBuiltin filterPVar
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
    filterP <- dsDPHBuiltin filterPVar
    crossMapP <- dsDPHBuiltin crossMapPVar
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
    let ety'cef = ety'ce                    -- filter doesn't change the element type
        pa'     = mkLHsPatTup [pa, p]

    dePArrComp qs pa' (mkApps (Var crossMapP)
                                 [Type ety'cea, Type ety'cef, cea, clam])
--
--  <<[:e' | let ds, qs:]>> pa ea =
--    <<[:e' | qs:]>> (pa, (x_1, ..., x_n))
--                    (mapP (\v@pa -> let ds in (v, (x_1, ..., x_n))) ea)
--  where
--    {x_1, ..., x_n} = DV (ds)         -- Defined Variables
--
dePArrComp (LetStmt ds : qs) pa cea = do
    mapP <- dsDPHBuiltin mapPVar
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
dePArrComp (ParStmt {} : _) _ _ =
  panic "DsListComp.dePArrComp: malformed comprehension AST: ParStmt"
dePArrComp (TransStmt {} : _) _ _ = panic "DsListComp.dePArrComp: TransStmt"
dePArrComp (RecStmt   {} : _) _ _ = panic "DsListComp.dePArrComp: RecStmt"

--  <<[:e' | qs | qss:]>> pa ea =
--    <<[:e' | qss:]>> (pa, (x_1, ..., x_n))
--                     (zipP ea <<[:(x_1, ..., x_n) | qs:]>>)
--    where
--      {x_1, ..., x_n} = DV (qs)
--
dePArrParComp :: [ParStmtBlock Id Id] -> [ExprStmt Id] -> DsM CoreExpr
dePArrParComp qss quals = do
    (pQss, ceQss) <- deParStmt qss
    dePArrComp quals pQss ceQss
  where
    deParStmt []             =
      -- empty parallel statement lists have no source representation
      panic "DsListComp.dePArrComp: Empty parallel list comprehension"
    deParStmt (ParStmtBlock qs xs _:qss) = do        -- first statement
      let res_expr = mkLHsVarTuple xs
      cqs <- dsPArrComp (map unLoc qs ++ [mkLastStmt res_expr])
      parStmts qss (mkLHsVarPatTup xs) cqs
    ---
    parStmts []             pa cea = return (pa, cea)
    parStmts (ParStmtBlock qs xs _:qss) pa cea = do  -- subsequent statements (zip'ed)
      zipP <- dsDPHBuiltin zipPVar
      let pa'      = mkLHsPatTup [pa, mkLHsVarPatTup xs]
          ty'cea   = parrElemType cea
          res_expr = mkLHsVarTuple xs
      cqs <- dsPArrComp (map unLoc qs ++ [mkLastStmt res_expr])
      let ty'cqs = parrElemType cqs
          cea'   = mkApps (Var zipP) [Type ty'cea, Type ty'cqs, cea, cqs]
      parStmts qss pa' cea'

-- generate Core corresponding to `\p -> e'
--
deLambda :: Type                        -- type of the argument
          -> LPat Id                    -- argument pattern
          -> LHsExpr Id                 -- body
          -> DsM (CoreExpr, Type)
deLambda ty p e =
    mkLambda ty p =<< dsLExpr e

-- generate Core for a lambda pattern match, where the body is already in Core
--
mkLambda :: Type                        -- type of the argument
         -> LPat Id                     -- argument pattern
         -> CoreExpr                    -- desugared body
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
    _                                                     -> panic
      "DsListComp.parrElemType: not a parallel array type"
\end{code}

Translation for monad comprehensions

\begin{code}
-- Entry point for monad comprehension desugaring
dsMonadComp :: [ExprLStmt Id] -> DsM CoreExpr
dsMonadComp stmts = dsMcStmts stmts

dsMcStmts :: [ExprLStmt Id] -> DsM CoreExpr
dsMcStmts []                    = panic "dsMcStmts"
dsMcStmts (L loc stmt : lstmts) = putSrcSpanDs loc (dsMcStmt stmt lstmts)

---------------
dsMcStmt :: ExprStmt Id -> [ExprLStmt Id] -> DsM CoreExpr

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
dsMcStmt (BodyStmt exp then_exp guard_exp _) stmts
  = do { exp'       <- dsLExpr exp
       ; guard_exp' <- dsExpr guard_exp
       ; then_exp'  <- dsExpr then_exp
       ; rest       <- dsMcStmts stmts
       ; return $ mkApps then_exp' [ mkApps guard_exp' [exp']
                                   , rest ] }

-- Group statements desugar like this:
--
--   [| (q, then group by e using f); rest |]
--   --->  f {qt} (\qv -> e) [| q; return qv |] >>= \ n_tup ->
--         case unzip n_tup of qv' -> [| rest |]
--
-- where   variables (v1:t1, ..., vk:tk) are bound by q
--         qv = (v1, ..., vk)
--         qt = (t1, ..., tk)
--         (>>=) :: m2 a -> (a -> m3 b) -> m3 b
--         f :: forall a. (a -> t) -> m1 a -> m2 (n a)
--         n_tup :: n qt
--         unzip :: n qt -> (n t1, ..., n tk)    (needs Functor n)

dsMcStmt (TransStmt { trS_stmts = stmts, trS_bndrs = bndrs
                    , trS_by = by, trS_using = using
                    , trS_ret = return_op, trS_bind = bind_op
                    , trS_fmap = fmap_op, trS_form = form }) stmts_rest
  = do { let (from_bndrs, to_bndrs) = unzip bndrs
             from_bndr_tys          = map idType from_bndrs     -- Types ty

       -- Desugar an inner comprehension which outputs a list of tuples of the "from" binders
       ; expr <- dsInnerMonadComp stmts from_bndrs return_op

       -- Work out what arguments should be supplied to that expression: i.e. is an extraction
       -- function required? If so, create that desugared function and add to arguments
       ; usingExpr' <- dsLExpr using
       ; usingArgs <- case by of
                        Nothing   -> return [expr]
                        Just by_e -> do { by_e' <- dsLExpr by_e
                                        ; lam <- matchTuple from_bndrs by_e'
                                        ; return [lam, expr] }

       -- Generate the expressions to build the grouped list
       -- Build a pattern that ensures the consumer binds into the NEW binders,
       -- which hold monads rather than single values
       ; bind_op' <- dsExpr bind_op
       ; let bind_ty  = exprType bind_op'    -- m2 (n (a,b,c)) -> (n (a,b,c) -> r1) -> r2
             n_tup_ty = funArgTy $ funArgTy $ funResultTy bind_ty   -- n (a,b,c)
             tup_n_ty = mkBigCoreVarTupTy to_bndrs

       ; body       <- dsMcStmts stmts_rest
       ; n_tup_var  <- newSysLocalDs n_tup_ty
       ; tup_n_var  <- newSysLocalDs tup_n_ty
       ; tup_n_expr <- mkMcUnzipM form fmap_op n_tup_var from_bndr_tys
       ; us         <- newUniqueSupply
       ; let rhs'  = mkApps usingExpr' usingArgs
             body' = mkTupleCase us to_bndrs body tup_n_var tup_n_expr

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

dsMcStmt (ParStmt blocks mzip_op bind_op) stmts_rest
 = do  { exps_w_tys  <- mapM ds_inner blocks   -- Pairs (exp :: m ty, ty)
       ; mzip_op'    <- dsExpr mzip_op

       ; let -- The pattern variables
             pats = [ mkBigLHsVarPatTup bs | ParStmtBlock _ bs _ <- blocks]
             -- Pattern with tuples of variables
             -- [v1,v2,v3]  =>  (v1, (v2, v3))
             pat = foldr1 (\p1 p2 -> mkLHsPatTup [p1, p2]) pats
             (rhs, _) = foldr1 (\(e1,t1) (e2,t2) ->
                                 (mkApps mzip_op' [Type t1, Type t2, e1, e2],
                                  mkBoxedTupleTy [t1,t2]))
                               exps_w_tys

       ; dsMcBindStmt pat rhs bind_op noSyntaxExpr stmts_rest }
  where
    ds_inner (ParStmtBlock stmts bndrs return_op) 
       = do { exp <- dsInnerMonadComp stmts bndrs return_op
            ; return (exp, mkBigCoreVarTupTy bndrs) }

dsMcStmt stmt _ = pprPanic "dsMcStmt: unexpected stmt" (ppr stmt)


matchTuple :: [Id] -> CoreExpr -> DsM CoreExpr
-- (matchTuple [a,b,c] body)
--       returns the Core term
--  \x. case x of (a,b,c) -> body
matchTuple ids body
  = do { us <- newUniqueSupply
       ; tup_id <- newSysLocalDs (mkBigCoreVarTupTy ids)
       ; return (Lam tup_id $ mkTupleCase us ids body tup_id (Var tup_id)) }

-- general `rhs' >>= \pat -> stmts` desugaring where `rhs'` is already a
-- desugared `CoreExpr`
dsMcBindStmt :: LPat Id
             -> CoreExpr        -- ^ the desugared rhs of the bind statement
             -> SyntaxExpr Id
             -> SyntaxExpr Id
             -> [ExprLStmt Id]
             -> DsM CoreExpr
dsMcBindStmt pat rhs' bind_op fail_op stmts
  = do  { body     <- dsMcStmts stmts
        ; bind_op' <- dsExpr bind_op
        ; var      <- selectSimpleMatchVarL pat
        ; let bind_ty = exprType bind_op'       -- rhs -> (pat -> res1) -> res2
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
             ; dflags <- getDynFlags
             ; fail_msg <- mkStringExpr (mk_fail_msg dflags pat)
             ; extractMatchResult match (App fail_op' fail_msg) }
      | otherwise
        = extractMatchResult match (error "It can't fail")

    mk_fail_msg :: DynFlags -> Located e -> String
    mk_fail_msg dflags pat
        = "Pattern match failure in monad comprehension at " ++
          showPpr dflags (getLoc pat)

-- Desugar nested monad comprehensions, for example in `then..` constructs
--    dsInnerMonadComp quals [a,b,c] ret_op
-- returns the desugaring of
--       [ (a,b,c) | quals ]

dsInnerMonadComp :: [ExprLStmt Id]
                 -> [Id]        -- Return a tuple of these variables
                 -> HsExpr Id   -- The monomorphic "return" operator
                 -> DsM CoreExpr
dsInnerMonadComp stmts bndrs ret_op
  = dsMcStmts (stmts ++ [noLoc (LastStmt (mkBigLHsVarTup bndrs) ret_op)])

-- The `unzip` function for `GroupStmt` in a monad comprehensions
--
--   unzip :: m (a,b,..) -> (m a,m b,..)
--   unzip m_tuple = ( liftM selN1 m_tuple
--                   , liftM selN2 m_tuple
--                   , .. )
--
--   mkMcUnzipM fmap ys [t1, t2]
--     = ( fmap (selN1 :: (t1, t2) -> t1) ys
--       , fmap (selN2 :: (t1, t2) -> t2) ys )

mkMcUnzipM :: TransForm
           -> SyntaxExpr TcId   -- fmap
           -> Id                -- Of type n (a,b,c)
           -> [Type]            -- [a,b,c]
           -> DsM CoreExpr      -- Of type (n a, n b, n c)
mkMcUnzipM ThenForm _ ys _
  = return (Var ys) -- No unzipping to do

mkMcUnzipM _ fmap_op ys elt_tys
  = do { fmap_op' <- dsExpr fmap_op
       ; xs       <- mapM newSysLocalDs elt_tys
       ; let tup_ty = mkBigCoreTupTy elt_tys
       ; tup_xs   <- newSysLocalDs tup_ty

       ; let mk_elt i = mkApps fmap_op'  -- fmap :: forall a b. (a -> b) -> n a -> n b
                           [ Type tup_ty, Type (getNth elt_tys i)
                           , mk_sel i, Var ys]

             mk_sel n = Lam tup_xs $
                        mkTupleSelector xs (getNth xs n) tup_xs (Var tup_xs)

       ; return (mkBigCoreTup (map mk_elt [0..length elt_tys - 1])) }
\end{code}
