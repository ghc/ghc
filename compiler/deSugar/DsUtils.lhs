%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Utilities for desugaring

This module exports some utility functions of no great interest.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Utility functions for constructing Core syntax, principally for desugaring
module DsUtils (
	EquationInfo(..), 
	firstPat, shiftEqns,

	MatchResult(..), CanItFail(..), 
	cantFailMatchResult, alwaysFailMatchResult,
	extractMatchResult, combineMatchResults, 
	adjustMatchResult,  adjustMatchResultDs,
	mkCoLetMatchResult, mkViewMatchResult, mkGuardedMatchResult, 
	matchCanFail, mkEvalMatchResult,
	mkCoPrimCaseMatchResult, mkCoAlgCaseMatchResult,
	wrapBind, wrapBinds,

	mkErrorAppDs, mkCoreAppDs, mkCoreAppsDs,

        seqVar,

        -- LHs tuples
        mkLHsVarPatTup, mkLHsPatTup, mkVanillaTuplePat,
        mkBigLHsVarTup, mkBigLHsTup, mkBigLHsVarPatTup, mkBigLHsPatTup,

        mkSelectorBinds,

	selectSimpleMatchVarL, selectMatchVars, selectMatchVar,
        mkOptTickBox, mkBinaryTickBox
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	Match ( matchSimply )

import HsSyn
import TcHsSyn
import TcType( tcSplitTyConApp )
import CoreSyn
import DsMonad

import CoreUtils
import MkCore
import MkId
import Id
import Literal
import TyCon
import DataCon
import Type
import Coercion
import TysPrim
import TysWiredIn
import BasicTypes
import UniqSet
import UniqSupply
import Module
import PrelNames
import Outputable
import SrcLoc
import Util
import DynFlags
import FastString

import Control.Monad    ( zipWithM )
\end{code}


%************************************************************************
%*									*
\subsection{ Selecting match variables}
%*									*
%************************************************************************

We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up.

\begin{code}
selectSimpleMatchVarL :: LPat Id -> DsM Id
selectSimpleMatchVarL pat = selectMatchVar (unLoc pat)

-- (selectMatchVars ps tys) chooses variables of type tys
-- to use for matching ps against.  If the pattern is a variable,
-- we try to use that, to save inventing lots of fresh variables.
--
-- OLD, but interesting note:
--    But even if it is a variable, its type might not match.  Consider
--	data T a where
--	  T1 :: Int -> T Int
--	  T2 :: a   -> T a
--
--	f :: T a -> a -> Int
--	f (T1 i) (x::Int) = x
--	f (T2 i) (y::a)   = 0
--    Then we must not choose (x::Int) as the matching variable!
-- And nowadays we won't, because the (x::Int) will be wrapped in a CoPat

selectMatchVars :: [Pat Id] -> DsM [Id]
selectMatchVars ps = mapM selectMatchVar ps

selectMatchVar :: Pat Id -> DsM Id
selectMatchVar (BangPat pat) = selectMatchVar (unLoc pat)
selectMatchVar (LazyPat pat) = selectMatchVar (unLoc pat)
selectMatchVar (ParPat pat)  = selectMatchVar (unLoc pat)
selectMatchVar (VarPat var)  = return (localiseId var)  -- Note [Localise pattern binders]
selectMatchVar (AsPat var _) = return (unLoc var)
selectMatchVar other_pat     = newSysLocalDs (hsPatType other_pat)
				  -- OK, better make up one...
\end{code}

Note [Localise pattern binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider     module M where
               [Just a] = e
After renaming it looks like
             module M where
               [Just M.a] = e

We don't generalise, since it's a pattern binding, monomorphic, etc,
so after desugaring we may get something like
             M.a = case e of (v:_) ->
                   case v of Just M.a -> M.a
Notice the "M.a" in the pattern; after all, it was in the original
pattern.  However, after optimisation those pattern binders can become
let-binders, and then end up floated to top level.  They have a
different *unique* by then (the simplifier is good about maintaining
proper scoping), but it's BAD to have two top-level bindings with the
External Name M.a, because that turns into two linker symbols for M.a.
It's quite rare for this to actually *happen* -- the only case I know
of is tc003 compiled with the 'hpc' way -- but that only makes it 
all the more annoying.

To avoid this, we craftily call 'localiseId' in the desugarer, which
simply turns the External Name for the Id into an Internal one, but
doesn't change the unique.  So the desugarer produces this:
             M.a{r8} = case e of (v:_) ->
                       case v of Just a{r8} -> M.a{r8}
The unique is still 'r8', but the binding site in the pattern
is now an Internal Name.  Now the simplifier's usual mechanisms
will propagate that Name to all the occurrence sites, as well as
un-shadowing it, so we'll get
             M.a{r8} = case e of (v:_) ->
                       case v of Just a{s77} -> a{s77}
In fact, even CoreSubst.simplOptExpr will do this, and simpleOptExpr
runs on the output of the desugarer, so all is well by the end of
the desugaring pass.


%************************************************************************
%*									*
%* type synonym EquationInfo and access functions for its pieces	*
%*									*
%************************************************************************
\subsection[EquationInfo-synonym]{@EquationInfo@: a useful synonym}

The ``equation info'' used by @match@ is relatively complicated and
worthy of a type synonym and a few handy functions.

\begin{code}
firstPat :: EquationInfo -> Pat Id
firstPat eqn = ASSERT( notNull (eqn_pats eqn) ) head (eqn_pats eqn)

shiftEqns :: [EquationInfo] -> [EquationInfo]
-- Drop the first pattern in each equation
shiftEqns eqns = [ eqn { eqn_pats = tail (eqn_pats eqn) } | eqn <- eqns ]
\end{code}

Functions on MatchResults

\begin{code}
matchCanFail :: MatchResult -> Bool
matchCanFail (MatchResult CanFail _)  = True
matchCanFail (MatchResult CantFail _) = False

alwaysFailMatchResult :: MatchResult
alwaysFailMatchResult = MatchResult CanFail (\fail -> return fail)

cantFailMatchResult :: CoreExpr -> MatchResult
cantFailMatchResult expr = MatchResult CantFail (\_ -> return expr)

extractMatchResult :: MatchResult -> CoreExpr -> DsM CoreExpr
extractMatchResult (MatchResult CantFail match_fn) _
  = match_fn (error "It can't fail!")

extractMatchResult (MatchResult CanFail match_fn) fail_expr = do
    (fail_bind, if_it_fails) <- mkFailurePair fail_expr
    body <- match_fn if_it_fails
    return (mkCoreLet fail_bind body)


combineMatchResults :: MatchResult -> MatchResult -> MatchResult
combineMatchResults (MatchResult CanFail      body_fn1)
                    (MatchResult can_it_fail2 body_fn2)
  = MatchResult can_it_fail2 body_fn
  where
    body_fn fail = do body2 <- body_fn2 fail
                      (fail_bind, duplicatable_expr) <- mkFailurePair body2
                      body1 <- body_fn1 duplicatable_expr
                      return (Let fail_bind body1)

combineMatchResults match_result1@(MatchResult CantFail _) _
  = match_result1

adjustMatchResult :: DsWrapper -> MatchResult -> MatchResult
adjustMatchResult encl_fn (MatchResult can_it_fail body_fn)
  = MatchResult can_it_fail (\fail -> encl_fn <$> body_fn fail)

adjustMatchResultDs :: (CoreExpr -> DsM CoreExpr) -> MatchResult -> MatchResult
adjustMatchResultDs encl_fn (MatchResult can_it_fail body_fn)
  = MatchResult can_it_fail (\fail -> encl_fn =<< body_fn fail)

wrapBinds :: [(Var,Var)] -> CoreExpr -> CoreExpr
wrapBinds [] e = e
wrapBinds ((new,old):prs) e = wrapBind new old (wrapBinds prs e)

wrapBind :: Var -> Var -> CoreExpr -> CoreExpr
wrapBind new old body	-- NB: this function must deal with term
  | new==old    = body	-- variables, type variables or coercion variables
  | otherwise   = Let (NonRec new (varToCoreExpr old)) body

seqVar :: Var -> CoreExpr -> CoreExpr
seqVar var body = Case (Var var) var (exprType body)
			[(DEFAULT, [], body)]

mkCoLetMatchResult :: CoreBind -> MatchResult -> MatchResult
mkCoLetMatchResult bind = adjustMatchResult (mkCoreLet bind)

-- (mkViewMatchResult var' viewExpr var mr) makes the expression
-- let var' = viewExpr var in mr
mkViewMatchResult :: Id -> CoreExpr -> Id -> MatchResult -> MatchResult
mkViewMatchResult var' viewExpr var = 
    adjustMatchResult (mkCoreLet (NonRec var' (mkCoreAppDs viewExpr (Var var))))

mkEvalMatchResult :: Id -> Type -> MatchResult -> MatchResult
mkEvalMatchResult var ty
  = adjustMatchResult (\e -> Case (Var var) var ty [(DEFAULT, [], e)]) 

mkGuardedMatchResult :: CoreExpr -> MatchResult -> MatchResult
mkGuardedMatchResult pred_expr (MatchResult _ body_fn)
  = MatchResult CanFail (\fail -> do body <- body_fn fail
                                     return (mkIfThenElse pred_expr body fail))

mkCoPrimCaseMatchResult :: Id				-- Scrutinee
                    -> Type                             -- Type of the case
		    -> [(Literal, MatchResult)]		-- Alternatives
		    -> MatchResult			-- Literals are all unlifted
mkCoPrimCaseMatchResult var ty match_alts
  = MatchResult CanFail mk_case
  where
    mk_case fail = do
        alts <- mapM (mk_alt fail) sorted_alts
        return (Case (Var var) var ty ((DEFAULT, [], fail) : alts))

    sorted_alts = sortWith fst match_alts	-- Right order for a Case
    mk_alt fail (lit, MatchResult _ body_fn)
       = ASSERT( not (litIsLifted lit) )
         do body <- body_fn fail
            return (LitAlt lit, [], body)


mkCoAlgCaseMatchResult 
  :: DynFlags
  -> Id					   -- Scrutinee
  -> Type                                  -- Type of exp
  -> [(DataCon, [CoreBndr], MatchResult)]  -- Alternatives (bndrs *include* tyvars, dicts)
  -> MatchResult
mkCoAlgCaseMatchResult dflags var ty match_alts 
  | isNewTyCon tycon		-- Newtype case; use a let
  = ASSERT( null (tail match_alts) && null (tail arg_ids1) )
    mkCoLetMatchResult (NonRec arg_id1 newtype_rhs) match_result1

  | isPArrFakeAlts match_alts	-- Sugared parallel array; use a literal case 
  = MatchResult CanFail mk_parrCase

  | otherwise			-- Datatype case; use a case
  = MatchResult fail_flag mk_case
  where
    tycon = dataConTyCon con1
	-- [Interesting: because of GADTs, we can't rely on the type of 
	--  the scrutinised Id to be sufficiently refined to have a TyCon in it]

	-- Stuff for newtype
    (con1, arg_ids1, match_result1) = ASSERT( notNull match_alts ) head match_alts
    arg_id1 	= ASSERT( notNull arg_ids1 ) head arg_ids1
    var_ty      = idType var
    (tc, ty_args) = tcSplitTyConApp var_ty	-- Don't look through newtypes
    	 	    		    		-- (not that splitTyConApp does, these days)
    newtype_rhs = unwrapNewTypeBody tc ty_args (Var var)
		
	-- Stuff for data types
    data_cons      = tyConDataCons tycon
    match_results  = [match_result | (_,_,match_result) <- match_alts]

    fail_flag | exhaustive_case
	      = foldr orFail CantFail [can_it_fail | MatchResult can_it_fail _ <- match_results]
	      | otherwise
	      = CanFail

    sorted_alts  = sortWith get_tag match_alts
    get_tag (con, _, _) = dataConTag con
    mk_case fail = do alts <- mapM (mk_alt fail) sorted_alts
                      return (mkWildCase (Var var) (idType var) ty (mk_default fail ++ alts))

    mk_alt fail (con, args, MatchResult _ body_fn)
      = do { body <- body_fn fail
           ; case dataConBoxer con of {
                Nothing -> return (DataAlt con, args, body) ;
                Just (DCB boxer) -> 
        do { us <- newUniqueSupply
           ; let (rep_ids, binds) = initUs_ us (boxer ty_args args)
           ; return (DataAlt con, rep_ids, mkLets binds body) } } }

    mk_default fail | exhaustive_case = []
		    | otherwise       = [(DEFAULT, [], fail)]

    un_mentioned_constructors
        = mkUniqSet data_cons `minusUniqSet` mkUniqSet [ con | (con, _, _) <- match_alts]
    exhaustive_case = isEmptyUniqSet un_mentioned_constructors

	-- Stuff for parallel arrays
	-- 
	--  * the following is to desugar cases over fake constructors for
	--   parallel arrays, which are introduced by `tidy1' in the `PArrPat'
	--   case
	--
	-- Concerning `isPArrFakeAlts':
	--
	--  * it is *not* sufficient to just check the type of the type
	--   constructor, as we have to be careful not to confuse the real
	--   representation of parallel arrays with the fake constructors;
	--   moreover, a list of alternatives must not mix fake and real
	--   constructors (this is checked earlier on)
	--
	-- FIXME: We actually go through the whole list and make sure that
	--	  either all or none of the constructors are fake parallel
	--	  array constructors.  This is to spot equations that mix fake
	--	  constructors with the real representation defined in
	--	  `PrelPArr'.  It would be nicer to spot this situation
	--	  earlier and raise a proper error message, but it can really
	--	  only happen in `PrelPArr' anyway.
	--
    isPArrFakeAlts [(dcon, _, _)]      = isPArrFakeCon dcon
    isPArrFakeAlts ((dcon, _, _):alts) = 
      case (isPArrFakeCon dcon, isPArrFakeAlts alts) of
        (True , True ) -> True
        (False, False) -> False
        _              -> panic "DsUtils: you may not mix `[:...:]' with `PArr' patterns"
    isPArrFakeAlts [] = panic "DsUtils: unexpectedly found an empty list of PArr fake alternatives"
    --
    mk_parrCase fail = do
      lengthP <- dsDPHBuiltin lengthPVar
      alt <- unboxAlt
      return (mkWildCase (len lengthP) intTy ty [alt])
      where
	elemTy      = case splitTyConApp (idType var) of
		        (_, [elemTy]) -> elemTy
		        _	        -> panic panicMsg
        panicMsg    = "DsUtils.mkCoAlgCaseMatchResult: not a parallel array?"
	len lengthP = mkApps (Var lengthP) [Type elemTy, Var var]
	--
	unboxAlt = do
	  l      <- newSysLocalDs intPrimTy
	  indexP <- dsDPHBuiltin indexPVar
	  alts   <- mapM (mkAlt indexP) sorted_alts
	  return (DataAlt intDataCon, [l], mkWildCase (Var l) intPrimTy ty (dft : alts))
          where
	    dft  = (DEFAULT, [], fail)
	--
	-- each alternative matches one array length (corresponding to one
	-- fake array constructor), so the match is on a literal; each
	-- alternative's body is extended by a local binding for each
	-- constructor argument, which are bound to array elements starting
	-- with the first
	--
	mkAlt indexP (con, args, MatchResult _ bodyFun) = do
	  body <- bodyFun fail
	  return (LitAlt lit, [], mkCoreLets binds body)
	  where
	    lit   = MachInt $ toInteger (dataConSourceArity con)
	    binds = [NonRec arg (indexExpr i) | (i, arg) <- zip [1..] args]
	    --
	    indexExpr i = mkApps (Var indexP) [Type elemTy, Var var, mkIntExpr dflags i]
\end{code}

%************************************************************************
%*									*
\subsection{Desugarer's versions of some Core functions}
%*									*
%************************************************************************

\begin{code}
mkErrorAppDs :: Id 		-- The error function
	     -> Type		-- Type to which it should be applied
	     -> SDoc		-- The error message string to pass
	     -> DsM CoreExpr

mkErrorAppDs err_id ty msg = do
    src_loc <- getSrcSpanDs
    dflags <- getDynFlags
    let
        full_msg = showSDoc dflags (hcat [ppr src_loc, text "|", msg])
        core_msg = Lit (mkMachString full_msg)
        -- mkMachString returns a result of type String#
    return (mkApps (Var err_id) [Type ty, core_msg])
\end{code}

'mkCoreAppDs' and 'mkCoreAppsDs' hand the special-case desugaring of 'seq'.

Note [Desugaring seq (1)]  cf Trac #1031
~~~~~~~~~~~~~~~~~~~~~~~~~
   f x y = x `seq` (y `seq` (# x,y #))

The [CoreSyn let/app invariant] means that, other things being equal, because 
the argument to the outer 'seq' has an unlifted type, we'll use call-by-value thus:

   f x y = case (y `seq` (# x,y #)) of v -> x `seq` v

But that is bad for two reasons: 
  (a) we now evaluate y before x, and 
  (b) we can't bind v to an unboxed pair

Seq is very, very special!  So we recognise it right here, and desugar to
        case x of _ -> case y of _ -> (# x,y #)

Note [Desugaring seq (2)]  cf Trac #2273
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   let chp = case b of { True -> fst x; False -> 0 }
   in chp `seq` ...chp...
Here the seq is designed to plug the space leak of retaining (snd x)
for too long.

If we rely on the ordinary inlining of seq, we'll get
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of _ { I# -> ...chp... }

But since chp is cheap, and the case is an alluring contet, we'll
inline chp into the case scrutinee.  Now there is only one use of chp,
so we'll inline a second copy.  Alas, we've now ruined the purpose of
the seq, by re-introducing the space leak:
    case (case b of {True -> fst x; False -> 0}) of
      I# _ -> ...case b of {True -> fst x; False -> 0}...

We can try to avoid doing this by ensuring that the binder-swap in the
case happens, so we get his at an early stage:
   case chp of chp2 { I# -> ...chp2... }
But this is fragile.  The real culprit is the source program.  Perhaps we
should have said explicitly
   let !chp2 = chp in ...chp2...

But that's painful.  So the code here does a little hack to make seq
more robust: a saturated application of 'seq' is turned *directly* into
the case expression, thus:
   x  `seq` e2 ==> case x of x -> e2    -- Note shadowing!
   e1 `seq` e2 ==> case x of _ -> e2

So we desugar our example to:
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of chp { I# -> ...chp... }
And now all is well.

The reason it's a hack is because if you define mySeq=seq, the hack
won't work on mySeq.  

Note [Desugaring seq (3)] cf Trac #2409
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The isLocalId ensures that we don't turn 
        True `seq` e
into
        case True of True { ... }
which stupidly tries to bind the datacon 'True'. 

\begin{code}
mkCoreAppDs  :: CoreExpr -> CoreExpr -> CoreExpr
mkCoreAppDs (Var f `App` Type ty1 `App` Type ty2 `App` arg1) arg2
  | f `hasKey` seqIdKey            -- Note [Desugaring seq (1), (2)]
  = Case arg1 case_bndr ty2 [(DEFAULT,[],arg2)]
  where
    case_bndr = case arg1 of
                   Var v1 | isLocalId v1 -> v1        -- Note [Desugaring seq (2) and (3)]
                   _                     -> mkWildValBinder ty1

mkCoreAppDs fun arg = mkCoreApp fun arg	 -- The rest is done in MkCore

mkCoreAppsDs :: CoreExpr -> [CoreExpr] -> CoreExpr
mkCoreAppsDs fun args = foldl mkCoreAppDs fun args
\end{code}


%************************************************************************
%*									*
\subsection[mkSelectorBind]{Make a selector bind}
%*									*
%************************************************************************

This is used in various places to do with lazy patterns.
For each binder $b$ in the pattern, we create a binding:
\begin{verbatim}
    b = case v of pat' -> b'
\end{verbatim}
where @pat'@ is @pat@ with each binder @b@ cloned into @b'@.

ToDo: making these bindings should really depend on whether there's
much work to be done per binding.  If the pattern is complex, it
should be de-mangled once, into a tuple (and then selected from).
Otherwise the demangling can be in-line in the bindings (as here).

Boring!  Boring!  One error message per binder.  The above ToDo is
even more helpful.  Something very similar happens for pattern-bound
expressions.

Note [mkSelectorBinds]
~~~~~~~~~~~~~~~~~~~~~~
Given   p = e, where p binds x,y
we are going to make EITHER

EITHER (A)   v = e   (where v is fresh)
             x = case v of p -> x
             y = case v of p -> y

OR (B)       t = case e of p -> (x,y)
             x = case t of (x,_) -> x
             y = case t of (_,y) -> y

We do (A) when 
 * Matching the pattern is cheap so we don't mind
   doing it twice.  
 * Or if the pattern binds only one variable (so we'll only
   match once)
 * AND the pattern can't fail (else we tiresomely get two inexhaustive 
   pattern warning messages)

Otherwise we do (B).  Really (A) is just an optimisation for very common
cases like
     Just x = e
     (p,q) = e

\begin{code}
mkSelectorBinds :: [Maybe (Tickish Id)]  -- ticks to add, possibly
                -> LPat Id      -- The pattern
		-> CoreExpr	-- Expression to which the pattern is bound
		-> DsM [(Id,CoreExpr)]

mkSelectorBinds ticks (L _ (VarPat v)) val_expr
  = return [(v, case ticks of
                  [t] -> mkOptTickBox t val_expr
                  _   -> val_expr)]

mkSelectorBinds ticks pat val_expr
  | null binders 
  = return []

  | isSingleton binders || is_simple_lpat pat
    -- See Note [mkSelectorBinds]
  = do { val_var <- newSysLocalDs (hsLPatType pat)
        -- Make up 'v' in Note [mkSelectorBinds]
        -- NB: give it the type of *pattern* p, not the type of the *rhs* e.
        -- This does not matter after desugaring, but there's a subtle 
        -- issue with implicit parameters. Consider
        --      (x,y) = ?i
        -- Then, ?i is given type {?i :: Int}, a PredType, which is opaque
        -- to the desugarer.  (Why opaque?  Because newtypes have to be.  Why
        -- does it get that type?  So that when we abstract over it we get the
        -- right top-level type  (?i::Int) => ...)
        --
        -- So to get the type of 'v', use the pattern not the rhs.  Often more
        -- efficient too.

        -- For the error message we make one error-app, to avoid duplication.
        -- But we need it at different types... so we use coerce for that
       ; err_expr <- mkErrorAppDs iRREFUT_PAT_ERROR_ID  unitTy (ppr pat)
       ; err_var <- newSysLocalDs unitTy
       ; binds <- zipWithM (mk_bind val_var err_var) ticks' binders
       ; return ( (val_var, val_expr) : 
                  (err_var, err_expr) :
                  binds ) }

  | otherwise
  = do { error_expr <- mkErrorAppDs iRREFUT_PAT_ERROR_ID   tuple_ty (ppr pat)
       ; tuple_expr <- matchSimply val_expr PatBindRhs pat local_tuple error_expr
       ; tuple_var <- newSysLocalDs tuple_ty
       ; let mk_tup_bind tick binder
              = (binder, mkOptTickBox tick $
                            mkTupleSelector local_binders binder
                                            tuple_var (Var tuple_var))
       ; return ( (tuple_var, tuple_expr) : zipWith mk_tup_bind ticks' binders ) }
  where
    binders       = collectPatBinders pat
    ticks'        = ticks ++ repeat Nothing

    local_binders = map localiseId binders      -- See Note [Localise pattern binders]
    local_tuple   = mkBigCoreVarTup binders
    tuple_ty      = exprType local_tuple

    mk_bind scrut_var err_var tick bndr_var = do
    -- (mk_bind sv err_var) generates
    --          bv = case sv of { pat -> bv; other -> coerce (type-of-bv) err_var }
    -- Remember, pat binds bv
        rhs_expr <- matchSimply (Var scrut_var) PatBindRhs pat
                                (Var bndr_var) error_expr
        return (bndr_var, mkOptTickBox tick rhs_expr)
      where
        error_expr = mkCast (Var err_var) co
        co         = mkUnsafeCo (exprType (Var err_var)) (idType bndr_var)

    is_simple_lpat p = is_simple_pat (unLoc p)

    is_simple_pat (TuplePat ps Boxed _) = all is_triv_lpat ps
    is_simple_pat pat@(ConPatOut{})     =  isProductTyCon (dataConTyCon (unLoc (pat_con pat)))
                                        && all is_triv_lpat (hsConPatArgs (pat_args pat))
    is_simple_pat (VarPat _)                   = True
    is_simple_pat (ParPat p)                   = is_simple_lpat p
    is_simple_pat _                                    = False

    is_triv_lpat p = is_triv_pat (unLoc p)

    is_triv_pat (VarPat _)  = True
    is_triv_pat (WildPat _) = True
    is_triv_pat (ParPat p)  = is_triv_lpat p
    is_triv_pat _           = False
\end{code}

Creating big tuples and their types for full Haskell expressions.
They work over *Ids*, and create tuples replete with their types,
which is whey they are not in HsUtils.

\begin{code}
mkLHsPatTup :: [LPat Id] -> LPat Id
mkLHsPatTup []     = noLoc $ mkVanillaTuplePat [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $ 
		     mkVanillaTuplePat lpats Boxed

mkLHsVarPatTup :: [Id] -> LPat Id
mkLHsVarPatTup bs  = mkLHsPatTup (map nlVarPat bs)

mkVanillaTuplePat :: [OutPat Id] -> Boxity -> Pat Id
-- A vanilla tuple pattern simply gets its type from its sub-patterns
mkVanillaTuplePat pats box 
  = TuplePat pats box (mkTupleTy (boxityNormalTupleSort box) (map hsLPatType pats))

-- The Big equivalents for the source tuple expressions
mkBigLHsVarTup :: [Id] -> LHsExpr Id
mkBigLHsVarTup ids = mkBigLHsTup (map nlHsVar ids)

mkBigLHsTup :: [LHsExpr Id] -> LHsExpr Id
mkBigLHsTup = mkChunkified mkLHsTupleExpr

-- The Big equivalents for the source tuple patterns
mkBigLHsVarPatTup :: [Id] -> LPat Id
mkBigLHsVarPatTup bs = mkBigLHsPatTup (map nlVarPat bs)

mkBigLHsPatTup :: [LPat Id] -> LPat Id
mkBigLHsPatTup = mkChunkified mkLHsPatTup
\end{code}

%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Generally, we handle pattern matching failure like this: let-bind a
fail-variable, and use that variable if the thing fails:
\begin{verbatim}
	let fail.33 = error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33
		p3 -> fail.33
		p4 -> ...
\end{verbatim}
Then
\begin{itemize}
\item
If the case can't fail, then there'll be no mention of @fail.33@, and the
simplifier will later discard it.

\item
If it can fail in only one way, then the simplifier will inline it.

\item
Only if it is used more than once will the let-binding remain.
\end{itemize}

There's a problem when the result of the case expression is of
unboxed type.  Then the type of @fail.33@ is unboxed too, and
there is every chance that someone will change the let into a case:
\begin{verbatim}
	case error "Help" of
	  fail.33 -> case ....
\end{verbatim}

which is of course utterly wrong.  Rather than drop the condition that
only boxed types can be let-bound, we just turn the fail into a function
for the primitive case:
\begin{verbatim}
	let fail.33 :: Void -> Int#
	    fail.33 = \_ -> error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33 void
		p3 -> fail.33 void
		p4 -> ...
\end{verbatim}

Now @fail.33@ is a function, so it can be let-bound.

\begin{code}
mkFailurePair :: CoreExpr	-- Result type of the whole case expression
	      -> DsM (CoreBind,	-- Binds the newly-created fail variable
				-- to \ _ -> expression
		      CoreExpr)	-- Fail variable applied to realWorld#
-- See Note [Failure thunks and CPR]
mkFailurePair expr
  = do { fail_fun_var <- newFailLocalDs (realWorldStatePrimTy `mkFunTy` ty)
       ; fail_fun_arg <- newSysLocalDs realWorldStatePrimTy
       ; return (NonRec fail_fun_var (Lam fail_fun_arg expr),
                 App (Var fail_fun_var) (Var realWorldPrimId)) }
  where
    ty = exprType expr
\end{code}

Note [Failure thunks and CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we make a failure point we ensure that it
does not look like a thunk. Example:

   let fail = \rw -> error "urk"
   in case x of 
        [] -> fail realWorld#
        (y:ys) -> case ys of
                    [] -> fail realWorld#  
                    (z:zs) -> (y,z)

Reason: we know that a failure point is always a "join point" and is
entered at most once.  Adding a dummy 'realWorld' token argument makes
it clear that sharing is not an issue.  And that in turn makes it more
CPR-friendly.  This matters a lot: if you don't get it right, you lose
the tail call property.  For example, see Trac #3403.

\begin{code}
mkOptTickBox :: Maybe (Tickish Id) -> CoreExpr -> CoreExpr
mkOptTickBox Nothing e        = e
mkOptTickBox (Just tickish) e = Tick tickish e

mkBinaryTickBox :: Int -> Int -> CoreExpr -> DsM CoreExpr
mkBinaryTickBox ixT ixF e = do
       uq <- newUnique 	
       this_mod <- getModule
       let bndr1 = mkSysLocal (fsLit "t1") uq boolTy
       let
           falseBox = Tick (HpcTick this_mod ixF) (Var falseDataConId)
           trueBox  = Tick (HpcTick this_mod ixT) (Var trueDataConId)
       --
       return $ Case e bndr1 boolTy
                       [ (DataAlt falseDataCon, [], falseBox)
                       , (DataAlt trueDataCon,  [], trueBox)
                       ]
\end{code}
