%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
#include "HsVersions.h"

module SimplUtils (

	floatExposesHNF,
	
	mkCoTyLamTryingEta, mkCoLamTryingEta,

	etaExpandCount,
	
	mkIdentityAlts,

	simplIdWantsToBeINLINEd,

	type_ok_for_let_to_case
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty

import TaggedCore
import PlainCore
import SimplEnv
import SimplMonad

import BinderInfo

import AbsPrel		( primOpIsCheap, realWorldStateTy,
			  buildId, augmentId
			  IF_ATTACK_PRAGMAS(COMMA realWorldTy)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( extractTyVarsFromTy, getTyVarMaybe, isPrimType,
			  splitTypeWithDictsAsArgs, getUniDataTyCon_maybe,
			  applyTy, isFunType, TyVar, TyVarTemplate
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar COMMA cmpClass)
			)
import Id		( getInstantiatedDataConSig, isDataCon, getIdUniType,
			  getIdArity, isBottomingId, idWantsToBeINLINEd,
			  DataCon(..), Id
			)
import IdInfo
import CmdLineOpts	( SimplifierSwitch(..) )
import Maybes		( maybeToBool, Maybe(..) )
import Outputable	-- isExported ...
import Util
\end{code}


Floating
~~~~~~~~
The function @floatExposesHNF@ tells whether let/case floating will
expose a head normal form.  It is passed booleans indicating the
desired strategy.

\begin{code}
floatExposesHNF
	:: Bool 		-- Float let(rec)s out of rhs
	-> Bool 		-- Float cheap primops out of rhs
	-> Bool 		-- OK to duplicate code
	-> CoreExpr bdr Id
	-> Bool

floatExposesHNF float_lets float_primops ok_to_dup rhs
  = try rhs
  where
    try (CoCase (CoPrim _ _ _) (CoPrimAlts alts deflt) )
      | float_primops && (null alts || ok_to_dup)
      = or (try_deflt deflt : map try_alt alts)

    try (CoLet bind body) | float_lets = try body

    --    `build g'
    -- is like a HNF,
    -- because it *will* become one.
    -- likewise for `augment g h'
    --
    try (CoApp (CoTyApp (CoVar bld) _) _) | bld == buildId = True
    try (CoApp (CoApp (CoTyApp (CoVar bld) _) _) _) | bld == augmentId = True

    try other = manifestlyWHNF other
	{- but *not* necessarily "manifestlyBottom other"...

	   We may want to float a let out of a let to expose WHNFs,
	    but to do that to expose a "bottom" is a Bad Idea:
	    let x = let y = ...
		    in ...error ...y... --  manifestly bottom using y
	    in ...
	    =/=>
	    let y = ...
	    in let x = ...error ...y...
	       in ...

	    as y is only used in case of an error, we do not want
	    to allocate it eagerly as that's a waste.
	-}

    try_alt (lit,rhs)               = try rhs

    try_deflt CoNoDefault           = False
    try_deflt (CoBindDefault _ rhs) = try rhs 
\end{code}


Eta reduction on ordinary lambdas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have a go at doing

	\ x y -> f x y	===>  f

But we only do this if it gets rid of a whole lambda, not part.
The idea is that lambdas are often quite helpful: they indicate 
head normal forms, so we don't want to chuck them away lightly.
But if they expose a simple variable then we definitely win.  Even
if they expose a type application we win.  So we check for this special
case.

It does arise:

	f xs = [y | (y,_) <- xs]

gives rise to a recursive function for the list comprehension, and
f turns out to be just a single call to this recursive function.

\begin{code}
mkCoLamTryingEta :: [Id]		-- Args to the lambda
	       -> PlainCoreExpr		-- Lambda body
	       -> PlainCoreExpr

mkCoLamTryingEta [] body = body

mkCoLamTryingEta orig_ids body
  = reduce_it (reverse orig_ids) body
  where
    bale_out = mkCoLam orig_ids body

    reduce_it [] residual
      | residual_ok residual = residual
      | otherwise	     = bale_out

    reduce_it (id:ids) (CoApp fun (CoVarAtom arg))
      | id == arg
      && getIdUniType id /= realWorldStateTy
         -- *never* eta-reduce away a PrimIO state token! (WDP 94/11)
      = reduce_it ids fun

    reduce_it ids other = bale_out

    is_elem = isIn "mkCoLamTryingEta"

    -----------
    residual_ok :: PlainCoreExpr -> Bool	-- Checks for type application
						-- and function not one of the 
						-- bound vars
    residual_ok (CoTyApp fun ty) = residual_ok fun
    residual_ok (CoVar v)        = not (v `is_elem` orig_ids)	-- Fun mustn't be one of
								-- the bound ids
    residual_ok other	         = False
\end{code}

Eta expansion
~~~~~~~~~~~~~
@etaExpandCount@ takes an expression, E, and returns an integer n,
such that

	E  ===>   (\x1::t1 x1::t2 ... xn::tn -> E x1 x2 ... xn)

is a safe transformation.  In particular, the transformation should not
cause work to be duplicated, unless it is ``cheap'' (see @manifestlyCheap@ below).

@etaExpandCount@ errs on the conservative side.  It is always safe to return 0.

An application of @error@ is special, because it can absorb as many
arguments as you care to give it.  For this special case we return 100,
to represent "infinity", which is a bit of a hack.

\begin{code}
etaExpandCount :: CoreExpr bdr Id
	       -> Int			-- Number of extra args you can safely abstract

etaExpandCount (CoLam ids body)
  = length ids + etaExpandCount body

etaExpandCount (CoLet bind body) 
  | all manifestlyCheap (rhssOfBind bind) 
  = etaExpandCount body
   
etaExpandCount (CoCase scrut alts)
  | manifestlyCheap scrut 
  = minimum [etaExpandCount rhs | rhs <- rhssOfAlts alts]

etaExpandCount (CoApp fun _) = case etaExpandCount fun of
				0 -> 0
				n -> n-1	-- Knock off one

etaExpandCount fun@(CoTyApp _ _) = eta_fun fun
etaExpandCount fun@(CoVar _)     = eta_fun fun

etaExpandCount other = 0			-- Give up
	-- CoLit, CoCon, CoPrim, 
	-- CoTyLam,
	-- CoScc (pessimistic; ToDo),
	-- CoLet with non-whnf rhs(s),
	-- CoCase with non-whnf scrutinee

eta_fun :: CoreExpr bdr Id 	-- The function
	-> Int			-- How many args it can safely be applied to

eta_fun (CoTyApp fun ty) = eta_fun fun

eta_fun expr@(CoVar v)
  | isBottomingId v			-- Bottoming ids have "infinite arity"
  = 10000				-- Blargh.  Infinite enough!

eta_fun expr@(CoVar v)
  | maybeToBool arity_maybe		-- We know the arity
  = arity
  where
    arity_maybe = arityMaybe (getIdArity v)
    arity 	= case arity_maybe of { Just arity -> arity }

eta_fun other = 0			-- Give up
\end{code}

@manifestlyCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
By ``cheap'' we mean a computation we're willing to duplicate in order
to bring a couple of lambdas together.  The main examples of things
which aren't WHNF but are ``cheap'' are:

  * 	case e of 
	  pi -> ei

	where e, and all the ei are cheap; and

  *	let x = e
	in b

	where e and b are cheap; and

  *	op x1 ... xn

	where op is a cheap primitive operator

\begin{code}
manifestlyCheap :: CoreExpr bndr Id -> Bool

manifestlyCheap (CoVar _)       = True
manifestlyCheap (CoLit _)       = True
manifestlyCheap (CoCon _ _ _)   = True
manifestlyCheap (CoLam _ _)     = True
manifestlyCheap (CoTyLam _ e)   = manifestlyCheap e
manifestlyCheap (CoSCC _ e)     = manifestlyCheap e

manifestlyCheap (CoPrim op _ _) = primOpIsCheap op

manifestlyCheap (CoLet bind body)
  = manifestlyCheap body && all manifestlyCheap (rhssOfBind bind)

manifestlyCheap (CoCase scrut alts)
  = manifestlyCheap scrut && all manifestlyCheap (rhssOfAlts alts)

manifestlyCheap other_expr   -- look for manifest partial application
  = case (collectArgs other_expr) of { (fun, args) ->
    case fun of

      CoVar f | isBottomingId f -> True		-- Application of a function which
						-- always gives bottom; we treat this as
						-- a WHNF, because it certainly doesn't
						-- need to be shared!

      CoVar f -> let
		    num_val_args = length [ a | (ValArg a) <- args ]
		 in 
		 num_val_args == 0 ||		-- Just a type application of
						-- a variable (f t1 t2 t3)
						-- counts as WHNF
		 case (arityMaybe (getIdArity f)) of
		   Nothing     -> False
		   Just arity  -> num_val_args < arity

      _ -> False
    }


-- ToDo: Move to CoreFuns

rhssOfBind :: CoreBinding bndr bdee -> [CoreExpr bndr bdee]

rhssOfBind (CoNonRec _ rhs) = [rhs]
rhssOfBind (CoRec pairs)    = [rhs | (_,rhs) <- pairs]

rhssOfAlts :: CoreCaseAlternatives bndr bdee -> [CoreExpr bndr bdee]

rhssOfAlts (CoAlgAlts alts deflt)  = rhssOfDeflt deflt ++ 
				     [rhs | (_,_,rhs) <- alts]
rhssOfAlts (CoPrimAlts alts deflt) = rhssOfDeflt deflt ++ 
				     [rhs | (_,rhs) <- alts]
rhssOfDeflt CoNoDefault = []
rhssOfDeflt (CoBindDefault _ rhs) = [rhs]
\end{code}

Eta reduction on type lambdas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have a go at doing 

	/\a -> <expr> a	   ===>     <expr>

where <expr> doesn't mention a.
This is sometimes quite useful, because we can get the sequence:

	f ab d = let d1 = ...d... in
		 letrec f' b x = ...d...(f' b)... in
		 f' b
specialise ==> 

	f.Int b = letrec f' b x = ...dInt...(f' b)... in
		  f' b

float ==> 

	f' b x = ...dInt...(f' b)...
	f.Int b = f' b

Now we really want to simplify to 

	f.Int = f'

and then replace all the f's with f.Ints.

N.B. We are careful not to partially eta-reduce a sequence of type
applications since this breaks the specialiser:

	/\ a -> f Char# a  	=NO=> f Char#

\begin{code}
mkCoTyLamTryingEta :: [TyVar] -> PlainCoreExpr -> PlainCoreExpr

mkCoTyLamTryingEta tyvars tylam_body
  = if
	tyvars == tyvar_args &&	-- Same args in same order
	check_fun fun		-- Function left is ok
    then
	-- Eta reduction worked
	fun
    else
	-- The vastly common case
	mkCoTyLam tyvars tylam_body
  where
    (tyvar_args, fun) = strip_tyvar_args [] tylam_body

    strip_tyvar_args args_so_far tyapp@(CoTyApp fun ty)
      = case getTyVarMaybe ty of
	  Just tyvar_arg -> strip_tyvar_args (tyvar_arg:args_so_far) fun
	  Nothing        -> (args_so_far, tyapp)

    strip_tyvar_args args_so_far fun
      = (args_so_far, fun)

    check_fun (CoVar f) = True	 -- Claim: tyvars not mentioned by type of f
    check_fun other     = False

{- OLD:
mkCoTyLamTryingEta :: TyVar -> PlainCoreExpr -> PlainCoreExpr

mkCoTyLamTryingEta tyvar body
  = case body of 
	CoTyApp fun ty ->
	    case getTyVarMaybe ty of
		Just tyvar' | tyvar == tyvar' &&
			      ok fun   			-> fun
			-- Ha!  So it's /\ a -> fun a, and fun is "ok"

		other -> CoTyLam tyvar body
	other -> CoTyLam tyvar body
  where
    is_elem = isIn "mkCoTyLamTryingEta"

    ok :: PlainCoreExpr -> Bool	-- Returns True iff the expression doesn't
				-- mention tyvar

    ok (CoVar v)	= True		-- Claim: tyvar not mentioned by type of v
    ok (CoApp fun arg)  = ok fun	-- Claim: tyvar not mentioned by type of arg
    ok (CoTyApp fun ty) = not (tyvar `is_elem` extractTyVarsFromTy ty) &&
			  ok fun
    ok other            = False
-}
\end{code}

Let to case
~~~~~~~~~~~

Given a type generate the case alternatives

	C a b -> C a b

if there's one constructor, or

	x -> x

if there's many, or if it's a primitive type.


\begin{code}
mkIdentityAlts
	:: UniType		-- type of RHS
	-> SmplM InAlts		-- result

mkIdentityAlts rhs_ty
  | isPrimType rhs_ty
  = newId rhs_ty	`thenSmpl` \ binder ->
    returnSmpl (CoPrimAlts [] (CoBindDefault (binder, bad_occ_info) (CoVar binder)))

  | otherwise
  = case getUniDataTyCon_maybe rhs_ty of
	Just (tycon, ty_args, [data_con]) ->  -- algebraic type suitable for unpacking
	    let
		(_,inst_con_arg_tys,_) = getInstantiatedDataConSig data_con ty_args
	    in
	    newIds inst_con_arg_tys	`thenSmpl` \ new_bindees ->
	    let
		new_binders = [ (b, bad_occ_info) | b <- new_bindees ] 
	    in
	    returnSmpl (
	      CoAlgAlts
		[(data_con, new_binders, CoCon data_con ty_args (map CoVarAtom new_bindees))]
		CoNoDefault
	    )

	_ -> -- Multi-constructor or abstract algebraic type 
	     newId rhs_ty	`thenSmpl` \ binder ->
    	     returnSmpl (CoAlgAlts [] (CoBindDefault (binder,bad_occ_info) (CoVar binder)))
  where
    bad_occ_info = ManyOcc 0	-- Non-committal!
\end{code}

\begin{code}
simplIdWantsToBeINLINEd :: Id -> SimplEnv -> Bool

simplIdWantsToBeINLINEd id env 
  = if switchIsSet env IgnoreINLINEPragma 
    then False
    else idWantsToBeINLINEd id

type_ok_for_let_to_case :: UniType -> Bool

type_ok_for_let_to_case ty 
  = case getUniDataTyCon_maybe ty of
      Nothing                                   -> False
      Just (tycon, ty_args, [])                 -> False
      Just (tycon, ty_args, non_null_data_cons) -> True
      -- Null data cons => type is abstract
\end{code}
