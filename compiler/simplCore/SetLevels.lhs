
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{SetLevels}

		***************************
			Overview
		***************************

1. We attach binding levels to Core bindings, in preparation for floating
   outwards (@FloatOut@).

2. We also let-ify many expressions (notably case scrutinees), so they
   will have a fighting chance of being floated sensible.

3. We clone the binders of any floatable let-binding, so that when it is
   floated out it will be unique.  (This used to be done by the simplifier
   but the latter now only ensures that there's no shadowing; indeed, even 
   that may not be true.) (Also, see Note [The Reason SetLevels Does Substitution].)

   NOTE: this can't be done using the uniqAway idea, because the variable
 	 must be unique in the whole program, not just its current scope,
	 because two variables in different scopes may float out to the
	 same top level place

   NOTE: Very tiresomely, we must apply this substitution to
	 the rules stored inside a variable too.

   We do *not* clone top-level bindings, because some of them must not change,
   but we *do* clone bindings that are heading for the top level

4. In the expression
	case x of wild { p -> ...wild... }
   we substitute x for wild in the RHS of the case alternatives:
	case x of wild { p -> ...x... }
   This means that a sub-expression involving x is not "trapped" inside the RHS.
   And it's not inconvenient because we already have a substitution.

  Note that this is EXACTLY BACKWARDS from the what the simplifier does.
  The simplifier tries to get rid of occurrences of x, in favour of wild,
  in the hope that there will only be one remaining occurrence of x, namely
  the scrutinee of the case, and we can inline it.  

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# LANGUAGE ViewPatterns #-}

module SetLevels (
	setLevels, 

	Level(..), tOP_LEVEL,
	LevelledBind, LevelledExpr, LevelledBndr,
	FloatSpec(..), floatSpecLevel,

	incMinorLvl, ltMajLvl, ltLvl, isTopLvl
    ) where

#include "HsVersions.h"

import StaticFlags
import DynFlags

import CoreSyn
import CoreUnfold       ( mkInlinableUnfolding )
import CoreMonad	( FloatOutSwitches(..), FinalPassSwitches(..) )
import CoreUtils	( exprType, exprOkForSpeculation )
import CoreArity	( exprBotStrictness_maybe )
import CoreFVs		-- all of it
import Coercion         ( isCoVar )
import CoreSubst	( Subst, emptySubst, extendInScope, substBndr, substRecBndrs,
			  extendIdSubst, extendSubstWithVar, cloneBndr, 
                          cloneRecIdBndrs, substTy, substCo )
import MkCore           ( sortQuantVars ) 

import SMRep            ( WordOff )
import StgCmmArgRep     ( ArgRep(P), argRepSizeW, toArgRep )
import StgCmmLayout     ( mkVirtHeapOffsets )
import StgCmmClosure    ( idPrimRep, addIdReps )

import Demand           ( isStrictDmd, splitStrictSig )
import Id
import IdInfo
import Var
import VarSet
import VarEnv
import Literal		( litIsTrivial )
import Demand           ( StrictSig, increaseStrictSigArity )
import Name		( getOccName, mkSystemVarName )
import OccName		( occNameString )
import Type		( isUnLiftedType, Type, mkPiTypes, tyVarsOfType )
import Coercion         ( tyCoVarsOfCo )
import BasicTypes	( Arity )
import UniqSupply
import Util
import MonadUtils
import Outputable
import FastString

import qualified Data.IntSet as IntSet; import Data.IntSet ( IntSet )
import qualified Data.IntMap as IntMap; import Data.IntMap ( IntMap )
import Data.Maybe       ( isJust, mapMaybe )
\end{code}

%************************************************************************
%*									*
\subsection{Level numbers}
%*									*
%************************************************************************

\begin{code}
type LevelledExpr = TaggedExpr FloatSpec
type LevelledBind = TaggedBind FloatSpec
type LevelledBndr = TaggedBndr FloatSpec

type MajorLevel = Int
data Level = Level MajorLevel	-- Level number of enclosing lambdas
	  	   Int	-- Number of big-lambda and/or case expressions between
			-- here and the nearest enclosing lambda

data FloatSpec 
  = FloatMe Level	-- Float to just inside the binding 
    	    		--    tagged with this level
  | StayPut Level	-- Stay where it is; binding is
    	    		--     tagged with tihs level

floatSpecLevel :: FloatSpec -> Level
floatSpecLevel (FloatMe l) = l
floatSpecLevel (StayPut l) = l
\end{code}

The {\em level number} on a (type-)lambda-bound variable is the
nesting depth of the (type-)lambda which binds it.  The outermost lambda
has level 1, so (Level 0 0) means that the variable is bound outside any lambda.

On an expression, it's the maximum level number of its free
(type-)variables.  On a let(rec)-bound variable, it's the level of its
RHS.  On a case-bound variable, it's the number of enclosing lambdas.

Top-level variables: level~0.  Those bound on the RHS of a top-level
definition but ``before'' a lambda; e.g., the \tr{x} in (levels shown
as ``subscripts'')...
\begin{verbatim}
a_0 = let  b_? = ...  in
	   x_1 = ... b ... in ...
\end{verbatim}

The main function @lvlExpr@ carries a ``context level'' (@ctxt_lvl@).
That's meant to be the level number of the enclosing binder in the
final (floated) program.  If the level number of a sub-expression is
less than that of the context, then it might be worth let-binding the
sub-expression so that it will indeed float.  

If you can float to level @Level 0 0@ worth doing so because then your
allocation becomes static instead of dynamic.  We always start with
context @Level 0 0@.  


Note [FloatOut inside INLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@InlineCtxt@ very similar to @Level 0 0@, but is used for one purpose:
to say "don't float anything out of here".  That's exactly what we
want for the body of an INLINE, where we don't want to float anything
out at all.  See notes with lvlMFE below.

But, check this out:

-- At one time I tried the effect of not float anything out of an InlineMe,
-- but it sometimes works badly.  For example, consider PrelArr.done.  It
-- has the form 	__inline (\d. e)
-- where e doesn't mention d.  If we float this to 
--	__inline (let x = e in \d. x)
-- things are bad.  The inliner doesn't even inline it because it doesn't look
-- like a head-normal form.  So it seems a lesser evil to let things float.
-- In SetLevels we do set the context to (Level 0 0) when we get to an InlineMe
-- which discourages floating out.

So the conclusion is: don't do any floating at all inside an InlineMe.
(In the above example, don't float the {x=e} out of the \d.)

One particular case is that of workers: we don't want to float the
call to the worker outside the wrapper, otherwise the worker might get
inlined into the floated expression, and an importing module won't see
the worker at all.

\begin{code}
instance Outputable FloatSpec where
  ppr (FloatMe l) = char 'F' <> ppr l
  ppr (StayPut l) = ppr l

tOP_LEVEL :: Level
tOP_LEVEL   = Level 0 0

incMajorLvl :: Level -> Level
incMajorLvl (Level major _) = Level (major + 1) 0

incMinorLvl :: Level -> Level
incMinorLvl (Level major minor) = Level major (minor+1)

maxLvl :: Level -> Level -> Level
maxLvl l1@(Level maj1 min1) l2@(Level maj2 min2)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise					   = l2

ltLvl :: Level -> Level -> Bool
ltLvl (Level maj1 min1) (Level maj2 min2)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl (Level maj1 _) (Level maj2 _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl (Level 0 0) = True
isTopLvl _           = False

instance Outputable Level where
  ppr (Level maj min) = hcat [ char '<', int maj, char ',', int min, char '>' ]

instance Eq Level where
  (Level maj1 min1) == (Level maj2 min2) = maj1 == maj2 && min1 == min2
\end{code}


%************************************************************************
%*									*
\subsection{Main level-setting code}
%*									*
%************************************************************************

\begin{code}
setLevels :: DynFlags
          -> FloatOutSwitches
	  -> CoreProgram
	  -> UniqSupply
	  -> [LevelledBind]

setLevels dflags float_lams binds us
  = initLvl us (do_them init_env binds)
  where
    init_env = initialEnv dflags float_lams

    do_them :: LevelEnv -> [CoreBind] -> LvlM [LevelledBind]
    do_them _ [] = return []
    do_them env (b:bs)
      = do { (lvld_bind, env') <- lvlTopBind dflags env b
           ; lvld_binds <- do_them env' bs
           ; return (lvld_bind : lvld_binds) }

lvlTopBind :: DynFlags -> LevelEnv -> Bind Id -> LvlM (LevelledBind, LevelEnv)
lvlTopBind dflags env (NonRec bndr rhs)
  = do rhs' <- lvlExpr tOP_LEVEL env (analyzeFVs (initFVEnv $ finalPass env) rhs)
       let  -- lambda lifting impedes specialization, so: if the old
            -- RHS has an unstable unfolding, "stablize it" so that it
            -- ends up in the .hi file
            bndr1 | lateFloatStabilizeFirst dflags,
                    isFinalPass env, isUnstableUnfolding (realIdUnfolding bndr)
                              = bndr `setIdUnfolding` mkInlinableUnfolding dflags rhs
                  | otherwise = bndr
            bndr2 = TB bndr1 (StayPut tOP_LEVEL)
            env'  = extendLvlEnv env [bndr2]
       return (NonRec bndr2 rhs', env')

lvlTopBind _ env (Rec pairs)
  = do let (bndrs,rhss) = unzip pairs
           bndrs' = [TB b (StayPut tOP_LEVEL) | b <- bndrs]
           env'   = extendLvlEnv env bndrs'
       rhss' <- mapM (lvlExpr tOP_LEVEL env' . analyzeFVs (initFVEnv $ finalPass env)) rhss
       return (Rec (bndrs' `zip` rhss'), env')
\end{code}

%************************************************************************
%*									*
\subsection{Setting expression levels}
%*									*
%************************************************************************

\begin{code}
lvlExpr :: Level		-- ctxt_lvl: Level of enclosing expression
	-> LevelEnv		-- Level of in-scope names/tyvars
	-> CoreExprWithFVIs	-- input expression
	-> LvlM LevelledExpr	-- Result expression
\end{code}

The @ctxt_lvl@ is, roughly, the level of the innermost enclosing
binder.  Here's an example

	v = \x -> ...\y -> let r = case (..x..) of
					..x..
			   in ..

When looking at the rhs of @r@, @ctxt_lvl@ will be 1 because that's
the level of @r@, even though it's inside a level-2 @\y@.  It's
important that @ctxt_lvl@ is 1 and not 2 in @r@'s rhs, because we
don't want @lvlExpr@ to turn the scrutinee of the @case@ into an MFE
--- because it isn't a *maximal* free expression.

If there were another lambda in @r@'s rhs, it would get level-2 as well.

\begin{code}
lvlExpr _ env (_, AnnType ty) = return (Type (substTy (le_subst env) ty))
lvlExpr _ env (_, AnnCoercion co) = return (Coercion (substCo (le_subst env) co))
lvlExpr _ env (_, AnnVar v)   = return (lookupVar env v)
lvlExpr _ _   (_, AnnLit lit) = return (Lit lit)

lvlExpr ctxt_lvl env expr@(_, AnnApp _ _) = do
    let
      (fun, args) = collectAnnArgs expr
    --
    case fun of
         -- float out partial applications.  This is very beneficial
         -- in some cases (-7% runtime -4% alloc over nofib -O2).
         -- In order to float a PAP, there must be a function at the
         -- head of the application, and the application must be
         -- over-saturated with respect to the function's arity.
      (_, AnnVar f) | floatPAPs env &&
                      arity > 0 && arity < n_val_args ->
        do
         let (lapp, rargs) = left (n_val_args - arity) expr []
         rargs' <- mapM (lvlMFE False ctxt_lvl env) rargs
         lapp' <- lvlMFE False ctxt_lvl env lapp
         return (foldl App lapp' rargs')
        where
         n_val_args = count (isValArg . deAnnotate) args
         arity = idArity f

         -- separate out the PAP that we are floating from the extra
         -- arguments, by traversing the spine until we have collected
         -- (n_val_args - arity) value arguments.
         left 0 e               rargs = (e, rargs)
         left n (_, AnnApp f a) rargs
            | isValArg (deAnnotate a) = left (n-1) f (a:rargs)
            | otherwise               = left n     f (a:rargs)
         left _ _ _                   = panic "SetLevels.lvlExpr.left"

         -- No PAPs that we can float: just carry on with the
         -- arguments and the function.
      _otherwise -> do
         args' <- mapM (lvlMFE False ctxt_lvl env) args
         fun'  <- lvlExpr ctxt_lvl env fun
         return (foldl App fun' args')

lvlExpr ctxt_lvl env (_, AnnTick tickish expr) = do
    expr' <- lvlExpr ctxt_lvl env expr
    return (Tick tickish expr')

lvlExpr ctxt_lvl env (_, AnnCast expr (_, co)) = do
    expr' <- lvlExpr ctxt_lvl env expr
    return (Cast expr' (substCo (le_subst env) co))

-- We don't split adjacent lambdas.  That is, given
--	\x y -> (x+1,y)
-- we don't float to give 
--	\x -> let v = x+y in \y -> (v,y)
-- Why not?  Because partial applications are fairly rare, and splitting
-- lambdas makes them more expensive.

lvlExpr ctxt_lvl env expr@(_, AnnLam {}) = do
    new_body <- lvlMFE True new_lvl new_env body
    return (mkLams new_bndrs new_body)
  where 
    (bndrsTB, body)	 = collectAnnBndrs expr
    bndrs = map unTag bndrsTB
    (new_lvl, new_bndrs) = lvlLamBndrs ctxt_lvl bndrs
    new_env 		 = extendLvlEnv env new_bndrs
	-- At one time we called a special verion of collectBinders,
	-- which ignored coercions, because we don't want to split
	-- a lambda like this (\x -> coerce t (\s -> ...))
	-- This used to happen quite a bit in state-transformer programs,
	-- but not nearly so much now non-recursive newtypes are transparent.
	-- [See SetLevels rev 1.50 for a version with this approach.]

lvlExpr ctxt_lvl env (_, AnnLet bind body) = do
    let (bndrs, isLNE) = case bind of
          AnnNonRec (TB b isLNE) _ -> ([b], isLNE)
          AnnRec pairs -> foldr (\ (TB b lne, _) (bs, !isLNE) -> (b : bs, isLNE && lne))
                                ([], True) pairs

    env <- return $ if isLNE then lneLvlEnv env bndrs else env

    (bind', new_lvl, new_env) <- lvlBind ctxt_lvl env isLNE (fvisOf body) bind
    body' <- lvlExpr new_lvl new_env body
    return (Let bind' body')

lvlExpr ctxt_lvl env (_, AnnCase scrut@(scrut_fvis,_) case_bndr ty alts)
  = do { scrut' <- lvlMFE True ctxt_lvl env scrut
       ; lvlCase ctxt_lvl env scrut_fvis scrut' (unTag case_bndr) ty (map unTagAnnAlt alts) }

-------------------------------------------
lvlCase :: Level		-- ctxt_lvl: Level of enclosing expression
	-> LevelEnv		-- Level of in-scope names/tyvars
        -> FVIs  		-- Free vars of input scrutinee
        -> LevelledExpr		-- Processed scrutinee
	-> Id -> Type		-- Case binder and result type
	-> [(AltCon, [Id], CoreExprWithFVIs)]	-- Input alternatives
	-> LvlM LevelledExpr	-- Result expression
lvlCase ctxt_lvl env scrut_fvis scrut' case_bndr ty alts
  | [(con@(DataAlt {}), bs, rhs)] <- alts
  , exprOkForSpeculation scrut'	  -- See Note [Check the output scrutinee for okForSpec]
  , not (isTopLvl dest_lvl)	  -- Can't have top-level cases
  =     -- See Note [Floating cases]
    	-- Always float the case if possible
  	-- Unlike lets we don't insist that it escapes a value lambda
    do { (rhs_env, (case_bndr':bs')) <- cloneVars env (case_bndr:bs) dest_lvl
       	 	   -- We don't need to use extendCaseBndrLvlEnv here
		   -- because we are floating the case outwards so
		   -- no need to do the binder-swap thing
       ; rhs' <- lvlMFE True ctxt_lvl rhs_env rhs
       ; let alt' = (con, [TB b (StayPut dest_lvl) | b <- bs'], rhs')
       ; return (Case scrut' (TB case_bndr' (FloatMe dest_lvl)) ty [alt']) }

  | otherwise	  -- Stays put
  = do { let case_bndr' = TB case_bndr bndr_spec
             alts_env   = extendCaseBndrLvlEnv env scrut' case_bndr'
       ; alts' <- mapM (lvl_alt alts_env) alts
       ; return (Case scrut' case_bndr' ty alts') }
  where
      incd_lvl  = incMinorLvl ctxt_lvl
      bndr_spec = StayPut incd_lvl
      dest_lvl = maxFvLevel (const True) env (prjFreeVars scrut_fvis)
   	      -- Don't abstact over type variables, hence const True

      lvl_alt alts_env (con, bs, rhs)
        = do { rhs' <- lvlMFE True incd_lvl new_env rhs
             ; return (con, bs', rhs') }
        where
          bs'     = [ TB b bndr_spec | b <- bs ]
          new_env = extendLvlEnv alts_env bs'
\end{code}

Note [Floating cases]
~~~~~~~~~~~~~~~~~~~~~
Consider this:
  data T a = MkT !a
  f :: T Int -> blah
  f x vs = case x of { MkT y -> 
             let f vs = ...(case y of I# w -> e)...f..
             in f vs
Here we can float the (case y ...) out , because y is sure
to be evaluated, to give
  f x vs = case x of { MkT y -> 
           caes y of I# w ->
             let f vs = ...(e)...f..
             in f vs

That saves unboxing it every time round the loop.  It's important in
some DPH stuff where we really want to avoid that repeated unboxing in
the inner loop.

Things to note
 * We can't float a case to top level
 * It's worth doing this float even if we don't float
   the case outside a value lambda.  Example
     case x of { 
       MkT y -> (case y of I# w2 -> ..., case y of I# w2 -> ...)
   If we floated the cases out we could eliminate one of them.
 * We only do this with a single-alternative case

Note [Check the output scrutinee for okForSpec]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
  case x of y { 
    A -> ....(case y of alts)....
  }
Because of the binder-swap, the inner case will get substituted to
(case x of ..).  So when testing whether the scrutinee is
okForSpecuation we must be careful to test the *result* scrutinee ('x'
in this case), not the *input* one 'y'.  The latter *is* ok for
speculation here, but the former is not -- and indeed we can't float
the inner case out, at least not unless x is also evaluated at its
binding site.

That's why we apply exprOkForSpeculation to scrut' and not to scrut.

\begin{code}
lvlMFE ::  Bool			-- True <=> strict context [body of case or let]
	-> Level		-- Level of innermost enclosing lambda/tylam
	-> LevelEnv		-- Level of in-scope names/tyvars
	-> CoreExprWithFVIs	-- input expression
	-> LvlM LevelledExpr	-- Result expression
-- lvlMFE is just like lvlExpr, except that it might let-bind
-- the expression, so that it can itself be floated.

lvlMFE _ _ env (_, AnnType ty)
  = return (Type (substTy (le_subst env) ty))

-- No point in floating out an expression wrapped in a coercion or note
-- If we do we'll transform  lvl = e |> co 
--			 to  lvl' = e; lvl = lvl' |> co
-- and then inline lvl.  Better just to float out the payload.
lvlMFE strict_ctxt ctxt_lvl env (_, AnnTick t e)
  = do { e' <- lvlMFE strict_ctxt ctxt_lvl env e
       ; return (Tick t e') }

lvlMFE strict_ctxt ctxt_lvl env (_, AnnCast e (_, co))
  = do	{ e' <- lvlMFE strict_ctxt ctxt_lvl env e
	; return (Cast e' (substCo (le_subst env) co)) }

-- Note [Case MFEs]
lvlMFE True ctxt_lvl env e@(_, AnnCase {})
  = lvlExpr ctxt_lvl env e     -- Don't share cases

lvlMFE strict_ctxt ctxt_lvl env ann_expr@(fvis, _)
  |  isFinalPass env -- see Note [Late Lambda Floating]; TODO float
                     -- anonymous lambdas in the late pass?
  || isUnLiftedType ty		-- Can't let-bind it; see Note [Unlifted MFEs]
     		    		-- This includes coercions, which we don't
				-- want to float anyway
  || notWorthFloating ann_expr abs_vars
  || not float_me
  = 	-- Don't float it out
    lvlExpr ctxt_lvl env ann_expr

  | otherwise	-- Float it out!
  = do expr' <- lvlFloatRhs abs_vars dest_lvl env ann_expr
       var <- newLvlVar abs_vars ty mb_bot
       return (Let (NonRec (TB var (FloatMe dest_lvl)) expr') 
                   (mkVarApps (Var var) abs_vars))
  where
    fvs = prjFreeVars fvis
    expr     = deTag $ deAnnotate ann_expr
    ty       = exprType expr
    mb_bot   = exprBotStrictness_maybe expr
    dest_lvl = destLevel env (isJust mb_bot) fvs
    abs_vars = abstractVars dest_lvl env fvs

	-- A decision to float entails let-binding this thing, and we only do 
	-- that if we'll escape a value lambda, or will go to the top level.
    float_me = dest_lvl `ltMajLvl` ctxt_lvl		-- Escapes a value lambda
    	     	-- OLD CODE: not (exprIsCheap expr) || isTopLvl dest_lvl
		-- 	     see Note [Escaping a value lambda]

            || (isTopLvl dest_lvl 	-- Only float if we are going to the top level
         	&& floatConsts env	--   and the floatConsts flag is on
              	&& not strict_ctxt)	-- Don't float from a strict context	
	  -- We are keen to float something to the top level, even if it does not
	  -- escape a lambda, because then it needs no allocation.  But it's controlled
	  -- by a flag, because doing this too early loses opportunities for RULES
	  -- which (needless to say) are important in some nofib programs
	  -- (gcd is an example).
	  --
	  -- Beware:
	  --	concat = /\ a -> foldr ..a.. (++) []
	  -- was getting turned into
	  --	lvl    = /\ a -> foldr ..a.. (++) []
	  --	concat = /\ a -> lvl a
	  -- which is pretty stupid.  Hence the strict_ctxt test
	  -- 
	  -- Also a strict contxt includes uboxed values, and they
	  -- can't be bound at top level
\end{code}

Note [Unlifted MFEs]
~~~~~~~~~~~~~~~~~~~~
We don't float unlifted MFEs, which potentially loses big opportunites.
For example:
	\x -> f (h y)
where h :: Int -> Int# is expensive. We'd like to float the (h y) outside
the \x, but we don't because it's unboxed.  Possible solution: box it.

Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~
If we see
	f = \x. g (error "urk")
we'd like to float the call to error, to get
	lvl = error "urk"
	f = \x. g lvl
Furthermore, we want to float a bottoming expression even if it has free
variables:
	f = \x. g (let v = h x in error ("urk" ++ v))
Then we'd like to abstact over 'x' can float the whole arg of g:
	lvl = \x. let v = h x in error ("urk" ++ v)
	f = \x. g (lvl x)
See Maessen's paper 1999 "Bottom extraction: factoring error handling out
of functional programs" (unpublished I think).

When we do this, we set the strictness and arity of the new bottoming 
Id, so that it's properly exposed as such in the interface file, even if
this is all happening after strictness analysis.  

Note [Bottoming floats: eta expansion] c.f Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tiresomely, though, the simplifier has an invariant that the manifest
arity of the RHS should be the same as the arity; but we can't call
etaExpand during SetLevels because it works over a decorated form of
CoreExpr.  So we do the eta expansion later, in FloatOut.

Note [Case MFEs]
~~~~~~~~~~~~~~~~
We don't float a case expression as an MFE from a strict context.  Why not?
Because in doing so we share a tiny bit of computation (the switch) but
in exchange we build a thunk, which is bad.  This case reduces allocation 
by 7% in spectral/puzzle (a rather strange benchmark) and 1.2% in real/fem.
Doesn't change any other allocation at all.

\begin{code}
annotateBotStr :: Id -> Maybe (Arity, StrictSig) -> Id
annotateBotStr id Nothing            = id
annotateBotStr id (Just (arity, sig)) = id `setIdArity` arity
     		                              `setIdStrictness` sig

notWorthFloating :: CoreExprWithFVIs -> [Var] -> Bool
-- Returns True if the expression would be replaced by
-- something bigger than it is now.  For example:
--   abs_vars = tvars only:  return True if e is trivial, 
--                           but False for anything bigger
--   abs_vars = [x] (an Id): return True for trivial, or an application (f x)
--   	      	    	     but False for (f x x)
--
-- One big goal is that floating should be idempotent.  Eg if
-- we replace e with (lvl79 x y) and then run FloatOut again, don't want
-- to replace (lvl79 x y) with (lvl83 x y)!

notWorthFloating e abs_vars
  = go e (count isId abs_vars)
  where
    go (_, AnnVar {}) n    = n >= 0
    go (_, AnnLit lit) n   = ASSERT( n==0 ) 
                             litIsTrivial lit	-- Note [Floating literals]
    go (_, AnnCast e _)  n = go e n
    go (_, AnnApp e arg) n 
       | (_, AnnType {}) <- arg = go e n
       | (_, AnnCoercion {}) <- arg = go e n
       | n==0                   = False
       | is_triv arg       	= go e (n-1)
       | otherwise         	= False
    go _ _                 	= False

    is_triv (_, AnnLit {})   	       	  = True	-- Treat all literals as trivial
    is_triv (_, AnnVar {})   	       	  = True	-- (ie not worth floating)
    is_triv (_, AnnCast e _) 	       	  = is_triv e
    is_triv (_, AnnApp e (_, AnnType {})) = is_triv e
    is_triv (_, AnnApp e (_, AnnCoercion {})) = is_triv e
    is_triv _                             = False     
\end{code}

Note [Floating literals]
~~~~~~~~~~~~~~~~~~~~~~~~
It's important to float Integer literals, so that they get shared,
rather than being allocated every time round the loop.
Hence the litIsTrivial.

We'd *like* to share MachStr literal strings too, mainly so we could
CSE them, but alas can't do so directly because they are unlifted.


Note [Escaping a value lambda]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to float even cheap expressions out of value lambdas, 
because that saves allocation.  Consider
	f = \x.  .. (\y.e) ...
Then we'd like to avoid allocating the (\y.e) every time we call f,
(assuming e does not mention x).   

An example where this really makes a difference is simplrun009.

Another reason it's good is because it makes SpecContr fire on functions.
Consider
	f = \x. ....(f (\y.e))....
After floating we get
	lvl = \y.e
	f = \x. ....(f lvl)...
and that is much easier for SpecConstr to generate a robust specialisation for.

The OLD CODE (given where this Note is referred to) prevents floating
of the example above, so I just don't understand the old code.  I
don't understand the old comment either (which appears below).  I
measured the effect on nofib of changing OLD CODE to 'True', and got
zeros everywhere, but a 4% win for 'puzzle'.  Very small 0.5% loss for
'cse'; turns out to be because our arity analysis isn't good enough
yet (mentioned in Simon-nofib-notes).

OLD comment was:
	 Even if it escapes a value lambda, we only
	 float if it's not cheap (unless it'll get all the
	 way to the top).  I've seen cases where we
	 float dozens of tiny free expressions, which cost
	 more to allocate than to evaluate.
	 NB: exprIsCheap is also true of bottom expressions, which
	     is good; we don't want to share them

	It's only Really Bad to float a cheap expression out of a
	strict context, because that builds a thunk that otherwise
	would never be built.  So another alternative would be to
	add 
		|| (strict_ctxt && not (exprIsBottom expr))
	to the condition above. We should really try this out.


%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

The binding stuff works for top level too.

\begin{code}
unTag :: TaggedBndr b -> CoreBndr
unTag (TB b _) = b

unTagAnnAlt :: (AltCon, [TaggedBndr b], AnnExpr (TaggedBndr b) annot) ->
               (AltCon, [  CoreBndr  ], AnnExpr (TaggedBndr b) annot)
unTagAnnAlt (con, args, rhs) = (con, map unTag args, rhs)

class DeTag sort where
  deTag :: sort (TaggedBndr t) -> sort CoreBndr

instance DeTag Expr where
  deTag (Var var) = Var var
  deTag (Lit lit) = Lit lit
  deTag (App fun arg) = App (deTag fun) (deTag arg)
  deTag (Lam (TB b _) e) = Lam b (deTag e)
  deTag (Let bind body) = Let (deTag bind) (deTag body)
  deTag (Case scrut (TB b _) ty alts) = Case (deTag scrut) b ty (map deTag_alt alts)
    where deTag_alt (con, args, rhs) = (con, map unTag args, deTag rhs)
  deTag (Cast e co) = Cast (deTag e) co
  deTag (Tick tick e) = Tick tick (deTag e)
  deTag (Type ty) = Type ty
  deTag (Coercion co) = Coercion co

instance DeTag Bind where
  deTag (NonRec (TB bndr _) rhs) = NonRec bndr (deTag rhs)
  deTag (Rec pairs) = Rec $ map (\(bndr, rhs) -> (unTag bndr, deTag rhs)) pairs

lvlBind :: Level		-- Context level; might be Top even for bindings 
				-- nested in the RHS of a top level binding
	-> LevelEnv
        -> Bool                 -- is it LNE?
        -> FVIs                 -- free variables (& info) of the body
	-> CoreBindWithFVIs
	-> LvlM (LevelledBind, Level, LevelEnv)

lvlBind ctxt_lvl env isLNE body_fvis (AnnNonRec (TB bndr _) rhs@(rhs_fvis,_))
  | isTyVar bndr    -- Don't do anything for TyVar binders
	            --   (simplifier gets rid of them pronto)
  || isCoVar bndr   -- Difficult to fix up CoVar occurrences (see extendPolyLvlEnv)
                    -- so we will ignore this case for now
     = doNotFloat
  | otherwise =
    case decideBindFloat ctxt_lvl env body_fvis (isJust mb_bot) (isFunctionAnn rhs) isLNE (Left (bndr, rhs_fvis)) of
      Nothing -> doNotFloat
      Just p -> uncurry doFloat p
  where
    mb_bot     = exprBotStrictness_maybe (deTag $ deAnnotate rhs)
    bndr_w_str = annotateBotStr bndr mb_bot

    doNotFloat = do
     rhs' <- lvlExpr ctxt_lvl env rhs
     let  (env', bndr') = substLetBndrNonRec env bndr bind_lvl
          bind_lvl      = incMinorLvl ctxt_lvl
          tagged_bndr   = TB bndr' (StayPut bind_lvl)
     return (NonRec tagged_bndr rhs', bind_lvl, env')

    doFloat dest_lvl abs_vars
      | (isTopLvl dest_lvl && isUnLiftedType (idType bndr)) = doNotFloat
	  -- We can't float an unlifted binding to top level, so we don't 
	  -- float it at all.  It's a bit brutal, but unlifted bindings 
	  -- aren't expensive either

      | null abs_vars = do  -- No type abstraction; clone existing binder
        rhs' <- lvlExpr dest_lvl env rhs
        (env', bndr') <- cloneVar env bndr dest_lvl
        return (NonRec (TB bndr' (FloatMe dest_lvl)) rhs', ctxt_lvl, env')

      | otherwise = do  -- Yes, type abstraction; create a new binder, extend substitution, etc
        rhs' <- lvlFloatRhs abs_vars dest_lvl env rhs
        (env', [bndr']) <- newPolyBndrs dest_lvl env abs_vars [bndr_w_str]
        return (NonRec (TB bndr' (FloatMe dest_lvl)) rhs', ctxt_lvl, env')

lvlBind ctxt_lvl env isLNE body_fvis (AnnRec pairsTB) =
  let pairs = map (\(bndr, rhs) -> (unTag bndr, rhs)) pairsTB
      (bndrs,rhss) = unzip pairs

  in
  case decideBindFloat ctxt_lvl env body_fvis False (all isFunctionAnn rhss) isLNE (Right (bndrs, map fvisOf rhss)) of
  Nothing -> do -- decided to not float
--  | Just pinners <- floatDecision emptyVarSet
  -- when (lateRetry env && not (isEmptyVarEnv pinners)) $ tellLvlM $ mkVarEnv [ (b, (b, pinners)) | b <- bndrs ]
    let bind_lvl = incMinorLvl ctxt_lvl
        (env', bndrs') = substLetBndrsRec env bndrs bind_lvl
        tagged_bndrs = [ TB bndr' (StayPut bind_lvl) 
                       | bndr' <- bndrs' ] 
    rhss' <- mapM (lvlExpr bind_lvl env') rhss
    return (Rec (tagged_bndrs `zip` rhss'), bind_lvl, env')

  Just (dest_lvl, abs_vars) -- decided to float
    | null abs_vars -> do
      (new_env, new_bndrs) <- cloneRecVars env bndrs dest_lvl
      new_rhss <- mapM (lvlExpr ctxt_lvl new_env) rhss
      return ( Rec ([TB b (FloatMe dest_lvl) | b <- new_bndrs] `zip` new_rhss)
             , ctxt_lvl, new_env)

  -- ToDo: when enabling the floatLambda stuff,
  --       I think we want to stop doing this
    | doSinglyRecSAT env && isSingleton pairs && count isId abs_vars > 1 -> do
  	-- Special case for self recursion where there are
	-- several variables carried around: build a local loop:	
	--	poly_f = \abs_vars. \lam_vars . letrec f = \lam_vars. rhs in f lam_vars
	-- This just makes the closures a bit smaller.  If we don't do
	-- this, allocation rises significantly on some programs
	--
	-- We could elaborate it for the case where there are several
	-- mutually functions, but it's quite a bit more complicated
	-- 
	-- This all seems a bit ad hoc -- sigh
      let
          (bndr,rhs) = head pairs
          (rhs_lvl, abs_vars_w_lvls) = lvlLamBndrs dest_lvl abs_vars
          rhs_env = extendLvlEnv env abs_vars_w_lvls
      (rhs_env', new_bndr) <- cloneVar rhs_env bndr rhs_lvl
      let
          (lam_bndrsTB, rhs_body)     = collectAnnBndrs rhs
          lam_bndrs                   = map unTag lam_bndrsTB
          (body_lvl, new_lam_bndrs) = lvlLamBndrs rhs_lvl lam_bndrs
          body_env                  = extendLvlEnv rhs_env' new_lam_bndrs
      new_rhs_body <- lvlExpr body_lvl body_env rhs_body
      (poly_env, [poly_bndr]) <- newPolyBndrs dest_lvl env abs_vars [bndr]
      return (Rec [(TB poly_bndr (FloatMe dest_lvl)
                  , mkLams abs_vars_w_lvls $
                    mkLams new_lam_bndrs $
                    Let (Rec [( TB new_bndr (StayPut rhs_lvl)
                             , mkLams new_lam_bndrs new_rhs_body)])
                      (mkVarApps (Var new_bndr) lam_bndrs))]
             , ctxt_lvl
             , poly_env)

    | otherwise -> do  -- Non-null abs_vars
      (new_env, new_bndrs) <- newPolyBndrs dest_lvl env abs_vars bndrs
      new_rhss <- mapM (lvlFloatRhs abs_vars dest_lvl new_env) rhss
      return ( Rec ([TB b (FloatMe dest_lvl) | b <- new_bndrs] `zip` new_rhss)
             , ctxt_lvl, new_env)

decideBindFloat ::
  Level -> LevelEnv -> FVIs ->
  Bool -> -- are all RHSs bottoming?
  Bool -> -- are all RHSs functions?
  Bool -> -- isLNE
  Either (Var, FVIs) ([Var], [FVIs]) -> -- let or letrec, with RHSs' infos
  Maybe (Level, [Var]) -- Just (lvl, vs) <=> float to lvl with
                       -- abs_vars = vs, Nothing <=> do not float
decideBindFloat ctxt_lvl env body_fvis is_bot all_funs isLNE binding_fvis_s =
  maybe conventionalFloatOut lateLambdaLift (finalPass env)
  where
    conventionalFloatOut =
     if isProfitableFloat then Just (dest_lvl, abs_vars) else Nothing
      where
        dest_lvl = destLevel env is_bot bindings_fvs

        abs_vars = abstractVars dest_lvl env bindings_fvs

        isProfitableFloat =
             (dest_lvl `ltMajLvl` ctxt_lvl) -- Escapes a value lambda
          || isTopLvl dest_lvl -- Going all the way to top level

    lateLambdaLift fps
      | all_funs || (fps_floatLNE0 fps && isLNE), -- only late lift functions and zero-arity LNEs
        Nothing <- decider emptyVarEnv = Just (dest_lvl, abs_vars)
           -- TODO Just x <- decider emptyVarEnv -> do the retry stuff
      | otherwise = Nothing -- do not lift
      where
        dest_lvl = tOP_LEVEL

        decider = decideLateLambdaFloat env isRec isLNE abs_ids badTime spaceInfo ids extra_sdoc fps

        abs_ids  = filter isId abs_vars
        abs_vars = abstractVars dest_lvl env bindings_fvs

        badTime   = wouldIncreaseRuntime    env abs_ids bindings_fvis
        spaceInfo = wouldIncreaseAllocation env isLNE abs_ids rhs_nonTopLevelFreeIds_s scope_fvis

        -- for -ddump-late-float with -dppr-debug
        extra_sdoc = vcat [ text "bindings:" <+> ppr bindings_fvis'
                          , text "scope:" <+> ppr scope_fvis'] where
          bindings_fvis' = filter (\x -> fviIsNonTopLevel x && isId (fviVar x)) $ varEnvElts bindings_fvis
          scope_fvis'    = filter (isId . fviVar) $ varEnvElts $ scope_fvis `restrictVarEnv` mkVarSet ids

    bindings_fvs = prjFreeVars bindings_fvis

    (isRec, ids, rhss_fvis, scope_fvis, bindings_fvis, rhs_nonTopLevelFreeIds_s) = case binding_fvis_s of
      Left (bndr, rhs_fvis)     -> -- a non-recursive let
        ( False
        , [bndr]
        , rhs_fvis
        , body_fvis
        , rhss_fvis `bothFVIs` assumeTheBest (idFreeVars bndr)
        , [(bndr, prjFreeNonTopLevelIds rhs_fvis)]
        )
      Right (bndrs, rhs_fvis_s) -> -- a letrec
        ( True
        , bndrs
        , rhss_fvis
        , body_fvis `bothFVIs` rhss_fvis
        , delBindersFVIs bndrs rhss_fvis
        , zipWith (\bndr rhs_fvis -> (bndr, prjFreeNonTopLevelIds rhs_fvis))
            bndrs rhs_fvis_s
        )
        where rhss_fvis = computeRecRHSsFVIs bndrs rhs_fvis_s

decideLateLambdaFloat ::
  LevelEnv ->
  Bool ->
  Bool ->
  [Id] ->
  VarSet -> (VarSet -> [(Bool, WordOff, WordOff)]) ->
  [Id] -> SDoc ->
  FinalPassSwitches ->
  VarSet -> -- pinnees to ignore
  Maybe VarSet -- Nothing <=> float to tOP_LEVEL, Just x <=> do not
               -- float, not (null x) <=> forgetting fast calls to the
               -- ids in x are the only thing pinning this binding
decideLateLambdaFloat env isRec isLNE abs_ids badTime spaceInfo' ids extra_sdoc fps pinnees
  = (if fps_trace fps then pprTrace ('\n' : msg) msg_sdoc else (\x -> x)) $
       if isBadSpace then Just emptyVarSet
       else if isBadTime then Just badTime
       else Nothing

  where
    msg = (if spoilsLNEs || isBadTime || isBadSpace then "late-no-float" else "late-float")
          ++ if isRec then "(rec " ++ show (length ids) ++ ")"  else ""

    isBadTime = not (isEmptyVarSet badTime)

    spaceInfo = spaceInfo' pinnees

    spoilsLNEs | fps_absLNEVar fps = False -- allow abstraction over let-no-escape variables
               | otherwise = not $ null spoiledLNEs
    spoiledLNEs = filter (`elemVarSet` le_LNEs env) abs_ids

    isBadSpace = flip any spaceInfo $ \(createsPAPs, tg, tgil) ->
      papViolation createsPAPs ||
      tgViolation tg || tgilViolation tgil

    papViolation x | fps_createPAPs fps = False
                   | otherwise = x

    tgViolation = case fps_cloGrowth fps of
      Nothing -> const False
      Just limit -> (> limit * wORDS_PTR)

      -- If the closure is NOT under a lambda, then we get a discount
      -- for no longer allocating these bindings' closures, since
      -- these bindings would be allocated at least as many times as
      -- the closure.

       -- TODO                | Just limit <- fps_ifInClo fps =

    tgilViolation = case fps_cloGrowthInLam fps of
      Nothing -> const False
      Just limit -> (> limit * wORDS_PTR)

      -- If the closure is under a lambda, we do NOT discount for not
      -- allocating these bindings' closures, since the closure could
      -- be allocated many more times than these bindings are.

    msg_sdoc = vcat (zipWith space ids spaceInfo) where
      space v (badPAP, tg, tgil) = vcat
       [ ppr v <+> if isLNE then parens (text "LNE") else empty
       , text "abs_ids:" <+> ppr (length abs_ids)
       , text "pinnees:" <+> ppr (varSetElems pinnees)
       , text "createsPAPs:" <+> ppr badPAP
       , text "closureGrowth:" <+> ppr tg
       , text "CG in lam:"   <+> ppr tgil
       , text "fast-calls:" <+> ppr (varSetElems badTime)
       , text "spoiledLNEs:" <+> ppr spoiledLNEs
       , if opt_PprStyle_Debug then extra_sdoc else empty
       ]

    wORDS_PTR = StgCmmArgRep.argRepSizeW (le_dflags env) StgCmmArgRep.P

-- see Note [Preserving Fast Entries]
wouldIncreaseRuntime ::
  LevelEnv ->
  [Id] ->      -- the abstracted value ids
  FVIs ->      -- FVIs for the bindings' RHS and RULES
  VarSet       -- the forgotten ids
wouldIncreaseRuntime env abs_ids binding_group_fvis =
  case prjFlags `fmap` finalPass env of
  -- is final pass...
  Just (noUnder, noExact, noOver) | noUnder || noExact || noOver ->
    mkVarSet $ flip mapMaybe abs_ids $ \abs_id ->
      case lookupVarEnv binding_group_fvis abs_id of
        Just fvi | not (IntSet.null uses),
                   arity > 0, -- NB (arity > 0) iff "is known function"
                      (noUnder && arity > IntSet.findMin uses)
                   || (noExact && arity  `IntSet.member` uses)
                   || (noOver  && arity < IntSet.findMax uses)
                 -> Just abs_id
          where arity = idArity (fviVar fvi)
                  -- NB cannot use abs_id's arity! As a parameter, it's arity is 0.
                uses = fviUseInfo fvi
        _ -> Nothing
  _ -> emptyVarSet
  where prjFlags fps = ( not (fps_absUnsatVar   fps) -- -fno-late-abstract-undersat-var
                       , not (fps_absSatVar     fps) -- -fno-late-abstract-sat-var
                       , not (fps_absOversatVar fps) -- -fno-late-abstract-oversat-var
                       )

wouldIncreaseAllocation ::
  LevelEnv ->
  Bool ->
  [Id] ->         -- the abstracted value ids
  [(Id, [Id])] -> -- the binders in the binding group and their RHS's free IDs
  FVIs ->         -- wrt the binding's *entire scope*
  VarSet ->       -- pinnees: ignore these as captors
  [] -- for each binder:
    ( Bool -- would create PAPs
    , WordOff  -- estimated increases for closures that are NOT
               -- allocated under a lambda
    , WordOff  -- estimated increases for closures that ARE allocated
               -- under a lambda
    )
wouldIncreaseAllocation env isLNE abs_ids pairs usage pinnees = case finalPass env of
  Nothing -> []
  Just _fps -> flip map bndrs $ \bndr -> case lookupVarEnv usage bndr of
    Nothing -> (False, 0, 0) -- it's a dead variable
    Just fvi -> (violatesPAPs, closureGrowth - closuresSize, closureGrowthInLambda)
      where
        v_lvl = fviLevel fvi
        violatesPAPs | isLNE = False -- might be a zero-arity LNE
                     | otherwise =
          -- TODO consider refining this along the lines of closure
          -- growth
          --
          -- TODO also, if we specialized on partial applications (eg
          -- "map (f a) xs" becomes "$smap f a xs"), then maybe we
          -- could relax this check
          0 `IntSet.member` fviUseInfo fvi

        -- TODO -8 under one lambda and +1 under another is still bad!
        -- + is not safe for cgil
        --
        -- TODO: should an LNE's lambda't count as CG-in-lam?
        withFreebies :: Captors -> (VarSet -> WordOff) -> (WordOff, WordOff)
        withFreebies (Captors cs) k = IntMap.foldl on_snoc (0, 0) cs where
          on_snoc acc@(!cg, !cgil) cli
            | Just id <- cli_id cli, id `elemVarSet` pinnees = acc -- ignore the pinnees
            | otherwise = (cg + cgNew, cgil + cgilNew)
            where (cgNew, cgilNew)
               -- there's a lambda between this cli and v's binding
                    | cli_lvl cli > v_lvl = (0, new)
                    | otherwise       = (new, 0) -- there is no such lambda

                  new = k $ expandFloatedIds $ prjFreeVars $ cli_fvis cli
        withFreebies (BothCaptors l r) k = withFreebies l k `both` withFreebies r k
          where both (a, b) (x, y) = (a + x, b + y)
        withFreebies (AltCaptors l r) k = withFreebies l k `alt` withFreebies r k
          where alt (a, b) (x, y) = (a `max` x, b `max` y)

        estimateGrowth :: VarSet -> WordOff
        estimateGrowth freebies = harm - benefit
          where harm = idWords $ filter (not . flip elemVarSet freebies) abs_ids
                  -- estimate for the increase in allocation
                  --
                  -- NB freebies prevents us from penalizing for the
                  -- abs_ids that the closure already captures

                benefit = wORDS_PTR * count (flip elemVarSet freebies) bndrs
                  -- decrease in allocation due to floating the bindings

        (closureGrowth, closureGrowthInLambda)
          = withFreebies (fviCaptors fvi) estimateGrowth
    where
      -- if a freebie was floated, then its abs_vars are now freebies
      expandFloatedIds = unionVarSets . map (absVarsOf (le_env env)) . varSetElems

      (bndrs, fidss) = unzip pairs

      dflags = le_dflags env
      wORDS_PTR = StgCmmArgRep.argRepSizeW dflags StgCmmArgRep.P

      -- TODO this is not including the header size?
      closuresSize = (\x -> if opt_PprStyle_Debug then pprTrace "\nclosuresSize" (ppr x) x else x) $
                     sum $ flip map fidss $ \fids ->
        let (words, _, _) =
              StgCmmLayout.mkVirtHeapOffsets dflags isUpdateable $
              StgCmmClosure.addIdReps $ filter (`elem` abs_ids) fids
              where isUpdateable = False -- functions are not updateable
        in words

      -- the number of words used to represent these Ids in a
      -- closure
      idWords :: [Id] -> WordOff
      idWords ids = sum $ map (StgCmmArgRep.argRepSizeW dflags . toArgRep . StgCmmClosure.idPrimRep) ids

{- TODO stuff for the retrying the lambda float

  | Just{} <- floatDecision emptyVarSet, not (lateRetry env) = doNotFloat
  | Just pinners <- floatDecision emptyVarSet =
      case isEmptyVarEnv pinners of
        False -> do -- merely pinned
          tellLvlM $ unitVarEnv bndr (bndr, pinners)
          doNotFloat
        True -> do -- not floating for space reasons
          (result, pinnees) <- hijackLvlM doNotFloat
          let (roots, pinnees') = partitionPinnees [bndr] pinnees
          tellLvlM pinnees'
          case isEmptyVarSet $ roots `delVarSet` bndr of
            True -> return result -- the space reasons are valid
            False -> case floatDecision roots of
              Nothing -> doFloat -- a successful unpinning: the space
                                 -- reasons were invalid
              Just pinners -> do
                -- if space is no longer the reason, announce that we're pinned
                when (not $ isEmptyVarSet pinners) $ tellLvlM $ unitVarEnv bndr (bndr, pinners)
                return result

-- partition the pinnees by whether or not they are ultimately (ie
-- transitively) pinned by nothing but these binders
partitionPinnees :: [Id] -> PinnedLBFs -> (VarSet, PinnedLBFs)
partitionPinnees bndrs pinnees = go $ PartitionState False (mkVarSet bndrs) pinnees where
  go st
    | ps_stop st = (ps_roots st, ps_nonroots st) -- no new roots
    | otherwise = go $ foldVarEnv isARoot (st { ps_stop = True, ps_nonroots = emptyVarEnv}) (ps_nonroots st)

data PartitionState = PartitionState {ps_stop :: !Bool, ps_roots :: VarSet, ps_nonroots :: PinnedLBFs }

isARoot :: (Id, VarSet) -> PartitionState -> PartitionState
isARoot p@(id, pinners) !st@PartitionState { ps_roots = roots }
  -- if id is pinned only by roots, it's also a root
  | isEmptyVarEnv (pinners `minusVarSet` roots) = st { ps_stop = False, ps_roots = extendVarSet roots id }
  | otherwise = st { ps_nonroots = extendVarEnv (ps_nonroots st) id p }

-}
----------------------------------------------------
-- Three help functions for the type-abstraction case

lvlFloatRhs :: [CoreBndr] -> Level -> LevelEnv -> CoreExprWithFVIs
            -> LvlM (Expr LevelledBndr)
lvlFloatRhs abs_vars dest_lvl env rhs = do
    rhs' <- lvlExpr rhs_lvl rhs_env rhs
    return (mkLams abs_vars_w_lvls rhs')
  where
    (rhs_lvl, abs_vars_w_lvls) = lvlLamBndrs dest_lvl abs_vars
    rhs_env = extendLvlEnv env abs_vars_w_lvls
\end{code}


%************************************************************************
%*									*
\subsection{Deciding floatability}
%*									*
p%************************************************************************

\begin{code}
lvlLamBndrs :: Level -> [CoreBndr] -> (Level, [LevelledBndr])
-- Compute the levels for the binders of a lambda group
-- The binders returned are exactly the same as the ones passed,
-- but they are now paired with a level
lvlLamBndrs lvl [] 
  = (lvl, [])

lvlLamBndrs lvl bndrs
  = (new_lvl, [TB bndr (StayPut new_lvl) | bndr <- bndrs])
  -- All the new binders get the same level, because
  -- any floating binding is either going to float past 
  -- all or none.  We never separate binders
  where
    new_lvl | any is_major bndrs = incMajorLvl lvl
            | otherwise          = incMinorLvl lvl

    is_major bndr = isId bndr && not (isOneShotLambda bndr)
\end{code}

\begin{code}
-- Destination level is the max Id level of the expression (We'll
-- abstract the type variables, if any.)
destLevel :: LevelEnv -> Bool -> VarSet -> Level
destLevel env is_bot fvs
  | is_bot = tOP_LEVEL -- see Note [Bottoming floats]
  | otherwise = maxFvLevel isId env fvs
  -- Max over Ids only; the tyvars will be abstracted

class HasVar b where
  getVar :: b -> Var
instance HasVar Var where getVar = id
instance HasVar (TaggedBndr tag) where getVar (TB id _) = getVar id

isFunctionAnn :: HasVar b => AnnExpr b annot -> Bool
isFunctionAnn = isFunction . deAnnotate

isFunction :: HasVar b => Expr b -> Bool
-- The idea here is that we want to float *functions* to
-- the top level.  This saves no work, but 
--	(a) it can make the host function body a lot smaller, 
--		and hence inlinable.  
--	(b) it can also save allocation when the function is recursive:
--	    h = \x -> letrec f = \y -> ...f...y...x...
--		      in f x
--     becomes
--	    f = \x y -> ...(f x)...y...x...
--	    h = \x -> f x x
--     No allocation for f now.
-- We may only want to do this if there are sufficiently few free 
-- variables.  We certainly only want to do it for values, and not for
-- constructors.  So the simple thing is just to look for lambdas
isFunction (Lam b e) | isId (getVar b)    = True
                     | otherwise = isFunction e
-- isFunction (_, AnnTick _ e)          = isFunction e  -- dubious
isFunction _                           = False

{-countFreeIds :: VarSet -> Int
countFreeIds = foldVarSet add 0
  where
    add :: Var -> Int -> Int
    add v n | isId v    = n+1
            | otherwise = n-}
\end{code}


%************************************************************************
%*									*
\subsection{Free-To-Level Monad}
%*									*
%************************************************************************

\begin{code}
data LevelEnv 
  = LE { le_switches :: FloatOutSwitches
       , le_lvl_env  :: VarEnv Level	-- Domain is *post-cloned* TyVars and Ids
       , le_subst    :: Subst 		-- Domain is pre-cloned Ids; tracks the in-scope set
					-- 	so that substitution is capture-avoiding
                                        -- The Id -> CoreExpr in the Subst is ignored
                                        -- (since we want to substitute in LevelledExpr
                                        -- instead) but we do use the Co/TyVar substs
       , le_env      :: IdEnv ([Var], LevelledExpr)	-- Domain is pre-cloned Ids
       , le_dflags   :: DynFlags
       , le_LNEs     :: VarSet
    }
        -- see Note [The Reason SetLevels Does Substitution]

        -- We clone let-bound variables so that they are still
	-- distinct when floated out; hence the le_subst/le_env.
        -- (see point 3 of the module overview comment).
	-- We also use these envs when making a variable polymorphic
	-- because we want to float it out past a big lambda.
	--
	-- The le_subst and le_env always implement the same mapping, but the
	-- le_subst maps to CoreExpr and the le_env to LevelledExpr
	-- Since the range is always a variable or type application,
	-- there is never any difference between the two, but sadly
	-- the types differ.  The le_subst is used when substituting in
	-- a variable's IdInfo; the le_env when we find a Var.
	--
	-- In addition the le_env records a list of tyvars free in the
	-- type application, just so we don't have to call freeVars on
	-- the type application repeatedly.
	--
	-- The domain of the both envs is *pre-cloned* Ids, though
	--
	-- The domain of the le_lvl_env is the *post-cloned* Ids

initialEnv :: DynFlags -> FloatOutSwitches -> LevelEnv
initialEnv dflags float_lams 
  = LE { le_switches = float_lams, le_lvl_env = emptyVarEnv
       , le_subst = emptySubst, le_env = emptyVarEnv, le_dflags = dflags, le_LNEs = emptyVarSet }

--floatLams :: LevelEnv -> Maybe Int
--floatLams le = floatOutLambdas (le_switches le)

finalPass :: LevelEnv -> Maybe FinalPassSwitches
finalPass le = finalPass_ (le_switches le)

isFinalPass :: LevelEnv -> Bool
isFinalPass le = case finalPass le of
  Nothing -> False
  Just _  -> True

doSinglyRecSAT :: LevelEnv -> Bool
doSinglyRecSAT le = case finalPass le of
  Nothing  -> False
  Just fps -> fps_doSinglyRecSAT fps

lateRetry :: LevelEnv -> Bool
lateRetry le = case finalPass le of
  Nothing  -> False
  Just fps -> fps_retry fps

floatConsts :: LevelEnv -> Bool
floatConsts le = floatOutConstants (le_switches le)

floatPAPs :: LevelEnv -> Bool
floatPAPs le = floatOutPartialApplications (le_switches le)

lneLvlEnv :: LevelEnv -> [Id] -> LevelEnv
lneLvlEnv env lnes = env { le_LNEs = extendVarSetList (le_LNEs env) lnes }

-- see Note [The Reason SetLevels Does Substitution]
extendLvlEnv :: LevelEnv -> [LevelledBndr] -> LevelEnv
-- Used when *not* cloning
extendLvlEnv le@(LE { le_lvl_env = lvl_env, le_subst = subst, le_env = id_env }) 
             prs
  = le { le_lvl_env = foldl add_lvl lvl_env prs
       , le_subst   = foldl del_subst subst prs
       , le_env     = foldl del_id id_env prs }
  where
    add_lvl   env (TB v s) = extendVarEnv env v (floatSpecLevel s)
    del_subst env (TB v _) = extendInScope env v
    del_id    env (TB v _) = delVarEnv env v
  -- We must remove any clone for this variable name in case of
  -- shadowing.  This bit me in the following case
  -- (in nofib/real/gg/Spark.hs):
  -- 
  --   case ds of wild {
  --     ... -> case e of wild {
  --              ... -> ... wild ...
  --            }
  --   }
  -- 
  -- The inside occurrence of @wild@ was being replaced with @ds@,
  -- incorrectly, because the SubstEnv was still lying around.  Ouch!
  -- KSW 2000-07.

-- extendCaseBndrLvlEnv adds the mapping case-bndr->scrut-var if it can
-- (see point 4 of the module overview comment)
extendCaseBndrLvlEnv :: LevelEnv -> Expr LevelledBndr
                     -> LevelledBndr -> LevelEnv
extendCaseBndrLvlEnv le@(LE { le_subst = subst, le_env = id_env }) 
                     (Var scrut_var) (TB case_bndr _)
  = le { le_subst   = extendSubstWithVar subst case_bndr scrut_var
       , le_env     = extendVarEnv id_env case_bndr ([scrut_var], ASSERT(not (isCoVar scrut_var)) Var scrut_var) }
     
extendCaseBndrLvlEnv env _scrut case_bndr
  = extendLvlEnv env [case_bndr]

extendPolyLvlEnv :: Level -> LevelEnv -> [Var] -> [(Var {- :: t -}, Var {- :: mkPiTypes abs_vars t -})] -> LevelEnv
extendPolyLvlEnv dest_lvl 
                 le@(LE { le_lvl_env = lvl_env, le_subst = subst, le_env = id_env }) 
                 abs_vars bndr_pairs
   = ASSERT( all (not . isCoVar . fst) bndr_pairs ) -- What would we add to the CoSubst in this case. No easy answer, so avoid floating 
    le { le_lvl_env = foldl add_lvl   lvl_env bndr_pairs
       , le_subst   = foldl add_subst subst   bndr_pairs
       , le_env     = foldl add_id    id_env  bndr_pairs }
  where
     add_lvl   env (_, v') = extendVarEnv env v' dest_lvl
     add_subst env (v, v') = extendIdSubst env v (mkVarApps (Var v') abs_vars)
     add_id    env (v, v') = extendVarEnv env v ((v':abs_vars), mkVarApps (Var v') abs_vars)

extendCloneLvlEnv :: Level -> LevelEnv -> Subst -> [(Var, Var)] -> LevelEnv
extendCloneLvlEnv lvl le@(LE { le_lvl_env = lvl_env, le_env = id_env }) 
                  new_subst bndr_pairs
  = le { le_lvl_env = foldl add_lvl lvl_env bndr_pairs
       , le_subst   = new_subst
       , le_env     = foldl add_id  id_env  bndr_pairs }
  where
     add_lvl env (_, v_cloned) = extendVarEnv env v_cloned lvl
     add_id  env (v, v_cloned) = if isTyVar v
                                 then delVarEnv    env v
                                 else extendVarEnv env v ([v_cloned], ASSERT(not (isCoVar v_cloned)) Var v_cloned)

maxFvLevel :: (Var -> Bool) -> LevelEnv -> VarSet -> Level
maxFvLevel max_me (LE { le_lvl_env = lvl_env, le_env = id_env }) var_set
  = foldVarSet max_in tOP_LEVEL var_set
  where
    max_in in_var lvl 
       = foldr max_out lvl (case lookupVarEnv id_env in_var of
				Just (abs_vars, _) -> abs_vars
				Nothing		   -> [in_var])

    max_out out_var lvl 
	| max_me out_var = case lookupVarEnv lvl_env out_var of
				Just lvl' -> maxLvl lvl' lvl
				Nothing   -> lvl 
	| otherwise = lvl	-- Ignore some vars depending on max_me

lookupVar :: LevelEnv -> Id -> LevelledExpr
lookupVar le v = case lookupVarEnv (le_env le) v of
		    Just (_, expr) -> expr
		    _              -> Var v

abstractVars :: Level -> LevelEnv -> VarSet -> [Var]
	-- Find the variables in fvs, free vars of the target expresion,
	-- whose level is greater than the destination level
	-- These are the ones we are going to abstract out
abstractVars dest_lvl (LE { le_lvl_env = lvl_env, le_env = id_env }) fvs
  = map zap $ uniq $ sortQuantVars
	[var | fv <- varSetElems fvs
	     , var <- varSetElems (absVarsOf id_env fv)
	     , abstract_me var ]
	-- NB: it's important to call abstract_me only on the OutIds the
	-- come from absVarsOf (not on fv, which is an InId)
  where
    uniq :: [Var] -> [Var]
	-- Remove adjacent duplicates; the sort will have brought them together
    uniq (v1:v2:vs) | v1 == v2  = uniq (v2:vs)
		    | otherwise = v1 : uniq (v2:vs)
    uniq vs = vs

    abstract_me v = case lookupVarEnv lvl_env v of
			Just lvl -> dest_lvl `ltLvl` lvl
			Nothing  -> False

	-- We are going to lambda-abstract, so nuke any IdInfo,
	-- and add the tyvars of the Id (if necessary)
    zap v | isId v = WARN( isStableUnfolding (idUnfolding v) ||
		           not (isEmptySpecInfo (idSpecialisation v)),
		           text "absVarsOf: discarding info on" <+> ppr v )
		     setIdInfo v vanillaIdInfo
	  | otherwise = v

absVarsOf :: IdEnv ([Var], LevelledExpr) -> Var -> VarSet
	-- If f is free in the expression, and f maps to poly_f a b c in the
	-- current substitution, then we must report a b c as candidate type
	-- variables
	--
	-- Also, if x::a is an abstracted variable, then so is a; that is,
	-- we must look in x's type. What's more, if a mentions kind variables,
	-- we must also return those.
absVarsOf id_env v 
  | isId v, Just (abs_vars, _) <- lookupVarEnv id_env v
  = foldr (unionVarSet . close) emptyVarSet abs_vars
  | otherwise
  = close v
  where
    close :: Var -> VarSet  -- Result include the input variable itself
    close v = foldVarSet (unionVarSet . close)
                         (unitVarSet v)
                         (varTypeTyVars v)
\end{code}

\begin{code}
type PinnedLBFs = VarEnv (Id, VarSet) -- (g, fs, hs) <=> pinned by fs, captured by hs

newtype LvlM a = LvlM (UniqSM (a, PinnedLBFs))
instance Monad LvlM where
  return a = LvlM $ return (a, emptyVarEnv)
  LvlM m >>= k = LvlM $ m >>= \ ~(a, w) ->
    case k a of
      LvlM m -> m >>= \ ~(b, w') -> return (b, plusVarEnv_C (\ ~(id, x) ~(_, y) -> (id, unionVarSet x y)) w w')
instance MonadUnique LvlM where
  getUniqueSupplyM = LvlM $ getUniqueSupplyM >>= \a -> return (a, emptyVarEnv)

tellLvlM :: PinnedLBFs -> LvlM ()
tellLvlM pinned = LvlM $ return ((), pinned)

hijackLvlM :: LvlM a -> LvlM (a, PinnedLBFs)
hijackLvlM (LvlM m) = LvlM $ m >>= \p -> return (p, emptyVarEnv)

initLvl :: UniqSupply -> LvlM a -> a
initLvl us (LvlM m) = fst $ initUs_ us m
\end{code}


\begin{code}
newPolyBndrs :: Level -> LevelEnv -> [Var] -> [Id] -> LvlM (LevelEnv, [Id])
newPolyBndrs dest_lvl env abs_vars bndrs = do
    uniqs <- getUniquesM
    let new_bndrs = zipWith mk_poly_bndr bndrs uniqs
    return (extendPolyLvlEnv dest_lvl env abs_vars (bndrs `zip` new_bndrs), new_bndrs)
  where
    mk_poly_bndr bndr uniq = transferPolyIdInfo bndr abs_vars $ 	-- Note [transferPolyIdInfo] in Id.lhs
			     mkSysLocal (mkFastString str) uniq poly_ty
			   where
			     str     = (if isFinalPass env then "llf_" else "poly_")
                                        ++ occNameString (getOccName bndr)
			     poly_ty = mkPiTypes abs_vars (idType bndr)

newLvlVar :: [CoreBndr] -> Type 	-- Abstract wrt these bndrs
	  -> Maybe (Arity, StrictSig)   -- Note [Bottoming floats]
	  -> LvlM Id
newLvlVar vars body_ty mb_bot
  = do { uniq <- getUniqueM
       ; return (mkLocalIdWithInfo (mk_name uniq) (mkPiTypes vars body_ty) info) }
  where
    mk_name uniq = mkSystemVarName uniq (mkFastString "lvl")
    arity = count isId vars
    info = case mb_bot of
		Nothing               -> vanillaIdInfo
		Just (bot_arity, sig) -> 
                     vanillaIdInfo 
		    `setArityInfo`      (arity + bot_arity)
		    `setStrictnessInfo` (increaseStrictSigArity arity sig)
    
-- The deeply tiresome thing is that we have to apply the substitution
-- to the rules inside each Id.  Grr.  But it matters.

substLetBndrNonRec :: LevelEnv -> Id -> Level -> (LevelEnv, Id)
substLetBndrNonRec 
    le@(LE { le_lvl_env = lvl_env, le_subst = subst, le_env = id_env }) 
    bndr bind_lvl
  = ASSERT( isId bndr )
    (env', bndr' )
  where
    (subst', bndr') = substBndr subst bndr
    env'	    = le { le_lvl_env = extendVarEnv lvl_env bndr bind_lvl
                         , le_subst = subst'
                         , le_env = delVarEnv id_env bndr }

substLetBndrsRec :: LevelEnv -> [Id] -> Level -> (LevelEnv, [Id])
substLetBndrsRec 
    le@(LE { le_lvl_env = lvl_env, le_subst = subst, le_env = id_env }) 
    bndrs bind_lvl
  = ASSERT( all isId bndrs )
    (env', bndrs')
  where
    (subst', bndrs') = substRecBndrs subst bndrs
    env'	     = le { le_lvl_env = extendVarEnvList lvl_env [(b,bind_lvl) | b <- bndrs]
                          , le_subst = subst'
                          , le_env = delVarEnvList id_env bndrs }

cloneVar :: LevelEnv -> Var -> Level -> LvlM (LevelEnv, Var)
cloneVar env v dest_lvl -- Works for Ids, TyVars and CoVars
  = do { u <- getUniqueM
       ; let (subst', v1) = cloneBndr (le_subst env) u v
      	     v2     	  = if isId v1 
                            then zapDemandIdInfo v1  
                            else v1
      	     env'	  = extendCloneLvlEnv dest_lvl env subst' [(v,v2)]
       ; return (env', v2) }

cloneVars :: LevelEnv -> [Var] -> Level -> LvlM (LevelEnv, [Var])
cloneVars env vs dest_lvl = mapAccumLM (\env v -> cloneVar env v dest_lvl) env vs

cloneRecVars :: LevelEnv -> [Id] -> Level -> LvlM (LevelEnv, [Id])
cloneRecVars env vs dest_lvl -- Works for CoVars too (since cloneRecIdBndrs does)
  = ASSERT( all isId vs ) do
    us <- getUniqueSupplyM
    let
      (subst', vs1) = cloneRecIdBndrs (le_subst env) us vs
      -- Note [Zapping the demand info]
      vs2	    = map zapDemandIdInfo vs1  
      env'	    = extendCloneLvlEnv dest_lvl env subst' (vs `zip` vs2)
    return (env', vs2)
\end{code}

Note [Preserving Fast Entries] (wrt Note [Late Lambda Floating])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The policy: avoid changing fast entry invocations of free variables
(known call) into slow entry invocations of the new parameter
representing that free variable (unknown call).

  ... let f x = ... in
      let g x = ... (f ...) ... in  -- GOOD: call to f is fast entry
      ... g a ...

  => -- NB f wasn't floated

  poly_g f x = ... (f ...) ... -- BAD: call to f is slow entry

  ... let f x = ... in
      ... poly_g f a ...

The mechanism: when considering a let-bound lambda, we disallow the
float if any of the variables being abstracted over are applied in the
RHS. The flags -f(no)-late-abstract-undersat-var and
-f(no)-late-abstract-sat-var determine the details of this check.

It is intended that only applications of locally-bound free variables
*whose bindings are not themselves floated* can prevent a float. This
comes for free. The free variable information is not updated during
the setLevels pass. On the other hand, the set of abstracted variables
is calculated using the current LevelEnv. Thus: while a floated
function's original Id may be in the FVInfo, it won't be in the
abs_vars.

Note [Zapping the demand info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VERY IMPORTANT: we must zap the demand info if the thing is going to
float out, becuause it may be less demanded than at its original
binding site.  Eg
   f :: Int -> Int
   f x = let v = 3*4 in v+x
Here v is strict; but if we float v to top level, it isn't any more.

Note [The Reason SetLevels Does Substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a binding is going to be floated, setLevels carries a substitution
in order to eagerly replace that binding's occurrences with a
reference to the floated binding. Why doesn't it instead create a
simple binding right next it and rely on the wise and weary simplifier
to handle the inlining? It's an issue with nested bindings.

  outer a = let x = ... a ... in
            let y = ... x ... in
            ... x ... y ...

Currently, when setLevels processes the x binding, the substitution
leads to the following intermediate step. (I am showing the result of
the substitution as if it were already applied.)

  x' a = ...

  out a = let y = ... x' a ... in
          ... x' a ... y ...

If we were to instead rely on the simplifier, we'd do something like this

  x' a = ...

  out a = let x = x' a in
          let y = ... x ... in
          ... x ... y ...

The problem here is that the subsequent step in which setLevels
analyzes the y binding would still treat x as y's only free
variable. With the eager substitution, on the other hand, x' is not
treated as a free variable since it's a global and a *is* recognized
as a free variable. That's the behavior we currently intend.

%************************************************************************
%*									*
\subsection{Determining unapplied variables}
%*									*
%************************************************************************


\begin{code}
type CoreBindWithFVIs = AnnBind (TaggedBndr Bool) FVIs
type CoreExprWithFVIs = AnnExpr (TaggedBndr Bool) FVIs

fvisOf :: CoreExprWithFVIs -> FVIs
fvisOf = fst

-- | Non-global free variables and their information
type FVIs = VarEnv FVInfo

prjFreeVars :: FVIs -> VarSet
prjFreeVars = mapVarEnv fviVar

prjFreeNonTopLevelIds :: FVIs -> [Id]
prjFreeNonTopLevelIds = mapMaybe each . varEnvElts where
  each fvi | isId v && fviIsNonTopLevel fvi = Just v
           | otherwise = Nothing
    where v = fviVar fvi

data FVInfo = FVInfo {fvi_var :: !Var, fvi_topLevel :: !Bool, fvi_info :: LBF}
  -- cannot be strict in fvi_info because of knot in analyzeFVsM

data LBF -- we need to record more information about let-bound functions
 = NotLBF
 | LBF { fvi_useInfo     :: !UseInfo
       -- ^ n `elem` useInfo ==> the variable occurs at least once
       -- applied to n-many arguments
       , fvi_captors    :: Captors
       -- ^ the closures this variable occurs in
       , fvi_lvl         :: MajorLevel -- of its declaration
       }

data ClosureInfo = ClosureInfo
  { cli_key      :: !Int -- unique marker
  , cli_id       :: Maybe Id -- Nothing <=> argument closure
  , cli_fvis     :: FVIs
  , cli_lvl      :: MajorLevel
  }

instance Outputable ClosureInfo where
  ppr (ClosureInfo key id fvis lvl)
    | opt_PprStyle_Debug = base <+> text "lvl:" <+> ppr lvl $$
       text "fvs:" <+> ppr (varSetElems $ prjFreeVars fvis)
    | otherwise = base
    where base = maybe (text "<arg>") ppr id <+> parens (ppr key)

data Captors = Captors (IntMap ClosureInfo)
                  -- key: closure id    (ie cli_key)
             | AltCaptors Captors Captors
             | BothCaptors Captors Captors

bothCaptors :: Captors -> Captors -> Captors
bothCaptors (Captors l) r | IntMap.null l = r
bothCaptors l (Captors r) | IntMap.null r = l
bothCaptors (Captors x) (Captors y) = Captors $ IntMap.union x y
bothCaptors x           y           = BothCaptors x y

altCaptors :: Captors -> Captors -> Captors
altCaptors (Captors l) r | IntMap.null l = r
altCaptors l (Captors r) | IntMap.null r = l
altCaptors l r = AltCaptors l r

insertCaptor :: ClosureInfo -> Captors -> Captors
insertCaptor cli cs = Captors (IntMap.singleton (cli_key cli) cli) `bothCaptors` cs

fviIsNonTopLevel :: FVInfo -> Bool
fviIsNonTopLevel = not . fvi_topLevel

fviVar :: FVInfo -> Var
fviVar = fvi_var

fviUseInfo :: FVInfo -> UseInfo
fviUseInfo = w . fvi_info where
  w LBF { fvi_useInfo = x } = x
  w NotLBF = zeroUseInfo

fviCaptors :: FVInfo -> Captors
fviCaptors = w . fvi_info where
  w LBF { fvi_captors = x } = x
  w NotLBF = zeroCaptors

fviLevel :: FVInfo -> MajorLevel
fviLevel = w . fvi_info where
  w LBF { fvi_lvl = x } = x
  w NotLBF = 0

instance Outputable FVInfo where
  ppr (FVInfo v tl info) = ppr v <> (if tl then text " (top)" else empty) <> ppr info
instance Outputable LBF where
  ppr NotLBF = empty
  ppr (LBF ui cs lvl) = text ":" <+> vcat
    [ text "lvl:" <+> ppr lvl
    , text "usage:" <+> ppr (IntSet.toList ui)
    , text "captors:" <+> ppr cs
    ]

-- TODO As of 14 Feb 2013, I'm currently only ever using the intset
-- elements by comparing them to statically known values (ie 0 and the
-- binder's arity), so we could do that comparison in the analysis and
-- just carry some booleans instead of the IntSet. But the IntSet is
-- nice because its union and ppr is free.
type UseInfo = IntSet -- for x in in this set, the variable might be
                      -- called at least once with x many arguments

zeroUseInfo :: UseInfo
zeroUseInfo = IntSet.empty

bothUseInfo :: UseInfo -> UseInfo -> UseInfo
bothUseInfo = IntSet.union

altUseInfo :: UseInfo -> UseInfo -> UseInfo
altUseInfo = bothUseInfo

zeroCaptors :: Captors
zeroCaptors = Captors IntMap.empty

instance Outputable Captors where
  ppr (BothCaptors l r) = parens $ vcat [ppr l, text "BOTH", ppr r]
  ppr (AltCaptors l r) = parens $ vcat [ppr l, text "ALT", ppr r]
  ppr (Captors m) = vcat $ map tweak $ IntMap.elems m
    where tweak cli | opt_PprStyle_Debug = ppr cli
                    | otherwise          = ppr $ cli_id cli



{-
-- NB I'm only ever using the mm_max capability, as of 4 Feb 2013 -NSF
data MinMax = MinMax {mm_min :: !Int, mm_max :: !Int}

instance Outputable MinMax where
  ppr (MinMax n x) = text "[" <> ppr n <> text "," <> ppr x <> text "]"


unitMinMax :: Int -> MinMax
unitMinMax i = MinMax i i

zeroMinMax :: MinMax
zeroMinMax = unitMinMax 0

oneMinMax :: MinMax
oneMinMax = unitMinMax 1

plusMinMax :: MinMax -> MinMax -> MinMax
plusMinMax (MinMax nL xL) (MinMax nR xR)
  = MinMax (nL + nR) (xL + xR)

mergeMinMax :: MinMax -> MinMax -> MinMax
mergeMinMax (MinMax nL xL) (MinMax nR xR)
  = MinMax (min nL nR) (max xL xR)
-}



bothFVI :: FVInfo -> FVInfo -> FVInfo
bothFVI (FVInfo v tl l) (FVInfo _ _ r) = FVInfo v tl $ w l r where
  w l NotLBF = l
  w NotLBF r = r
  w l r = LBF
    { fvi_useInfo    = fvi_useInfo    l `bothUseInfo`  fvi_useInfo    r
    , fvi_captors    = fvi_captors    l `bothCaptors`  fvi_captors    r
    , fvi_lvl        = fvi_lvl l
    }

altFVI :: FVInfo -> FVInfo -> FVInfo
altFVI (FVInfo v tl l) (FVInfo _ _ r) = FVInfo v tl $ w l r where
  w l NotLBF = l
  w NotLBF r = r
  w l r = LBF
    { fvi_useInfo    = fvi_useInfo    l `altUseInfo` fvi_useInfo    r
    , fvi_captors    = fvi_captors    l `altCaptors` fvi_captors    r
    , fvi_lvl        = fvi_lvl l
    }



noneFVIs :: FVIs
noneFVIs = emptyVarEnv

assumeTheBest :: VarSet -> FVIs
assumeTheBest s = zipVarEnv vs $ flip map vs $ \v -> FVInfo v False NotLBF
  where vs = varSetElems s

nonValueFVs :: VarSet -> FVIs
nonValueFVs = assumeTheBest -- only value vars matter

bothFVIs :: FVIs -> FVIs -> FVIs
bothFVIs = plusVarEnv_C bothFVI

altFVIs :: FVIs -> FVIs -> FVIs
altFVIs = plusVarEnv_C altFVI

delBindersFVIs :: [Var] -> FVIs -> FVIs
delBindersFVIs bs fvs = foldr delBinderFVIs fvs bs

delBinderFVIs :: Var -> FVIs -> FVIs
-- see comment on CoreFVs.delBinderFV
delBinderFVIs b fvis =
  fvis `delVarEnv` b
  `bothFVIs` nonValueFVs (varTypeTyVars b)

\end{code}


%************************************************************************
%*                                                                      *
\subsection{Free variables (and types) and unapplied variables}
%*                                                                      *
%************************************************************************

\begin{code}
data PFVM a = PFVM a !Int -- cannot be strict on a b/c of mfix
newtype FVM a = FVM {runFVM :: Int -> PFVM a}
instance Monad FVM where
  return a = FVM $ \i -> PFVM a i
  FVM f >>= k = FVM $ \i -> case f i of
    PFVM a i -> runFVM (k a) i
instance MonadFix FVM where
  mfix f = FVM $ \i -> let p = runFVM (f $ (\(PFVM a _) -> a) p) i
                       in p

-- TODO: drop all the mfix crap you took 30 minutes to get right since
-- AN LNE CANNOT POSSIBLY BE CAPTURED BY A CLOSURE, you idiot Nick

evalFVM :: FVM a -> Int -> a
evalFVM (FVM f) = (\(PFVM a _) -> a) . f

gensymFVM :: FVM Int
gensymFVM = FVM $ \i -> PFVM i (i + 1)

data FVEnv = FVEnv
  { fve_isFinal      :: !Bool
  , fve_useDmd       :: !Bool
  , fve_ignoreLNEClo :: !Bool
  , fve_floatLNE0    :: !Bool
  , fve_argumentDemands :: Maybe [Bool]
  , fve_runtimeArgs  :: !NumRuntimeArgs
  , fve_letBoundLvls :: !LetBoundFunLvls
  , fve_majorLevel   :: !MajorLevel
  -- ^ number of enclosing lambdas
  , fve_nonTopLevel  :: !VarSet
  -- ^ the non-TopLevel variables in scope
  }

type NumRuntimeArgs = Int -- i <=> applied to i runtime arguments
type LetBoundFunLvls = VarEnv (MajorLevel, Bool)
  -- (k, (lvl, b)): k is let-bound (ie nested), bound under lvl-many
  -- lambdas, b <=> k is a function

initFVEnv :: Maybe FinalPassSwitches -> FVEnv
initFVEnv mb_fps = FVEnv {
  fve_isFinal = isFinal,
  fve_useDmd = useDmd,
  fve_ignoreLNEClo = ignoreLNEClo,
  fve_floatLNE0 = floatLNE0,
  fve_argumentDemands = Nothing,
  fve_runtimeArgs = 0,
  fve_letBoundLvls = emptyVarEnv,
  fve_majorLevel = 0,
  fve_nonTopLevel = emptyVarSet
  }
  where (isFinal, useDmd, ignoreLNEClo, floatLNE0) = case mb_fps of
          Nothing -> (False, False, False, False)
          Just fps -> (True, fps_strictness fps, fps_ignoreLNEClo fps, fps_floatLNE0 fps)

unappliedEnv :: FVEnv -> FVEnv
unappliedEnv env = env { fve_runtimeArgs = 0, fve_argumentDemands = Nothing }

appliedEnv :: FVEnv -> FVEnv
appliedEnv env =
  env { fve_runtimeArgs = 1 + fve_runtimeArgs env }

letBoundEnv :: Id -> Bool -> CoreExpr -> FVEnv -> FVEnv
letBoundEnv bndr isLNE rhs env =
   -- for the info that needs fve_letBoundLvls, we are only interested
   -- in let-bound functions (or zero-arity LNEs)
   env { fve_letBoundLvls = extendVarEnv_C (\_ new -> new)
           (fve_letBoundLvls env)
           bndr
           (fve_majorLevel env, isFunction rhs || (fve_floatLNE0 env && isLNE)) }

letBoundsEnv :: [(Id, CoreExpr)] -> Bool -> FVEnv -> FVEnv
letBoundsEnv binds isLNE env = foldl (\e (id, rhs) -> letBoundEnv id isLNE rhs e) env binds

closureBindingFVIs :: Int -> FVEnv -> Maybe Id -> FVIs -> FVIs
closureBindingFVIs key env mb_id rhs_fvis =
  let cli = ClosureInfo { cli_key = key, cli_id = mb_id, cli_fvis = fvis', cli_lvl = fve_majorLevel env }
      fvis' = flip mapVarEnv rhs_fvis $ \(FVInfo v tl lbf) -> FVInfo v tl $ case lbf of
        LBF use cs lvl -> (\cs -> LBF use cs lvl) $
          if Just v == mb_id
          then cs -- a closure does not capture itself
          else insertCaptor cli cs
        o -> o
  in fvis'

lambdaEnv :: FVEnv -> FVEnv
lambdaEnv env = env { fve_majorLevel = 1 + fve_majorLevel env }

extendEnv :: [Id] -> FVEnv -> FVEnv
extendEnv bndrs env =
  env { fve_nonTopLevel = extendVarSetList (fve_nonTopLevel env) bndrs }

-- | Annotate a 'CoreExpr' with its non-TopLevel free type and value
-- variables and its unapplied variables at every tree node
analyzeFVs :: FVEnv -> CoreExpr -> CoreExprWithFVIs
analyzeFVs env e = fst $ evalFVM (analyzeFVsM env e) 0

type EscVarSet = VarSet -- we're tracking the escaping variables in
                        -- order to identify LNEs

analyzeFVsM :: FVEnv -> CoreExpr -> FVM (CoreExprWithFVIs, EscVarSet)
analyzeFVsM  env (Var v) =
  return ( (if isLocalVar v then unitVarEnv v (FVInfo v isTopLevel fvi) else noneFVIs, AnnVar v)
         , if escapes then unitVarSet v else emptyVarSet)
  where
    n_runtime_args = fve_runtimeArgs env

    isTopLevel = not $ v `elemVarSet` fve_nonTopLevel env

    (fvi, escapes)
        | fve_isFinal env,
          Just (v_lvl, v_is_fun) <- lookupVarEnv (fve_letBoundLvls env) v
          = -- v is let-bound
--            let (clo_lvl, closureNest) = fve_feedback env
--            in
--            let all_captors = case take (clo_lvl - v_clo_lvl) closureNest of
{-            let all_captors = case take 1 $ snd $ fve_feedback env of
                       -- a closure does not capture itself
                  (cl:cls) | Just id <- cli_id cl, id == v -> cls
                  o -> o
            in
            let (escaping_captors, dominated_captors) =
                  -- TODO I think this can use Data.List.break
                  Data.List.partition (\cli -> cli_lvl cli > v_lvl) all_captors
            in -} ( if not v_is_fun then NotLBF -- it's not a function
                    else LBF { fvi_useInfo    = IntSet.singleton n_runtime_args
                             , fvi_captors    = zeroCaptors -- filled in on the way up
                             , fvi_lvl        = v_lvl
                             }
                  , -- an occurrence of a let-bound variable escapes
                    -- if it is under- or over-saturated
                    n_runtime_args /= idArity v
                  )
        | otherwise = (NotLBF, False)

analyzeFVsM _env (Lit lit) = return ((noneFVIs, AnnLit lit), emptyVarSet)

analyzeFVsM  env (Lam b body) = do
  (body', esc_vars) <- flip analyzeFVsM body $ extendEnv [b] $ lambdaEnv $ unappliedEnv env
  return ( (b `delBinderFVIs` fvisOf body', AnnLam (TB b False) body')
         , esc_vars `delVarSet` b
         )

analyzeFVsM  env app@(App fun arg) = do
  let argDmds = case fve_argumentDemands env of
        Nothing   -> computeArgumentDemands app
        Just dmds -> dmds

  let (argIsStrictlyDemanded, dmds') = case argDmds of
        [] -> (False, []) -- for some reason, we couldn't determine
                          -- argument strictness for this application
        isStrDmd : dmds -> (isStrDmd, dmds)
      funEnv = env { fve_argumentDemands = Just dmds' }

  let argIsAClosure = not $ (fve_useDmd env && argIsStrictlyDemanded) || exprIsTrivial' arg

  (fun2, fun_esc) <- flip analyzeFVsM fun $ if isRuntimeArg arg
                                            then appliedEnv funEnv
                                            else            funEnv
  (arg2, _arg_esc) <- analyzeFVsM (unappliedEnv env) arg
  let arg_esc = prjFreeNonTopLevelIds (fvisOf arg2)
        -- all arg free ids escape

  x <- gensymFVM
  arg2 <- return $
    let arg_fvis = fvisOf arg2
        new_arg_fvis = if argIsAClosure
                   then closureBindingFVIs x env Nothing arg_fvis
                   else arg_fvis
    in (new_arg_fvis, snd arg2)

  return ( (fvisOf fun2 `bothFVIs` fvisOf arg2, AnnApp fun2 arg2)
         , fun_esc `extendVarSetList` arg_esc
         )

analyzeFVsM env (Case scrut bndr ty alts) = do
  (scrut2, _scrut_esc) <- analyzeFVsM (unappliedEnv env) scrut
  let scrut_fvis = fvisOf scrut2

  x <- flip mapM alts $ \(con,args,rhs) -> do
    (rhs2, rhs_esc) <- flip analyzeFVsM rhs $ unappliedEnv $
                                              extendEnv (bndr : args) env
    return ( (delBindersFVIs args (fvisOf rhs2), (con, map (flip TB False) args, rhs2))
           , rhs_esc `delVarSetList` args
           )
  let (pairs, rhs_escs) = unzip x
      (alts_fvis_s, alts2) = unzip pairs

  let alts_fvis = foldr altFVIs noneFVIs alts_fvis_s

  return ( ( (bndr `delBinderFVIs` alts_fvis)
             `bothFVIs` scrut_fvis
             `bothFVIs` nonValueFVs (tyVarsOfType ty)
           , AnnCase scrut2 (TB bndr False) ty alts2 )
         , (unionVarSets rhs_escs `delVarSet` bndr)
           `extendVarSetList` prjFreeNonTopLevelIds scrut_fvis
         )

analyzeFVsM env (Let (NonRec binder rhs) body) = do
  p@(rhs2, _rhs_esc) <- analyzeFVsM (unappliedEnv env) rhs

  (body2, body_esc, isLNE) <- mfix $ \knot -> do
    let ~(_, _, isLNE) = knot
    (body2, body_esc) <- flip analyzeFVsM body $ extendEnv [binder] $ unappliedEnv $ letBoundEnv binder isLNE rhs env
    let isLNE = not $ binder `elemVarSet` body_esc
    return (body2, body_esc, isLNE)

  x <- gensymFVM
  (rhs2, rhs_esc) <- return $
    let rhs_fvis = fvisOf rhs2 in
    if fve_ignoreLNEClo env && isLNE then p else
    let rhs_esc = mkVarSet $ prjFreeNonTopLevelIds rhs_fvis in -- all closure/scrutinee free ids escape
    if fve_useDmd env && isStrictDmd (idDemandInfo binder)
    then (rhs2, rhs_esc) -- won't be a closure, since it's strictly demanded
    else let new_rhs_fvis = closureBindingFVIs x env (Just binder) rhs_fvis
         in ( (new_rhs_fvis, snd rhs2) , rhs_esc )

  return ( ( fvisOf rhs2 `bothFVIs`
             (binder `delBinderFVIs` fvisOf body2) `bothFVIs`
             assumeTheBest (bndrRuleAndUnfoldingVars binder)
           , AnnLet (AnnNonRec (TB binder isLNE) rhs2) body2 )
         , (body_esc `delVarSet` binder) `unionVarSet` rhs_esc
         )

analyzeFVsM env (Let (Rec binds) body) = do
  let binders = map fst binds

  (binds2, body2, body_esc, scope_esc, isLNE) <- mfix $ \knot -> do
    let ~(_, _, _, _, isLNE) = knot
    (binds2, rhs_esc_s) <- flip mapAndUnzipM binds $ \(binder, rhs) -> do
      (rhs2, rhs_esc) <- flip analyzeFVsM rhs $ unappliedEnv $ extendEnv binders $ letBoundsEnv binds isLNE env
      return ((binder, rhs2), rhs_esc)

    (body2, body_esc) <- flip analyzeFVsM body $ extendEnv binders $ unappliedEnv $ letBoundsEnv binds isLNE env

    let scope_esc = unionVarSets (body_esc : rhs_esc_s)
    let isLNE = not $ any (`elemVarSet` scope_esc) binders
    return (binds2, body2, body_esc, scope_esc, isLNE)

  x <- gensymFVM
  (rhss2, scope_esc) <- return $ if fve_ignoreLNEClo env && isLNE then (map snd binds2, scope_esc) else
    let (rhss2, rhs_esc_s) = unzip $ flip map binds2 $ \(binder, rhs2) ->
          let rhs_fvis = fvisOf rhs2
              new_rhs_fvis = closureBindingFVIs x env (Just binder) rhs_fvis
          in ( (new_rhs_fvis, snd rhs2)
             , prjFreeNonTopLevelIds new_rhs_fvis -- all closure free ids escape
             )
    in (rhss2, body_esc `unionVarSet` mkVarSet (concat rhs_esc_s))

  return ( ( delBindersFVIs binders $
           fvisOf body2 `bothFVIs` computeRecRHSsFVIs binders (map fvisOf rhss2)
           , AnnLet (AnnRec (map (flip TB isLNE) binders `zip` rhss2)) body2)
         , scope_esc `delVarSetList` binders
         )

analyzeFVsM  env (Cast expr co) = do
  let cfvis = nonValueFVs $ tyCoVarsOfCo co

  (expr2, esc) <- analyzeFVsM env expr
  return ( ( fvisOf expr2 `bothFVIs` cfvis , AnnCast expr2 (cfvis, co) )
         , esc )

analyzeFVsM  env (Tick tickish expr) = do
 (expr2, esc ) <- analyzeFVsM env expr
 return ( ( tickishFVIs tickish `bothFVIs` fvisOf expr2 , AnnTick tickish expr2 )
        , esc)

analyzeFVsM _env (Type ty) = return ( (nonValueFVs $ tyVarsOfType ty, AnnType ty), emptyVarSet )

analyzeFVsM _env (Coercion co) = return ( (nonValueFVs $ tyCoVarsOfCo co, AnnCoercion co), emptyVarSet )


computeRecRHSsFVIs :: [Var] -> [FVIs] -> FVIs
computeRecRHSsFVIs binders rhs_fvis =
  foldr (bothFVIs . assumeTheBest . idRuleAndUnfoldingVars)
        (foldr bothFVIs emptyVarEnv rhs_fvis)
        binders

-- should mirror CorePrep.cpeApp.collect_args
computeArgumentDemands :: CoreExpr -> [Bool]
computeArgumentDemands e = go e 0 where
  go (App f a) as | isRuntimeArg a = go f (1 + as)
                  | otherwise      = go f as
  go (Cast f _) as = go f as
  go (Tick _ f) as = go f as
  go e          as = case e of
    Var fid | length argStricts <= as -> -- at least saturated
      reverse argStricts ++ replicate (as - length argStricts) False
      where argStricts = map isStrictDmd $ fst $ splitStrictSig $ idStrictness fid
    _       -> []


tickishFVIs :: Tickish Id -> FVIs
tickishFVIs (Breakpoint _ ids) = nonValueFVs (mkVarSet ids)
tickishFVIs _                  = noneFVIs


exprIsTrivial' :: CoreExpr -> Bool
exprIsTrivial' (Var _)          = True        -- See Note [Variables are trivial]
exprIsTrivial' (Type _)        = True
exprIsTrivial' (Coercion _)     = True
exprIsTrivial' (Lit lit)        = litIsTrivial lit
exprIsTrivial' (App e arg)      = not (isRuntimeArg arg) && exprIsTrivial' e
exprIsTrivial' (Tick _ e)       = exprIsTrivial' e
exprIsTrivial' (Cast e _)       = exprIsTrivial' e
exprIsTrivial' (Lam b body)     = not (isRuntimeVar b) && exprIsTrivial' body
exprIsTrivial' _                = False

\end{code}
