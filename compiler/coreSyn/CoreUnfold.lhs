%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1994-1998
%

Core-syntax unfoldings

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @CoreUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CoreUnfold (
	Unfolding, UnfoldingGuidance,	-- Abstract types

	noUnfolding, mkImplicitUnfolding,
        mkUnfolding, mkCoreUnfolding,
	mkTopUnfolding, mkSimpleUnfolding,
	mkInlineUnfolding, mkInlinableUnfolding, mkWwInlineRule,
	mkCompulsoryUnfolding, mkDFunUnfolding,

	interestingArg, ArgSummary(..),

	couldBeSmallEnoughToInline, inlineBoringOk,
	certainlyWillInline, smallEnoughToInline,

	callSiteInline, CallCtxt(..),

        -- Reexport from CoreSubst (it only live there so it can be used
        -- by the Very Simple Optimiser)
        exprIsConApp_maybe, exprIsLiteral_maybe
    ) where

#include "HsVersions.h"

import DynFlags
import CoreSyn
import PprCore		()	-- Instances
import OccurAnal        ( occurAnalyseExpr )
import CoreSubst hiding( substTy )
import CoreArity       ( manifestArity, exprBotStrictness_maybe )
import CoreUtils
import Id
import DataCon
import Literal
import PrimOp
import IdInfo
import BasicTypes	( Arity )
import Type
import PrelNames
import TysPrim          ( realWorldStatePrimTy )
import Bag
import Util
import FastTypes
import FastString
import Outputable
import ForeignCall

import qualified Data.ByteString as BS
import Data.Maybe
\end{code}


%************************************************************************
%*									*
\subsection{Making unfoldings}
%*									*
%************************************************************************

\begin{code}
mkTopUnfolding :: DynFlags -> Bool -> CoreExpr -> Unfolding
mkTopUnfolding dflags = mkUnfolding dflags InlineRhs True {- Top level -}

mkImplicitUnfolding :: DynFlags -> CoreExpr -> Unfolding
-- For implicit Ids, do a tiny bit of optimising first
mkImplicitUnfolding dflags expr
    = mkTopUnfolding dflags False (simpleOptExpr expr)

-- Note [Top-level flag on inline rules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Slight hack: note that mk_inline_rules conservatively sets the
-- top-level flag to True.  It gets set more accurately by the simplifier
-- Simplify.simplUnfolding.

mkSimpleUnfolding :: DynFlags -> CoreExpr -> Unfolding
mkSimpleUnfolding dflags = mkUnfolding dflags InlineRhs False False

mkDFunUnfolding :: [Var] -> DataCon -> [CoreExpr] -> Unfolding
mkDFunUnfolding bndrs con ops
  = DFunUnfolding { df_bndrs = bndrs
                  , df_con = con
                  , df_args = map occurAnalyseExpr ops }
                  -- See Note [Occurrrence analysis of unfoldings]

mkWwInlineRule :: CoreExpr -> Arity -> Unfolding
mkWwInlineRule expr arity
  = mkCoreUnfolding InlineStable True
                   (simpleOptExpr expr) arity
                   (UnfWhen unSaturatedOk boringCxtNotOk)

mkCompulsoryUnfolding :: CoreExpr -> Unfolding
mkCompulsoryUnfolding expr	   -- Used for things that absolutely must be unfolded
  = mkCoreUnfolding InlineCompulsory True
                    (simpleOptExpr expr) 0    -- Arity of unfolding doesn't matter
                    (UnfWhen unSaturatedOk boringCxtOk)

mkInlineUnfolding :: Maybe Arity -> CoreExpr -> Unfolding
mkInlineUnfolding mb_arity expr 
  = mkCoreUnfolding InlineStable
    		    True 	 -- Note [Top-level flag on inline rules]
                    expr' arity 
		    (UnfWhen unsat_ok boring_ok)
  where
    expr' = simpleOptExpr expr
    (unsat_ok, arity) = case mb_arity of
                          Nothing -> (unSaturatedOk, manifestArity expr')
                          Just ar -> (needSaturated, ar)
              
    boring_ok = inlineBoringOk expr'

mkInlinableUnfolding :: DynFlags -> CoreExpr -> Unfolding
mkInlinableUnfolding dflags expr
  = mkUnfolding dflags InlineStable True is_bot expr'
  where
    expr' = simpleOptExpr expr
    is_bot = isJust (exprBotStrictness_maybe expr')
\end{code}

Internal functions

\begin{code}
mkCoreUnfolding :: UnfoldingSource -> Bool -> CoreExpr
                -> Arity -> UnfoldingGuidance -> Unfolding
-- Occurrence-analyses the expression before capturing it
mkCoreUnfolding src top_lvl expr arity guidance 
  = CoreUnfolding { uf_tmpl   	    = occurAnalyseExpr expr,
                      -- See Note [Occurrrence analysis of unfoldings]
    		    uf_src          = src,
    		    uf_arity        = arity,
		    uf_is_top 	    = top_lvl,
		    uf_is_value     = exprIsHNF        expr,
                    uf_is_conlike   = exprIsConLike    expr,
		    uf_is_work_free = exprIsWorkFree   expr,
		    uf_expandable   = exprIsExpandable expr,
		    uf_guidance     = guidance }

mkUnfolding :: DynFlags -> UnfoldingSource -> Bool -> Bool -> CoreExpr
            -> Unfolding
-- Calculates unfolding guidance
-- Occurrence-analyses the expression before capturing it
mkUnfolding dflags src top_lvl is_bottoming expr
  | top_lvl && is_bottoming
  , not (exprIsTrivial expr)
  = NoUnfolding    -- See Note [Do not inline top-level bottoming functions]
  | otherwise
  = CoreUnfolding { uf_tmpl   	    = occurAnalyseExpr expr,
                      -- See Note [Occurrrence analysis of unfoldings]
    		    uf_src          = src,
    		    uf_arity        = arity,
		    uf_is_top 	    = top_lvl,
		    uf_is_value     = exprIsHNF        expr,
                    uf_is_conlike   = exprIsConLike    expr,
		    uf_expandable   = exprIsExpandable expr,
		    uf_is_work_free = exprIsWorkFree   expr,
		    uf_guidance     = guidance }
  where
    (arity, guidance) = calcUnfoldingGuidance dflags expr
        -- NB: *not* (calcUnfoldingGuidance (occurAnalyseExpr expr))!
	-- See Note [Calculate unfolding guidance on the non-occ-anal'd expression]
\end{code}

Note [Occurrence analysis of unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do occurrence-analysis of unfoldings once and for all, when the
unfolding is built, rather than each time we inline them.

But given this decision it's vital that we do
*always* do it.  Consider this unfolding
    \x -> letrec { f = ...g...; g* = f } in body
where g* is (for some strange reason) the loop breaker.  If we don't
occ-anal it when reading it in, we won't mark g as a loop breaker, and
we may inline g entirely in body, dropping its binding, and leaving
the occurrence in f out of scope. This happened in Trac #8892, where
the unfolding in question was a DFun unfolding.

But more generally, the simplifier is designed on the
basis that it is looking at occurrence-analysed expressions, so better
ensure that they acutally are.

Note [Calculate unfolding guidance on the non-occ-anal'd expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we give the non-occur-analysed expression to
calcUnfoldingGuidance.  In some ways it'd be better to occur-analyse
first; for example, sometimes during simplification, there's a large
let-bound thing which has been substituted, and so is now dead; so
'expr' contains two copies of the thing while the occurrence-analysed
expression doesn't.

Nevertheless, we *don't* and *must not* occ-analyse before computing
the size because 

a) The size computation bales out after a while, whereas occurrence
   analysis does not.

b) Residency increases sharply if you occ-anal first.  I'm not 
   100% sure why, but it's a large effect.  Compiling Cabal went 
   from residency of 534M to over 800M with this one change.

This can occasionally mean that the guidance is very pessimistic;
it gets fixed up next round.  And it should be rare, because large
let-bound things that are dead are usually caught by preInlineUnconditionally


%************************************************************************
%*									*
\subsection{The UnfoldingGuidance type}
%*									*
%************************************************************************

\begin{code}
inlineBoringOk :: CoreExpr -> Bool
-- See Note [INLINE for small functions]
-- True => the result of inlining the expression is 
--         no bigger than the expression itself
--     eg      (\x y -> f y x)
-- This is a quick and dirty version. It doesn't attempt
-- to deal with  (\x y z -> x (y z))
-- The really important one is (x `cast` c)
inlineBoringOk e
  = go 0 e
  where
    go :: Int -> CoreExpr -> Bool
    go credit (Lam x e) | isId x           = go (credit+1) e
                        | otherwise        = go credit e
    go credit (App f (Type {}))            = go credit f
    go credit (App f a) | credit > 0  
                        , exprIsTrivial a  = go (credit-1) f
    go credit (Tick _ e)                 = go credit e -- dubious
    go credit (Cast e _) 		   = go credit e
    go _      (Var {})         		   = boringCxtOk
    go _      _                		   = boringCxtNotOk

calcUnfoldingGuidance
        :: DynFlags
        -> CoreExpr    -- Expression to look at
        -> (Arity, UnfoldingGuidance)
calcUnfoldingGuidance dflags expr
  = case collectBinders expr of { (bndrs, body) ->
    let
        bOMB_OUT_SIZE = ufCreationThreshold dflags
               -- Bomb out if size gets bigger than this
        val_bndrs   = filter isId bndrs
	n_val_bndrs = length val_bndrs

    	guidance 
          = case sizeExpr dflags (iUnbox bOMB_OUT_SIZE) val_bndrs body of
      	      TooBig -> UnfNever
      	      SizeIs size cased_bndrs scrut_discount
      	        | uncondInline expr n_val_bndrs (iBox size)
      	        -> UnfWhen unSaturatedOk boringCxtOk   -- Note [INLINE for small functions]
	        | otherwise
      	        -> UnfIfGoodArgs { ug_args  = map (discount cased_bndrs) val_bndrs
      	                         , ug_size  = iBox size
      	        	  	 , ug_res   = iBox scrut_discount }

        discount :: Bag (Id,Int) -> Id -> Int
        discount cbs bndr = foldlBag combine 0 cbs
           where
             combine acc (bndr', disc) 
               | bndr == bndr' = acc `plus_disc` disc
               | otherwise     = acc
   
             plus_disc :: Int -> Int -> Int
             plus_disc | isFunTy (idType bndr) = max
                       | otherwise             = (+)
             -- See Note [Function and non-function discounts]
    in
    (n_val_bndrs, guidance) }
\end{code}

Note [Computing the size of an expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of sizeExpr is obvious enough: count nodes.  But getting the
heuristics right has taken a long time.  Here's the basic strategy:

    * Variables, literals: 0
      (Exception for string literals, see litSize.)

    * Function applications (f e1 .. en): 1 + #value args

    * Constructor applications: 1, regardless of #args

    * Let(rec): 1 + size of components

    * Note, cast: 0

Examples

  Size	Term
  --------------
    0	  42#
    0	  x
    0     True
    2	  f x
    1	  Just x
    4 	  f (g x)

Notice that 'x' counts 0, while (f x) counts 2.  That's deliberate: there's
a function call to account for.  Notice also that constructor applications 
are very cheap, because exposing them to a caller is so valuable.

[25/5/11] All sizes are now multiplied by 10, except for primops
(which have sizes like 1 or 4.  This makes primops look fantastically
cheap, and seems to be almost unversally beneficial.  Done partly as a
result of #4978.

Note [Do not inline top-level bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatOut pass has gone to some trouble to float out calls to 'error' 
and similar friends.  See Note [Bottoming floats] in SetLevels.
Do not re-inline them!  But we *do* still inline if they are very small
(the uncondInline stuff).

Note [INLINE for small functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider	{-# INLINE f #-}
                f x = Just x
                g y = f y
Then f's RHS is no larger than its LHS, so we should inline it into
even the most boring context.  In general, f the function is
sufficiently small that its body is as small as the call itself, the
inline unconditionally, regardless of how boring the context is.

Things to note:

(1) We inline *unconditionally* if inlined thing is smaller (using sizeExpr)
    than the thing it's replacing.  Notice that
      (f x) --> (g 3) 		  -- YES, unconditionally
      (f x) --> x : []		  -- YES, *even though* there are two
      	    	    		  --      arguments to the cons
      x     --> g 3		  -- NO
      x	    --> Just v		  -- NO

    It's very important not to unconditionally replace a variable by
    a non-atomic term.

(2) We do this even if the thing isn't saturated, else we end up with the
    silly situation that
       f x y = x
       ...map (f 3)...
    doesn't inline.  Even in a boring context, inlining without being
    saturated will give a lambda instead of a PAP, and will be more
    efficient at runtime.

(3) However, when the function's arity > 0, we do insist that it 
    has at least one value argument at the call site.  (This check is
    made in the UnfWhen case of callSiteInline.) Otherwise we find this:
         f = /\a \x:a. x
         d = /\b. MkD (f b)
    If we inline f here we get
         d = /\b. MkD (\x:b. x)
    and then prepareRhs floats out the argument, abstracting the type
    variables, so we end up with the original again!

(4) We must be much more cautious about arity-zero things. Consider
       let x = y +# z in ...
    In *size* terms primops look very small, because the generate a
    single instruction, but we do not want to unconditionally replace
    every occurrence of x with (y +# z).  So we only do the
    unconditional-inline thing for *trivial* expressions.
  
    NB: you might think that PostInlineUnconditionally would do this
    but it doesn't fire for top-level things; see SimplUtils
    Note [Top level and postInlineUnconditionally]

\begin{code}
uncondInline :: CoreExpr -> Arity -> Int -> Bool
-- Inline unconditionally if there no size increase
-- Size of call is arity (+1 for the function)
-- See Note [INLINE for small functions]
uncondInline rhs arity size 
  | arity > 0 = size <= 10 * (arity + 1) -- See Note [INLINE for small functions] (1)
  | otherwise = exprIsTrivial rhs        -- See Note [INLINE for small functions] (4)
\end{code}


\begin{code}
sizeExpr :: DynFlags
         -> FastInt 	    -- Bomb out if it gets bigger than this
	 -> [Id]	    -- Arguments; we're interested in which of these
			    -- get case'd
	 -> CoreExpr
	 -> ExprSize

-- Note [Computing the size of an expression]

sizeExpr dflags bOMB_OUT_SIZE top_args expr
  = size_up expr
  where
    size_up (Cast e _) = size_up e
    size_up (Tick _ e) = size_up e
    size_up (Type _)   = sizeZero           -- Types cost nothing
    size_up (Coercion _) = sizeZero
    size_up (Lit lit)  = sizeN (litSize lit)
    size_up (Var f) | isRealWorldId f = sizeZero
                      -- Make sure we get constructor discounts even
                      -- on nullary constructors
                    | otherwise       = size_up_call f [] 0

    size_up (App fun arg)
      | isTyCoArg arg = size_up fun
      | otherwise     = size_up arg  `addSizeNSD`
                        size_up_app fun [arg] (if isRealWorldExpr arg then 1 else 0)

    size_up (Lam b e)
      | isId b && not (isRealWorldId b) = lamScrutDiscount dflags (size_up e `addSizeN` 10)
      | otherwise = size_up e

    size_up (Let (NonRec binder rhs) body)
      = size_up rhs		`addSizeNSD`
	size_up body		`addSizeN`
        (if isUnLiftedType (idType binder) then 0 else 10)
		-- For the allocation
		-- If the binder has an unlifted type there is no allocation

    size_up (Let (Rec pairs) body)
      = foldr (addSizeNSD . size_up . snd) 
              (size_up body `addSizeN` (10 * length pairs))     -- (length pairs) for the allocation
              pairs

    size_up (Case (Var v) _ _ alts) 
	| v `elem` top_args		-- We are scrutinising an argument variable
	= alts_size (foldr addAltSize sizeZero alt_sizes)
		    (foldr maxSize    sizeZero alt_sizes)
		-- Good to inline if an arg is scrutinised, because
		-- that may eliminate allocation in the caller
		-- And it eliminates the case itself
	where
	  alt_sizes = map size_up_alt alts

		-- alts_size tries to compute a good discount for
		-- the case when we are scrutinising an argument variable
	  alts_size (SizeIs tot tot_disc tot_scrut)  -- Size of all alternatives
		    (SizeIs max _        _)          -- Size of biggest alternative
                = SizeIs tot (unitBag (v, iBox (_ILIT(20) +# tot -# max)) `unionBags` tot_disc) tot_scrut
			-- If the variable is known, we produce a discount that
			-- will take us back to 'max', the size of the largest alternative
			-- The 1+ is a little discount for reduced allocation in the caller
			--
			-- Notice though, that we return tot_disc, the total discount from 
			-- all branches.  I think that's right.

	  alts_size tot_size _ = tot_size

    size_up (Case e _ _ alts) = size_up e  `addSizeNSD`
                                foldr (addAltSize . size_up_alt) case_size alts
      where
          case_size
           | is_inline_scrut e, not (lengthExceeds alts 1)  = sizeN (-10)
           | otherwise = sizeZero
                -- Normally we don't charge for the case itself, but
                -- we charge one per alternative (see size_up_alt,
                -- below) to account for the cost of the info table
                -- and comparisons.
                --
                -- However, in certain cases (see is_inline_scrut
                -- below), no code is generated for the case unless
                -- there are multiple alts.  In these cases we
                -- subtract one, making the first alt free.
                -- e.g. case x# +# y# of _ -> ...   should cost 1
                --      case touch# x# of _ -> ...  should cost 0
                -- (see #4978)
                --
                -- I would like to not have the "not (lengthExceeds alts 1)"
                -- condition above, but without that some programs got worse
                -- (spectral/hartel/event and spectral/para).  I don't fully
                -- understand why. (SDM 24/5/11)

                -- unboxed variables, inline primops and unsafe foreign calls
                -- are all "inline" things:
          is_inline_scrut (Var v) = isUnLiftedType (idType v)
          is_inline_scrut scrut
              | (Var f, _) <- collectArgs scrut
                = case idDetails f of
                    FCallId fc  -> not (isSafeForeignCall fc)
                    PrimOpId op -> not (primOpOutOfLine op)
                    _other      -> False
              | otherwise
                = False

    ------------ 
    -- size_up_app is used when there's ONE OR MORE value args
    size_up_app (App fun arg) args voids
	| isTyCoArg arg                  = size_up_app fun args voids
	| isRealWorldExpr arg            = size_up_app fun (arg:args) (voids + 1)
	| otherwise		         = size_up arg  `addSizeNSD`
                                           size_up_app fun (arg:args) voids
    size_up_app (Var fun)     args voids = size_up_call fun args voids
    size_up_app other         args voids = size_up other `addSizeN` (length args - voids)

    ------------ 
    size_up_call :: Id -> [CoreExpr] -> Int -> ExprSize
    size_up_call fun val_args voids
       = case idDetails fun of
           FCallId _        -> sizeN (10 * (1 + length val_args))
           DataConWorkId dc -> conSize    dc (length val_args)
           PrimOpId op      -> primOpSize op (length val_args)
	   ClassOpId _ 	    -> classOpSize dflags top_args val_args
	   _     	    -> funSize dflags top_args fun (length val_args) voids

    ------------ 
    size_up_alt (_con, _bndrs, rhs) = size_up rhs `addSizeN` 10
 	-- Don't charge for args, so that wrappers look cheap
	-- (See comments about wrappers with Case)
	--
	-- IMPORATANT: *do* charge 1 for the alternative, else we 
	-- find that giant case nests are treated as practically free
	-- A good example is Foreign.C.Error.errrnoToIOError

    ------------
	-- These addSize things have to be here because
	-- I don't want to give them bOMB_OUT_SIZE as an argument
    addSizeN TooBig          _  = TooBig
    addSizeN (SizeIs n xs d) m 	= mkSizeIs bOMB_OUT_SIZE (n +# iUnbox m) xs d
    
        -- addAltSize is used to add the sizes of case alternatives
    addAltSize TooBig	         _	= TooBig
    addAltSize _		 TooBig	= TooBig
    addAltSize (SizeIs n1 xs d1) (SizeIs n2 ys d2) 
	= mkSizeIs bOMB_OUT_SIZE (n1 +# n2) 
                                 (xs `unionBags` ys) 
                                 (d1 +# d2)   -- Note [addAltSize result discounts]

        -- This variant ignores the result discount from its LEFT argument
	-- It's used when the second argument isn't part of the result
    addSizeNSD TooBig	         _	= TooBig
    addSizeNSD _		 TooBig	= TooBig
    addSizeNSD (SizeIs n1 xs _) (SizeIs n2 ys d2) 
	= mkSizeIs bOMB_OUT_SIZE (n1 +# n2) 
                                 (xs `unionBags` ys) 
                                 d2  -- Ignore d1

    isRealWorldId id = idType id `eqType` realWorldStatePrimTy

    -- an expression of type State# RealWorld must be a variable
    isRealWorldExpr (Var id) = isRealWorldId id
    isRealWorldExpr _        = False
\end{code}


\begin{code}
-- | Finds a nominal size of a string literal.
litSize :: Literal -> Int
-- Used by CoreUnfold.sizeExpr
litSize (LitInteger {}) = 100	-- Note [Size of literal integers]
litSize (MachStr str)   = 10 + 10 * ((BS.length str + 3) `div` 4)
	-- If size could be 0 then @f "x"@ might be too small
	-- [Sept03: make literal strings a bit bigger to avoid fruitless 
	--  duplication of little strings]
litSize _other = 0    -- Must match size of nullary constructors
	       	      -- Key point: if  x |-> 4, then x must inline unconditionally
		      --     	    (eg via case binding)

classOpSize :: DynFlags -> [Id] -> [CoreExpr] -> ExprSize
-- See Note [Conlike is interesting]
classOpSize _ _ []
  = sizeZero
classOpSize dflags top_args (arg1 : other_args)
  = SizeIs (iUnbox size) arg_discount (_ILIT(0))
  where
    size = 20 + (10 * length other_args)
    -- If the class op is scrutinising a lambda bound dictionary then
    -- give it a discount, to encourage the inlining of this function
    -- The actual discount is rather arbitrarily chosen
    arg_discount = case arg1 of
    		     Var dict | dict `elem` top_args 
		     	      -> unitBag (dict, ufDictDiscount dflags)
		     _other   -> emptyBag
    		     
funSize :: DynFlags -> [Id] -> Id -> Int -> Int -> ExprSize
-- Size for functions that are not constructors or primops
-- Note [Function applications]
funSize dflags top_args fun n_val_args voids
  | fun `hasKey` buildIdKey   = buildSize
  | fun `hasKey` augmentIdKey = augmentSize
  | otherwise = SizeIs (iUnbox size) arg_discount (iUnbox res_discount)
  where
    some_val_args = n_val_args > 0

    size | some_val_args = 10 * (1 + n_val_args - voids)
         | otherwise     = 0
	-- The 1+ is for the function itself
	-- Add 1 for each non-trivial arg;
	-- the allocation cost, as in let(rec)
  
        --                  DISCOUNTS
        --  See Note [Function and non-function discounts]
    arg_discount | some_val_args && fun `elem` top_args
    		 = unitBag (fun, ufFunAppDiscount dflags)
		 | otherwise = emptyBag
	-- If the function is an argument and is applied
	-- to some values, give it an arg-discount

    res_discount | idArity fun > n_val_args = ufFunAppDiscount dflags
    		 | otherwise   	 	    = 0
        -- If the function is partially applied, show a result discount

conSize :: DataCon -> Int -> ExprSize
conSize dc n_val_args
  | n_val_args == 0 = SizeIs (_ILIT(0)) emptyBag (_ILIT(10))    -- Like variables

-- See Note [Unboxed tuple size and result discount]
  | isUnboxedTupleCon dc = SizeIs (_ILIT(0)) emptyBag (iUnbox (10 * (1 + n_val_args)))

-- See Note [Constructor size and result discount]
  | otherwise = SizeIs (_ILIT(10)) emptyBag (iUnbox (10 * (1 + n_val_args)))
\end{code}

Note [Constructor size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Treat a constructors application as size 10, regardless of how many
arguments it has; we are keen to expose them (and we charge separately
for their args).  We can't treat them as size zero, else we find that
(Just x) has size 0, which is the same as a lone variable; and hence
'v' will always be replaced by (Just x), where v is bound to Just x.

The "result discount" is applied if the result of the call is
scrutinised (say by a case).  For a constructor application that will
mean the constructor application will disappear, so we don't need to
charge it to the function.  So the discount should at least match the
cost of the constructor application, namely 10.  But to give a bit
of extra incentive we give a discount of 10*(1 + n_val_args).

Simon M tried a MUCH bigger discount: (10 * (10 + n_val_args)), 
and said it was an "unambiguous win", but its terribly dangerous
because a fuction with many many case branches, each finishing with
a constructor, can have an arbitrarily large discount.  This led to
terrible code bloat: see Trac #6099.

Note [Unboxed tuple size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However, unboxed tuples count as size zero. I found occasions where we had 
	f x y z = case op# x y z of { s -> (# s, () #) }
and f wasn't getting inlined.

I tried giving unboxed tuples a *result discount* of zero (see the
commented-out line).  Why?  When returned as a result they do not
allocate, so maybe we don't want to charge so much for them If you
have a non-zero discount here, we find that workers often get inlined
back into wrappers, because it look like
    f x = case $wf x of (# a,b #) -> (a,b)
and we are keener because of the case.  However while this change
shrank binary sizes by 0.5% it also made spectral/boyer allocate 5%
more. All other changes were very small. So it's not a big deal but I
didn't adopt the idea.

Note [Function and non-function discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want a discount if the function is applied. A good example is
monadic combinators with continuation arguments, where inlining is
quite important.

But we don't want a big discount when a function is called many times
(see the detailed comments with Trac #6048) because if the function is 
big it won't be inlined at its many call sites and no benefit results.
Indeed, we can get exponentially big inlinings this way; that is what
Trac #6048 is about.

On the other hand, for data-valued arguments, if there are lots of
case expressions in the body, each one will get smaller if we apply
the function to a constructor application, so we *want* a big discount
if the argument is scrutinised by many case expressions.

Conclusion:
  - For functions, take the max of the discounts
  - For data values, take the sum of the discounts


Note [Literal integer size]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal integers *can* be big (mkInteger [...coefficients...]), but
need not be (S# n).  We just use an aribitrary big-ish constant here
so that, in particular, we don't inline top-level defns like
   n = S# 5
There's no point in doing so -- any optimisations will see the S#
through n's unfolding.  Nor will a big size inhibit unfoldings functions
that mention a literal Integer, because the float-out pass will float
all those constants to top level.

\begin{code}
primOpSize :: PrimOp -> Int -> ExprSize
primOpSize op n_val_args
 = if primOpOutOfLine op
      then sizeN (op_size + n_val_args)
      else sizeN op_size
 where
   op_size = primOpCodeSize op


buildSize :: ExprSize
buildSize = SizeIs (_ILIT(0)) emptyBag (_ILIT(40))
	-- We really want to inline applications of build
	-- build t (\cn -> e) should cost only the cost of e (because build will be inlined later)
	-- Indeed, we should add a result_discount becuause build is 
	-- very like a constructor.  We don't bother to check that the
	-- build is saturated (it usually is).  The "-2" discounts for the \c n, 
	-- The "4" is rather arbitrary.

augmentSize :: ExprSize
augmentSize = SizeIs (_ILIT(0)) emptyBag (_ILIT(40))
	-- Ditto (augment t (\cn -> e) ys) should cost only the cost of
	-- e plus ys. The -2 accounts for the \cn 

-- When we return a lambda, give a discount if it's used (applied)
lamScrutDiscount :: DynFlags -> ExprSize -> ExprSize
lamScrutDiscount dflags (SizeIs n vs _) = SizeIs n vs (iUnbox (ufFunAppDiscount dflags))
lamScrutDiscount _      TooBig          = TooBig
\end{code}

Note [addAltSize result discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib 
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.

Note [Discounts and thresholds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Constants for discounts and thesholds are defined in main/DynFlags,
all of form ufXxxx.   They are:

ufCreationThreshold
     At a definition site, if the unfolding is bigger than this, we
     may discard it altogether

ufUseThreshold
     At a call site, if the unfolding, less discounts, is smaller than
     this, then it's small enough inline

ufKeenessFactor
     Factor by which the discounts are multiplied before 
     subtracting from size

ufDictDiscount
     The discount for each occurrence of a dictionary argument
     as an argument of a class method.  Should be pretty small
     else big functions may get inlined

ufFunAppDiscount
     Discount for a function argument that is applied.  Quite
     large, because if we inline we avoid the higher-order call.

ufDearOp
     The size of a foreign call or not-dupable PrimOp


Note [Function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a function application (f a b)

  - If 'f' is an argument to the function being analysed, 
    and there's at least one value arg, record a FunAppDiscount for f

  - If the application if a PAP (arity > 2 in this example)
    record a *result* discount (because inlining
    with "extra" args in the call may mean that we now 
    get a saturated application)

Code for manipulating sizes

\begin{code}
data ExprSize = TooBig
	      | SizeIs FastInt		-- Size found
		       !(Bag (Id,Int))	-- Arguments cased herein, and discount for each such
		       FastInt		-- Size to subtract if result is scrutinised 
					-- by a case expression

instance Outputable ExprSize where
  ppr TooBig         = ptext (sLit "TooBig")
  ppr (SizeIs a _ c) = brackets (int (iBox a) <+> int (iBox c))

-- subtract the discount before deciding whether to bale out. eg. we
-- want to inline a large constructor application into a selector:
--  	tup = (a_1, ..., a_99)
--  	x = case tup of ...
--
mkSizeIs :: FastInt -> FastInt -> Bag (Id, Int) -> FastInt -> ExprSize
mkSizeIs max n xs d | (n -# d) ># max = TooBig
		    | otherwise	      = SizeIs n xs d
 
maxSize :: ExprSize -> ExprSize -> ExprSize
maxSize TooBig         _ 				  = TooBig
maxSize _              TooBig				  = TooBig
maxSize s1@(SizeIs n1 _ _) s2@(SizeIs n2 _ _) | n1 ># n2  = s1
					      | otherwise = s2

sizeZero :: ExprSize
sizeN :: Int -> ExprSize

sizeZero = SizeIs (_ILIT(0))  emptyBag (_ILIT(0))
sizeN n  = SizeIs (iUnbox n) emptyBag (_ILIT(0))
\end{code}


%************************************************************************
%*									*
\subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}
%*									*
%************************************************************************

We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
we ``couldn't possibly use'' on the other side.  Can be overridden w/
flaggery.  Just the same as smallEnoughToInline, except that it has no
actual arguments.

\begin{code}
couldBeSmallEnoughToInline :: DynFlags -> Int -> CoreExpr -> Bool
couldBeSmallEnoughToInline dflags threshold rhs 
  = case sizeExpr dflags (iUnbox threshold) [] body of
       TooBig -> False
       _      -> True
  where
    (_, body) = collectBinders rhs

----------------
smallEnoughToInline :: DynFlags -> Unfolding -> Bool
smallEnoughToInline dflags (CoreUnfolding {uf_guidance = UnfIfGoodArgs {ug_size = size}})
  = size <= ufUseThreshold dflags
smallEnoughToInline _ _
  = False

----------------
certainlyWillInline :: DynFlags -> Unfolding -> Bool
  -- Sees if the unfolding is pretty certain to inline	
certainlyWillInline dflags (CoreUnfolding { uf_arity = n_vals, uf_guidance = guidance })
  = case guidance of
      UnfNever      -> False
      UnfWhen {}    -> True
      UnfIfGoodArgs { ug_size = size} 
                    -> n_vals > 0     -- See Note [certainlyWillInline: be caseful of thunks]
                    && size - (10 * (n_vals +1)) <= ufUseThreshold dflags

certainlyWillInline _ _
  = False
\end{code}

Note [certainlyWillInline: be caseful of thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't claim that thunks will certainly inline, because that risks work
duplication.  Even if the work duplication is not great (eg is_cheap
holds), it can make a big difference in an inner loop In Trac #5623 we
found that the WorkWrap phase thought that
       y = case x of F# v -> F# (v +# v)
was certainlyWillInline, so the addition got duplicated.  


%************************************************************************
%*									*
\subsection{callSiteInline}
%*									*
%************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or 
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work, 
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in 
StrictAnal.addStrictnessInfoToTopId

\begin{code}
callSiteInline :: DynFlags
	       -> Id			-- The Id
	       -> Bool			-- True <=> unfolding is active
	       -> Bool			-- True if there are are no arguments at all (incl type args)
	       -> [ArgSummary]		-- One for each value arg; True if it is interesting
	       -> CallCtxt		-- True <=> continuation is interesting
	       -> Maybe CoreExpr	-- Unfolding, if any

instance Outputable ArgSummary where
  ppr TrivArg    = ptext (sLit "TrivArg")
  ppr NonTrivArg = ptext (sLit "NonTrivArg")
  ppr ValueArg   = ptext (sLit "ValueArg")

data CallCtxt
  = BoringCtxt
  | RhsCtxt             -- Rhs of a let-binding; see Note [RHS of lets]
  | DiscArgCtxt         -- Argument of a fuction with non-zero arg discount
  | RuleArgCtxt		-- We are somewhere in the argument of a function with rules

  | ValAppCtxt 	        -- We're applied to at least one value arg
		        -- This arises when we have ((f x |> co) y)
		        -- Then the (f x) has argument 'x' but in a ValAppCtxt

  | CaseCtxt	        -- We're the scrutinee of a case
		        -- that decomposes its scrutinee

instance Outputable CallCtxt where
  ppr CaseCtxt 	  = ptext (sLit "CaseCtxt")
  ppr ValAppCtxt  = ptext (sLit "ValAppCtxt")
  ppr BoringCtxt  = ptext (sLit "BoringCtxt")
  ppr RhsCtxt     = ptext (sLit "RhsCtxt")
  ppr DiscArgCtxt = ptext (sLit "DiscArgCtxt")
  ppr RuleArgCtxt = ptext (sLit "RuleArgCtxt")

callSiteInline dflags id active_unfolding lone_variable arg_infos cont_info
  = case idUnfolding id of 
      -- idUnfolding checks for loop-breakers, returning NoUnfolding
      -- Things with an INLINE pragma may have an unfolding *and* 
      -- be a loop breaker  (maybe the knot is not yet untied)
	CoreUnfolding { uf_tmpl = unf_template, uf_is_top = is_top 
		      , uf_is_work_free = is_wf, uf_arity = uf_arity
                      , uf_guidance = guidance, uf_expandable = is_exp }
          | active_unfolding -> tryUnfolding dflags id lone_variable 
                                    arg_infos cont_info unf_template is_top 
                                    is_wf is_exp uf_arity guidance
          | dopt Opt_D_dump_inlinings dflags && dopt Opt_D_verbose_core2core dflags
          -> pprTrace "Inactive unfolding:" (ppr id) Nothing
          | otherwise -> Nothing
	NoUnfolding 	 -> Nothing 
	OtherCon {} 	 -> Nothing 
	DFunUnfolding {} -> Nothing 	-- Never unfold a DFun

tryUnfolding :: DynFlags -> Id -> Bool -> [ArgSummary] -> CallCtxt
             -> CoreExpr -> Bool -> Bool -> Bool -> Arity -> UnfoldingGuidance
	     -> Maybe CoreExpr	
tryUnfolding dflags id lone_variable 
             arg_infos cont_info unf_template is_top 
             is_wf is_exp uf_arity guidance
			-- uf_arity will typically be equal to (idArity id), 
			-- but may be less for InlineRules
 | dopt Opt_D_dump_inlinings dflags && dopt Opt_D_verbose_core2core dflags
 = pprTrace ("Considering inlining: " ++ showSDocDump dflags (ppr id))
		 (vcat [text "arg infos" <+> ppr arg_infos,
			text "uf arity" <+> ppr uf_arity,
			text "interesting continuation" <+> ppr cont_info,
			text "some_benefit" <+> ppr some_benefit,
                        text "is exp:" <+> ppr is_exp,
                        text "is work-free:" <+> ppr is_wf,
			text "guidance" <+> ppr guidance,
			extra_doc,
			text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO"])
	         result
  | otherwise  = result

  where
    n_val_args = length arg_infos
    saturated  = n_val_args >= uf_arity
    cont_info' | n_val_args > uf_arity = ValAppCtxt
               | otherwise             = cont_info

    result | yes_or_no = Just unf_template
           | otherwise = Nothing

    interesting_args = any nonTriv arg_infos 
    	-- NB: (any nonTriv arg_infos) looks at the
    	-- over-saturated args too which is "wrong"; 
    	-- but if over-saturated we inline anyway.

           -- some_benefit is used when the RHS is small enough
           -- and the call has enough (or too many) value
           -- arguments (ie n_val_args >= arity). But there must
           -- be *something* interesting about some argument, or the
           -- result context, to make it worth inlining
    some_benefit 
       | not saturated = interesting_args	-- Under-saturated
    	   	      		     	-- Note [Unsaturated applications]
       | otherwise = interesting_args	-- Saturated or over-saturated
                  || interesting_call

    interesting_call 
      = case cont_info' of
          CaseCtxt   -> not (lone_variable && is_wf)  -- Note [Lone variables]
          ValAppCtxt -> True			      -- Note [Cast then apply]
          RuleArgCtxt -> uf_arity > 0  -- See Note [Unfold info lazy contexts]
          DiscArgCtxt -> uf_arity > 0  --
          RhsCtxt     -> uf_arity > 0  --
          _           -> not is_top && uf_arity > 0   -- Note [Nested functions]
                                                      -- Note [Inlining in ArgCtxt]

    (yes_or_no, extra_doc)
      = case guidance of
          UnfNever -> (False, empty)

          UnfWhen unsat_ok boring_ok 
             -> (enough_args && (boring_ok || some_benefit), empty )
             where      -- See Note [INLINE for small functions (3)]
               enough_args = saturated || (unsat_ok && n_val_args > 0)

          UnfIfGoodArgs { ug_args = arg_discounts, ug_res = res_discount, ug_size = size }
      	     -> ( is_wf && some_benefit && small_enough
                , (text "discounted size =" <+> int discounted_size) )
    	     where
    	       discounted_size = size - discount
    	       small_enough = discounted_size <= ufUseThreshold dflags
    	       discount = computeDiscount dflags uf_arity arg_discounts 
    	         		          res_discount arg_infos cont_info'
\end{code}

Note [Unfold into lazy contexts], Note [RHS of lets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the call is the argument of a function with a RULE, or the RHS of a let,
we are a little bit keener to inline.  For example
     f y = (y,y,y)
     g y = let x = f y in ...(case x of (a,b,c) -> ...) ...
We'd inline 'f' if the call was in a case context, and it kind-of-is,
only we can't see it.  Also
     x = f v
could be expensive whereas
     x = case v of (a,b) -> a
is patently cheap and may allow more eta expansion.
So we treat the RHS of a let as not-totally-boring.

Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a call is not saturated, we *still* inline if one of the
arguments has interesting structure.  That's sometimes very important.
A good example is the Ord instance for Bool in Base:

 Rec {
    $fOrdBool =GHC.Classes.D:Ord
        	 @ Bool
      		 ...
      		 $cmin_ajX

    $cmin_ajX [Occ=LoopBreaker] :: Bool -> Bool -> Bool
    $cmin_ajX = GHC.Classes.$dmmin @ Bool $fOrdBool
  }

But the defn of GHC.Classes.$dmmin is:

  $dmmin :: forall a. GHC.Classes.Ord a => a -> a -> a
    {- Arity: 3, HasNoCafRefs, Strictness: SLL,
       Unfolding: (\ @ a $dOrd :: GHC.Classes.Ord a x :: a y :: a ->
                   case @ a GHC.Classes.<= @ a $dOrd x y of wild {
                     GHC.Types.False -> y GHC.Types.True -> x }) -}

We *really* want to inline $dmmin, even though it has arity 3, in
order to unravel the recursion.


Note [Things to watch]
~~~~~~~~~~~~~~~~~~~~~~
*   { y = I# 3; x = y `cast` co; ...case (x `cast` co) of ... }
    Assume x is exported, so not inlined unconditionally.
    Then we want x to inline unconditionally; no reason for it 
    not to, and doing so avoids an indirection.

*   { x = I# 3; ....f x.... }
    Make sure that x does not inline unconditionally!  
    Lest we get extra allocation.

Note [Inlining an InlineRule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An InlineRules is used for
  (a) programmer INLINE pragmas
  (b) inlinings from worker/wrapper

For (a) the RHS may be large, and our contract is that we *only* inline
when the function is applied to all the arguments on the LHS of the
source-code defn.  (The uf_arity in the rule.)

However for worker/wrapper it may be worth inlining even if the 
arity is not satisfied (as we do in the CoreUnfolding case) so we don't
require saturation.


Note [Nested functions]
~~~~~~~~~~~~~~~~~~~~~~~
If a function has a nested defn we also record some-benefit, on the
grounds that we are often able to eliminate the binding, and hence the
allocation, for the function altogether; this is good for join points.
But this only makes sense for *functions*; inlining a constructor
doesn't help allocation unless the result is scrutinised.  UNLESS the
constructor occurs just once, albeit possibly in multiple case
branches.  Then inlining it doesn't increase allocation, but it does
increase the chance that the constructor won't be allocated at all in
the branches that don't use it.

Note [Cast then apply]
~~~~~~~~~~~~~~~~~~~~~~
Consider
   myIndex = __inline_me ( (/\a. <blah>) |> co )
   co :: (forall a. a -> a) ~ (forall a. T a)
     ... /\a.\x. case ((myIndex a) |> sym co) x of { ... } ...

We need to inline myIndex to unravel this; but the actual call (myIndex a) has
no value arguments.  The ValAppCtxt gives it enough incentive to inline.

Note [Inlining in ArgCtxt]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The condition (arity > 0) here is very important, because otherwise
we end up inlining top-level stuff into useless places; eg
   x = I# 3#
   f = \y.  g x
This can make a very big difference: it adds 16% to nofib 'integer' allocs,
and 20% to 'power'.

At one stage I replaced this condition by 'True' (leading to the above 
slow-down).  The motivation was test eyeball/inline1.hs; but that seems
to work ok now.

NOTE: arguably, we should inline in ArgCtxt only if the result of the
call is at least CONLIKE.  At least for the cases where we use ArgCtxt
for the RHS of a 'let', we only profit from the inlining if we get a 
CONLIKE thing (modulo lets).

Note [Lone variables]	See also Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~   which appears below
The "lone-variable" case is important.  I spent ages messing about
with unsatisfactory varaints, but this is nice.  The idea is that if a
variable appears all alone

	as an arg of lazy fn, or rhs	BoringCtxt
	as scrutinee of a case		CaseCtxt
	as arg of a fn			ArgCtxt
AND
	it is bound to a cheap expression

then we should not inline it (unless there is some other reason,
e.g. is is the sole occurrence).  That is what is happening at 
the use of 'lone_variable' in 'interesting_call'.

Why?  At least in the case-scrutinee situation, turning
	let x = (a,b) in case x of y -> ...
into
	let x = (a,b) in case (a,b) of y -> ...
and thence to 
	let x = (a,b) in let y = (a,b) in ...
is bad if the binding for x will remain.

Another example: I discovered that strings
were getting inlined straight back into applications of 'error'
because the latter is strict.
	s = "foo"
	f = \x -> ...(error s)...

Fundamentally such contexts should not encourage inlining because the
context can ``see'' the unfolding of the variable (e.g. case or a
RULE) so there's no gain.  If the thing is bound to a value.

However, watch out:

 * Consider this:
	foo = _inline_ (\n. [n])
	bar = _inline_ (foo 20)
	baz = \n. case bar of { (m:_) -> m + n }
   Here we really want to inline 'bar' so that we can inline 'foo'
   and the whole thing unravels as it should obviously do.  This is 
   important: in the NDP project, 'bar' generates a closure data
   structure rather than a list. 

   So the non-inlining of lone_variables should only apply if the
   unfolding is regarded as cheap; because that is when exprIsConApp_maybe
   looks through the unfolding.  Hence the "&& is_wf" in the
   InlineRule branch.

 * Even a type application or coercion isn't a lone variable.
   Consider
	case $fMonadST @ RealWorld of { :DMonad a b c -> c }
   We had better inline that sucker!  The case won't see through it.

   For now, I'm treating treating a variable applied to types 
   in a *lazy* context "lone". The motivating example was
	f = /\a. \x. BIG
	g = /\a. \y.  h (f a)
   There's no advantage in inlining f here, and perhaps
   a significant disadvantage.  Hence some_val_args in the Stop case

Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The lone-variable test says "don't inline if a case expression
scrutines a lone variable whose unfolding is cheap".  It's very 
important that, under these circumstances, exprIsConApp_maybe
can spot a constructor application. So, for example, we don't
consider
	let x = e in (x,x)
to be cheap, and that's good because exprIsConApp_maybe doesn't
think that expression is a constructor application.

In the 'not (lone_variable && is_wf)' test, I used to test is_value
rather than is_wf, which was utterly wrong, because the above
expression responds True to exprIsHNF, which is what sets is_value.

This kind of thing can occur if you have

	{-# INLINE foo #-}
	foo = let x = e in (x,x)

which Roman did.

\begin{code}
computeDiscount :: DynFlags -> Arity -> [Int] -> Int -> [ArgSummary] -> CallCtxt
                -> Int
computeDiscount dflags uf_arity arg_discounts res_discount arg_infos cont_info
 	-- We multiple the raw discounts (args_discount and result_discount)
	-- ty opt_UnfoldingKeenessFactor because the former have to do with
	--  *size* whereas the discounts imply that there's some extra 
	--  *efficiency* to be gained (e.g. beta reductions, case reductions) 
	-- by inlining.

  = 10          -- Discount of 1 because the result replaces the call
		-- so we count 1 for the function itself

    + 10 * length (take uf_arity arg_infos)
      	       -- Discount of (un-scaled) 1 for each arg supplied, 
   	       -- because the result replaces the call

    + round (ufKeenessFactor dflags *
	     fromIntegral (arg_discount + res_discount'))
  where
    arg_discount = sum (zipWith mk_arg_discount arg_discounts arg_infos)

    mk_arg_discount _ 	     TrivArg    = 0 
    mk_arg_discount _        NonTrivArg = 10
    mk_arg_discount discount ValueArg   = discount 

    res_discount' = case cont_info of
			BoringCtxt  -> 0
			CaseCtxt    -> res_discount  -- Presumably a constructor
			ValAppCtxt  -> res_discount  -- Presumably a function
			_           -> 40 `min` res_discount
                -- ToDo: this 40 `min` res_dicount doesn't seem right
                --   for DiscArgCtxt it shouldn't matter because the function will
                --    get the arg discount for any non-triv arg
                --   for RuleArgCtxt we do want to be keener to inline; but not only
                --    constructor results
                --   for RhsCtxt I suppose that exposing a data con is good in general
                --   And 40 seems very arbitrary
                --
		-- res_discount can be very large when a function returns
		-- constructors; but we only want to invoke that large discount
		-- when there's a case continuation.
		-- Otherwise we, rather arbitrarily, threshold it.  Yuk.
		-- But we want to aovid inlining large functions that return 
		-- constructors into contexts that are simply "interesting"
\end{code}

%************************************************************************
%*									*
	Interesting arguments
%*									*
%************************************************************************

Note [Interesting arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An argument is interesting if it deserves a discount for unfoldings
with a discount in that argument position.  The idea is to avoid
unfolding a function that is applied only to variables that have no
unfolding (i.e. they are probably lambda bound): f x y z There is
little point in inlining f here.

Generally, *values* (like (C a b) and (\x.e)) deserve discounts.  But
we must look through lets, eg (let x = e in C a b), because the let will
float, exposing the value, if we inline.  That makes it different to
exprIsHNF.

Before 2009 we said it was interesting if the argument had *any* structure
at all; i.e. (hasSomeUnfolding v).  But does too much inlining; see Trac #3016.

But we don't regard (f x y) as interesting, unless f is unsaturated.
If it's saturated and f hasn't inlined, then it's probably not going
to now!

Note [Conlike is interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	f d = ...((*) d x y)...
	... f (df d')...
where df is con-like. Then we'd really like to inline 'f' so that the
rule for (*) (df d) can fire.  To do this 
  a) we give a discount for being an argument of a class-op (eg (*) d)
  b) we say that a con-like argument (eg (df d)) is interesting

\begin{code}
data ArgSummary = TrivArg	-- Nothing interesting
     		| NonTrivArg	-- Arg has structure
		| ValueArg	-- Arg is a con-app or PAP
		  		-- ..or con-like. Note [Conlike is interesting]

interestingArg :: CoreExpr -> ArgSummary
-- See Note [Interesting arguments]
interestingArg e = go e 0
  where
    -- n is # value args to which the expression is applied
    go (Lit {}) _   	   = ValueArg
    go (Var v)  n
       | isConLikeId v     = ValueArg	-- Experimenting with 'conlike' rather that
       	 	     	     		--    data constructors here
       | idArity v > n	   = ValueArg	-- Catches (eg) primops with arity but no unfolding
       | n > 0	           = NonTrivArg	-- Saturated or unknown call
       | conlike_unfolding = ValueArg	-- n==0; look for an interesting unfolding
                                        -- See Note [Conlike is interesting]
       | otherwise	   = TrivArg	-- n==0, no useful unfolding
       where
         conlike_unfolding = isConLikeUnfolding (idUnfolding v)

    go (Type _)          _ = TrivArg
    go (Coercion _)      _ = TrivArg
    go (App fn (Type _)) n = go fn n
    go (App fn (Coercion _)) n = go fn n
    go (App fn _)        n = go fn (n+1)
    go (Tick _ a)      n = go a n
    go (Cast e _) 	 n = go e n
    go (Lam v e)  	 n 
       | isTyVar v	   = go e n
       | n>0	 	   = go e (n-1)
       | otherwise	   = ValueArg
    go (Let _ e)  	 n = case go e n of { ValueArg -> ValueArg; _ -> NonTrivArg }
    go (Case {})  	 _ = NonTrivArg

nonTriv ::  ArgSummary -> Bool
nonTriv TrivArg = False
nonTriv _       = True
\end{code}
