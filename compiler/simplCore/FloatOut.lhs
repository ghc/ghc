%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FloatOut]{Float bindings outwards (towards the top level)}

``Long-distance'' floating of bindings towards the top level.

\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module FloatOut ( floatOutwards ) where

import CoreSyn
import CoreUtils
import MkCore
import CoreArity	( etaExpand )
import CoreMonad	( FloatOutSwitches(..) )

import DynFlags
import ErrUtils		( dumpIfSet_dyn )
import Id		( Id, idArity, isBottomingId )
import Var		( Var )
import SetLevels
import UniqSupply       ( UniqSupply )
import Bag
import Util
import Maybes
import Outputable
import FastString
import qualified Data.IntMap as M

#include "HsVersions.h"
\end{code}

	-----------------
	Overall game plan
	-----------------

The Big Main Idea is:

  	To float out sub-expressions that can thereby get outside
	a non-one-shot value lambda, and hence may be shared.


To achieve this we may need to do two thing:

   a) Let-bind the sub-expression:

	f (g x)  ==>  let lvl = f (g x) in lvl

      Now we can float the binding for 'lvl'.  

   b) More than that, we may need to abstract wrt a type variable

	\x -> ... /\a -> let v = ...a... in ....

      Here the binding for v mentions 'a' but not 'x'.  So we
      abstract wrt 'a', to give this binding for 'v':

	    vp = /\a -> ...a...
	    v  = vp a

      Now the binding for vp can float out unimpeded.
      I can't remember why this case seemed important enough to
      deal with, but I certainly found cases where important floats
      didn't happen if we did not abstract wrt tyvars.

With this in mind we can also achieve another goal: lambda lifting.
We can make an arbitrary (function) binding float to top level by
abstracting wrt *all* local variables, not just type variables, leaving
a binding that can be floated right to top level.  Whether or not this
happens is controlled by a flag.


Random comments
~~~~~~~~~~~~~~~

At the moment we never float a binding out to between two adjacent
lambdas.  For example:

@
	\x y -> let t = x+x in ...
===>
	\x -> let t = x+x in \y -> ...
@
Reason: this is less efficient in the case where the original lambda
is never partially applied.

But there's a case I've seen where this might not be true.  Consider:
@
elEm2 x ys
  = elem' x ys
  where
    elem' _ []	= False
    elem' x (y:ys)	= x==y || elem' x ys
@
It turns out that this generates a subexpression of the form
@
	\deq x ys -> let eq = eqFromEqDict deq in ...
@
vwhich might usefully be separated to
@
	\deq -> let eq = eqFromEqDict deq in \xy -> ...
@
Well, maybe.  We don't do this at the moment.


%************************************************************************
%*									*
\subsection[floatOutwards]{@floatOutwards@: let-floating interface function}
%*									*
%************************************************************************

\begin{code}
floatOutwards :: FloatOutSwitches
	      -> DynFlags
	      -> UniqSupply 
	      -> CoreProgram -> IO CoreProgram

floatOutwards float_sws dflags us pgm
  = do {
	let { annotated_w_levels = setLevels float_sws pgm us ;
	      (fss, binds_s')    = unzip (map floatTopBind annotated_w_levels)
	    } ;

	dumpIfSet_dyn dflags Opt_D_verbose_core2core "Levels added:"
	          (vcat (map ppr annotated_w_levels));

	let { (tlets, ntlets, lams) = get_stats (sum_stats fss) };

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats "FloatOut stats:"
		(hcat [	int tlets,  ptext (sLit " Lets floated to top level; "),
			int ntlets, ptext (sLit " Lets floated elsewhere; from "),
			int lams,   ptext (sLit " Lambda groups")]);

	return (bagToList (unionManyBags binds_s'))
    }

floatTopBind :: LevelledBind -> (FloatStats, Bag CoreBind)
floatTopBind bind
  = case (floatBind bind) of { (fs, floats, bind') ->
    let float_bag = flattenTopFloats floats
    in case bind' of
      Rec prs   -> (fs, unitBag (Rec (addTopFloatPairs float_bag prs)))
      NonRec {} -> (fs, float_bag `snocBag` bind') }
\end{code}

%************************************************************************
%*									*
\subsection[FloatOut-Bind]{Floating in a binding (the business end)}
%*									*
%************************************************************************

\begin{code}
floatBind :: LevelledBind -> (FloatStats, FloatBinds, CoreBind)
floatBind (NonRec (TB var _) rhs)
  = case (floatExpr rhs) of { (fs, rhs_floats, rhs') ->

	-- A tiresome hack: 
	-- see Note [Bottoming floats: eta expansion] in SetLevels
    let rhs'' | isBottomingId var = etaExpand (idArity var) rhs'
	      | otherwise         = rhs'

    in (fs, rhs_floats, NonRec var rhs'') }

floatBind (Rec pairs)
  = case floatList do_pair pairs of { (fs, rhs_floats, new_pairs) ->
    (fs, rhs_floats, Rec (concat new_pairs)) }
  where
    do_pair (TB name spec, rhs)
      | isTopLvl dest_lvl  -- See Note [floatBind for top level]
      = case (floatExpr rhs) of { (fs, rhs_floats, rhs') ->
        (fs, emptyFloats, addTopFloatPairs (flattenTopFloats rhs_floats) [(name, rhs')])}
      | otherwise	  -- Note [Floating out of Rec rhss]
      = case (floatExpr rhs) of { (fs, rhs_floats, rhs') ->
        case (partitionByLevel dest_lvl rhs_floats) of { (rhs_floats', heres) ->
        case (splitRecFloats heres) of { (pairs, case_heres) ->
        (fs, rhs_floats', (name, installUnderLambdas case_heres rhs') : pairs) }}}
      where
        dest_lvl = floatSpecLevel spec

splitRecFloats :: Bag FloatBind -> ([(Id,CoreExpr)], Bag FloatBind)
-- The "tail" begins with a case
-- See Note [Floating out of Rec rhss]
splitRecFloats fs
  = go [] (bagToList fs)
  where
    go prs (FloatLet (NonRec b r) : fs) = go ((b,r):prs) fs
    go prs (FloatLet (Rec prs')   : fs) = go (prs' ++ prs) fs
    go prs fs                           = (prs, listToBag fs)

installUnderLambdas :: Bag FloatBind -> CoreExpr -> CoreExpr
-- Note [Floating out of Rec rhss]
installUnderLambdas floats e
  | isEmptyBag floats = e
  | otherwise         = go e
  where
    go (Lam b e)                 = Lam b (go e)
    go e                         = install floats e

---------------
floatList :: (a -> (FloatStats, FloatBinds, b)) -> [a] -> (FloatStats, FloatBinds, [b])
floatList _ [] = (zeroStats, emptyFloats, [])
floatList f (a:as) = case f a		 of { (fs_a,  binds_a,  b)  ->
		     case floatList f as of { (fs_as, binds_as, bs) ->
		     (fs_a `add_stats` fs_as, binds_a `plusFloats`  binds_as, b:bs) }}
\end{code}

Note [Floating out of Rec rhss]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   Rec { f<1,0> = \xy. body }
From the body we may get some floats. The ones with level <1,0> must
stay here, since they may mention f.  Ideally we'd like to make them
part of the Rec block pairs -- but we can't if there are any
FloatCases involved.  

Nor is it a good idea to dump them in the rhs, but outside the lambda
    f = case x of I# y -> \xy. body
because now f's arity might get worse, which is Not Good. (And if
there's an SCC around the RHS it might not get better again.  
See Trac #5342.)

So, gruesomely, we split the floats into 
 * the outer FloatLets, which can join the Rec, and 
 * an inner batch starting in a FloatCase, which are then
   pushed *inside* the lambdas.  
This loses full-laziness the rare situation where there is a 
FloatCase and a Rec interacting.

Note [floatBind for top level]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may have a *nested* binding whose destination level is (FloatMe tOP_LEVEL), thus
         letrec { foo <0,0> = .... (let bar<0,0> = .. in ..) .... }
The binding for bar will be in the "tops" part of the floating binds,
and thus not partioned by floatBody.  

We could perhaps get rid of the 'tops' component of the floating binds,
but this case works just as well.


%************************************************************************

\subsection[FloatOut-Expr]{Floating in expressions}
%*									*
%************************************************************************

\begin{code}
floatBody :: Level
          -> LevelledExpr
	  -> (FloatStats, FloatBinds, CoreExpr)

floatBody lvl arg	-- Used rec rhss, and case-alternative rhss
  = case (floatExpr arg) of { (fsa, floats, arg') ->
    case (partitionByLevel lvl floats) of { (floats', heres) ->
	-- Dump bindings are bound here
    (fsa, floats', install heres arg') }}

-----------------
floatExpr :: LevelledExpr
	  -> (FloatStats, FloatBinds, CoreExpr)
floatExpr (Var v)   = (zeroStats, emptyFloats, Var v)
floatExpr (Type ty) = (zeroStats, emptyFloats, Type ty)
floatExpr (Coercion co) = (zeroStats, emptyFloats, Coercion co)
floatExpr (Lit lit) = (zeroStats, emptyFloats, Lit lit)
	  
floatExpr (App e a)
  = case (floatExpr  e) of { (fse, floats_e, e') ->
    case (floatExpr  a) of { (fsa, floats_a, a') ->
    (fse `add_stats` fsa, floats_e `plusFloats` floats_a, App e' a') }}

floatExpr lam@(Lam (TB _ lam_spec) _)
  = let (bndrs_w_lvls, body) = collectBinders lam
	bndrs		     = [b | TB b _ <- bndrs_w_lvls]
        bndr_lvl             = floatSpecLevel lam_spec
	-- All the binders have the same level
	-- See SetLevels.lvlLamBndrs
    in
    case (floatBody bndr_lvl body) of { (fs, floats, body') ->
    (add_to_stats fs floats, floats, mkLams bndrs body') }

floatExpr (Tick tickish expr)
  | tickishScoped tickish
  = case (floatExpr expr)    of { (fs, floating_defns, expr') ->
    let
	-- Annotate bindings floated outwards past an scc expression
	-- with the cc.  We mark that cc as "duplicated", though.
        annotated_defns = wrapTick (mkNoCount tickish) floating_defns
    in
    (fs, annotated_defns, Tick tickish expr') }

  | otherwise  -- not scoped, can just float
  = case (floatExpr expr)    of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Tick tickish expr') }

floatExpr (Cast expr co)
  = case (floatExpr expr) of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Cast expr' co) }

floatExpr (Let bind body)
  = case bind_spec of
      FloatMe dest_lvl 
        -> case (floatBind bind) of { (fsb, bind_floats, bind') ->
    	   case (floatExpr body) of { (fse, body_floats, body') ->
    	   ( add_stats fsb fse 
    	   , bind_floats `plusFloats` unitLetFloat dest_lvl bind' 
                         `plusFloats` body_floats
    	   , body') }}

      StayPut bind_lvl  -- See Note [Avoiding unnecessary floating]
        -> case (floatBind bind)          of { (fsb, bind_floats, bind') ->
    	   case (floatBody bind_lvl body) of { (fse, body_floats, body') ->
    	   ( add_stats fsb fse
    	   , bind_floats `plusFloats` body_floats
    	   , Let bind' body') }}
  where
    bind_spec = case bind of 
    	         NonRec (TB _ s) _     -> s
		 Rec ((TB _ s, _) : _) -> s
                 Rec []                -> panic "floatExpr:rec"

floatExpr (Case scrut (TB case_bndr case_spec) ty alts)
  = case case_spec of
      FloatMe dest_lvl  -- Case expression moves  
        | [(con@(DataAlt {}), bndrs, rhs)] <- alts
        -> case floatExpr scrut of { (fse, fde, scrut') ->
    	   case floatExpr rhs   of { (fsb, fdb, rhs') ->
    	   let 
    	     float = unitCaseFloat dest_lvl scrut' 
                          case_bndr con [b | TB b _ <- bndrs]
    	   in
    	   (add_stats fse fsb, fde `plusFloats` float `plusFloats` fdb, rhs') }}
        | otherwise
        -> pprPanic "Floating multi-case" (ppr alts)

      StayPut bind_lvl  -- Case expression stays put
    	-> case floatExpr scrut of { (fse, fde, scrut') ->
    	   case floatList (float_alt bind_lvl) alts of { (fsa, fda, alts')  ->
    	   (add_stats fse fsa, fda `plusFloats` fde, Case scrut' case_bndr ty alts')
    	   }}
  where
    float_alt bind_lvl (con, bs, rhs)
	= case (floatBody bind_lvl rhs)	of { (fs, rhs_floats, rhs') ->
	  (fs, rhs_floats, (con, [b | TB b _ <- bs], rhs')) }
\end{code}

Note [Avoiding unnecessary floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we want to avoid floating a let unnecessarily, because
it might worsen strictness:
    let 
       x = ...(let y = e in y+y)....
Here y is demanded.  If we float it outside the lazy 'x=..' then
we'd have to zap its demand info, and it may never be restored.

So at a 'let' we leave the binding right where the are unless
the binding will escape a value lambda, e.g.  

(\x -> let y = fac 100 in y)

That's what the partitionByMajorLevel does in the floatExpr (Let ...)
case.

Notice, though, that we must take care to drop any bindings
from the body of the let that depend on the staying-put bindings.

We used instead to do the partitionByMajorLevel on the RHS of an '=',
in floatRhs.  But that was quite tiresome.  We needed to test for
values or trival rhss, because (in particular) we don't want to insert
new bindings between the "=" and the "\".  E.g.
	f = \x -> let <bind> in <body>
We do not want
	f = let <bind> in \x -> <body>
(a) The simplifier will immediately float it further out, so we may
	as well do so right now; in general, keeping rhss as manifest 
	values is good
(b) If a float-in pass follows immediately, it might add yet more
	bindings just after the '='.  And some of them might (correctly)
	be strict even though the 'let f' is lazy, because f, being a value,
	gets its demand-info zapped by the simplifier.
And even all that turned out to be very fragile, and broke
altogether when profiling got in the way.

So now we do the partition right at the (Let..) itself.

%************************************************************************
%*									*
\subsection{Utility bits for floating stats}
%*									*
%************************************************************************

I didn't implement this with unboxed numbers.  I don't want to be too
strict in this stuff, as it is rarely turned on.  (WDP 95/09)

\begin{code}
data FloatStats
  = FlS	Int  -- Number of top-floats * lambda groups they've been past
	Int  -- Number of non-top-floats * lambda groups they've been past
	Int  -- Number of lambda (groups) seen

get_stats :: FloatStats -> (Int, Int, Int)
get_stats (FlS a b c) = (a, b, c)

zeroStats :: FloatStats
zeroStats = FlS 0 0 0

sum_stats :: [FloatStats] -> FloatStats
sum_stats xs = foldr add_stats zeroStats xs

add_stats :: FloatStats -> FloatStats -> FloatStats
add_stats (FlS a1 b1 c1) (FlS a2 b2 c2)
  = FlS (a1 + a2) (b1 + b2) (c1 + c2)

add_to_stats :: FloatStats -> FloatBinds -> FloatStats
add_to_stats (FlS a b c) (FB tops others)
  = FlS (a + lengthBag tops) (b + lengthBag (flattenMajor others)) (c + 1)
\end{code}


%************************************************************************
%*									*
\subsection{Utility bits for floating}
%*									*
%************************************************************************

Note [Representation of FloatBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatBinds types is somewhat important.  We can get very large numbers
of floating bindings, often all destined for the top level.  A typical example
is     x = [4,2,5,2,5, .... ]
Then we get lots of small expressions like (fromInteger 4), which all get
lifted to top level.  

The trouble is that  
  (a) we partition these floating bindings *at every binding site* 
  (b) SetLevels introduces a new bindings site for every float
So we had better not look at each binding at each binding site!

That is why MajorEnv is represented as a finite map.

We keep the bindings destined for the *top* level separate, because
we float them out even if they don't escape a *value* lambda; see
partitionByMajorLevel.


\begin{code}
type FloatLet = CoreBind	-- INVARIANT: a FloatLet is always lifted
type MajorEnv = M.IntMap MinorEnv	  -- Keyed by major level
type MinorEnv = M.IntMap (Bag FloatBind)  -- Keyed by minor level

data FloatBinds  = FB !(Bag FloatLet)	   	-- Destined for top level
     		      !MajorEnv 		-- Levels other than top
     -- See Note [Representation of FloatBinds]

instance Outputable FloatBind where
  ppr (FloatLet b) = ptext (sLit "LET") <+> ppr b
  ppr (FloatCase e b c bs) = hang (ptext (sLit "CASE") <+> ppr e <+> ptext (sLit "of") <+> ppr b)
                                2 (ppr c <+> ppr bs)

instance Outputable FloatBinds where
  ppr (FB fbs defs) 
      = ptext (sLit "FB") <+> (braces $ vcat
           [ ptext (sLit "tops =")     <+> ppr fbs
           , ptext (sLit "non-tops =") <+> ppr defs ])

flattenTopFloats :: FloatBinds -> Bag CoreBind
flattenTopFloats (FB tops defs) 
  = ASSERT2( isEmptyBag (flattenMajor defs), ppr defs )
    tops 

addTopFloatPairs :: Bag CoreBind -> [(Id,CoreExpr)] -> [(Id,CoreExpr)]
addTopFloatPairs float_bag prs
  = foldrBag add prs float_bag
  where
    add (NonRec b r) prs  = (b,r):prs
    add (Rec prs1)   prs2 = prs1 ++ prs2

flattenMajor :: MajorEnv -> Bag FloatBind
flattenMajor = M.fold (unionBags . flattenMinor) emptyBag

flattenMinor :: MinorEnv -> Bag FloatBind
flattenMinor = M.fold unionBags emptyBag

emptyFloats :: FloatBinds
emptyFloats = FB emptyBag M.empty

unitCaseFloat :: Level -> CoreExpr -> Id -> AltCon -> [Var] -> FloatBinds
unitCaseFloat (Level major minor) e b con bs 
  = FB emptyBag (M.singleton major (M.singleton minor (unitBag (FloatCase e b con bs))))

unitLetFloat :: Level -> FloatLet -> FloatBinds
unitLetFloat lvl@(Level major minor) b 
  | isTopLvl lvl = FB (unitBag b) M.empty
  | otherwise    = FB emptyBag (M.singleton major (M.singleton minor floats))
  where
    floats = unitBag (FloatLet b)

plusFloats :: FloatBinds -> FloatBinds -> FloatBinds
plusFloats (FB t1 l1) (FB t2 l2) 
  = FB (t1 `unionBags` t2) (l1 `plusMajor` l2)

plusMajor :: MajorEnv -> MajorEnv -> MajorEnv
plusMajor = M.unionWith plusMinor

plusMinor :: MinorEnv -> MinorEnv -> MinorEnv
plusMinor = M.unionWith unionBags

install :: Bag FloatBind -> CoreExpr -> CoreExpr
install defn_groups expr
  = foldrBag wrapFloat expr defn_groups

partitionByLevel
	:: Level		-- Partitioning level
	-> FloatBinds   	-- Defns to be divided into 2 piles...
	-> (FloatBinds,		-- Defns  with level strictly < partition level,
	    Bag FloatBind)	-- The rest

{-
-- 	 ---- partitionByMajorLevel ----
-- Float it if we escape a value lambda, 
--     *or* if we get to the top level
--     *or* if it's a case-float and its minor level is < current
-- 
-- If we can get to the top level, say "yes" anyway. This means that 
--	x = f e
-- transforms to 
--    lvl = e
--    x = f lvl
-- which is as it should be

partitionByMajorLevel (Level major _) (FB tops defns)
  = (FB tops outer, heres `unionBags` flattenMajor inner)
  where
    (outer, mb_heres, inner) = M.splitLookup major defns
    heres = case mb_heres of 
               Nothing -> emptyBag
               Just h  -> flattenMinor h
-}

partitionByLevel (Level major minor) (FB tops defns)
  = (FB tops (outer_maj `plusMajor` M.singleton major outer_min),
     here_min `unionBags` flattenMinor inner_min 
              `unionBags` flattenMajor inner_maj)

  where
    (outer_maj, mb_here_maj, inner_maj) = M.splitLookup major defns
    (outer_min, mb_here_min, inner_min) = case mb_here_maj of
                                            Nothing -> (M.empty, Nothing, M.empty)
                                            Just min_defns -> M.splitLookup minor min_defns
    here_min = mb_here_min `orElse` emptyBag

wrapTick :: Tickish Id -> FloatBinds -> FloatBinds
wrapTick t (FB tops defns)
  = FB (mapBag wrap_bind tops) (M.map (M.map wrap_defns) defns)
  where
    wrap_defns = mapBag wrap_one 

    wrap_bind (NonRec binder rhs) = NonRec binder (maybe_tick rhs)
    wrap_bind (Rec pairs)         = Rec (mapSnd maybe_tick pairs)

    wrap_one (FloatLet bind)      = FloatLet (wrap_bind bind)
    wrap_one (FloatCase e b c bs) = FloatCase (maybe_tick e) b c bs

    maybe_tick e | exprIsHNF e = tickHNFArgs t e
                 | otherwise   = mkTick t e
      -- we don't need to wrap a tick around an HNF when we float it
      -- outside a tick: that is an invariant of the tick semantics
      -- Conversely, inlining of HNFs inside an SCC is allowed, and
      -- indeed the HNF we're floating here might well be inlined back
      -- again, and we don't want to end up with duplicate ticks.
\end{code}
