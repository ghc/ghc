%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | CoreSyn holds all the main data types for use by for the Glasgow Haskell Compiler midsection
module CoreSyn (
	-- * Main data types
        Expr(..), Alt, Bind(..), AltCon(..), Arg, Tickish(..),
        CoreProgram, CoreExpr, CoreAlt, CoreBind, CoreArg, CoreBndr,
        TaggedExpr, TaggedAlt, TaggedBind, TaggedArg, TaggedBndr(..), deTagExpr,

        -- ** 'Expr' construction
	mkLets, mkLams,
	mkApps, mkTyApps, mkCoApps, mkVarApps,
	
	mkIntLit, mkIntLitInt,
	mkWordLit, mkWordLitWord,
	mkWord64LitWord64, mkInt64LitInt64,
	mkCharLit, mkStringLit,
	mkFloatLit, mkFloatLitFloat,
	mkDoubleLit, mkDoubleLitDouble,
	
	mkConApp, mkConApp2, mkTyBind, mkCoBind,
	varToCoreExpr, varsToCoreExprs,

        isId, cmpAltCon, cmpAlt, ltAlt,
	
	-- ** Simple 'Expr' access functions and predicates
	bindersOf, bindersOfBinds, rhssOfBind, rhssOfAlts, 
	collectBinders, collectTyBinders, collectValBinders, collectTyAndValBinders,
        collectArgs, flattenBinds,

        isValArg, isTypeArg, isTyCoArg, valArgCount, valBndrCount,
        isRuntimeArg, isRuntimeVar,

        tickishCounts, tickishScoped, tickishIsCode, mkNoCount, mkNoScope,
        tickishCanSplit,

        -- * Unfolding data types
        Unfolding(..),  UnfoldingGuidance(..), UnfoldingSource(..),

	-- ** Constructing 'Unfolding's
	noUnfolding, evaldUnfolding, mkOtherCon,
        unSaturatedOk, needSaturated, boringCxtOk, boringCxtNotOk,
	
	-- ** Predicates and deconstruction on 'Unfolding'
	unfoldingTemplate, setUnfoldingTemplate, expandUnfolding_maybe,
	maybeUnfoldingTemplate, otherCons, unfoldingArity,
	isValueUnfolding, isEvaldUnfolding, isCheapUnfolding,
        isExpandableUnfolding, isConLikeUnfolding, isCompulsoryUnfolding,
        isStableUnfolding, isStableCoreUnfolding_maybe, isUnstableUnfolding,
        isClosedUnfolding, hasSomeUnfolding, 
	canUnfold, neverUnfoldGuidance, isStableSource,

	-- * Strictness
	seqExpr, seqExprs, seqUnfolding, 

	-- * Annotated expression data types
	AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt,
	
        -- ** Operations on annotated expressions
        collectAnnArgs,

	-- ** Operations on annotations
	deAnnotate, deAnnotate', deAnnAlt, collectAnnBndrs,

	-- * Core rule data types
	CoreRule(..),	-- CoreSubst, CoreTidy, CoreFVs, PprCore only
	RuleName, RuleFun, IdUnfoldingFun, InScopeEnv,
	
	-- ** Operations on 'CoreRule's 
	seqRules, ruleArity, ruleName, ruleIdName, ruleActivation,
	setRuleIdName,
	isBuiltinRule, isLocalRule,

	-- * Core vectorisation declarations data type
	CoreVect(..)
    ) where

#include "HsVersions.h"

import CostCentre
import VarEnv( InScopeSet )
import Var
import Type
import Coercion
import Name
import Literal
import DataCon
import Module
import TyCon
import BasicTypes
import DynFlags
import FastString
import Outputable
import Util

import Data.Data hiding (TyCon)
import Data.Int
import Data.Word

infixl 4 `mkApps`, `mkTyApps`, `mkVarApps`, `App`, `mkCoApps`
-- Left associative, so that we can say (f `mkTyApps` xs `mkVarApps` ys)
\end{code}

%************************************************************************
%*									*
\subsection{The main data types}
%*									*
%************************************************************************

These data types are the heart of the compiler

\begin{code}
-- | This is the data type that represents GHCs core intermediate language. Currently
-- GHC uses System FC <http://research.microsoft.com/~simonpj/papers/ext-f/> for this purpose,
-- which is closely related to the simpler and better known System F <http://en.wikipedia.org/wiki/System_F>.
--
-- We get from Haskell source to this Core language in a number of stages:
--
-- 1. The source code is parsed into an abstract syntax tree, which is represented
--    by the data type 'HsExpr.HsExpr' with the names being 'RdrName.RdrNames'
--
-- 2. This syntax tree is /renamed/, which attaches a 'Unique.Unique' to every 'RdrName.RdrName'
--    (yielding a 'Name.Name') to disambiguate identifiers which are lexically identical. 
--    For example, this program:
--
-- @
--      f x = let f x = x + 1
--            in f (x - 2)
-- @
--
--    Would be renamed by having 'Unique's attached so it looked something like this:
--
-- @
--      f_1 x_2 = let f_3 x_4 = x_4 + 1
--                in f_3 (x_2 - 2)
-- @
--    But see Note [Shadowing] below.
--
-- 3. The resulting syntax tree undergoes type checking (which also deals with instantiating
--    type class arguments) to yield a 'HsExpr.HsExpr' type that has 'Id.Id' as it's names.
--
-- 4. Finally the syntax tree is /desugared/ from the expressive 'HsExpr.HsExpr' type into
--    this 'Expr' type, which has far fewer constructors and hence is easier to perform
--    optimization, analysis and code generation on.
--
-- The type parameter @b@ is for the type of binders in the expression tree.
--
-- The language consists of the following elements:
--
-- *  Variables
--
-- *  Primitive literals
--
-- *  Applications: note that the argument may be a 'Type'.
--
--    See "CoreSyn#let_app_invariant" for another invariant
--
-- *  Lambda abstraction
--
-- *  Recursive and non recursive @let@s. Operationally
--    this corresponds to allocating a thunk for the things
--    bound and then executing the sub-expression.
--    
--    #top_level_invariant#
--    #letrec_invariant#
--    
--    The right hand sides of all top-level and recursive @let@s
--    /must/ be of lifted type (see "Type#type_classification" for
--    the meaning of /lifted/ vs. /unlifted/).
--    
--    #let_app_invariant#
--    The right hand side of of a non-recursive 'Let' 
--    _and_ the argument of an 'App',
--    /may/ be of unlifted type, but only if the expression 
--    is ok-for-speculation.  This means that the let can be floated 
--    around without difficulty. For example, this is OK:
--    
--    > y::Int# = x +# 1#
--    
--    But this is not, as it may affect termination if the 
--    expression is floated out:
--    
--    > y::Int# = fac 4#
--    
--    In this situation you should use @case@ rather than a @let@. The function
--    'CoreUtils.needsCaseBinding' can help you determine which to generate, or
--    alternatively use 'MkCore.mkCoreLet' rather than this constructor directly,
--    which will generate a @case@ if necessary
--    
--    #type_let#
--    We allow a /non-recursive/ let to bind a type variable, thus:
--    
--    > Let (NonRec tv (Type ty)) body
--    
--    This can be very convenient for postponing type substitutions until
--    the next run of the simplifier.
--    
--    At the moment, the rest of the compiler only deals with type-let
--    in a Let expression, rather than at top level.  We may want to revist
--    this choice.
--
-- *  Case split. Operationally this corresponds to evaluating
--    the scrutinee (expression examined) to weak head normal form
--    and then examining at most one level of resulting constructor (i.e. you
--    cannot do nested pattern matching directly with this).
--    
--    The binder gets bound to the value of the scrutinee,
--    and the 'Type' must be that of all the case alternatives
--    
--    #case_invariants#
--    This is one of the more complicated elements of the Core language, 
--    and comes with a number of restrictions:
--    
--    1. The list of alternatives may be empty; 
--       See Note [Empty case alternatives]
--
--    2. The 'DEFAULT' case alternative must be first in the list, 
--       if it occurs at all.
--    
--    3. The remaining cases are in order of increasing 
--         tag	(for 'DataAlts') or
--         lit	(for 'LitAlts').
--       This makes finding the relevant constructor easy, 
--       and makes comparison easier too.
--    
--    4. The list of alternatives must be exhaustive. An /exhaustive/ case 
--       does not necessarily mention all constructors:
--    
--    	 @
--    	      data Foo = Red | Green | Blue
--    	 ... case x of 
--    	      Red   -> True
--    	      other -> f (case x of 
--    	                      Green -> ...
--    	                      Blue  -> ... ) ...
--    	 @
--    
--    	 The inner case does not need a @Red@ alternative, because @x@ 
--    	 can't be @Red@ at that program point.
--
-- *  Cast an expression to a particular type. 
--    This is used to implement @newtype@s (a @newtype@ constructor or 
--    destructor just becomes a 'Cast' in Core) and GADTs.
--
-- *  Notes. These allow general information to be added to expressions
--    in the syntax tree
--
-- *  A type: this should only show up at the top level of an Arg
--
-- *  A coercion

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data Expr b
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]	-- See #case_invariant#
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion
  deriving (Data, Typeable)

-- | Type synonym for expressions that occur in function argument positions.
-- Only 'Arg' should contain a 'Type' at top level, general 'Expr' should not
type Arg b = Expr b

-- | A case split alternative. Consists of the constructor leading to the alternative,
-- the variables bound from the constructor, and the expression to be executed given that binding.
-- The default alternative is @(DEFAULT, [], rhs)@

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
type Alt b = (AltCon, [b], Expr b)

-- | A case alternative constructor (i.e. pattern match)

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data AltCon 
  = DataAlt DataCon   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@

  | LitAlt  Literal   -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
		      -- See Note [Literal alternatives]
	      	      
  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq, Ord, Data, Typeable)

-- | Binding, used for top level bindings in a module and local bindings in a @let@.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data Bind b = NonRec b (Expr b)
	    | Rec [(b, (Expr b))]
  deriving (Data, Typeable)
\end{code}

Note [Shadowing]
~~~~~~~~~~~~~~~~
While various passes attempt to rename on-the-fly in a manner that
avoids "shadowing" (thereby simplifying downstream optimizations),
neither the simplifier nor any other pass GUARANTEES that shadowing is
avoided. Thus, all passes SHOULD work fine even in the presence of
arbitrary shadowing in their inputs.

In particular, scrutinee variables `x` in expressions of the form
`Case e x t` are often renamed to variables with a prefix
"wild_". These "wild" variables may appear in the body of the
case-expression, and further, may be shadowed within the body.

So the Unique in an Var is not really unique at all.  Still, it's very
useful to give a constant-time equality/ordering for Vars, and to give
a key that can be used to make sets of Vars (VarSet), or mappings from
Vars to other things (VarEnv).   Moreover, if you do want to eliminate
shadowing, you can give a new Unique to an Id without changing its
printable name, which makes debugging easier.

Note [Literal alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal alternatives (LitAlt lit) are always for *un-lifted* literals.
We have one literal, a literal Integer, that is lifted, and we don't
allow in a LitAlt, because LitAlt cases don't do any evaluation. Also
(see Trac #5603) if you say
    case 3 of
      S# x -> ...
      J# _ _ -> ...
(where S#, J# are the constructors for Integer) we don't want the
simplifier calling findAlt with argument (LitAlt 3).  No no.  Integer
literals are an opaque encoding of an algebraic data type, not of
an unlifted literal, like all the others.


-------------------------- CoreSyn INVARIANTS ---------------------------

Note [CoreSyn top-level invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #toplevel_invariant#

Note [CoreSyn letrec invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #letrec_invariant#

Note [CoreSyn let/app invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #let_app_invariant#

This is intially enforced by DsUtils.mkCoreLet and mkCoreApp

Note [CoreSyn case invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #case_invariants#

Note [CoreSyn let goal]
~~~~~~~~~~~~~~~~~~~~~~~
* The simplifier tries to ensure that if the RHS of a let is a constructor
  application, its arguments are trivial, so that the constructor can be
  inlined vigorously.

Note [Type let]
~~~~~~~~~~~~~~~
See #type_let#

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The alternatives of a case expression should be exhaustive.  A case expression
can have empty alternatives if (and only if) the scrutinee is bound to raise
an exception or diverge.  So:
   Case (error Int "Hello") b Bool []
is fine, and has type Bool.  This is one reason we need a type on 
the case expression: if the alternatives are empty we can't get the type
from the alternatives!  I'll write this
   case (error Int "Hello") of Bool {}
with the return type just before the alternatives.

Here's another example:
  data T
  f :: T -> Bool
  f = \(x:t). case x of Bool {}
Since T has no data constructors, the case alternatives are of course
empty.  However note that 'x' is not bound to a visibly-bottom value;
it's the *type* that tells us it's going to diverge.  Its a bit of a
degnerate situation but we do NOT want to replace
   case x of Bool {}   -->   error Bool "Inaccessible case"
because x might raise an exception, and *that*'s what we want to see!
(Trac #6067 is an example.) To preserve semantics we'd have to say
   x `seq` error Bool "Inaccessible case"   
 but the 'seq' is just a case, so we are back to square 1.  Or I suppose
we could say
   x |> UnsafeCoerce T Bool
but that loses all trace of the fact that this originated with an empty
set of alternatives.

We can use the empty-alternative construct to coerce error values from
one type to another.  For example

    f :: Int -> Int
    f n = error "urk"
   
    g :: Int -> (# Char, Bool #)
    g x = case f x of { 0 -> ..., n -> ... }

Then if we inline f in g's RHS we get
    case (error Int "urk") of (# Char, Bool #) { ... }
and we can discard the alternatives since the scrutinee is bottom to give
    case (error Int "urk") of (# Char, Bool #) {}

This is nicer than using an unsafe coerce between Int ~ (# Char,Bool #),
if for no other reason that we don't need to instantiate the (~) at an 
unboxed type.


%************************************************************************
%*									*
              Ticks
%*									*
%************************************************************************

\begin{code}
-- | Allows attaching extra information to points in expressions

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data Tickish id =
    -- | An @{-# SCC #-}@ profiling annotation, either automatically
    -- added by the desugarer as a result of -auto-all, or added by
    -- the user.
    ProfNote {
      profNoteCC    :: CostCentre, -- ^ the cost centre
      profNoteCount :: !Bool,      -- ^ bump the entry count?
      profNoteScope :: !Bool       -- ^ scopes over the enclosed expression
                                   -- (i.e. not just a tick)
    }

  -- | A "tick" used by HPC to track the execution of each
  -- subexpression in the original source code.
  | HpcTick {
      tickModule :: Module,
      tickId     :: !Int
    }

  -- | A breakpoint for the GHCi debugger.  This behaves like an HPC
  -- tick, but has a list of free variables which will be available
  -- for inspection in GHCi when the program stops at the breakpoint.
  --
  -- NB. we must take account of these Ids when (a) counting free variables,
  -- and (b) substituting (don't substitute for them)
  | Breakpoint
    { breakpointId     :: !Int
    , breakpointFVs    :: [id]  -- ^ the order of this list is important:
                                -- it matches the order of the lists in the
                                -- appropriate entry in HscTypes.ModBreaks.
                                --
                                -- Careful about substitution!  See
                                -- Note [substTickish] in CoreSubst.
    }

  deriving (Eq, Ord, Data, Typeable)


-- | A "counting tick" (where tickishCounts is True) is one that
-- counts evaluations in some way.  We cannot discard a counting tick,
-- and the compiler should preserve the number of counting ticks as
-- far as possible.
--
-- However, we still allow the simplifier to increase or decrease
-- sharing, so in practice the actual number of ticks may vary, except
-- that we never change the value from zero to non-zero or vice versa.
--
tickishCounts :: Tickish id -> Bool
tickishCounts n@ProfNote{} = profNoteCount n
tickishCounts HpcTick{}    = True
tickishCounts Breakpoint{} = True

tickishScoped :: Tickish id -> Bool
tickishScoped n@ProfNote{} = profNoteScope n
tickishScoped HpcTick{}    = False
tickishScoped Breakpoint{} = True
   -- Breakpoints are scoped: eventually we're going to do call
   -- stacks, but also this helps prevent the simplifier from moving
   -- breakpoints around and changing their result type (see #1531).

mkNoCount :: Tickish id -> Tickish id
mkNoCount n@ProfNote{} = n {profNoteCount = False}
mkNoCount Breakpoint{} = panic "mkNoCount: Breakpoint" -- cannot split a BP
mkNoCount HpcTick{}    = panic "mkNoCount: HpcTick"

mkNoScope :: Tickish id -> Tickish id
mkNoScope n@ProfNote{} = n {profNoteScope = False}
mkNoScope Breakpoint{} = panic "mkNoScope: Breakpoint" -- cannot split a BP
mkNoScope HpcTick{}    = panic "mkNoScope: HpcTick"

-- | Return True if this source annotation compiles to some code, or will
-- disappear before the backend.
tickishIsCode :: Tickish id -> Bool
tickishIsCode _tickish = True  -- all of them for now

-- | Return True if this Tick can be split into (tick,scope) parts with
-- 'mkNoScope' and 'mkNoCount' respectively.
tickishCanSplit :: Tickish Id -> Bool
tickishCanSplit Breakpoint{} = False
tickishCanSplit HpcTick{}    = False
tickishCanSplit _ = True
\end{code}


%************************************************************************
%*									*
\subsection{Transformation rules}
%*									*
%************************************************************************

The CoreRule type and its friends are dealt with mainly in CoreRules,
but CoreFVs, Subst, PprCore, CoreTidy also inspect the representation.

\begin{code}
-- | A 'CoreRule' is:
--
-- * \"Local\" if the function it is a rule for is defined in the
--   same module as the rule itself.
--
-- * \"Orphan\" if nothing on the LHS is defined in the same module
--   as the rule itself
data CoreRule
  = Rule { 
	ru_name :: RuleName,            -- ^ Name of the rule, for communication with the user
	ru_act  :: Activation,          -- ^ When the rule is active

	-- Rough-matching stuff
	-- see comments with InstEnv.ClsInst( is_cls, is_rough )
	ru_fn    :: Name,	        -- ^ Name of the 'Id.Id' at the head of this rule
	ru_rough :: [Maybe Name],	-- ^ Name at the head of each argument to the left hand side
	
	-- Proper-matching stuff
	-- see comments with InstEnv.ClsInst( is_tvs, is_tys )
	ru_bndrs :: [CoreBndr],         -- ^ Variables quantified over
	ru_args  :: [CoreExpr],         -- ^ Left hand side arguments
	
	-- And the right-hand side
	ru_rhs   :: CoreExpr,           -- ^ Right hand side of the rule
		    			-- Occurrence info is guaranteed correct
					-- See Note [OccInfo in unfoldings and rules]

	-- Locality
        ru_auto :: Bool,	-- ^ @True@  <=> this rule is auto-generated
		   		--   @False@ <=> generated at the users behest
				--   Main effect: reporting of orphan-hood

	ru_local :: Bool	-- ^ @True@ iff the fn at the head of the rule is
				-- defined in the same module as the rule
				-- and is not an implicit 'Id' (like a record selector,
				-- class operation, or data constructor)

		-- NB: ru_local is *not* used to decide orphan-hood
		--	c.g. MkIface.coreRuleToIfaceRule
    }

  -- | Built-in rules are used for constant folding
  -- and suchlike.  They have no free variables.
  | BuiltinRule {               
	ru_name  :: RuleName,   -- ^ As above
	ru_fn    :: Name,       -- ^ As above
	ru_nargs :: Int,	-- ^ Number of arguments that 'ru_try' consumes,
				-- if it fires, including type arguments
	ru_try   :: RuleFun
		-- ^ This function does the rewrite.  It given too many
		-- arguments, it simply discards them; the returned 'CoreExpr'
		-- is just the rewrite of 'ru_fn' applied to the first 'ru_nargs' args
    }
		-- See Note [Extra args in rule matching] in Rules.lhs

type RuleFun = DynFlags -> InScopeEnv -> Id -> [CoreExpr] -> Maybe CoreExpr
type InScopeEnv = (InScopeSet, IdUnfoldingFun)

type IdUnfoldingFun = Id -> Unfolding
-- A function that embodies how to unfold an Id if you need
-- to do that in the Rule.  The reason we need to pass this info in
-- is that whether an Id is unfoldable depends on the simplifier phase

isBuiltinRule :: CoreRule -> Bool
isBuiltinRule (BuiltinRule {}) = True
isBuiltinRule _		       = False

-- | The number of arguments the 'ru_fn' must be applied 
-- to before the rule can match on it
ruleArity :: CoreRule -> Int
ruleArity (BuiltinRule {ru_nargs = n}) = n
ruleArity (Rule {ru_args = args})      = length args

ruleName :: CoreRule -> RuleName
ruleName = ru_name

ruleActivation :: CoreRule -> Activation
ruleActivation (BuiltinRule { })       = AlwaysActive
ruleActivation (Rule { ru_act = act }) = act

-- | The 'Name' of the 'Id.Id' at the head of the rule left hand side
ruleIdName :: CoreRule -> Name
ruleIdName = ru_fn

isLocalRule :: CoreRule -> Bool
isLocalRule = ru_local

-- | Set the 'Name' of the 'Id.Id' at the head of the rule left hand side
setRuleIdName :: Name -> CoreRule -> CoreRule
setRuleIdName nm ru = ru { ru_fn = nm }
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Vectorisation declarations}
%*                                                                      *
%************************************************************************

Representation of desugared vectorisation declarations that are fed to the vectoriser (via
'ModGuts').

\begin{code}
data CoreVect = Vect      Id   CoreExpr
              | NoVect    Id
              | VectType  Bool TyCon (Maybe TyCon)
              | VectClass TyCon                     -- class tycon
              | VectInst  Id                        -- instance dfun (always SCALAR)  !!!FIXME: should be superfluous now
\end{code}


%************************************************************************
%*                                                                      *
                Unfoldings
%*                                                                      *
%************************************************************************

The @Unfolding@ type is declared here to avoid numerous loops

\begin{code}
-- | Records the /unfolding/ of an identifier, which is approximately the form the
-- identifier would have if we substituted its definition in for the identifier.
-- This type should be treated as abstract everywhere except in "CoreUnfold"
data Unfolding
  = NoUnfolding        -- ^ We have no information about the unfolding

  | OtherCon [AltCon]  -- ^ It ain't one of these constructors.
		       -- @OtherCon xs@ also indicates that something has been evaluated
		       -- and hence there's no point in re-evaluating it.
		       -- @OtherCon []@ is used even for non-data-type values
		       -- to indicated evaluated-ness.  Notably:
		       --
		       -- > data C = C !(Int -> Int)
		       -- > case x of { C f -> ... }
		       --
		       -- Here, @f@ gets an @OtherCon []@ unfolding.

  | DFunUnfolding {     -- The Unfolding of a DFunId  
    			-- See Note [DFun unfoldings]
      		  	--     df = /\a1..am. \d1..dn. MkD t1 .. tk
                        --                                 (op1 a1..am d1..dn)
     		      	--     	    	      	       	   (op2 a1..am d1..dn)
        df_bndrs :: [Var],      -- The bound variables [a1..m],[d1..dn]
        df_con   :: DataCon,    -- The dictionary data constructor (never a newtype datacon)
        df_args  :: [CoreExpr]  -- Args of the data con: types, superclasses and methods,
    }                           -- in positional order

  | CoreUnfolding {		-- An unfolding for an Id with no pragma, 
                                -- or perhaps a NOINLINE pragma
				-- (For NOINLINE, the phase, if any, is in the 
                                -- InlinePragInfo for this Id.)
	uf_tmpl       :: CoreExpr,	  -- Template; occurrence info is correct
	uf_src        :: UnfoldingSource, -- Where the unfolding came from
	uf_is_top     :: Bool,		-- True <=> top level binding
	uf_arity      :: Arity,		-- Number of value arguments expected
	uf_is_value   :: Bool,		-- exprIsHNF template (cached); it is ok to discard 
		      			--	a `seq` on this variable
        uf_is_conlike :: Bool,          -- True <=> applicn of constructor or CONLIKE function
                                        --      Cached version of exprIsConLike
	uf_is_work_free :: Bool,		-- True <=> doesn't waste (much) work to expand 
                                        --          inside an inlining
					-- 	Cached version of exprIsCheap
	uf_expandable :: Bool,		-- True <=> can expand in RULE matching
		      	 		--      Cached version of exprIsExpandable
	uf_guidance   :: UnfoldingGuidance	-- Tells about the *size* of the template.
    }
  -- ^ An unfolding with redundant cached information. Parameters:
  --
  --  uf_tmpl: Template used to perform unfolding; 
  --           NB: Occurrence info is guaranteed correct: 
  --	           see Note [OccInfo in unfoldings and rules]
  --
  --  uf_is_top: Is this a top level binding?
  --
  --  uf_is_value: 'exprIsHNF' template (cached); it is ok to discard a 'seq' on
  --     this variable
  --
  --  uf_is_work_free:  Does this waste only a little work if we expand it inside an inlining?
  --     Basically this is a cached version of 'exprIsWorkFree'
  --
  --  uf_guidance:  Tells us about the /size/ of the unfolding template


------------------------------------------------
data UnfoldingSource
  = -- See also Note [Historical note: unfoldings for wrappers]
   
    InlineRhs          -- The current rhs of the function
    		       -- Replace uf_tmpl each time around

  | InlineStable       -- From an INLINE or INLINABLE pragma 
                       --   INLINE     if guidance is UnfWhen
                       --   INLINABLE  if guidance is UnfIfGoodArgs/UnfoldNever
                       -- (well, technically an INLINABLE might be made
                       -- UnfWhen if it was small enough, and then
                       -- it will behave like INLINE outside the current
                       -- module, but that is the way automatic unfoldings
                       -- work so it is consistent with the intended
                       -- meaning of INLINABLE).
                       --
    		       -- uf_tmpl may change, but only as a result of
                       -- gentle simplification, it doesn't get updated
                       -- to the current RHS during compilation as with
                       -- InlineRhs.
                       --
    		       -- See Note [InlineRules]

  | InlineCompulsory   -- Something that *has* no binding, so you *must* inline it
    		       -- Only a few primop-like things have this property 
                       -- (see MkId.lhs, calls to mkCompulsoryUnfolding).
                       -- Inline absolutely always, however boring the context.



-- | 'UnfoldingGuidance' says when unfolding should take place
data UnfoldingGuidance
  = UnfWhen {	-- Inline without thinking about the *size* of the uf_tmpl
    		-- Used (a) for small *and* cheap unfoldings
 		--      (b) for INLINE functions 
                -- See Note [INLINE for small functions] in CoreUnfold
      ug_unsat_ok  :: Bool,	-- True <=> ok to inline even if unsaturated
      ug_boring_ok :: Bool      -- True <=> ok to inline even if the context is boring
      		-- So True,True means "always"
    }

  | UnfIfGoodArgs {	-- Arose from a normal Id; the info here is the
    		     	-- result of a simple analysis of the RHS

      ug_args ::  [Int],  -- Discount if the argument is evaluated.
			  -- (i.e., a simplification will definitely
			  -- be possible).  One elt of the list per *value* arg.

      ug_size :: Int,	  -- The "size" of the unfolding.

      ug_res :: Int	  -- Scrutinee discount: the discount to substract if the thing is in
    }			  -- a context (case (thing args) of ...),
			  -- (where there are the right number of arguments.)

  | UnfNever	    -- The RHS is big, so don't inline it
\end{code}

Note [Historical note: unfoldings for wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to have a nice clever scheme in interface files for
wrappers. A wrapper's unfolding can be reconstructed from its worker's
id and its strictness. This decreased .hi file size (sometimes
significantly, for modules like GHC.Classes with many high-arity w/w
splits) and had a slight corresponding effect on compile times.

However, when we added the second demand analysis, this scheme lead to
some Core lint errors. The second analysis could change the strictness
signatures, which sometimes resulted in a wrapper's regenerated
unfolding applying the wrapper to too many arguments.

Instead of repairing the clever .hi scheme, we abandoned it in favor
of simplicity. The .hi sizes are usually insignificant (excluding the
+1M for base libraries), and compile time barely increases (~+1% for
nofib). The nicer upshot is that the UnfoldingSource no longer mentions
an Id, so, eg, substitutions need not traverse them.


Note [DFun unfoldings]
~~~~~~~~~~~~~~~~~~~~~~
The Arity in a DFunUnfolding is total number of args (type and value)
that the DFun needs to produce a dictionary.  That's not necessarily 
related to the ordinary arity of the dfun Id, esp if the class has
one method, so the dictionary is represented by a newtype.  Example

     class C a where { op :: a -> Int }
     instance C a -> C [a] where op xs = op (head xs)

The instance translates to

     $dfCList :: forall a. C a => C [a]  -- Arity 2!
     $dfCList = /\a.\d. $copList {a} d |> co
 
     $copList :: forall a. C a => [a] -> Int  -- Arity 2!
     $copList = /\a.\d.\xs. op {a} d (head xs)

Now we might encounter (op (dfCList {ty} d) a1 a2)
and we want the (op (dfList {ty} d)) rule to fire, because $dfCList
has all its arguments, even though its (value) arity is 2.  That's
why we record the number of expected arguments in the DFunUnfolding.

Note that although it's an Arity, it's most convenient for it to give
the *total* number of arguments, both type and value.  See the use
site in exprIsConApp_maybe.

\begin{code}
-- Constants for the UnfWhen constructor
needSaturated, unSaturatedOk :: Bool
needSaturated = False
unSaturatedOk = True

boringCxtNotOk, boringCxtOk :: Bool
boringCxtOk    = True
boringCxtNotOk = False

------------------------------------------------
noUnfolding :: Unfolding
-- ^ There is no known 'Unfolding'
evaldUnfolding :: Unfolding
-- ^ This unfolding marks the associated thing as being evaluated

noUnfolding    = NoUnfolding
evaldUnfolding = OtherCon []

mkOtherCon :: [AltCon] -> Unfolding
mkOtherCon = OtherCon

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding { uf_tmpl = e, uf_is_top = top, 
		uf_is_value = b1, uf_is_work_free = b2, 
	   	uf_expandable = b3, uf_is_conlike = b4,
                uf_arity = a, uf_guidance = g})
  = seqExpr e `seq` top `seq` b1 `seq` a `seq` b2 `seq` b3 `seq` b4 `seq` seqGuidance g

seqUnfolding _ = ()

seqGuidance :: UnfoldingGuidance -> ()
seqGuidance (UnfIfGoodArgs ns n b) = n `seq` sum ns `seq` b `seq` ()
seqGuidance _                      = ()
\end{code}

\begin{code}
isStableSource :: UnfoldingSource -> Bool
-- Keep the unfolding template
isStableSource InlineCompulsory   = True
isStableSource InlineStable       = True
isStableSource InlineRhs          = False

-- | Retrieves the template of an unfolding: panics if none is known
unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate = uf_tmpl

setUnfoldingTemplate :: Unfolding -> CoreExpr -> Unfolding
setUnfoldingTemplate unf rhs = unf { uf_tmpl = rhs }

-- | Retrieves the template of an unfolding if possible
maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding { uf_tmpl = expr })       = Just expr
maybeUnfoldingTemplate _                            		= Nothing

-- | The constructors that the unfolding could never be: 
-- returns @[]@ if no information is available
otherCons :: Unfolding -> [AltCon]
otherCons (OtherCon cons) = cons
otherCons _               = []

-- | Determines if it is certainly the case that the unfolding will
-- yield a value (something in HNF): returns @False@ if unsure
isValueUnfolding :: Unfolding -> Bool
	-- Returns False for OtherCon
isValueUnfolding (CoreUnfolding { uf_is_value = is_evald }) = is_evald
isValueUnfolding _                                          = False

-- | Determines if it possibly the case that the unfolding will
-- yield a value. Unlike 'isValueUnfolding' it returns @True@
-- for 'OtherCon'
isEvaldUnfolding :: Unfolding -> Bool
	-- Returns True for OtherCon
isEvaldUnfolding (OtherCon _)		                    = True
isEvaldUnfolding (CoreUnfolding { uf_is_value = is_evald }) = is_evald
isEvaldUnfolding _                                          = False

-- | @True@ if the unfolding is a constructor application, the application
-- of a CONLIKE function or 'OtherCon'
isConLikeUnfolding :: Unfolding -> Bool
isConLikeUnfolding (OtherCon _)                             = True
isConLikeUnfolding (CoreUnfolding { uf_is_conlike = con })  = con
isConLikeUnfolding _                                        = False

-- | Is the thing we will unfold into certainly cheap?
isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding { uf_is_work_free = is_wf }) = is_wf
isCheapUnfolding _                                           = False

isExpandableUnfolding :: Unfolding -> Bool
isExpandableUnfolding (CoreUnfolding { uf_expandable = is_expable }) = is_expable
isExpandableUnfolding _                                              = False

expandUnfolding_maybe :: Unfolding -> Maybe CoreExpr
-- Expand an expandable unfolding; this is used in rule matching 
--   See Note [Expanding variables] in Rules.lhs
-- The key point here is that CONLIKE things can be expanded
expandUnfolding_maybe (CoreUnfolding { uf_expandable = True, uf_tmpl = rhs }) = Just rhs
expandUnfolding_maybe _                                                       = Nothing

isStableCoreUnfolding_maybe :: Unfolding -> Maybe UnfoldingSource
isStableCoreUnfolding_maybe (CoreUnfolding { uf_src = src })
   | isStableSource src   = Just src
isStableCoreUnfolding_maybe _ = Nothing

isCompulsoryUnfolding :: Unfolding -> Bool
isCompulsoryUnfolding (CoreUnfolding { uf_src = InlineCompulsory }) = True
isCompulsoryUnfolding _                                             = False

isStableUnfolding :: Unfolding -> Bool
-- True of unfoldings that should not be overwritten 
-- by a CoreUnfolding for the RHS of a let-binding
isStableUnfolding (CoreUnfolding { uf_src = src }) = isStableSource src
isStableUnfolding (DFunUnfolding {})		   = True
isStableUnfolding _                                = False

isUnstableUnfolding :: Unfolding -> Bool
isUnstableUnfolding (CoreUnfolding { uf_src = src }) = not (isStableSource src)
isUnstableUnfolding _                                = False

unfoldingArity :: Unfolding -> Arity
unfoldingArity (CoreUnfolding { uf_arity = arity }) = arity
unfoldingArity _	      		   	    = panic "unfoldingArity"

isClosedUnfolding :: Unfolding -> Bool		-- No free variables
isClosedUnfolding (CoreUnfolding {}) = False
isClosedUnfolding (DFunUnfolding {}) = False
isClosedUnfolding _                  = True

-- | Only returns False if there is no unfolding information available at all
hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding = False
hasSomeUnfolding _           = True

neverUnfoldGuidance :: UnfoldingGuidance -> Bool
neverUnfoldGuidance UnfNever = True
neverUnfoldGuidance _        = False

canUnfold :: Unfolding -> Bool
canUnfold (CoreUnfolding { uf_guidance = g }) = not (neverUnfoldGuidance g)
canUnfold _  				      = False
\end{code}

Note [InlineRules]
~~~~~~~~~~~~~~~~~
When you say 
      {-# INLINE f #-}
      f x = <rhs>
you intend that calls (f e) are replaced by <rhs>[e/x] So we
should capture (\x.<rhs>) in the Unfolding of 'f', and never meddle
with it.  Meanwhile, we can optimise <rhs> to our heart's content,
leaving the original unfolding intact in Unfolding of 'f'. For example
	all xs = foldr (&&) True xs
	any p = all . map p  {-# INLINE any #-}
We optimise any's RHS fully, but leave the InlineRule saying "all . map p",
which deforests well at the call site.

So INLINE pragma gives rise to an InlineRule, which captures the original RHS.

Moreover, it's only used when 'f' is applied to the
specified number of arguments; that is, the number of argument on 
the LHS of the '=' sign in the original source definition. 
For example, (.) is now defined in the libraries like this
   {-# INLINE (.) #-}
   (.) f g = \x -> f (g x)
so that it'll inline when applied to two arguments. If 'x' appeared
on the left, thus
   (.) f g x = f (g x)
it'd only inline when applied to three arguments.  This slightly-experimental
change was requested by Roman, but it seems to make sense.

See also Note [Inlining an InlineRule] in CoreUnfold.


Note [OccInfo in unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In unfoldings and rules, we guarantee that the template is occ-analysed,
so that the occurrence info on the binders is correct.  This is important,
because the Simplifier does not re-analyse the template when using it. If
the occurrence info is wrong
  - We may get more simpifier iterations than necessary, because
    once-occ info isn't there
  - More seriously, we may get an infinite loop if there's a Rec
    without a loop breaker marked


%************************************************************************
%*									*
                  AltCon
%*									*
%************************************************************************

\begin{code}
-- The Ord is needed for the FiniteMap used in the lookForConstructor
-- in SimplEnv.  If you declared that lookForConstructor *ignores*
-- constructor-applications with LitArg args, then you could get
-- rid of this Ord.

instance Outputable AltCon where
  ppr (DataAlt dc) = ppr dc
  ppr (LitAlt lit) = ppr lit
  ppr DEFAULT      = ptext (sLit "__DEFAULT")

cmpAlt :: (AltCon, a, b) -> (AltCon, a, b) -> Ordering
cmpAlt (con1, _, _) (con2, _, _) = con1 `cmpAltCon` con2

ltAlt :: (AltCon, a, b) -> (AltCon, a, b) -> Bool
ltAlt a1 a2 = (a1 `cmpAlt` a2) == LT

cmpAltCon :: AltCon -> AltCon -> Ordering
-- ^ Compares 'AltCon's within a single list of alternatives
cmpAltCon DEFAULT      DEFAULT	   = EQ
cmpAltCon DEFAULT      _           = LT

cmpAltCon (DataAlt d1) (DataAlt d2) = dataConTag d1 `compare` dataConTag d2
cmpAltCon (DataAlt _)  DEFAULT      = GT
cmpAltCon (LitAlt  l1) (LitAlt  l2) = l1 `compare` l2
cmpAltCon (LitAlt _)   DEFAULT      = GT

cmpAltCon con1 con2 = WARN( True, text "Comparing incomparable AltCons" <+> 
			 	  ppr con1 <+> ppr con2 )
		      LT
\end{code}

%************************************************************************
%*									*
\subsection{Useful synonyms}
%*									*
%************************************************************************

Note [CoreProgram]
~~~~~~~~~~~~~~~~~~
The top level bindings of a program, a CoreProgram, are represented as
a list of CoreBind

 * Later bindings in the list can refer to earlier ones, but not vice
   versa.  So this is OK
      NonRec { x = 4 }
      Rec { p = ...q...x...
          ; q = ...p...x }
      Rec { f = ...p..x..f.. }
      NonRec { g = ..f..q...x.. }
   But it would NOT be ok for 'f' to refer to 'g'.

 * The occurrence analyser does strongly-connected component analysis
   on each Rec binding, and splits it into a sequence of smaller
   bindings where possible.  So the program typically starts life as a
   single giant Rec, which is then dependency-analysed into smaller
   chunks.  

\begin{code}

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
type CoreProgram = [CoreBind]	-- See Note [CoreProgram]

-- | The common case for the type of binders and variables when
-- we are manipulating the Core language within GHC
type CoreBndr = Var
-- | Expressions where binders are 'CoreBndr's
type CoreExpr = Expr CoreBndr
-- | Argument expressions where binders are 'CoreBndr's
type CoreArg  = Arg  CoreBndr
-- | Binding groups where binders are 'CoreBndr's
type CoreBind = Bind CoreBndr
-- | Case alternatives where binders are 'CoreBndr's
type CoreAlt  = Alt  CoreBndr
\end{code}

%************************************************************************
%*									*
\subsection{Tagging}
%*									*
%************************************************************************

\begin{code}
-- | Binders are /tagged/ with a t
data TaggedBndr t = TB CoreBndr t	-- TB for "tagged binder"

type TaggedBind t = Bind (TaggedBndr t)
type TaggedExpr t = Expr (TaggedBndr t)
type TaggedArg  t = Arg  (TaggedBndr t)
type TaggedAlt  t = Alt  (TaggedBndr t)

instance Outputable b => Outputable (TaggedBndr b) where
  ppr (TB b l) = char '<' <> ppr b <> comma <> ppr l <> char '>'

instance Outputable b => OutputableBndr (TaggedBndr b) where
  pprBndr _ b = ppr b	-- Simple
  pprInfixOcc  b = ppr b
  pprPrefixOcc b = ppr b

deTagExpr :: TaggedExpr t -> CoreExpr
deTagExpr (Var v)                   = Var v
deTagExpr (Lit l)                   = Lit l
deTagExpr (Type ty)                 = Type ty
deTagExpr (Coercion co)             = Coercion co
deTagExpr (App e1 e2)               = App (deTagExpr e1) (deTagExpr e2)
deTagExpr (Lam (TB b _) e)          = Lam b (deTagExpr e)
deTagExpr (Let bind body)           = Let (deTagBind bind) (deTagExpr body)
deTagExpr (Case e (TB b _) ty alts) = Case (deTagExpr e) b ty (map deTagAlt alts)
deTagExpr (Tick t e)                = Tick t (deTagExpr e)
deTagExpr (Cast e co)               = Cast (deTagExpr e) co

deTagBind :: TaggedBind t -> CoreBind
deTagBind (NonRec (TB b _) rhs) = NonRec b (deTagExpr rhs)
deTagBind (Rec prs)             = Rec [(b, deTagExpr rhs) | (TB b _, rhs) <- prs]

deTagAlt :: TaggedAlt t -> CoreAlt
deTagAlt (con, bndrs, rhs) = (con, [b | TB b _ <- bndrs], deTagExpr rhs)
\end{code}


%************************************************************************
%*									*
\subsection{Core-constructing functions with checking}
%*									*
%************************************************************************

\begin{code}
-- | Apply a list of argument expressions to a function expression in a nested fashion. Prefer to
-- use 'MkCore.mkCoreApps' if possible
mkApps    :: Expr b -> [Arg b]  -> Expr b
-- | Apply a list of type argument expressions to a function expression in a nested fashion
mkTyApps  :: Expr b -> [Type]   -> Expr b
-- | Apply a list of coercion argument expressions to a function expression in a nested fashion
mkCoApps  :: Expr b -> [Coercion] -> Expr b
-- | Apply a list of type or value variables to a function expression in a nested fashion
mkVarApps :: Expr b -> [Var] -> Expr b
-- | Apply a list of argument expressions to a data constructor in a nested fashion. Prefer to
-- use 'MkCore.mkCoreConApps' if possible
mkConApp      :: DataCon -> [Arg b] -> Expr b

mkApps    f args = foldl App		  	   f args
mkTyApps  f args = foldl (\ e a -> App e (Type a)) f args
mkCoApps  f args = foldl (\ e a -> App e (Coercion a)) f args
mkVarApps f vars = foldl (\ e a -> App e (varToCoreExpr a)) f vars
mkConApp con args = mkApps (Var (dataConWorkId con)) args

mkConApp2 :: DataCon -> [Type] -> [Var] -> Expr b
mkConApp2 con tys arg_ids = Var (dataConWorkId con) 
                            `mkApps` map Type tys
                            `mkApps` map varToCoreExpr arg_ids


-- | Create a machine integer literal expression of type @Int#@ from an @Integer@.
-- If you want an expression of type @Int@ use 'MkCore.mkIntExpr'
mkIntLit      :: DynFlags -> Integer -> Expr b
-- | Create a machine integer literal expression of type @Int#@ from an @Int@.
-- If you want an expression of type @Int@ use 'MkCore.mkIntExpr'
mkIntLitInt   :: DynFlags -> Int     -> Expr b

mkIntLit    dflags n = Lit (mkMachInt dflags n)
mkIntLitInt dflags n = Lit (mkMachInt dflags (toInteger n))

-- | Create a machine word literal expression of type  @Word#@ from an @Integer@.
-- If you want an expression of type @Word@ use 'MkCore.mkWordExpr'
mkWordLit     :: DynFlags -> Integer -> Expr b
-- | Create a machine word literal expression of type  @Word#@ from a @Word@.
-- If you want an expression of type @Word@ use 'MkCore.mkWordExpr'
mkWordLitWord :: DynFlags -> Word -> Expr b

mkWordLit     dflags w = Lit (mkMachWord dflags w)
mkWordLitWord dflags w = Lit (mkMachWord dflags (toInteger w))

mkWord64LitWord64 :: Word64 -> Expr b
mkWord64LitWord64 w = Lit (mkMachWord64 (toInteger w))

mkInt64LitInt64 :: Int64 -> Expr b
mkInt64LitInt64 w = Lit (mkMachInt64 (toInteger w))

-- | Create a machine character literal expression of type @Char#@.
-- If you want an expression of type @Char@ use 'MkCore.mkCharExpr'
mkCharLit :: Char -> Expr b
-- | Create a machine string literal expression of type @Addr#@.
-- If you want an expression of type @String@ use 'MkCore.mkStringExpr'
mkStringLit :: String -> Expr b

mkCharLit   c = Lit (mkMachChar c)
mkStringLit s = Lit (mkMachString s)

-- | Create a machine single precision literal expression of type @Float#@ from a @Rational@.
-- If you want an expression of type @Float@ use 'MkCore.mkFloatExpr'
mkFloatLit :: Rational -> Expr b
-- | Create a machine single precision literal expression of type @Float#@ from a @Float@.
-- If you want an expression of type @Float@ use 'MkCore.mkFloatExpr'
mkFloatLitFloat :: Float -> Expr b

mkFloatLit      f = Lit (mkMachFloat f)
mkFloatLitFloat f = Lit (mkMachFloat (toRational f))

-- | Create a machine double precision literal expression of type @Double#@ from a @Rational@.
-- If you want an expression of type @Double@ use 'MkCore.mkDoubleExpr'
mkDoubleLit :: Rational -> Expr b
-- | Create a machine double precision literal expression of type @Double#@ from a @Double@.
-- If you want an expression of type @Double@ use 'MkCore.mkDoubleExpr'
mkDoubleLitDouble :: Double -> Expr b

mkDoubleLit       d = Lit (mkMachDouble d)
mkDoubleLitDouble d = Lit (mkMachDouble (toRational d))

-- | Bind all supplied binding groups over an expression in a nested let expression. Prefer to
-- use 'MkCore.mkCoreLets' if possible
mkLets	      :: [Bind b] -> Expr b -> Expr b
-- | Bind all supplied binders over an expression in a nested lambda expression. Prefer to
-- use 'MkCore.mkCoreLams' if possible
mkLams	      :: [b] -> Expr b -> Expr b

mkLams binders body = foldr Lam body binders
mkLets binds body   = foldr Let body binds


-- | Create a binding group where a type variable is bound to a type. Per "CoreSyn#type_let",
-- this can only be used to bind something in a non-recursive @let@ expression
mkTyBind :: TyVar -> Type -> CoreBind
mkTyBind tv ty      = NonRec tv (Type ty)

-- | Create a binding group where a type variable is bound to a type. Per "CoreSyn#type_let",
-- this can only be used to bind something in a non-recursive @let@ expression
mkCoBind :: CoVar -> Coercion -> CoreBind
mkCoBind cv co      = NonRec cv (Coercion co)

-- | Convert a binder into either a 'Var' or 'Type' 'Expr' appropriately
varToCoreExpr :: CoreBndr -> Expr b
varToCoreExpr v | isTyVar v = Type (mkTyVarTy v)
                | isCoVar v = Coercion (mkCoVarCo v)
                | otherwise = ASSERT( isId v ) Var v

varsToCoreExprs :: [CoreBndr] -> [Expr b]
varsToCoreExprs vs = map varToCoreExpr vs
\end{code}


%************************************************************************
%*									*
\subsection{Simple access functions}
%*									*
%************************************************************************

\begin{code}
-- | Extract every variable by this group
bindersOf  :: Bind b -> [b]
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
bindersOf (NonRec binder _) = [binder]
bindersOf (Rec pairs)       = [binder | (binder, _) <- pairs]

-- | 'bindersOf' applied to a list of binding groups
bindersOfBinds :: [Bind b] -> [b]
bindersOfBinds binds = foldr ((++) . bindersOf) [] binds

rhssOfBind :: Bind b -> [Expr b]
rhssOfBind (NonRec _ rhs) = [rhs]
rhssOfBind (Rec pairs)    = [rhs | (_,rhs) <- pairs]

rhssOfAlts :: [Alt b] -> [Expr b]
rhssOfAlts alts = [e | (_,_,e) <- alts]

-- | Collapse all the bindings in the supplied groups into a single
-- list of lhs\/rhs pairs suitable for binding in a 'Rec' binding group
flattenBinds :: [Bind b] -> [(b, Expr b)]
flattenBinds (NonRec b r : binds) = (b,r) : flattenBinds binds
flattenBinds (Rec prs1   : binds) = prs1 ++ flattenBinds binds
flattenBinds []			  = []
\end{code}

\begin{code}
-- | We often want to strip off leading lambdas before getting down to
-- business. This function is your friend.
collectBinders	             :: Expr b -> ([b],         Expr b)
-- | Collect as many type bindings as possible from the front of a nested lambda
collectTyBinders       	     :: CoreExpr -> ([TyVar],     CoreExpr)
-- | Collect as many value bindings as possible from the front of a nested lambda
collectValBinders      	     :: CoreExpr -> ([Id],        CoreExpr)
-- | Collect type binders from the front of the lambda first, 
-- then follow up by collecting as many value bindings as possible
-- from the resulting stripped expression
collectTyAndValBinders 	     :: CoreExpr -> ([TyVar], [Id], CoreExpr)

collectBinders expr
  = go [] expr
  where
    go bs (Lam b e) = go (b:bs) e
    go bs e	     = (reverse bs, e)

collectTyAndValBinders expr
  = (tvs, ids, body)
  where
    (tvs, body1) = collectTyBinders expr
    (ids, body)  = collectValBinders body1

collectTyBinders expr
  = go [] expr
  where
    go tvs (Lam b e) | isTyVar b = go (b:tvs) e
    go tvs e			 = (reverse tvs, e)

collectValBinders expr
  = go [] expr
  where
    go ids (Lam b e) | isId b = go (b:ids) e
    go ids body		      = (reverse ids, body)
\end{code}

\begin{code}
-- | Takes a nested application expression and returns the the function
-- being applied and the arguments to which it is applied
collectArgs :: Expr b -> (Expr b, [Arg b])
collectArgs expr
  = go expr []
  where
    go (App f a) as = go f (a:as)
    go e 	 as = (e, as)
\end{code}

%************************************************************************
%*									*
\subsection{Predicates}
%*									*
%************************************************************************

At one time we optionally carried type arguments through to runtime.
@isRuntimeVar v@ returns if (Lam v _) really becomes a lambda at runtime,
i.e. if type applications are actual lambdas because types are kept around
at runtime.  Similarly isRuntimeArg.  

\begin{code}
-- | Will this variable exist at runtime?
isRuntimeVar :: Var -> Bool
isRuntimeVar = isId 

-- | Will this argument expression exist at runtime?
isRuntimeArg :: CoreExpr -> Bool
isRuntimeArg = isValArg

-- | Returns @True@ for value arguments, false for type args
-- NB: coercions are value arguments (zero width, to be sure,
-- like State#, but still value args).
isValArg :: Expr b -> Bool
isValArg e = not (isTypeArg e)

-- | Returns @True@ iff the expression is a 'Type' or 'Coercion'
-- expression at its top level
isTyCoArg :: Expr b -> Bool
isTyCoArg (Type {})     = True
isTyCoArg (Coercion {}) = True
isTyCoArg _             = False

-- | Returns @True@ iff the expression is a 'Type' expression at its
-- top level.  Note this does NOT include 'Coercion's.
isTypeArg :: Expr b -> Bool
isTypeArg (Type {}) = True
isTypeArg _         = False

-- | The number of binders that bind values rather than types
valBndrCount :: [CoreBndr] -> Int
valBndrCount = count isId

-- | The number of argument expressions that are values rather than types at their top level
valArgCount :: [Arg b] -> Int
valArgCount = count isValArg
\end{code}


%************************************************************************
%*									*
\subsection{Seq stuff}
%*									*
%************************************************************************

\begin{code}
seqExpr :: CoreExpr -> ()
seqExpr (Var v)         = v `seq` ()
seqExpr (Lit lit)       = lit `seq` ()
seqExpr (App f a)       = seqExpr f `seq` seqExpr a
seqExpr (Lam b e)       = seqBndr b `seq` seqExpr e
seqExpr (Let b e)       = seqBind b `seq` seqExpr e
seqExpr (Case e b t as) = seqExpr e `seq` seqBndr b `seq` seqType t `seq` seqAlts as
seqExpr (Cast e co)     = seqExpr e `seq` seqCo co
seqExpr (Tick n e)    = seqTickish n `seq` seqExpr e
seqExpr (Type t)       = seqType t
seqExpr (Coercion co)   = seqCo co

seqExprs :: [CoreExpr] -> ()
seqExprs [] = ()
seqExprs (e:es) = seqExpr e `seq` seqExprs es

seqTickish :: Tickish Id -> ()
seqTickish ProfNote{ profNoteCC = cc } = cc `seq` ()
seqTickish HpcTick{} = ()
seqTickish Breakpoint{ breakpointFVs = ids } = seqBndrs ids

seqBndr :: CoreBndr -> ()
seqBndr b = b `seq` ()

seqBndrs :: [CoreBndr] -> ()
seqBndrs [] = ()
seqBndrs (b:bs) = seqBndr b `seq` seqBndrs bs

seqBind :: Bind CoreBndr -> ()
seqBind (NonRec b e) = seqBndr b `seq` seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs :: [(CoreBndr, CoreExpr)] -> ()
seqPairs [] = ()
seqPairs ((b,e):prs) = seqBndr b `seq` seqExpr e `seq` seqPairs prs

seqAlts :: [CoreAlt] -> ()
seqAlts [] = ()
seqAlts ((c,bs,e):alts) = c `seq` seqBndrs bs `seq` seqExpr e `seq` seqAlts alts

seqRules :: [CoreRule] -> ()
seqRules [] = ()
seqRules (Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs } : rules) 
  = seqBndrs bndrs `seq` seqExprs (rhs:args) `seq` seqRules rules
seqRules (BuiltinRule {} : rules) = seqRules rules
\end{code}

%************************************************************************
%*									*
\subsection{Annotated core}
%*									*
%************************************************************************

\begin{code}
-- | Annotated core: allows annotation at every node in the tree
type AnnExpr bndr annot = (annot, AnnExpr' bndr annot)

-- | A clone of the 'Expr' type but allowing annotation at every tree node
data AnnExpr' bndr annot
  = AnnVar	Id
  | AnnLit	Literal
  | AnnLam	bndr (AnnExpr bndr annot)
  | AnnApp	(AnnExpr bndr annot) (AnnExpr bndr annot)
  | AnnCase	(AnnExpr bndr annot) bndr Type [AnnAlt bndr annot]
  | AnnLet	(AnnBind bndr annot) (AnnExpr bndr annot)
  | AnnCast     (AnnExpr bndr annot) (annot, Coercion)
    		   -- Put an annotation on the (root of) the coercion
  | AnnTick     (Tickish Id) (AnnExpr bndr annot)
  | AnnType	Type
  | AnnCoercion Coercion

-- | A clone of the 'Alt' type but allowing annotation at every tree node
type AnnAlt bndr annot = (AltCon, [bndr], AnnExpr bndr annot)

-- | A clone of the 'Bind' type but allowing annotation at every tree node
data AnnBind bndr annot
  = AnnNonRec bndr (AnnExpr bndr annot)
  | AnnRec    [(bndr, AnnExpr bndr annot)]
\end{code}

\begin{code}
-- | Takes a nested application expression and returns the the function
-- being applied and the arguments to which it is applied
collectAnnArgs :: AnnExpr b a -> (AnnExpr b a, [AnnExpr b a])
collectAnnArgs expr
  = go expr []
  where
    go (_, AnnApp f a) as = go f (a:as)
    go e 	       as = (e, as)
\end{code}

\begin{code}
deAnnotate :: AnnExpr bndr annot -> Expr bndr
deAnnotate (_, e) = deAnnotate' e

deAnnotate' :: AnnExpr' bndr annot -> Expr bndr
deAnnotate' (AnnType t)           = Type t
deAnnotate' (AnnCoercion co)      = Coercion co
deAnnotate' (AnnVar  v)           = Var v
deAnnotate' (AnnLit  lit)         = Lit lit
deAnnotate' (AnnLam  binder body) = Lam binder (deAnnotate body)
deAnnotate' (AnnApp  fun arg)     = App (deAnnotate fun) (deAnnotate arg)
deAnnotate' (AnnCast e (_,co))    = Cast (deAnnotate e) co
deAnnotate' (AnnTick tick body)   = Tick tick (deAnnotate body)

deAnnotate' (AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
  where
    deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
    deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

deAnnotate' (AnnCase scrut v t alts)
  = Case (deAnnotate scrut) v t (map deAnnAlt alts)

deAnnAlt :: AnnAlt bndr annot -> Alt bndr
deAnnAlt (con,args,rhs) = (con,args,deAnnotate rhs)
\end{code}

\begin{code}
-- | As 'collectBinders' but for 'AnnExpr' rather than 'Expr'
collectAnnBndrs :: AnnExpr bndr annot -> ([bndr], AnnExpr bndr annot)
collectAnnBndrs e
  = collect [] e
  where
    collect bs (_, AnnLam b body) = collect (b:bs) body
    collect bs body		  = (reverse bs, body)
\end{code}
