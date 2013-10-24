%
% (c) The University of Glasgow 2006
%

\begin{code}
-- | Module for (a) type kinds and (b) type coercions,
-- as used in System FC. See 'CoreSyn.Expr' for
-- more on System FC and how coercions fit into it.
--
module Coercion (
        -- * Main data type
        Coercion(..), Var, CoVar,
        LeftOrRight(..), pickLR,
        Role(..), ltRole,

        -- ** Functions over coercions
        coVarKind, coVarRole,
        coercionType, coercionKind, coercionKinds, isReflCo,
        isReflCo_maybe, coercionRole,
        mkCoercionType,

        -- ** Constructing coercions
        mkReflCo, mkCoVarCo,
        mkAxInstCo, mkUnbranchedAxInstCo, mkAxInstLHS, mkAxInstRHS,
        mkUnbranchedAxInstRHS,
        mkPiCo, mkPiCos, mkCoCast,
        mkSymCo, mkTransCo, mkNthCo, mkNthCoRole, mkLRCo,
        mkInstCo, mkAppCo, mkAppCoFlexible, mkTyConAppCo, mkFunCo,
        mkForAllCo, mkUnsafeCo, mkUnivCo, mkSubCo, mkPhantomCo,
        mkNewTypeCo, maybeSubCo, maybeSubCo2,
        mkAxiomRuleCo,

        -- ** Decomposition
        instNewTyCon_maybe,
        topNormaliseNewType_maybe,

        decomposeCo, getCoVar_maybe,
        splitAppCo_maybe,
        splitForAllCo_maybe,
        nthRole, tyConRolesX,
        tvUsedAtNominalRole, nextRole,

        -- ** Coercion variables
        mkCoVar, isCoVar, isCoVarType, coVarName, setCoVarName, setCoVarUnique,

        -- ** Free variables
        tyCoVarsOfCo, tyCoVarsOfCos, coVarsOfCo, coercionSize,

        -- ** Substitution
        CvSubstEnv, emptyCvSubstEnv,
        CvSubst(..), emptyCvSubst, Coercion.lookupTyVar, lookupCoVar,
        isEmptyCvSubst, zapCvSubstEnv, getCvInScope,
        substCo, substCos, substCoVar, substCoVars,
        substCoWithTy, substCoWithTys,
        cvTvSubst, tvCvSubst, mkCvSubst, zipOpenCvSubst,
        substTy, extendTvSubst,
        extendCvSubstAndInScope, extendTvSubstAndInScope,
        substTyVarBndr, substCoVarBndr,

        -- ** Lifting
        liftCoMatch, liftCoSubstTyVar, liftCoSubstWith,

        -- ** Comparison
        coreEqCoercion, coreEqCoercion2,

        -- ** Forcing evaluation of coercions
        seqCo,

        -- * Pretty-printing
        pprCo, pprParendCo,
        pprCoAxiom, pprCoAxBranch, pprCoAxBranchHdr,

        -- * Tidying
        tidyCo, tidyCos,

        -- * Other
        applyCo
       ) where

#include "HsVersions.h"

import Unify    ( MatchEnv(..), matchList )
import TypeRep
import qualified Type
import Type hiding( substTy, substTyVarBndr, extendTvSubst )
import TyCon
import CoAxiom
import Var
import VarEnv
import VarSet
import Binary
import Maybes   ( orElse )
import Name     ( Name, NamedThing(..), nameUnique, nameModule, getSrcSpan )
import OccName  ( parenSymOcc )
import Util
import BasicTypes
import Outputable
import Unique
import Pair
import SrcLoc
import PrelNames        ( funTyConKey, eqPrimTyConKey, eqReprPrimTyConKey )
import Control.Applicative
import Data.Traversable (traverse, sequenceA)
import FastString

import qualified Data.Data as Data hiding ( TyCon )
\end{code}

%************************************************************************
%*                                                                      *
            Coercions
%*                                                                      *
%************************************************************************

\begin{code}
-- | A 'Coercion' is concrete evidence of the equality/convertibility
-- of two types.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data Coercion
  -- Each constructor has a "role signature", indicating the way roles are
  -- propagated through coercions. P, N, and R stand for coercions of the
  -- given role. e stands for a coercion of a specific unknown role (think
  -- "role polymorphism"). "e" stands for an explicit role parameter
  -- indicating role e. _ stands for a parameter that is not a Role or
  -- Coercion.

  -- These ones mirror the shape of types
  = -- Refl :: "e" -> _ -> e
    Refl Role Type  -- See Note [Refl invariant]
          -- Invariant: applications of (Refl T) to a bunch of identity coercions
          --            always show up as Refl.
          -- For example  (Refl T) (Refl a) (Refl b) shows up as (Refl (T a b)).

          -- Applications of (Refl T) to some coercions, at least one of
          -- which is NOT the identity, show up as TyConAppCo.
          -- (They may not be fully saturated however.)
          -- ConAppCo coercions (like all coercions other than Refl)
          -- are NEVER the identity.

          -- Use (Refl Representational _), not (SubCo (Refl Nominal _))

  -- These ones simply lift the correspondingly-named
  -- Type constructors into Coercions

  -- TyConAppCo :: "e" -> _ -> ?? -> e
  -- See Note [TyConAppCo roles]
  | TyConAppCo Role TyCon [Coercion]    -- lift TyConApp
               -- The TyCon is never a synonym;
               -- we expand synonyms eagerly
               -- But it can be a type function

  | AppCo Coercion Coercion        -- lift AppTy
          -- AppCo :: e -> N -> e

  -- See Note [Forall coercions]
  | ForAllCo TyVar Coercion       -- forall a. g
         -- :: _ -> e -> e

  -- These are special
  | CoVarCo CoVar      -- :: _ -> (N or R)
                       -- result role depends on the tycon of the variable's type

    -- AxiomInstCo :: e -> _ -> [N] -> e
  | AxiomInstCo (CoAxiom Branched) BranchIndex [Coercion]
     -- See also [CoAxiom index]
     -- The coercion arguments always *precisely* saturate
     -- arity of (that branch of) the CoAxiom.  If there are
     -- any left over, we use AppCo.  See
     -- See [Coercion axioms applied to coercions]

         -- see Note [UnivCo]
  | UnivCo Role Type Type      -- :: "e" -> _ -> _ -> e
  | SymCo Coercion             -- :: e -> e
  | TransCo Coercion Coercion  -- :: e -> e -> e

    -- The number of types and coercions should match exactly the expectations
    -- of the CoAxiomRule (i.e., the rule is fully saturated).
  | AxiomRuleCo CoAxiomRule [Type] [Coercion]

  -- These are destructors

  | NthCo  Int         Coercion     -- Zero-indexed; decomposes (T t0 ... tn)
    -- :: _ -> e -> ?? (inverse of TyConAppCo, see Note [TyConAppCo roles])
  | LRCo   LeftOrRight Coercion     -- Decomposes (t_left t_right)
    -- :: _ -> N -> N
  | InstCo Coercion Type
    -- :: e -> _ -> e

  | SubCo Coercion                  -- Turns a ~N into a ~R
    -- :: N -> R
  deriving (Data.Data, Data.Typeable)

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data LeftOrRight = CLeft | CRight
                 deriving( Eq, Data.Data, Data.Typeable )

instance Binary LeftOrRight where
   put_ bh CLeft  = putByte bh 0
   put_ bh CRight = putByte bh 1

   get bh = do { h <- getByte bh
               ; case h of
                   0 -> return CLeft
                   _ -> return CRight }

pickLR :: LeftOrRight -> (a,a) -> a
pickLR CLeft  (l,_) = l
pickLR CRight (_,r) = r
\end{code}

Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~
Coercions have the following invariant
     Refl is always lifted as far as possible.

You might think that a consequencs is:
     Every identity coercions has Refl at the root

But that's not quite true because of coercion variables.  Consider
     g         where g :: Int~Int
     Left h    where h :: Maybe Int ~ Maybe Int
etc.  So the consequence is only true of coercions that
have no coercion variables.

Note [Coercion axioms applied to coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The reason coercion axioms can be applied to coercions and not just
types is to allow for better optimization.  There are some cases where
we need to be able to "push transitivity inside" an axiom in order to
expose further opportunities for optimization.

For example, suppose we have

  C a : t[a] ~ F a
  g   : b ~ c

and we want to optimize

  sym (C b) ; t[g] ; C c

which has the kind

  F b ~ F c

(stopping through t[b] and t[c] along the way).

We'd like to optimize this to just F g -- but how?  The key is
that we need to allow axioms to be instantiated by *coercions*,
not just by types.  Then we can (in certain cases) push
transitivity inside the axiom instantiations, and then react
opposite-polarity instantiations of the same axiom.  In this
case, e.g., we match t[g] against the LHS of (C c)'s kind, to
obtain the substitution  a |-> g  (note this operation is sort
of the dual of lifting!) and hence end up with

  C g : t[b] ~ F c

which indeed has the same kind as  t[g] ; C c.

Now we have

  sym (C b) ; C g

which can be optimized to F g.

Note [CoAxiom index]
~~~~~~~~~~~~~~~~~~~~
A CoAxiom has 1 or more branches. Each branch has contains a list
of the free type variables in that branch, the LHS type patterns,
and the RHS type for that branch. When we apply an axiom to a list
of coercions, we must choose which branch of the axiom we wish to
use, as the different branches may have different numbers of free
type variables. (The number of type patterns is always the same
among branches, but that doesn't quite concern us here.)

The Int in the AxiomInstCo constructor is the 0-indexed number
of the chosen branch.

Note [Forall coercions]
~~~~~~~~~~~~~~~~~~~~~~~
Constructing coercions between forall-types can be a bit tricky.
Currently, the situation is as follows:

  ForAllCo TyVar Coercion

represents a coercion between polymorphic types, with the rule

           v : k       g : t1 ~ t2
  ----------------------------------------------
  ForAllCo v g : (all v:k . t1) ~ (all v:k . t2)

Note that it's only necessary to coerce between polymorphic types
where the type variables have identical kinds, because equality on
kinds is trivial.

Note [Predicate coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   g :: a~b
How can we coerce between types
   ([c]~a) => [a] -> c
and
   ([c]~b) => [b] -> c
where the equality predicate *itself* differs?

Answer: we simply treat (~) as an ordinary type constructor, so these
types really look like

   ((~) [c] a) -> [a] -> c
   ((~) [c] b) -> [b] -> c

So the coercion between the two is obviously

   ((~) [c] g) -> [g] -> c

Another way to see this to say that we simply collapse predicates to
their representation type (see Type.coreView and Type.predTypeRep).

This collapse is done by mkPredCo; there is no PredCo constructor
in Coercion.  This is important because we need Nth to work on
predicates too:
    Nth 1 ((~) [c] g) = g
See Simplify.simplCoercionF, which generates such selections.

Note [Kind coercions]
~~~~~~~~~~~~~~~~~~~~~
Suppose T :: * -> *, and g :: A ~ B
Then the coercion
   TyConAppCo T [g]      T g : T A ~ T B

Now suppose S :: forall k. k -> *, and g :: A ~ B
Then the coercion
   TyConAppCo S [Refl *, g]   T <*> g : T * A ~ T * B

Notice that the arguments to TyConAppCo are coercions, but the first
represents a *kind* coercion. Now, we don't allow any non-trivial kind
coercions, so it's an invariant that any such kind coercions are Refl.
Lint checks this.

However it's inconvenient to insist that these kind coercions are always
*structurally* (Refl k), because the key function exprIsConApp_maybe
pushes coercions into constructor arguments, so
       C k ty e |> g
may turn into
       C (Nth 0 g) ....
Now (Nth 0 g) will optimise to Refl, but perhaps not instantly.

Note [Roles]
~~~~~~~~~~~~
Roles are a solution to the GeneralizedNewtypeDeriving problem, articulated
in Trac #1496. The full story is in docs/core-spec/core-spec.pdf. Also, see
http://ghc.haskell.org/trac/ghc/wiki/RolesImplementation

Here is one way to phrase the problem:

Given:
newtype Age = MkAge Int
type family F x
type instance F Age = Bool
type instance F Int = Char

This compiles down to:
axAge :: Age ~ Int
axF1 :: F Age ~ Bool
axF2 :: F Int ~ Char

Then, we can make:
(sym (axF1) ; F axAge ; axF2) :: Bool ~ Char

Yikes!

The solution is _roles_, as articulated in "Generative Type Abstraction and
Type-level Computation" (POPL 2010), available at
http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf

The specification for roles has evolved somewhat since that paper. For the
current full details, see the documentation in docs/core-spec. Here are some
highlights.

We label every equality with a notion of type equivalence, of which there are
three options: Nominal, Representational, and Phantom. A ground type is
nominally equivalent only with itself. A newtype (which is considered a ground
type in Haskell) is representationally equivalent to its representation.
Anything is "phantomly" equivalent to anything else. We use "N", "R", and "P"
to denote the equivalences.

The axioms above would be:
axAge :: Age ~R Int
axF1 :: F Age ~N Bool
axF2 :: F Age ~N Char

Then, because transitivity applies only to coercions proving the same notion
of equivalence, the above construction is impossible.

However, there is still an escape hatch: we know that any two types that are
nominally equivalent are representationally equivalent as well. This is what
the form SubCo proves -- it "demotes" a nominal equivalence into a
representational equivalence. So, it would seem the following is possible:

sub (sym axF1) ; F axAge ; sub axF2 :: Bool ~R Char   -- WRONG

What saves us here is that the arguments to a type function F, lifted into a
coercion, *must* prove nominal equivalence. So, (F axAge) is ill-formed, and
we are safe.

Roles are attached to parameters to TyCons. When lifting a TyCon into a
coercion (through TyConAppCo), we need to ensure that the arguments to the
TyCon respect their roles. For example:

data T a b = MkT a (F b)

If we know that a1 ~R a2, then we know (T a1 b) ~R (T a2 b). But, if we know
that b1 ~R b2, we know nothing about (T a b1) and (T a b2)! This is because
the type function F branches on b's *name*, not representation. So, we say
that 'a' has role Representational and 'b' has role Nominal. The third role,
Phantom, is for parameters not used in the type's definition. Given the
following definition

data Q a = MkQ Int

the Phantom role allows us to say that (Q Bool) ~R (Q Char), because we
can construct the coercion Bool ~P Char (using UnivCo).

See the paper cited above for more examples and information.

Note [UnivCo]
~~~~~~~~~~~~~
The UnivCo ("universal coercion") serves two rather separate functions:
 - the implementation for unsafeCoerce#
 - placeholder for phantom parameters in a TyConAppCo

At Representational, it asserts that two (possibly unrelated)
types have the same representation and can be casted to one another.
This form is necessary for unsafeCoerce#.

For optimisation purposes, it is convenient to allow UnivCo to appear
at Nominal role. If we have

data Foo a = MkFoo (F a)   -- F is a type family

and we want an unsafe coercion from Foo Int to Foo Bool, then it would
be nice to have (TyConAppCo Foo (UnivCo Nominal Int Bool)). So, we allow
Nominal UnivCo's.

At Phantom role, it is used as an argument to TyConAppCo in the place
of a phantom parameter (a type parameter unused in the type definition).

For example:

data Q a = MkQ Int

We want a coercion for (Q Bool) ~R (Q Char).

(TyConAppCo Representational Q [UnivCo Phantom Bool Char]) does the trick.

Note [TyConAppCo roles]
~~~~~~~~~~~~~~~~~~~~~~~
The TyConAppCo constructor has a role parameter, indicating the role at
which the coercion proves equality. The choice of this parameter affects
the required roles of the arguments of the TyConAppCo. To help explain
it, assume the following definition:

newtype Age = MkAge Int

Nominal: All arguments must have role Nominal. Why? So that Foo Age ~N Foo Int
does *not* hold.

Representational: All arguments must have the roles corresponding to the
result of tyConRoles on the TyCon. This is the whole point of having
roles on the TyCon to begin with. So, we can have Foo Age ~R Foo Int,
if Foo's parameter has role R.

If a Representational TyConAppCo is over-saturated (which is otherwise fine),
the spill-over arguments must all be at Nominal. This corresponds to the
behavior for AppCo.

Phantom: All arguments must have role Phantom. This one isn't strictly
necessary for soundness, but this choice removes ambiguity.



The rules here also dictate what the parameters to mkTyConAppCo.

%************************************************************************
%*                                                                      *
\subsection{Coercion variables}
%*                                                                      *
%************************************************************************

\begin{code}
coVarName :: CoVar -> Name
coVarName = varName

setCoVarUnique :: CoVar -> Unique -> CoVar
setCoVarUnique = setVarUnique

setCoVarName :: CoVar -> Name -> CoVar
setCoVarName   = setVarName

isCoVar :: Var -> Bool
isCoVar v = isCoVarType (varType v)

isCoVarType :: Type -> Bool
isCoVarType ty      -- Tests for t1 ~# t2, the unboxed equality
  = case splitTyConApp_maybe ty of
      Just (tc,tys) -> (tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey)
                       && tys `lengthAtLeast` 2
      Nothing       -> False
\end{code}


\begin{code}
tyCoVarsOfCo :: Coercion -> VarSet
-- Extracts type and coercion variables from a coercion
tyCoVarsOfCo (Refl _ ty)           = tyVarsOfType ty
tyCoVarsOfCo (TyConAppCo _ _ cos)  = tyCoVarsOfCos cos
tyCoVarsOfCo (AppCo co1 co2)       = tyCoVarsOfCo co1 `unionVarSet` tyCoVarsOfCo co2
tyCoVarsOfCo (ForAllCo tv co)      = tyCoVarsOfCo co `delVarSet` tv
tyCoVarsOfCo (CoVarCo v)           = unitVarSet v
tyCoVarsOfCo (AxiomInstCo _ _ cos) = tyCoVarsOfCos cos
tyCoVarsOfCo (UnivCo _ ty1 ty2)    = tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2
tyCoVarsOfCo (SymCo co)            = tyCoVarsOfCo co
tyCoVarsOfCo (TransCo co1 co2)     = tyCoVarsOfCo co1 `unionVarSet` tyCoVarsOfCo co2
tyCoVarsOfCo (NthCo _ co)          = tyCoVarsOfCo co
tyCoVarsOfCo (LRCo _ co)           = tyCoVarsOfCo co
tyCoVarsOfCo (InstCo co ty)        = tyCoVarsOfCo co `unionVarSet` tyVarsOfType ty
tyCoVarsOfCo (SubCo co)            = tyCoVarsOfCo co
tyCoVarsOfCo (AxiomRuleCo _ ts cs) = tyVarsOfTypes ts `unionVarSet` tyCoVarsOfCos cs

tyCoVarsOfCos :: [Coercion] -> VarSet
tyCoVarsOfCos cos = foldr (unionVarSet . tyCoVarsOfCo) emptyVarSet cos

coVarsOfCo :: Coercion -> VarSet
-- Extract *coerction* variables only.  Tiresome to repeat the code, but easy.
coVarsOfCo (Refl _ _)            = emptyVarSet
coVarsOfCo (TyConAppCo _ _ cos)  = coVarsOfCos cos
coVarsOfCo (AppCo co1 co2)       = coVarsOfCo co1 `unionVarSet` coVarsOfCo co2
coVarsOfCo (ForAllCo _ co)       = coVarsOfCo co
coVarsOfCo (CoVarCo v)           = unitVarSet v
coVarsOfCo (AxiomInstCo _ _ cos) = coVarsOfCos cos
coVarsOfCo (UnivCo _ _ _)        = emptyVarSet
coVarsOfCo (SymCo co)            = coVarsOfCo co
coVarsOfCo (TransCo co1 co2)     = coVarsOfCo co1 `unionVarSet` coVarsOfCo co2
coVarsOfCo (NthCo _ co)          = coVarsOfCo co
coVarsOfCo (LRCo _ co)           = coVarsOfCo co
coVarsOfCo (InstCo co _)         = coVarsOfCo co
coVarsOfCo (SubCo co)            = coVarsOfCo co
coVarsOfCo (AxiomRuleCo _ _ cos) = coVarsOfCos cos

coVarsOfCos :: [Coercion] -> VarSet
coVarsOfCos cos = foldr (unionVarSet . coVarsOfCo) emptyVarSet cos

coercionSize :: Coercion -> Int
coercionSize (Refl _ ty)           = typeSize ty
coercionSize (TyConAppCo _ _ cos)  = 1 + sum (map coercionSize cos)
coercionSize (AppCo co1 co2)       = coercionSize co1 + coercionSize co2
coercionSize (ForAllCo _ co)       = 1 + coercionSize co
coercionSize (CoVarCo _)           = 1
coercionSize (AxiomInstCo _ _ cos) = 1 + sum (map coercionSize cos)
coercionSize (UnivCo _ ty1 ty2)  = typeSize ty1 + typeSize ty2
coercionSize (SymCo co)            = 1 + coercionSize co
coercionSize (TransCo co1 co2)     = 1 + coercionSize co1 + coercionSize co2
coercionSize (NthCo _ co)          = 1 + coercionSize co
coercionSize (LRCo  _ co)          = 1 + coercionSize co
coercionSize (InstCo co ty)        = 1 + coercionSize co + typeSize ty
coercionSize (SubCo co)            = 1 + coercionSize co
coercionSize (AxiomRuleCo _ tys cos) = 1 + sum (map typeSize tys)
                                         + sum (map coercionSize cos)
\end{code}

%************************************************************************
%*                                                                      *
                            Tidying coercions
%*                                                                      *
%************************************************************************

\begin{code}
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo env@(_, subst) co
  = go co
  where
    go (Refl r ty)            = Refl r (tidyType env ty)
    go (TyConAppCo r tc cos)  = let args = map go cos
                                in args `seqList` TyConAppCo r tc args
    go (AppCo co1 co2)        = (AppCo $! go co1) $! go co2
    go (ForAllCo tv co)       = ForAllCo tvp $! (tidyCo envp co)
                                where
                                  (envp, tvp) = tidyTyVarBndr env tv
    go (CoVarCo cv)           = case lookupVarEnv subst cv of
                                  Nothing  -> CoVarCo cv
                                  Just cv' -> CoVarCo cv'
    go (AxiomInstCo con ind cos) = let args = tidyCos env cos
                                   in args `seqList` AxiomInstCo con ind args
    go (UnivCo r ty1 ty2)     = (UnivCo r $! tidyType env ty1) $! tidyType env ty2
    go (SymCo co)             = SymCo $! go co
    go (TransCo co1 co2)      = (TransCo $! go co1) $! go co2
    go (NthCo d co)           = NthCo d $! go co
    go (LRCo lr co)           = LRCo lr $! go co
    go (InstCo co ty)         = (InstCo $! go co) $! tidyType env ty
    go (SubCo co)             = SubCo $! go co

    go (AxiomRuleCo ax tys cos) = let tys1 = map (tidyType env) tys
                                      cos1 = tidyCos env cos
                                  in tys1 `seqList` cos1 `seqList`
                                     AxiomRuleCo ax tys1 cos1


tidyCos :: TidyEnv -> [Coercion] -> [Coercion]
tidyCos env = map (tidyCo env)
\end{code}

%************************************************************************
%*                                                                      *
                   Pretty-printing coercions
%*                                                                      *
%************************************************************************

@pprCo@ is the standard @Coercion@ printer; the overloaded @ppr@
function is defined to use this.  @pprParendCo@ is the same, except it
puts parens around the type, except for the atomic cases.
@pprParendCo@ works just by setting the initial context precedence
very high.

\begin{code}
instance Outputable Coercion where
  ppr = pprCo

pprCo, pprParendCo :: Coercion -> SDoc
pprCo       co = ppr_co TopPrec   co
pprParendCo co = ppr_co TyConPrec co

ppr_co :: Prec -> Coercion -> SDoc
ppr_co _ (Refl r ty) = angleBrackets (ppr ty) <> ppr_role r

ppr_co p co@(TyConAppCo _ tc [_,_])
  | tc `hasKey` funTyConKey = ppr_fun_co p co

ppr_co _ (TyConAppCo r tc cos)  = pprTcApp TyConPrec ppr_co tc cos <> ppr_role r
ppr_co p (AppCo co1 co2)        = maybeParen p TyConPrec $
                                  pprCo co1 <+> ppr_co TyConPrec co2
ppr_co p co@(ForAllCo {})       = ppr_forall_co p co
ppr_co _ (CoVarCo cv)           = parenSymOcc (getOccName cv) (ppr cv)
ppr_co p (AxiomInstCo con index cos)
  = pprPrefixApp p (ppr (getName con) <> brackets (ppr index))
                   (map (ppr_co TyConPrec) cos)

ppr_co p co@(TransCo {}) = maybeParen p FunPrec $
                           case trans_co_list co [] of
                             [] -> panic "ppr_co"
                             (co:cos) -> sep ( ppr_co FunPrec co
                                             : [ char ';' <+> ppr_co FunPrec co | co <- cos])
ppr_co p (InstCo co ty) = maybeParen p TyConPrec $
                          pprParendCo co <> ptext (sLit "@") <> pprType ty

ppr_co p (UnivCo r ty1 ty2) = pprPrefixApp p (ptext (sLit "UnivCo") <+> ppr r)
                                           [pprParendType ty1, pprParendType ty2]
ppr_co p (SymCo co)         = pprPrefixApp p (ptext (sLit "Sym")) [pprParendCo co]
ppr_co p (NthCo n co)       = pprPrefixApp p (ptext (sLit "Nth:") <> int n) [pprParendCo co]
ppr_co p (LRCo sel co)      = pprPrefixApp p (ppr sel) [pprParendCo co]
ppr_co p (SubCo co)         = pprPrefixApp p (ptext (sLit "Sub")) [pprParendCo co]
ppr_co p (AxiomRuleCo co ts cs) = maybeParen p TopPrec $
                                  ppr_axiom_rule_co co ts cs

ppr_axiom_rule_co :: CoAxiomRule -> [Type] -> [Coercion] -> SDoc
ppr_axiom_rule_co co ts ps = ppr (coaxrName co) <> ppTs ts $$ nest 2 (ppPs ps)
  where
  ppTs []   = Outputable.empty
  ppTs [t]  = ptext (sLit "@") <> ppr_type TopPrec t
  ppTs ts   = ptext (sLit "@") <>
                parens (hsep $ punctuate comma $ map pprType ts)

  ppPs []   = Outputable.empty
  ppPs [p]  = pprParendCo p
  ppPs (p : ps) = ptext (sLit "(") <+> pprCo p $$
                  vcat [ ptext (sLit ",") <+> pprCo q | q <- ps ] $$
                  ptext (sLit ")")



ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

trans_co_list :: Coercion -> [Coercion] -> [Coercion]
trans_co_list (TransCo co1 co2) cos = trans_co_list co1 (trans_co_list co2 cos)
trans_co_list co                cos = co : cos

instance Outputable LeftOrRight where
  ppr CLeft    = ptext (sLit "Left")
  ppr CRight   = ptext (sLit "Right")

ppr_fun_co :: Prec -> Coercion -> SDoc
ppr_fun_co p co = pprArrowChain p (split co)
  where
    split :: Coercion -> [SDoc]
    split (TyConAppCo _ f [arg,res])
      | f `hasKey` funTyConKey
      = ppr_co FunPrec arg : split res
    split co = [ppr_co TopPrec co]

ppr_forall_co :: Prec -> Coercion -> SDoc
ppr_forall_co p ty
  = maybeParen p FunPrec $
    sep [pprForAll tvs, ppr_co TopPrec rho]
  where
    (tvs,  rho) = split1 [] ty
    split1 tvs (ForAllCo tv ty) = split1 (tv:tvs) ty
    split1 tvs ty               = (reverse tvs, ty)
\end{code}

\begin{code}
pprCoAxiom :: CoAxiom br -> SDoc
pprCoAxiom ax@(CoAxiom { co_ax_tc = tc, co_ax_branches = branches })
  = hang (ptext (sLit "axiom") <+> ppr ax <+> dcolon)
       2 (vcat (map (pprCoAxBranch tc) $ fromBranchList branches))

pprCoAxBranch :: TyCon -> CoAxBranch -> SDoc
pprCoAxBranch fam_tc (CoAxBranch { cab_tvs = tvs
                                 , cab_lhs = lhs
                                 , cab_rhs = rhs })
  = hang (ifPprDebug (pprForAll tvs))
       2 (hang (pprTypeApp fam_tc lhs) 2 (equals <+> (ppr rhs)))

pprCoAxBranchHdr :: CoAxiom br -> BranchIndex -> SDoc
pprCoAxBranchHdr ax@(CoAxiom { co_ax_tc = fam_tc, co_ax_name = name }) index
  | CoAxBranch { cab_lhs = tys, cab_loc = loc } <- coAxiomNthBranch ax index
  = hang (pprTypeApp fam_tc tys)
       2 (ptext (sLit "-- Defined") <+> ppr_loc loc)
  where
        ppr_loc loc
          | isGoodSrcSpan loc
          = ptext (sLit "at") <+> ppr (srcSpanStart loc)

          | otherwise
          = ptext (sLit "in") <+>
              quotes (ppr (nameModule name))
\end{code}

%************************************************************************
%*                                                                      *
        Functions over Kinds
%*                                                                      *
%************************************************************************

\begin{code}
-- | This breaks a 'Coercion' with type @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c = [nth 0 c, nth 1 c, nth 2 c]
decomposeCo :: Arity -> Coercion -> [Coercion]
decomposeCo arity co
  = [mkNthCo n co | n <- [0..(arity-1)] ]
           -- Remember, Nth is zero-indexed

-- | Attempts to obtain the type variable underlying a 'Coercion'
getCoVar_maybe :: Coercion -> Maybe CoVar
getCoVar_maybe (CoVarCo cv) = Just cv
getCoVar_maybe _            = Nothing

-- first result has role equal to input; second result is Nominal
splitAppCo_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- ^ Attempt to take a coercion application apart.
splitAppCo_maybe (AppCo co1 co2) = Just (co1, co2)
splitAppCo_maybe (TyConAppCo r tc cos)
  | isDecomposableTyCon tc || cos `lengthExceeds` tyConArity tc
  , Just (cos', co') <- snocView cos
  , Just co'' <- unSubCo_maybe co'
  = Just (mkTyConAppCo r tc cos', co'') -- Never create unsaturated type family apps!
       -- Use mkTyConAppCo to preserve the invariant
       --  that identity coercions are always represented by Refl
splitAppCo_maybe (Refl r ty)
  | Just (ty1, ty2) <- splitAppTy_maybe ty
  = Just (Refl r ty1, Refl Nominal ty2)
splitAppCo_maybe _ = Nothing

splitForAllCo_maybe :: Coercion -> Maybe (TyVar, Coercion)
splitForAllCo_maybe (ForAllCo tv co) = Just (tv, co)
splitForAllCo_maybe _                = Nothing

-------------------------------------------------------
-- and some coercion kind stuff

coVarKind :: CoVar -> (Type,Type)
coVarKind cv
 | Just (tc, [_kind,ty1,ty2]) <- splitTyConApp_maybe (varType cv)
 = ASSERT(tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey)
   (ty1,ty2)
 | otherwise = panic "coVarKind, non coercion variable"

coVarRole :: CoVar -> Role
coVarRole cv
  | tc `hasKey` eqPrimTyConKey
  = Nominal
  | tc `hasKey` eqReprPrimTyConKey
  = Representational
  | otherwise
  = pprPanic "coVarRole: unknown tycon" (ppr cv)

  where
    tc = case tyConAppTyCon_maybe (varType cv) of
           Just tc0 -> tc0
           Nothing  -> pprPanic "coVarRole: not tyconapp" (ppr cv)

-- | Makes a coercion type from two types: the types whose equality
-- is proven by the relevant 'Coercion'
mkCoercionType :: Role -> Type -> Type -> Type
mkCoercionType Nominal          = mkPrimEqPred
mkCoercionType Representational = mkReprPrimEqPred
mkCoercionType Phantom          = panic "mkCoercionType"

isReflCo :: Coercion -> Bool
isReflCo (Refl {})         = True
isReflCo _                 = False

isReflCo_maybe :: Coercion -> Maybe Type
isReflCo_maybe (Refl _ ty)       = Just ty
isReflCo_maybe _                 = Nothing
\end{code}

%************************************************************************
%*                                                                      *
            Building coercions
%*                                                                      *
%************************************************************************

\begin{code}
mkCoVarCo :: CoVar -> Coercion
-- cv :: s ~# t
mkCoVarCo cv
  | ty1 `eqType` ty2 = Refl (coVarRole cv) ty1
  | otherwise        = CoVarCo cv
  where
    (ty1, ty2) = ASSERT( isCoVar cv ) coVarKind cv

mkReflCo :: Role -> Type -> Coercion
mkReflCo = Refl

mkAxInstCo :: Role -> CoAxiom br -> BranchIndex -> [Type] -> Coercion
-- mkAxInstCo can legitimately be called over-staturated;
-- i.e. with more type arguments than the coercion requires
mkAxInstCo role ax index tys
  | arity == n_tys = maybeSubCo2 role ax_role $ AxiomInstCo ax_br index rtys
  | otherwise      = ASSERT( arity < n_tys )
                     maybeSubCo2 role ax_role $
                     foldl AppCo (AxiomInstCo ax_br index (take arity rtys))
                                 (drop arity rtys)
  where
    n_tys     = length tys
    ax_br     = toBranchedAxiom ax
    branch    = coAxiomNthBranch ax_br index
    arity     = length $ coAxBranchTyVars branch
    arg_roles = coAxBranchRoles branch
    rtys      = zipWith mkReflCo (arg_roles ++ repeat Nominal) tys
    ax_role   = coAxiomRole ax

-- to be used only with unbranched axioms
mkUnbranchedAxInstCo :: Role -> CoAxiom Unbranched -> [Type] -> Coercion
mkUnbranchedAxInstCo role ax tys
  = mkAxInstCo role ax 0 tys

mkAxInstLHS, mkAxInstRHS :: CoAxiom br -> BranchIndex -> [Type] -> Type
-- Instantiate the axiom with specified types,
-- returning the instantiated RHS
-- A companion to mkAxInstCo:
--    mkAxInstRhs ax index tys = snd (coercionKind (mkAxInstCo ax index tys))
mkAxInstLHS ax index tys
  | CoAxBranch { cab_tvs = tvs, cab_lhs = lhs } <- coAxiomNthBranch ax index
  , (tys1, tys2) <- splitAtList tvs tys
  = ASSERT( tvs `equalLength` tys1 )
    mkTyConApp (coAxiomTyCon ax) (substTysWith tvs tys1 lhs ++ tys2)

mkAxInstRHS ax index tys
  | CoAxBranch { cab_tvs = tvs, cab_rhs = rhs } <- coAxiomNthBranch ax index
  , (tys1, tys2) <- splitAtList tvs tys
  = ASSERT( tvs `equalLength` tys1 )
    mkAppTys (substTyWith tvs tys1 rhs) tys2

mkUnbranchedAxInstRHS :: CoAxiom Unbranched -> [Type] -> Type
mkUnbranchedAxInstRHS ax = mkAxInstRHS ax 0

-- | Apply a 'Coercion' to another 'Coercion'.
-- The second coercion must be Nominal, unless the first is Phantom.
-- If the first is Phantom, then the second can be either Phantom or Nominal.
mkAppCo :: Coercion -> Coercion -> Coercion
mkAppCo co1 co2 = mkAppCoFlexible co1 Nominal co2
-- Note, mkAppCo is careful to maintain invariants regarding
-- where Refl constructors appear; see the comments in the definition
-- of Coercion and the Note [Refl invariant] in types/TypeRep.lhs.

-- | Apply a 'Coercion' to another 'Coercion'.
-- The second 'Coercion's role is given, making this more flexible than
-- 'mkAppCo'.
mkAppCoFlexible :: Coercion -> Role -> Coercion -> Coercion
mkAppCoFlexible (Refl r ty1) _ (Refl _ ty2)
  = Refl r (mkAppTy ty1 ty2)
mkAppCoFlexible (Refl r (TyConApp tc tys)) r2 co2
  = TyConAppCo r tc (zip_roles (tyConRolesX r tc) tys)
  where
    zip_roles (r1:_)  []        = [maybeSubCo2 r1 r2 co2]
    zip_roles (r1:rs) (ty1:tys) = mkReflCo r1 ty1 : zip_roles rs tys
    zip_roles _       _         = panic "zip_roles" -- but the roles are infinite...
mkAppCoFlexible (TyConAppCo r tc cos) r2 co
  = case r of
      Nominal          -> ASSERT( r2 == Nominal )
                          TyConAppCo Nominal tc (cos ++ [co])
      Representational -> TyConAppCo Representational tc (cos ++ [co'])
        where new_role = (tyConRolesX Representational tc) !! (length cos)
              co'      = maybeSubCo2 new_role r2 co
      Phantom          -> TyConAppCo Phantom tc (cos ++ [mkPhantomCo co])

mkAppCoFlexible co1 _r2 co2 = ASSERT( _r2 == Nominal )
                              AppCo co1 co2


-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCo'.
mkAppCos :: Coercion -> [Coercion] -> Coercion
mkAppCos co1 cos = foldl mkAppCo co1 cos

-- | Apply a type constructor to a list of coercions. It is the
-- caller's responsibility to get the roles correct on argument coercions.
mkTyConAppCo :: Role -> TyCon -> [Coercion] -> Coercion
mkTyConAppCo r tc cos
               -- Expand type synonyms
  | Just (tv_co_prs, rhs_ty, leftover_cos) <- tcExpandTyCon_maybe tc cos
  = mkAppCos (liftCoSubst r tv_co_prs rhs_ty) leftover_cos

  | Just tys <- traverse isReflCo_maybe cos
  = Refl r (mkTyConApp tc tys)  -- See Note [Refl invariant]

  | otherwise = TyConAppCo r tc cos

-- | Make a function 'Coercion' between two other 'Coercion's
mkFunCo :: Role -> Coercion -> Coercion -> Coercion
mkFunCo r co1 co2 = mkTyConAppCo r funTyCon [co1, co2]

-- | Make a 'Coercion' which binds a variable within an inner 'Coercion'
mkForAllCo :: Var -> Coercion -> Coercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkForAllCo tv (Refl r ty)  = ASSERT( isTyVar tv ) Refl r (mkForAllTy tv ty)
mkForAllCo tv  co          = ASSERT( isTyVar tv ) ForAllCo tv co

-------------------------------

-- | Create a symmetric version of the given 'Coercion' that asserts
--   equality between the same types but in the other "direction", so
--   a kind of @t1 ~ t2@ becomes the kind @t2 ~ t1@.
mkSymCo :: Coercion -> Coercion

-- Do a few simple optimizations, but don't bother pushing occurrences
-- of symmetry to the leaves; the optimizer will take care of that.
mkSymCo co@(Refl {})             = co
mkSymCo    (UnivCo r ty1 ty2)    = UnivCo r ty2 ty1
mkSymCo    (SymCo co)            = co
mkSymCo co                       = SymCo co

-- | Create a new 'Coercion' by composing the two given 'Coercion's transitively.
mkTransCo :: Coercion -> Coercion -> Coercion
mkTransCo (Refl {}) co = co
mkTransCo co (Refl {}) = co
mkTransCo co1 co2      = TransCo co1 co2

-- the Role is the desired one. It is the caller's responsibility to make
-- sure this request is reasonable
mkNthCoRole :: Role -> Int -> Coercion -> Coercion
mkNthCoRole role n co
  = maybeSubCo2 role nth_role $ nth_co
  where
    nth_co = mkNthCo n co
    nth_role = coercionRole nth_co

mkNthCo :: Int -> Coercion -> Coercion
mkNthCo n (Refl r ty) = ASSERT( ok_tc_app ty n )
                        Refl r' (tyConAppArgN n ty)
  where tc = tyConAppTyCon ty
        r' = nthRole r tc n
mkNthCo n co        = ASSERT( ok_tc_app _ty1 n && ok_tc_app _ty2 n )
                      NthCo n co
                    where
                      Pair _ty1 _ty2 = coercionKind co


mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkLRCo lr (Refl eq ty) = Refl eq (pickLR lr (splitAppTy ty))
mkLRCo lr co           = LRCo lr co

ok_tc_app :: Type -> Int -> Bool
ok_tc_app ty n = case splitTyConApp_maybe ty of
                   Just (_, tys) -> tys `lengthExceeds` n
                   Nothing       -> False

-- | Instantiates a 'Coercion' with a 'Type' argument.
mkInstCo :: Coercion -> Type -> Coercion
mkInstCo co ty = InstCo co ty

-- | Manufacture a coercion from thin air. Needless to say, this is
--   not usually safe, but it is used when we know we are dealing with
--   bottom, which is one case in which it is safe.  This is also used
--   to implement the @unsafeCoerce#@ primitive.  Optimise by pushing
--   down through type constructors.
mkUnsafeCo :: Type -> Type -> Coercion
mkUnsafeCo = mkUnivCo Representational

mkUnivCo :: Role -> Type -> Type -> Coercion
mkUnivCo role ty1 ty2
  | ty1 `eqType` ty2 = Refl role ty1
  | otherwise        = UnivCo role ty1 ty2

mkAxiomRuleCo :: CoAxiomRule -> [Type] -> [Coercion] -> Coercion
mkAxiomRuleCo = AxiomRuleCo

-- input coercion is Nominal
mkSubCo :: Coercion -> Coercion
mkSubCo (Refl Nominal ty) = Refl Representational ty
mkSubCo (TyConAppCo Nominal tc cos)
  = TyConAppCo Representational tc (applyRoles tc cos)
mkSubCo (UnivCo Nominal ty1 ty2) = UnivCo Representational ty1 ty2
mkSubCo co = ASSERT2( coercionRole co == Nominal, ppr co )
             SubCo co


-- takes a Nominal coercion and possibly casts it into a Representational one
maybeSubCo :: Role -> Coercion -> Coercion
maybeSubCo Nominal          = id
maybeSubCo Representational = mkSubCo
maybeSubCo Phantom          = pprPanic "maybeSubCo Phantom" . ppr

maybeSubCo2_maybe :: Role   -- desired role
                  -> Role   -- current role
                  -> Coercion -> Maybe Coercion
maybeSubCo2_maybe Representational Nominal = Just . mkSubCo
maybeSubCo2_maybe Nominal Representational = const Nothing
maybeSubCo2_maybe Phantom Phantom          = Just
maybeSubCo2_maybe Phantom _                = Just . mkPhantomCo
maybeSubCo2_maybe _ Phantom                = const Nothing
maybeSubCo2_maybe _ _                      = Just

maybeSubCo2 :: Role  -- desired role
            -> Role  -- current role
            -> Coercion -> Coercion
maybeSubCo2 r1 r2 co
  = case maybeSubCo2_maybe r1 r2 co of
      Just co' -> co'
      Nothing  -> pprPanic "maybeSubCo2" (ppr co)

-- if co is Nominal, returns it; otherwise, unwraps a SubCo; otherwise, fails
unSubCo_maybe :: Coercion -> Maybe Coercion
unSubCo_maybe (SubCo co)  = Just co
unSubCo_maybe (Refl _ ty) = Just $ Refl Nominal ty
unSubCo_maybe (TyConAppCo Representational tc cos)
  = do { cos' <- mapM unSubCo_maybe cos
       ; return $ TyConAppCo Nominal tc cos' }
unSubCo_maybe (UnivCo Representational ty1 ty2) = Just $ UnivCo Nominal ty1 ty2
  -- We do *not* promote UnivCo Phantom, as that's unsafe.
  -- UnivCo Nominal is no more unsafe than UnivCo Representational
unSubCo_maybe co
  | Nominal <- coercionRole co = Just co
unSubCo_maybe _ = Nothing

-- takes any coercion and turns it into a Phantom coercion
mkPhantomCo :: Coercion -> Coercion
mkPhantomCo co
  | Just ty <- isReflCo_maybe co    = Refl Phantom ty
  | Pair ty1 ty2 <- coercionKind co = UnivCo Phantom ty1 ty2
  -- don't optimise here... wait for OptCoercion

-- All input coercions are assumed to be Nominal,
-- or, if Role is Phantom, the Coercion can be Phantom, too.
applyRole :: Role -> Coercion -> Coercion
applyRole Nominal          = id
applyRole Representational = mkSubCo
applyRole Phantom          = mkPhantomCo

-- Convert args to a TyConAppCo Nominal to the same TyConAppCo Representational
applyRoles :: TyCon -> [Coercion] -> [Coercion]
applyRoles tc cos
  = zipWith applyRole (tyConRolesX Representational tc) cos

-- the Role parameter is the Role of the TyConAppCo
-- defined here because this is intimiately concerned with the implementation
-- of TyConAppCo
tyConRolesX :: Role -> TyCon -> [Role]
tyConRolesX Representational tc = tyConRoles tc ++ repeat Nominal
tyConRolesX role             _  = repeat role

nthRole :: Role -> TyCon -> Int -> Role
nthRole Nominal _ _ = Nominal
nthRole Phantom _ _ = Phantom
nthRole Representational tc n
  = (tyConRolesX Representational tc) !! n

-- is one role "less" than another?
ltRole :: Role -> Role -> Bool
ltRole Phantom          _       = False
ltRole Representational Phantom = True
ltRole Representational _       = False
ltRole Nominal          Nominal = False
ltRole Nominal          _       = True

-- Is the given tyvar used in a nominal position anywhere?
-- This is used in the GeneralizedNewtypeDeriving check.
tvUsedAtNominalRole :: TyVar -> Type -> Bool
tvUsedAtNominalRole tv = go Representational
  where go r (TyVarTy tv')
          | tv == tv' = (r == Nominal)
          | otherwise = False
        go r (AppTy t1 t2)      = go r t1 || go Nominal t2
        go r (TyConApp tc args) = or $ zipWith go (tyConRolesX r tc) args
        go r (FunTy t1 t2)      = go r t1 || go r t2
        go r (ForAllTy qtv ty)
          | tv == qtv  = False -- shadowed
          | otherwise  = go r ty
        go _ (LitTy _) = False

-- if we wish to apply `co` to some other coercion, what would be its best
-- role?
nextRole :: Coercion -> Role
nextRole (Refl r (TyConApp tc tys)) = head $ dropList tys (tyConRolesX r tc)
nextRole (TyConAppCo r tc cos)      = head $ dropList cos (tyConRolesX r tc)
nextRole _                          = Nominal

-- See note [Newtype coercions] in TyCon

-- | Create a coercion constructor (axiom) suitable for the given
--   newtype 'TyCon'. The 'Name' should be that of a new coercion
--   'CoAxiom', the 'TyVar's the arguments expected by the @newtype@ and
--   the type the appropriate right hand side of the @newtype@, with
--   the free variables a subset of those 'TyVar's.
mkNewTypeCo :: Name -> TyCon -> [TyVar] -> [Role] -> Type -> CoAxiom Unbranched
mkNewTypeCo name tycon tvs roles rhs_ty
  = CoAxiom { co_ax_unique   = nameUnique name
            , co_ax_name     = name
            , co_ax_implicit = True  -- See Note [Implicit axioms] in TyCon
            , co_ax_role     = Representational
            , co_ax_tc       = tycon
            , co_ax_branches = FirstBranch branch }
  where branch = CoAxBranch { cab_loc     = getSrcSpan name
                            , cab_tvs     = tvs
                            , cab_lhs     = mkTyVarTys tvs
                            , cab_roles   = roles
                            , cab_rhs     = rhs_ty
                            , cab_incomps = [] }

mkPiCos :: Role -> [Var] -> Coercion -> Coercion
mkPiCos r vs co = foldr (mkPiCo r) co vs

mkPiCo  :: Role -> Var -> Coercion -> Coercion
mkPiCo r v co | isTyVar v = mkForAllCo v co
              | otherwise = mkFunCo r (mkReflCo r (varType v)) co

-- The first coercion *must* be Nominal.
mkCoCast :: Coercion -> Coercion -> Coercion
-- (mkCoCast (c :: s1 ~# t1) (g :: (s1 ~# t1) ~# (s2 ~# t2)
mkCoCast c g
  = mkSymCo g1 `mkTransCo` c `mkTransCo` g2
  where
       -- g  :: (s1 ~# s2) ~# (t1 ~#  t2)
       -- g1 :: s1 ~# t1
       -- g2 :: s2 ~# t2
    [_reflk, g1, g2] = decomposeCo 3 g
            -- Remember, (~#) :: forall k. k -> k -> *
            -- so it takes *three* arguments, not two
\end{code}

%************************************************************************
%*                                                                      *
            Newtypes
%*                                                                      *
%************************************************************************

\begin{code}
instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, Coercion)
-- ^ If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
-- Checks for a newtype, and for being saturated
instNewTyCon_maybe tc tys
  | Just (tvs, ty, co_tc) <- unwrapNewTyCon_maybe tc  -- Check for newtype
  , tys `lengthIs` tyConArity tc                      -- Check saturated
  = Just (substTyWith tvs tys ty, mkUnbranchedAxInstCo Representational co_tc tys)
  | otherwise
  = Nothing

topNormaliseNewType_maybe :: Type -> Maybe (Coercion, Type)
-- ^ Sometimes we want to look through a @newtype@ and get its associated coercion.
-- This function strips off @newtype@ layers enough to reveal something that isn't
-- a @newtype@.  Specifically, here's the invariant:
--
-- > topNormaliseNewType_maybe rec_nts ty = Just (co, ty')
--
-- then (a)  @co : ty0 ~ ty'@.
--      (b)  ty' is not a newtype.
--
-- The function returns @Nothing@ for non-@newtypes@,
-- or unsaturated applications
topNormaliseNewType_maybe ty
  = go initRecTc Nothing ty
  where
    go rec_nts mb_co1 ty
       | Just (tc, tys) <- splitTyConApp_maybe ty
       , Just (ty', co2) <- instNewTyCon_maybe tc tys
       , let co' = case mb_co1 of
                      Nothing  -> co2
                      Just co1 -> mkTransCo co1 co2
       = case checkRecTc rec_nts tc of
           Just rec_nts' -> go rec_nts' (Just co') ty'
           Nothing       -> Nothing
                  -- Return Nothing overall if we get stuck
                  -- so that the return invariant is satisfied
                  -- See Note [Expanding newtypes] in TyCon

       | Just co1 <- mb_co1     -- Progress, but stopped on a non-newtype
       = Just (co1, ty)

       | otherwise              -- No progress
       = Nothing
\end{code}


%************************************************************************
%*                                                                      *
                   Equality of coercions
%*                                                                      *
%************************************************************************

\begin{code}
-- | Determines syntactic equality of coercions
coreEqCoercion :: Coercion -> Coercion -> Bool
coreEqCoercion co1 co2 = coreEqCoercion2 rn_env co1 co2
  where rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfCo co1 `unionVarSet` tyCoVarsOfCo co2))

coreEqCoercion2 :: RnEnv2 -> Coercion -> Coercion -> Bool
coreEqCoercion2 env (Refl eq1 ty1) (Refl eq2 ty2) = eq1 == eq2 && eqTypeX env ty1 ty2
coreEqCoercion2 env (TyConAppCo eq1 tc1 cos1) (TyConAppCo eq2 tc2 cos2)
  = eq1 == eq2 && tc1 == tc2 && all2 (coreEqCoercion2 env) cos1 cos2

coreEqCoercion2 env (AppCo co11 co12) (AppCo co21 co22)
  = coreEqCoercion2 env co11 co21 && coreEqCoercion2 env co12 co22

coreEqCoercion2 env (ForAllCo v1 co1) (ForAllCo v2 co2)
  = coreEqCoercion2 (rnBndr2 env v1 v2) co1 co2

coreEqCoercion2 env (CoVarCo cv1) (CoVarCo cv2)
  = rnOccL env cv1 == rnOccR env cv2

coreEqCoercion2 env (AxiomInstCo con1 ind1 cos1) (AxiomInstCo con2 ind2 cos2)
  = con1 == con2
    && ind1 == ind2
    && all2 (coreEqCoercion2 env) cos1 cos2

coreEqCoercion2 env (UnivCo r1 ty11 ty12) (UnivCo r2 ty21 ty22)
  = r1 == r2 && eqTypeX env ty11 ty21 && eqTypeX env ty12 ty22

coreEqCoercion2 env (SymCo co1) (SymCo co2)
  = coreEqCoercion2 env co1 co2

coreEqCoercion2 env (TransCo co11 co12) (TransCo co21 co22)
  = coreEqCoercion2 env co11 co21 && coreEqCoercion2 env co12 co22

coreEqCoercion2 env (NthCo d1 co1) (NthCo d2 co2)
  = d1 == d2 && coreEqCoercion2 env co1 co2
coreEqCoercion2 env (LRCo d1 co1) (LRCo d2 co2)
  = d1 == d2 && coreEqCoercion2 env co1 co2

coreEqCoercion2 env (InstCo co1 ty1) (InstCo co2 ty2)
  = coreEqCoercion2 env co1 co2 && eqTypeX env ty1 ty2

coreEqCoercion2 env (SubCo co1) (SubCo co2)
  = coreEqCoercion2 env co1 co2

coreEqCoercion2 env (AxiomRuleCo a1 ts1 cs1) (AxiomRuleCo a2 ts2 cs2)
  = a1 == a2 && all2 (eqTypeX env) ts1 ts2 && all2 (coreEqCoercion2 env) cs1 cs2

coreEqCoercion2 _ _ _ = False
\end{code}

%************************************************************************
%*                                                                      *
                   Substitution of coercions
%*                                                                      *
%************************************************************************

\begin{code}
-- | A substitution of 'Coercion's for 'CoVar's (OR 'TyVar's, when
--   doing a \"lifting\" substitution)
type CvSubstEnv = VarEnv Coercion

emptyCvSubstEnv :: CvSubstEnv
emptyCvSubstEnv = emptyVarEnv

data CvSubst
  = CvSubst InScopeSet  -- The in-scope type variables
            TvSubstEnv  -- Substitution of types
            CvSubstEnv  -- Substitution of coercions

instance Outputable CvSubst where
  ppr (CvSubst ins tenv cenv)
    = brackets $ sep[ ptext (sLit "CvSubst"),
                      nest 2 (ptext (sLit "In scope:") <+> ppr ins),
                      nest 2 (ptext (sLit "Type env:") <+> ppr tenv),
                      nest 2 (ptext (sLit "Coercion env:") <+> ppr cenv) ]

emptyCvSubst :: CvSubst
emptyCvSubst = CvSubst emptyInScopeSet emptyVarEnv emptyVarEnv

isEmptyCvSubst :: CvSubst -> Bool
isEmptyCvSubst (CvSubst _ tenv cenv) = isEmptyVarEnv tenv && isEmptyVarEnv cenv

getCvInScope :: CvSubst -> InScopeSet
getCvInScope (CvSubst in_scope _ _) = in_scope

zapCvSubstEnv :: CvSubst -> CvSubst
zapCvSubstEnv (CvSubst in_scope _ _) = CvSubst in_scope emptyVarEnv emptyVarEnv

cvTvSubst :: CvSubst -> TvSubst
cvTvSubst (CvSubst in_scope tvs _) = TvSubst in_scope tvs

tvCvSubst :: TvSubst -> CvSubst
tvCvSubst (TvSubst in_scope tenv) = CvSubst in_scope tenv emptyCvSubstEnv

extendTvSubst :: CvSubst -> TyVar -> Type -> CvSubst
extendTvSubst (CvSubst in_scope tenv cenv) tv ty
  = CvSubst in_scope (extendVarEnv tenv tv ty) cenv

extendTvSubstAndInScope :: CvSubst -> TyVar -> Type -> CvSubst
extendTvSubstAndInScope (CvSubst in_scope tenv cenv) tv ty
  = CvSubst (in_scope `extendInScopeSetSet` tyVarsOfType ty)
            (extendVarEnv tenv tv ty)
            cenv

extendCvSubstAndInScope :: CvSubst -> CoVar -> Coercion -> CvSubst
-- Also extends the in-scope set
extendCvSubstAndInScope (CvSubst in_scope tenv cenv) cv co
  = CvSubst (in_scope `extendInScopeSetSet` tyCoVarsOfCo co)
            tenv
            (extendVarEnv cenv cv co)

substCoVarBndr :: CvSubst -> CoVar -> (CvSubst, CoVar)
substCoVarBndr subst@(CvSubst in_scope tenv cenv) old_var
  = ASSERT( isCoVar old_var )
    (CvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv, new_var)
  where
    -- When we substitute (co :: t1 ~ t2) we may get the identity (co :: t ~ t)
    -- In that case, mkCoVarCo will return a ReflCoercion, and
    -- we want to substitute that (not new_var) for old_var
    new_co    = mkCoVarCo new_var
    no_change = new_var == old_var && not (isReflCo new_co)

    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co

    new_var = uniqAway in_scope subst_old_var
    subst_old_var = mkCoVar (varName old_var) (substTy subst (varType old_var))
                  -- It's important to do the substitution for coercions,
                  -- because they can have free type variables

substTyVarBndr :: CvSubst -> TyVar -> (CvSubst, TyVar)
substTyVarBndr (CvSubst in_scope tenv cenv) old_var
  = case Type.substTyVarBndr (TvSubst in_scope tenv) old_var of
      (TvSubst in_scope' tenv', new_var) -> (CvSubst in_scope' tenv' cenv, new_var)

mkCvSubst :: InScopeSet -> [(Var,Coercion)] -> CvSubst
mkCvSubst in_scope prs = CvSubst in_scope Type.emptyTvSubstEnv (mkVarEnv prs)

zipOpenCvSubst :: [Var] -> [Coercion] -> CvSubst
zipOpenCvSubst vs cos
  | debugIsOn && (length vs /= length cos)
  = pprTrace "zipOpenCvSubst" (ppr vs $$ ppr cos) emptyCvSubst
  | otherwise
  = CvSubst (mkInScopeSet (tyCoVarsOfCos cos)) emptyTvSubstEnv (zipVarEnv vs cos)

substCoWithTy :: InScopeSet -> TyVar -> Type -> Coercion -> Coercion
substCoWithTy in_scope tv ty = substCoWithTys in_scope [tv] [ty]

substCoWithTys :: InScopeSet -> [TyVar] -> [Type] -> Coercion -> Coercion
substCoWithTys in_scope tvs tys co
  | debugIsOn && (length tvs /= length tys)
  = pprTrace "substCoWithTys" (ppr tvs $$ ppr tys) co
  | otherwise
  = ASSERT( length tvs == length tys )
    substCo (CvSubst in_scope (zipVarEnv tvs tys) emptyVarEnv) co

-- | Substitute within a 'Coercion'
substCo :: CvSubst -> Coercion -> Coercion
substCo subst co | isEmptyCvSubst subst = co
                 | otherwise            = subst_co subst co

-- | Substitute within several 'Coercion's
substCos :: CvSubst -> [Coercion] -> [Coercion]
substCos subst cos | isEmptyCvSubst subst = cos
                   | otherwise            = map (substCo subst) cos

substTy :: CvSubst -> Type -> Type
substTy subst = Type.substTy (cvTvSubst subst)

subst_co :: CvSubst -> Coercion -> Coercion
subst_co subst co
  = go co
  where
    go_ty :: Type -> Type
    go_ty = Coercion.substTy subst

    go :: Coercion -> Coercion
    go (Refl eq ty)          = Refl eq $! go_ty ty
    go (TyConAppCo eq tc cos)   = let args = map go cos
                                  in  args `seqList` TyConAppCo eq tc args
    go (AppCo co1 co2)       = mkAppCo (go co1) $! go co2
    go (ForAllCo tv co)      = case substTyVarBndr subst tv of
                                 (subst', tv') ->
                                   ForAllCo tv' $! subst_co subst' co
    go (CoVarCo cv)          = substCoVar subst cv
    go (AxiomInstCo con ind cos) = AxiomInstCo con ind $! map go cos
    go (UnivCo r ty1 ty2)    = (UnivCo r $! go_ty ty1) $! go_ty ty2
    go (SymCo co)            = mkSymCo (go co)
    go (TransCo co1 co2)     = mkTransCo (go co1) (go co2)
    go (NthCo d co)          = mkNthCo d (go co)
    go (LRCo lr co)          = mkLRCo lr (go co)
    go (InstCo co ty)        = mkInstCo (go co) $! go_ty ty
    go (SubCo co)            = mkSubCo (go co)
    go (AxiomRuleCo co ts cs)  = let ts1 = map go_ty ts
                                     cs1 = map go cs
                                 in ts1 `seqList` cs1 `seqList`
                                    AxiomRuleCo co ts1 cs1



substCoVar :: CvSubst -> CoVar -> Coercion
substCoVar (CvSubst in_scope _ cenv) cv
  | Just co  <- lookupVarEnv cenv cv      = co
  | Just cv1 <- lookupInScope in_scope cv = ASSERT( isCoVar cv1 ) CoVarCo cv1
  | otherwise = WARN( True, ptext (sLit "substCoVar not in scope") <+> ppr cv $$ ppr in_scope)
                ASSERT( isCoVar cv ) CoVarCo cv

substCoVars :: CvSubst -> [CoVar] -> [Coercion]
substCoVars subst cvs = map (substCoVar subst) cvs

lookupTyVar :: CvSubst -> TyVar  -> Maybe Type
lookupTyVar (CvSubst _ tenv _) tv = lookupVarEnv tenv tv

lookupCoVar :: CvSubst -> Var  -> Maybe Coercion
lookupCoVar (CvSubst _ _ cenv) v = lookupVarEnv cenv v
\end{code}

%************************************************************************
%*                                                                      *
                   "Lifting" substitution
           [(TyVar,Coercion)] -> Type -> Coercion
%*                                                                      *
%************************************************************************

Note [Lifting coercions over types: liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The KPUSH rule deals with this situation
   data T a = MkK (a -> Maybe a)
   g :: T t1 ~ K t2
   x :: t1 -> Maybe t1

   case (K @t1 x) |> g of
     K (y:t2 -> Maybe t2) -> rhs

We want to push the coercion inside the constructor application.
So we do this

   g' :: t1~t2  =  Nth 0 g

   case K @t2 (x |> g' -> Maybe g') of
     K (y:t2 -> Maybe t2) -> rhs

The crucial operation is that we
  * take the type of K's argument: a -> Maybe a
  * and substitute g' for a
thus giving *coercion*.  This is what liftCoSubst does.

Note [Substituting kinds in liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to take care with kind polymorphism.  Suppose
  K :: forall k (a:k). (forall b:k. a -> b) -> T k a

Now given  (K @kk1 @ty1 v) |> g) where
  g :: T kk1 ty1 ~ T kk2 ty2
we want to compute
   (forall b:k a->b) [ Nth 0 g/k, Nth 1 g/a ]
Notice that we MUST substitute for 'k'; this happens in
liftCoSubstTyVarBndr.  But what should we substitute?
We need to take b's kind 'k' and return a Kind, not a Coercion!

Happily we can do this because we know that all kind coercions
((Nth 0 g) in this case) are Refl.  So we need a special purpose
   subst_kind: LiftCoSubst -> Kind -> Kind
that expects a Refl coercion (or something equivalent to Refl)
when it looks up a kind variable.

\begin{code}
-- ----------------------------------------------------
-- See Note [Lifting coercions over types: liftCoSubst]
-- ----------------------------------------------------

data LiftCoSubst = LCS InScopeSet LiftCoEnv

type LiftCoEnv = VarEnv Coercion
     -- Maps *type variables* to *coercions*
     -- That's the whole point of this function!

liftCoSubstWith :: Role -> [TyVar] -> [Coercion] -> Type -> Coercion
liftCoSubstWith r tvs cos ty
  = liftCoSubst r (zipEqual "liftCoSubstWith" tvs cos) ty

liftCoSubst :: Role -> [(TyVar,Coercion)] -> Type -> Coercion
liftCoSubst r prs ty
 | null prs  = Refl r ty
 | otherwise = ty_co_subst (LCS (mkInScopeSet (tyCoVarsOfCos (map snd prs)))
                                (mkVarEnv prs)) r ty

-- | The \"lifting\" operation which substitutes coercions for type
--   variables in a type to produce a coercion.
--
--   For the inverse operation, see 'liftCoMatch'

-- The Role parameter is the _desired_ role
ty_co_subst :: LiftCoSubst -> Role -> Type -> Coercion
ty_co_subst subst role ty
  = go role ty
  where
    go Phantom ty             = lift_phantom ty
    go role (TyVarTy tv)      = liftCoSubstTyVar subst role tv
                                `orElse` Refl role (TyVarTy tv)
                             -- A type variable from a non-cloned forall
                             -- won't be in the substitution
    go role (AppTy ty1 ty2)   = mkAppCo (go role ty1) (go Nominal ty2)
    go role (TyConApp tc tys) = mkTyConAppCo role tc
                                           (zipWith go (tyConRolesX role tc) tys)
                           -- IA0_NOTE: Do we need to do anything
                           -- about kind instantiations? I don't think
                           -- so.  see Note [Kind coercions]
    go role (FunTy ty1 ty2)   = mkFunCo role (go role ty1) (go role ty2)
    go role (ForAllTy v ty)   = mkForAllCo v' $! (ty_co_subst subst' role ty)
                         where
                           (subst', v') = liftCoSubstTyVarBndr subst v
    go role ty@(LitTy {})     = ASSERT( role == Nominal )
                                mkReflCo role ty

    lift_phantom ty = mkUnivCo Phantom (liftCoSubstLeft  subst ty)
                                       (liftCoSubstRight subst ty)

\end{code}

Note [liftCoSubstTyVar]
~~~~~~~~~~~~~~~~~~~~~~~
This function can fail (i.e., return Nothing) for two separate reasons:
 1) The variable is not in the substutition
 2) The coercion found is of too low a role

liftCoSubstTyVar is called from two places: in liftCoSubst (naturally), and
also in matchAxiom in OptCoercion. From liftCoSubst, the so-called lifting
lemma guarantees that the roles work out. If we fail for reason 2) in this
case, we really should panic -- something is deeply wrong. But, in matchAxiom,
failing for reason 2) is fine. matchAxiom is trying to find a set of coercions
that match, but it may fail, and this is healthy behavior. Bottom line: if
you find that liftCoSubst is doing weird things (like leaving out-of-scope
variables lying around), disable coercion optimization (bypassing matchAxiom)
and use maybeSubCo2 instead of maybeSubCo2_maybe. The panic will then happen,
and you may learn something useful.

\begin{code}

liftCoSubstTyVar :: LiftCoSubst -> Role -> TyVar -> Maybe Coercion
liftCoSubstTyVar (LCS _ cenv) r tv
  = do { co <- lookupVarEnv cenv tv
       ; let co_role = coercionRole co   -- could theoretically take this as
                                         -- a parameter, but painful
       ; maybeSubCo2_maybe r co_role co } -- see Note [liftCoSubstTyVar]

liftCoSubstTyVarBndr :: LiftCoSubst -> TyVar -> (LiftCoSubst, TyVar)
liftCoSubstTyVarBndr subst@(LCS in_scope cenv) old_var
  = (LCS (in_scope `extendInScopeSet` new_var) new_cenv, new_var)
  where
    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var (Refl Nominal (TyVarTy new_var))

    no_change = no_kind_change && (new_var == old_var)

    new_var1 = uniqAway in_scope old_var

    old_ki = tyVarKind old_var
    no_kind_change = isEmptyVarSet (tyVarsOfType old_ki)
    new_var | no_kind_change = new_var1
            | otherwise      = setTyVarKind new_var1 (subst_kind subst old_ki)

-- map every variable to the type on the *left* of its mapped coercion
liftCoSubstLeft :: LiftCoSubst -> Type -> Type
liftCoSubstLeft (LCS in_scope cenv) ty
  = Type.substTy (mkTvSubst in_scope (mapVarEnv (pFst . coercionKind) cenv)) ty

-- same, but to the type on the right
liftCoSubstRight :: LiftCoSubst -> Type -> Type
liftCoSubstRight (LCS in_scope cenv) ty
  = Type.substTy (mkTvSubst in_scope (mapVarEnv (pSnd . coercionKind) cenv)) ty

subst_kind :: LiftCoSubst -> Kind -> Kind
-- See Note [Substituting kinds in liftCoSubst]
subst_kind subst@(LCS _ cenv) kind
  = go kind
  where
    go (LitTy n)         = n `seq` LitTy n
    go (TyVarTy kv)      = subst_kv kv
    go (TyConApp tc tys) = let args = map go tys
                           in  args `seqList` TyConApp tc args

    go (FunTy arg res)   = (FunTy $! (go arg)) $! (go res)
    go (AppTy fun arg)   = mkAppTy (go fun) $! (go arg)
    go (ForAllTy tv ty)  = case liftCoSubstTyVarBndr subst tv of
                              (subst', tv') ->
                                 ForAllTy tv' $! (subst_kind subst' ty)

    subst_kv kv
      | Just co <- lookupVarEnv cenv kv
      , let co_kind = coercionKind co
      = ASSERT2( pFst co_kind `eqKind` pSnd co_kind, ppr kv $$ ppr co )
        pFst co_kind
      | otherwise
      = TyVarTy kv
\end{code}

\begin{code}
-- | 'liftCoMatch' is sort of inverse to 'liftCoSubst'.  In particular, if
--   @liftCoMatch vars ty co == Just s@, then @tyCoSubst s ty == co@.
--   That is, it matches a type against a coercion of the same
--   "shape", and returns a lifting substitution which could have been
--   used to produce the given coercion from the given type.
liftCoMatch :: TyVarSet -> Type -> Coercion -> Maybe LiftCoSubst
liftCoMatch tmpls ty co
  = case ty_co_match menv emptyVarEnv ty co of
      Just cenv -> Just (LCS in_scope cenv)
      Nothing   -> Nothing
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyCoVarsOfCo co)
    -- Like tcMatchTy, assume all the interesting variables
    -- in ty are in tmpls

-- | 'ty_co_match' does all the actual work for 'liftCoMatch'.
ty_co_match :: MatchEnv -> LiftCoEnv -> Type -> Coercion -> Maybe LiftCoEnv
ty_co_match menv subst ty co
  | Just ty' <- coreView ty = ty_co_match menv subst ty' co

  -- Match a type variable against a non-refl coercion
ty_co_match menv cenv (TyVarTy tv1) co
  | Just co1' <- lookupVarEnv cenv tv1'      -- tv1' is already bound to co1
  = if coreEqCoercion2 (nukeRnEnvL rn_env) co1' co
    then Just cenv
    else Nothing       -- no match since tv1 matches two different coercions

  | tv1' `elemVarSet` me_tmpls menv           -- tv1' is a template var
  = if any (inRnEnvR rn_env) (varSetElems (tyCoVarsOfCo co))
    then Nothing      -- occurs check failed
    else return (extendVarEnv cenv tv1' co)
        -- BAY: I don't think we need to do any kind matching here yet
        -- (compare 'match'), but we probably will when moving to SHE.

  | otherwise    -- tv1 is not a template ty var, so the only thing it
                 -- can match is a reflexivity coercion for itself.
                 -- But that case is dealt with already
  = Nothing

  where
    rn_env = me_env menv
    tv1' = rnOccL rn_env tv1

ty_co_match menv subst (AppTy ty1 ty2) co
  | Just (co1, co2) <- splitAppCo_maybe co      -- c.f. Unify.match on AppTy
  = do { subst' <- ty_co_match menv subst ty1 co1
       ; ty_co_match menv subst' ty2 co2 }

ty_co_match menv subst (TyConApp tc1 tys) (TyConAppCo _ tc2 cos)
  | tc1 == tc2 = ty_co_matches menv subst tys cos

ty_co_match menv subst (FunTy ty1 ty2) (TyConAppCo _ tc cos)
  | tc == funTyCon = ty_co_matches menv subst [ty1,ty2] cos

ty_co_match menv subst (ForAllTy tv1 ty) (ForAllCo tv2 co)
  = ty_co_match menv' subst ty co
  where
    menv' = menv { me_env = rnBndr2 (me_env menv) tv1 tv2 }

ty_co_match menv subst ty co
  | Just co' <- pushRefl co = ty_co_match menv subst ty co'
  | otherwise               = Nothing

ty_co_matches :: MatchEnv -> LiftCoEnv -> [Type] -> [Coercion] -> Maybe LiftCoEnv
ty_co_matches menv = matchList (ty_co_match menv)

pushRefl :: Coercion -> Maybe Coercion
pushRefl (Refl Nominal (AppTy ty1 ty2))
  = Just (AppCo (Refl Nominal ty1) (Refl Nominal ty2))
pushRefl (Refl r (FunTy ty1 ty2))
  = Just (TyConAppCo r funTyCon [Refl r ty1, Refl r ty2])
pushRefl (Refl r (TyConApp tc tys))
  = Just (TyConAppCo r tc (zipWith mkReflCo (tyConRolesX r tc) tys))
pushRefl (Refl r (ForAllTy tv ty)) = Just (ForAllCo tv (Refl r ty))
pushRefl _                          = Nothing
\end{code}

%************************************************************************
%*                                                                      *
            Sequencing on coercions
%*                                                                      *
%************************************************************************

\begin{code}
seqCo :: Coercion -> ()
seqCo (Refl eq ty)              = eq `seq` seqType ty
seqCo (TyConAppCo eq tc cos)    = eq `seq` tc `seq` seqCos cos
seqCo (AppCo co1 co2)           = seqCo co1 `seq` seqCo co2
seqCo (ForAllCo tv co)          = tv `seq` seqCo co
seqCo (CoVarCo cv)              = cv `seq` ()
seqCo (AxiomInstCo con ind cos) = con `seq` ind `seq` seqCos cos
seqCo (UnivCo r ty1 ty2)        = r `seq` seqType ty1 `seq` seqType ty2
seqCo (SymCo co)                = seqCo co
seqCo (TransCo co1 co2)         = seqCo co1 `seq` seqCo co2
seqCo (NthCo _ co)              = seqCo co
seqCo (LRCo _ co)               = seqCo co
seqCo (InstCo co ty)            = seqCo co `seq` seqType ty
seqCo (SubCo co)                = seqCo co
seqCo (AxiomRuleCo _ ts cs)     = seqTypes ts `seq` seqCos cs

seqCos :: [Coercion] -> ()
seqCos []       = ()
seqCos (co:cos) = seqCo co `seq` seqCos cos
\end{code}


%************************************************************************
%*                                                                      *
             The kind of a type, and of a coercion
%*                                                                      *
%************************************************************************

\begin{code}
coercionType :: Coercion -> Type
coercionType co = case coercionKind co of
                    Pair ty1 ty2 -> mkCoercionType (coercionRole co) ty1 ty2

------------------
-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ relates @t1@ and @t2@, then @coercionKind c = Pair t1 t2@.

coercionKind :: Coercion -> Pair Type
coercionKind co = go co
  where
    go (Refl _ ty)           = Pair ty ty
    go (TyConAppCo _ tc cos) = mkTyConApp tc <$> (sequenceA $ map go cos)
    go (AppCo co1 co2)       = mkAppTy <$> go co1 <*> go co2
    go (ForAllCo tv co)      = mkForAllTy tv <$> go co
    go (CoVarCo cv)          = toPair $ coVarKind cv
    go (AxiomInstCo ax ind cos)
      | CoAxBranch { cab_tvs = tvs, cab_lhs = lhs, cab_rhs = rhs } <- coAxiomNthBranch ax ind
      , Pair tys1 tys2 <- sequenceA (map go cos)
      = ASSERT( cos `equalLength` tvs )  -- Invariant of AxiomInstCo: cos should
                                         -- exactly saturate the axiom branch
        Pair (substTyWith tvs tys1 (mkTyConApp (coAxiomTyCon ax) lhs))
             (substTyWith tvs tys2 rhs)
    go (UnivCo _ ty1 ty2)    = Pair ty1 ty2
    go (SymCo co)            = swap $ go co
    go (TransCo co1 co2)     = Pair (pFst $ go co1) (pSnd $ go co2)
    go (NthCo d co)          = tyConAppArgN d <$> go co
    go (LRCo lr co)          = (pickLR lr . splitAppTy) <$> go co
    go (InstCo aco ty)       = go_app aco [ty]
    go (SubCo co)            = go co
    go (AxiomRuleCo ax tys cos) =
      case coaxrProves ax tys (map coercionKind cos) of
        Just res -> res
        Nothing  -> panic "coercionKind: Malformed coercion"


    go_app :: Coercion -> [Type] -> Pair Type
    -- Collect up all the arguments and apply all at once
    -- See Note [Nested InstCos]
    go_app (InstCo co ty) tys = go_app co (ty:tys)
    go_app co             tys = (`applyTys` tys) <$> go co

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> Pair [Type]
coercionKinds tys = sequenceA $ map coercionKind tys

coercionRole :: Coercion -> Role
coercionRole = go
  where
    go (Refl r _)           = r
    go (TyConAppCo r _ _)   = r
    go (AppCo co _)         = go co
    go (ForAllCo _ co)      = go co
    go (CoVarCo cv)         = coVarRole cv
    go (AxiomInstCo ax _ _) = coAxiomRole ax
    go (UnivCo r _ _)       = r
    go (SymCo co)           = go co
    go (TransCo co1 _)      = go co1 -- same as go co2
    go (NthCo n co)         = let Pair ty1 _ = coercionKind co
                                  (tc, _) = splitTyConApp ty1
                              in nthRole (coercionRole co) tc n
    go (LRCo _ _)           = Nominal
    go (InstCo co _)        = go co
    go (SubCo _)            = Representational
    go (AxiomRuleCo c _ _)  = coaxrRole c
\end{code}

Note [Nested InstCos]
~~~~~~~~~~~~~~~~~~~~~
In Trac #5631 we found that 70% of the entire compilation time was
being spent in coercionKind!  The reason was that we had
   (g @ ty1 @ ty2 .. @ ty100)    -- The "@s" are InstCos
where
   g :: forall a1 a2 .. a100. phi
If we deal with the InstCos one at a time, we'll do this:
   1.  Find the kind of (g @ ty1 .. @ ty99) : forall a100. phi'
   2.  Substitute phi'[ ty100/a100 ], a single tyvar->type subst
But this is a *quadratic* algorithm, and the blew up Trac #5631.
So it's very important to do the substitution simultaneously.

cf Type.applyTys (which in fact we call here)


\begin{code}
applyCo :: Type -> Coercion -> Type
-- Gives the type of (e co) where e :: (a~b) => ty
applyCo ty co | Just ty' <- coreView ty = applyCo ty' co
applyCo (FunTy _ ty) _ = ty
applyCo _            _ = panic "applyCo"
\end{code}

Note [Kind coercions]
~~~~~~~~~~~~~~~~~~~~~
Kind coercions are only of the form: Refl kind. They are only used to
instantiate kind polymorphic type constructors in TyConAppCo. Remember
that kind instantiation only happens with TyConApp, not AppTy.
