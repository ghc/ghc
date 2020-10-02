{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[DataCon]{@DataCon@: Data Constructors}
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module GHC.Core.DataCon (
        -- * Main data types
        DataCon, DataConRep(..),
        SrcStrictness(..), SrcUnpackedness(..),
        HsSrcBang(..), HsImplBang(..),
        StrictnessMark(..),
        ConTag,

        -- ** Equality specs
        EqSpec, mkEqSpec, eqSpecTyVar, eqSpecType,
        eqSpecPair, eqSpecPreds,
        substEqSpec, filterEqSpec,

        -- ** Field labels
        FieldLabel(..), FieldLabelString,

        -- ** Type construction
        mkDataCon, fIRST_TAG,

        -- ** Type deconstruction
        dataConRepType, dataConInstSig, dataConFullSig,
        dataConName, dataConIdentity, dataConTag, dataConTagZ,
        dataConTyCon, dataConOrigTyCon,
        dataConWrapperType,
        dataConNonlinearType,
        dataConDisplayType,
        dataConUnivTyVars, dataConExTyCoVars, dataConUnivAndExTyCoVars,
        dataConUserTyVars, dataConUserTyVarBinders,
        dataConEqSpec, dataConTheta,
        dataConStupidTheta,
        dataConOtherTheta,
        dataConInstArgTys, dataConOrigArgTys, dataConOrigResTy,
        dataConInstOrigArgTys, dataConRepArgTys,
        dataConFieldLabels, dataConFieldType, dataConFieldType_maybe,
        dataConSrcBangs,
        dataConSourceArity, dataConRepArity,
        dataConIsInfix,
        dataConWorkId, dataConWrapId, dataConWrapId_maybe,
        dataConImplicitTyThings,
        dataConRepStrictness, dataConImplBangs, dataConBoxer,

        splitDataProductType_maybe,

        -- ** Predicates on DataCons
        isNullarySrcDataCon, isNullaryRepDataCon,
        isTupleDataCon, isBoxedTupleDataCon, isUnboxedTupleDataCon,
        isUnboxedSumDataCon,
        isVanillaDataCon, isNewDataCon, classDataCon, dataConCannotMatch,
        dataConUserTyVarsArePermuted,
        isBanged, isMarkedStrict, eqHsBang, isSrcStrict, isSrcUnpacked,
        specialPromotedDc,

        -- ** Promotion related functions
        promoteDataCon
    ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Types.Id.Make ( DataConBoxer )
import GHC.Core.Type as Type
import GHC.Core.Coercion
import GHC.Core.Unify
import GHC.Core.TyCon
import GHC.Core.Multiplicity
import {-# SOURCE #-} GHC.Types.TyThing
import GHC.Types.FieldLabel
import GHC.Types.SourceText
import GHC.Core.Class
import GHC.Types.Name
import GHC.Builtin.Names
import GHC.Core.Predicate
import GHC.Types.Var
import GHC.Types.Basic
import GHC.Data.FastString
import GHC.Unit.Types
import GHC.Unit.Module.Name
import GHC.Utils.Binary
import GHC.Types.Unique.Set
import GHC.Builtin.Uniques( mkAlphaTyVarUnique )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Data as Data
import Data.Char
import Data.List( find )

{-
Data constructor representation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following Haskell data type declaration

        data T = T !Int ![Int]

Using the strictness annotations, GHC will represent this as

        data T = T Int# [Int]

That is, the Int has been unboxed.  Furthermore, the Haskell source construction

        T e1 e2

is translated to

        case e1 of { I# x ->
        case e2 of { r ->
        T x r }}

That is, the first argument is unboxed, and the second is evaluated.  Finally,
pattern matching is translated too:

        case e of { T a b -> ... }

becomes

        case e of { T a' b -> let a = I# a' in ... }

To keep ourselves sane, we name the different versions of the data constructor
differently, as follows.


Note [Data Constructor Naming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each data constructor C has two, and possibly up to four, Names associated with it:

                   OccName   Name space   Name of   Notes
 ---------------------------------------------------------------------------
 The "data con itself"   C     DataName   DataCon   In dom( GlobalRdrEnv )
 The "worker data con"   C     VarName    Id        The worker
 The "wrapper data con"  $WC   VarName    Id        The wrapper
 The "newtype coercion"  :CoT  TcClsName  TyCon

EVERY data constructor (incl for newtypes) has the former two (the
data con itself, and its worker.  But only some data constructors have a
wrapper (see Note [The need for a wrapper]).

Each of these three has a distinct Unique.  The "data con itself" name
appears in the output of the renamer, and names the Haskell-source
data constructor.  The type checker translates it into either the wrapper Id
(if it exists) or worker Id (otherwise).

The data con has one or two Ids associated with it:

The "worker Id", is the actual data constructor.
* Every data constructor (newtype or data type) has a worker

* The worker is very like a primop, in that it has no binding.

* For a *data* type, the worker *is* the data constructor;
  it has no unfolding

* For a *newtype*, the worker has a compulsory unfolding which
  does a cast, e.g.
        newtype T = MkT Int
        The worker for MkT has unfolding
                \\(x:Int). x `cast` sym CoT
  Here CoT is the type constructor, witnessing the FC axiom
        axiom CoT : T = Int

The "wrapper Id", \$WC, goes as follows

* Its type is exactly what it looks like in the source program.

* It is an ordinary function, and it gets a top-level binding
  like any other function.

* The wrapper Id isn't generated for a data type if there is
  nothing for the wrapper to do.  That is, if its defn would be
        \$wC = C

Note [Data constructor workers and wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Algebraic data types
  - Always have a worker, with no unfolding
  - May or may not have a wrapper; see Note [The need for a wrapper]

* Newtypes
  - Always have a worker, which has a compulsory unfolding (just a cast)
  - May or may not have a wrapper; see Note [The need for a wrapper]

* INVARIANT: the dictionary constructor for a class
             never has a wrapper.

* Neither_ the worker _nor_ the wrapper take the dcStupidTheta dicts as arguments

* The wrapper (if it exists) takes dcOrigArgTys as its arguments.
  The worker takes dataConRepArgTys as its arguments
  If the worker is absent, dataConRepArgTys is the same as dcOrigArgTys

* The 'NoDataConRep' case of DataConRep is important. Not only is it
  efficient, but it also ensures that the wrapper is replaced by the
  worker (because it *is* the worker) even when there are no
  args. E.g. in
               f (:) x
  the (:) *is* the worker.  This is really important in rule matching,
  (We could match on the wrappers, but that makes it less likely that
  rules will match when we bring bits of unfoldings together.)

Note [The need for a wrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why might the wrapper have anything to do?  The full story is
in wrapper_reqd in GHC.Types.Id.Make.mkDataConRep.

* Unboxing strict fields (with -funbox-strict-fields)
        data T = MkT !(Int,Int)
        \$wMkT :: (Int,Int) -> T
        \$wMkT (x,y) = MkT x y
  Notice that the worker has two fields where the wapper has
  just one.  That is, the worker has type
                MkT :: Int -> Int -> T

* Equality constraints for GADTs
        data T a where { MkT :: a -> T [a] }

  The worker gets a type with explicit equality
  constraints, thus:
        MkT :: forall a b. (a=[b]) => b -> T a

  The wrapper has the programmer-specified type:
        \$wMkT :: a -> T [a]
        \$wMkT a x = MkT [a] a [a] x
  The third argument is a coercion
        [a] :: [a]~[a]

* Data family instances may do a cast on the result

* Type variables may be permuted; see MkId
  Note [Data con wrappers and GADT syntax]


Note [The stupid context]
~~~~~~~~~~~~~~~~~~~~~~~~~
Data types can have a context:

        data (Eq a, Ord b) => T a b = T1 a b | T2 a

and that makes the constructors have a context too
(notice that T2's context is "thinned"):

        T1 :: (Eq a, Ord b) => a -> b -> T a b
        T2 :: (Eq a) => a -> T a b

Furthermore, this context pops up when pattern matching
(though GHC hasn't implemented this, but it is in H98, and
I've fixed GHC so that it now does):

        f (T2 x) = x
gets inferred type
        f :: Eq a => T a b -> a

I say the context is "stupid" because the dictionaries passed
are immediately discarded -- they do nothing and have no benefit.
It's a flaw in the language.

        Up to now [March 2002] I have put this stupid context into the
        type of the "wrapper" constructors functions, T1 and T2, but
        that turned out to be jolly inconvenient for generics, and
        record update, and other functions that build values of type T
        (because they don't have suitable dictionaries available).

        So now I've taken the stupid context out.  I simply deal with
        it separately in the type checker on occurrences of a
        constructor, either in an expression or in a pattern.

        [May 2003: actually I think this decision could easily be
        reversed now, and probably should be.  Generics could be
        disabled for types with a stupid context; record updates now
        (H98) needs the context too; etc.  It's an unforced change, so
        I'm leaving it for now --- but it does seem odd that the
        wrapper doesn't include the stupid context.]

[July 04] With the advent of generalised data types, it's less obvious
what the "stupid context" is.  Consider
        C :: forall a. Ord a => a -> a -> T (Foo a)
Does the C constructor in Core contain the Ord dictionary?  Yes, it must:

        f :: T b -> Ordering
        f = /\b. \x:T b.
            case x of
                C a (d:Ord a) (p:a) (q:a) -> compare d p q

Note that (Foo a) might not be an instance of Ord.

************************************************************************
*                                                                      *
\subsection{Data constructors}
*                                                                      *
************************************************************************
-}

-- | A data constructor
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
--             'GHC.Parser.Annotation.AnnClose','GHC.Parser.Annotation.AnnComma'

-- For details on above see note [Api annotations] in GHC.Parser.Annotation
data DataCon
  = MkData {
        dcName    :: Name,      -- This is the name of the *source data con*
                                -- (see "Note [Data Constructor Naming]" above)
        dcUnique :: Unique,     -- Cached from Name
        dcTag    :: ConTag,     -- ^ Tag, used for ordering 'DataCon's

        -- Running example:
        --
        --      *** As declared by the user
        --  data T a b c where
        --    MkT :: forall c y x b. (x~y,Ord x) => x -> y -> T (x,y) b c

        --      *** As represented internally
        --  data T a b c where
        --    MkT :: forall a b c. forall x y. (a~(x,y),x~y,Ord x)
        --        => x -> y -> T a b c
        --
        -- The next six fields express the type of the constructor, in pieces
        -- e.g.
        --
        --      dcUnivTyVars       = [a,b,c]
        --      dcExTyCoVars       = [x,y]
        --      dcUserTyVarBinders = [c,y,x,b]
        --      dcEqSpec           = [a~(x,y)]
        --      dcOtherTheta       = [x~y, Ord x]
        --      dcOrigArgTys       = [x,y]
        --      dcRepTyCon         = T

        -- In general, the dcUnivTyVars are NOT NECESSARILY THE SAME AS THE
        -- TYVARS FOR THE PARENT TyCon. (This is a change (Oct05): previously,
        -- vanilla datacons guaranteed to have the same type variables as their
        -- parent TyCon, but that seems ugly.) They can be different in the case
        -- where a GADT constructor uses different names for the universal
        -- tyvars than does the tycon. For example:
        --
        --   data H a where
        --     MkH :: b -> H b
        --
        -- Here, the tyConTyVars of H will be [a], but the dcUnivTyVars of MkH
        -- will be [b].

        dcVanilla :: Bool,      -- True <=> This is a vanilla Haskell 98 data constructor
                                --          Its type is of form
                                --              forall a1..an . t1 -> ... tm -> T a1..an
                                --          No existentials, no coercions, nothing.
                                -- That is: dcExTyCoVars = dcEqSpec = dcOtherTheta = []
                -- NB 1: newtypes always have a vanilla data con
                -- NB 2: a vanilla constructor can still be declared in GADT-style
                --       syntax, provided its type looks like the above.
                --       The declaration format is held in the TyCon (algTcGadtSyntax)

        -- Universally-quantified type vars [a,b,c]
        -- INVARIANT: length matches arity of the dcRepTyCon
        -- INVARIANT: result type of data con worker is exactly (T a b c)
        -- COROLLARY: The dcUnivTyVars are always in one-to-one correspondence with
        --            the tyConTyVars of the parent TyCon
        dcUnivTyVars     :: [TyVar],

        -- Existentially-quantified type and coercion vars [x,y]
        -- For an example involving coercion variables,
        -- Why tycovars? See Note [Existential coercion variables]
        dcExTyCoVars     :: [TyCoVar],

        -- INVARIANT: the UnivTyVars and ExTyCoVars all have distinct OccNames
        -- Reason: less confusing, and easier to generate Iface syntax

        -- The type/coercion vars in the order the user wrote them [c,y,x,b]
        -- INVARIANT: the set of tyvars in dcUserTyVarBinders is exactly the set
        --            of tyvars (*not* covars) of dcExTyCoVars unioned with the
        --            set of dcUnivTyVars whose tyvars do not appear in dcEqSpec
        -- See Note [DataCon user type variable binders]
        dcUserTyVarBinders :: [InvisTVBinder],

        dcEqSpec :: [EqSpec],   -- Equalities derived from the result type,
                                -- _as written by the programmer_.
                                -- Only non-dependent GADT equalities (dependent
                                -- GADT equalities are in the covars of
                                -- dcExTyCoVars).

                -- This field allows us to move conveniently between the two ways
                -- of representing a GADT constructor's type:
                --      MkT :: forall a b. (a ~ [b]) => b -> T a
                --      MkT :: forall b. b -> T [b]
                -- Each equality is of the form (a ~ ty), where 'a' is one of
                -- the universally quantified type variables. Moreover, the
                -- only place in the DataCon where this 'a' will occur is in
                -- dcUnivTyVars. See [The dcEqSpec domain invariant].

                -- The next two fields give the type context of the data constructor
                --      (aside from the GADT constraints,
                --       which are given by the dcExpSpec)
                -- In GADT form, this is *exactly* what the programmer writes, even if
                -- the context constrains only universally quantified variables
                --      MkT :: forall a b. (a ~ b, Ord b) => a -> T a b
        dcOtherTheta :: ThetaType,  -- The other constraints in the data con's type
                                    -- other than those in the dcEqSpec

        dcStupidTheta :: ThetaType,     -- The context of the data type declaration
                                        --      data Eq a => T a = ...
                                        -- or, rather, a "thinned" version thereof
                -- "Thinned", because the Report says
                -- to eliminate any constraints that don't mention
                -- tyvars free in the arg types for this constructor
                --
                -- INVARIANT: the free tyvars of dcStupidTheta are a subset of dcUnivTyVars
                -- Reason: dcStupidTeta is gotten by thinning the stupid theta from the tycon
                --
                -- "Stupid", because the dictionaries aren't used for anything.
                -- Indeed, [as of March 02] they are no longer in the type of
                -- the wrapper Id, because that makes it harder to use the wrap-id
                -- to rebuild values after record selection or in generics.

        dcOrigArgTys :: [Scaled Type],  -- Original argument types
                                        -- (before unboxing and flattening of strict fields)
        dcOrigResTy :: Type,            -- Original result type, as seen by the user
                -- NB: for a data instance, the original user result type may
                -- differ from the DataCon's representation TyCon.  Example
                --      data instance T [a] where MkT :: a -> T [a]
                -- The dcOrigResTy is T [a], but the dcRepTyCon might be R:TList

        -- Now the strictness annotations and field labels of the constructor
        dcSrcBangs :: [HsSrcBang],
                -- See Note [Bangs on data constructor arguments]
                --
                -- The [HsSrcBang] as written by the programmer.
                --
                -- Matches 1-1 with dcOrigArgTys
                -- Hence length = dataConSourceArity dataCon

        dcFields  :: [FieldLabel],
                -- Field labels for this constructor, in the
                -- same order as the dcOrigArgTys;
                -- length = 0 (if not a record) or dataConSourceArity.

        -- The curried worker function that corresponds to the constructor:
        -- It doesn't have an unfolding; the code generator saturates these Ids
        -- and allocates a real constructor when it finds one.
        dcWorkId :: Id,

        -- Constructor representation
        dcRep      :: DataConRep,

        -- Cached; see Note [DataCon arities]
        -- INVARIANT: dcRepArity    == length dataConRepArgTys + count isCoVar (dcExTyCoVars)
        -- INVARIANT: dcSourceArity == length dcOrigArgTys
        dcRepArity    :: Arity,
        dcSourceArity :: Arity,

        -- Result type of constructor is T t1..tn
        dcRepTyCon  :: TyCon,           -- Result tycon, T

        dcRepType   :: Type,    -- Type of the constructor
                                --      forall a x y. (a~(x,y), x~y, Ord x) =>
                                --        x -> y -> T a
                                -- (this is *not* of the constructor wrapper Id:
                                --  see Note [Data con representation] below)
        -- Notice that the existential type parameters come *second*.
        -- Reason: in a case expression we may find:
        --      case (e :: T t) of
        --        MkT x y co1 co2 (d:Ord x) (v:r) (w:F s) -> ...
        -- It's convenient to apply the rep-type of MkT to 't', to get
        --      forall x y. (t~(x,y), x~y, Ord x) => x -> y -> T t
        -- and use that to check the pattern.  Mind you, this is really only
        -- used in GHC.Core.Lint.


        dcInfix :: Bool,        -- True <=> declared infix
                                -- Used for Template Haskell and 'deriving' only
                                -- The actual fixity is stored elsewhere

        dcPromoted :: TyCon    -- The promoted TyCon
                               -- See Note [Promoted data constructors] in GHC.Core.TyCon
  }


{- Note [TyVarBinders in DataCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the TyVarBinders in a DataCon and PatSyn:

 * Each argument flag is Inferred or Specified.
   None are Required. (A DataCon is a term-level function; see
   Note [No Required TyCoBinder in terms] in GHC.Core.TyCo.Rep.)

Why do we need the TyVarBinders, rather than just the TyVars?  So that
we can construct the right type for the DataCon with its foralls
attributed the correct visibility.  That in turn governs whether you
can use visible type application at a call of the data constructor.

See also [DataCon user type variable binders] for an extended discussion on the
order in which TyVarBinders appear in a DataCon.

Note [Existential coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For now (Aug 2018) we can't write coercion quantifications in source Haskell, but
we can in Core. Consider having:

  data T :: forall k. k -> k -> Constraint where
    MkT :: forall k (a::k) (b::k). forall k' (c::k') (co::k'~k). (b~(c|>co))
        => T k a b

  dcUnivTyVars       = [k,a,b]
  dcExTyCoVars       = [k',c,co]
  dcUserTyVarBinders = [k,a,k',c]
  dcEqSpec           = [b~(c|>co)]
  dcOtherTheta       = []
  dcOrigArgTys       = []
  dcRepTyCon         = T

  Function call 'dataConKindEqSpec' returns [k'~k]

Note [DataCon arities]
~~~~~~~~~~~~~~~~~~~~~~
dcSourceArity does not take constraints into account,
but dcRepArity does.  For example:
   MkT :: Ord a => a -> T a
    dcSourceArity = 1
    dcRepArity    = 2

Note [DataCon user type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In System FC, data constructor type signatures always quantify over all of
their universal type variables, followed by their existential type variables.
Normally, this isn't a problem, as most datatypes naturally quantify their type
variables in this order anyway. For example:

  data T a b = forall c. MkT b c

Here, we have `MkT :: forall {k} (a :: k) (b :: *) (c :: *). b -> c -> T a b`,
where k, a, and b are universal and c is existential. (The inferred variable k
isn't available for TypeApplications, hence why it's in braces.) This is a
perfectly reasonable order to use, as the syntax of H98-style datatypes
(+ ExistentialQuantification) suggests it.

Things become more complicated when GADT syntax enters the picture. Consider
this example:

  data X a where
    MkX :: forall b a. b -> Proxy a -> X a

If we adopt the earlier approach of quantifying all the universal variables
followed by all the existential ones, GHC would come up with this type
signature for MkX:

  MkX :: forall {k} (a :: k) (b :: *). b -> Proxy a -> X a

But this is not what we want at all! After all, if a user were to use
TypeApplications on MkX, they would expect to instantiate `b` before `a`,
as that's the order in which they were written in the `forall`. (See #11721.)
Instead, we'd like GHC to come up with this type signature:

  MkX :: forall {k} (b :: *) (a :: k). b -> Proxy a -> X a

In fact, even if we left off the explicit forall:

  data X a where
    MkX :: b -> Proxy a -> X a

Then a user should still expect `b` to be quantified before `a`, since
according to the rules of TypeApplications, in the absence of `forall` GHC
performs a stable topological sort on the type variables in the user-written
type signature, which would place `b` before `a`.

But as noted above, enacting this behavior is not entirely trivial, as System
FC demands the variables go in universal-then-existential order under the hood.
Our solution is thus to equip DataCon with two different sets of type
variables:

* dcUnivTyVars and dcExTyCoVars, for the universal type variable and existential
  type/coercion variables, respectively. Their order is irrelevant for the
  purposes of TypeApplications, and as a consequence, they do not come equipped
  with visibilities (that is, they are TyVars/TyCoVars instead of
  TyCoVarBinders).

* dcUserTyVarBinders, for the type variables binders in the order in which they
  originally arose in the user-written type signature. Their order *does* matter
  for TypeApplications, so they are full TyVarBinders, complete with
  visibilities.

This encoding has some redundancy. The set of tyvars in dcUserTyVarBinders
consists precisely of:

* The set of tyvars in dcUnivTyVars whose type variables do not appear in
  dcEqSpec, unioned with:
* The set of tyvars (*not* covars) in dcExTyCoVars
  No covars here because because they're not user-written

The word "set" is used above because the order in which the tyvars appear in
dcUserTyVarBinders can be completely different from the order in dcUnivTyVars or
dcExTyCoVars. That is, the tyvars in dcUserTyVarBinders are a permutation of
(tyvars of dcExTyCoVars + a subset of dcUnivTyVars). But aside from the
ordering, they in fact share the same type variables (with the same Uniques). We
sometimes refer to this as "the dcUserTyVarBinders invariant".

dcUserTyVarBinders, as the name suggests, is the one that users will
see most of the time. It's used when computing the type signature of a
data constructor wrapper (see dataConWrapperType), and as a result,
it's what matters from a TypeApplications perspective.

Note [The dcEqSpec domain invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example of a GADT constructor:

  data Y a where
    MkY :: Bool -> Y Bool

The user-written type of MkY is `Bool -> Y Bool`, but what is the underlying
Core type for MkY? There are two conceivable possibilities:

1. MkY :: forall a. (a ~# Bool) => Bool -> Y a
2. MkY :: forall a. (a ~# Bool) => a    -> Y a

In practice, GHC picks (1) as the Core type for MkY. This is because we
maintain an invariant that the type variables in the domain of dcEqSpec will
only ever appear in the dcUnivTyVars. As a consequence, the type variables in
the domain of dcEqSpec will /never/ appear in the dcExTyCoVars, dcOtherTheta,
dcOrigArgTys, or dcOrigResTy; these can only ever mention variables from
dcUserTyVarBinders, which excludes things in the domain of dcEqSpec.
(See Note [DataCon user type variable binders].) This explains why GHC would
not pick (2) as the Core type, since the argument type `a` mentions a type
variable in the dcEqSpec.

There are certain parts of the codebase where it is convenient to apply the
substitution arising from the dcEqSpec to the dcUnivTyVars in order to obtain
the user-written return type of a GADT constructor. A consequence of the
dcEqSpec domain invariant is that you /never/ need to apply the substitution
to any other part of the constructor type, as they don't require it.
-}

-- | Data Constructor Representation
-- See Note [Data constructor workers and wrappers]
data DataConRep
  = -- NoDataConRep means that the data con has no wrapper
    NoDataConRep

    -- DCR means that the data con has a wrapper
  | DCR { dcr_wrap_id :: Id   -- Takes src args, unboxes/flattens,
                              -- and constructs the representation

        , dcr_boxer   :: DataConBoxer

        , dcr_arg_tys :: [Scaled Type]    -- Final, representation argument types,
                                          -- after unboxing and flattening,
                                          -- and *including* all evidence args

        , dcr_stricts :: [StrictnessMark]  -- 1-1 with dcr_arg_tys
                -- See also Note [Data-con worker strictness]

        , dcr_bangs :: [HsImplBang]  -- The actual decisions made (including failures)
                                     -- about the original arguments; 1-1 with orig_arg_tys
                                     -- See Note [Bangs on data constructor arguments]

    }

-------------------------

-- | Haskell Source Bang
--
-- Bangs on data constructor arguments as the user wrote them in the
-- source code.
--
-- @(HsSrcBang _ SrcUnpack SrcLazy)@ and
-- @(HsSrcBang _ SrcUnpack NoSrcStrict)@ (without StrictData) makes no sense, we
-- emit a warning (in checkValidDataCon) and treat it like
-- @(HsSrcBang _ NoSrcUnpack SrcLazy)@
data HsSrcBang =
  HsSrcBang SourceText -- Note [Pragma source text] in GHC.Types.SourceText
            SrcUnpackedness
            SrcStrictness
  deriving Data.Data

-- | Haskell Implementation Bang
--
-- Bangs of data constructor arguments as generated by the compiler
-- after consulting HsSrcBang, flags, etc.
data HsImplBang
  = HsLazy    -- ^ Lazy field, or one with an unlifted type
  | HsStrict  -- ^ Strict but not unpacked field
  | HsUnpack (Maybe Coercion)
    -- ^ Strict and unpacked field
    -- co :: arg-ty ~ product-ty HsBang
  deriving Data.Data

-- | Source Strictness
--
-- What strictness annotation the user wrote
data SrcStrictness = SrcLazy -- ^ Lazy, ie '~'
                   | SrcStrict -- ^ Strict, ie '!'
                   | NoSrcStrict -- ^ no strictness annotation
     deriving (Eq, Data.Data)

-- | Source Unpackedness
--
-- What unpackedness the user requested
data SrcUnpackedness = SrcUnpack -- ^ {-# UNPACK #-} specified
                     | SrcNoUnpack -- ^ {-# NOUNPACK #-} specified
                     | NoSrcUnpack -- ^ no unpack pragma
     deriving (Eq, Data.Data)



-------------------------
-- StrictnessMark is internal only, used to indicate strictness
-- of the DataCon *worker* fields
data StrictnessMark = MarkedStrict | NotMarkedStrict

-- | An 'EqSpec' is a tyvar/type pair representing an equality made in
-- rejigging a GADT constructor
data EqSpec = EqSpec TyVar
                     Type

-- | Make a non-dependent 'EqSpec'
mkEqSpec :: TyVar -> Type -> EqSpec
mkEqSpec tv ty = EqSpec tv ty

eqSpecTyVar :: EqSpec -> TyVar
eqSpecTyVar (EqSpec tv _) = tv

eqSpecType :: EqSpec -> Type
eqSpecType (EqSpec _ ty) = ty

eqSpecPair :: EqSpec -> (TyVar, Type)
eqSpecPair (EqSpec tv ty) = (tv, ty)

eqSpecPreds :: [EqSpec] -> ThetaType
eqSpecPreds spec = [ mkPrimEqPred (mkTyVarTy tv) ty
                   | EqSpec tv ty <- spec ]

-- | Substitute in an 'EqSpec'. Precondition: if the LHS of the EqSpec
-- is mapped in the substitution, it is mapped to a type variable, not
-- a full type.
substEqSpec :: TCvSubst -> EqSpec -> EqSpec
substEqSpec subst (EqSpec tv ty)
  = EqSpec tv' (substTy subst ty)
  where
    tv' = getTyVar "substEqSpec" (substTyVar subst tv)

-- | Filter out any 'TyVar's mentioned in an 'EqSpec'.
filterEqSpec :: [EqSpec] -> [TyVar] -> [TyVar]
filterEqSpec eq_spec
  = filter not_in_eq_spec
  where
    not_in_eq_spec var = all (not . (== var) . eqSpecTyVar) eq_spec

instance Outputable EqSpec where
  ppr (EqSpec tv ty) = ppr (tv, ty)

{- Note [Data-con worker strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that we do *not* say the worker Id is strict even if the data
constructor is declared strict
     e.g.    data T = MkT !(Int,Int)
Why?  Because the *wrapper* $WMkT is strict (and its unfolding has case
expressions that do the evals) but the *worker* MkT itself is not. If we
pretend it is strict then when we see
     case x of y -> MkT y
the simplifier thinks that y is "sure to be evaluated" (because the worker MkT
is strict) and drops the case.  No, the workerId MkT is not strict.

However, the worker does have StrictnessMarks.  When the simplifier sees a
pattern
     case e of MkT x -> ...
it uses the dataConRepStrictness of MkT to mark x as evaluated; but that's
fine... dataConRepStrictness comes from the data con not from the worker Id.

Note [Bangs on data constructor arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T = MkT !Int {-# UNPACK #-} !Int Bool

When compiling the module, GHC will decide how to represent
MkT, depending on the optimisation level, and settings of
flags like -funbox-small-strict-fields.

Terminology:
  * HsSrcBang:  What the user wrote
                Constructors: HsSrcBang

  * HsImplBang: What GHC decided
                Constructors: HsLazy, HsStrict, HsUnpack

* If T was defined in this module, MkT's dcSrcBangs field
  records the [HsSrcBang] of what the user wrote; in the example
    [ HsSrcBang _ NoSrcUnpack SrcStrict
    , HsSrcBang _ SrcUnpack SrcStrict
    , HsSrcBang _ NoSrcUnpack NoSrcStrictness]

* However, if T was defined in an imported module, the importing module
  must follow the decisions made in the original module, regardless of
  the flag settings in the importing module.
  Also see Note [Bangs on imported data constructors] in GHC.Types.Id.Make

* The dcr_bangs field of the dcRep field records the [HsImplBang]
  If T was defined in this module, Without -O the dcr_bangs might be
    [HsStrict, HsStrict, HsLazy]
  With -O it might be
    [HsStrict, HsUnpack _, HsLazy]
  With -funbox-small-strict-fields it might be
    [HsUnpack, HsUnpack _, HsLazy]
  With -XStrictData it might be
    [HsStrict, HsUnpack _, HsStrict]

Note [Data con representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The dcRepType field contains the type of the representation of a constructor
This may differ from the type of the constructor *Id* (built
by MkId.mkDataConId) for two reasons:
        a) the constructor Id may be overloaded, but the dictionary isn't stored
           e.g.    data Eq a => T a = MkT a a

        b) the constructor may store an unboxed version of a strict field.

Here's an example illustrating both:
        data Ord a => T a = MkT Int! a
Here
        T :: Ord a => Int -> a -> T a
but the rep type is
        Trep :: Int# -> a -> T a
Actually, the unboxed part isn't implemented yet!



************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Eq DataCon where
    a == b = getUnique a == getUnique b
    a /= b = getUnique a /= getUnique b

instance Uniquable DataCon where
    getUnique = dcUnique

instance NamedThing DataCon where
    getName = dcName

instance Outputable DataCon where
    ppr con = ppr (dataConName con)

instance OutputableBndr DataCon where
    pprInfixOcc con = pprInfixName (dataConName con)
    pprPrefixOcc con = pprPrefixName (dataConName con)

instance Data.Data DataCon where
    -- don't traverse?
    toConstr _   = abstractConstr "DataCon"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "DataCon"

instance Outputable HsSrcBang where
    ppr (HsSrcBang _ prag mark) = ppr prag <+> ppr mark

instance Outputable HsImplBang where
    ppr HsLazy                  = text "Lazy"
    ppr (HsUnpack Nothing)      = text "Unpacked"
    ppr (HsUnpack (Just co))    = text "Unpacked" <> parens (ppr co)
    ppr HsStrict                = text "StrictNotUnpacked"

instance Outputable SrcStrictness where
    ppr SrcLazy     = char '~'
    ppr SrcStrict   = char '!'
    ppr NoSrcStrict = empty

instance Outputable SrcUnpackedness where
    ppr SrcUnpack   = text "{-# UNPACK #-}"
    ppr SrcNoUnpack = text "{-# NOUNPACK #-}"
    ppr NoSrcUnpack = empty

instance Outputable StrictnessMark where
    ppr MarkedStrict    = text "!"
    ppr NotMarkedStrict = empty

instance Binary SrcStrictness where
    put_ bh SrcLazy     = putByte bh 0
    put_ bh SrcStrict   = putByte bh 1
    put_ bh NoSrcStrict = putByte bh 2

    get bh =
      do h <- getByte bh
         case h of
           0 -> return SrcLazy
           1 -> return SrcStrict
           _ -> return NoSrcStrict

instance Binary SrcUnpackedness where
    put_ bh SrcNoUnpack = putByte bh 0
    put_ bh SrcUnpack   = putByte bh 1
    put_ bh NoSrcUnpack = putByte bh 2

    get bh =
      do h <- getByte bh
         case h of
           0 -> return SrcNoUnpack
           1 -> return SrcUnpack
           _ -> return NoSrcUnpack

-- | Compare strictness annotations
eqHsBang :: HsImplBang -> HsImplBang -> Bool
eqHsBang HsLazy               HsLazy              = True
eqHsBang HsStrict             HsStrict            = True
eqHsBang (HsUnpack Nothing)   (HsUnpack Nothing)  = True
eqHsBang (HsUnpack (Just c1)) (HsUnpack (Just c2))
  = eqType (coercionType c1) (coercionType c2)
eqHsBang _ _                                       = False

isBanged :: HsImplBang -> Bool
isBanged (HsUnpack {}) = True
isBanged (HsStrict {}) = True
isBanged HsLazy        = False

isSrcStrict :: SrcStrictness -> Bool
isSrcStrict SrcStrict = True
isSrcStrict _ = False

isSrcUnpacked :: SrcUnpackedness -> Bool
isSrcUnpacked SrcUnpack = True
isSrcUnpacked _ = False

isMarkedStrict :: StrictnessMark -> Bool
isMarkedStrict NotMarkedStrict = False
isMarkedStrict _               = True   -- All others are strict

{- *********************************************************************
*                                                                      *
\subsection{Construction}
*                                                                      *
********************************************************************* -}

-- | Build a new data constructor
mkDataCon :: Name
          -> Bool           -- ^ Is the constructor declared infix?
          -> TyConRepName   -- ^  TyConRepName for the promoted TyCon
          -> [HsSrcBang]    -- ^ Strictness/unpack annotations, from user
          -> [FieldLabel]   -- ^ Field labels for the constructor,
                            -- if it is a record, otherwise empty
          -> [TyVar]        -- ^ Universals.
          -> [TyCoVar]      -- ^ Existentials.
          -> [InvisTVBinder]    -- ^ User-written 'TyVarBinder's.
                                --   These must be Inferred/Specified.
                                --   See @Note [TyVarBinders in DataCons]@
          -> [EqSpec]           -- ^ GADT equalities
          -> KnotTied ThetaType -- ^ Theta-type occurring before the arguments proper
          -> [KnotTied (Scaled Type)]    -- ^ Original argument types
          -> KnotTied Type      -- ^ Original result type
          -> RuntimeRepInfo     -- ^ See comments on 'GHC.Core.TyCon.RuntimeRepInfo'
          -> KnotTied TyCon     -- ^ Representation type constructor
          -> ConTag             -- ^ Constructor tag
          -> ThetaType          -- ^ The "stupid theta", context of the data
                                -- declaration e.g. @data Eq a => T a ...@
          -> Id                 -- ^ Worker Id
          -> DataConRep         -- ^ Representation
          -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name declared_infix prom_info
          arg_stricts   -- Must match orig_arg_tys 1-1
          fields
          univ_tvs ex_tvs user_tvbs
          eq_spec theta
          orig_arg_tys orig_res_ty rep_info rep_tycon tag
          stupid_theta work_id rep
-- Warning: mkDataCon is not a good place to check certain invariants.
-- If the programmer writes the wrong result type in the decl, thus:
--      data T a where { MkT :: S }
-- then it's possible that the univ_tvs may hit an assertion failure
-- if you pull on univ_tvs.  This case is checked by checkValidDataCon,
-- so the error is detected properly... it's just that assertions here
-- are a little dodgy.

  = con
  where
    is_vanilla = null ex_tvs && null eq_spec && null theta

    con = MkData {dcName = name, dcUnique = nameUnique name,
                  dcVanilla = is_vanilla, dcInfix = declared_infix,
                  dcUnivTyVars = univ_tvs,
                  dcExTyCoVars = ex_tvs,
                  dcUserTyVarBinders = user_tvbs,
                  dcEqSpec = eq_spec,
                  dcOtherTheta = theta,
                  dcStupidTheta = stupid_theta,
                  dcOrigArgTys = orig_arg_tys, dcOrigResTy = orig_res_ty,
                  dcRepTyCon = rep_tycon,
                  dcSrcBangs = arg_stricts,
                  dcFields = fields, dcTag = tag, dcRepType = rep_ty,
                  dcWorkId = work_id,
                  dcRep = rep,
                  dcSourceArity = length orig_arg_tys,
                  dcRepArity = length rep_arg_tys + count isCoVar ex_tvs,
                  dcPromoted = promoted }

        -- The 'arg_stricts' passed to mkDataCon are simply those for the
        -- source-language arguments.  We add extra ones for the
        -- dictionary arguments right here.

    rep_arg_tys = dataConRepArgTys con

    rep_ty =
      case rep of
        -- If the DataCon has no wrapper, then the worker's type *is* the
        -- user-facing type, so we can simply use dataConWrapperType.
        NoDataConRep -> dataConWrapperType con
        -- If the DataCon has a wrapper, then the worker's type is never seen
        -- by the user. The visibilities we pick do not matter here.
        DCR{} -> mkInfForAllTys univ_tvs $ mkTyCoInvForAllTys ex_tvs $
                 mkVisFunTys rep_arg_tys $
                 mkTyConApp rep_tycon (mkTyVarTys univ_tvs)

      -- See Note [Promoted data constructors] in GHC.Core.TyCon
    prom_tv_bndrs = [ mkNamedTyConBinder (Invisible spec) tv
                    | Bndr tv spec <- user_tvbs ]

    fresh_names = freshNames (map getName user_tvbs)
      -- fresh_names: make sure that the "anonymous" tyvars don't
      -- clash in name or unique with the universal/existential ones.
      -- Tiresome!  And unnecessary because these tyvars are never looked at
    prom_theta_bndrs = [ mkAnonTyConBinder InvisArg (mkTyVar n t)
     {- Invisible -}   | (n,t) <- fresh_names `zip` theta ]
    prom_arg_bndrs   = [ mkAnonTyConBinder VisArg (mkTyVar n t)
     {- Visible -}     | (n,t) <- dropList theta fresh_names `zip` map scaledThing orig_arg_tys ]
    prom_bndrs       = prom_tv_bndrs ++ prom_theta_bndrs ++ prom_arg_bndrs
    prom_res_kind    = orig_res_ty
    promoted         = mkPromotedDataCon con name prom_info prom_bndrs
                                         prom_res_kind roles rep_info

    roles = map (\tv -> if isTyVar tv then Nominal else Phantom)
                (univ_tvs ++ ex_tvs)
            ++ map (const Representational) (theta ++ map scaledThing orig_arg_tys)

freshNames :: [Name] -> [Name]
-- Make an infinite list of Names whose Uniques and OccNames
-- differ from those in the 'avoid' list
freshNames avoids
  = [ mkSystemName uniq occ
    | n <- [0..]
    , let uniq = mkAlphaTyVarUnique n
          occ = mkTyVarOccFS (mkFastString ('x' : show n))

    , not (uniq `elementOfUniqSet` avoid_uniqs)
    , not (occ `elemOccSet` avoid_occs) ]

  where
    avoid_uniqs :: UniqSet Unique
    avoid_uniqs = mkUniqSet (map getUnique avoids)

    avoid_occs :: OccSet
    avoid_occs = mkOccSet (map getOccName avoids)

-- | The 'Name' of the 'DataCon', giving it a unique, rooted identification
dataConName :: DataCon -> Name
dataConName = dcName

-- | The tag used for ordering 'DataCon's
dataConTag :: DataCon -> ConTag
dataConTag  = dcTag

dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG

-- | The type constructor that we are building via this data constructor
dataConTyCon :: DataCon -> TyCon
dataConTyCon = dcRepTyCon

-- | The original type constructor used in the definition of this data
-- constructor.  In case of a data family instance, that will be the family
-- type constructor.
dataConOrigTyCon :: DataCon -> TyCon
dataConOrigTyCon dc
  | Just (tc, _) <- tyConFamInst_maybe (dcRepTyCon dc) = tc
  | otherwise                                          = dcRepTyCon dc

-- | The representation type of the data constructor, i.e. the sort
-- type that will represent values of this type at runtime
dataConRepType :: DataCon -> Type
dataConRepType = dcRepType

-- | Should the 'DataCon' be presented infix?
dataConIsInfix :: DataCon -> Bool
dataConIsInfix = dcInfix

-- | The universally-quantified type variables of the constructor
dataConUnivTyVars :: DataCon -> [TyVar]
dataConUnivTyVars (MkData { dcUnivTyVars = tvbs }) = tvbs

-- | The existentially-quantified type/coercion variables of the constructor
-- including dependent (kind-) GADT equalities
dataConExTyCoVars :: DataCon -> [TyCoVar]
dataConExTyCoVars (MkData { dcExTyCoVars = tvbs }) = tvbs

-- | Both the universal and existential type/coercion variables of the constructor
dataConUnivAndExTyCoVars :: DataCon -> [TyCoVar]
dataConUnivAndExTyCoVars (MkData { dcUnivTyVars = univ_tvs, dcExTyCoVars = ex_tvs })
  = univ_tvs ++ ex_tvs

-- See Note [DataCon user type variable binders]
-- | The type variables of the constructor, in the order the user wrote them
dataConUserTyVars :: DataCon -> [TyVar]
dataConUserTyVars (MkData { dcUserTyVarBinders = tvbs }) = binderVars tvbs

-- See Note [DataCon user type variable binders]
-- | 'InvisTVBinder's for the type variables of the constructor, in the order the
-- user wrote them
dataConUserTyVarBinders :: DataCon -> [InvisTVBinder]
dataConUserTyVarBinders = dcUserTyVarBinders

-- | Equalities derived from the result type of the data constructor, as written
-- by the programmer in any GADT declaration. This includes *all* GADT-like
-- equalities, including those written in by hand by the programmer.
dataConEqSpec :: DataCon -> [EqSpec]
dataConEqSpec con@(MkData { dcEqSpec = eq_spec, dcOtherTheta = theta })
  = dataConKindEqSpec con
    ++ eq_spec ++
    [ spec   -- heterogeneous equality
    | Just (tc, [_k1, _k2, ty1, ty2]) <- map splitTyConApp_maybe theta
    , tc `hasKey` heqTyConKey
    , spec <- case (getTyVar_maybe ty1, getTyVar_maybe ty2) of
                    (Just tv1, _) -> [mkEqSpec tv1 ty2]
                    (_, Just tv2) -> [mkEqSpec tv2 ty1]
                    _             -> []
    ] ++
    [ spec   -- homogeneous equality
    | Just (tc, [_k, ty1, ty2]) <- map splitTyConApp_maybe theta
    , tc `hasKey` eqTyConKey
    , spec <- case (getTyVar_maybe ty1, getTyVar_maybe ty2) of
                    (Just tv1, _) -> [mkEqSpec tv1 ty2]
                    (_, Just tv2) -> [mkEqSpec tv2 ty1]
                    _             -> []
    ]

-- | Dependent (kind-level) equalities in a constructor.
-- There are extracted from the existential variables.
-- See Note [Existential coercion variables]
dataConKindEqSpec :: DataCon -> [EqSpec]
dataConKindEqSpec (MkData {dcExTyCoVars = ex_tcvs})
  -- It is used in 'dataConEqSpec' (maybe also 'dataConFullSig' in the future),
  -- which are frequently used functions.
  -- For now (Aug 2018) this function always return empty set as we don't really
  -- have coercion variables.
  -- In the future when we do, we might want to cache this information in DataCon
  -- so it won't be computed every time when aforementioned functions are called.
  = [ EqSpec tv ty
    | cv <- ex_tcvs
    , isCoVar cv
    , let (_, _, ty1, ty, _) = coVarKindsTypesRole cv
          tv = getTyVar "dataConKindEqSpec" ty1
    ]

-- | The *full* constraints on the constructor type, including dependent GADT
-- equalities.
dataConTheta :: DataCon -> ThetaType
dataConTheta con@(MkData { dcEqSpec = eq_spec, dcOtherTheta = theta })
  = eqSpecPreds (dataConKindEqSpec con ++ eq_spec) ++ theta

-- | Get the Id of the 'DataCon' worker: a function that is the "actual"
-- constructor and has no top level binding in the program. The type may
-- be different from the obvious one written in the source program. Panics
-- if there is no such 'Id' for this 'DataCon'
dataConWorkId :: DataCon -> Id
dataConWorkId dc = dcWorkId dc

-- | Get the Id of the 'DataCon' wrapper: a function that wraps the "actual"
-- constructor so it has the type visible in the source program: c.f.
-- 'dataConWorkId'.
-- Returns Nothing if there is no wrapper, which occurs for an algebraic data
-- constructor and also for a newtype (whose constructor is inlined
-- compulsorily)
dataConWrapId_maybe :: DataCon -> Maybe Id
dataConWrapId_maybe dc = case dcRep dc of
                           NoDataConRep -> Nothing
                           DCR { dcr_wrap_id = wrap_id } -> Just wrap_id

-- | Returns an Id which looks like the Haskell-source constructor by using
-- the wrapper if it exists (see 'dataConWrapId_maybe') and failing over to
-- the worker (see 'dataConWorkId')
dataConWrapId :: DataCon -> Id
dataConWrapId dc = case dcRep dc of
                     NoDataConRep-> dcWorkId dc    -- worker=wrapper
                     DCR { dcr_wrap_id = wrap_id } -> wrap_id

-- | Find all the 'Id's implicitly brought into scope by the data constructor. Currently,
-- the union of the 'dataConWorkId' and the 'dataConWrapId'
dataConImplicitTyThings :: DataCon -> [TyThing]
dataConImplicitTyThings (MkData { dcWorkId = work, dcRep = rep })
  = [mkAnId work] ++ wrap_ids
  where
    wrap_ids = case rep of
                 NoDataConRep               -> []
                 DCR { dcr_wrap_id = wrap } -> [mkAnId wrap]

-- | The labels for the fields of this particular 'DataCon'
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels = dcFields

-- | Extract the type for any given labelled field of the 'DataCon'
dataConFieldType :: DataCon -> FieldLabelString -> Type
dataConFieldType con label = case dataConFieldType_maybe con label of
      Just (_, ty) -> ty
      Nothing      -> pprPanic "dataConFieldType" (ppr con <+> ppr label)

-- | Extract the label and type for any given labelled field of the
-- 'DataCon', or return 'Nothing' if the field does not belong to it
dataConFieldType_maybe :: DataCon -> FieldLabelString
                       -> Maybe (FieldLabel, Type)
dataConFieldType_maybe con label
  = find ((== label) . flLabel . fst) (dcFields con `zip` (scaledThing <$> dcOrigArgTys con))

-- | Strictness/unpack annotations, from user; or, for imported
-- DataCons, from the interface file
-- The list is in one-to-one correspondence with the arity of the 'DataCon'

dataConSrcBangs :: DataCon -> [HsSrcBang]
dataConSrcBangs = dcSrcBangs

-- | Source-level arity of the data constructor
dataConSourceArity :: DataCon -> Arity
dataConSourceArity (MkData { dcSourceArity = arity }) = arity

-- | Gives the number of actual fields in the /representation/ of the
-- data constructor. This may be more than appear in the source code;
-- the extra ones are the existentially quantified dictionaries
dataConRepArity :: DataCon -> Arity
dataConRepArity (MkData { dcRepArity = arity }) = arity

-- | Return whether there are any argument types for this 'DataCon's original source type
-- See Note [DataCon arities]
isNullarySrcDataCon :: DataCon -> Bool
isNullarySrcDataCon dc = dataConSourceArity dc == 0

-- | Return whether there are any argument types for this 'DataCon's runtime representation type
-- See Note [DataCon arities]
isNullaryRepDataCon :: DataCon -> Bool
isNullaryRepDataCon dc = dataConRepArity dc == 0

dataConRepStrictness :: DataCon -> [StrictnessMark]
-- ^ Give the demands on the arguments of a
-- Core constructor application (Con dc args)
dataConRepStrictness dc = case dcRep dc of
                            NoDataConRep -> [NotMarkedStrict | _ <- dataConRepArgTys dc]
                            DCR { dcr_stricts = strs } -> strs

dataConImplBangs :: DataCon -> [HsImplBang]
-- The implementation decisions about the strictness/unpack of each
-- source program argument to the data constructor
dataConImplBangs dc
  = case dcRep dc of
      NoDataConRep              -> replicate (dcSourceArity dc) HsLazy
      DCR { dcr_bangs = bangs } -> bangs

dataConBoxer :: DataCon -> Maybe DataConBoxer
dataConBoxer (MkData { dcRep = DCR { dcr_boxer = boxer } }) = Just boxer
dataConBoxer _ = Nothing

dataConInstSig
  :: DataCon
  -> [Type]    -- Instantiate the *universal* tyvars with these types
  -> ([TyCoVar], ThetaType, [Type])  -- Return instantiated existentials
                                     -- theta and arg tys
-- ^ Instantiate the universal tyvars of a data con,
--   returning
--     ( instantiated existentials
--     , instantiated constraints including dependent GADT equalities
--         which are *also* listed in the instantiated existentials
--     , instantiated args)
dataConInstSig con@(MkData { dcUnivTyVars = univ_tvs, dcExTyCoVars = ex_tvs
                           , dcOrigArgTys = arg_tys })
               univ_tys
  = ( ex_tvs'
    , substTheta subst (dataConTheta con)
    , substTys subst (map scaledThing arg_tys))
  where
    univ_subst = zipTvSubst univ_tvs univ_tys
    (subst, ex_tvs') = Type.substVarBndrs univ_subst ex_tvs


-- | The \"full signature\" of the 'DataCon' returns, in order:
--
-- 1) The result of 'dataConUnivTyVars'
--
-- 2) The result of 'dataConExTyCoVars'
--
-- 3) The non-dependent GADT equalities.
--    Dependent GADT equalities are implied by coercion variables in
--    return value (2).
--
-- 4) The other constraints of the data constructor type, excluding GADT
-- equalities
--
-- 5) The original argument types to the 'DataCon' (i.e. before
--    any change of the representation of the type) with linearity
--    annotations
--
-- 6) The original result type of the 'DataCon'
dataConFullSig :: DataCon
               -> ([TyVar], [TyCoVar], [EqSpec], ThetaType, [Scaled Type], Type)
dataConFullSig (MkData {dcUnivTyVars = univ_tvs, dcExTyCoVars = ex_tvs,
                        dcEqSpec = eq_spec, dcOtherTheta = theta,
                        dcOrigArgTys = arg_tys, dcOrigResTy = res_ty})
  = (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, res_ty)

dataConOrigResTy :: DataCon -> Type
dataConOrigResTy dc = dcOrigResTy dc

-- | The \"stupid theta\" of the 'DataCon', such as @data Eq a@ in:
--
-- > data Eq a => T a = ...
dataConStupidTheta :: DataCon -> ThetaType
dataConStupidTheta dc = dcStupidTheta dc

{-
Note [Displaying linear fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor with a linear field can be written either as
MkT :: a %1 -> T a (with -XLinearTypes)
or
MkT :: a  -> T a (with -XNoLinearTypes)

There are two different methods to retrieve a type of a datacon.
They differ in how linear fields are handled.

1. dataConWrapperType:
The type of the wrapper in Core.
For example, dataConWrapperType for Maybe is a %1 -> Just a.

2. dataConNonlinearType:
The type of the constructor, with linear arrows replaced by unrestricted ones.
Used when we don't want to introduce linear types to user (in holes
and in types in hie used by haddock).

3. dataConDisplayType (take a boolean indicating if -XLinearTypes is enabled):
The type we'd like to show in error messages, :info and -ddump-types.
Ideally, it should reflect the type written by the user;
the function returns a type with arrows that would be required
to write this constructor under the current setting of -XLinearTypes.
In principle, this type can be different from the user's source code
when the value of -XLinearTypes has changed, but we don't
expect this to cause much trouble.

Due to internal plumbing in checkValidDataCon, we can't just return a Doc.
The multiplicity of arrows returned by dataConDisplayType and
dataConDisplayType is used only for pretty-printing.
-}

dataConWrapperType :: DataCon -> Type
-- ^ The user-declared type of the data constructor
-- in the nice-to-read form:
--
-- > T :: forall a b. a -> b -> T [a]
--
-- rather than:
--
-- > T :: forall a c. forall b. (c~[a]) => a -> b -> T c
--
-- The type variables are quantified in the order that the user wrote them.
-- See @Note [DataCon user type variable binders]@.
--
-- NB: If the constructor is part of a data instance, the result type
-- mentions the family tycon, not the internal one.
dataConWrapperType (MkData { dcUserTyVarBinders = user_tvbs,
                             dcOtherTheta = theta, dcOrigArgTys = arg_tys,
                             dcOrigResTy = res_ty })
  = mkInvisForAllTys user_tvbs $
    mkInvisFunTysMany theta $
    mkVisFunTys arg_tys $
    res_ty

dataConNonlinearType :: DataCon -> Type
dataConNonlinearType (MkData { dcUserTyVarBinders = user_tvbs,
                               dcOtherTheta = theta, dcOrigArgTys = arg_tys,
                               dcOrigResTy = res_ty })
  = let arg_tys' = map (\(Scaled w t) -> Scaled (case w of One -> Many; _ -> w) t) arg_tys
    in mkInvisForAllTys user_tvbs $
       mkInvisFunTysMany theta $
       mkVisFunTys arg_tys' $
       res_ty

dataConDisplayType :: Bool -> DataCon -> Type
dataConDisplayType show_linear_types dc
  = if show_linear_types
    then dataConWrapperType dc
    else dataConNonlinearType dc

-- | Finds the instantiated types of the arguments required to construct a
-- 'DataCon' representation
-- NB: these INCLUDE any dictionary args
--     but EXCLUDE the data-declaration context, which is discarded
-- It's all post-flattening etc; this is a representation type
dataConInstArgTys :: DataCon    -- ^ A datacon with no existentials or equality constraints
                                -- However, it can have a dcTheta (notably it can be a
                                -- class dictionary, with superclasses)
                  -> [Type]     -- ^ Instantiated at these types
                  -> [Scaled Type]
dataConInstArgTys dc@(MkData {dcUnivTyVars = univ_tvs,
                              dcExTyCoVars = ex_tvs}) inst_tys
 = ASSERT2( univ_tvs `equalLength` inst_tys
          , text "dataConInstArgTys" <+> ppr dc $$ ppr univ_tvs $$ ppr inst_tys)
   ASSERT2( null ex_tvs, ppr dc )
   map (mapScaledType (substTyWith univ_tvs inst_tys)) (dataConRepArgTys dc)

-- | Returns just the instantiated /value/ argument types of a 'DataCon',
-- (excluding dictionary args)
dataConInstOrigArgTys
        :: DataCon      -- Works for any DataCon
        -> [Type]       -- Includes existential tyvar args, but NOT
                        -- equality constraints or dicts
        -> [Scaled Type]
-- For vanilla datacons, it's all quite straightforward
-- But for the call in GHC.HsToCore.Match.Constructor, we really do want just
-- the value args
dataConInstOrigArgTys dc@(MkData {dcOrigArgTys = arg_tys,
                                  dcUnivTyVars = univ_tvs,
                                  dcExTyCoVars = ex_tvs}) inst_tys
  = ASSERT2( tyvars `equalLength` inst_tys
           , text "dataConInstOrigArgTys" <+> ppr dc $$ ppr tyvars $$ ppr inst_tys )
    substScaledTys subst arg_tys
  where
    tyvars = univ_tvs ++ ex_tvs
    subst  = zipTCvSubst tyvars inst_tys

-- | Returns the argument types of the wrapper, excluding all dictionary arguments
-- and without substituting for any type variables
dataConOrigArgTys :: DataCon -> [Scaled Type]
dataConOrigArgTys dc = dcOrigArgTys dc

-- | Returns constraints in the wrapper type, other than those in the dataConEqSpec
dataConOtherTheta :: DataCon -> ThetaType
dataConOtherTheta dc = dcOtherTheta dc

-- | Returns the arg types of the worker, including *all* non-dependent
-- evidence, after any flattening has been done and without substituting for
-- any type variables
dataConRepArgTys :: DataCon -> [Scaled Type]
dataConRepArgTys (MkData { dcRep = rep
                         , dcEqSpec = eq_spec
                         , dcOtherTheta = theta
                         , dcOrigArgTys = orig_arg_tys })
  = case rep of
      NoDataConRep -> ASSERT( null eq_spec ) (map unrestricted theta) ++ orig_arg_tys
      DCR { dcr_arg_tys = arg_tys } -> arg_tys

-- | The string @package:module.name@ identifying a constructor, which is attached
-- to its info table and used by the GHCi debugger and the heap profiler
dataConIdentity :: DataCon -> ByteString
-- We want this string to be UTF-8, so we get the bytes directly from the FastStrings.
dataConIdentity dc = LBS.toStrict $ BSB.toLazyByteString $ mconcat
   [ BSB.shortByteString $ fastStringToShortByteString $
       unitFS $ moduleUnit mod
   , BSB.int8 $ fromIntegral (ord ':')
   , BSB.shortByteString $ fastStringToShortByteString $
       moduleNameFS $ moduleName mod
   , BSB.int8 $ fromIntegral (ord '.')
   , BSB.shortByteString $ fastStringToShortByteString $
       occNameFS $ nameOccName name
   ]
  where name = dataConName dc
        mod  = ASSERT( isExternalName name ) nameModule name

isTupleDataCon :: DataCon -> Bool
isTupleDataCon (MkData {dcRepTyCon = tc}) = isTupleTyCon tc

isBoxedTupleDataCon :: DataCon -> Bool
isBoxedTupleDataCon (MkData {dcRepTyCon = tc}) = isBoxedTupleTyCon tc

isUnboxedTupleDataCon :: DataCon -> Bool
isUnboxedTupleDataCon (MkData {dcRepTyCon = tc}) = isUnboxedTupleTyCon tc

isUnboxedSumDataCon :: DataCon -> Bool
isUnboxedSumDataCon (MkData {dcRepTyCon = tc}) = isUnboxedSumTyCon tc

-- | Vanilla 'DataCon's are those that are nice boring Haskell 98 constructors
isVanillaDataCon :: DataCon -> Bool
isVanillaDataCon dc = dcVanilla dc

-- | Is this the 'DataCon' of a newtype?
isNewDataCon :: DataCon -> Bool
isNewDataCon dc = isNewTyCon (dataConTyCon dc)

-- | Should this DataCon be allowed in a type even without -XDataKinds?
-- Currently, only Lifted & Unlifted
specialPromotedDc :: DataCon -> Bool
specialPromotedDc = isKindTyCon . dataConTyCon

classDataCon :: Class -> DataCon
classDataCon clas = case tyConDataCons (classTyCon clas) of
                      (dict_constr:no_more) -> ASSERT( null no_more ) dict_constr
                      [] -> panic "classDataCon"

dataConCannotMatch :: [Type] -> DataCon -> Bool
-- Returns True iff the data con *definitely cannot* match a
--                  scrutinee of type (T tys)
--                  where T is the dcRepTyCon for the data con
dataConCannotMatch tys con
  -- See (U6) in Note [Implementing unsafeCoerce]
  -- in base:Unsafe.Coerce
  | dataConName con == unsafeReflDataConName
                      = False
  | null inst_theta   = False   -- Common
  | all isTyVarTy tys = False   -- Also common
  | otherwise         = typesCantMatch (concatMap predEqs inst_theta)
  where
    (_, inst_theta, _) = dataConInstSig con tys

    -- TODO: could gather equalities from superclasses too
    predEqs pred = case classifyPredType pred of
                     EqPred NomEq ty1 ty2         -> [(ty1, ty2)]
                     ClassPred eq args
                       | eq `hasKey` eqTyConKey
                       , [_, ty1, ty2] <- args    -> [(ty1, ty2)]
                       | eq `hasKey` heqTyConKey
                       , [_, _, ty1, ty2] <- args -> [(ty1, ty2)]
                     _                            -> []

-- | Were the type variables of the data con written in a different order
-- than the regular order (universal tyvars followed by existential tyvars)?
--
-- This is not a cheap test, so we minimize its use in GHC as much as possible.
-- Currently, its only call site in the GHC codebase is in 'mkDataConRep' in
-- "MkId", and so 'dataConUserTyVarsArePermuted' is only called at most once
-- during a data constructor's lifetime.

-- See Note [DataCon user type variable binders], as well as
-- Note [Data con wrappers and GADT syntax] for an explanation of what
-- mkDataConRep is doing with this function.
dataConUserTyVarsArePermuted :: DataCon -> Bool
dataConUserTyVarsArePermuted (MkData { dcUnivTyVars = univ_tvs
                                     , dcExTyCoVars = ex_tvs, dcEqSpec = eq_spec
                                     , dcUserTyVarBinders = user_tvbs }) =
  (filterEqSpec eq_spec univ_tvs ++ ex_tvs) /= binderVars user_tvbs

{-
%************************************************************************
%*                                                                      *
        Promoting of data types to the kind level
*                                                                      *
************************************************************************

-}

promoteDataCon :: DataCon -> TyCon
promoteDataCon (MkData { dcPromoted = tc }) = tc

{-
************************************************************************
*                                                                      *
\subsection{Splitting products}
*                                                                      *
************************************************************************
-}

-- | Extract the type constructor, type argument, data constructor and it's
-- /representation/ argument types from a type if it is a product type.
--
-- Precisely, we return @Just@ for any data type that is all of:
--
--  * Concrete (i.e. constructors visible)
--  * Single-constructor
--  * ... which has no existentials
--
-- Whether the type is a @data@ type or a @newtype@.
splitDataProductType_maybe
        :: Type                         -- ^ A product type, perhaps
        -> Maybe (TyCon,                -- The type constructor
                  [Type],               -- Type args of the tycon
                  DataCon,              -- The data constructor
                  [Scaled Type])        -- Its /representation/ arg types

        -- Rejecting existentials means we don't have to worry about
        -- freshening and substituting type variables
        -- (See "GHC.Type.Id.Make.dataConArgUnpack")

splitDataProductType_maybe ty
  | Just (tycon, ty_args) <- splitTyConApp_maybe ty
  , Just con <- tyConSingleDataCon_maybe tycon
  , null (dataConExTyCoVars con) -- no existentials! See above
  = Just (tycon, ty_args, con, dataConInstArgTys con ty_args)
  | otherwise
  = Nothing
