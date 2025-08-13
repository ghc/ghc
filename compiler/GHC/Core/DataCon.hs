{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[DataCon]{@DataCon@: Data Constructors}
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable, Binary

module GHC.Core.DataCon (
        -- * Main data types
        DataCon, DataConRep(..),
        SrcStrictness(..), SrcUnpackedness(..),
        HsSrcBang(..), HsImplBang(..),
        StrictnessMark(..),
        ConTag,
        DataConEnv,

        -- ** Equality specs
        EqSpec, mkEqSpec, eqSpecTyVar, eqSpecType,
        eqSpecPair, eqSpecPreds,

        -- ** Field labels
        FieldLabel(..), flLabel, FieldLabelString,

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
        dataConConcreteTyVars,
        dataConUserTyVars, dataConUserTyVarBinders,
        dataConTheta,
        dataConStupidTheta,
        dataConOtherTheta,
        dataConInstArgTys, dataConOrigArgTys, dataConOrigResTy,
        dataConInstOrigArgTys, dataConRepArgTys, dataConResRepTyArgs,
        dataConInstUnivs,
        dataConFieldLabels, dataConFieldType, dataConFieldType_maybe,
        dataConSrcBangs,
        dataConSourceArity, dataConVisArity, dataConRepArity,
        dataConIsInfix,
        dataConWorkId, dataConWrapId, dataConWrapId_maybe,
        dataConImplicitTyThings,
        dataConRepStrictness,
        dataConImplBangs, dataConBoxer,

        splitDataProductType_maybe,

        -- ** Predicates on DataCons
        isNullarySrcDataCon, isNullaryRepDataCon,
        isLazyDataConRep,
        isTupleDataCon, isBoxedTupleDataCon, isUnboxedTupleDataCon,
        isUnboxedSumDataCon, isCovertGadtDataCon, isUnaryClassDataCon,
        isVanillaDataCon, isNewDataCon, isTypeDataCon,
        classDataCon, dataConCannotMatch,
        dataConUserTyVarBindersNeedWrapper, checkDataConTyVars,
        isBanged, isUnpacked, isMarkedStrict, cbvFromStrictMark, eqHsBang, isSrcStrict, isSrcUnpacked,
        specialPromotedDc,

        -- ** Promotion related functions
        promoteDataCon
    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Basic
import Language.Haskell.Syntax.Module.Name

import {-# SOURCE #-} GHC.Types.Id.Make ( DataConBoxer )
import GHC.Core.Type as Type
import GHC.Core.Coercion
import GHC.Core.Unify
import GHC.Core.TyCon
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.Compare( eqType, eqForAllVis )
import GHC.Core.Multiplicity
import {-# SOURCE #-} GHC.Types.TyThing
import GHC.Types.FieldLabel
import GHC.Types.SourceText
import GHC.Core.Class
import GHC.Types.Name
import GHC.Builtin.Names
import GHC.Core.Predicate
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Data.FastString
import GHC.Unit.Types
import GHC.Utils.Binary
import GHC.Types.Unique.FM ( UniqFM )
import GHC.Types.Unique.Set
import GHC.Builtin.Uniques( mkAlphaTyVarUnique )
import GHC.Data.Graph.UnVar  -- UnVarSet and operations

import {-# SOURCE #-} GHC.Tc.Utils.TcType ( ConcreteTyVars )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Data as Data
import Data.Char
import Data.List( find )
import Control.DeepSeq

{-
Note [Data constructor representation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
differently, as follows in Note [Data Constructor Naming].

The `dcRepType` field of a `DataCon` contains the type of the representation of
the constructor /worker/, also called the Core representation.

The Core representation may differ from the type of the constructor /wrapper/
(built by `mkDataConRep`). Besides unpacking (as seen in the example above),
dictionaries and coercions become explict arguments in the Core representation
of a constructor.

Note that this representation is still *different* from runtime
representation. (Which is what STG uses after unarise).
See Note [Constructor applications in STG] in GHC.Stg.Syntax.


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

* See Note [Data Constructor Naming] for how the worker and wrapper
  are named

* The workers don't take the dcStupidTheta dicts as arguments, while the
  wrappers currently do

* The wrapper (if it exists) takes dcOrigArgTys as its arguments.
  The worker takes dataConRepArgTys as its arguments
  If the wrapper is absent, dataConRepArgTys is the same as dcOrigArgTys

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
  Notice that the worker has two fields where the wrapper has
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

* Datatype contexts require dropping some dictionary arguments.
  See Note [Instantiating stupid theta].

Note [The stupid context]
~~~~~~~~~~~~~~~~~~~~~~~~~
Data types can have a context:

        data (Eq a, Ord b) => T a b = T1 a b | T2 a

And that makes the constructors have a context too. A constructor's context
isn't necessarily the same as the data type's context, however. Per the
Haskell98 Report, the part of the datatype context that is used in a data
constructor is the largest subset of the datatype context that constrains
only the type variables free in the data constructor's field types. For
example, here are the types of T1 and T2:

        T1 :: (Eq a, Ord b) => a -> b -> T a b
        T2 :: (Eq a) => a -> T a b

Notice that T2's context is "thinned". Since its field is of type `a`, only
the part of the datatype context that mentions `a`—that is, `Eq a`—is
included in T2's context. On the other hand, T1's fields mention both `a`
and `b`, so T1's context includes all of the datatype context.

Furthermore, this context pops up when pattern matching
(though GHC hasn't implemented this, but it is in H98, and
I've fixed GHC so that it now does):

        f (T2 x) = x
gets inferred type
        f :: Eq a => T a b -> a

I say the context is "stupid" because the dictionaries passed
are immediately discarded -- they do nothing and have no benefit.
(See Note [Instantiating stupid theta].)
It's a flaw in the language.

GHC has made some efforts to correct this flaw. In GHC, datatype contexts
are not available by default. Instead, one must explicitly opt in to them by
using the DatatypeContexts extension. To discourage their use, GHC has
deprecated DatatypeContexts.

Some other notes about stupid contexts:

* Stupid contexts can interact badly with `deriving`. For instance, it's
  unclear how to make this derived Functor instance typecheck:

    data Eq a => T a = MkT a
      deriving Functor

  This is because the derived instance would need to look something like
  `instance Functor T where ...`, but there is nowhere to mention the
  requisite `Eq a` constraint. For this reason, GHC will throw an error if a
  user attempts to derive an instance for Functor (or a Functor-like class)
  where the last type variable is used in a datatype context. For Generic(1),
  the requirements are even harsher, as stupid contexts are not allowed at all
  in derived Generic(1) instances. (We could consider relaxing this requirement
  somewhat, although no one has asked for this yet.)

  Stupid contexts are permitted when deriving instances of non-Functor-like
  classes, or when deriving instances of Functor-like classes where the last
  type variable isn't mentioned in the stupid context. For example, the
  following is permitted:

    data Show a => T a = MkT deriving Eq

  Note that because of the "thinning" behavior mentioned above, the generated
  Eq instance should not mention `Show a`, as the type of MkT doesn't require
  it. That is, the following should be generated (#20501):

    instance Eq (T a) where
      (MkT == MkT) = True

* It's not obvious how stupid contexts should interact with GADTs. For this
  reason, GHC disallows combining datatype contexts with GADT syntax. As a
  result, dcStupidTheta is always empty for data types defined using GADT
  syntax.

Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a data type with a "stupid theta" (see
Note [The stupid context]):

  data Ord a => T a = MkT (Maybe a)

We want to generate an Ord constraint for every use of MkT; but
we also want to allow visible type application, such as

   MkT @Int

To achieve this, the wrapper for a data (or newtype) constructor
with a datatype context contains a lambda which drops the dictionary
argments corresponding to the datatype context:

   /\a \(_d:Ord a). MkT @a

Notice that the wrapper discards the dictionary argument d.
We don't need it; it was only there to generate a Wanted constraint.
(That is why it is stupid.)

This all happens in GHC.Types.Id.Make.mkDataConRep.

************************************************************************
*                                                                      *
\subsection{Data constructors}
*                                                                      *
************************************************************************
-}

-- | A data constructor
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

        -- dcUnivTyVars: Universally-quantified type vars [a,b,c]
        -- INVARIANT: length matches arity of the dcRepTyCon
        -- INVARIANT: result type of data con worker is exactly (T a b c)
        -- COROLLARY: The dcUnivTyVars are always in one-to-one correspondence with
        --            the tyConTyVars of the parent TyCon
        dcUnivTyVars     :: [TyVar],

        -- Existentially-quantified type and coercion vars [x,y]
        -- For an example involving coercion variables,
        -- Why TyCoVars? See Note [Existential coercion variables]
        dcExTyCoVars     :: [TyCoVar],

        -- INVARIANT: the UnivTyVars and ExTyCoVars all have distinct OccNames
        -- Reason: less confusing, and easier to generate Iface syntax

        -- The type variables of this data constructor that must be
        -- instantiated to concrete types. For example: the RuntimeRep
        -- variables of unboxed tuples and unboxed sums.
        --
        -- See Note [Representation-polymorphism checking built-ins]
        -- in GHC.Tc.Utils.Concrete.
        dcConcreteTyVars :: ConcreteTyVars,

        -- The type/coercion vars in the order the user wrote them [c,y,x,b]
        -- INVARIANT(dataConTyVars): the set of tyvars in dcUserTyVarBinders is
        --    exactly the set of tyvars (*not* covars) of dcExTyCoVars unioned
        --    with the set of dcUnivTyVars whose tyvars do not appear in dcEqSpec
        -- So dcUserTyVarBinders is a subset of (dcUnivTyVars ++ dcExTyCoVars)
        -- See Note [DataCon user type variable binders]
        dcUserTyVarBinders :: [TyVarBinder],

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
                -- tyvars free in the arg types for this constructor.
                -- See Note [The stupid context].
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

        dcImplBangs :: [HsImplBang],
                -- The actual decisions made (including failures)
                -- about the original arguments; 1-1 with orig_arg_tys
                -- See Note [Bangs on data constructor arguments]

        dcStricts :: [StrictnessMark],
                -- One mark for every field of the DataCon worker;
                -- if it's empty, then all fields are lazy,
                -- otherwise 1-1 with dataConRepArgTys.
                -- See also Note [Strict fields in Core] in GHC.Core
                -- for the effect on the strictness signature

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
                                --  see Note [Data constructor representation])
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


{- Note [Existential coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For now (Aug 2018) we can't write coercion quantifications in source Haskell, but
we can in Core. Consider having:

  data T :: forall k. k -> k -> Constraint where
    MkT :: forall k (a::k) (b::k).
           forall k' (c::k') (co::k'~k).
           (b ~# (c|>co)) => T k a b

  dcUnivTyVars       = [k,a,b]
  dcExTyCoVars       = [k',c,co]
  dcUserTyVarBinders = [k,a,k',c]
  dcEqSpec           = [b ~# (c|>co)]
  dcOtherTheta       = []
  dcOrigArgTys       = []
  dcRepTyCon         = T

Function call 'dataConKindEqSpec' returns [k'~k]

Note [DataCon arities]
~~~~~~~~~~~~~~~~~~~~~~
A `DataCon`'s source and core representation may differ, meaning the source
arity (`dcSourceArity`) and the core representation arity (`dcRepArity`) may
differ too.

Note that the source arity isn't exactly the number of arguments the data con
/wrapper/ has, since `dcSourceArity` doesn't count constraints -- which may
appear in the wrapper through `DatatypeContexts`, or if the constructor stores a
dictionary. In this sense, the source arity counts the number of non-constraint
arguments that appear at the source level.
  On the other hand, the Core representation arity is the number of arguments
of the data constructor in its Core representation, which is also the number
of arguments of the data con /worker/.

The arity might differ since `dcRepArity` takes into account arguments such as
quantified dictionaries and coercion arguments, lifted and unlifted (despite
the unlifted coercion arguments having a zero-width runtime representation).
For example:
   MkT :: Ord a => a -> T a
    dcSourceArity = 1
    dcRepArity    = 2

   MkU :: (b ~ '[]) => U b
    dcSourceArity = 0
    dcRepArity    = 1

The arity might also differ due to unpacking, for example, consider the
following datatype and its wrapper and worker's type:
   data V = MkV !() !Int
   $WMkV :: () -> Int -> V
     MkV :: Int# -> V
As you see, because of unpacking we have both dropped the unit argument and
unboxed the Int. In this case, the source arity (which is the arity of the
wrapper) is 2, while the Core representation arity (the arity of the worker) is 1.


Note [DataCon user type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A DataCon has two different sets of type variables:

* dcUserTyVarBinders, for the type variables binders in the order in which they
  originally arose in the user-written type signature, and with user-specified
  visibilities.

  - They are the forall'd binders of the data con /wrapper/, which the user calls.

  - With RequiredTypeArguments, some of the foralls may be visible, e.g.
      MkT :: forall a b. forall c -> (a, b, c) -> T a b c
    so the binders are full TyVarBinders, complete with visibilities.

  - Even if we only consider invisible foralls, the order and specificity of
    binders matter for TypeApplications.

* dcUnivTyVars and dcExTyCoVars, for the "true underlying" (i.e. of the data
  con worker) universal type variable and existential type/coercion variables,
  respectively.

  - They (i.e. univ ++ ex) are the forall'd variables of the data con /worker/

  - They do not come equipped with visibilities:
        dcUnivTyVars :: [TyVar]     -- not [TyVarBinder]
        dcExTyCoVars :: [TyCoVar]   -- not [ForAllTyBinder]
    Instead, we treat them as having the Specified (coreTyLamForAllTyFlag)
    visibility. For example:
        wrapper type: forall {a} b. forall c -> ...
        worker type:  forall a b c. ...
    This is a design choice. Reasons:
      * Workers are never called by the user. They are part of the Core
        language where visibilities don't matter as much.
      * Consistency with type lambdas in Core. As Note [Required foralls in Core]
        in GHC.Core.TyCo.Rep explains, (/\a. e) :: (forall a. e_ty), and we need
        a coercion to cast it to (forall a -> e_ty).
    As a consequence, we may need to adjust visibilities with a cast in the
    wrapper. See Note [Flag cast in data con wrappers].

Often (dcUnivTyVars ++ dcExTyCoVars) = binderVars dcUserTyVarBinders; but they
may differ for two reasons, coming next:

--- Reason (R1): Order of quantification in GADT syntax ---

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

--- Reason (R2): GADT constructors quantify over different variables ---

GADT constructors may quantify over different variables than the worker
would.  Consider
   data T a b where
      MkT :: forall c d. c -> T [c] d

The dcUserTyVarBinders must be [c, d] -- that's what the user quantified over.
But c is actually existential, as it is not equal to either of the two
universal variables.

Here is what we'll get:

  dcUserTyVarBinders = [c, d]
  dcUnivTyVars = [a, d]
  dcExTyCoVars = [c]

Note that dcUnivTyVars contains `a` from the type header (the `data T a b`)
and `d` from the signature for MkT. This is done because d is used in place
of b in the result of MkT, and so we use the name d for the universal, as that
might improve error messages. On the other hand, we need to use a fresh name
for the first universal (recalling that the result of a worker must be the
type constructor applied to a sequence of plain variables), so we use `a`, from
the header. This choice of universals is made in GHC.Tc.TyCl.mkGADTVars.

Because c is not a universal, it is an existential. Here, we see that (even
ignoring order) dcUserTyVarBinders is not dcUnivTyVars ⋃ dcExTyCoVars, because
the latter has `a` while the former does not. To understand this better, let's
look at this type for the "true underlying" worker data con:

      MkT :: forall a d. forall c. (a ~# [c]) => c -> T a d

We see here that the `a` universal is connected with the `c` existential via
an equality constraint. It will always be the case (see the code in mkGADTVars)
that the universals not mentioned in dcUserTyVarBinders will be used in a
GADT equality -- that is, used on the left-hand side of an element of dcEqSpec:

  dcEqSpec = [a ~# [c]]

Putting this all together, all variables used on the left-hand side of an
equation in the dcEqSpec will be in dcUnivTyVars but *not* in
dcUserTyVarBinders.

--- End of Reasons ---

INVARIANT(dataConTyVars): the set of tyvars in dcUserTyVarBinders
consists of:

* The set of tyvars in dcUnivTyVars whose type variables do not appear in
  dcEqSpec, unioned with:

* The set of tyvars (*not* covars) in dcExTyCoVars
  No covars here because because they're not user-written

When comparing for equality, we ignore differences concerning type variables
whose kinds have kind Constraint.

The word "set" is used above because the order in which the tyvars appear in
dcUserTyVarBinders can be completely different from the order in dcUnivTyVars or
dcExTyCoVars. That is, the tyvars in dcUserTyVarBinders are a permutation of
(tyvars of dcExTyCoVars + a subset of dcUnivTyVars). But aside from the
ordering, they in fact share the same type variables (with the same Uniques). We
sometimes refer to this as "the dcUserTyVarBinders invariant". It is checked
in checkDataConTyVars.

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

    }

type DataConEnv a = UniqFM DataCon a     -- Keyed by DataCon

-------------------------

-- | Haskell Source Bang
--
-- Bangs on data constructor arguments as written by the user, including the
-- source code for exact-printing.
--
-- @(HsSrcBang _ SrcUnpack SrcLazy)@ and
-- @(HsSrcBang _ SrcUnpack NoSrcStrict)@ (without StrictData) makes no sense, we
-- emit a warning (in checkValidDataCon) and treat it like
-- @(HsSrcBang _ NoSrcUnpack SrcLazy)@
--
-- In the AST, the @SourceText@ is hidden inside the extension point
-- 'Language.Haskell.Syntax.Extension.XConDeclField'.
data HsSrcBang
  = HsSrcBang SourceText SrcUnpackedness SrcStrictness -- See Note [Pragma source text] in "GHC.Types.SourceText"
  deriving Data.Data

-- | Haskell Implementation Bang
--
-- Bangs of data constructor arguments as generated by the compiler
-- after consulting HsSrcBang, flags, etc.
data HsImplBang
  = HsLazy    -- ^ Lazy field, or one with an unlifted type
  | HsStrict Bool -- ^ Strict but not unpacked field
                  -- True <=> we could have unpacked, but opted not to
                  -- because of -O0.
                  -- See Note [Detecting useless UNPACK pragmas]
  | HsUnpack (Maybe Coercion)
    -- ^ Strict and unpacked field
    -- co :: arg-ty ~ product-ty HsBang
  deriving Data.Data



-------------------------
-- StrictnessMark is used to indicate strictness
-- of the DataCon *worker* fields
data StrictnessMark = MarkedStrict | NotMarkedStrict
    deriving Eq

-- | An 'EqSpec' is a tyvar/type pair representing an equality made in
-- rejigging a GADT constructor
data EqSpec = EqSpec TyVar Type

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
eqSpecPreds spec = [ mkNomEqPred (mkTyVarTy tv) ty
                   | EqSpec tv ty <- spec ]

instance Outputable EqSpec where
  ppr (EqSpec tv ty) = ppr (tv, ty)

{- Note [Bangs on data constructor arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

* The dcImplBangs field records the [HsImplBang]
  If T was defined in this module, Without -O the dcImplBangs might be
    [HsStrict _, HsStrict _, HsLazy]
  With -O it might be
    [HsStrict _, HsUnpack _, HsLazy]
  With -funbox-small-strict-fields it might be
    [HsUnpack, HsUnpack _, HsLazy]
  With -XStrictData it might be
    [HsStrict _, HsUnpack _, HsStrict _]

* Core passes will often need to know whether the DataCon worker or wrapper in
  an application is strict in some (lifted) field or not. This is tracked in the
  demand signature attached to a DataCon's worker resp. wrapper Id.

  So if you've got a DataCon dc, you can get the demand signature by
  `idDmdSig (dataConWorkId dc)` and make out strict args by testing with
  `isStrictDmd`. Similarly, `idDmdSig <$> dataConWrapId_maybe dc` gives
  you the demand signature of the wrapper, if it exists.

  These demand signatures are set in GHC.Types.Id.Make.mkDataConWorkId,
  computed from the single source of truth `dataConRepStrictness`, which is
  generated from `dcStricts`.
  Note that `dataConRepStrictness` lines up 1-1 with `idDmdSig (dataConWorkId dc)`.

Note [Detecting useless UNPACK pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to issue a warning when there's an UNPACK pragma in the source code,
but we decided not to unpack.
However, when compiling with -O0, we never unpack, and that'd generate
spurious warnings.
Therefore, we remember in HsStrict a boolean flag, whether we _could_
have unpacked. This flag is set in GHC.Types.Id.Make.dataConSrcToImplBang.
Then, in GHC.Tc.TyCl.checkValidDataCon (sub-function check_bang),
if the user wrote an `{-# UNPACK #-}` pragma (i.e. HsSrcBang contains SrcUnpack)
we consult HsImplBang:

  HsUnpack _     => field unpacked, no warning
                    Example: data T = MkT {-# UNPACK #-} !Int   [with -O]
  HsStrict True  => field not unpacked because -O0, no warning
                    Example: data T = MkT {-# UNPACK #-} !Int   [with -O0]
  HsStrict False => field not unpacked, warning
                    Example: data T = MkT {-# UNPACK #-} !(Int -> Int)
  HsLazy         => field not unpacked, warning
                    This can happen in two scenarios:

                    1) UNPACK without a bang
                    Example: data T = MkT {-# UNPACK #-} Int
                    This will produce a warning about missing ! before UNPACK.

                    2) UNPACK of an unlifted datatype
                    Because of bug #20204, we currently do not unpack type T,
                    and therefore issue a warning:
                    type IntU :: UnliftedType
                    data IntU = IntU Int#
                    data T = Test {-# UNPACK #-} IntU

The boolean flag is used only for this warning.
See #11270 for motivation.

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
    ppr (HsStrict b)            = text "StrictNotUnpacked" <> parens (ppr b)

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

instance Binary StrictnessMark where
    put_ bh NotMarkedStrict = putByte bh 0
    put_ bh MarkedStrict    = putByte bh 1
    get bh =
      do h <- getByte bh
         case h of
           0 -> return NotMarkedStrict
           1 -> return MarkedStrict
           _ -> panic "Invalid binary format"

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

instance NFData SrcStrictness where
  rnf SrcLazy = ()
  rnf SrcStrict = ()
  rnf NoSrcStrict = ()

instance NFData SrcUnpackedness where
  rnf SrcNoUnpack = ()
  rnf SrcUnpack = ()
  rnf NoSrcUnpack = ()

-- | Compare strictness annotations
eqHsBang :: HsImplBang -> HsImplBang -> Bool
eqHsBang HsLazy               HsLazy              = True
eqHsBang (HsStrict _)         (HsStrict _)        = True
eqHsBang (HsUnpack Nothing)   (HsUnpack Nothing)  = True
eqHsBang (HsUnpack (Just c1)) (HsUnpack (Just c2))
  = eqType (coercionType c1) (coercionType c2)
eqHsBang _ _                                       = False

isBanged :: HsImplBang -> Bool
isBanged (HsUnpack {}) = True
isBanged (HsStrict {}) = True
isBanged HsLazy        = False

isUnpacked :: HsImplBang -> Bool
isUnpacked (HsUnpack {}) = True
isUnpacked (HsStrict {}) = False
isUnpacked HsLazy        = False

isSrcStrict :: SrcStrictness -> Bool
isSrcStrict SrcStrict = True
isSrcStrict _ = False

isSrcUnpacked :: SrcUnpackedness -> Bool
isSrcUnpacked SrcUnpack = True
isSrcUnpacked _ = False

isMarkedStrict :: StrictnessMark -> Bool
isMarkedStrict NotMarkedStrict = False
isMarkedStrict _               = True   -- All others are strict

cbvFromStrictMark :: StrictnessMark -> CbvMark
cbvFromStrictMark NotMarkedStrict = NotMarkedCbv
cbvFromStrictMark MarkedStrict = MarkedCbv


{- *********************************************************************
*                                                                      *
\subsection{Construction}
*                                                                      *
********************************************************************* -}

-- | Build a new data constructor
mkDataCon :: Name
          -> Bool               -- ^ Is the constructor declared infix?
          -> TyConRepName       -- ^  TyConRepName for the promoted TyCon
          -> [HsSrcBang]        -- ^ Strictness/unpack annotations, from user
          -> [HsImplBang]       -- ^ Strictness/unpack annotations, as inferred by the compiler
          -> [StrictnessMark]   -- ^ Strictness marks for the DataCon worker's fields in Core
          -> [FieldLabel]       -- ^ Field labels for the constructor,
                                -- if it is a record, otherwise empty
          -> [TyVar]            -- ^ Universals.
          -> [TyCoVar]          -- ^ Existentials.
          -> ConcreteTyVars
                                -- ^ TyVars which must be instantiated with
                                -- concrete types
          -> [TyVarBinder]      -- ^ User-written 'TyVarBinder's
          -> [EqSpec]           -- ^ GADT equalities
          -> KnotTied ThetaType -- ^ Theta-type occurring before the arguments proper
          -> [KnotTied (Scaled Type)]    -- ^ Original argument types
          -> KnotTied Type      -- ^ Original result type
          -> PromDataConInfo    -- ^ See comments on 'GHC.Core.TyCon.PromDataConInfo'
          -> KnotTied TyCon     -- ^ Representation type constructor
          -> ConTag             -- ^ Constructor tag
          -> ThetaType          -- ^ The "stupid theta", context of the data
                                -- declaration e.g. @data Eq a => T a ...@
          -> Id                 -- ^ Worker Id
          -> DataConRep         -- ^ Representation
          -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name declared_infix prom_info
          arg_stricts  -- Must match orig_arg_tys 1-1
          impl_bangs   -- Must match orig_arg_tys 1-1
          str_marks    -- Must be empty or match dataConRepArgTys 1-1
          fields
          univ_tvs ex_tvs conc_tvs user_tvbs
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
    str_marks' | not $ any isMarkedStrict str_marks = []
               | otherwise                          = str_marks

    con = MkData {dcName = name, dcUnique = nameUnique name,
                  dcVanilla = is_vanilla, dcInfix = declared_infix,
                  dcUnivTyVars = univ_tvs,
                  dcExTyCoVars = ex_tvs,
                  dcConcreteTyVars = conc_tvs,
                  dcUserTyVarBinders = user_tvbs,
                  dcEqSpec = eq_spec,
                  dcOtherTheta = theta,
                  dcStupidTheta = stupid_theta,
                  dcOrigArgTys = orig_arg_tys, dcOrigResTy = orig_res_ty,
                  dcRepTyCon = rep_tycon,
                  dcSrcBangs = arg_stricts, dcImplBangs = impl_bangs,
                  dcStricts = str_marks',
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
                 mkScaledFunctionTys rep_arg_tys $
                 mkTyConApp rep_tycon (mkTyVarTys univ_tvs)
                 -- res_arg_tys is a mixture of TypeLike and ConstraintLike,
                 -- so we don't know which FunTyFlag to use
                 -- Hence using mkScaledFunctionTys.

      -- See Note [Promoted data constructors] in GHC.Core.TyCon
    prom_tv_bndrs = [ mkNamedTyConBinder vis tv
                    | Bndr tv vis <- user_tvbs ]

    fresh_names = freshNames (map getName user_tvbs)
      -- fresh_names: make sure that the "anonymous" tyvars don't
      -- clash in name or unique with the universal/existential ones.
      -- Tiresome!  And unnecessary because these tyvars are never looked at
    prom_arg_bndrs   = [ mkAnonTyConBinder (mkTyVar n t)
     {- Visible -}     | (n,t) <- dropList theta fresh_names `zip` map scaledThing orig_arg_tys ]
    prom_bndrs       = prom_tv_bndrs ++ prom_arg_bndrs
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

    , not (uniq `memberUniqueSet` avoid_uniqs)
    , not (occ `elemOccSet` avoid_occs) ]

  where
    avoid_uniqs :: UniqueSet
    avoid_uniqs = fromListUniqueSet (map getUnique avoids)

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

-- | Which type variables of this data constructor that must be
-- instantiated to concrete types?
-- For example: the RuntimeRep variables of unboxed tuples and unboxed sums.
--
-- See Note [Representation-polymorphism checking built-ins]
-- in GHC.Tc.Utils.Concrete
dataConConcreteTyVars :: DataCon -> ConcreteTyVars
dataConConcreteTyVars (MkData { dcConcreteTyVars = concs }) = concs

-- See Note [DataCon user type variable binders]
-- | The type variables of the constructor, in the order the user wrote them
dataConUserTyVars :: DataCon -> [TyVar]
dataConUserTyVars (MkData { dcUserTyVarBinders = tvbs }) = binderVars tvbs

-- See Note [DataCon user type variable binders]
-- | 'TyVarBinder's for the type variables of the constructor, in the order the
-- user wrote them
dataConUserTyVarBinders :: DataCon -> [TyVarBinder]
dataConUserTyVarBinders = dcUserTyVarBinders

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
    , let (ty1, ty, _) = coVarTypesRole cv
          tv = getTyVar ty1
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

-- | Number of value arguments of the data constructor
dataConSourceArity :: DataCon -> Arity
dataConSourceArity (MkData { dcSourceArity = arity }) = arity

-- | Number of visible arguments of the data constructor
dataConVisArity :: DataCon -> VisArity
dataConVisArity (MkData { dcUserTyVarBinders = tvbs, dcSourceArity = arity })
  = n_of_required_ty_args + n_of_val_args
  where
    n_of_val_args         = arity
    n_of_required_ty_args = count isVisibleForAllTyBinder tvbs

-- | Gives the number of value arguments (including zero-width coercions)
-- stored by the given `DataCon`'s worker in its Core representation. This may
-- differ from the number of arguments that appear in the source code; see also
-- Note [DataCon arities]
dataConRepArity :: DataCon -> Arity
dataConRepArity (MkData { dcRepArity = arity }) = arity

-- | Return whether there are any argument types for this 'DataCon's original source type
-- See Note [DataCon arities]
isNullarySrcDataCon :: DataCon -> Bool
isNullarySrcDataCon dc = dataConSourceArity dc == 0

-- | Return whether this `DataCon`'s worker, in its Core representation, takes
-- any value arguments.
--
-- In particular, remember that we include coercion arguments in the arity of
-- the Core representation of the `DataCon` -- both lifted and unlifted
-- coercions, despite the latter having zero-width runtime representation.
--
-- See also Note [DataCon arities].
isNullaryRepDataCon :: DataCon -> Bool
isNullaryRepDataCon dc = dataConRepArity dc == 0

isLazyDataConRep :: DataCon -> Bool
-- ^ True <==> All fields are lazy
isLazyDataConRep dc = null (dcStricts dc)

dataConRepStrictness :: DataCon -> [StrictnessMark]
-- ^ Give the demands on the runtime arguments of a Core DataCon worker
-- application.
-- The length of the list matches `dataConRepArgTys` (e.g., the number
-- of runtime arguments).
dataConRepStrictness dc
  | isLazyDataConRep dc
  = replicate (dataConRepArity dc) NotMarkedStrict
  | otherwise
  = dcStricts dc

dataConImplBangs :: DataCon -> [HsImplBang]
-- The implementation decisions about the strictness/unpack of each
-- source program argument to the data constructor
dataConImplBangs dc = dcImplBangs dc

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
--
-- See @Note [The stupid context]@.
dataConStupidTheta :: DataCon -> ThetaType
dataConStupidTheta dc = dcStupidTheta dc

{-
Note [Displaying linear fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor with a linear field can be written either as
MkT :: a %1 -> T a (with -XLinearTypes)
or
MkT :: a  -> T a (with -XNoLinearTypes)

There are three different methods to retrieve a type of a datacon.
They differ in how linear fields are handled.

1. dataConWrapperType:
The type of the wrapper in Core.
For example, dataConWrapperType for Maybe is a %1 -> Just a.

2. dataConNonlinearType:
The type of the constructor, with linear arrows replaced by unrestricted ones.
Used when we don't want to introduce linear types to user (in holes
and in types in hie used by haddock).

3. dataConDisplayType (takes a boolean indicating if -XLinearTypes is enabled):
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
                             dcOrigResTy = res_ty,
                             dcStupidTheta = stupid_theta })
  = mkForAllTys user_tvbs $
    mkInvisFunTys (stupid_theta ++ theta) $
    mkScaledFunTys arg_tys $
    res_ty

dataConNonlinearType :: DataCon -> Type
-- Just like dataConWrapperType, but with the
-- linearity on the arguments all zapped to Many
dataConNonlinearType (MkData { dcUserTyVarBinders = user_tvbs,
                               dcOtherTheta = theta, dcOrigArgTys = arg_tys,
                               dcOrigResTy = res_ty,
                               dcStupidTheta = stupid_theta })
  = mkForAllTys user_tvbs $
    mkInvisFunTys (stupid_theta ++ theta) $
    mkScaledFunTys arg_tys' $
    res_ty
  where
    arg_tys' = map (\(Scaled w t) -> Scaled (case w of OneTy -> ManyTy; _ -> w) t) arg_tys

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
 = assertPpr (univ_tvs `equalLength` inst_tys)
             (text "dataConInstArgTys" <+> ppr dc $$ ppr univ_tvs $$ ppr inst_tys) $
   assertPpr (null ex_tvs) (ppr dc) $
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
  = assertPpr (tyvars `equalLength` inst_tys)
              (text "dataConInstOrigArgTys" <+> ppr dc $$ ppr tyvars $$ ppr inst_tys) $
    substScaledTys subst arg_tys
  where
    tyvars = univ_tvs ++ ex_tvs
    subst  = zipTCvSubst tyvars inst_tys

-- | Given a data constructor @dc@ with /n/ universally quantified type
-- variables @a_{1}@, @a_{2}@, ..., @a_{n}@, and given a list of argument
-- types @dc_args@ of length /m/ where /m/ <= /n/, then:
--
-- @
-- dataConInstUnivs dc dc_args
-- @
--
-- Will return:
--
-- @
-- [dc_arg_{1}, dc_arg_{2}, ..., dc_arg_{m}, a_{m+1}, ..., a_{n}]
-- @
--
-- That is, return the list of universal type variables with
-- @a_{1}@, @a_{2}@, ..., @a_{m}@ instantiated with
-- @dc_arg_{1}@, @dc_arg_{2}@, ..., @dc_arg_{m}@. It is possible for @m@ to
-- be less than @n@, in which case the remaining @n - m@ elements will simply
-- be universal type variables (with their kinds possibly instantiated).
--
-- Examples:
--
-- * Given the data constructor @D :: forall a b. Foo a b@ and
--   @dc_args@ @[Int, Bool]@, then @dataConInstUnivs D dc_args@ will return
--   @[Int, Bool]@.
--
-- * Given the data constructor @D :: forall a b. Foo a b@ and
--   @dc_args@ @[Int]@, then @@dataConInstUnivs D dc_args@ will return
--   @[Int, b]@.
--
-- * Given the data constructor @E :: forall k (a :: k). Bar k a@ and
--   @dc_args@ @[Type]@, then @@dataConInstUnivs D dc_args@ will return
--   @[Type, (a :: Type)]@.
--
-- This is primarily used in @GHC.Tc.Deriv.*@ in service of instantiating data
-- constructors' field types.
-- See @Note [Instantiating field types in stock deriving]@ for a notable
-- example of this.
dataConInstUnivs :: DataCon -> [Type] -> [Type]
dataConInstUnivs dc dc_args = chkAppend dc_args $ map mkTyVarTy dc_args_suffix
  where
    (dc_univs_prefix, dc_univs_suffix)
                        = -- Assert that m <= n
                          assertPpr (dc_args `leLength` dataConUnivTyVars dc)
                                    (text "dataConInstUnivs"
                                      <+> ppr dc_args
                                      <+> ppr (dataConUnivTyVars dc)) $
                          splitAtList dc_args $ dataConUnivTyVars dc
    (_, dc_args_suffix) = substTyVarBndrs prefix_subst dc_univs_suffix
    prefix_subst        = mkTvSubst prefix_in_scope prefix_env
    prefix_in_scope     = mkInScopeSet $ tyCoVarsOfTypes dc_args
    prefix_env          = zipTyEnv dc_univs_prefix dc_args

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
dataConRepArgTys (MkData { dcRep        = rep
                         , dcEqSpec     = eq_spec
                         , dcOtherTheta = theta
                         , dcOrigArgTys = orig_arg_tys
                         , dcRepTyCon   = tc })
  = case rep of
      DCR { dcr_arg_tys = arg_tys } -> arg_tys
      NoDataConRep
        | isTypeDataTyCon tc -> assert (null theta)   $
                                orig_arg_tys
          -- `type data` declarations can be GADTs (and hence have an eq_spec)
          -- but no wrapper.  They cannot have a theta.
          -- See Note [Type data declarations] in GHC.Rename.Module
          -- You might wonder why we ever call dataConRepArgTys for `type data`;
          -- I think it's because of the call in mkDataCon, which in turn feeds
          -- into dcRepArity, which in turn is used in mkDataConWorkId.
          -- c.f. #23022
        | otherwise          -> assert (null eq_spec) $
                                map unrestricted theta ++ orig_arg_tys

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
        mod  = assert (isExternalName name) $ nameModule name

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

-- | Is this data constructor in a "type data" declaration?
-- See Note [Type data declarations] in GHC.Rename.Module.
isTypeDataCon :: DataCon -> Bool
isTypeDataCon dc = isTypeDataTyCon (dataConTyCon dc)

isCovertGadtDataCon :: DataCon -> Bool
-- See Note [isCovertGadtDataCon]
isCovertGadtDataCon (MkData { dcUnivTyVars  = univ_tvs
                            , dcEqSpec     = eq_spec
                            , dcRepTyCon   = rep_tc })
  =  not (null eq_spec)                -- There are some constraints
  && not (any is_visible_spec eq_spec) -- But none of them are visible
  where
    visible_univ_tvs :: [TyVar]  -- Visible arguments in result type
    visible_univ_tvs
      = [ univ_tv | (univ_tv, tcb) <- univ_tvs `zip` tyConBinders rep_tc
                  , isVisibleTyConBinder tcb ]

    is_visible_spec :: EqSpec -> Bool
    is_visible_spec (EqSpec univ_tv ty)
       = univ_tv `elem` visible_univ_tvs
         && not (isTyVarTy ty)  -- See Note [isCovertGadtDataCon] for
                                -- an example where 'ty' is a tyvar

isUnaryClassDataCon :: DataCon -> Bool
isUnaryClassDataCon dc = isUnaryClassTyCon (dataConTyCon dc)

{- Note [isCovertGadtDataCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(isCovertGadtDataCon K) returns True if K is a GADT data constructor, but
does not /look/ like it. It is used only to help in error message printing.

Consider (#21447)
    type T :: TYPE r -> Type
    data T a where { MkT :: b -> T b }
Here MkT doesn't look GADT-like, but it is. If we make the kind applications
explicit we'd see:
    data T a where { MkT :: b -> T @LiftedRep b }

The test for covert-ness is bit tricky, because we want to see if
  - dcEqSpec is non-empty
  - dcEqSpec does not constrain any of the /required/ (i.e. visible)
    arguments of the TyCon to a non-tyvar

In the example above, the DataCon for MkT will have
    dcUnivTyVars: [(r::RuntimeRep), (a :: TYPE r)]
    dcExTyVars:   [(b :: Type)]
    dcEqSpec:     [(r, LiftedRep), (a, b)]
Here
  * `r :: RuntimeRep` is constrained by dcEqSpec to LiftedRep
  * `a :: TYPE r` is constrained by dcEqSpec to `b :: Type`
But the constraint on `a` is not visible to the user, so this counts
as a covert GADT data con.  The declaration
     MkT :: forall (b :: Type). b -> T b
looks entirely non-GADT-ish.

Wrinkles:
* The visibility or otherwise is a property of the /TyCon/ binders
* The dcUnivTyVars may or may not be the same as the TyCon binders
* So we have to zip them together.
* For a data family the TyCon in question is the /representation/ TyCon
  hence dcRepTyCon
-}


-- | Should this DataCon be allowed in a type even without -XDataKinds?
-- Currently, only Lifted & Unlifted
specialPromotedDc :: DataCon -> Bool
specialPromotedDc = isKindTyCon . dataConTyCon

classDataCon :: Class -> DataCon
classDataCon clas = case tyConDataCons (classTyCon clas) of
                      (dict_constr:no_more) -> assert (null no_more) dict_constr
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
-- "MkId", and so 'dataConUserTyVarsNeedWrapper' is only called at most once
-- during a data constructor's lifetime.

dataConResRepTyArgs :: DataCon -> [Type]
-- Returns the arguments of a GADT version of the /representation/ TyCon
-- Thus   data instance T [(x,y)] z where
--           MkT :: forall p q. Int -> T [(Int,p)] (Maybe q)
-- The "GADT version of the representation type" is
--        data R:T x y z where
--           MkT :: forall p q. Int -> R:T Int p (Maybe q)
-- so dataConResRepTyArgs for MkT returns [Int, p, Maybe q]
-- This is almost the same as (subst eq_spec univ_tvs); but not quite,
--   because eq_spec omits constraint-kinded equalities
dataConResRepTyArgs dc@(MkData { dcRepTyCon = rep_tc, dcOrigResTy = orig_res_ty })
  | Just (fam_tc, fam_args) <- tyConFamInst_maybe rep_tc
  = -- fvs(fam_args) = tyConTyVars rep_tc
    -- These tyvars are the domain of subst
    -- Fvs(range(subst)) = tvars of the datacon
    case  tcMatchTy (mkTyConApp fam_tc fam_args) orig_res_ty of
       Just subst -> map (substTyVar subst) (tyConTyVars rep_tc)
       Nothing    -> pprPanic "datacOnResRepTyArgs" $
                     vcat [ ppr dc, ppr fam_tc <+> ppr fam_args
                          , ppr orig_res_ty ]
  | otherwise
  = tyConAppArgs orig_res_ty

checkDataConTyVars :: DataCon -> Bool
-- Check that the worker and wrapper have the same set of type variables
-- See Note [DataCon user type variable binders]
-- Also ensures that no user tyvar is in the eq_spec (the eq_spec should
-- only relate fresh universals from (R2) of the note)
checkDataConTyVars dc@(MkData { dcUnivTyVars = univ_tvs
                              , dcExTyCoVars = ex_tvs
                              , dcEqSpec = eq_spec })
     -- use of sets here: (R1) from the Note
  = mkUnVarSet depleted_worker_vars == mkUnVarSet wrapper_vars &&
    all (not . is_eq_spec_var) wrapper_vars
  where
    worker_vars = univ_tvs ++ ex_tvs
    eq_spec_tvs = mkUnVarSet (map eqSpecTyVar eq_spec)
    is_eq_spec_var = (`elemUnVarSet` eq_spec_tvs)  -- (R2) from the Note
    depleted_worker_vars = filterOut is_eq_spec_var worker_vars

    wrapper_vars = dataConUserTyVars dc

dataConUserTyVarBindersNeedWrapper :: DataCon -> Bool
-- Check whether the worker and wrapper have the same type variables
-- in the same order and with the same visibility. If not, we need a
-- wrapper to swizzle them.
-- See Note [DataCon user type variable binders], as well as
-- Note [Data con wrappers and GADT syntax] for an explanation of what
-- mkDataConRep is doing with this function.
dataConUserTyVarBindersNeedWrapper (MkData { dcUnivTyVars = univ_tvs
                                           , dcExTyCoVars = ex_tvs
                                           , dcUserTyVarBinders = user_tvbs
                                           , dcEqSpec = eq_spec })
  = assert (null eq_spec || answer)  -- all GADTs should say "yes" here
    answer
  where
    answer = need_reorder || need_flag_cast
    need_reorder   = (univ_tvs ++ ex_tvs) /= binderVars user_tvbs
    need_flag_cast = any (not . eqForAllVis coreTyLamForAllTyFlag)
                         (binderFlags user_tvbs)
      -- See Note [Flag cast in data con wrappers]

{- Note [Flag cast in data con wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the data declaration

  data G a where
    MkG :: forall a -> a -> G a

The user-facing type of MkG has a 'Required' forall. Workers, on the other hand,
always use 'Specified' foralls (coreTyLamForAllTyFlag). So we need a wrapper:

  wrapper type: forall a -> a -> G a
  worker type:  forall a.   a -> G a

Concretely, it looks like this:

   $WMkG = /\a. \(x:a). MkG a x |> co

where 'co' is a coercion constructed by GHC.Core.Coercion.mkForAllVisCos.
The cast is added by the call to mkCoreTyLams in GHC.Types.Id.Make.mkDataConRep.

In general, wrappers may use 'Inferred', 'Specified', or 'Required' foralls.
However, we do /not/ need a cast to convert 'Inferred' to 'Specified' because they are
'eqType'-equal. Only a 'Required' forall necessitates a cast in the wrapper.

See Note [ForAllTy and type equality], Note [Comparing visibility],
and Note [Required foralls in Core].
-}

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
