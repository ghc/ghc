%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The @TyCon@ datatype

\begin{code}

module TyCon(
        -- * Main TyCon data types
        TyCon, FieldLabel,

        AlgTyConRhs(..), visibleDataCons,
        TyConParent(..), isNoParent,
        SynTyConRhs(..), 

        -- ** Constructing TyCons
        mkAlgTyCon,
        mkClassTyCon,
        mkFunTyCon,
        mkPrimTyCon,
        mkKindTyCon,
        mkLiftedPrimTyCon,
        mkTupleTyCon,
        mkSynTyCon,
        mkForeignTyCon,
        mkPromotedDataCon,
        mkPromotedTyCon,

        -- ** Predicates on TyCons
        isAlgTyCon,
        isClassTyCon, isFamInstTyCon,
        isFunTyCon,
        isPrimTyCon,
        isTupleTyCon, isUnboxedTupleTyCon, isBoxedTupleTyCon,
        isSynTyCon, isOpenSynFamilyTyCon,
        isDecomposableTyCon,
        isForeignTyCon, 
        isPromotedDataCon, isPromotedTyCon,
        isPromotedDataCon_maybe, isPromotedTyCon_maybe,
        promotableTyCon_maybe, promoteTyCon,

        isInjectiveTyCon,
        isDataTyCon, isProductTyCon, isDataProductTyCon_maybe,
        isEnumerationTyCon,
        isNewTyCon, isAbstractTyCon,
        isFamilyTyCon, isSynFamilyTyCon, isDataFamilyTyCon,
        isUnLiftedTyCon,
        isGadtSyntaxTyCon, isDistinctTyCon, isDistinctAlgRhs,
        isTyConAssoc, tyConAssoc_maybe,
        isRecursiveTyCon,
        isImplicitTyCon,

        -- ** Extracting information out of TyCons
        tyConName,
        tyConKind,
        tyConUnique,
        tyConTyVars,
        tyConCType, tyConCType_maybe,
        tyConDataCons, tyConDataCons_maybe, 
        tyConSingleDataCon_maybe, tyConSingleAlgDataCon_maybe,
        tyConFamilySize,
        tyConStupidTheta,
        tyConArity,
        tyConParent,
        tyConTuple_maybe, tyConClass_maybe,
        tyConFamInst_maybe, tyConFamInstSig_maybe, tyConFamilyCoercion_maybe,
        synTyConDefn_maybe, synTyConRhs_maybe, 
        tyConExtName,           -- External name for foreign types
        algTyConRhs,
        newTyConRhs, newTyConEtadRhs, unwrapNewTyCon_maybe,
        tupleTyConBoxity, tupleTyConSort, tupleTyConArity,

        -- ** Manipulating TyCons
        tcExpandTyCon_maybe, coreExpandTyCon_maybe,
        makeTyConAbstract,
        newTyConCo, newTyConCo_maybe,
        pprPromotionQuote,

        -- * Primitive representations of Types
        PrimRep(..),
        tyConPrimRep,
        primRepSizeW
) where

#include "HsVersions.h"

import {-# SOURCE #-} TypeRep ( Kind, Type, PredType )
import {-# SOURCE #-} DataCon ( DataCon, isVanillaDataCon )

import Var
import Class
import BasicTypes
import DynFlags
import ForeignCall
import Name
import CoAxiom
import PrelNames
import Maybes
import Outputable
import FastString
import Constants
import Util
import qualified Data.Data as Data
import Data.Typeable (Typeable)
\end{code}

-----------------------------------------------
        Notes about type families
-----------------------------------------------

Note [Type synonym families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Type synonym families, also known as "type functions", map directly
  onto the type functions in FC:

        type family F a :: *
        type instance F Int = Bool
        ..etc...

* Reply "yes" to isSynFamilyTyCon, and isFamilyTyCon

* From the user's point of view (F Int) and Bool are simply
  equivalent types.

* A Haskell 98 type synonym is a degenerate form of a type synonym
  family.

* Type functions can't appear in the LHS of a type function:
        type instance F (F Int) = ...   -- BAD!

* Translation of type family decl:
        type family F a :: *
  translates to
    a SynTyCon 'F', whose SynTyConRhs is SynFamilyTyCon

* Translation of type family decl:
        type family F a :: *
  translates to
    a SynTyCon 'F', whose SynTyConRhs is SynFamilyTyCon

* In the future we might want to support
    * closed type families (esp when we have proper kinds)
    * injective type families (allow decomposition)
  but we don't at the moment [2010]

Note [Data type families]
~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Wrappers for data instance tycons] in MkId.lhs

* Data type families are declared thus
        data family T a :: *
        data instance T Int = T1 | T2 Bool

  Here T is the "family TyCon".

* Reply "yes" to isDataFamilyTyCon, and isFamilyTyCon

* The user does not see any "equivalent types" as he did with type
  synonym families.  He just sees constructors with types
        T1 :: T Int
        T2 :: Bool -> T Int

* Here's the FC version of the above declarations:

        data T a
        data R:TInt = T1 | T2 Bool
        axiom ax_ti : T Int ~ R:TInt

  The R:TInt is the "representation TyCons".
  It has an AlgTyConParent of
        FamInstTyCon T [Int] ax_ti

* The data contructor T2 has a wrapper (which is what the
  source-level "T2" invokes):

        $WT2 :: Bool -> T Int
        $WT2 b = T2 b `cast` sym ax_ti

* A data instance can declare a fully-fledged GADT:

        data instance T (a,b) where
          X1 :: T (Int,Bool)
          X2 :: a -> b -> T (a,b)

  Here's the FC version of the above declaration:

        data R:TPair a where
          X1 :: R:TPair Int Bool
          X2 :: a -> b -> R:TPair a b
        axiom ax_pr :: T (a,b) ~ R:TPair a b

        $WX1 :: forall a b. a -> b -> T (a,b)
        $WX1 a b (x::a) (y::b) = X2 a b x y `cast` sym (ax_pr a b)

  The R:TPair are the "representation TyCons".
  We have a bit of work to do, to unpick the result types of the
  data instance declaration for T (a,b), to get the result type in the
  representation; e.g.  T (a,b) --> R:TPair a b

  The representation TyCon R:TList, has an AlgTyConParent of

        FamInstTyCon T [(a,b)] ax_pr

* Notice that T is NOT translated to a FC type function; it just
  becomes a "data type" with no constructors, which can be coerced inot
  into R:TInt, R:TPair by the axioms.  These axioms
  axioms come into play when (and *only* when) you
        - use a data constructor
        - do pattern matching
  Rather like newtype, in fact

  As a result

  - T behaves just like a data type so far as decomposition is concerned

  - (T Int) is not implicitly converted to R:TInt during type inference.
    Indeed the latter type is unknown to the programmer.

  - There *is* an instance for (T Int) in the type-family instance
    environment, but it is only used for overlap checking

  - It's fine to have T in the LHS of a type function:
    type instance F (T a) = [a]

  It was this last point that confused me!  The big thing is that you
  should not think of a data family T as a *type function* at all, not
  even an injective one!  We can't allow even injective type functions
  on the LHS of a type function:
        type family injective G a :: *
        type instance F (G Int) = Bool
  is no good, even if G is injective, because consider
        type instance G Int = Bool
        type instance F Bool = Char

  So a data type family is not an injective type function. It's just a
  data type with some axioms that connect it to other data types.

Note [Associated families and their parent class]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Associated* families are just like *non-associated* families, except
that they have a TyConParent of AssocFamilyTyCon, which identifies the
parent class.

However there is an important sharing relationship between
  * the tyConTyVars of the parent Class
  * the tyConTyvars of the associated TyCon

   class C a b where
     data T p a
     type F a q b

Here the 'a' and 'b' are shared with the 'Class'; that is, they have
the same Unique.

This is important. In an instance declaration we expect
  * all the shared variables to be instantiated the same way
  * the non-shared variables of the associated type should not
    be instantiated at all

  instance C [x] (Tree y) where
     data T p [x] = T1 x | T2 p
     type F [x] q (Tree y) = (x,y,q)

%************************************************************************
%*                                                                      *
\subsection{The data type}
%*                                                                      *
%************************************************************************

\begin{code}
-- | TyCons represent type constructors. Type constructors are introduced by things such as:
--
-- 1) Data declarations: @data Foo = ...@ creates the @Foo@ type constructor of kind @*@
--
-- 2) Type synonyms: @type Foo = ...@ creates the @Foo@ type constructor
--
-- 3) Newtypes: @newtype Foo a = MkFoo ...@ creates the @Foo@ type constructor of kind @* -> *@
--
-- 4) Class declarations: @class Foo where@ creates the @Foo@ type constructor of kind @*@
--
-- This data type also encodes a number of primitive, built in type constructors such as those
-- for function and tuple types.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data TyCon
  = -- | The function type constructor, @(->)@
    FunTyCon {
        tyConUnique :: Unique,
        tyConName   :: Name,
        tc_kind   :: Kind,
        tyConArity  :: Arity
    }

  -- | Algebraic type constructors, which are defined to be those
  -- arising @data@ type and @newtype@ declarations.  All these
  -- constructors are lifted and boxed. See 'AlgTyConRhs' for more
  -- information.
  | AlgTyCon {
        tyConUnique :: Unique,
        tyConName   :: Name,
        tc_kind     :: Kind,
        tyConArity  :: Arity,

        tyConTyVars :: [TyVar],   -- ^ The kind and type variables used in the type constructor.
                                  -- Invariant: length tyvars = arity
                                  -- Precisely, this list scopes over:
                                  --
                                  -- 1. The 'algTcStupidTheta'
                                  -- 2. The cached types in 'algTyConRhs.NewTyCon'
                                  -- 3. The family instance types if present
                                  --
                                  -- Note that it does /not/ scope over the data constructors.
        tyConCType   :: Maybe CType, -- The C type that should be used
                                     -- for this type when using the FFI
                                     -- and CAPI

        algTcGadtSyntax  :: Bool,       -- ^ Was the data type declared with GADT syntax?
                                        -- If so, that doesn't mean it's a true GADT;
                                        -- only that the "where" form was used.
                                        -- This field is used only to guide pretty-printing

        algTcStupidTheta :: [PredType], -- ^ The \"stupid theta\" for the data type
                                        -- (always empty for GADTs).
                                        -- A \"stupid theta\" is the context to the left
                                        -- of an algebraic type declaration,
                                        -- e.g. @Eq a@ in the declaration
                                        --    @data Eq a => T a ...@.

        algTcRhs :: AlgTyConRhs,  -- ^ Contains information about the
                                  -- data constructors of the algebraic type

        algTcRec :: RecFlag,      -- ^ Tells us whether the data type is part
                                  -- of a mutually-recursive group or not

        algTcParent :: TyConParent,     -- ^ Gives the class or family declaration 'TyCon'
                                        -- for derived 'TyCon's representing class
                                        -- or family instances, respectively.
                                        -- See also 'synTcParent'
        
        tcPromoted :: Maybe TyCon    -- ^ Promoted TyCon, if any
    }

  -- | Represents the infinite family of tuple type constructors,
  --   @()@, @(a,b)@, @(# a, b #)@ etc.
  | TupleTyCon {
        tyConUnique    :: Unique,
        tyConName      :: Name,
        tc_kind        :: Kind,
        tyConArity     :: Arity,
        tyConTupleSort :: TupleSort,
        tyConTyVars    :: [TyVar],
        dataCon        :: DataCon, -- ^ Corresponding tuple data constructor
        tcPromoted     :: Maybe TyCon    -- Nothing for unboxed tuples
    }

  -- | Represents type synonyms
  | SynTyCon {
        tyConUnique  :: Unique,
        tyConName    :: Name,
        tc_kind    :: Kind,
        tyConArity   :: Arity,

        tyConTyVars  :: [TyVar],        -- Bound tyvars

        synTcRhs     :: SynTyConRhs Type,  -- ^ Contains information about the
                                           -- expansion of the synonym

        synTcParent  :: TyConParent     -- ^ Gives the family declaration 'TyCon'
                                        -- of 'TyCon's representing family instances

    }

  -- | Primitive types; cannot be defined in Haskell. This includes
  -- the usual suspects (such as @Int#@) as well as foreign-imported
  -- types and kinds
  | PrimTyCon {
        tyConUnique   :: Unique,
        tyConName     :: Name,
        tc_kind       :: Kind,
        tyConArity    :: Arity,         -- SLPJ Oct06: I'm not sure what the significance
                                        --             of the arity of a primtycon is!

        primTyConRep  :: PrimRep,       -- ^ Many primitive tycons are unboxed, but some are
                                        --   boxed (represented by pointers). This 'PrimRep'
                                        --   holds that information.
                                        -- Only relevant if tc_kind = *

        isUnLifted   :: Bool,           -- ^ Most primitive tycons are unlifted
                                        --   (may not contain bottom)
                                        --   but foreign-imported ones may be lifted

        tyConExtName :: Maybe FastString   -- ^ @Just e@ for foreign-imported types,
                                           --   holds the name of the imported thing
    }

  -- | Represents promoted data constructor.
  | PromotedDataCon {         -- See Note [Promoted data constructors]
        tyConUnique :: Unique, -- ^ Same Unique as the data constructor
        tyConName   :: Name,   -- ^ Same Name as the data constructor
        tyConArity  :: Arity,
        tc_kind     :: Kind,   -- ^ Translated type of the data constructor
        dataCon     :: DataCon -- ^ Corresponding data constructor
    }

  -- | Represents promoted type constructor.
  | PromotedTyCon {
        tyConUnique :: Unique, -- ^ Same Unique as the type constructor
        tyConName   :: Name,   -- ^ Same Name as the type constructor
        tyConArity  :: Arity,  -- ^ n if ty_con :: * -> ... -> *  n times
        tc_kind     :: Kind,   -- ^ Always TysPrim.superKind
        ty_con      :: TyCon   -- ^ Corresponding type constructor
    }

  deriving Typeable

-- | Names of the fields in an algebraic record type
type FieldLabel = Name

-- | Represents right-hand-sides of 'TyCon's for algebraic types
data AlgTyConRhs

    -- | Says that we know nothing about this data type, except that
    -- it's represented by a pointer.  Used when we export a data type
    -- abstractly into an .hi file.
  = AbstractTyCon
      Bool      -- True  <=> It's definitely a distinct data type,
                --           equal only to itself; ie not a newtype
                -- False <=> Not sure
                -- See Note [AbstractTyCon and type equality]

    -- | Represents an open type family without a fixed right hand
    -- side.  Additional instances can appear at any time.
    --
    -- These are introduced by either a top level declaration:
    --
    -- > data T a :: *
    --
    -- Or an associated data type declaration, within a class declaration:
    --
    -- > class C a b where
    -- >   data T b :: *
  | DataFamilyTyCon

    -- | Information about those 'TyCon's derived from a @data@
    -- declaration. This includes data types with no constructors at
    -- all.
  | DataTyCon {
        data_cons :: [DataCon],
                          -- ^ The data type constructors; can be empty if the user
                          --   declares the type to have no constructors
                          --
                          -- INVARIANT: Kept in order of increasing 'DataCon' tag
                          --      (see the tag assignment in DataCon.mkDataCon)

        is_enum :: Bool   -- ^ Cached value: is this an enumeration type?
                          --   See Note [Enumeration types]
    }

  -- | Information about those 'TyCon's derived from a @newtype@ declaration
  | NewTyCon {
        data_con :: DataCon,    -- ^ The unique constructor for the @newtype@.
                                --   It has no existentials

        nt_rhs :: Type,         -- ^ Cached value: the argument type of the constructor,
                                -- which is just the representation type of the 'TyCon'
                                -- (remember that @newtype@s do not exist at runtime
                                -- so need a different representation type).
                                --
                                -- The free 'TyVar's of this type are the 'tyConTyVars'
                                -- from the corresponding 'TyCon'

        nt_etad_rhs :: ([TyVar], Type),
                        -- ^ Same as the 'nt_rhs', but this time eta-reduced.
                        -- Hence the list of 'TyVar's in this field may be
                        -- shorter than the declared arity of the 'TyCon'.

                        -- See Note [Newtype eta]
        nt_co :: CoAxiom Unbranched
                             -- The axiom coercion that creates the @newtype@ from
                             -- the representation 'Type'.

                             -- See Note [Newtype coercions]
                             -- Invariant: arity = #tvs in nt_etad_rhs;
                             -- See Note [Newtype eta]
                             -- Watch out!  If any newtypes become transparent
                             -- again check Trac #1072.
    }
\end{code}

Note [AbstractTyCon and type equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO

\begin{code}

-- | Extract those 'DataCon's that we are able to learn about.  Note
-- that visibility in this sense does not correspond to visibility in
-- the context of any particular user program!
visibleDataCons :: AlgTyConRhs -> [DataCon]
visibleDataCons (AbstractTyCon {})            = []
visibleDataCons DataFamilyTyCon {}            = []
visibleDataCons (DataTyCon{ data_cons = cs }) = cs
visibleDataCons (NewTyCon{ data_con = c })    = [c]

-- ^ Both type classes as well as family instances imply implicit
-- type constructors.  These implicit type constructors refer to their parent
-- structure (ie, the class or family from which they derive) using a type of
-- the following form.  We use 'TyConParent' for both algebraic and synonym
-- types, but the variant 'ClassTyCon' will only be used by algebraic 'TyCon's.
data TyConParent
  = -- | An ordinary type constructor has no parent.
    NoParentTyCon

  -- | Type constructors representing a class dictionary.
  -- See Note [ATyCon for classes] in TypeRep
  | ClassTyCon
        Class           -- INVARIANT: the classTyCon of this Class is the current tycon

  -- | An *associated* type of a class.
  | AssocFamilyTyCon
        Class           -- The class in whose declaration the family is declared
                        -- See Note [Associated families and their parent class]

  -- | Type constructors representing an instance of a *data* family. Parameters:
  --
  --  1) The type family in question
  --
  --  2) Instance types; free variables are the 'tyConTyVars'
  --  of the current 'TyCon' (not the family one). INVARIANT:
  --  the number of types matches the arity of the family 'TyCon'
  --
  --  3) A 'CoTyCon' identifying the representation
  --  type with the type instance family
  | FamInstTyCon          -- See Note [Data type families]
        (CoAxiom Unbranched)  -- The coercion constructor,
                              -- always of kind   T ty1 ty2 ~ R:T a b c
                              -- where T is the family TyCon,
                              -- and R:T is the representation TyCon (ie this one)
                              -- and a,b,c are the tyConTyVars of this TyCon

          -- Cached fields of the CoAxiom, but adjusted to
          -- use the tyConTyVars of this TyCon
        TyCon   -- The family TyCon
        [Type]  -- Argument types (mentions the tyConTyVars of this TyCon)
                -- Match in length the tyConTyVars of the family TyCon

        -- E.g.  data intance T [a] = ...
        -- gives a representation tycon:
        --      data R:TList a = ...
        --      axiom co a :: T [a] ~ R:TList a
        -- with R:TList's algTcParent = FamInstTyCon T [a] co

instance Outputable TyConParent where
    ppr NoParentTyCon           = text "No parent"
    ppr (ClassTyCon cls)        = text "Class parent" <+> ppr cls
    ppr (AssocFamilyTyCon cls)  = text "Class parent (assoc. family)" <+> ppr cls
    ppr (FamInstTyCon _ tc tys) = text "Family parent (family instance)" <+> ppr tc <+> sep (map ppr tys)

-- | Checks the invariants of a 'TyConParent' given the appropriate type class name, if any
okParent :: Name -> TyConParent -> Bool
okParent _       NoParentTyCon               = True
okParent tc_name (AssocFamilyTyCon cls)      = tc_name `elem` map tyConName (classATs cls)
okParent tc_name (ClassTyCon cls)            = tc_name == tyConName (classTyCon cls)
okParent _       (FamInstTyCon _ fam_tc tys) = tyConArity fam_tc == length tys

isNoParent :: TyConParent -> Bool
isNoParent NoParentTyCon = True
isNoParent _             = False

--------------------

-- | Information pertaining to the expansion of a type synonym (@type@)
data SynTyConRhs ty
  = -- | An ordinary type synonyn.
    SynonymTyCon
       ty             -- This 'Type' is the rhs, and may mention from 'tyConTyVars'.
                      -- It acts as a template for the expansion when the 'TyCon'
                      -- is applied to some types.

   -- | A type synonym family  e.g. @type family F x y :: * -> *@
   | SynFamilyTyCon {
        synf_open :: Bool,         -- See Note [Closed type families]
        synf_injective :: Bool 
     }
\end{code}

Note [Closed type families]
~~~~~~~~~~~~~~~~~~~~~~~~~
* In an open type family you can add new instances later.  This is the 
  usual case.  

* In a closed type family you can only put instnaces where the family
  is defined.  GHC doesn't support syntax for this yet.

Note [Promoted data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data constructor can be promoted to become a type constructor,
via the PromotedTyCon alternative in TyCon.

* Only data constructors with  
     (a) no kind polymorphism
     (b) no constraints in its type (eg GADTs)
  are promoted.  Existentials are ok; see Trac #7347.

* The TyCon promoted from a DataCon has the *same* Name and Unique as
  the DataCon.  Eg. If the data constructor Data.Maybe.Just(unique 78,
  say) is promoted to a TyCon whose name is Data.Maybe.Just(unique 78)

* The *kind* of a promoted DataCon may be polymorphic.  Example:
    type of DataCon           Just :: forall (a:*). a -> Maybe a
    kind of (promoted) tycon  Just :: forall (a:box). a -> Maybe a
  The kind is not identical to the type, because of the */box
  kind signature on the forall'd variable; so the tc_kind field of
  PromotedTyCon is not identical to the dataConUserType of the
  DataCon.  But it's the same modulo changing the variable kinds,
  done by DataCon.promoteType.

* Small note: We promote the *user* type of the DataCon.  Eg
     data T = MkT {-# UNPACK #-} !(Bool, Bool)
  The promoted kind is
     MkT :: (Bool,Bool) -> T
  *not*
     MkT :: Bool -> Bool -> T

Note [Enumeration types]
~~~~~~~~~~~~~~~~~~~~~~~~
We define datatypes with no constructors to *not* be
enumerations; this fixes trac #2578,  Otherwise we
end up generating an empty table for
  <mod>_<type>_closure_tbl
which is used by tagToEnum# to map Int# to constructors
in an enumeration. The empty table apparently upset
the linker.

Moreover, all the data constructor must be enumerations, meaning
they have type  (forall abc. T a b c).  GADTs are not enumerations.
For example consider
    data T a where
      T1 :: T Int
      T2 :: T Bool
      T3 :: T a
What would [T1 ..] be?  [T1,T3] :: T Int? Easiest thing is to exclude them.
See Trac #4528.

Note [Newtype coercions]
~~~~~~~~~~~~~~~~~~~~~~~~
The NewTyCon field nt_co is a CoAxiom which is used for coercing from
the representation type of the newtype, to the newtype itself. For
example,

   newtype T a = MkT (a -> a)

the NewTyCon for T will contain nt_co = CoT where CoT t : T t ~ t -> t.

In the case that the right hand side is a type application
ending with the same type variables as the left hand side, we
"eta-contract" the coercion.  So if we had

   newtype S a = MkT [a]

then we would generate the arity 0 axiom CoS : S ~ [].  The
primary reason we do this is to make newtype deriving cleaner.

In the paper we'd write
        axiom CoT : (forall t. T t) ~ (forall t. [t])
and then when we used CoT at a particular type, s, we'd say
        CoT @ s
which encodes as (TyConApp instCoercionTyCon [TyConApp CoT [], s])

Note [Newtype eta]
~~~~~~~~~~~~~~~~~~
Consider
        newtype Parser m a = MkParser (Foogle m a)
Are these two types equal (to Core)?
        Monad (Parser m)
        Monad (Foogle m)
Well, yes.  But to see that easily we eta-reduce the RHS type of
Parser, in this case to ([], Froogle), so that even unsaturated applications
of Parser will work right.  This eta reduction is done when the type
constructor is built, and cached in NewTyCon.  The cached field is
only used in coreExpandTyCon_maybe.

Here's an example that I think showed up in practice
Source code:
        newtype T a = MkT [a]
        newtype Foo m = MkFoo (forall a. m a -> Int)

        w1 :: Foo []
        w1 = ...

        w2 :: Foo T
        w2 = MkFoo (\(MkT x) -> case w1 of MkFoo f -> f x)

After desugaring, and discarding the data constructors for the newtypes,
we get:
        w2 :: Foo T
        w2 = w1
And now Lint complains unless Foo T == Foo [], and that requires T==[]

This point carries over to the newtype coercion, because we need to
say
        w2 = w1 `cast` Foo CoT

so the coercion tycon CoT must have
        kind:    T ~ []
 and    arity:   0

%************************************************************************
%*                                                                      *
\subsection{PrimRep}
%*                                                                      *
%************************************************************************

Note [rep swamp]

GHC has a rich selection of types that represent "primitive types" of
one kind or another.  Each of them makes a different set of
distinctions, and mostly the differences are for good reasons,
although it's probably true that we could merge some of these.

Roughly in order of "includes more information":

 - A Width (cmm/CmmType) is simply a binary value with the specified
   number of bits.  It may represent a signed or unsigned integer, a
   floating-point value, or an address.

    data Width = W8 | W16 | W32 | W64 | W80 | W128

 - Size, which is used in the native code generator, is Width +
   floating point information.

   data Size = II8 | II16 | II32 | II64 | FF32 | FF64 | FF80

   it is necessary because e.g. the instruction to move a 64-bit float
   on x86 (movsd) is different from the instruction to move a 64-bit
   integer (movq), so the mov instruction is parameterised by Size.

 - CmmType wraps Width with more information: GC ptr, float, or
   other value.

    data CmmType = CmmType CmmCat Width
    
    data CmmCat     -- "Category" (not exported)
       = GcPtrCat   -- GC pointer
       | BitsCat    -- Non-pointer
       | FloatCat   -- Float

   It is important to have GcPtr information in Cmm, since we generate
   info tables containing pointerhood for the GC from this.  As for
   why we have float (and not signed/unsigned) here, see Note [Signed
   vs unsigned].

 - ArgRep makes only the distinctions necessary for the call and
   return conventions of the STG machine.  It is essentially CmmType
   + void.

 - PrimRep makes a few more distinctions than ArgRep: it divides
   non-GC-pointers into signed/unsigned and addresses, information
   that is necessary for passing these values to foreign functions.

There's another tension here: whether the type encodes its size in
bytes, or whether its size depends on the machine word size.  Width
and CmmType have the size built-in, whereas ArgRep and PrimRep do not.

This means to turn an ArgRep/PrimRep into a CmmType requires DynFlags.

On the other hand, CmmType includes some "nonsense" values, such as
CmmType GcPtrCat W32 on a 64-bit machine.

\begin{code}
-- | A 'PrimRep' is an abstraction of a type.  It contains information that
-- the code generator needs in order to pass arguments, return results,
-- and store values of this type.
data PrimRep
  = VoidRep
  | PtrRep
  | IntRep              -- ^ Signed, word-sized value
  | WordRep             -- ^ Unsigned, word-sized value
  | Int64Rep            -- ^ Signed, 64 bit value (with 32-bit words only)
  | Word64Rep           -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | AddrRep             -- ^ A pointer, but /not/ to a Haskell value (use 'PtrRep')
  | FloatRep
  | DoubleRep
  deriving( Eq, Show )

instance Outputable PrimRep where
  ppr r = text (show r)

-- | Find the size of a 'PrimRep', in words
primRepSizeW :: DynFlags -> PrimRep -> Int
primRepSizeW _      IntRep   = 1
primRepSizeW _      WordRep  = 1
primRepSizeW dflags Int64Rep = wORD64_SIZE `quot` wORD_SIZE dflags
primRepSizeW dflags Word64Rep= wORD64_SIZE `quot` wORD_SIZE dflags
primRepSizeW _      FloatRep = 1    -- NB. might not take a full word
primRepSizeW dflags DoubleRep= dOUBLE_SIZE dflags `quot` wORD_SIZE dflags
primRepSizeW _      AddrRep  = 1
primRepSizeW _      PtrRep   = 1
primRepSizeW _      VoidRep  = 0
\end{code}

%************************************************************************
%*                                                                      *
\subsection{TyCon Construction}
%*                                                                      *
%************************************************************************

Note: the TyCon constructors all take a Kind as one argument, even though
they could, in principle, work out their Kind from their other arguments.
But to do so they need functions from Types, and that makes a nasty
module mutual-recursion.  And they aren't called from many places.
So we compromise, and move their Kind calculation to the call site.

\begin{code}
-- | Given the name of the function type constructor and it's kind, create the
-- corresponding 'TyCon'. It is reccomended to use 'TypeRep.funTyCon' if you want
-- this functionality
mkFunTyCon :: Name -> Kind -> TyCon
mkFunTyCon name kind
  = FunTyCon {
        tyConUnique = nameUnique name,
        tyConName   = name,
        tc_kind   = kind,
        tyConArity  = 2
    }

-- | This is the making of an algebraic 'TyCon'. Notably, you have to
-- pass in the generic (in the -XGenerics sense) information about the
-- type constructor - you can get hold of it easily (see Generics
-- module)
mkAlgTyCon :: Name
           -> Kind              -- ^ Kind of the resulting 'TyCon'
           -> [TyVar]           -- ^ 'TyVar's scoped over: see 'tyConTyVars'.
                                --   Arity is inferred from the length of this list
           -> Maybe CType       -- ^ The C type this type corresponds to
                                --   when using the CAPI FFI
           -> [PredType]        -- ^ Stupid theta: see 'algTcStupidTheta'
           -> AlgTyConRhs       -- ^ Information about dat aconstructors
           -> TyConParent
           -> RecFlag           -- ^ Is the 'TyCon' recursive?
           -> Bool              -- ^ Was the 'TyCon' declared with GADT syntax?
           -> Maybe TyCon       -- ^ Promoted version
           -> TyCon
mkAlgTyCon name kind tyvars cType stupid rhs parent is_rec gadt_syn prom_tc
  = AlgTyCon {
        tyConName        = name,
        tyConUnique      = nameUnique name,
        tc_kind          = kind,
        tyConArity       = length tyvars,
        tyConTyVars      = tyvars,
        tyConCType       = cType,
        algTcStupidTheta = stupid,
        algTcRhs         = rhs,
        algTcParent      = ASSERT2( okParent name parent, ppr name $$ ppr parent ) parent,
        algTcRec         = is_rec,
        algTcGadtSyntax  = gadt_syn,
        tcPromoted       = prom_tc
    }

-- | Simpler specialization of 'mkAlgTyCon' for classes
mkClassTyCon :: Name -> Kind -> [TyVar] -> AlgTyConRhs -> Class -> RecFlag -> TyCon
mkClassTyCon name kind tyvars rhs clas is_rec
  = mkAlgTyCon name kind tyvars Nothing [] rhs (ClassTyCon clas) 
               is_rec False 
               Nothing    -- Class TyCons are not pormoted

mkTupleTyCon :: Name
             -> Kind    -- ^ Kind of the resulting 'TyCon'
             -> Arity   -- ^ Arity of the tuple
             -> [TyVar] -- ^ 'TyVar's scoped over: see 'tyConTyVars'
             -> DataCon
             -> TupleSort    -- ^ Whether the tuple is boxed or unboxed
             -> Maybe TyCon  -- ^ Promoted version
             -> TyCon
mkTupleTyCon name kind arity tyvars con sort prom_tc
  = TupleTyCon {
        tyConUnique = nameUnique name,
        tyConName = name,
        tc_kind = kind,
        tyConArity = arity,
        tyConTupleSort = sort,
        tyConTyVars = tyvars,
        dataCon = con,
        tcPromoted = prom_tc
    }

-- ^ Foreign-imported (.NET) type constructors are represented
-- as primitive, but /lifted/, 'TyCons' for now. They are lifted
-- because the Haskell type @T@ representing the (foreign) .NET
-- type @T@ is actually implemented (in ILX) as a @thunk<T>@
mkForeignTyCon :: Name
               -> Maybe FastString -- ^ Name of the foreign imported thing, maybe
               -> Kind
               -> Arity
               -> TyCon
mkForeignTyCon name ext_name kind arity
  = PrimTyCon {
        tyConName    = name,
        tyConUnique  = nameUnique name,
        tc_kind    = kind,
        tyConArity   = arity,
        primTyConRep = PtrRep, -- they all do
        isUnLifted   = False,
        tyConExtName = ext_name
    }


-- | Create an unlifted primitive 'TyCon', such as @Int#@
mkPrimTyCon :: Name  -> Kind -> Arity -> PrimRep -> TyCon
mkPrimTyCon name kind arity rep
  = mkPrimTyCon' name kind arity rep True

-- | Kind constructors
mkKindTyCon :: Name -> Kind -> TyCon
mkKindTyCon name kind
  = mkPrimTyCon' name kind 0 VoidRep True

-- | Create a lifted primitive 'TyCon' such as @RealWorld@
mkLiftedPrimTyCon :: Name  -> Kind -> Arity -> PrimRep -> TyCon
mkLiftedPrimTyCon name kind arity rep
  = mkPrimTyCon' name kind arity rep False

mkPrimTyCon' :: Name  -> Kind -> Arity -> PrimRep -> Bool -> TyCon
mkPrimTyCon' name kind arity rep is_unlifted
  = PrimTyCon {
        tyConName    = name,
        tyConUnique  = nameUnique name,
        tc_kind    = kind,
        tyConArity   = arity,
        primTyConRep = rep,
        isUnLifted   = is_unlifted,
        tyConExtName = Nothing
    }

-- | Create a type synonym 'TyCon'
mkSynTyCon :: Name -> Kind -> [TyVar] -> SynTyConRhs Type -> TyConParent -> TyCon
mkSynTyCon name kind tyvars rhs parent
  = SynTyCon {
        tyConName = name,
        tyConUnique = nameUnique name,
        tc_kind = kind,
        tyConArity = length tyvars,
        tyConTyVars = tyvars,
        synTcRhs = rhs,
        synTcParent = parent
    }

-- | Create a promoted data constructor 'TyCon'
-- Somewhat dodgily, we give it the same Name
-- as the data constructor itself; when we pretty-print
-- the TyCon we add a quote; see the Outputable TyCon instance
mkPromotedDataCon :: DataCon -> Name -> Unique -> Kind -> Arity -> TyCon
mkPromotedDataCon con name unique kind arity
  = PromotedDataCon {
        tyConName   = name,
        tyConUnique = unique,
        tyConArity  = arity,
        tc_kind     = kind,
        dataCon     = con
  }

-- | Create a promoted type constructor 'TyCon'
-- Somewhat dodgily, we give it the same Name
-- as the type constructor itself
mkPromotedTyCon :: TyCon -> Kind -> TyCon
mkPromotedTyCon tc kind
  = PromotedTyCon {
        tyConName   = getName tc,
        tyConUnique = getUnique tc,
        tyConArity  = tyConArity tc,
        tc_kind     = kind,
        ty_con      = tc
  }
\end{code}

\begin{code}
isFunTyCon :: TyCon -> Bool
isFunTyCon (FunTyCon {}) = True
isFunTyCon _             = False

-- | Test if the 'TyCon' is algebraic but abstract (invisible data constructors)
isAbstractTyCon :: TyCon -> Bool
isAbstractTyCon (AlgTyCon { algTcRhs = AbstractTyCon {} }) = True
isAbstractTyCon _ = False

-- | Make an algebraic 'TyCon' abstract. Panics if the supplied 'TyCon' is not algebraic
makeTyConAbstract :: TyCon -> TyCon
makeTyConAbstract tc@(AlgTyCon { algTcRhs = rhs })
  = tc { algTcRhs = AbstractTyCon (isDistinctAlgRhs rhs) }
makeTyConAbstract tc = pprPanic "makeTyConAbstract" (ppr tc)

-- | Does this 'TyCon' represent something that cannot be defined in Haskell?
isPrimTyCon :: TyCon -> Bool
isPrimTyCon (PrimTyCon {}) = True
isPrimTyCon _              = False

-- | Is this 'TyCon' unlifted (i.e. cannot contain bottom)? Note that this can only
-- be true for primitive and unboxed-tuple 'TyCon's
isUnLiftedTyCon :: TyCon -> Bool
isUnLiftedTyCon (PrimTyCon  {isUnLifted = is_unlifted}) = is_unlifted
isUnLiftedTyCon (TupleTyCon {tyConTupleSort = sort})    = not (isBoxed (tupleSortBoxity sort))
isUnLiftedTyCon _                                       = False

-- | Returns @True@ if the supplied 'TyCon' resulted from either a
-- @data@ or @newtype@ declaration
isAlgTyCon :: TyCon -> Bool
isAlgTyCon (AlgTyCon {})   = True
isAlgTyCon (TupleTyCon {}) = True
isAlgTyCon _               = False

isDataTyCon :: TyCon -> Bool
-- ^ Returns @True@ for data types that are /definitely/ represented by
-- heap-allocated constructors.  These are scrutinised by Core-level
-- @case@ expressions, and they get info tables allocated for them.
--
-- Generally, the function will be true for all @data@ types and false
-- for @newtype@s, unboxed tuples and type family 'TyCon's. But it is
-- not guaranteed to return @True@ in all cases that it could.
--
-- NB: for a data type family, only the /instance/ 'TyCon's
--     get an info table.  The family declaration 'TyCon' does not
isDataTyCon (AlgTyCon {algTcRhs = rhs})
  = case rhs of
        DataTyCon {}       -> True
        NewTyCon {}        -> False
        DataFamilyTyCon {} -> False
        AbstractTyCon {}   -> False      -- We don't know, so return False
isDataTyCon (TupleTyCon {tyConTupleSort = sort}) = isBoxed (tupleSortBoxity sort)
isDataTyCon _ = False

-- | 'isDistinctTyCon' is true of 'TyCon's that are equal only to
-- themselves, even via coercions (except for unsafeCoerce).
-- This excludes newtypes, type functions, type synonyms.
-- It relates directly to the FC consistency story:
--     If the axioms are consistent,
--     and  co : S tys ~ T tys, and S,T are "distinct" TyCons,
--     then S=T.
-- Cf Note [Pruning dead case alternatives] in Unify
isDistinctTyCon :: TyCon -> Bool
isDistinctTyCon (AlgTyCon {algTcRhs = rhs}) = isDistinctAlgRhs rhs
isDistinctTyCon (FunTyCon {})               = True
isDistinctTyCon (TupleTyCon {})             = True
isDistinctTyCon (PrimTyCon {})              = True
isDistinctTyCon (PromotedDataCon {})        = True
isDistinctTyCon _                           = False

isDistinctAlgRhs :: AlgTyConRhs -> Bool
isDistinctAlgRhs (DataTyCon {})           = True
isDistinctAlgRhs (DataFamilyTyCon {})     = True
isDistinctAlgRhs (AbstractTyCon distinct) = distinct
isDistinctAlgRhs (NewTyCon {})            = False

-- | Is this 'TyCon' that for a @newtype@
isNewTyCon :: TyCon -> Bool
isNewTyCon (AlgTyCon {algTcRhs = NewTyCon {}}) = True
isNewTyCon _                                   = False

-- | Take a 'TyCon' apart into the 'TyVar's it scopes over, the 'Type' it expands
-- into, and (possibly) a coercion from the representation type to the @newtype@.
-- Returns @Nothing@ if this is not possible.
unwrapNewTyCon_maybe :: TyCon -> Maybe ([TyVar], Type, CoAxiom Unbranched)
unwrapNewTyCon_maybe (AlgTyCon { tyConTyVars = tvs,
                                 algTcRhs = NewTyCon { nt_co = co,
                                                       nt_rhs = rhs }})
                           = Just (tvs, rhs, co)
unwrapNewTyCon_maybe _     = Nothing

isProductTyCon :: TyCon -> Bool
-- True of datatypes or newtypes that have
--   one, vanilla, data constructor
isProductTyCon tc@(AlgTyCon {}) = case algTcRhs tc of
                                    DataTyCon{ data_cons = [data_con] }
                                                -> isVanillaDataCon data_con
                                    NewTyCon {} -> True
                                    _           -> False
isProductTyCon (TupleTyCon {})  = True
isProductTyCon _                = False


isDataProductTyCon_maybe :: TyCon -> Maybe DataCon
-- True of datatypes (not newtypes) with 
--   one, vanilla, data constructor
isDataProductTyCon_maybe (AlgTyCon { algTcRhs = DataTyCon { data_cons = cons } })
  | [con] <- cons         -- Singleton
  , isVanillaDataCon con  -- Vanilla
  = Just con
isDataProductTyCon_maybe (TupleTyCon { dataCon = con })
  = Just con
isDataProductTyCon_maybe _ = Nothing

-- | Is this a 'TyCon' representing a type synonym (@type@)?
isSynTyCon :: TyCon -> Bool
isSynTyCon (SynTyCon {}) = True
isSynTyCon _             = False

-- As for newtypes, it is in some contexts important to distinguish between
-- closed synonyms and synonym families, as synonym families have no unique
-- right hand side to which a synonym family application can expand.
--

isDecomposableTyCon :: TyCon -> Bool
-- True iff we can decompose (T a b c) into ((T a b) c)
-- Specifically NOT true of synonyms (open and otherwise)
isDecomposableTyCon (SynTyCon {}) = False
isDecomposableTyCon _other        = True

-- | Is this an algebraic 'TyCon' declared with the GADT syntax?
isGadtSyntaxTyCon :: TyCon -> Bool
isGadtSyntaxTyCon (AlgTyCon { algTcGadtSyntax = res }) = res
isGadtSyntaxTyCon _                                    = False

-- | Is this an algebraic 'TyCon' which is just an enumeration of values?
isEnumerationTyCon :: TyCon -> Bool
-- See Note [Enumeration types] in TyCon
isEnumerationTyCon (AlgTyCon {algTcRhs = DataTyCon { is_enum = res }}) = res
isEnumerationTyCon (TupleTyCon {tyConArity = arity}) = arity == 0
isEnumerationTyCon _                                                   = False

-- | Is this a 'TyCon', synonym or otherwise, that may have further instances appear?
isFamilyTyCon :: TyCon -> Bool
isFamilyTyCon (SynTyCon {synTcRhs = SynFamilyTyCon {}})  = True
isFamilyTyCon (AlgTyCon {algTcRhs = DataFamilyTyCon {}}) = True
isFamilyTyCon _ = False

-- | Is this a synonym 'TyCon' that can have may have further instances appear?
isSynFamilyTyCon :: TyCon -> Bool
isSynFamilyTyCon (SynTyCon {synTcRhs = SynFamilyTyCon {}}) = True
isSynFamilyTyCon _ = False

isOpenSynFamilyTyCon :: TyCon -> Bool
isOpenSynFamilyTyCon (SynTyCon {synTcRhs = SynFamilyTyCon { synf_open = is_open } }) = is_open
isOpenSynFamilyTyCon _ = False

-- | Is this a synonym 'TyCon' that can have may have further instances appear?
isDataFamilyTyCon :: TyCon -> Bool
isDataFamilyTyCon (AlgTyCon {algTcRhs = DataFamilyTyCon {}}) = True
isDataFamilyTyCon _ = False

-- | Injective 'TyCon's can be decomposed, so that
--     T ty1 ~ T ty2  =>  ty1 ~ ty2
isInjectiveTyCon :: TyCon -> Bool
isInjectiveTyCon tc = not (isSynTyCon tc)
        -- Ultimately we may have injective associated types
        -- in which case this test will become more interesting
        --
        -- It'd be unusual to call isInjectiveTyCon on a regular H98
        -- type synonym, because you should probably have expanded it first
        -- But regardless, it's not injective!

-- | Are we able to extract informationa 'TyVar' to class argument list
-- mappping from a given 'TyCon'?
isTyConAssoc :: TyCon -> Bool
isTyConAssoc tc = isJust (tyConAssoc_maybe tc)

tyConAssoc_maybe :: TyCon -> Maybe Class
tyConAssoc_maybe tc = case tyConParent tc of
                        AssocFamilyTyCon cls -> Just cls
                        _                    -> Nothing

-- The unit tycon didn't used to be classed as a tuple tycon
-- but I thought that was silly so I've undone it
-- If it can't be for some reason, it should be a AlgTyCon
isTupleTyCon :: TyCon -> Bool
-- ^ Does this 'TyCon' represent a tuple?
--
-- NB: when compiling @Data.Tuple@, the tycons won't reply @True@ to
-- 'isTupleTyCon', because they are built as 'AlgTyCons'.  However they
-- get spat into the interface file as tuple tycons, so I don't think
-- it matters.
isTupleTyCon (TupleTyCon {}) = True
isTupleTyCon _               = False

-- | Is this the 'TyCon' for an unboxed tuple?
isUnboxedTupleTyCon :: TyCon -> Bool
isUnboxedTupleTyCon (TupleTyCon {tyConTupleSort = sort}) = not (isBoxed (tupleSortBoxity sort))
isUnboxedTupleTyCon _                                    = False

-- | Is this the 'TyCon' for a boxed tuple?
isBoxedTupleTyCon :: TyCon -> Bool
isBoxedTupleTyCon (TupleTyCon {tyConTupleSort = sort}) = isBoxed (tupleSortBoxity sort)
isBoxedTupleTyCon _                                    = False

-- | Extract the boxity of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConBoxity :: TyCon -> Boxity
tupleTyConBoxity tc = tupleSortBoxity (tyConTupleSort tc)

-- | Extract the 'TupleSort' of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConSort :: TyCon -> TupleSort
tupleTyConSort tc = tyConTupleSort tc

-- | Extract the arity of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConArity :: TyCon -> Arity
tupleTyConArity tc = tyConArity tc

-- | Is this a recursive 'TyCon'?
isRecursiveTyCon :: TyCon -> Bool
isRecursiveTyCon (AlgTyCon {algTcRec = Recursive}) = True
isRecursiveTyCon _                                 = False

promotableTyCon_maybe :: TyCon -> Maybe TyCon
promotableTyCon_maybe (AlgTyCon { tcPromoted = prom })   = prom
promotableTyCon_maybe (TupleTyCon { tcPromoted = prom }) = prom
promotableTyCon_maybe _                                  = Nothing

promoteTyCon :: TyCon -> TyCon
promoteTyCon tc = case promotableTyCon_maybe tc of
                    Just prom_tc -> prom_tc
                    Nothing      -> pprPanic "promoteTyCon" (ppr tc)

-- | Is this the 'TyCon' of a foreign-imported type constructor?
isForeignTyCon :: TyCon -> Bool
isForeignTyCon (PrimTyCon {tyConExtName = Just _}) = True
isForeignTyCon _                                   = False

-- | Is this a PromotedTyCon?
isPromotedTyCon :: TyCon -> Bool
isPromotedTyCon (PromotedTyCon {}) = True
isPromotedTyCon _                  = False

-- | Retrieves the promoted TyCon if this is a PromotedTyCon;
isPromotedTyCon_maybe :: TyCon -> Maybe TyCon
isPromotedTyCon_maybe (PromotedTyCon { ty_con = tc }) = Just tc
isPromotedTyCon_maybe _ = Nothing

-- | Is this a PromotedDataCon?
isPromotedDataCon :: TyCon -> Bool
isPromotedDataCon (PromotedDataCon {}) = True
isPromotedDataCon _                    = False

-- | Retrieves the promoted DataCon if this is a PromotedDataCon;
isPromotedDataCon_maybe :: TyCon -> Maybe DataCon
isPromotedDataCon_maybe (PromotedDataCon { dataCon = dc }) = Just dc
isPromotedDataCon_maybe _ = Nothing

-- | Identifies implicit tycons that, in particular, do not go into interface
-- files (because they are implicitly reconstructed when the interface is
-- read).
--
-- Note that:
--
-- * Associated families are implicit, as they are re-constructed from
--   the class declaration in which they reside, and
--
-- * Family instances are /not/ implicit as they represent the instance body
--   (similar to a @dfun@ does that for a class instance).
isImplicitTyCon :: TyCon -> Bool
isImplicitTyCon tycon
  | isTyConAssoc tycon = True
  | isSynTyCon tycon   = False
  | isAlgTyCon tycon   = isTupleTyCon tycon
  | otherwise          = True
        -- 'otherwise' catches: FunTyCon, PrimTyCon,
        -- PromotedDataCon, PomotedTypeTyCon

tyConCType_maybe :: TyCon -> Maybe CType
tyConCType_maybe tc@(AlgTyCon {}) = tyConCType tc
tyConCType_maybe _ = Nothing
\end{code}


-----------------------------------------------
--      Expand type-constructor applications
-----------------------------------------------

\begin{code}
tcExpandTyCon_maybe, coreExpandTyCon_maybe
        :: TyCon
        -> [tyco]                 -- ^ Arguments to 'TyCon'
        -> Maybe ([(TyVar,tyco)],
                  Type,
                  [tyco])         -- ^ Returns a 'TyVar' substitution, the body type
                                  -- of the synonym (not yet substituted) and any arguments
                                  -- remaining from the application

-- ^ Used to create the view the /typechecker/ has on 'TyCon's.
-- We expand (closed) synonyms only, cf. 'coreExpandTyCon_maybe'
tcExpandTyCon_maybe (SynTyCon {tyConTyVars = tvs,
                               synTcRhs = SynonymTyCon rhs }) tys
   = expand tvs rhs tys
tcExpandTyCon_maybe _ _ = Nothing

---------------

-- ^ Used to create the view /Core/ has on 'TyCon's. We expand
-- not only closed synonyms like 'tcExpandTyCon_maybe',
-- but also non-recursive @newtype@s
coreExpandTyCon_maybe tycon tys = tcExpandTyCon_maybe tycon tys


----------------
expand  :: [TyVar] -> Type                 -- Template
        -> [a]                             -- Args
        -> Maybe ([(TyVar,a)], Type, [a])  -- Expansion
expand tvs rhs tys
  = case n_tvs `compare` length tys of
        LT -> Just (tvs `zip` tys, rhs, drop n_tvs tys)
        EQ -> Just (tvs `zip` tys, rhs, [])
        GT -> Nothing
   where
     n_tvs = length tvs
\end{code}

\begin{code}
tyConKind :: TyCon -> Kind
tyConKind = tc_kind

-- | As 'tyConDataCons_maybe', but returns the empty list of constructors if no constructors
-- could be found
tyConDataCons :: TyCon -> [DataCon]
-- It's convenient for tyConDataCons to return the
-- empty list for type synonyms etc
tyConDataCons tycon = tyConDataCons_maybe tycon `orElse` []

-- | Determine the 'DataCon's originating from the given 'TyCon', if the 'TyCon' is the
-- sort that can have any constructors (note: this does not include abstract algebraic types)
tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConDataCons_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons = cons }}) = Just cons
tyConDataCons_maybe (AlgTyCon {algTcRhs = NewTyCon { data_con = con }})    = Just [con]
tyConDataCons_maybe (TupleTyCon {dataCon = con})                           = Just [con]
tyConDataCons_maybe _                                                      = Nothing

-- | Determine the number of value constructors a 'TyCon' has. Panics if the 'TyCon'
-- is not algebraic or a tuple
tyConFamilySize  :: TyCon -> Int
tyConFamilySize (AlgTyCon   {algTcRhs = DataTyCon {data_cons = cons}}) =
  length cons
tyConFamilySize (AlgTyCon   {algTcRhs = NewTyCon {}})        = 1
tyConFamilySize (AlgTyCon   {algTcRhs = DataFamilyTyCon {}}) = 0
tyConFamilySize (TupleTyCon {})                              = 1
tyConFamilySize other = pprPanic "tyConFamilySize:" (ppr other)

-- | Extract an 'AlgTyConRhs' with information about data constructors from an algebraic or tuple
-- 'TyCon'. Panics for any other sort of 'TyCon'
algTyConRhs :: TyCon -> AlgTyConRhs
algTyConRhs (AlgTyCon {algTcRhs = rhs}) = rhs
algTyConRhs (TupleTyCon {dataCon = con, tyConArity = arity})
    = DataTyCon { data_cons = [con], is_enum = arity == 0 }
algTyConRhs other = pprPanic "algTyConRhs" (ppr other)
\end{code}

\begin{code}
-- | Extract the bound type variables and type expansion of a type synonym 'TyCon'. Panics if the
-- 'TyCon' is not a synonym
newTyConRhs :: TyCon -> ([TyVar], Type)
newTyConRhs (AlgTyCon {tyConTyVars = tvs, algTcRhs = NewTyCon { nt_rhs = rhs }}) = (tvs, rhs)
newTyConRhs tycon = pprPanic "newTyConRhs" (ppr tycon)

-- | Extract the bound type variables and type expansion of an eta-contracted type synonym 'TyCon'.
-- Panics if the 'TyCon' is not a synonym
newTyConEtadRhs :: TyCon -> ([TyVar], Type)
newTyConEtadRhs (AlgTyCon {algTcRhs = NewTyCon { nt_etad_rhs = tvs_rhs }}) = tvs_rhs
newTyConEtadRhs tycon = pprPanic "newTyConEtadRhs" (ppr tycon)

-- | Extracts the @newtype@ coercion from such a 'TyCon', which can be used to construct something
-- with the @newtype@s type from its representation type (right hand side). If the supplied 'TyCon'
-- is not a @newtype@, returns @Nothing@
newTyConCo_maybe :: TyCon -> Maybe (CoAxiom Unbranched)
newTyConCo_maybe (AlgTyCon {algTcRhs = NewTyCon { nt_co = co }}) = Just co
newTyConCo_maybe _                                               = Nothing

newTyConCo :: TyCon -> CoAxiom Unbranched
newTyConCo tc = case newTyConCo_maybe tc of
                 Just co -> co
                 Nothing -> pprPanic "newTyConCo" (ppr tc)

-- | Find the primitive representation of a 'TyCon'
tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon {primTyConRep = rep}) = rep
tyConPrimRep tc = ASSERT(not (isUnboxedTupleTyCon tc)) PtrRep
\end{code}

\begin{code}
-- | Find the \"stupid theta\" of the 'TyCon'. A \"stupid theta\" is the context to the left of
-- an algebraic type declaration, e.g. @Eq a@ in the declaration @data Eq a => T a ...@
tyConStupidTheta :: TyCon -> [PredType]
tyConStupidTheta (AlgTyCon {algTcStupidTheta = stupid}) = stupid
tyConStupidTheta (TupleTyCon {})                        = []
tyConStupidTheta tycon = pprPanic "tyConStupidTheta" (ppr tycon)
\end{code}

\begin{code}
-- | Extract the 'TyVar's bound by a vanilla type synonym (not familiy)
-- and the corresponding (unsubstituted) right hand side.
synTyConDefn_maybe :: TyCon -> Maybe ([TyVar], Type)
synTyConDefn_maybe (SynTyCon {tyConTyVars = tyvars, synTcRhs = SynonymTyCon ty})
  = Just (tyvars, ty)
synTyConDefn_maybe _ = Nothing

-- | Extract the information pertaining to the right hand side of a type synonym (@type@) declaration.
synTyConRhs_maybe :: TyCon -> Maybe (SynTyConRhs Type)
synTyConRhs_maybe (SynTyCon {synTcRhs = rhs}) = Just rhs
synTyConRhs_maybe _                           = Nothing
\end{code}

\begin{code}
-- | If the given 'TyCon' has a /single/ data constructor, i.e. it is a @data@ type with one
-- alternative, a tuple type or a @newtype@ then that constructor is returned. If the 'TyCon'
-- has more than one constructor, or represents a primitive or function type constructor then
-- @Nothing@ is returned. In any other case, the function panics
tyConSingleDataCon_maybe :: TyCon -> Maybe DataCon
tyConSingleDataCon_maybe (TupleTyCon {dataCon = c})                            = Just c
tyConSingleDataCon_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons = [c] }}) = Just c
tyConSingleDataCon_maybe (AlgTyCon {algTcRhs = NewTyCon { data_con = c }})     = Just c
tyConSingleDataCon_maybe _                                                     = Nothing

tyConSingleAlgDataCon_maybe :: TyCon -> Maybe DataCon
-- Returns (Just con) for single-constructor *algebraic* data types
-- *not* newtypes
tyConSingleAlgDataCon_maybe (TupleTyCon {dataCon = c})                            = Just c
tyConSingleAlgDataCon_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons = [c] }}) = Just c
tyConSingleAlgDataCon_maybe _                                                     = Nothing
\end{code}

\begin{code}
-- | Is this 'TyCon' that for a class instance?
isClassTyCon :: TyCon -> Bool
isClassTyCon (AlgTyCon {algTcParent = ClassTyCon _}) = True
isClassTyCon _                                       = False

-- | If this 'TyCon' is that for a class instance, return the class it is for.
-- Otherwise returns @Nothing@
tyConClass_maybe :: TyCon -> Maybe Class
tyConClass_maybe (AlgTyCon {algTcParent = ClassTyCon clas}) = Just clas
tyConClass_maybe _                                          = Nothing

tyConTuple_maybe :: TyCon -> Maybe TupleSort
tyConTuple_maybe (TupleTyCon {tyConTupleSort = sort}) = Just sort
tyConTuple_maybe _                                    = Nothing

----------------------------------------------------------------------------
tyConParent :: TyCon -> TyConParent
tyConParent (AlgTyCon {algTcParent = parent}) = parent
tyConParent (SynTyCon {synTcParent = parent}) = parent
tyConParent _                                 = NoParentTyCon

----------------------------------------------------------------------------
-- | Is this 'TyCon' that for a data family instance?
isFamInstTyCon :: TyCon -> Bool
isFamInstTyCon tc = case tyConParent tc of
                      FamInstTyCon {} -> True
                      _               -> False

tyConFamInstSig_maybe :: TyCon -> Maybe (TyCon, [Type], CoAxiom Unbranched)
tyConFamInstSig_maybe tc
  = case tyConParent tc of
      FamInstTyCon ax f ts -> Just (f, ts, ax)
      _                    -> Nothing

-- | If this 'TyCon' is that of a family instance, return the family in question
-- and the instance types. Otherwise, return @Nothing@
tyConFamInst_maybe :: TyCon -> Maybe (TyCon, [Type])
tyConFamInst_maybe tc
  = case tyConParent tc of
      FamInstTyCon _ f ts -> Just (f, ts)
      _                   -> Nothing

-- | If this 'TyCon' is that of a family instance, return a 'TyCon' which represents
-- a coercion identifying the representation type with the type instance family.
-- Otherwise, return @Nothing@
tyConFamilyCoercion_maybe :: TyCon -> Maybe (CoAxiom Unbranched)
tyConFamilyCoercion_maybe tc
  = case tyConParent tc of
      FamInstTyCon co _ _ -> Just co
      _                   -> Nothing
\end{code}


%************************************************************************
%*                                                                      *
\subsection[TyCon-instances]{Instance declarations for @TyCon@}
%*                                                                      *
%************************************************************************

@TyCon@s are compared by comparing their @Unique@s.

The strictness analyser needs @Ord@. It is a lexicographic order with
the property @(a<=b) || (b<=a)@.

\begin{code}
instance Eq TyCon where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord TyCon where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = getUnique a `compare` getUnique b

instance Uniquable TyCon where
    getUnique tc = tyConUnique tc

instance Outputable TyCon where
  -- At the moment a promoted TyCon has the same Name as its
  -- corresponding TyCon, so we add the quote to distinguish it here
  ppr tc = pprPromotionQuote tc <> ppr (tyConName tc)

pprPromotionQuote :: TyCon -> SDoc
pprPromotionQuote (PromotedDataCon {}) = char '\''   -- Quote promoted DataCons in types
pprPromotionQuote (PromotedTyCon {})   = ifPprDebug (char '\'') 
pprPromotionQuote _                    = empty       -- However, we don't quote TyCons in kinds
                                                     -- e.g.   type family T a :: Bool -> *
                                                     -- cf Trac #5952.  Except with -dppr-debug

instance NamedThing TyCon where
    getName = tyConName

instance Data.Data TyCon where
    -- don't traverse?
    toConstr _   = abstractConstr "TyCon"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "TyCon"

\end{code}
