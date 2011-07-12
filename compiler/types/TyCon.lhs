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

	-- ** Coercion axiom constructors
        CoAxiom(..), coAxiomName, coAxiomArity,

        -- ** Constructing TyCons
	mkAlgTyCon,
	mkClassTyCon,
	mkFunTyCon,
	mkPrimTyCon,
	mkKindTyCon,
	mkLiftedPrimTyCon,
	mkTupleTyCon,
	mkSynTyCon,
        mkSuperKindTyCon,
        mkForeignTyCon,
        mkAnyTyCon,

        -- ** Predicates on TyCons
        isAlgTyCon,
        isClassTyCon, isFamInstTyCon, 
        isFunTyCon, 
        isPrimTyCon,
        isTupleTyCon, isUnboxedTupleTyCon, isBoxedTupleTyCon, 
        isSynTyCon, isClosedSynTyCon,
        isSuperKindTyCon, isDecomposableTyCon,
        isForeignTyCon, isAnyTyCon, tyConHasKind,

	isInjectiveTyCon,
	isDataTyCon, isProductTyCon, isEnumerationTyCon, 
        isNewTyCon, isAbstractTyCon,
        isFamilyTyCon, isSynFamilyTyCon, isDataFamilyTyCon,
        isUnLiftedTyCon,
	isGadtSyntaxTyCon,
	isTyConAssoc,
	isRecursiveTyCon,
	isHiBootTyCon,
        isImplicitTyCon, 

        -- ** Extracting information out of TyCons
	tyConName,
	tyConKind,
	tyConUnique,
	tyConTyVars,
	tyConDataCons, tyConDataCons_maybe, tyConSingleDataCon_maybe,
	tyConFamilySize,
	tyConStupidTheta,
	tyConArity,
        tyConParent,
	tyConClass_maybe,
	tyConFamInst_maybe, tyConFamilyCoercion_maybe,tyConFamInstSig_maybe,
        synTyConDefn, synTyConRhs, synTyConType,
        tyConExtName,           -- External name for foreign types
	algTyConRhs,
        newTyConRhs, newTyConEtadRhs, unwrapNewTyCon_maybe, 
        tupleTyConBoxity, tupleTyConArity,

        -- ** Manipulating TyCons
	tcExpandTyCon_maybe, coreExpandTyCon_maybe,
	makeTyConAbstract,
	newTyConCo, newTyConCo_maybe,

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
import Name
import PrelNames
import Maybes
import Outputable
import FastString
import Constants
import Util
import qualified Data.Data as Data
import Data.Typeable hiding (TyCon)
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
	type instance F (F Int) = ...	-- BAD!

* Translation of type family decl:
	type family F a :: *
  translates to
    a SynTyCon 'F', whose SynTyConRhs is SynFamilyTyCon

* Translation of type instance decl:
	type instance F [a] = Maybe a
  translates to a "representation TyCon", 'R:FList', where
     R:FList is a SynTyCon, whose 
       SynTyConRhs is (SynonymTyCon (Maybe a))
       TyConParent is (FamInstTyCon F [a] co)
         where co :: F [a] ~ R:FList a

  It's very much as if the user had written
       type instance F [a] = R:FList a
       type R:FList a = Maybe a
  Indeed, in GHC's internal representation, the RHS of every
  'type instance' is simply an application of the representation
  TyCon to the quantified varaibles.

  The intermediate representation TyCon is a bit gratuitous, but 
  it means that:

        each 'type instance' decls is in 1-1 correspondance 
	with its representation TyCon

  So the result of typechecking a 'type instance' decl is just a
  TyCon.  In turn this means that type and data families can be
  treated uniformly.

* Translation of type family decl:
	type family F a :: *
  translates to
    a SynTyCon 'F', whose SynTyConRhs is SynFamilyTyCon

* Translation of type instance decl:
	type instance F [a] = Maybe a
  translates to
    A SynTyCon 'R:FList a', whose 
       SynTyConRhs is (SynonymTyCon (Maybe a))
       TyConParent is (FamInstTyCon F [a] co)
         where co :: F [a] ~ R:FList a
    Notice that we introduce a gratuitous vanilla type synonym
       type R:FList a = Maybe a
    solely so that type and data families can be treated more
    uniformly, via a single FamInstTyCon descriptor        

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

%************************************************************************
%*									*
\subsection{The data type}
%*									*
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
	tc_kind   :: Kind,
	tyConArity  :: Arity,

	tyConTyVars :: [TyVar],	  -- ^ The type variables used in the type constructor.
                                  -- Invariant: length tyvars = arity
	                          -- Precisely, this list scopes over:
	                          --
	                          -- 1. The 'algTcStupidTheta'
				  -- 2. The cached types in 'algTyConRhs.NewTyCon'
				  -- 3. The family instance types if present
				  --
				  -- Note that it does /not/ scope over the data constructors.

	algTcGadtSyntax  :: Bool,	-- ^ Was the data type declared with GADT syntax? 
					-- If so, that doesn't mean it's a true GADT; 
					-- only that the "where" form was used. 
                                        -- This field is used only to guide pretty-printing

	algTcStupidTheta :: [PredType],	-- ^ The \"stupid theta\" for the data type 
                                        -- (always empty for GADTs).
	                                -- A \"stupid theta\" is the context to the left 
	                                -- of an algebraic type declaration, 
                                        -- e.g. @Eq a@ in the declaration 
                                        --    @data Eq a => T a ...@.

	algTcRhs :: AlgTyConRhs,  -- ^ Contains information about the 
                                  -- data constructors of the algebraic type

	algTcRec :: RecFlag,	  -- ^ Tells us whether the data type is part 
                                  -- of a mutually-recursive group or not
	
	algTcParent :: TyConParent	-- ^ Gives the class or family declaration 'TyCon' 
                                        -- for derived 'TyCon's representing class 
                                        -- or family instances, respectively. 
                                        -- See also 'synTcParent'
    }

  -- | Represents the infinite family of tuple type constructors, 
  --   @()@, @(a,b)@, @(# a, b #)@ etc.
  | TupleTyCon {
	tyConUnique :: Unique,
	tyConName   :: Name,
	tc_kind   :: Kind,
	tyConArity  :: Arity,
	tyConBoxed  :: Boxity,
	tyConTyVars :: [TyVar],
	dataCon     :: DataCon -- ^ Corresponding tuple data constructor
    }

  -- | Represents type synonyms
  | SynTyCon {
	tyConUnique  :: Unique,
	tyConName    :: Name,
	tc_kind    :: Kind,
	tyConArity   :: Arity,

	tyConTyVars  :: [TyVar],	-- Bound tyvars

	synTcRhs     :: SynTyConRhs,	-- ^ Contains information about the 
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
	tyConArity    :: Arity,		-- SLPJ Oct06: I'm not sure what the significance
					--	       of the arity of a primtycon is!

	primTyConRep  :: PrimRep,	-- ^ Many primitive tycons are unboxed, but some are
                       			--   boxed (represented by pointers). This 'PrimRep'
			                --   holds that information.
					-- Only relevant if tc_kind = *

	isUnLifted   :: Bool,		-- ^ Most primitive tycons are unlifted 
                                        --   (may not contain bottom)
					--   but foreign-imported ones may be lifted

	tyConExtName :: Maybe FastString   -- ^ @Just e@ for foreign-imported types, 
                                           --   holds the name of the imported thing
    }

  -- | Any types.  Like tuples, this is a potentially-infinite family of TyCons
  --   one for each distinct Kind. They have no values at all.
  --   Because there are infinitely many of them (like tuples) they are 
  --   defined in GHC.Prim and have names like "Any(*->*)".  
  --   Their Unique is derived from the OccName.
  -- See Note [Any types] in TysPrim
  | AnyTyCon {
	tyConUnique  :: Unique,
	tyConName    :: Name,
	tc_kind      :: Kind	-- Never = *; that is done via PrimTyCon
		     		-- See Note [Any types] in TysPrim
    }

  -- | Super-kinds. These are "kinds-of-kinds" and are never seen in
  -- Haskell source programs.  There are only two super-kinds: TY (aka
  -- "box"), which is the super-kind of kinds that construct types
  -- eventually, and CO (aka "diamond"), which is the super-kind of
  -- kinds that just represent coercions.
  --
  -- Super-kinds have no kind themselves, and have arity zero
  | SuperKindTyCon {
        tyConUnique :: Unique,
        tyConName   :: Name
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
			  --	  (see the tag assignment in DataCon.mkDataCon)

	is_enum :: Bool   -- ^ Cached value: is this an enumeration type? 
                          --   See Note [Enumeration types]
    }

  -- | Information about those 'TyCon's derived from a @newtype@ declaration
  | NewTyCon {
	data_con :: DataCon,	-- ^ The unique constructor for the @newtype@. 
                                --   It has no existentials

	nt_rhs :: Type,		-- ^ Cached value: the argument type of the constructor, 
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
        nt_co :: CoAxiom     -- The axiom coercion that creates the @newtype@ from 
                             -- the representation 'Type'.
                                
                             -- See Note [Newtype coercions]
                             -- Invariant: arity = #tvs in nt_etad_rhs;
                             --	See Note [Newtype eta]
                             -- Watch out!  If any newtypes become transparent
                             -- again check Trac #1072.
    }

-- | Extract those 'DataCon's that we are able to learn about.  Note
-- that visibility in this sense does not correspond to visibility in
-- the context of any particular user program!
visibleDataCons :: AlgTyConRhs -> [DataCon]
visibleDataCons AbstractTyCon      	      = []
visibleDataCons DataFamilyTyCon {}		      = []
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
  | ClassTyCon      	
	Class		-- INVARIANT: the classTyCon of this Class is the current tycon

  -- | An *associated* type of a class.  
  | AssocFamilyTyCon   
        Class		-- The class in whose declaration the family is declared
                        -- The 'tyConTyVars' of this 'TyCon' may mention some
                        -- of the same type variables as the classTyVars of the
                        -- parent 'Class'.  E.g.
                        --
                        -- @
                        --    class C a b where
                        --      data T c a
                        -- @
                        --
                        -- Here the 'a' is shared with the 'Class', and that is
                        -- important. In an instance declaration we expect the
                        -- two to be instantiated the same way.  Eg.
                        --
                        -- @
                        --    instanc C [x] (Tree y) where
                        --      data T c [x] = T1 x | T2 c
                        -- @

  -- | Type constructors representing an instance of a type family. Parameters:
  --
  --  1) The type family in question
  --
  --  2) Instance types; free variables are the 'tyConTyVars'
  --  of the current 'TyCon' (not the family one). INVARIANT: 
  --  the number of types matches the arity of the family 'TyCon'
  --
  --  3) A 'CoTyCon' identifying the representation
  --  type with the type instance family
  | FamInstTyCon	  -- See Note [Data type families]
    			  -- and Note [Type synonym families]
	TyCon   -- The family TyCon
	[Type]	-- Argument types (mentions the tyConTyVars of this TyCon)
        CoAxiom   -- The coercion constructor

	-- E.g.  data intance T [a] = ...
	-- gives a representation tycon:
	--	data R:TList a = ...
	-- 	axiom co a :: T [a] ~ R:TList a
	-- with R:TList's algTcParent = FamInstTyCon T [a] co

-- | Checks the invariants of a 'TyConParent' given the appropriate type class name, if any
okParent :: Name -> TyConParent -> Bool
okParent _       NoParentTyCon                    = True
okParent tc_name (AssocFamilyTyCon cls)           = tc_name `elem` map tyConName (classATs cls)
okParent tc_name (ClassTyCon cls)                 = tc_name == tyConName (classTyCon cls)
okParent _       (FamInstTyCon fam_tc tys _co_tc) = tyConArity fam_tc == length tys

isNoParent :: TyConParent -> Bool
isNoParent NoParentTyCon = True
isNoParent _             = False

--------------------

-- | Information pertaining to the expansion of a type synonym (@type@)
data SynTyConRhs
  = -- | An ordinary type synonyn.
    SynonymTyCon      
       Type	      -- This 'Type' is the rhs, and may mention from 'tyConTyVars'. 
		      -- It acts as a template for the expansion when the 'TyCon' 
                      -- is applied to some types.

   -- | A type synonym family  e.g. @type family F x y :: * -> *@
   | SynFamilyTyCon
\end{code}

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
The NewTyCon field nt_co is a a TyCon (a coercion constructor in fact)
which is used for coercing from the representation type of the
newtype, to the newtype itself. For example,

   newtype T a = MkT (a -> a)

the NewTyCon for T will contain nt_co = CoT where CoT t : T t ~ t ->
t.  This TyCon is a CoTyCon, so it does not have a kind on its
own; it basically has its own typing rule for the fully-applied
version.  If the newtype T has k type variables then CoT has arity at
most k.  In the case that the right hand side is a type application
ending with the same type variables as the left hand side, we
"eta-contract" the coercion.  So if we had

   newtype S a = MkT [a]

then we would generate the arity 0 coercion CoS : S ~ [].  The
primary reason we do this is to make newtype deriving cleaner.

In the paper we'd write
	axiom CoT : (forall t. T t) ~ (forall t. [t])
and then when we used CoT at a particular type, s, we'd say
	CoT @ s
which encodes as (TyConApp instCoercionTyCon [TyConApp CoT [], s])

But in GHC we instead make CoT into a new piece of type syntax, CoTyCon,
(like instCoercionTyCon, symCoercionTyCon etc), which must always
be saturated, but which encodes as
	TyConApp CoT [s]
In the vocabulary of the paper it's as if we had axiom declarations
like
	axiom CoT t :  T t ~ [t]

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
 and	arity:   0


%************************************************************************
%*									*
                    Coercion axioms
%*									*
%************************************************************************

\begin{code}
-- | A 'CoAxiom' is a \"coercion constructor\", i.e. a named equality axiom.
data CoAxiom
  = CoAxiom                   -- type equality axiom.
    { co_ax_unique :: Unique   -- unique identifier
    , co_ax_name   :: Name     -- name for pretty-printing
    , co_ax_tvs    :: [TyVar]  -- bound type variables 
    , co_ax_lhs    :: Type     -- left-hand side of the equality
    , co_ax_rhs    :: Type     -- right-hand side of the equality
    }
  deriving Typeable

coAxiomArity :: CoAxiom -> Arity
coAxiomArity ax = length (co_ax_tvs ax)

coAxiomName :: CoAxiom -> Name
coAxiomName = co_ax_name
\end{code}


%************************************************************************
%*									*
\subsection{PrimRep}
%*									*
%************************************************************************

A PrimRep is somewhat similar to a CgRep (see codeGen/SMRep) and a
MachRep (see cmm/CmmExpr), although each of these types has a distinct
and clearly defined purpose:

  - A PrimRep is a CgRep + information about signedness + information
    about primitive pointers (AddrRep).  Signedness and primitive
    pointers are required when passing a primitive type to a foreign
    function, but aren't needed for call/return conventions of Haskell
    functions.

  - A MachRep is a basic machine type (non-void, doesn't contain
    information on pointerhood or signedness, but contains some
    reps that don't have corresponding Haskell types).

\begin{code}
-- | A 'PrimRep' is an abstraction of a type.  It contains information that
-- the code generator needs in order to pass arguments, return results,
-- and store values of this type.
data PrimRep
  = VoidRep
  | PtrRep
  | IntRep		-- ^ Signed, word-sized value
  | WordRep		-- ^ Unsigned, word-sized value
  | Int64Rep		-- ^ Signed, 64 bit value (with 32-bit words only)
  | Word64Rep		-- ^ Unsigned, 64 bit value (with 32-bit words only)
  | AddrRep		-- ^ A pointer, but /not/ to a Haskell value (use 'PtrRep')
  | FloatRep
  | DoubleRep
  deriving( Eq, Show )

instance Outputable PrimRep where
  ppr r = text (show r)

-- | Find the size of a 'PrimRep', in words
primRepSizeW :: PrimRep -> Int
primRepSizeW IntRep   = 1
primRepSizeW WordRep  = 1
primRepSizeW Int64Rep = wORD64_SIZE `quot` wORD_SIZE
primRepSizeW Word64Rep= wORD64_SIZE `quot` wORD_SIZE
primRepSizeW FloatRep = 1    -- NB. might not take a full word
primRepSizeW DoubleRep= dOUBLE_SIZE `quot` wORD_SIZE
primRepSizeW AddrRep  = 1
primRepSizeW PtrRep   = 1
primRepSizeW VoidRep  = 0
\end{code}

%************************************************************************
%*									*
\subsection{TyCon Construction}
%*									*
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
           -> [PredType]        -- ^ Stupid theta: see 'algTcStupidTheta'
           -> AlgTyConRhs       -- ^ Information about dat aconstructors
           -> TyConParent
           -> RecFlag           -- ^ Is the 'TyCon' recursive?
           -> Bool              -- ^ Was the 'TyCon' declared with GADT syntax?
           -> TyCon
mkAlgTyCon name kind tyvars stupid rhs parent is_rec gadt_syn
  = AlgTyCon {	
	tyConName 	 = name,
	tyConUnique	 = nameUnique name,
	tc_kind	         = kind,
	tyConArity	 = length tyvars,
	tyConTyVars	 = tyvars,
	algTcStupidTheta = stupid,
	algTcRhs         = rhs,
	algTcParent	 = ASSERT( okParent name parent ) parent,
	algTcRec	 = is_rec,
	algTcGadtSyntax  = gadt_syn
    }

-- | Simpler specialization of 'mkAlgTyCon' for classes
mkClassTyCon :: Name -> Kind -> [TyVar] -> AlgTyConRhs -> Class -> RecFlag -> TyCon
mkClassTyCon name kind tyvars rhs clas is_rec =
  mkAlgTyCon name kind tyvars [] rhs (ClassTyCon clas) is_rec False

mkTupleTyCon :: Name 
             -> Kind    -- ^ Kind of the resulting 'TyCon'
             -> Arity   -- ^ Arity of the tuple
             -> [TyVar] -- ^ 'TyVar's scoped over: see 'tyConTyVars'
             -> DataCon 
             -> Boxity  -- ^ Whether the tuple is boxed or unboxed
             -> TyCon
mkTupleTyCon name kind arity tyvars con boxed 
  = TupleTyCon {
	tyConUnique = nameUnique name,
	tyConName = name,
	tc_kind = kind,
	tyConArity = arity,
	tyConBoxed = boxed,
	tyConTyVars = tyvars,
	dataCon = con
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
mkSynTyCon :: Name -> Kind -> [TyVar] -> SynTyConRhs -> TyConParent -> TyCon
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

mkAnyTyCon :: Name -> Kind -> TyCon
mkAnyTyCon name kind 
  = AnyTyCon {  tyConName = name,
		tc_kind = kind,
        	tyConUnique = nameUnique name }

-- | Create a super-kind 'TyCon'
mkSuperKindTyCon :: Name -> TyCon -- Super kinds always have arity zero
mkSuperKindTyCon name
  = SuperKindTyCon {
        tyConName = name,
        tyConUnique = nameUnique name
  }
\end{code}

\begin{code}
isFunTyCon :: TyCon -> Bool
isFunTyCon (FunTyCon {}) = True
isFunTyCon _             = False

-- | Test if the 'TyCon' is algebraic but abstract (invisible data constructors)
isAbstractTyCon :: TyCon -> Bool
isAbstractTyCon (AlgTyCon { algTcRhs = AbstractTyCon }) = True
isAbstractTyCon _ = False

-- | Make an algebraic 'TyCon' abstract. Panics if the supplied 'TyCon' is not algebraic
makeTyConAbstract :: TyCon -> TyCon
makeTyConAbstract tc@(AlgTyCon {}) = tc { algTcRhs = AbstractTyCon }
makeTyConAbstract tc = pprPanic "makeTyConAbstract" (ppr tc)

-- | Does this 'TyCon' represent something that cannot be defined in Haskell?
isPrimTyCon :: TyCon -> Bool
isPrimTyCon (PrimTyCon {}) = True
isPrimTyCon _              = False

-- | Is this 'TyCon' unlifted (i.e. cannot contain bottom)? Note that this can only
-- be true for primitive and unboxed-tuple 'TyCon's
isUnLiftedTyCon :: TyCon -> Bool
isUnLiftedTyCon (PrimTyCon  {isUnLifted = is_unlifted}) = is_unlifted
isUnLiftedTyCon (TupleTyCon {tyConBoxed = boxity})      = not (isBoxed boxity)
isUnLiftedTyCon _    				        = False

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
-- not guarenteed to return @True@ in all cases that it could.
-- 
-- NB: for a data type family, only the /instance/ 'TyCon's
--     get an info table.  The family declaration 'TyCon' does not
isDataTyCon (AlgTyCon {algTcRhs = rhs})
  = case rhs of
        DataFamilyTyCon {}  -> False
	DataTyCon {}  -> True
	NewTyCon {}   -> False
	AbstractTyCon -> False	 -- We don't know, so return False
isDataTyCon (TupleTyCon {tyConBoxed = boxity}) = isBoxed boxity
isDataTyCon _ = False

-- | Is this 'TyCon' that for a @newtype@
isNewTyCon :: TyCon -> Bool
isNewTyCon (AlgTyCon {algTcRhs = NewTyCon {}}) = True
isNewTyCon _                                   = False

-- | Take a 'TyCon' apart into the 'TyVar's it scopes over, the 'Type' it expands
-- into, and (possibly) a coercion from the representation type to the @newtype@.
-- Returns @Nothing@ if this is not possible.
unwrapNewTyCon_maybe :: TyCon -> Maybe ([TyVar], Type, CoAxiom)
unwrapNewTyCon_maybe (AlgTyCon { tyConTyVars = tvs, 
				 algTcRhs = NewTyCon { nt_co = co, 
						       nt_rhs = rhs }})
			   = Just (tvs, rhs, co)
unwrapNewTyCon_maybe _     = Nothing

isProductTyCon :: TyCon -> Bool
-- | A /product/ 'TyCon' must both:
--
-- 1. Have /one/ constructor
-- 
-- 2. /Not/ be existential
-- 
-- However other than this there are few restrictions: they may be @data@ or @newtype@ 
-- 'TyCon's of any boxity and may even be recursive.
isProductTyCon tc@(AlgTyCon {}) = case algTcRhs tc of
				    DataTyCon{ data_cons = [data_con] } 
						-> isVanillaDataCon data_con
				    NewTyCon {}	-> True
				    _           -> False
isProductTyCon (TupleTyCon {})  = True   
isProductTyCon _                = False

-- | Is this a 'TyCon' representing a type synonym (@type@)?
isSynTyCon :: TyCon -> Bool
isSynTyCon (SynTyCon {}) = True
isSynTyCon _		 = False

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
isFamilyTyCon _	= False

-- | Is this a synonym 'TyCon' that can have may have further instances appear?
isSynFamilyTyCon :: TyCon -> Bool
isSynFamilyTyCon (SynTyCon {synTcRhs = SynFamilyTyCon {}}) = True
isSynFamilyTyCon _ = False

-- | Is this a synonym 'TyCon' that can have may have further instances appear?
isDataFamilyTyCon :: TyCon -> Bool
isDataFamilyTyCon (AlgTyCon {algTcRhs = DataFamilyTyCon {}}) = True
isDataFamilyTyCon _ = False

-- | Is this a synonym 'TyCon' that can have no further instances appear?
isClosedSynTyCon :: TyCon -> Bool
isClosedSynTyCon tycon = isSynTyCon tycon && not (isFamilyTyCon tycon)

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
isTyConAssoc tc = case tyConParent tc of
                     AssocFamilyTyCon {} -> True
                     _                   -> False

-- The unit tycon didn't used to be classed as a tuple tycon
-- but I thought that was silly so I've undone it
-- If it can't be for some reason, it should be a AlgTyCon
isTupleTyCon :: TyCon -> Bool
-- ^ Does this 'TyCon' represent a tuple?
--
-- NB: when compiling @Data.Tuple@, the tycons won't reply @True@ to
-- 'isTupleTyCon', becuase they are built as 'AlgTyCons'.  However they
-- get spat into the interface file as tuple tycons, so I don't think
-- it matters.
isTupleTyCon (TupleTyCon {}) = True
isTupleTyCon _               = False

-- | Is this the 'TyCon' for an unboxed tuple?
isUnboxedTupleTyCon :: TyCon -> Bool
isUnboxedTupleTyCon (TupleTyCon {tyConBoxed = boxity}) = not (isBoxed boxity)
isUnboxedTupleTyCon _                                  = False

-- | Is this the 'TyCon' for a boxed tuple?
isBoxedTupleTyCon :: TyCon -> Bool
isBoxedTupleTyCon (TupleTyCon {tyConBoxed = boxity}) = isBoxed boxity
isBoxedTupleTyCon _                                  = False

-- | Extract the boxity of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConBoxity :: TyCon -> Boxity
tupleTyConBoxity tc = tyConBoxed tc

-- | Extract the arity of the given 'TyCon', if it is a 'TupleTyCon'.
-- Panics otherwise
tupleTyConArity :: TyCon -> Arity
tupleTyConArity tc = tyConArity tc

-- | Is this a recursive 'TyCon'?
isRecursiveTyCon :: TyCon -> Bool
isRecursiveTyCon (AlgTyCon {algTcRec = Recursive}) = True
isRecursiveTyCon _                                 = False

-- | Did this 'TyCon' originate from type-checking a .h*-boot file?
isHiBootTyCon :: TyCon -> Bool
-- Used for knot-tying in hi-boot files
isHiBootTyCon (AlgTyCon {algTcRhs = AbstractTyCon}) = True
isHiBootTyCon _                                     = False

-- | Is this the 'TyCon' of a foreign-imported type constructor?
isForeignTyCon :: TyCon -> Bool
isForeignTyCon (PrimTyCon {tyConExtName = Just _}) = True
isForeignTyCon _                                   = False

-- | Is this a super-kind 'TyCon'?
isSuperKindTyCon :: TyCon -> Bool
isSuperKindTyCon (SuperKindTyCon {}) = True
isSuperKindTyCon _                   = False

-- | Is this an AnyTyCon?
isAnyTyCon :: TyCon -> Bool
isAnyTyCon (AnyTyCon {}) = True
isAnyTyCon _              = False

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
isImplicitTyCon tycon | isTyConAssoc tycon           = True
		      | isSynTyCon tycon	     = False
		      | isAlgTyCon tycon	     = isClassTyCon tycon ||
						       isTupleTyCon tycon
isImplicitTyCon _other                               = True
        -- catches: FunTyCon, PrimTyCon, 
        -- CoTyCon, SuperKindTyCon
\end{code}


-----------------------------------------------
--	Expand type-constructor applications
-----------------------------------------------

\begin{code}
tcExpandTyCon_maybe, coreExpandTyCon_maybe 
	:: TyCon 
	-> [tyco]		  -- ^ Arguments to 'TyCon'
	-> Maybe ([(TyVar,tyco)], 	
		  Type,			
		  [tyco])	  -- ^ Returns a 'TyVar' substitution, the body type
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
expand	:: [TyVar] -> Type 		   -- Template
	-> [a]				   -- Args
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
tyConKind (FunTyCon   { tc_kind = k }) = k
tyConKind (AlgTyCon   { tc_kind = k }) = k
tyConKind (TupleTyCon { tc_kind = k }) = k
tyConKind (SynTyCon   { tc_kind = k }) = k
tyConKind (PrimTyCon  { tc_kind = k }) = k
tyConKind (AnyTyCon   { tc_kind = k }) = k
tyConKind tc = pprPanic "tyConKind" (ppr tc)	-- SuperKindTyCon and CoTyCon

tyConHasKind :: TyCon -> Bool
tyConHasKind (SuperKindTyCon {}) = False
tyConHasKind _                   = True

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
tyConDataCons_maybe (TupleTyCon {dataCon = con})	       		   = Just [con]
tyConDataCons_maybe _                                                      = Nothing

-- | Determine the number of value constructors a 'TyCon' has. Panics if the 'TyCon'
-- is not algebraic or a tuple
tyConFamilySize  :: TyCon -> Int
tyConFamilySize (AlgTyCon   {algTcRhs = DataTyCon {data_cons = cons}}) = 
  length cons
tyConFamilySize (AlgTyCon   {algTcRhs = NewTyCon {}})        = 1
tyConFamilySize (AlgTyCon   {algTcRhs = DataFamilyTyCon {}}) = 0
tyConFamilySize (TupleTyCon {})	 		             = 1
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
newTyConCo_maybe :: TyCon -> Maybe CoAxiom
newTyConCo_maybe (AlgTyCon {algTcRhs = NewTyCon { nt_co = co }}) = Just co
newTyConCo_maybe _					         = Nothing

newTyConCo :: TyCon -> CoAxiom
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
tyConStupidTheta (TupleTyCon {})			= []
tyConStupidTheta tycon = pprPanic "tyConStupidTheta" (ppr tycon)
\end{code}

\begin{code}
-- | Extract the 'TyVar's bound by a type synonym and the corresponding (unsubstituted) right hand side.
-- If the given 'TyCon' is not a type synonym, panics
synTyConDefn :: TyCon -> ([TyVar], Type)
synTyConDefn (SynTyCon {tyConTyVars = tyvars, synTcRhs = SynonymTyCon ty}) 
  = (tyvars, ty)
synTyConDefn tycon = pprPanic "getSynTyConDefn" (ppr tycon)

-- | Extract the information pertaining to the right hand side of a type synonym (@type@) declaration. Panics
-- if the given 'TyCon' is not a type synonym
synTyConRhs :: TyCon -> SynTyConRhs
synTyConRhs (SynTyCon {synTcRhs = rhs}) = rhs
synTyConRhs tc				= pprPanic "synTyConRhs" (ppr tc)

-- | Find the expansion of the type synonym represented by the given 'TyCon'. The free variables of this
-- type will typically include those 'TyVar's bound by the 'TyCon'. Panics if the 'TyCon' is not that of
-- a type synonym
synTyConType :: TyCon -> Type
synTyConType tc = case synTcRhs tc of
		    SynonymTyCon t -> t
		    _		   -> pprPanic "synTyConType" (ppr tc)
\end{code}

\begin{code}
-- | If the given 'TyCon' has a /single/ data constructor, i.e. it is a @data@ type with one
-- alternative, a tuple type or a @newtype@ then that constructor is returned. If the 'TyCon'
-- has more than one constructor, or represents a primitive or function type constructor then
-- @Nothing@ is returned. In any other case, the function panics
tyConSingleDataCon_maybe :: TyCon -> Maybe DataCon
tyConSingleDataCon_maybe (TupleTyCon {dataCon = c}) 			       = Just c
tyConSingleDataCon_maybe (AlgTyCon {algTcRhs = DataTyCon { data_cons = [c] }}) = Just c
tyConSingleDataCon_maybe (AlgTyCon {algTcRhs = NewTyCon { data_con = c }})     = Just c
tyConSingleDataCon_maybe _                           			       = Nothing
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

----------------------------------------------------------------------------
tyConParent :: TyCon -> TyConParent
tyConParent (AlgTyCon {algTcParent = parent}) = parent
tyConParent (SynTyCon {synTcParent = parent}) = parent
tyConParent _                                 = NoParentTyCon

----------------------------------------------------------------------------
-- | Is this 'TyCon' that for a family instance, be that for a synonym or an
-- algebraic family instance?
isFamInstTyCon :: TyCon -> Bool
isFamInstTyCon tc = case tyConParent tc of
                      FamInstTyCon {} -> True
                      _               -> False

tyConFamInstSig_maybe :: TyCon -> Maybe (TyCon, [Type], CoAxiom)
tyConFamInstSig_maybe tc
  = case tyConParent tc of
      FamInstTyCon f ts co_tc -> Just (f, ts, co_tc)
      _                       -> Nothing

-- | If this 'TyCon' is that of a family instance, return the family in question
-- and the instance types. Otherwise, return @Nothing@
tyConFamInst_maybe :: TyCon -> Maybe (TyCon, [Type])
tyConFamInst_maybe tc
  = case tyConParent tc of
      FamInstTyCon f ts _ -> Just (f, ts)
      _                   -> Nothing

-- | If this 'TyCon' is that of a family instance, return a 'TyCon' which represents 
-- a coercion identifying the representation type with the type instance family.
-- Otherwise, return @Nothing@
tyConFamilyCoercion_maybe :: TyCon -> Maybe CoAxiom
tyConFamilyCoercion_maybe tc
  = case tyConParent tc of
      FamInstTyCon _ _ co -> Just co
      _                   -> Nothing
\end{code}


%************************************************************************
%*									*
\subsection[TyCon-instances]{Instance declarations for @TyCon@}
%*									*
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
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = getUnique a `compare` getUnique b

instance Uniquable TyCon where
    getUnique tc = tyConUnique tc

instance Outputable TyCon where
    ppr tc  = ppr (getName tc) 

instance NamedThing TyCon where
    getName = tyConName

instance Data.Data TyCon where
    -- don't traverse?
    toConstr _   = abstractConstr "TyCon"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "TyCon"

-------------------
instance Eq CoAxiom where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }
  
instance Ord CoAxiom where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = getUnique a `compare` getUnique b  

instance Uniquable CoAxiom where
    getUnique = co_ax_unique

instance Outputable CoAxiom where
    ppr = ppr . getName

instance NamedThing CoAxiom where
    getName = co_ax_name

instance Data.Data CoAxiom where
    -- don't traverse?
    toConstr _   = abstractConstr "CoAxiom"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "CoAxiom"
\end{code}
