{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics
-- Copyright   :  (c) Universiteit Utrecht 2010-2011, University of Oxford 2012-2014
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- @since 4.6.0.0
--
-- If you're using @GHC.Generics@, you should consider using the
-- <http://hackage.haskell.org/package/generic-deriving> package, which
-- contains many useful generic functions.

module GHC.Generics  (
-- * Introduction
--
-- |
--
-- Datatype-generic functions are based on the idea of converting values of
-- a datatype @T@ into corresponding values of a (nearly) isomorphic type @'Rep' T@.
-- The type @'Rep' T@ is
-- built from a limited set of type constructors, all provided by this module. A
-- datatype-generic function is then an overloaded function with instances
-- for most of these type constructors, together with a wrapper that performs
-- the mapping between @T@ and @'Rep' T@. By using this technique, we merely need
-- a few generic instances in order to implement functionality that works for any
-- representable type.
--
-- Representable types are collected in the 'Generic' class, which defines the
-- associated type 'Rep' as well as conversion functions 'from' and 'to'.
-- Typically, you will not define 'Generic' instances by hand, but have the compiler
-- derive them for you.

-- ** Representing datatypes
--
-- |
--
-- The key to defining your own datatype-generic functions is to understand how to
-- represent datatypes using the given set of type constructors.
--
-- Let us look at an example first:
--
-- @
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving 'Generic'
-- @
--
-- The above declaration (which requires the language pragma @DeriveGeneric@)
-- causes the following representation to be generated:
--
-- @
-- instance 'Generic' (Tree a) where
--   type 'Rep' (Tree a) =
--     'D1' ('MetaData \"Tree\" \"Main\" \"package-name\" 'False)
--       ('C1' ('MetaCons \"Leaf\" 'PrefixI 'False)
--          ('S1' '(MetaSel 'Nothing
--                           'NoSourceUnpackedness
--                           'NoSourceStrictness
--                           'DecidedLazy)
--                 ('Rec0' a))
--        ':+:'
--        'C1' ('MetaCons \"Node\" 'PrefixI 'False)
--          ('S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--                ('Rec0' (Tree a))
--           ':*:'
--           'S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--                ('Rec0' (Tree a))))
--   ...
-- @
--
-- /Hint:/ You can obtain information about the code being generated from GHC by passing
-- the @-ddump-deriv@ flag. In GHCi, you can expand a type family such as 'Rep' using
-- the @:kind!@ command.
--
-- This is a lot of information! However, most of it is actually merely meta-information
-- that makes names of datatypes and constructors and more available on the type level.
--
-- Here is a reduced representation for 'Tree' with nearly all meta-information removed,
-- for now keeping only the most essential aspects:
--
-- @
-- instance 'Generic' (Tree a) where
--   type 'Rep' (Tree a) =
--     'Rec0' a
--     ':+:'
--     ('Rec0' (Tree a) ':*:' 'Rec0' (Tree a))
-- @
--
-- The @Tree@ datatype has two constructors. The representation of individual constructors
-- is combined using the binary type constructor ':+:'.
--
-- The first constructor consists of a single field, which is the parameter @a@. This is
-- represented as @'Rec0' a@.
--
-- The second constructor consists of two fields. Each is a recursive field of type @Tree a@,
-- represented as @'Rec0' (Tree a)@. Representations of individual fields are combined using
-- the binary type constructor ':*:'.
--
-- Now let us explain the additional tags being used in the complete representation:
--
--    * The @'S1' ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness
--      'DecidedLazy)@ tag indicates several things. The @'Nothing@ indicates
--      that there is no record field selector associated with this field of
--      the constructor (if there were, it would have been marked @'Just
--      \"recordName\"@ instead). The other types contain meta-information on
--      the field's strictness:
--
--      * There is no @{\-\# UNPACK \#-\}@ or @{\-\# NOUNPACK \#-\}@ annotation
--        in the source, so it is tagged with @'NoSourceUnpackedness@.
--
--      * There is no strictness (@!@) or laziness (@~@) annotation in the
--        source, so it is tagged with @'NoSourceStrictness@.
--
--      * The compiler infers that the field is lazy, so it is tagged with
--        @'DecidedLazy@. Bear in mind that what the compiler decides may be
--        quite different from what is written in the source. See
--        'DecidedStrictness' for a more detailed explanation.
--
--      The @'MetaSel@ type is also an instance of the type class 'Selector',
--      which can be used to obtain information about the field at the value
--      level.
--
--    * The @'C1' ('MetaCons \"Leaf\" 'PrefixI 'False)@ and
--      @'C1' ('MetaCons \"Node\" 'PrefixI 'False)@ invocations indicate that the enclosed part is
--      the representation of the first and second constructor of datatype @Tree@, respectively.
--      Here, the meta-information regarding constructor names, fixity and whether
--      it has named fields or not is encoded at the type level. The @'MetaCons@
--      type is also an instance of the type class 'Constructor'. This type class can be used
--      to obtain information about the constructor at the value level.
--
--    * The @'D1' ('MetaData \"Tree\" \"Main\" \"package-name\" 'False)@ tag
--      indicates that the enclosed part is the representation of the
--      datatype @Tree@. Again, the meta-information is encoded at the type level.
--      The @'MetaData@ type is an instance of class 'Datatype', which
--      can be used to obtain the name of a datatype, the module it has been
--      defined in, the package it is located under, and whether it has been
--      defined using @data@ or @newtype@ at the value level.

-- ** Derived and fundamental representation types
--
-- |
--
-- There are many datatype-generic functions that do not distinguish between positions that
-- are parameters or positions that are recursive calls. There are also many datatype-generic
-- functions that do not care about the names of datatypes and constructors at all. To keep
-- the number of cases to consider in generic functions in such a situation to a minimum,
-- it turns out that many of the type constructors introduced above are actually synonyms,
-- defining them to be variants of a smaller set of constructors.

-- *** Individual fields of constructors: 'K1'
--
-- |
--
-- The type constructor 'Rec0' is a variant of 'K1':
--
-- @
-- type 'Rec0' = 'K1' 'R'
-- @
--
-- Here, 'R' is a type-level proxy that does not have any associated values.
--
-- There used to be another variant of 'K1' (namely 'Par0'), but it has since
-- been deprecated.

-- *** Meta information: 'M1'
--
-- |
--
-- The type constructors 'S1', 'C1' and 'D1' are all variants of 'M1':
--
-- @
-- type 'S1' = 'M1' 'S'
-- type 'C1' = 'M1' 'C'
-- type 'D1' = 'M1' 'D'
-- @
--
-- The types 'S', 'C' and 'D' are once again type-level proxies, just used to create
-- several variants of 'M1'.

-- *** Additional generic representation type constructors
--
-- |
--
-- Next to 'K1', 'M1', ':+:' and ':*:' there are a few more type constructors that occur
-- in the representations of other datatypes.

-- **** Empty datatypes: 'V1'
--
-- |
--
-- For empty datatypes, 'V1' is used as a representation. For example,
--
-- @
-- data Empty deriving 'Generic'
-- @
--
-- yields
--
-- @
-- instance 'Generic' Empty where
--   type 'Rep' Empty =
--     'D1' ('MetaData \"Empty\" \"Main\" \"package-name\" 'False) 'V1'
-- @

-- **** Constructors without fields: 'U1'
--
-- |
--
-- If a constructor has no arguments, then 'U1' is used as its representation. For example
-- the representation of 'Bool' is
--
-- @
-- instance 'Generic' Bool where
--   type 'Rep' Bool =
--     'D1' ('MetaData \"Bool\" \"Data.Bool\" \"package-name\" 'False)
--       ('C1' ('MetaCons \"False\" 'PrefixI 'False) 'U1' ':+:' 'C1' ('MetaCons \"True\" 'PrefixI 'False) 'U1')
-- @

-- *** Representation of types with many constructors or many fields
--
-- |
--
-- As ':+:' and ':*:' are just binary operators, one might ask what happens if the
-- datatype has more than two constructors, or a constructor with more than two
-- fields. The answer is simple: the operators are used several times, to combine
-- all the constructors and fields as needed. However, users /should not rely on
-- a specific nesting strategy/ for ':+:' and ':*:' being used. The compiler is
-- free to choose any nesting it prefers. (In practice, the current implementation
-- tries to produce a more or less balanced nesting, so that the traversal of the
-- structure of the datatype from the root to a particular component can be performed
-- in logarithmic rather than linear time.)

-- ** Defining datatype-generic functions
--
-- |
--
-- A datatype-generic function comprises two parts:
--
--    1. /Generic instances/ for the function, implementing it for most of the representation
--       type constructors introduced above.
--
--    2. A /wrapper/ that for any datatype that is in `Generic`, performs the conversion
--       between the original value and its `Rep`-based representation and then invokes the
--       generic instances.
--
-- As an example, let us look at a function 'encode' that produces a naive, but lossless
-- bit encoding of values of various datatypes. So we are aiming to define a function
--
-- @
-- encode :: 'Generic' a => a -> [Bool]
-- @
--
-- where we use 'Bool' as our datatype for bits.
--
-- For part 1, we define a class @Encode'@. Perhaps surprisingly, this class is parameterized
-- over a type constructor @f@ of kind @* -> *@. This is a technicality: all the representation
-- type constructors operate with kind @* -> *@ as base kind. But the type argument is never
-- being used. This may be changed at some point in the future. The class has a single method,
-- and we use the type we want our final function to have, but we replace the occurrences of
-- the generic type argument @a@ with @f p@ (where the @p@ is any argument; it will not be used).
--
-- > class Encode' f where
-- >   encode' :: f p -> [Bool]
--
-- With the goal in mind to make @encode@ work on @Tree@ and other datatypes, we now define
-- instances for the representation type constructors 'V1', 'U1', ':+:', ':*:', 'K1', and 'M1'.

-- *** Definition of the generic representation types
--
-- |
--
-- In order to be able to do this, we need to know the actual definitions of these types:
--
-- @
-- data    'V1'        p                       -- lifted version of Empty
-- data    'U1'        p = 'U1'                  -- lifted version of ()
-- data    (':+:') f g p = 'L1' (f p) | 'R1' (g p) -- lifted version of 'Either'
-- data    (':*:') f g p = (f p) ':*:' (g p)     -- lifted version of (,)
-- newtype 'K1'    i c p = 'K1' { 'unK1' :: c }    -- a container for a c
-- newtype 'M1'  i t f p = 'M1' { 'unM1' :: f p }  -- a wrapper
-- @
--
-- So, 'U1' is just the unit type, ':+:' is just a binary choice like 'Either',
-- ':*:' is a binary pair like the pair constructor @(,)@, and 'K1' is a value
-- of a specific type @c@, and 'M1' wraps a value of the generic type argument,
-- which in the lifted world is an @f p@ (where we do not care about @p@).

-- *** Generic instances
--
-- |
--
-- The instance for 'V1' is slightly awkward (but also rarely used):
--
-- @
-- instance Encode' 'V1' where
--   encode' x = undefined
-- @
--
-- There are no values of type @V1 p@ to pass (except undefined), so this is
-- actually impossible. One can ask why it is useful to define an instance for
-- 'V1' at all in this case? Well, an empty type can be used as an argument to
-- a non-empty type, and you might still want to encode the resulting type.
-- As a somewhat contrived example, consider @[Empty]@, which is not an empty
-- type, but contains just the empty list. The 'V1' instance ensures that we
-- can call the generic function on such types.
--
-- There is exactly one value of type 'U1', so encoding it requires no
-- knowledge, and we can use zero bits:
--
-- @
-- instance Encode' 'U1' where
--   encode' 'U1' = []
-- @
--
-- In the case for ':+:', we produce 'False' or 'True' depending on whether
-- the constructor of the value provided is located on the left or on the right:
--
-- @
-- instance (Encode' f, Encode' g) => Encode' (f ':+:' g) where
--   encode' ('L1' x) = False : encode' x
--   encode' ('R1' x) = True  : encode' x
-- @
--
-- In the case for ':*:', we append the encodings of the two subcomponents:
--
-- @
-- instance (Encode' f, Encode' g) => Encode' (f ':*:' g) where
--   encode' (x ':*:' y) = encode' x ++ encode' y
-- @
--
-- The case for 'K1' is rather interesting. Here, we call the final function
-- 'encode' that we yet have to define, recursively. We will use another type
-- class 'Encode' for that function:
--
-- @
-- instance (Encode c) => Encode' ('K1' i c) where
--   encode' ('K1' x) = encode x
-- @
--
-- Note how 'Par0' and 'Rec0' both being mapped to 'K1' allows us to define
-- a uniform instance here.
--
-- Similarly, we can define a uniform instance for 'M1', because we completely
-- disregard all meta-information:
--
-- @
-- instance (Encode' f) => Encode' ('M1' i t f) where
--   encode' ('M1' x) = encode' x
-- @
--
-- Unlike in 'K1', the instance for 'M1' refers to 'encode'', not 'encode'.

-- *** The wrapper and generic default
--
-- |
--
-- We now define class 'Encode' for the actual 'encode' function:
--
-- @
-- class Encode a where
--   encode :: a -> [Bool]
--   default encode :: (Generic a, Encode' (Rep a)) => a -> [Bool]
--   encode x = encode' ('from' x)
-- @
--
-- The incoming 'x' is converted using 'from', then we dispatch to the
-- generic instances using 'encode''. We use this as a default definition
-- for 'encode'. We need the 'default encode' signature because ordinary
-- Haskell default methods must not introduce additional class constraints,
-- but our generic default does.
--
-- Defining a particular instance is now as simple as saying
--
-- @
-- instance (Encode a) => Encode (Tree a)
-- @
--
#if 0
-- /TODO:/ Add usage example?
--
#endif
-- The generic default is being used. In the future, it will hopefully be
-- possible to use @deriving Encode@ as well, but GHC does not yet support
-- that syntax for this situation.
--
-- Having 'Encode' as a class has the advantage that we can define
-- non-generic special cases, which is particularly useful for abstract
-- datatypes that have no structural representation. For example, given
-- a suitable integer encoding function 'encodeInt', we can define
--
-- @
-- instance Encode Int where
--   encode = encodeInt
-- @

-- *** Omitting generic instances
--
-- |
--
-- It is not always required to provide instances for all the generic
-- representation types, but omitting instances restricts the set of
-- datatypes the functions will work for:
--
--    * If no ':+:' instance is given, the function may still work for
--      empty datatypes or datatypes that have a single constructor,
--      but will fail on datatypes with more than one constructor.
--
--    * If no ':*:' instance is given, the function may still work for
--      datatypes where each constructor has just zero or one field,
--      in particular for enumeration types.
--
--    * If no 'K1' instance is given, the function may still work for
--      enumeration types, where no constructor has any fields.
--
--    * If no 'V1' instance is given, the function may still work for
--      any datatype that is not empty.
--
--    * If no 'U1' instance is given, the function may still work for
--      any datatype where each constructor has at least one field.
--
-- An 'M1' instance is always required (but it can just ignore the
-- meta-information, as is the case for 'encode' above).
#if 0
-- *** Using meta-information
--
-- |
--
-- TODO
#endif
-- ** Generic constructor classes
--
-- |
--
-- Datatype-generic functions as defined above work for a large class
-- of datatypes, including parameterized datatypes. (We have used 'Tree'
-- as our example above, which is of kind @* -> *@.) However, the
-- 'Generic' class ranges over types of kind @*@, and therefore, the
-- resulting generic functions (such as 'encode') must be parameterized
-- by a generic type argument of kind @*@.
--
-- What if we want to define generic classes that range over type
-- constructors (such as 'Functor', 'Traversable', or 'Foldable')?

-- *** The 'Generic1' class
--
-- |
--
-- Like 'Generic', there is a class 'Generic1' that defines a
-- representation 'Rep1' and conversion functions 'from1' and 'to1',
-- only that 'Generic1' ranges over types of kind @* -> *@.
-- The 'Generic1' class is also derivable.
--
-- The representation 'Rep1' is ever so slightly different from 'Rep'.
-- Let us look at 'Tree' as an example again:
--
-- @
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--   deriving 'Generic1'
-- @
--
-- The above declaration causes the following representation to be generated:
--
-- @
-- instance 'Generic1' Tree where
--   type 'Rep1' Tree =
--     'D1' ('MetaData \"Tree\" \"Main\" \"package-name\" 'False)
--       ('C1' ('MetaCons \"Leaf\" 'PrefixI 'False)
--          ('S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--                'Par1')
--        ':+:'
--        'C1' ('MetaCons \"Node\" 'PrefixI 'False)
--          ('S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--                ('Rec1' Tree)
--           ':*:'
--           'S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--                ('Rec1' Tree)))
--   ...
--  @
--
-- The representation reuses 'D1', 'C1', 'S1' (and thereby 'M1') as well
-- as ':+:' and ':*:' from 'Rep'. (This reusability is the reason that we
-- carry around the dummy type argument for kind-@*@-types, but there are
-- already enough different names involved without duplicating each of
-- these.)
--
-- What's different is that we now use 'Par1' to refer to the parameter
-- (and that parameter, which used to be @a@), is not mentioned explicitly
-- by name anywhere; and we use 'Rec1' to refer to a recursive use of @Tree a@.

-- *** Representation of @* -> *@ types
--
-- |
--
-- Unlike 'Rec0', the 'Par1' and 'Rec1' type constructors do not
-- map to 'K1'. They are defined directly, as follows:
--
-- @
-- newtype 'Par1'   p = 'Par1' { 'unPar1' ::   p } -- gives access to parameter p
-- newtype 'Rec1' f p = 'Rec1' { 'unRec1' :: f p } -- a wrapper
-- @
--
-- In 'Par1', the parameter @p@ is used for the first time, whereas 'Rec1' simply
-- wraps an application of @f@ to @p@.
--
-- Note that 'K1' (in the guise of 'Rec0') can still occur in a 'Rep1' representation,
-- namely when the datatype has a field that does not mention the parameter.
--
-- The declaration
--
-- @
-- data WithInt a = WithInt Int a
--   deriving 'Generic1'
-- @
--
-- yields
--
-- @
-- class 'Rep1' WithInt where
--   type 'Rep1' WithInt =
--     'D1' ('MetaData \"WithInt\" \"Main\" \"package-name\" 'False)
--       ('C1' ('MetaCons \"WithInt\" 'PrefixI 'False)
--         ('S1' ('MetaSel 'Nothing
--                         'NoSourceUnpackedness
--                         'NoSourceStrictness
--                         'DecidedLazy)
--               ('Rec0' Int)
--          ':*:'
--          'S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--               'Par1'))
-- @
--
-- If the parameter @a@ appears underneath a composition of other type constructors,
-- then the representation involves composition, too:
--
-- @
-- data Rose a = Fork a [Rose a]
-- @
--
-- yields
--
-- @
-- class 'Rep1' Rose where
--   type 'Rep1' Rose =
--     'D1' ('MetaData \"Rose\" \"Main\" \"package-name\" 'False)
--       ('C1' ('MetaCons \"Fork\" 'PrefixI 'False)
--         ('S1' ('MetaSel 'Nothing
--                         'NoSourceUnpackedness
--                         'NoSourceStrictness
--                         'DecidedLazy)
--               'Par1'
--          ':*:'
--          'S1' ('MetaSel 'Nothing
--                          'NoSourceUnpackedness
--                          'NoSourceStrictness
--                          'DecidedLazy)
--               ([] ':.:' 'Rec1' Rose)))
-- @
--
-- where
--
-- @
-- newtype (':.:') f g p = 'Comp1' { 'unComp1' :: f (g p) }
-- @

-- *** Representation of unlifted types
--
-- |
--
-- If one were to attempt to derive a Generic instance for a datatype with an
-- unlifted argument (for example, 'Int#'), one might expect the occurrence of
-- the 'Int#' argument to be marked with @'Rec0' 'Int#'@. This won't work,
-- though, since 'Int#' is of kind @#@ and 'Rec0' expects a type of kind @*@.
-- In fact, polymorphism over unlifted types is disallowed completely.
--
-- One solution would be to represent an occurrence of 'Int#' with 'Rec0 Int'
-- instead. With this approach, however, the programmer has no way of knowing
-- whether the 'Int' is actually an 'Int#' in disguise.
--
-- Instead of reusing 'Rec0', a separate data family 'URec' is used to mark
-- occurrences of common unlifted types:
--
-- @
-- data family URec a p
--
-- data instance 'URec' ('Ptr' ()) p = 'UAddr'   { 'uAddr#'   :: 'Addr#'   }
-- data instance 'URec' 'Char'     p = 'UChar'   { 'uChar#'   :: 'Char#'   }
-- data instance 'URec' 'Double'   p = 'UDouble' { 'uDouble#' :: 'Double#' }
-- data instance 'URec' 'Int'      p = 'UFloat'  { 'uFloat#'  :: 'Float#'  }
-- data instance 'URec' 'Float'    p = 'UInt'    { 'uInt#'    :: 'Int#'    }
-- data instance 'URec' 'Word'     p = 'UWord'   { 'uWord#'   :: 'Word#'   }
-- @
--
-- Several type synonyms are provided for convenience:
--
-- @
-- type 'UAddr'   = 'URec' ('Ptr' ())
-- type 'UChar'   = 'URec' 'Char'
-- type 'UDouble' = 'URec' 'Double'
-- type 'UFloat'  = 'URec' 'Float'
-- type 'UInt'    = 'URec' 'Int'
-- type 'UWord'   = 'URec' 'Word'
-- @
--
-- The declaration
--
-- @
-- data IntHash = IntHash Int#
--   deriving 'Generic'
-- @
--
-- yields
--
-- @
-- instance 'Generic' IntHash where
--   type 'Rep' IntHash =
--     'D1' ('MetaData \"IntHash\" \"Main\" \"package-name\" 'False)
--       ('C1' ('MetaCons \"IntHash\" 'PrefixI 'False)
--         ('S1' ('MetaSel 'Nothing
--                         'NoSourceUnpackedness
--                         'NoSourceStrictness
--                         'DecidedLazy)
--               'UInt'))
-- @
--
-- Currently, only the six unlifted types listed above are generated, but this
-- may be extended to encompass more unlifted types in the future.
#if 0
-- *** Limitations
--
-- |
--
-- /TODO/
--
-- /TODO:/ Also clear up confusion about 'Rec0' and 'Rec1' not really indicating recursion.
--
#endif
-----------------------------------------------------------------------------

  -- * Generic representation types
    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  -- ** Unboxed representation types
  , URec(..)
  , type UAddr, type UChar, type UDouble
  , type UFloat, type UInt, type UWord

  -- ** Synonyms for convenience
  , Rec0, R
  , D1, C1, S1, D, C, S

  -- * Meta-information
  , Datatype(..), Constructor(..), Selector(..)
  , Fixity(..), FixityI(..), Associativity(..), prec
  , SourceUnpackedness(..), SourceStrictness(..), DecidedStrictness(..)
  , Meta(..)

  -- * Generic type classes
  , Generic(..), Generic1(..)

  ) where

-- We use some base types
import Data.Either ( Either (..) )
import Data.Maybe  ( Maybe(..), fromMaybe )
import GHC.Integer ( Integer, integerToInt )
import GHC.Prim    ( Addr#, Char#, Double#, Float#, Int#, Word# )
import GHC.Ptr     ( Ptr )
import GHC.Types

-- Needed for instances
import GHC.Arr     ( Ix )
import GHC.Base    ( Alternative(..), Applicative(..), Functor(..)
                   , Monad(..), MonadPlus(..), String )
import GHC.Classes ( Eq(..), Ord(..) )
import GHC.Enum    ( Bounded, Enum )
import GHC.Read    ( Read(..), lex, readParen )
import GHC.Show    ( Show(..), showString )

-- Needed for metadata
import Data.Proxy   ( Proxy(..), KProxy(..) )
import GHC.TypeLits ( Nat, Symbol, KnownSymbol, KnownNat, symbolVal, natVal )

--------------------------------------------------------------------------------
-- Representation types
--------------------------------------------------------------------------------

-- | Void: used for datatypes without constructors
data V1 (p :: *)
  deriving (Functor, Generic, Generic1)

deriving instance Eq   (V1 p)
deriving instance Ord  (V1 p)
deriving instance Read (V1 p)
deriving instance Show (V1 p)

-- | Unit: used for constructors without arguments
data U1 (p :: *) = U1
  deriving (Generic, Generic1)

instance Eq (U1 p) where
  _ == _ = True

instance Ord (U1 p) where
  compare _ _ = EQ

instance Read (U1 p) where
  readsPrec d = readParen (d > 10) (\r -> [(U1, s) | ("U1",s) <- lex r ])

instance Show (U1 p) where
  showsPrec _ _ = showString "U1"

instance Functor U1 where
  fmap _ _ = U1

instance Applicative U1 where
  pure _ = U1
  _ <*> _ = U1

instance Alternative U1 where
  empty = U1
  _ <|> _ = U1

instance Monad U1 where
  _ >>= _ = U1

instance MonadPlus U1

-- | Used for marking occurrences of the parameter
newtype Par1 p = Par1 { unPar1 :: p }
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance Applicative Par1 where
  pure a = Par1 a
  Par1 f <*> Par1 x = Par1 (f x)

instance Monad Par1 where
  Par1 x >>= f = f x

-- | Recursive calls of kind * -> *
newtype Rec1 f (p :: *) = Rec1 { unRec1 :: f p }
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance Applicative f => Applicative (Rec1 f) where
  pure a = Rec1 (pure a)
  Rec1 f <*> Rec1 x = Rec1 (f <*> x)

instance Alternative f => Alternative (Rec1 f) where
  empty = Rec1 empty
  Rec1 l <|> Rec1 r = Rec1 (l <|> r)

instance Monad f => Monad (Rec1 f) where
  Rec1 x >>= f = Rec1 (x >>= \a -> unRec1 (f a))

instance MonadPlus f => MonadPlus (Rec1 f)

-- | Constants, additional parameters and recursion of kind *
newtype K1 (i :: *) c (p :: *) = K1 { unK1 :: c }
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance Applicative f => Applicative (M1 i c f) where
  pure a = M1 (pure a)
  M1 f <*> M1 x = M1 (f <*> x)

instance Alternative f => Alternative (M1 i c f) where
  empty = M1 empty
  M1 l <|> M1 r = M1 (l <|> r)

instance Monad f => Monad (M1 i c f) where
  M1 x >>= f = M1 (x >>= \a -> unM1 (f a))

instance MonadPlus f => MonadPlus (M1 i c f)

-- | Meta-information (constructor names, etc.)
newtype M1 (i :: *) (c :: Meta) f (p :: *) = M1 { unM1 :: f p }
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g (p :: *) = L1 (f p) | R1 (g p)
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g (p :: *) = f p :*: g p
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure a = pure a :*: pure a
  (f :*: g) <*> (x :*: y) = (f <*> x) :*: (g <*> y)

instance (Alternative f, Alternative g) => Alternative (f :*: g) where
  empty = empty :*: empty
  (x1 :*: y1) <|> (x2 :*: y2) = (x1 <|> x2) :*: (y1 <|> y2)

instance (Monad f, Monad g) => Monad (f :*: g) where
  (m :*: n) >>= f = (m >>= \a -> fstP (f a)) :*: (n >>= \a -> sndP (f a))
    where
      fstP (a :*: _) = a
      sndP (_ :*: b) = b

instance (MonadPlus f, MonadPlus g) => MonadPlus (f :*: g)

-- | Composition of functors
infixr 7 :.:
newtype (:.:) f (g :: * -> *) (p :: *) = Comp1 { unComp1 :: f (g p) }
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x = Comp1 (pure (pure x))
  Comp1 f <*> Comp1 x = Comp1 (fmap (<*>) f <*> x)

instance (Alternative f, Applicative g) => Alternative (f :.: g) where
  empty = Comp1 empty
  Comp1 x <|> Comp1 y = Comp1 (x <|> y)

-- | Constants of kind @#@
data family URec (a :: *) (p :: *)

-- | Used for marking occurrences of 'Addr#'
data instance URec (Ptr ()) p = UAddr { uAddr# :: Addr# }
  deriving (Eq, Ord, Functor, Generic, Generic1)

-- | Used for marking occurrences of 'Char#'
data instance URec Char p = UChar { uChar# :: Char# }
  deriving (Eq, Ord, Show, Functor, Generic, Generic1)

-- | Used for marking occurrences of 'Double#'
data instance URec Double p = UDouble { uDouble# :: Double# }
  deriving (Eq, Ord, Show, Functor, Generic, Generic1)

-- | Used for marking occurrences of 'Float#'
data instance URec Float p = UFloat { uFloat# :: Float# }
  deriving (Eq, Ord, Show, Functor, Generic, Generic1)

-- | Used for marking occurrences of 'Int#'
data instance URec Int p = UInt { uInt# :: Int# }
  deriving (Eq, Ord, Show, Functor, Generic, Generic1)

-- | Used for marking occurrences of 'Word#'
data instance URec Word p = UWord { uWord# :: Word# }
  deriving (Eq, Ord, Show, Functor, Generic, Generic1)

-- | Type synonym for 'URec': 'Addr#'
type UAddr   = URec (Ptr ())
-- | Type synonym for 'URec': 'Char#'
type UChar   = URec Char
-- | Type synonym for 'URec': 'Double#'
type UDouble = URec Double
-- | Type synonym for 'URec': 'Float#'
type UFloat  = URec Float
-- | Type synonym for 'URec': 'Int#'
type UInt    = URec Int
-- | Type synonym for 'URec': 'Word#'
type UWord   = URec Word

-- | Tag for K1: recursion (of kind *)
data R

-- | Type synonym for encoding recursion (of kind *)
type Rec0  = K1 R

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C
-- | Tag for M1: record selector
data S

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C

-- | Type synonym for encoding meta-information for record selectors
type S1 = M1 S

-- | Class for datatypes that represent datatypes
class Datatype d where
  -- | The name of the datatype (unqualified)
  datatypeName :: t d (f :: * -> *) a -> [Char]
  -- | The fully-qualified name of the module where the type is declared
  moduleName   :: t d (f :: * -> *) a -> [Char]
  -- | The package name of the module where the type is declared
  packageName :: t d (f :: * -> *) a -> [Char]
  -- | Marks if the datatype is actually a newtype
  isNewtype    :: t d (f :: * -> *) a -> Bool
  isNewtype _ = False

instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, SingI nt)
    => Datatype ('MetaData n m p nt) where
  datatypeName _ = symbolVal (Proxy :: Proxy n)
  moduleName   _ = symbolVal (Proxy :: Proxy m)
  packageName  _ = symbolVal (Proxy :: Proxy p)
  isNewtype    _ = fromSing  (sing  :: Sing nt)

-- | Class for datatypes that represent data constructors
class Constructor c where
  -- | The name of the constructor
  conName :: t c (f :: * -> *) a -> [Char]

  -- | The fixity of the constructor
  conFixity :: t c (f :: * -> *) a -> Fixity
  conFixity _ = Prefix

  -- | Marks if this constructor is a record
  conIsRecord :: t c (f :: * -> *) a -> Bool
  conIsRecord _ = False

instance (KnownSymbol n, SingI f, SingI r)
    => Constructor ('MetaCons n f r) where
  conName     _ = symbolVal (Proxy :: Proxy n)
  conFixity   _ = fromSing  (sing  :: Sing f)
  conIsRecord _ = fromSing  (sing  :: Sing r)

-- | Datatype to represent the fixity of a constructor. An infix
-- | declaration directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read, Generic)

-- | This variant of 'Fixity' appears at the type level.
data FixityI = PrefixI | InfixI Associativity Nat

-- | Get the precedence of a fixity value.
prec :: Fixity -> Int
prec Prefix      = 10
prec (Infix _ n) = n

-- | Datatype to represent the associativity of a constructor
data Associativity = LeftAssociative
                   | RightAssociative
                   | NotAssociative
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix, Generic)

-- | The unpackedness of a field as the user wrote it in the source code. For
-- example, in the following data type:
--
-- @
-- data E = ExampleConstructor     Int
--            {\-\# NOUNPACK \#-\} Int
--            {\-\#   UNPACK \#-\} Int
-- @
--
-- The fields of @ExampleConstructor@ have 'NoSourceUnpackedness',
-- 'SourceNoUnpack', and 'SourceUnpack', respectively.
data SourceUnpackedness = NoSourceUnpackedness
                        | SourceNoUnpack
                        | SourceUnpack
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix, Generic)

-- | The strictness of a field as the user wrote it in the source code. For
-- example, in the following data type:
--
-- @
-- data E = ExampleConstructor Int ~Int !Int
-- @
--
-- The fields of @ExampleConstructor@ have 'NoSourceStrictness',
-- 'SourceLazy', and 'SourceStrict', respectively.
data SourceStrictness = NoSourceStrictness
                      | SourceLazy
                      | SourceStrict
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix, Generic)

-- | The strictness that GHC infers for a field during compilation. Whereas
-- there are nine different combinations of 'SourceUnpackedness' and
-- 'SourceStrictness', the strictness that GHC decides will ultimately be one
-- of lazy, strict, or unpacked. What GHC decides is affected both by what the
-- user writes in the source code and by GHC flags. As an example, consider
-- this data type:
--
-- @
-- data E = ExampleConstructor {\-\# UNPACK \#-\} !Int !Int Int
-- @
--
-- * If compiled without optimization or other language extensions, then the
--   fields of @ExampleConstructor@ will have 'DecidedStrict', 'DecidedStrict',
--   and 'DecidedLazy', respectively.
--
-- * If compiled with @-XStrictData@ enabled, then the fields will have
--   'DecidedStrict', 'DecidedStrict', and 'DecidedStrict', respectively.
--
-- * If compiled with @-O2@ enabled, then the fields will have 'DecidedUnpack',
--   'DecidedStrict', and 'DecidedLazy', respectively.
data DecidedStrictness = DecidedLazy
                       | DecidedStrict
                       | DecidedUnpack
  deriving (Eq, Show, Ord, Read, Enum, Bounded, Ix, Generic)

-- | Class for datatypes that represent records
class Selector s where
  -- | The name of the selector
  selName :: t s (f :: * -> *) a -> [Char]
  -- | The selector's unpackedness annotation (if any)
  selSourceUnpackedness :: t s (f :: * -> *) a -> SourceUnpackedness
  -- | The selector's strictness annotation (if any)
  selSourceStrictness :: t s (f :: * -> *) a -> SourceStrictness
  -- | The strictness that the compiler inferred for the selector
  selDecidedStrictness :: t s (f :: * -> *) a -> DecidedStrictness

instance (SingI mn, SingI su, SingI ss, SingI ds)
    => Selector ('MetaSel mn su ss ds) where
  selName _ = fromMaybe "" (fromSing (sing :: Sing mn))
  selSourceUnpackedness _ = fromSing (sing :: Sing su)
  selSourceStrictness   _ = fromSing (sing :: Sing ss)
  selDecidedStrictness  _ = fromSing (sing :: Sing ds)

-- | Representable types of kind *.
-- This class is derivable in GHC with the DeriveGeneric flag on.
class Generic a where
  -- | Generic representation type
  type Rep a :: * -> *
  -- | Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- | Convert from the representation to the datatype
  to    :: (Rep a) x -> a


-- | Representable types of kind * -> *.
-- This class is derivable in GHC with the DeriveGeneric flag on.
class Generic1 f where
  -- | Generic representation type
  type Rep1 f :: * -> *
  -- | Convert from the datatype to its representation
  from1  :: f a -> (Rep1 f) a
  -- | Convert from the representation to the datatype
  to1    :: (Rep1 f) a -> f a

--------------------------------------------------------------------------------
-- Meta-data
--------------------------------------------------------------------------------

-- | Datatype to represent metadata associated with a datatype (@MetaData@),
-- constructor (@MetaCons@), or field selector (@MetaSel@).
--
-- * In @MetaData n m p nt@, @n@ is the datatype's name, @m@ is the module in
--   which the datatype is defined, @p@ is the package in which the datatype
--   is defined, and @nt@ is @'True@ if the datatype is a @newtype@.
--
-- * In @MetaCons n f s@, @n@ is the constructor's name, @f@ is its fixity,
--   and @s@ is @'True@ if the constructor contains record selectors.
--
-- * In @MetaSel mn su ss ds@, if the field is uses record syntax, then @mn@ is
--   'Just' the record name. Otherwise, @mn@ is 'Nothing. @su@ and @ss@ are the
--   field's unpackedness and strictness annotations, and @ds@ is the
--   strictness that GHC infers for the field.
data Meta = MetaData Symbol Symbol Symbol Bool
          | MetaCons Symbol FixityI Bool
          | MetaSel  (Maybe Symbol)
                     SourceUnpackedness SourceStrictness DecidedStrictness

--------------------------------------------------------------------------------
-- Derived instances
--------------------------------------------------------------------------------

deriving instance Generic [a]
deriving instance Generic (Maybe a)
deriving instance Generic (Either a b)
deriving instance Generic Bool
deriving instance Generic Ordering
deriving instance Generic (Proxy t)
deriving instance Generic ()
deriving instance Generic ((,) a b)
deriving instance Generic ((,,) a b c)
deriving instance Generic ((,,,) a b c d)
deriving instance Generic ((,,,,) a b c d e)
deriving instance Generic ((,,,,,) a b c d e f)
deriving instance Generic ((,,,,,,) a b c d e f g)

deriving instance Generic1 []
deriving instance Generic1 Maybe
deriving instance Generic1 (Either a)
deriving instance Generic1 Proxy
deriving instance Generic1 ((,) a)
deriving instance Generic1 ((,,) a b)
deriving instance Generic1 ((,,,) a b c)
deriving instance Generic1 ((,,,,) a b c d)
deriving instance Generic1 ((,,,,,) a b c d e)
deriving instance Generic1 ((,,,,,,) a b c d e f)

--------------------------------------------------------------------------------
-- Copied from the singletons package
--------------------------------------------------------------------------------

-- | The singleton kind-indexed data family.
data family Sing (a :: k)

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI'.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

-- | The 'SingKind' class is essentially a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  -- | Get a base type from a proxy for the promoted kind. For example,
  -- @DemoteRep ('KProxy :: KProxy Bool)@ will be the type @Bool@.
  type DemoteRep kparam :: *

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> DemoteRep kparam

-- Singleton symbols
data instance Sing (s :: Symbol) where
  SSym :: KnownSymbol s => Sing s

instance KnownSymbol a => SingI a where sing = SSym

instance SingKind ('KProxy :: KProxy Symbol) where
  type DemoteRep ('KProxy :: KProxy Symbol) = String
  fromSing (SSym :: Sing s) = symbolVal (Proxy :: Proxy s)

-- Singleton booleans
data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

instance SingI 'True  where sing = STrue
instance SingI 'False where sing = SFalse

instance SingKind ('KProxy :: KProxy Bool) where
  type DemoteRep ('KProxy :: KProxy Bool) = Bool
  fromSing STrue  = True
  fromSing SFalse = False

-- Singleton Maybe
data instance Sing (b :: Maybe a) where
  SNothing :: Sing 'Nothing
  SJust    :: Sing a -> Sing ('Just a)

instance            SingI 'Nothing  where sing = SNothing
instance SingI a => SingI ('Just a) where sing = SJust sing

instance SingKind ('KProxy :: KProxy a) =>
    SingKind ('KProxy :: KProxy (Maybe a)) where
  type DemoteRep ('KProxy :: KProxy (Maybe a)) =
      Maybe (DemoteRep ('KProxy :: KProxy a))
  fromSing SNothing  = Nothing
  fromSing (SJust a) = Just (fromSing a)

-- Singleton Fixity
data instance Sing (a :: FixityI) where
  SPrefix :: Sing 'PrefixI
  SInfix  :: Sing a -> Integer -> Sing ('InfixI a n)

instance SingI 'PrefixI where sing = SPrefix
instance (SingI a, KnownNat n) => SingI ('InfixI a n) where
  sing = SInfix (sing :: Sing a) (natVal (Proxy :: Proxy n))

instance SingKind ('KProxy :: KProxy FixityI) where
  type DemoteRep ('KProxy :: KProxy FixityI) = Fixity
  fromSing SPrefix      = Prefix
  fromSing (SInfix a n) = Infix (fromSing a) (I# (integerToInt n))

-- Singleton Associativity
data instance Sing (a :: Associativity) where
  SLeftAssociative  :: Sing 'LeftAssociative
  SRightAssociative :: Sing 'RightAssociative
  SNotAssociative   :: Sing 'NotAssociative

instance SingI 'LeftAssociative  where sing = SLeftAssociative
instance SingI 'RightAssociative where sing = SRightAssociative
instance SingI 'NotAssociative   where sing = SNotAssociative

instance SingKind ('KProxy :: KProxy Associativity) where
  type DemoteRep ('KProxy :: KProxy Associativity) = Associativity
  fromSing SLeftAssociative  = LeftAssociative
  fromSing SRightAssociative = RightAssociative
  fromSing SNotAssociative   = NotAssociative

-- Singleton SourceUnpackedness
data instance Sing (a :: SourceUnpackedness) where
  SNoSourceUnpackedness :: Sing 'NoSourceUnpackedness
  SSourceNoUnpack       :: Sing 'SourceNoUnpack
  SSourceUnpack         :: Sing 'SourceUnpack

instance SingI 'NoSourceUnpackedness where sing = SNoSourceUnpackedness
instance SingI 'SourceNoUnpack       where sing = SSourceNoUnpack
instance SingI 'SourceUnpack         where sing = SSourceUnpack

instance SingKind ('KProxy :: KProxy SourceUnpackedness) where
  type DemoteRep ('KProxy :: KProxy SourceUnpackedness) = SourceUnpackedness
  fromSing SNoSourceUnpackedness = NoSourceUnpackedness
  fromSing SSourceNoUnpack       = SourceNoUnpack
  fromSing SSourceUnpack         = SourceUnpack

-- Singleton SourceStrictness
data instance Sing (a :: SourceStrictness) where
  SNoSourceStrictness :: Sing 'NoSourceStrictness
  SSourceLazy         :: Sing 'SourceLazy
  SSourceStrict       :: Sing 'SourceStrict

instance SingI 'NoSourceStrictness where sing = SNoSourceStrictness
instance SingI 'SourceLazy         where sing = SSourceLazy
instance SingI 'SourceStrict       where sing = SSourceStrict

instance SingKind ('KProxy :: KProxy SourceStrictness) where
  type DemoteRep ('KProxy :: KProxy SourceStrictness) = SourceStrictness
  fromSing SNoSourceStrictness = NoSourceStrictness
  fromSing SSourceLazy         = SourceLazy
  fromSing SSourceStrict       = SourceStrict

-- Singleton DecidedStrictness
data instance Sing (a :: DecidedStrictness) where
  SDecidedLazy   :: Sing 'DecidedLazy
  SDecidedStrict :: Sing 'DecidedStrict
  SDecidedUnpack :: Sing 'DecidedUnpack

instance SingI 'DecidedLazy   where sing = SDecidedLazy
instance SingI 'DecidedStrict where sing = SDecidedStrict
instance SingI 'DecidedUnpack where sing = SDecidedUnpack

instance SingKind ('KProxy :: KProxy DecidedStrictness) where
  type DemoteRep ('KProxy :: KProxy DecidedStrictness) = DecidedStrictness
  fromSing SDecidedLazy   = DecidedLazy
  fromSing SDecidedStrict = DecidedStrict
  fromSing SDecidedUnpack = DecidedUnpack
