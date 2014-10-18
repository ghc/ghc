{-# LANGUAGE MultiParamTypeClasses, KindSignatures, DataKinds,
             TypeFamilies, RankNTypes, FlexibleInstances, FlexibleContexts,
             NoImplicitPrelude, EmptyDataDecls, MagicHash, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Records
-- Copyright   :  (c) Adam Gundry, 2013-2014
-- License     :  BSD-style (see libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This is an internal GHC module that defines classes relating to the
-- OverloadedRecordFields extension.  For notes on the implementation
-- of OverloadedRecordFields, see
-- https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Implementation
-----------------------------------------------------------------------------

{-
Note [Dependency on GHC.Records]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module must be compiled before any module that declares a record
field, because the class declarations below are loaded in order to
generate the supporting definitions for overloaded record fields. To
achieve this, this module is imported by GHC.Base. If you receive the
error "Failed to load interface for ‛GHC.Records’" while compiling
base, this module has not been compiled early enough.
-}

module GHC.Records where

import GHC.Integer ()
import GHC.Prim (Proxy#)

-- | (Kind) This is the kind of type-level symbols.
data Symbol


{-
The OverloadedRecordFields extension generates instances for the
following type classes ('Has' and 'Upd') and type families
('FldTy' and 'UpdTy'). For example, the datatype

    data T a = MkT { foo :: [a] }

gives rise to the instances

    type instance FldTy (T a) "foo"     = [a]
    type instance UpdTy (T a) "foo" [c] = T c
    instance b ~ [a] => Has (T a) "foo" b
    instance b ~ [c] => Upd (T a) "foo" b

See compiler/typecheck/TcFldInsts.lhs for the code that generates
these instances.  The instances are generated for every datatype,
regardless of whether the extension is enabled, but they are not
exported using the normal mechanism, because the instances in scope
correspond exactly to the record fields in scope.  See
Note [Instance scoping for OverloadedRecordFields] in TcFldInsts.
-}


-- | @FldTy r n@ is the type of the field @n@ in record type @r@.
type family FldTy (r :: *) (n :: Symbol) :: *
-- See Note [Why not associated types]

-- | @UpdTy r n t@ is the record type that results from setting
-- the field @n@ of record type @r@ to @t@.
type family UpdTy (r :: *) (n :: Symbol) (t :: *) :: *

-- | @Has r n t@ means that @r@ is a record type with field @n@ of type @t@.
class t ~ FldTy r n  -- See Note [Functional dependency via equality superclass]
          => Has r (n :: Symbol) t where
  -- | Polymorphic field selector
  getField :: Proxy# n -> r -> t

-- | @Upd r n t@ means that @r@ is a record type with field @n@ which
-- can be assigned type @t@.
class (Has r n (FldTy r n), r ~ UpdTy r n (FldTy r n))
              -- See Note [Superclasses of Upd]
          => Upd (r :: *) (n :: Symbol) (t :: *) where
  -- | Polymorphic field update
  setField :: Proxy# n -> r -> t -> UpdTy r n t


{-
Note [Functional dependency via equality superclass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The third parameter of the 'Has' class (the field type) is
functionally dependent on the first two (the record type and field
name), but is present to allow for syntactic sugar:

    r { f :: t }    translates to    Has r "f" t

The functional dependency is encoded using the 'FldTy' type
family, via the equality superclass 't ~ FldTy r n' in the
declaration of 'Has'. Thanks to this superclass, if we have a
constraint

    [Wanted] Has (T alpha) "foo" beta

then we get

    [Derived] beta ~ FldTy (T alpha) "foo".

Now substituting for 'beta' in the wanted constraint and reducing
'FldTy' gives

    [Wanted] Has (T alpha) "foo" [alpha].

This constraint could be solved via

    instance Has (T a) "foo" [a].

However, if the field type involved a type family, for example

    type family F x
    data U a = MkU { foo :: F a }

then we would end up with

    [Wanted] Has (U alpha) "foo" (F alpha)

which does not obviously match

    instance Has (U a) "foo" (F a).

Thus we always generate an instance like

    instance b ~ F a => Has (U a) "foo" b

that matches only on the first two parameters.


In any case, the third parameter of 'Upd' is not functionally
dependent on the first two, because it represents the new type being
assigned to the field, not its current type. Thus we must generate

    instance b ~ [c] => Upd (T a) "foo" b

to ensure that a constraint like

    [Wanted] Upd (T alpha) "foo" beta

will be solved.


Note [Why not associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'FldTy' could be an associated type, but 'UpdTy' cannot, so
for consistency both are separate top-level type families.  The
parameters of associated types must be exactly the same as the class
header (they cannot be more specific instances), so this is currently
illegal:

    instance t ~ [b] => Upd (T a) "foo" t where
        type UpdTy (T a) "foo" [b] = T b

If this were allowed, both type families could become associated
types. See Trac #8161. The difference is minimal, however.


Note [Superclasses of Upd]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclasses of 'Upd' ensure that there is always a corresponding
'Has' instance, and that the invariant

    r ~ UpdTy r n (FldTy r n)

always holds. This says that setting a field without changing its type
does not change the type of the record. It is included so that

    [Given] Upd r n (FldTy r n)

implies

    setField :: Proxy# n -> r -> FldTy r n -> r

which may make it easier to write some very polymorphic code to update
fields. If you can think of a concrete example of why this is useful,
please add it here!
-}


-- | @Accessor p r n t@ means that @p@ is a type into which a field
-- with name @n@ having type @t@ in record @r@ can be translated.  The
-- canonical instance is for the function space (->), which just
-- returns the getter (completely ignoring the setter).  Lens
-- libraries may give instances of 'Accessor' so that overloaded
-- fields can be used as lenses.
class Accessor (p :: * -> * -> *) (r :: *) (n :: Symbol) (t :: *) where
  -- | @accessField z getter setter@ injects a getter and setter pair into @p@
  accessField :: Proxy# n ->
                 (Has r n t => r -> t) ->
                 (forall t' . Upd r n t' => r -> t' -> UpdTy r n t') ->
                 p r t

instance Has r n t => Accessor (->) r n t where
  accessField _ getter _ = getter


{-
When the OverloadedRecordFields extension is enabled, a field @foo@ in
an expression is translated into

    field (proxy# :: Proxy# "foo") :: Accessor p r "foo" t => p r t
-}

-- | Target for translation of overloaded record field occurrences
field :: forall p r n t . Accessor p r n t => Proxy# n -> p r t
field z = accessField z (getField z) (setField z)


{-
Note [On the multiplicity of parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One might me tempted to remove the third redundant parameter of the
'Has' class, since it is always determined by the first two.
Similarly, the 'Accessor' class can be defined using only the 'p' and
'n' parameters.  However, apart from the three-parameter version of
'Has' naturally supporting the syntactic sugar, this approach leads to
better error messages for misues of fields.  For example, we get

    No instance for (Int -> Int) {x :: Bool}
      arising from a use of the record selector ‛x’
    The type ‛(->)’ does not have a field ‛x’

instead of

    Couldn't match type ‛GHC.Records.FldTy (Int -> Int) "x"’
                  with ‛Bool’
    Expected type: (Int -> Int) -> Bool
      Actual type: (Int -> Int) -> GHC.Records.FldTy (Int -> Int) "x"

Crucially, the type of 'field', into which overloaded fields are
translated, does not mention the 'FldTy' type family.  Thus we get an
error from failing to find the necessary 'Has' instance instead of
failing to expand 'FldTy'.

This also means that the type of an overloaded field 'foo' is

    GHC.Records.Accessor t t1 "foo" t2 => t t1 t2

rather than

     GHC.Records.Accessor t t1 "foo" => t t1 (GHC.Records.FldTy t1 "foo")
-}
