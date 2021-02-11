.. _deriving-via:

Deriving via
------------

.. extension:: DerivingVia
    :shortdesc: Enable deriving instances ``via`` types of the same runtime
        representation.
        Implies :extension:`DerivingStrategies`.

    :implies: :extension:`DerivingStrategies`

    :since: 8.6.1

This allows ``deriving`` a class instance for a type by specifying
another type of equal runtime representation (such that there exists a
``Coercible`` instance between the two: see :ref:`coercible`) that is
already an instance of the that class.

:extension:`DerivingVia` is indicated by the use of the ``via``
deriving strategy. ``via`` requires specifying another type (the ``via`` type)
to ``coerce`` through. For example, this code: ::

    {-# LANGUAGE DerivingVia #-}

    import Numeric

    newtype Hex a = Hex a

    instance (Integral a, Show a) => Show (Hex a) where
      show (Hex a) = "0x" ++ showHex a ""

    newtype Unicode = U Int
      deriving Show
        via (Hex Int)

    -- >>> euroSign
    -- 0x20ac
    euroSign :: Unicode
    euroSign = U 0x20ac

Generates the following instance ::

    instance Show Unicode where
      show :: Unicode -> String
      show = Data.Coerce.coerce
        @(Hex Int -> String)
        @(Unicode -> String)
        show

This extension generalizes :extension:`GeneralizedNewtypeDeriving`. To
derive ``Num Unicode`` with GND (``deriving newtype Num``) it must
reuse the ``Num Int`` instance. With ``DerivingVia``, we can explicitly
specify the representation type ``Int``: ::

    newtype Unicode = U Int
      deriving Num
        via Int

      deriving Show
        via (Hex Int)

    euroSign :: Unicode
    euroSign = 0x20ac

Code duplication is common in instance declarations. A familiar
pattern is lifting operations over an ``Applicative`` functor.
Instead of having catch-all instances for ``f a`` which overlap
with all other such instances, like so: ::

    instance (Applicative f, Semigroup a) => Semigroup (f a) ..
    instance (Applicative f, Monoid    a) => Monoid    (f a) ..

We can instead create a newtype ``App``
(where ``App f a`` and ``f a`` are represented the same in memory)
and use :extension:`DerivingVia` to explicitly enable uses of this
pattern: ::

    {-# LANGUAGE DerivingVia, DeriveFunctor, GeneralizedNewtypeDeriving #-}

    import Control.Applicative

    newtype App f a = App (f a) deriving newtype (Functor, Applicative)

    instance (Applicative f, Semigroup a) => Semigroup (App f a) where
      (<>) = liftA2 (<>)

    instance (Applicative f, Monoid a) => Monoid (App f a) where
      mempty = pure mempty

    data Pair a = MkPair a a
      deriving stock
        Functor

      deriving (Semigroup, Monoid)
        via (App Pair a)

    instance Applicative Pair where
      pure a = MkPair a a

      MkPair f g <*> MkPair a b = MkPair (f a) (g b)

Note that the ``via`` type does not have to be a ``newtype``.
The only restriction is that it is coercible with the
original data type. This means there can be arbitrary nesting of newtypes,
as in the following example: ::

    newtype Kleisli m a b = Kleisli (a -> m b)
      deriving (Semigroup, Monoid)
        via (a -> App m b)

Here we make use of the ``Monoid ((->) a)`` instance.

When used in combination with :extension:`StandaloneDeriving` we swap the order
for the instance we base our derivation on and the instance we define e.g.: ::

  deriving via (a -> App m b) instance Monoid (Kleisli m a b)



