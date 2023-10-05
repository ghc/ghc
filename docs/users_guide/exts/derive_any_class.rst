.. _derive-any-class:

Deriving any other class
------------------------

.. extension:: DeriveAnyClass
    :shortdesc: Enable deriving for any class.

    :since: 7.10.1

    Allow use of any typeclass in ``deriving`` clauses.

With :extension:`DeriveAnyClass` you can derive any other class. The compiler
will simply generate an instance declaration with no explicitly-defined
methods.
This is
mostly useful in classes whose `minimal set <#minimal-pragma>`__ is
empty, and especially when writing
`generic functions <#generic-programming>`__.

As an example, consider a simple pretty-printer class ``SPretty``, which outputs
pretty strings: ::

    {-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}

    class SPretty a where
      sPpr :: a -> String
      default sPpr :: Show a => a -> String
      sPpr = show

If a user does not provide a manual implementation for ``sPpr``, then it will
default to ``show``. Now we can leverage the :extension:`DeriveAnyClass` extension to
easily implement a ``SPretty`` instance for a new data type: ::

    data Foo = Foo deriving (Show, SPretty)

The above code is equivalent to: ::

    data Foo = Foo deriving Show
    instance SPretty Foo

That is, an ``SPretty Foo`` instance will be created with empty implementations
for all methods. Since we are using :extension:`DefaultSignatures` in this example, a
default implementation of ``sPpr`` is filled in automatically.

Note the following details

- In case you try to derive some
  class on a newtype, and :extension:`GeneralizedNewtypeDeriving` is also on,
  :extension:`DeriveAnyClass` takes precedence.

- The instance context is determined by the type signatures of the derived
  class's methods. For instance, if the class is: ::

    class Foo a where
      bar :: a -> String
      default bar :: Show a => a -> String
      bar = show

      baz :: a -> a -> Bool
      default baz :: Ord a => a -> a -> Bool
      baz x y = compare x y == EQ

  And you attempt to derive it using :extension:`DeriveAnyClass`: ::

    instance Eq   a => Eq   (Option a) where ...
    instance Ord  a => Ord  (Option a) where ...
    instance Show a => Show (Option a) where ...

    data Option a = None | Some a deriving Foo

  Then the derived ``Foo`` instance will be: ::

    instance (Show a, Ord a) => Foo (Option a)

  Since the default type signatures for ``bar`` and ``baz`` require ``Show a``
  and ``Ord a`` constraints, respectively.

  Constraints on the non-default type signatures can play a role in inferring
  the instance context as well. For example, if you have this class: ::

    class HigherEq f where
      (==#) :: f a -> f a -> Bool
      default (==#) :: Eq (f a) => f a -> f a -> Bool
      x ==# y = (x == y)

  And you tried to derive an instance for it: ::

    instance Eq a => Eq (Option a) where ...
    data Option a = None | Some a deriving HigherEq

  Then it will fail with an error to the effect of: ::

    No instance for (Eq a)
        arising from the 'deriving' clause of a data type declaration

  That is because we require an ``Eq (Option a)`` instance from the default
  type signature for ``(==#)``, which in turn requires an ``Eq a`` instance,
  which we don't have in scope. But if you tweak the definition of
  ``HigherEq`` slightly: ::

    class HigherEq f where
      (==#) :: Eq a => f a -> f a -> Bool
      default (==#) :: Eq (f a) => f a -> f a -> Bool
      x ==# y = (x == y)

  Then it becomes possible to derive a ``HigherEq Option`` instance. Note that
  the only difference is that now the non-default type signature for ``(==#)``
  brings in an ``Eq a`` constraint. Constraints from non-default type
  signatures never appear in the derived instance context itself, but they can
  be used to discharge obligations that are demanded by the default type
  signatures. In the example above, the default type signature demanded an
  ``Eq a`` instance, and the non-default signature was able to satisfy that
  request, so the derived instance is simply: ::

    instance HigherEq Option

- :extension:`DeriveAnyClass` can be used with partially applied classes,
  such as ::

    data T a = MKT a deriving( D Int )

  which generates ::

    instance D Int a => D Int (T a) where {}

- :extension:`DeriveAnyClass` can be used to fill in default instances for
  associated type families: ::

    {-# LANGUAGE DeriveAnyClass, TypeFamilies #-}

    class Sizable a where
      type Size a
      type Size a = Int

    data Bar = Bar deriving Sizable

    doubleBarSize :: Size Bar -> Size Bar
    doubleBarSize s = 2*s

  The ``deriving( Sizable )`` is equivalent to saying ::

    instance Sizeable Bar where {}

  and then the normal rules for filling in associated types from the
  default will apply, making ``Size Bar`` equal to ``Int``.


