.. _deriving-extra:

Deriving instances of extra classes (``Data``, etc.)
----------------------------------------------------

Haskell 98 allows the programmer to add "``deriving( Eq, Ord )``" to a
data type declaration, to generate a standard instance declaration for
classes specified in the ``deriving`` clause. In Haskell 98, the only
classes that may appear in the ``deriving`` clause are the standard
classes ``Eq``, ``Ord``, ``Enum``, ``Ix``, ``Bounded``, ``Read``, and
``Show``.

GHC extends this list with several more classes that may be
automatically derived:

-  With :extension:`DeriveGeneric`, you can derive instances of the classes
   ``Generic`` and ``Generic1``, defined in ``GHC.Generics``. You can
   use these to define generic functions, as described in
   :ref:`generic-programming`.

-  With :extension:`DeriveFunctor`, you can derive instances of the class
   ``Functor``, defined in ``GHC.Base``.

-  With :extension:`DeriveDataTypeable`, you can derive instances of the class
   ``Data``, defined in ``Data.Data``.

-  With :extension:`DeriveFoldable`, you can derive instances of the class
   ``Foldable``, defined in ``Data.Foldable``.

-  With :extension:`DeriveTraversable`, you can derive instances of the class
   ``Traversable``, defined in ``Data.Traversable``. Since the
   ``Traversable`` instance dictates the instances of ``Functor`` and
   ``Foldable``, you'll probably want to derive them too, so
   :extension:`DeriveTraversable` implies :extension:`DeriveFunctor` and
   :extension:`DeriveFoldable`.

-  With :extension:`DeriveLift`, you can derive instances of the class ``Lift``,
   defined in the ``Language.Haskell.TH.Syntax`` module of the
   ``template-haskell`` package.

You can also use a standalone deriving declaration instead (see
:ref:`stand-alone-deriving`).

In each case the appropriate class must be in scope before it can be
mentioned in the ``deriving`` clause.

.. _deriving-functor:

Deriving ``Functor`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: DeriveFunctor
    :shortdesc: Enable deriving for the Functor class.
        Implied by :extension:`DeriveTraversable`.

    :since: 7.10.1

    Allow automatic deriving of instances for the ``Functor`` typeclass.


With :extension:`DeriveFunctor`, one can derive ``Functor`` instances for data types
of kind ``Type -> Type``. For example, this declaration::

    data Example a = Ex a Char (Example a) (Example Char)
      deriving Functor

would generate the following instance: ::

    instance Functor Example where
      fmap f (Ex a1 a2 a3 a4) = Ex (f a1) a2 (fmap f a3) a4

The basic algorithm for :extension:`DeriveFunctor` walks the arguments of each
constructor of a data type, applying a mapping function depending on the type
of each argument. If a plain type variable is found that is syntactically
equivalent to the last type parameter of the data type (``a`` in the above
example), then we apply the function ``f`` directly to it. If a type is
encountered that is not syntactically equivalent to the last type parameter
*but does mention* the last type parameter somewhere in it, then a recursive
call to ``fmap`` is made. If a type is found which doesn't mention the last
type parameter at all, then it is left alone.

The second of those cases, in which a type is unequal to the type parameter but
does contain the type parameter, can be surprisingly tricky. For example, the
following example compiles::

    newtype Right a = Right (Either Int a) deriving Functor

Modifying the code slightly, however, produces code which will not compile::

    newtype Wrong a = Wrong (Either a Int) deriving Functor

The difference involves the placement of the last type parameter, ``a``. In the
``Right`` case, ``a`` occurs within the type ``Either Int a``, and moreover, it
appears as the last type argument of ``Either``. In the ``Wrong`` case,
however, ``a`` is not the last type argument to ``Either``; rather, ``Int`` is.

This distinction is important because of the way :extension:`DeriveFunctor` works. The
derived ``Functor Right`` instance would be::

    instance Functor Right where
      fmap f (Right a) = Right (fmap f a)

Given a value of type ``Right a``, GHC must produce a value of type
``Right b``. Since the argument to the ``Right`` constructor has type
``Either Int a``, the code recursively calls ``fmap`` on it to produce a value
of type ``Either Int b``, which is used in turn to construct a final value of
type ``Right b``.

The generated code for the ``Functor Wrong`` instance would look exactly the
same, except with ``Wrong`` replacing every occurrence of ``Right``. The
problem is now that ``fmap`` is being applied recursively to a value of type
``Either a Int``. This cannot possibly produce a value of type
``Either b Int``, as ``fmap`` can only change the last type parameter! This
causes the generated code to be ill-typed.

As a general rule, if a data type has a derived ``Functor`` instance and its
last type parameter occurs on the right-hand side of the data declaration, then
either it must (1) occur bare (e.g., ``newtype Id a = Id a``), or (2) occur as the
last argument of a type constructor (as in ``Right`` above).

There are two exceptions to this rule:

#. Tuple types. When a non-unit tuple is used on the right-hand side of a data
   declaration, :extension:`DeriveFunctor` treats it as a product of distinct types.
   In other words, the following code::

       newtype Triple a = Triple (a, Int, [a]) deriving Functor

   Would result in a generated ``Functor`` instance like so::

       instance Functor Triple where
         fmap f (Triple a) =
           Triple (case a of
                        (a1, a2, a3) -> (f a1, a2, fmap f a3))

   That is, :extension:`DeriveFunctor` pattern-matches its way into tuples and maps
   over each type that constitutes the tuple. The generated code is
   reminiscent of what would be generated from
   ``data Triple a = Triple a Int [a]``, except with extra machinery to handle
   the tuple.

#. Function types. The last type parameter can appear anywhere in a function
   type as long as it occurs in a *covariant* position. To illustrate what this
   means, consider the following three examples::

       newtype CovFun1 a = CovFun1 (Int -> a) deriving Functor
       newtype CovFun2 a = CovFun2 ((a -> Int) -> a) deriving Functor
       newtype CovFun3 a = CovFun3 (((Int -> a) -> Int) -> a) deriving Functor

   All three of these examples would compile without issue. On the other hand::

       newtype ContraFun1 a = ContraFun1 (a -> Int) deriving Functor
       newtype ContraFun2 a = ContraFun2 ((Int -> a) -> Int) deriving Functor
       newtype ContraFun3 a = ContraFun3 (((a -> Int) -> a) -> Int) deriving Functor

   While these examples look similar, none of them would successfully compile.
   This is because all occurrences of the last type parameter ``a`` occur in *contravariant* positions, not covariant ones.

   Intuitively, a covariant type is *produced*, and a contravariant type is
   *consumed*. Most types in Haskell are covariant, but the function type is
   special in that the lefthand side of a function arrow reverses variance. If
   a function type ``a -> b`` appears in a covariant position (e.g.,
   ``CovFun1`` above), then ``a`` is in a contravariant position and ``b`` is
   in a covariant position. Similarly, if ``a -> b`` appears in a contravariant
   position (e.g., ``CovFun2`` above), then ``a`` is in ``a`` covariant
   position and ``b`` is in a contravariant position.

   To see why a data type with a contravariant occurrence of its last type
   parameter cannot have a derived ``Functor`` instance, let's suppose that a
   ``Functor ContraFun1`` instance exists. The implementation would look
   something like this::

       instance Functor ContraFun1 where
         fmap f (ContraFun g) = ContraFun (\x -> _)

   We have ``f :: a -> b``, ``g :: a -> Int``, and ``x :: b``. Using these, we
   must somehow fill in the hole (denoted with an underscore) with a value of
   type ``Int``. What are our options?

   We could try applying ``g`` to ``x``. This won't work though, as ``g``
   expects an argument of type ``a``, and ``x :: b``. Even worse, we can't turn
   ``x`` into something of type ``a``, since ``f`` also needs an argument of
   type ``a``! In short, there's no good way to make this work.

   On the other hand, a derived ``Functor`` instances for the ``CovFun``\ s are
   within the realm of possibility::

       instance Functor CovFun1 where
         fmap f (CovFun1 g) = CovFun1 (\x -> f (g x))

       instance Functor CovFun2 where
         fmap f (CovFun2 g) = CovFun2 (\h -> f (g (\x -> h (f x))))

       instance Functor CovFun3 where
         fmap f (CovFun3 g) = CovFun3 (\h -> f (g (\k -> h (\x -> f (k x)))))

There are some other scenarios in which a derived ``Functor`` instance will
fail to compile:

#. A data type has no type parameters (e.g., ``data Nothing = Nothing``).

#. A data type's last type variable is used in a :extension:`DatatypeContexts`
   constraint (e.g., ``data Ord a => O a = O a``).

#. A data type's last type variable is used in an
   :extension:`ExistentialQuantification` constraint, or is refined in a GADT. For
   example, ::

       data T a b where
           T4 :: Ord b => b -> T a b
           T5 :: b -> T b b
           T6 :: T a (b,b)

       deriving instance Functor (T a)

   would not compile successfully due to the way in which ``b`` is constrained.

When the last type parameter has a phantom role (see :ref:`roles`), the derived
``Functor`` instance will not be produced using the usual algorithm. Instead,
the entire value will be coerced. ::

    data Phantom a = Z | S (Phantom a) deriving Functor

will produce the following instance: ::

    instance Functor Phantom where
      fmap _ = coerce

When a type has no constructors, the derived ``Functor`` instance will
simply force the (bottom) value of the argument using
:extension:`EmptyCase`. ::

    data V a deriving Functor
    type role V nominal

will produce ::

    instance Functor V where
      fmap _ z = case z of

.. _deriving-foldable:

Deriving ``Foldable`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: DeriveFoldable
    :shortdesc: Enable deriving for the Foldable class.
        Implied by :extension:`DeriveTraversable`.

    :since: 7.10.1

    Allow automatic deriving of instances for the ``Foldable`` typeclass.

With :extension:`DeriveFoldable`, one can derive ``Foldable`` instances for data types
of kind ``Type -> Type``. For example, this declaration::

    data Example a = Ex a Char (Example a) (Example Char)
      deriving Foldable

would generate the following instance::

    instance Foldable Example where
      foldr f z (Ex a1 a2 a3 a4) = f a1 (foldr f z a3)
      foldMap f (Ex a1 a2 a3 a4) = mappend (f a1) (foldMap f a3)

The algorithm for :extension:`DeriveFoldable` is adapted from the
:extension:`DeriveFunctor` algorithm, but it generates definitions for
``foldMap``, ``foldr``, and ``null`` instead of ``fmap``. In addition,
:extension:`DeriveFoldable` filters out all constructor arguments on the RHS
expression whose types do not mention the last type parameter, since those
arguments do not need to be folded over.

When the type parameter has a phantom role (see :ref:`roles`),
:extension:`DeriveFoldable` derives a trivial instance. For example, this
declaration: ::

    data Phantom a = Z | S (Phantom a)

will generate the following instance. ::

    instance Foldable Phantom where
      foldMap _ _ = mempty

Similarly, when the type has no constructors, :extension:`DeriveFoldable` will
derive a trivial instance: ::

    data V a deriving Foldable
    type role V nominal

will generate the following. ::

    instance Foldable V where
      foldMap _ _ = mempty

Here are the differences between the generated code for ``Functor`` and
``Foldable``:

#. When a bare type variable ``a`` is encountered, :extension:`DeriveFunctor`
would generate ``f a`` for an ``fmap`` definition. :extension:`DeriveFoldable`
would generate ``f a z`` for ``foldr``, ``f a`` for ``foldMap``, and ``False``
for ``null``.

#. When a type that is not syntactically equivalent to ``a``, but which does
   contain ``a``, is encountered, :extension:`DeriveFunctor` recursively calls
   ``fmap`` on it. Similarly, :extension:`DeriveFoldable` would recursively call
   ``foldr`` and ``foldMap``. Depending on the context, ``null`` may recursively
   call ``null`` or ``all null``. For example, given ::

       data F a = F (P a)
       data G a = G (P (a, Int))
       data H a = H (P (Q a))

   ``Foldable`` deriving will produce ::

       null (F x) = null x
       null (G x) = null x
       null (H x) = all null x

#. :extension:`DeriveFunctor` puts everything back together again at the end by
   invoking the constructor. :extension:`DeriveFoldable`, however, builds up a value
   of some type. For ``foldr``, this is accomplished by chaining applications
   of ``f`` and recursive ``foldr`` calls on the state value ``z``. For
   ``foldMap``, this happens by combining all values with ``mappend``. For ``null``,
   the values are usually combined with ``&&``. However, if any of the values is
   known to be ``False``, all the rest will be dropped. For example, ::

       data SnocList a = Nil | Snoc (SnocList a) a

   will not produce ::

       null (Snoc xs _) = null xs && False

   (which would walk the whole list), but rather ::

       null (Snoc _ _) = False

There are some other differences regarding what data types can have derived
``Foldable`` instances:

#. Data types containing function types on the right-hand side cannot have
   derived ``Foldable`` instances.

#. ``Foldable`` instances can be derived for data types in which the last type
   parameter is existentially constrained or refined in a GADT. For example,
   this data type::

       data E a where
           E1 :: (a ~ Int) => a   -> E a
           E2 ::              Int -> E Int
           E3 :: (a ~ Int) => a   -> E Int
           E4 :: (a ~ Int) => Int -> E a

       deriving instance Foldable E

   would have the following generated ``Foldable`` instance::

       instance Foldable E where
           foldr f z (E1 e) = f e z
           foldr f z (E2 e) = z
           foldr f z (E3 e) = z
           foldr f z (E4 e) = z

           foldMap f (E1 e) = f e
           foldMap f (E2 e) = mempty
           foldMap f (E3 e) = mempty
           foldMap f (E4 e) = mempty

   Notice how every constructor of ``E`` utilizes some sort of existential
   quantification, but only the argument of ``E1`` is actually "folded over".
   This is because we make a deliberate choice to only fold over universally
   polymorphic types that are syntactically equivalent to the last type
   parameter. In particular:

  -  We don't fold over the arguments of ``E1`` or ``E4`` because even though
     ``(a ~ Int)``, ``Int`` is not syntactically equivalent to ``a``.

  -  We don't fold over the argument of ``E3`` because ``a`` is not universally
     polymorphic. The ``a`` in ``E3`` is (implicitly) existentially quantified,
     so it is not the same as the last type parameter of ``E``.

.. _deriving-traversable:

Deriving ``Traversable`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.. extension:: DeriveTraversable
    :shortdesc: Enable deriving for the Traversable class.
        Implies :extension:`DeriveFunctor` and :extension:`DeriveFoldable`.

    :implies: :extension:`DeriveFoldable`, :extension:`DeriveFunctor`
    :since: 7.10.1

    Allow automatic deriving of instances for the ``Traversable`` typeclass.

With :extension:`DeriveTraversable`, one can derive ``Traversable`` instances for data
types of kind ``Type -> Type``. For example, this declaration::

    data Example a = Ex a Char (Example a) (Example Char)
      deriving (Functor, Foldable, Traversable)

would generate the following ``Traversable`` instance::

    instance Traversable Example where
      traverse f (Ex a1 a2 a3 a4)
        = fmap (\b1 b3 -> Ex b1 a2 b3 a4) (f a1) <*> traverse f a3

The algorithm for :extension:`DeriveTraversable` is adapted from the
:extension:`DeriveFunctor` algorithm, but it generates a definition for ``traverse``
instead of ``fmap``. In addition, :extension:`DeriveTraversable` filters out
all constructor arguments on the RHS expression whose types do not mention the
last type parameter, since those arguments do not produce any effects in a
traversal.

When the type parameter has a phantom role (see :ref:`roles`),
:extension:`DeriveTraversable` coerces its argument. For example, this
declaration::

    data Phantom a = Z | S (Phantom a) deriving Traversable

will generate the following instance::

    instance Traversable Phantom where
      traverse _ z = pure (coerce z)

When the type has no constructors, :extension:`DeriveTraversable` will
derive the laziest instance it can. ::

    data V a deriving Traversable
    type role V nominal

will generate the following, using :extension:`EmptyCase`: ::

    instance Traversable V where
      traverse _ z = pure (case z of)

Here are the differences between the generated code in each
extension:

#. When a bare type variable ``a`` is encountered, both :extension:`DeriveFunctor` and
   :extension:`DeriveTraversable` would generate ``f a`` for an ``fmap`` and
   ``traverse`` definition, respectively.

#. When a type that is not syntactically equivalent to ``a``, but which does
   contain ``a``, is encountered, :extension:`DeriveFunctor` recursively calls
   ``fmap`` on it. Similarly, :extension:`DeriveTraversable` would recursively call
   ``traverse``.

#. :extension:`DeriveFunctor` puts everything back together again at the end by
   invoking the constructor. :extension:`DeriveTraversable` does something similar,
   but it works in an ``Applicative`` context by chaining everything together
   with ``(<*>)``.

Unlike :extension:`DeriveFunctor`, :extension:`DeriveTraversable` cannot be used on data
types containing a function type on the right-hand side.

For a full specification of the algorithms used in :extension:`DeriveFunctor`,
:extension:`DeriveFoldable`, and :extension:`DeriveTraversable`, see
:ghc-wiki:`this wiki page <commentary/compiler/derive-functor>`.

.. _deriving-data:

Deriving ``Data`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: DeriveDataTypeable
    :shortdesc: Enable deriving for the ``Data`` class.
       Implied by (deprecated) ``AutoDeriveTypeable``.

    :since: 6.8.1

    Enable automatic deriving of instances for the ``Data`` typeclass

.. _deriving-typeable:

Deriving ``Typeable`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The class ``Typeable`` is very special:

-  ``Typeable`` is kind-polymorphic (see :ref:`kind-polymorphism`).

-  GHC has a custom solver for discharging constraints that involve
   class ``Typeable``, and handwritten instances are forbidden. This
   ensures that the programmer cannot subvert the type system by writing
   bogus instances.

-  Derived instances of ``Typeable`` may be declared if the
   :extension:`DeriveDataTypeable` extension is enabled, but they are ignored,
   and they may be reported as an error in a later version of the compiler.

-  The rules for solving ``Typeable`` constraints are as follows:

   -  A concrete type constructor applied to some types. ::

          instance (Typeable t1, .., Typeable t_n) =>
            Typeable (T t1 .. t_n)

      This rule works for any concrete type constructor, including type
      constructors with polymorphic kinds. The only restriction is that
      if the type constructor has a polymorphic kind, then it has to be
      applied to all of its kinds parameters, and these kinds need to be
      concrete (i.e., they cannot mention kind variables).

   -  A type variable applied to some types::

          instance (Typeable f, Typeable t1, .., Typeable t_n) =>
            Typeable (f t1 .. t_n)

   -  A concrete type literal.::

          instance Typeable 0       -- Type natural literals
          instance Typeable "Hello" -- Type-level symbols

.. _deriving-lift:

Deriving ``Lift`` instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: DeriveLift
    :shortdesc: Enable deriving for the Lift class

    :since: 8.0.1

    Enable automatic deriving of instances for the ``Lift`` typeclass for
    Template Haskell.

The class ``Lift``, unlike other derivable classes, lives in
``template-haskell`` instead of ``base``. Having a data type be an instance of
``Lift`` permits its values to be promoted to Template Haskell expressions (of
type ``ExpQ`` and ``Code Q a``), which can then be spliced into Haskell source
code.

Here is an example of how one can derive ``Lift``:

::

    {-# LANGUAGE DeriveLift #-}
    module Bar where

    import Language.Haskell.TH.Syntax

    data Foo a = Foo a | a :^: a deriving Lift

    {-
    instance (Lift a) => Lift (Foo a) where
        lift (Foo a) = [| Foo a |]
        lift ((:^:) u v) = [| (:^:) u v |]

        liftTyped (Foo a) = [|| Foo a ||]
        liftTyped ((:^:) u v) = [|| (:^:) u v ||]
    -}

    -----
    {-# LANGUAGE TemplateHaskell #-}
    module Baz where

    import Bar
    import Language.Haskell.TH.Lift

    foo :: Foo String
    foo = $(lift $ Foo "foo")

    fooExp :: Lift a => Foo a -> Q Exp
    fooExp f = [| f |]

Note that the ``Lift`` typeclass takes advantage of :ref:`runtime-rep` in order
to support instances involving unboxed types. This means :extension:`DeriveLift`
also works for these types:

::

    {-# LANGUAGE DeriveLift, MagicHash #-}
    module Unboxed where

    import GHC.Exts
    import Language.Haskell.TH.Syntax

    data IntHash = IntHash Int# deriving Lift

    {-
    instance Lift IntHash where
        lift (IntHash i) = [| IntHash i |]
        liftTyped (IntHash i) = [|| IntHash i ||]
    -}


