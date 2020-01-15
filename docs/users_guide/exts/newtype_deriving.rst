.. _newtype-deriving:

Generalised derived instances for newtypes
------------------------------------------

.. extension:: GeneralisedNewtypeDeriving
               GeneralizedNewtypeDeriving
    :shortdesc: Enable newtype deriving.

    :since: 6.8.1. British spelling since 8.6.1.

    Enable GHC's cunning generalised deriving mechanism for ``newtype``\s

When you define an abstract type using ``newtype``, you may want the new
type to inherit some instances from its representation. In Haskell 98,
you can inherit instances of ``Eq``, ``Ord``, ``Enum`` and ``Bounded``
by deriving them, but for any other classes you have to write an
explicit instance declaration. For example, if you define ::

      newtype Dollars = Dollars Int

and you want to use arithmetic on ``Dollars``, you have to explicitly
define an instance of ``Num``: ::

      instance Num Dollars where
        Dollars a + Dollars b = Dollars (a+b)
        ...

All the instance does is apply and remove the ``newtype`` constructor.
It is particularly galling that, since the constructor doesn't appear at
run-time, this instance declaration defines a dictionary which is
*wholly equivalent* to the ``Int`` dictionary, only slower!

:extension:`DerivingVia` (see :ref:`deriving-via`) is a generalization of
this idea.

.. _generalized-newtype-deriving:

Generalising the deriving clause
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC now permits such instances to be derived instead, using the extension
:extension:`GeneralizedNewtypeDeriving`, so one can write ::

      newtype Dollars = Dollars { getDollars :: Int } deriving (Eq,Show,Num)

and the implementation uses the *same* ``Num`` dictionary for
``Dollars`` as for ``Int``. In other words, GHC will generate something that
resembles the following code ::

      instance Num Int => Num Dollars

and then attempt to simplify the ``Num Int`` context as much as possible.
GHC knows that there is a ``Num Int`` instance in scope, so it is able to
discharge the ``Num Int`` constraint, leaving the code that GHC actually
generates ::

      instance Num Dollars

One can think of this instance being implemented with the same code as the
``Num Int`` instance, but with ``Dollars`` and ``getDollars`` added wherever
necessary in order to make it typecheck. (In practice, GHC uses a somewhat
different approach to code generation. See the :ref:`precise-gnd-specification`
section below for more details.)

We can also derive instances of constructor classes in a similar way.
For example, suppose we have implemented state and failure monad
transformers, such that ::

      instance Monad m => Monad (State s m)
      instance Monad m => Monad (Failure m)

In Haskell 98, we can define a parsing monad by ::

      type Parser tok m a = State [tok] (Failure m) a

which is automatically a monad thanks to the instance declarations
above. With the extension, we can make the parser type abstract, without
needing to write an instance of class ``Monad``, via ::

      newtype Parser tok m a = Parser (State [tok] (Failure m) a)
                             deriving Monad

In this case the derived instance declaration is of the form ::

      instance Monad (State [tok] (Failure m)) => Monad (Parser tok m)

Notice that, since ``Monad`` is a constructor class, the instance is a
*partial application* of the newtype, not the entire left hand side. We
can imagine that the type declaration is "eta-converted" to generate the
context of the instance declaration.

We can even derive instances of multi-parameter classes, provided the
newtype is the last class parameter. In this case, a "partial
application" of the class appears in the ``deriving`` clause. For
example, given the class ::

      class StateMonad s m | m -> s where ...
      instance Monad m => StateMonad s (State s m) where ...

then we can derive an instance of ``StateMonad`` for ``Parser`` by ::

      newtype Parser tok m a = Parser (State [tok] (Failure m) a)
                             deriving (Monad, StateMonad [tok])

The derived instance is obtained by completing the application of the
class to the new type: ::

      instance StateMonad [tok] (State [tok] (Failure m)) =>
               StateMonad [tok] (Parser tok m)

As a result of this extension, all derived instances in newtype
declarations are treated uniformly (and implemented just by reusing the
dictionary for the representation type), *except* ``Show`` and ``Read``,
which really behave differently for the newtype and its representation.

.. note::

    It is sometimes necessary to enable additional language extensions when
    deriving instances via :extension:`GeneralizedNewtypeDeriving`. For instance,
    consider a simple class and instance using :extension:`UnboxedTuples`
    syntax: ::

        {-# LANGUAGE UnboxedTuples #-}

        module Lib where

        class AClass a where
          aMethod :: a -> (# Int, a #)

        instance AClass Int where
          aMethod x = (# x, x #)

    The following will fail with an "Illegal unboxed tuple" error, since the
    derived instance produced by the compiler makes use of unboxed tuple syntax,
    ::

        {-# LANGUAGE GeneralizedNewtypeDeriving #-}

        import Lib

        newtype Int' = Int' Int
                     deriving (AClass)

    However, enabling the :extension:`UnboxedTuples` extension allows the module
    to compile. Similar errors may occur with a variety of extensions,
    including:

      * :extension:`UnboxedTuples`
      * :extension:`PolyKinds`
      * :extension:`MultiParamTypeClasses`
      * :extension:`FlexibleContexts`

.. _precise-gnd-specification:

A more precise specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A derived instance is derived only for declarations of these forms
(after expansion of any type synonyms) ::

      newtype T v1..vn                   = MkT (t vk+1..vn) deriving (C t1..tj)
      newtype instance T s1..sk vk+1..vn = MkT (t vk+1..vn) deriving (C t1..tj)

where

-  ``v1..vn`` are type variables, and ``t``, ``s1..sk``, ``t1..tj`` are
   types.

-  The ``(C t1..tj)`` is a partial applications of the class ``C``,
   where the arity of ``C`` is exactly ``j+1``. That is, ``C`` lacks
   exactly one type argument.

-  ``k`` is chosen so that ``C t1..tj (T v1...vk)`` is well-kinded. (Or,
   in the case of a ``data instance``, so that ``C t1..tj (T s1..sk)``
   is well kinded.)

-  The type ``t`` is an arbitrary type.

-  The type variables ``vk+1...vn`` do not occur in the types ``t``,
   ``s1..sk``, or ``t1..tj``.

-  ``C`` is not ``Read``, ``Show``, ``Typeable``, or ``Data``. These
   classes should not "look through" the type or its constructor. You
   can still derive these classes for a newtype, but it happens in the
   usual way, not via this new mechanism. Confer with
   :ref:`default-deriving-strategy`.

-  It is safe to coerce each of the methods of ``C``. That is, the
   missing last argument to ``C`` is not used at a nominal role in any
   of the ``C``'s methods. (See :ref:`roles`.)

- ``C`` is allowed to have associated type families, provided they meet the
  requirements laid out in the section on :ref:`GND and associated types
  <gnd-and-associated-types>`.

Then the derived instance declaration is of the form ::

      instance C t1..tj t => C t1..tj (T v1...vk)

Note that if ``C`` does not contain any class methods, the instance context
is wholly unnecessary, and as such GHC will instead generate: ::

      instance C t1..tj (T v1..vk)

As an example which does *not* work, consider ::

      newtype NonMonad m s = NonMonad (State s m s) deriving Monad

Here we cannot derive the instance ::

      instance Monad (State s m) => Monad (NonMonad m)

because the type variable ``s`` occurs in ``State s m``, and so cannot
be "eta-converted" away. It is a good thing that this ``deriving``
clause is rejected, because ``NonMonad m`` is not, in fact, a monad ---
for the same reason. Try defining ``>>=`` with the correct type: you
won't be able to.

Notice also that the *order* of class parameters becomes important,
since we can only derive instances for the last one. If the
``StateMonad`` class above were instead defined as ::

      class StateMonad m s | m -> s where ...

then we would not have been able to derive an instance for the
``Parser`` type above. We hypothesise that multi-parameter classes
usually have one "main" parameter for which deriving new instances is
most interesting.

Lastly, all of this applies only for classes other than ``Read``,
``Show``, ``Typeable``, and ``Data``, for which the stock derivation
applies (section 4.3.3. of the Haskell Report). (For the standard
classes ``Eq``, ``Ord``, ``Ix``, and ``Bounded`` it is immaterial
whether the stock method is used or the one described here.)

.. _gnd-and-associated-types:

Associated type families
~~~~~~~~~~~~~~~~~~~~~~~~

:extension:`GeneralizedNewtypeDeriving` also works for some type classes with
associated type families. Here is an example: ::

      class HasRing a where
        type Ring a

      newtype L1Norm a = L1Norm a
        deriving HasRing

The derived ``HasRing`` instance would look like ::

      instance HasRing (L1Norm a) where
        type Ring (L1Norm a) = Ring a

To be precise, if the class being derived is of the form ::

      class C c_1 c_2 ... c_m where
        type T1 t1_1 t1_2 ... t1_n
        ...
        type Tk tk_1 tk_2 ... tk_p

and the newtype is of the form ::

      newtype N n_1 n_2 ... n_q = MkN <rep-type>

then you can derive a ``C c_1 c_2 ... c_(m-1)`` instance for
``N n_1 n_2 ... n_q``, provided that:

- The type parameter ``c_m`` occurs once in each of the type variables of
  ``T1`` through ``Tk``. Imagine a class where this condition didn't hold.
  For example: ::

      class Bad a b where
        type B a

      instance Bad Int a where
        type B Int = Char

      newtype Foo a = Foo a
        deriving (Bad Int)

  For the derived ``Bad Int`` instance, GHC would need to generate something
  like this: ::

      instance Bad Int (Foo a) where
        type B Int = B ???

  Now we're stuck, since we have no way to refer to ``a`` on the right-hand
  side of the ``B`` family instance, so this instance doesn't really make sense
  in a :extension:`GeneralizedNewtypeDeriving` setting.

- ``C`` does not have any associated data families (only type families). To
  see why data families are forbidden, imagine the following scenario: ::

      class Ex a where
        data D a

      instance Ex Int where
        data D Int = DInt Bool

      newtype Age = MkAge Int deriving Ex

  For the derived ``Ex`` instance, GHC would need to generate something like
  this: ::

      instance Ex Age where
        data D Age = ???

  But it is not clear what GHC would fill in for ``???``, as each data family
  instance must generate fresh data constructors.

If both of these conditions are met, GHC will generate this instance: ::

      instance C c_1 c_2 ... c_(m-1) <rep-type> =>
               C c_1 c_2 ... c_(m-1) (N n_1 n_2 ... n_q) where
        type T1 t1_1 t1_2 ... (N n_1 n_2 ... n_q) ... t1_n
           = T1 t1_1 t1_2 ... <rep-type>          ... t1_n
        ...
        type Tk tk_1 tk_2 ... (N n_1 n_2 ... n_q) ... tk_p
           = Tk tk_1 tk_2 ... <rep-type>          ... tk_p

Again, if ``C`` contains no class methods, the instance context will be
redundant, so GHC will instead generate
``instance C c_1 c_2 ... c_(m-1) (N n_1 n_2 ... n_q)``.

Beware that in some cases, you may need to enable the
:extension:`UndecidableInstances` extension in order to use this feature.
Here's a pathological case that illustrates why this might happen: ::

      class C a where
        type T a

      newtype Loop = MkLoop Loop
        deriving C

This will generate the derived instance: ::

      instance C Loop where
        type T Loop = T Loop

Here, it is evident that attempting to use the type ``T Loop`` will throw the
typechecker into an infinite loop, as its definition recurses endlessly. In
other cases, you might need to enable :extension:`UndecidableInstances` even
if the generated code won't put the typechecker into a loop. For example: ::

      instance C Int where
        type C Int = Int

      newtype MyInt = MyInt Int
        deriving C

This will generate the derived instance: ::

      instance C MyInt where
        type T MyInt = T Int

Although typechecking ``T MyInt`` will terminate, GHC's termination checker
isn't sophisticated enough to determine this, so you'll need to enable
:extension:`UndecidableInstances` in order to use this derived instance. If
you do go down this route, make sure you can convince yourself that all of
the type family instances you're deriving will eventually terminate if used!

Note that :extension:`DerivingVia` (see :ref:`deriving-via`) uses essentially
the same specification to derive instances of associated type families as well
(except that it uses the ``via`` type instead of the underlying ``rep-type``
of a newtype).


