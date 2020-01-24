.. _static-pointers:

Static pointers
===============

.. index::
   single: Static pointers

.. extension:: StaticPointers
    :shortdesc: Enable static pointers.

    :since: 7.10.1

    Allow use of static pointer syntax.

The language extension :extension:`StaticPointers` adds a new syntactic form
``static e``, which stands for a reference to the closed expression ⟨e⟩.
This reference is stable and portable, in the sense that it remains
valid across different processes on possibly different machines. Thus, a
process can create a reference and send it to another process that can
resolve it to ⟨e⟩.

With this extension turned on, ``static`` is no longer a valid
identifier.

Static pointers were first proposed in the paper `Towards Haskell in the
cloud <http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf>`__,
Jeff Epstein, Andrew P. Black and Simon Peyton-Jones, Proceedings of the
4th ACM Symposium on Haskell, pp. 118-129, ACM, 2011.

.. _using-static-pointers:

Using static pointers
---------------------

Each reference is given a key which can be used to locate it at runtime
with :base-ref:`GHC.StaticPtr.unsafeLookupStaticPtr`
which uses a global and immutable table called the Static Pointer Table.
The compiler includes entries in this table for all static forms found
in the linked modules. The value can be obtained from the reference via
:base-ref:`GHC.StaticPtr.deRefStaticPtr`.

The body ``e`` of a ``static e`` expression must be a closed expression. Where
we say an expression is *closed* when all of its free (type) variables are
closed. And a variable is *closed* if it is let-bound to a *closed* expression
and its type is *closed* as well. And a type is *closed* if it has no free
variables.

All of the following are permissible: ::

    inc :: Int -> Int
    inc x = x + 1

    ref1 = static 1
    ref2 = static inc
    ref3 = static (inc 1)
    ref4 = static ((\x -> x + 1) (1 :: Int))
    ref5 y = static (let x = 1 in x)
    ref6 y = let x = 1 in static x

While the following definitions are rejected: ::

    ref7 y = let x = y in static x    -- x is not closed
    ref8 y = static (let x = 1 in y)  -- y is not let-bound
    ref8 (y :: a) = let x = undefined :: a
                     in static x      -- x has a non-closed type

.. note::

    While modules loaded in GHCi with the :ghci-cmd:`:load` command may use
    :extension:`StaticPointers` and ``static`` expressions, statements
    entered on the REPL may not. This is a limitation of GHCi; see
    :ghc-ticket:`12356` for details.

.. note::

    The set of keys used for locating static pointers in the Static Pointer
    Table is not guaranteed to remain stable for different program binaries.
    Or in other words, only processes launched from the same program binary
    are guaranteed to use the same set of keys.

.. _typechecking-static-pointers:

Static semantics of static pointers
-----------------------------------

Informally, if we have a closed expression ::

    e :: forall a_1 ... a_n . t

the static form is of type ::

    static e :: (IsStatic p, Typeable a_1, ... , Typeable a_n) => p t


A static form determines a value of type ``StaticPtr t``, but just
like ``OverloadedLists`` and ``OverloadedStrings``, this literal
expression is overloaded to allow lifting a ``StaticPtr`` into another
type implicitly, via the ``IsStatic`` class: ::

    class IsStatic p where
        fromStaticPtr :: StaticPtr a -> p a

The only predefined instance is the obvious one that does nothing: ::

    instance IsStatic StaticPtr where
        fromStaticPtr sptr = sptr

See :base-ref:`GHC.StaticPtr.IsStatic`.

Furthermore, type ``t`` is constrained to have a ``Typeable`` instance.
The following are therefore illegal: ::

    static show                    -- No Typeable instance for (Show a => a -> String)
    static Control.Monad.ST.runST  -- No Typeable instance for ((forall s. ST s a) -> a)

That being said, with the appropriate use of wrapper datatypes, the
above limitations induce no loss of generality: ::

    {-# LANGUAGE ConstraintKinds           #-}
    {-# LANGUAGE ExistentialQuantification #-}
    {-# LANGUAGE Rank2Types                #-}
    {-# LANGUAGE StandaloneDeriving        #-}
    {-# LANGUAGE StaticPointers            #-}

    import Control.Monad.ST
    import Data.Typeable
    import GHC.StaticPtr

    data Dict c = c => Dict

    g1 :: Typeable a => StaticPtr (Dict (Show a) -> a -> String)
    g1 = static (\Dict -> show)

    data Rank2Wrapper f = R2W (forall s. f s)
      deriving Typeable
    newtype Flip f a s = Flip { unFlip :: f s a }
      deriving Typeable

    g2 :: Typeable a => StaticPtr (Rank2Wrapper (Flip ST a) -> a)
    g2 = static (\(R2W f) -> runST (unFlip f))


