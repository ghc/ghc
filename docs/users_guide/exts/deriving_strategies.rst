.. _deriving-strategies:

Deriving strategies
-------------------

.. extension:: DerivingStrategies
    :shortdesc: Enables deriving strategies.

    :since: 8.2.1

    Allow multiple ``deriving``, each optionally qualified with a *strategy*.

In most scenarios, every ``deriving`` statement generates a typeclass instance
in an unambiguous fashion. There is a corner case, however, where
simultaneously enabling both the :extension:`GeneralizedNewtypeDeriving` and
:extension:`DeriveAnyClass` extensions can make deriving become ambiguous.
Consider the following example ::

    {-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}
    newtype Foo = MkFoo Bar deriving C

One could either pick the ``DeriveAnyClass`` approach to deriving ``C`` or the
``GeneralizedNewtypeDeriving`` approach to deriving ``C``, both of which would
be equally as valid. GHC defaults to favoring ``DeriveAnyClass`` in such a
dispute, but this is not a satisfying solution, since that leaves users unable
to use both language extensions in a single module.

To make this more robust, GHC has a notion of deriving strategies, which allow
the user to explicitly request which approach to use when deriving an instance.
To enable this feature, one must enable the :extension:`DerivingStrategies`
language extension. A deriving strategy can be specified in a deriving
clause ::

    newtype Foo = MkFoo Bar
      deriving newtype C

Or in a standalone deriving declaration ::

    deriving anyclass instance C Foo

:extension:`DerivingStrategies` also allows the use of multiple deriving
clauses per data declaration so that a user can derive some instance with
one deriving strategy and other instances with another deriving strategy.
For example ::

    newtype Baz = Baz Quux
      deriving          (Eq, Ord)
      deriving stock    (Read, Show)
      deriving newtype  (Num, Floating)
      deriving anyclass C

Currently, the deriving strategies are:

- ``stock``: Have GHC implement a "standard" instance for a data type,
  if possible (e.g., ``Eq``, ``Ord``, ``Generic``, ``Data``, ``Functor``, etc.)

- ``anyclass``: Use :extension:`DeriveAnyClass` (see :ref:`derive-any-class`)

- ``newtype``: Use :extension:`GeneralizedNewtypeDeriving`
               (see :ref:`newtype-deriving`)

- ``via``: Use :extension:`DerivingVia` (see :ref:`deriving-via`)

.. _default-deriving-strategy:

Default deriving strategy
~~~~~~~~~~~~~~~~~~~~~~~~~

If an explicit deriving strategy is not given, multiple strategies may apply.
In that case, GHC chooses the strategy as follows:

1. Stock type classes, i.e. those specified in the report and those enabled by
   `language extensions <#deriving-extra>`__, are derived using the ``stock``
   strategy, with the following exception:

   * For newtypes, ``Eq``, ``Ord``, ``Ix`` and ``Bounded`` are always derived
     using the ``newtype`` strategy, even without
     ``GeneralizedNewtypeDeriving`` enabled. (There should be no observable
     difference to instances derived using the stock strategy.)

   * Also for newtypes, ``Functor``, ``Foldable`` and ``Enum`` are derived
     using the ``newtype`` strategy if ``GeneralizedNewtypeDeriving`` is
     enabled and the derivation succeeds.

2. For other any type class:

   1. When :extension:`DeriveAnyClass` is enabled, use ``anyclass``.

   2. When :extension:`GeneralizedNewtypeDeriving` is enabled and we are
      deriving for a newtype, then use ``newtype``.

   If both rules apply to a deriving clause, then ``anyclass`` is used and the
   user is warned about the ambiguity. The warning can be avoided by explicitly
   stating the desired deriving strategy.


