.. _deriving:

Deriving mechanism
==================

Haskell 98 allows the programmer to add a deriving clause to a data type
declaration, to generate a standard instance declaration for specified class.
GHC extends this mechanism along several axes:

* The derivation mechanism can be used separately from the data type
  declaration, using the :ref:`standalone deriving mechanism
  <stand-alone-deriving>`.

* In Haskell 98, the only derivable classes are ``Eq``,
  ``Ord``, ``Enum``, ``Ix``, ``Bounded``, ``Read``, and ``Show``. :ref:`Various
  language extensions <deriving-extra>` extend this list.

* Besides the stock approach to deriving instances by generating all method
  definitions, GHC supports two additional deriving strategies, which can
  derive arbitrary classes:

  * :ref:`Generalised newtype deriving <newtype-deriving>` for newtypes and
  * :ref:`deriving any class <derive-any-class>` using an empty instance
    declaration.

  The user can optionally declare the desired :ref:`deriving strategy
  <deriving-strategies>`, especially if the compiler chooses the wrong
  one :ref:`by default <default-deriving-strategy>`.


.. toctree::
    :maxdepth: 1

    empty_data_deriving
    deriving_inferred
    standalone_deriving
    deriving_extra
    newtype_deriving
    derive_any_class
    deriving_strategies
    deriving_via
