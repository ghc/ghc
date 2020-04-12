.. _empty-data-deriving:

Deriving instances for empty data types
---------------------------------------

.. extension:: EmptyDataDeriving
    :shortdesc: Allow deriving instances of standard type classes for
                empty data types.

    :since: 8.4.1

    Allow deriving instances of standard type classes for empty data types.

One can write data types with no constructors using the
:extension:`EmptyDataDecls` flag (see :ref:`nullary-types`), which is on by
default in Haskell 2010. What is not on by default is the ability to derive
type class instances for these types. This ability is enabled through use of
the :extension:`EmptyDataDeriving` flag. For instance, this lets one write: ::

    data Empty deriving (Eq, Ord, Read, Show)

This would generate the following instances: ::

    instance Eq Empty where
      _ == _ = True

    instance Ord Empty where
      compare _ _ = EQ

    instance Read Empty where
      readPrec = pfail

    instance Show Empty where
      showsPrec _ x = case x of {}

The :extension:`EmptyDataDeriving` flag is only required to enable deriving
of these four "standard" type classes (which are mentioned in the Haskell
Report). Other extensions to the ``deriving`` mechanism, which are explained
below in greater detail, do not require :extension:`EmptyDataDeriving` to be
used in conjunction with empty data types. These include:

* :extension:`StandaloneDeriving` (see :ref:`stand-alone-deriving`)
* Type classes which require their own extensions to be enabled to be derived,
  such as :extension:`DeriveFunctor` (see :ref:`deriving-extra`)
* :extension:`DeriveAnyClass` (see :ref:`derive-any-class`)


