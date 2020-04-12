.. _datatype-contexts:

Data type contexts
------------------

.. extension:: DatatypeContexts
    :shortdesc: Allow contexts on ``data`` types.

    :since: 7.0.1

    Allow contexts on ``data`` types.

Haskell allows datatypes to be given contexts, e.g. ::

    data Eq a => Set a = NilSet | ConsSet a (Set a)

give constructors with types: ::

    NilSet :: Set a
    ConsSet :: Eq a => a -> Set a -> Set a

This is widely considered a misfeature, and is going to be removed from
the language. In GHC, it is controlled by the deprecated extension
``DatatypeContexts``.


