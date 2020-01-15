.. _generic-programming:

Generic programming
===================

Using a combination of :extension:`DeriveGeneric`,
:extension:`DefaultSignatures`, and :extension:`DeriveAnyClass`, you can
easily do datatype-generic programming using the :base-ref:`GHC.Generics.`
framework. This section gives a very brief overview of how to do it.

Generic programming support in GHC allows defining classes with methods
that do not need a user specification when instantiating: the method
body is automatically derived by GHC. This is similar to what happens
for standard classes such as ``Read`` and ``Show``, for instance, but
now for user-defined classes.

.. _generic-classes:

.. note::

   GHC used to have an implementation of generic classes as defined in the paper
   "Derivable type classes", Ralf Hinze and Simon Peyton Jones, Haskell
   Workshop, Montreal Sept 2000, pp. 94-105. These have been removed and
   replaced by the more general support for generic programming.

Deriving representations
------------------------

The first thing we need is generic representations. The ``GHC.Generics``
module defines a couple of primitive types that are used to represent
Haskell datatypes: ::

    -- | Unit: used for constructors without arguments
    data U1 p = U1

    -- | Constants, additional parameters and recursion of kind Type
    newtype K1 i c p = K1 { unK1 :: c }

    -- | Meta-information (constructor names, etc.)
    newtype M1 i c f p = M1 { unM1 :: f p }

    -- | Sums: encode choice between constructors
    infixr 5 :+:
    data (:+:) f g p = L1 (f p) | R1 (g p)

    -- | Products: encode multiple arguments to constructors
    infixr 6 :*:
    data (:*:) f g p = f p :*: g p

The ``Generic`` and ``Generic1`` classes mediate between user-defined
datatypes and their internal representation as a sum-of-products: ::

    class Generic a where
      -- Encode the representation of a user datatype
      type Rep a :: Type -> Type
      -- Convert from the datatype to its representation
      from  :: a -> (Rep a) x
      -- Convert from the representation to the datatype
      to    :: (Rep a) x -> a

    class Generic1 (f :: k -> Type) where
      type Rep1 f :: k -> Type

      from1  :: f a -> Rep1 f a
      to1    :: Rep1 f a -> f a

``Generic1`` is used for functions that can only be defined over type
containers, such as ``map``. Note that ``Generic1`` ranges over types of kind
``Type -> Type`` by default, but if the :extension:`PolyKinds` extension is
enabled, then it can range of types of kind ``k -> Type``, for any kind ``k``.

.. extension:: DeriveGeneric
    :shortdesc: Enable deriving for the Generic class.

    :since: 7.2.1

    Allow automatic deriving of instances for the ``Generic`` typeclass.


Instances of these classes can be derived by GHC with the
:extension:`DeriveGeneric` extension, and are necessary to be able to define
generic instances automatically.

For example, a user-defined datatype of trees ::

    data UserTree a = Node a (UserTree a) (UserTree a) | Leaf

in a ``Main`` module in a package named ``foo`` will get the following
representation: ::

    instance Generic (UserTree a) where
      -- Representation type
      type Rep (UserTree a) =
        M1 D ('MetaData "UserTree" "Main" "package-name" 'False) (
              M1 C ('MetaCons "Node" 'PrefixI 'False) (
                    M1 S ('MetaSel 'Nothing
                                   'NoSourceUnpackedness
                                   'NoSourceStrictness
                                   'DecidedLazy)
                         (K1 R a)
                :*: M1 S ('MetaSel 'Nothing
                                   'NoSourceUnpackedness
                                   'NoSourceStrictness
                                   'DecidedLazy)
                         (K1 R (UserTree a))
                :*: M1 S ('MetaSel 'Nothing
                                   'NoSourceUnpackedness
                                   'NoSourceStrictness
                                   'DecidedLazy)
                         (K1 R (UserTree a)))
          :+: M1 C ('MetaCons "Leaf" 'PrefixI 'False) U1)

      -- Conversion functions
      from (Node x l r) = M1 (L1 (M1 (M1 (K1 x) :*: M1 (K1 l) :*: M1 (K1 r))))
      from Leaf         = M1 (R1 (M1 U1))
      to (M1 (L1 (M1 (M1 (K1 x) :*: M1 (K1 l) :*: M1 (K1 r))))) = Node x l r
      to (M1 (R1 (M1 U1)))                                      = Leaf

This representation is generated automatically if a ``deriving Generic``
clause is attached to the datatype. `Standalone
deriving <#stand-alone-deriving>`__ can also be used.

Writing generic functions
-------------------------

A generic function is defined by creating a class and giving instances
for each of the representation types of ``GHC.Generics``. As an example
we show generic serialization: ::

    data Bin = O | I

    class GSerialize f where
      gput :: f a -> [Bin]

    instance GSerialize U1 where
      gput U1 = []

    instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
      gput (x :*: y) = gput x ++ gput y

    instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
      gput (L1 x) = O : gput x
      gput (R1 x) = I : gput x

    instance (GSerialize a) => GSerialize (M1 i c a) where
      gput (M1 x) = gput x

    instance (Serialize a) => GSerialize (K1 i a) where
      gput (K1 x) = put x

A caveat: this encoding strategy may not be reliable across different versions
of GHC. When deriving a ``Generic`` instance is free to choose any nesting of
``:+:`` and ``:*:`` it chooses, so if GHC chooses ``(a :+: b) :+: c``, then the
encoding for ``a`` would be ``[O, O]``, ``b`` would be ``[O, I]``, and ``c``
would be ``[I]``. However, if GHC chooses ``a :+: (b :+: c)``, then the
encoding for ``a`` would be ``[O]``, ``b`` would be ``[I, O]``, and ``c`` would
be ``[I, I]``. (In practice, the current implementation tries to produce a
more-or-less balanced nesting of ``:+:`` and ``:*:`` so that the traversal of
the structure of the datatype from the root to a particular component can be
performed in logarithmic rather than linear time.)

Typically this ``GSerialize`` class will not be exported, as it only makes
sense to have instances for the representation types.

Unlifted representation types
-----------------------------

The data family ``URec`` is provided to enable generic programming over
datatypes with certain unlifted arguments. There are six instances corresponding
to common unlifted types: ::

    data family URec a p

    data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#   }
    data instance URec Char     p = UChar   { uChar#   :: Char#   }
    data instance URec Double   p = UDouble { uDouble# :: Double# }
    data instance URec Int      p = UInt    { uInt#    :: Int#    }
    data instance URec Float    p = UFloat  { uFloat#  :: Float#  }
    data instance URec Word     p = UWord   { uWord#   :: Word#   }

Six type synonyms are provided for convenience: ::

    type UAddr   = URec (Ptr ())
    type UChar   = URec Char
    type UDouble = URec Double
    type UFloat  = URec Float
    type UInt    = URec Int
    type UWord   = URec Word

As an example, this data declaration: ::

    data IntHash = IntHash Int#
      deriving Generic

results in the following ``Generic`` instance: ::

    instance 'Generic' IntHash where
      type 'Rep' IntHash =
        'D1' ('MetaData "IntHash" "Main" "package-name" 'False)
          ('C1' ('MetaCons "IntHash" 'PrefixI 'False)
            ('S1' ('MetaSel 'Nothing
                            'NoSourceUnpackedness
                            'NoSourceStrictness
                            'DecidedLazy)
                  'UInt'))

A user could provide, for example, a ``GSerialize UInt`` instance so that a
``Serialize IntHash`` instance could be easily defined in terms of
``GSerialize``.

Generic defaults
----------------

The only thing left to do now is to define a "front-end" class, which is
exposed to the user: ::

    class Serialize a where
      put :: a -> [Bin]

      default put :: (Generic a, GSerialize (Rep a)) => a -> [Bin]
      put = gput . from

Here we use a `default signature <#class-default-signatures>`__ to
specify that the user does not have to provide an implementation for
``put``, as long as there is a ``Generic`` instance for the type to
instantiate. For the ``UserTree`` type, for instance, the user can just
write: ::

    instance (Serialize a) => Serialize (UserTree a)

The default method for ``put`` is then used, corresponding to the
generic implementation of serialization. If you are using
:extension:`DeriveAnyClass`, the same instance is generated by simply attaching
a ``deriving Serialize`` clause to the ``UserTree`` datatype
declaration. For more examples of generic functions please refer to the
`generic-deriving <http://hackage.haskell.org/package/generic-deriving>`__
package on Hackage.

More information
----------------

For more details please refer to the `Haskell Wiki
page <http://www.haskell.org/haskellwiki/GHC.Generics>`__ or the
original paper [Generics2010]_.

.. [Generics2010] Jose Pedro Magalhaes, Atze Dijkstra, Johan Jeuring, and Andres Loeh.
   `A generic deriving mechanism for Haskell
   <http://dreixel.net/research/pdf/gdmh.pdf>`__. Proceedings of
   the third ACM Haskell symposium on Haskell (Haskell'2010), pp. 37-48,
   ACM, 2010.

