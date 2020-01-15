.. _overloaded-lists:

Overloaded lists
----------------

.. extension:: OverloadedLists
    :shortdesc: Enable overloaded lists.

    :since: 7.8.1

    Enable overloaded list syntax (e.g. desugaring of lists via the
    ``IsList`` class).

GHC supports *overloading of the list notation*. Let us recap the
notation for constructing lists. In Haskell, the list notation can be
used in the following seven ways:

::

    []          -- Empty list
    [x]         -- x : []
    [x,y,z]     -- x : y : z : []
    [x .. ]     -- enumFrom x
    [x,y ..]    -- enumFromThen x y
    [x .. y]    -- enumFromTo x y
    [x,y .. z]  -- enumFromThenTo x y z

When the ``OverloadedLists`` extension is turned on, the aforementioned
seven notations are desugared as follows:

::

    []          -- fromListN 0 []
    [x]         -- fromListN 1 (x : [])
    [x,y,z]     -- fromListN 3 (x : y : z : [])
    [x .. ]     -- fromList (enumFrom x)
    [x,y ..]    -- fromList (enumFromThen x y)
    [x .. y]    -- fromList (enumFromTo x y)
    [x,y .. z]  -- fromList (enumFromThenTo x y z)

This extension allows programmers to use the list notation for
construction of structures like: ``Set``, ``Map``, ``IntMap``,
``Vector``, ``Text`` and ``Array``. The following code listing gives a
few examples:

::

    ['0' .. '9']             :: Set Char
    [1 .. 10]                :: Vector Int
    [("default",0), (k1,v1)] :: Map String Int
    ['a' .. 'z']             :: Text

List patterns are also overloaded. When the ``OverloadedLists``
extension is turned on, these definitions are desugared as follows

::

    f [] = ...          -- f (toList -> []) = ...
    g [x,y,z] = ...     -- g (toList -> [x,y,z]) = ...

(Here we are using view-pattern syntax for the translation, see
:ref:`view-patterns`.)

The ``IsList`` class
~~~~~~~~~~~~~~~~~~~~

In the above desugarings, the functions ``toList``, ``fromList`` and
``fromListN`` are all methods of the ``IsList`` class, which is itself
exported from the ``GHC.Exts`` module. The type class is defined as
follows:

::

    class IsList l where
      type Item l

      fromList :: [Item l] -> l
      toList   :: l -> [Item l]

      fromListN :: Int -> [Item l] -> l
      fromListN _ = fromList

The ``IsList`` class and its methods are intended to be used in
conjunction with the ``OverloadedLists`` extension.

-  The type function ``Item`` returns the type of items of the structure
   ``l``.

-  The function ``fromList`` constructs the structure ``l`` from the
   given list of ``Item l``.

-  The function ``fromListN`` takes the input list's length as a hint.
   Its behaviour should be equivalent to ``fromList``. The hint can be
   used for more efficient construction of the structure ``l`` compared
   to ``fromList``. If the given hint is not equal to the input list's
   length the behaviour of ``fromListN`` is not specified.

-  The function ``toList`` should be the inverse of ``fromList``.

It is perfectly fine to declare new instances of ``IsList``, so that
list notation becomes useful for completely new data types. Here are
several example instances:

::

    instance IsList [a] where
      type Item [a] = a
      fromList = id
      toList = id

    instance (Ord a) => IsList (Set a) where
      type Item (Set a) = a
      fromList = Set.fromList
      toList = Set.toList

    instance (Ord k) => IsList (Map k v) where
      type Item (Map k v) = (k,v)
      fromList = Map.fromList
      toList = Map.toList

    instance IsList (IntMap v) where
      type Item (IntMap v) = (Int,v)
      fromList = IntMap.fromList
      toList = IntMap.toList

    instance IsList Text where
      type Item Text = Char
      fromList = Text.pack
      toList = Text.unpack

    instance IsList (Vector a) where
      type Item (Vector a) = a
      fromList  = Vector.fromList
      fromListN = Vector.fromListN
      toList = Vector.toList

Rebindable syntax
~~~~~~~~~~~~~~~~~

When desugaring list notation with :extension:`OverloadedLists` GHC uses the
``fromList`` (etc) methods from module ``GHC.Exts``. You do not need to
import ``GHC.Exts`` for this to happen.

However if you use :extension:`RebindableSyntax`, then GHC instead uses
whatever is in scope with the names of ``toList``, ``fromList`` and
``fromListN``. That is, these functions are rebindable; c.f.
:ref:`rebindable-syntax`.

Defaulting
~~~~~~~~~~

Currently, the ``IsList`` class is not accompanied with defaulting
rules. Although feasible, not much thought has gone into how to specify
the meaning of the default declarations like: ::

    default ([a])

Speculation about the future
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The current implementation of the ``OverloadedLists`` extension can be
improved by handling the lists that are only populated with literals in
a special way. More specifically, the compiler could allocate such lists
statically using a compact representation and allow ``IsList`` instances
to take advantage of the compact representation. Equipped with this
capability the ``OverloadedLists`` extension will be in a good position
to subsume the ``OverloadedStrings`` extension (currently, as a special
case, string literals benefit from statically allocated compact
representation).


