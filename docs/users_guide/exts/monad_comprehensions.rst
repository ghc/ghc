.. _monad-comprehensions:

Monad comprehensions
--------------------

.. index::
   single: monad comprehensions

.. extension:: MonadComprehensions
    :shortdesc: Enable monad comprehensions.

    :since: 7.2.1

    Enable list comprehension syntax for arbitrary monads.

Monad comprehensions generalise the list comprehension notation,
including parallel comprehensions (:ref:`parallel-list-comprehensions`)
and transform comprehensions (:ref:`generalised-list-comprehensions`) to
work for any monad.

Monad comprehensions support:

-  Bindings: ::

       [ x + y | x <- Just 1, y <- Just 2 ]

   Bindings are translated with the ``(>>=)`` and ``return`` functions
   to the usual do-notation: ::

       do x <- Just 1
          y <- Just 2
          return (x+y)

-  Guards: ::

       [ x | x <- [1..10], x <= 5 ]

   Guards are translated with the ``guard`` function, which requires a
   ``MonadPlus`` instance: ::

       do x <- [1..10]
          guard (x <= 5)
          return x

-  Transform statements (as with :extension:`TransformListComp`): ::

       [ x+y | x <- [1..10], y <- [1..x], then take 2 ]

   This translates to: ::

       do (x,y) <- take 2 (do x <- [1..10]
                              y <- [1..x]
                              return (x,y))
          return (x+y)

-  Group statements (as with :extension:`TransformListComp`):

   ::

       [ x | x <- [1,1,2,2,3], then group by x using GHC.Exts.groupWith ]
       [ x | x <- [1,1,2,2,3], then group using myGroup ]

-  Parallel statements (as with :extension:`ParallelListComp`):

   ::

       [ (x+y) | x <- [1..10]
               | y <- [11..20]
               ]

   Parallel statements are translated using the ``mzip`` function, which
   requires a ``MonadZip`` instance defined in
   :base-ref:`Control.Monad.Zip.`:

   ::

       do (x,y) <- mzip (do x <- [1..10]
                            return x)
                        (do y <- [11..20]
                            return y)
          return (x+y)

All these features are enabled by default if the :extension:`MonadComprehensions`
extension is enabled. The types and more detailed examples on how to use
comprehensions are explained in the previous chapters
:ref:`generalised-list-comprehensions` and
:ref:`parallel-list-comprehensions`. In general you just have to replace
the type ``[a]`` with the type ``Monad m => m a`` for monad
comprehensions.

.. note::
    Even though most of these examples are using the list monad, monad
    comprehensions work for any monad. The ``base`` package offers all
    necessary instances for lists, which make :extension:`MonadComprehensions`
    backward compatible to built-in, transform and parallel list
    comprehensions.

More formally, the desugaring is as follows. We write ``D[ e | Q]`` to
mean the desugaring of the monad comprehension ``[ e | Q]``:

.. code-block:: none

    Expressions: e
    Declarations: d
    Lists of qualifiers: Q,R,S

    -- Basic forms
    D[ e | ]               = return e
    D[ e | p <- e, Q ]  = e >>= \p -> D[ e | Q ]
    D[ e | e, Q ]          = guard e >> \p -> D[ e | Q ]
    D[ e | let d, Q ]      = let d in D[ e | Q ]

    -- Parallel comprehensions (iterate for multiple parallel branches)
    D[ e | (Q | R), S ]    = mzip D[ Qv | Q ] D[ Rv | R ] >>= \(Qv,Rv) -> D[ e | S ]

    -- Transform comprehensions
    D[ e | Q then f, R ]                  = f D[ Qv | Q ] >>= \Qv -> D[ e | R ]

    D[ e | Q then f by b, R ]             = f (\Qv -> b) D[ Qv | Q ] >>= \Qv -> D[ e | R ]

    D[ e | Q then group using f, R ]      = f D[ Qv | Q ] >>= \ys ->
                                            case (fmap selQv1 ys, ..., fmap selQvn ys) of
                                             Qv -> D[ e | R ]

    D[ e | Q then group by b using f, R ] = f (\Qv -> b) D[ Qv | Q ] >>= \ys ->
                                            case (fmap selQv1 ys, ..., fmap selQvn ys) of
                                               Qv -> D[ e | R ]

    where  Qv is the tuple of variables bound by Q (and used subsequently)
           selQvi is a selector mapping Qv to the ith component of Qv

    Operator     Standard binding       Expected type
    --------------------------------------------------------------------
    return       GHC.Base               t1 -> m t2
    (>>=)        GHC.Base               m1 t1 -> (t2 -> m2 t3) -> m3 t3
    (>>)         GHC.Base               m1 t1 -> m2 t2         -> m3 t3
    guard        Control.Monad          t1 -> m t2
    fmap         GHC.Base               forall a b. (a->b) -> n a -> n b
    mzip         Control.Monad.Zip      forall a b. m a -> m b -> m (a,b)

The comprehension should typecheck when its desugaring would typecheck,
except that (as discussed in :ref:`generalised-list-comprehensions`) in the
"then ``f``" and "then group using ``f``" clauses, when the "by ``b``" qualifier
is omitted, argument ``f`` should have a polymorphic type. In particular, "then
``Data.List.sort``" and "then group using ``Data.List.group``" are
insufficiently polymorphic.

Monad comprehensions support rebindable syntax
(:ref:`rebindable-syntax`). Without rebindable syntax, the operators
from the "standard binding" module are used; with rebindable syntax, the
operators are looked up in the current lexical scope. For example,
parallel comprehensions will be typechecked and desugared using whatever
"``mzip``" is in scope.

The rebindable operators must have the "Expected type" given in the
table above. These types are surprisingly general. For example, you can
use a bind operator with the type

::

    (>>=) :: T x y a -> (a -> T y z b) -> T x z b

In the case of transform comprehensions, notice that the groups are
parameterised over some arbitrary type ``n`` (provided it has an
``fmap``, as well as the comprehension being over an arbitrary monad.


