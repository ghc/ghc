.. _applicative-do:

Applicative do-notation
-----------------------

.. index::
   single: Applicative do-notation
   single: do-notation; Applicative

.. extension:: ApplicativeDo
    :shortdesc: Enable Applicative do-notation desugaring

    :since: 8.0.1

    Allow use of ``Applicative`` ``do`` notation.

The language option :extension:`ApplicativeDo` enables an alternative translation for
the do-notation, which uses the operators ``<$>``, ``<*>``, along with ``join``
as far as possible. There are two main reasons for wanting to do this:

-  We can use do-notation with types that are an instance of ``Applicative`` and
   ``Functor``, but not ``Monad``
-  In some monads, using the applicative operators is more efficient than monadic
   bind. For example, it may enable more parallelism.

Applicative do-notation desugaring preserves the original semantics, provided
that the ``Applicative`` instance satisfies ``<*> = ap`` and ``pure = return``
(these are true of all the common monadic types). Thus, you can normally turn on
:extension:`ApplicativeDo` without fear of breaking your program. There is one pitfall
to watch out for; see :ref:`applicative-do-pitfall`.

There are no syntactic changes with :extension:`ApplicativeDo`. The only way it shows
up at the source level is that you can have a ``do`` expression that doesn't
require a ``Monad`` constraint. For example, in GHCi: ::

    Prelude> :set -XApplicativeDo
    Prelude> :t \m -> do { x <- m; return (not x) }
    \m -> do { x <- m; return (not x) }
      :: Functor f => f Bool -> f Bool

This example only requires ``Functor``, because it is translated into ``(\x ->
not x) <$> m``. A more complex example requires ``Applicative``, ::

    Prelude> :t \m -> do { x <- m 'a'; y <- m 'b'; return (x || y) }
    \m -> do { x <- m 'a'; y <- m 'b'; return (x || y) }
      :: Applicative f => (Char -> f Bool) -> f Bool

Here GHC has translated the expression into ::

    (\x y -> x || y) <$> m 'a' <*> m 'b'

It is possible to see the actual translation by using :ghc-flag:`-ddump-ds`, but be
warned, the output is quite verbose.

Note that if the expression can't be translated into uses of ``<$>``, ``<*>``
only, then it will incur a ``Monad`` constraint as usual. This happens when
there is a dependency on a value produced by an earlier statement in the
``do``-block: ::

    Prelude> :t \m -> do { x <- m True; y <- m x; return (x || y) }
    \m -> do { x <- m True; y <- m x; return (x || y) }
      :: Monad m => (Bool -> m Bool) -> m Bool

Here, ``m x`` depends on the value of ``x`` produced by the first statement, so
the expression cannot be translated using ``<*>``.

In general, the rule for when a ``do`` statement incurs a ``Monad`` constraint
is as follows. If the do-expression has the following form: ::

    do p1 <- E1; ...; pn <- En; return E

where none of the variables defined by ``p1...pn`` are mentioned in ``E1...En``,
and ``p1...pn`` are all variables or lazy patterns,
then the expression will only require ``Applicative``. Otherwise, the expression
will require ``Monad``. The block may return a pure expression ``E`` depending
upon the results ``p1...pn`` with either ``return`` or ``pure``.

Note: the final statement must match one of these patterns exactly:

- ``return E``
- ``return $ E``
- ``pure E``
- ``pure $ E``

otherwise GHC cannot recognise it as a ``return`` statement, and the
transformation to use ``<$>`` that we saw above does not apply.  In
particular, slight variations such as ``return . Just $ x`` or ``let x
= e in return x`` would not be recognised.

If the final statement is not of one of these forms, GHC falls back to
standard ``do`` desugaring, and the expression will require a
``Monad`` constraint.

When the statements of a ``do`` expression have dependencies between
them, and ``ApplicativeDo`` cannot infer an ``Applicative`` type, it
uses a heuristic algorithm to try to use ``<*>`` as much as possible.
This algorithm usually finds the best solution, but in rare complex
cases it might miss an opportunity.  There is an algorithm that finds
the optimal solution, provided as an option:

.. ghc-flag:: -foptimal-applicative-do
    :shortdesc: Use a slower but better algorithm for ApplicativeDo
    :type: dynamic
    :reverse: -fno-optimal-applicative-do
    :category: optimization

    :since: 8.0.1

    Enables an alternative algorithm for choosing where to use ``<*>``
    in conjunction with the ``ApplicativeDo`` language extension.
    This algorithm always finds the optimal solution, but it is
    expensive: ``O(n^3)``, so this option can lead to long compile
    times when there are very large ``do`` expressions (over 100
    statements).  The default ``ApplicativeDo`` algorithm is ``O(n^2)``.


.. _applicative-do-strict:

Strict patterns
~~~~~~~~~~~~~~~


A strict pattern match in a bind statement prevents
``ApplicativeDo`` from transforming that statement to use
``Applicative``.  This is because the transformation would change the
semantics by making the expression lazier.

For example, this code will require a ``Monad`` constraint::

    > :t \m -> do { (x:xs) <- m; return x }
    \m -> do { (x:xs) <- m; return x } :: Monad m => m [b] -> m b

but making the pattern match lazy allows it to have a ``Functor`` constraint::

    > :t \m -> do { ~(x:xs) <- m; return x }
    \m -> do { ~(x:xs) <- m; return x } :: Functor f => f [b] -> f b

A "strict pattern match" is any pattern match that can fail.  For
example, ``()``, ``(x:xs)``, ``!z``, and ``C x`` are strict patterns,
but ``x`` and ``~(1,2)`` are not.  For the purposes of
``ApplicativeDo``, a pattern match against a ``newtype`` constructor
is considered strict.

When there's a strict pattern match in a sequence of statements,
``ApplicativeDo`` places a ``>>=`` between that statement and the one
that follows it.  The sequence may be transformed to use ``<*>``
elsewhere, but the strict pattern match and the following statement
will always be connected with ``>>=``, to retain the same strictness
semantics as the standard do-notation.  If you don't want this, simply
put a ``~`` on the pattern match to make it lazy.

.. _applicative-do-pitfall:

Things to watch out for
~~~~~~~~~~~~~~~~~~~~~~~

Your code should just work as before when :extension:`ApplicativeDo` is enabled,
provided you use conventional ``Applicative`` instances. However, if you define
a ``Functor`` or ``Applicative`` instance using do-notation, then it will likely
get turned into an infinite loop by GHC. For example, if you do this: ::

    instance Functor MyType where
        fmap f m = do x <- m; return (f x)

Then applicative desugaring will turn it into ::

    instance Functor MyType where
        fmap f m = fmap (\x -> f x) m

And the program will loop at runtime. Similarly, an ``Applicative`` instance
like this ::

    instance Applicative MyType where
        pure = return
        x <*> y = do f <- x; a <- y; return (f a)

will result in an infinite loop when ``<*>`` is called.

Just as you wouldn't define a ``Monad`` instance using the do-notation, you
shouldn't define ``Functor`` or ``Applicative`` instance using do-notation (when
using ``ApplicativeDo``) either. The correct way to define these instances in
terms of ``Monad`` is to use the ``Monad`` operations directly, e.g. ::

    instance Functor MyType where
        fmap f m = m >>= return . f

    instance Applicative MyType where
        pure = return
        (<*>) = ap



