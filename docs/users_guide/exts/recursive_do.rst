.. _recursive-do-notation:

The recursive do-notation
-------------------------

.. extension:: RecursiveDo
    :shortdesc: Enable recursive do (mdo) notation.

    :since: 6.8.1

    Allow the use of recursive ``do`` notation.

The do-notation of Haskell 98 does not allow *recursive bindings*, that
is, the variables bound in a do-expression are visible only in the
textually following code block. Compare this to a let-expression, where
bound variables are visible in the entire binding group.

It turns out that such recursive bindings do indeed make sense for a
variety of monads, but not all. In particular, recursion in this sense
requires a fixed-point operator for the underlying monad, captured by
the ``mfix`` method of the ``MonadFix`` class, defined in
``Control.Monad.Fix`` as follows: ::

    class Monad m => MonadFix m where
       mfix :: (a -> m a) -> m a

Haskell's ``Maybe``, ``[]`` (list), ``ST`` (both strict and lazy
versions), ``IO``, and many other monads have ``MonadFix`` instances. On
the negative side, the continuation monad, with the signature
``(a -> r) -> r``, does not.

For monads that do belong to the ``MonadFix`` class, GHC provides an
extended version of the do-notation that allows recursive bindings. The
:extension:`RecursiveDo` (language pragma: ``RecursiveDo``) provides the
necessary syntactic support, introducing the keywords ``mdo`` and
``rec`` for higher and lower levels of the notation respectively. Unlike
bindings in a ``do`` expression, those introduced by ``mdo`` and ``rec``
are recursively defined, much like in an ordinary let-expression. Due to
the new keyword ``mdo``, we also call this notation the *mdo-notation*.

Here is a simple (albeit contrived) example:

::

    {-# LANGUAGE RecursiveDo #-}
    justOnes = mdo { xs <- Just (1:xs)
                   ; return (map negate xs) }

or equivalently

::

    {-# LANGUAGE RecursiveDo #-}
    justOnes = do { rec { xs <- Just (1:xs) }
                  ; return (map negate xs) }

As you can guess ``justOnes`` will evaluate to ``Just [-1,-1,-1,...``.

GHC's implementation the mdo-notation closely follows the original
translation as described in the paper `A recursive do for
Haskell <http://leventerkok.github.io/papers/recdo.pdf>`__, which
in turn is based on the work `Value Recursion in Monadic
Computations <http://leventerkok.github.io/papers/erkok-thesis.pdf>`__.
Furthermore, GHC extends the syntax described in the former paper with a
lower level syntax flagged by the ``rec`` keyword, as we describe next.

Recursive binding groups
~~~~~~~~~~~~~~~~~~~~~~~~

The extension :extension:`RecursiveDo` also introduces a new keyword ``rec``, which
wraps a mutually-recursive group of monadic statements inside a ``do``
expression, producing a single statement. Similar to a ``let`` statement
inside a ``do``, variables bound in the ``rec`` are visible throughout
the ``rec`` group, and below it. For example, compare

::

        do { a <- getChar            do { a <- getChar
           ; let { r1 = f a r2          ; rec { r1 <- f a r2
           ;     ; r2 = g r1 }          ;     ; r2 <- g r1 }
           ; return (r1 ++ r2) }        ; return (r1 ++ r2) }

In both cases, ``r1`` and ``r2`` are available both throughout the
``let`` or ``rec`` block, and in the statements that follow it. The
difference is that ``let`` is non-monadic, while ``rec`` is monadic. (In
Haskell ``let`` is really ``letrec``, of course.)

The semantics of ``rec`` is fairly straightforward. Whenever GHC finds a
``rec`` group, it will compute its set of bound variables, and will
introduce an appropriate call to the underlying monadic value-recursion
operator ``mfix``, belonging to the ``MonadFix`` class. Here is an
example:

::

    rec { b <- f a c     ===>    (b,c) <- mfix (\ ~(b,c) -> do { b <- f a c
        ; c <- f b a }                                         ; c <- f b a
                                                               ; return (b,c) })

As usual, the meta-variables ``b``, ``c`` etc., can be arbitrary
patterns. In general, the statement ``rec ss`` is desugared to the
statement

::

    vs <- mfix (\ ~vs -> do { ss; return vs })

where ``vs`` is a tuple of the variables bound by ``ss``.

Note in particular that the translation for a ``rec`` block only
involves wrapping a call to ``mfix``: it performs no other analysis on
the bindings. The latter is the task for the ``mdo`` notation, which is
described next.

The ``mdo`` notation
~~~~~~~~~~~~~~~~~~~~

A ``rec``-block tells the compiler where precisely the recursive knot
should be tied. It turns out that the placement of the recursive knots
can be rather delicate: in particular, we would like the knots to be
wrapped around as minimal groups as possible. This process is known as
*segmentation*, and is described in detail in Section 3.2 of `A
recursive do for
Haskell <http://leventerkok.github.io/papers/recdo.pdf>`__.
Segmentation improves polymorphism and reduces the size of the recursive
knot. Most importantly, it avoids unnecessary interference caused by a
fundamental issue with the so-called *right-shrinking* axiom for monadic
recursion. In brief, most monads of interest (IO, strict state, etc.) do
*not* have recursion operators that satisfy this axiom, and thus not
performing segmentation can cause unnecessary interference, changing the
termination behavior of the resulting translation. (Details can be found
in Sections 3.1 and 7.2.2 of `Value Recursion in Monadic
Computations <http://leventerkok.github.io/papers/erkok-thesis.pdf>`__.)

The ``mdo`` notation removes the burden of placing explicit ``rec``
blocks in the code. Unlike an ordinary ``do`` expression, in which
variables bound by statements are only in scope for later statements,
variables bound in an ``mdo`` expression are in scope for all statements
of the expression. The compiler then automatically identifies minimal
mutually recursively dependent segments of statements, treating them as
if the user had wrapped a ``rec`` qualifier around them.

The definition is syntactic:

-  A generator ⟨g⟩ *depends* on a textually following generator ⟨g'⟩, if

   -  ⟨g'⟩ defines a variable that is used by ⟨g⟩, or

   -  ⟨g'⟩ textually appears between ⟨g⟩ and ⟨g''⟩, where ⟨g⟩ depends on
      ⟨g''⟩.

-  A *segment* of a given ``mdo``-expression is a minimal sequence of
   generators such that no generator of the sequence depends on an
   outside generator. As a special case, although it is not a generator,
   the final expression in an ``mdo``-expression is considered to form a
   segment by itself.

Segments in this sense are related to *strongly-connected components*
analysis, with the exception that bindings in a segment cannot be
reordered and must be contiguous.

Here is an example ``mdo``-expression, and its translation to ``rec``
blocks:

::

    mdo { a <- getChar      ===> do { a <- getChar
        ; b <- f a c                ; rec { b <- f a c
        ; c <- f b a                ;     ; c <- f b a }
        ; z <- h a b                ; z <- h a b
        ; d <- g d e                ; rec { d <- g d e
        ; e <- g a z                ;     ; e <- g a z }
        ; putChar c }               ; putChar c }

Note that a given ``mdo`` expression can cause the creation of multiple
``rec`` blocks. If there are no recursive dependencies, ``mdo`` will
introduce no ``rec`` blocks. In this latter case an ``mdo`` expression
is precisely the same as a ``do`` expression, as one would expect.

In summary, given an ``mdo`` expression, GHC first performs
segmentation, introducing ``rec`` blocks to wrap over minimal recursive
groups. Then, each resulting ``rec`` is desugared, using a call to
``Control.Monad.Fix.mfix`` as described in the previous section. The
original ``mdo``-expression typechecks exactly when the desugared
version would do so.

Here are some other important points in using the recursive-do notation:

-  It is enabled with the extension :extension:`RecursiveDo`, or the
   ``LANGUAGE RecursiveDo`` pragma. (The same extension enables both
   ``mdo``-notation, and the use of ``rec`` blocks inside ``do``
   expressions.)

-  ``rec`` blocks can also be used inside ``mdo``-expressions, which
   will be treated as a single statement. However, it is good style to
   either use ``mdo`` or ``rec`` blocks in a single expression.

-  If recursive bindings are required for a monad, then that monad must
   be declared an instance of the ``MonadFix`` class.

-  The following instances of ``MonadFix`` are automatically provided:
   List, Maybe, IO. Furthermore, the ``Control.Monad.ST`` and
   ``Control.Monad.ST.Lazy`` modules provide the instances of the
   ``MonadFix`` class for Haskell's internal state monad (strict and
   lazy, respectively).

-  Like ``let`` and ``where`` bindings, name shadowing is not allowed
   within an ``mdo``-expression or a ``rec``-block; that is, all the
   names bound in a single ``rec`` must be distinct. (GHC will complain
   if this is not the case.)


