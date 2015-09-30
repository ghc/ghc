.. _lang-parallel:

Concurrent and Parallel Haskell
===============================

.. index::
   single: parallelism
   single: concurrency

GHC implements some major extensions to Haskell to support concurrent
and parallel programming. Let us first establish terminology:

-  *Parallelism* means running a Haskell program on multiple processors,
   with the goal of improving performance. Ideally, this should be done
   invisibly, and with no semantic changes.

-  *Concurrency* means implementing a program by using multiple
   I/O-performing threads. While a concurrent Haskell program *can* run
   on a parallel machine, the primary goal of using concurrency is not
   to gain performance, but rather because that is the simplest and most
   direct way to write the program. Since the threads perform I/O, the
   semantics of the program is necessarily non-deterministic.

GHC supports both concurrency and parallelism.

.. _concurrent-haskell:

Concurrent Haskell
------------------

Concurrent Haskell is the name given to GHC's concurrency extension. It
is enabled by default, so no special flags are required. The `Concurrent
Haskell
paper <https://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz>`__
is still an excellent resource, as is `Tackling the awkward
squad <http://research.microsoft.com/%7Esimonpj/papers/marktoberdorf/>`__.

To the programmer, Concurrent Haskell introduces no new language
constructs; rather, it appears simply as a library,
:base-ref:`Control.Concurrent <Control-Concurrent.html>`.
The functions exported by this library include:

-  Forking and killing threads.

-  Sleeping.

-  Synchronised mutable variables, called ``MVars``

-  Support for bound threads; see the paper `Extending the FFI with
   concurrency <http://research.microsoft.com/%7Esimonpj/Papers/conc-ffi/index.htm>`__.

Software Transactional Memory
-----------------------------

GHC now supports a new way to coordinate the activities of Concurrent
Haskell threads, called Software Transactional Memory (STM). The `STM
papers <http://research.microsoft.com/%7Esimonpj/papers/stm/index.htm>`__
are an excellent introduction to what STM is, and how to use it.

The main library you need to use is the `stm
library <http://hackage.haskell.org/package/stm>`__. The main features
supported are these:

-  Atomic blocks.

-  Transactional variables.

-  Operations for composing transactions: ``retry``, and ``orElse``.

-  Data invariants.

All these features are described in the papers mentioned earlier.

Parallel Haskell
----------------

.. index::
   single: SMP

GHC includes support for running Haskell programs in parallel on
symmetric, shared-memory multi-processor (SMP). By default GHC runs
your program on one processor; if you want it to run in parallel you
must link your program with the ``-threaded``, and run it with the RTS
``-N`` option; see :ref:`using-smp`). The runtime will schedule the
running Haskell threads among the available OS threads, running as many
in parallel as you specified with the ``-N`` RTS option.

Annotating pure code for parallelism
------------------------------------

Ordinary single-threaded Haskell programs will not benefit from enabling
SMP parallelism alone: you must expose parallelism to the compiler. One
way to do so is forking threads using Concurrent Haskell
(:ref:`concurrent-haskell`), but the simplest mechanism for extracting
parallelism from pure code is to use the ``par`` combinator, which is
closely related to (and often used with) ``seq``. Both of these are
available from the
`parallel library <http://hackage.haskell.org/package/parallel>`__:

::

    infixr 0 `par`
    infixr 1 `pseq`

    par  :: a -> b -> b
    pseq :: a -> b -> b

The expression ``(x `par` y)`` *sparks* the evaluation of ``x`` (to weak
head normal form) and returns ``y``. Sparks are queued for execution in
FIFO order, but are not executed immediately. If the runtime detects
that there is an idle CPU, then it may convert a spark into a real
thread, and run the new thread on the idle CPU. In this way the
available parallelism is spread amongst the real CPUs.

For example, consider the following parallel version of our old nemesis,
``nfib``:

::

    import Control.Parallel

    nfib :: Int -> Int
    nfib n | n <= 1 = 1
           | otherwise = par n1 (pseq n2 (n1 + n2 + 1))
                         where n1 = nfib (n-1)
                               n2 = nfib (n-2)

For values of ``n`` greater than 1, we use ``par`` to spark a thread to
evaluate ``nfib (n-1)``, and then we use ``pseq`` to force the parent
thread to evaluate ``nfib (n-2)`` before going on to add together these
two subexpressions. In this divide-and-conquer approach, we only spark a
new thread for one branch of the computation (leaving the parent to
evaluate the other branch). Also, we must use ``pseq`` to ensure that
the parent will evaluate ``n2`` *before* ``n1`` in the expression
``(n1 + n2 + 1)``. It is not sufficient to reorder the expression as
``(n2 + n1 + 1)``, because the compiler may not generate code to
evaluate the addends from left to right.

Note that we use ``pseq`` rather than ``seq``. The two are almost
equivalent, but differ in their runtime behaviour in a subtle way:
``seq`` can evaluate its arguments in either order, but ``pseq`` is
required to evaluate its first argument before its second, which makes
it more suitable for controlling the evaluation order in conjunction
with ``par``.

When using ``par``, the general rule of thumb is that the sparked
computation should be required at a later time, but not too soon. Also,
the sparked computation should not be too small, otherwise the cost of
forking it in parallel will be too large relative to the amount of
parallelism gained. Getting these factors right is tricky in practice.

It is possible to glean a little information about how well ``par`` is
working from the runtime statistics; see :ref:`rts-options-gc`.

More sophisticated combinators for expressing parallelism are available
from the ``Control.Parallel.Strategies`` module in the `parallel
package <http://hackage.haskell.org/package/parallel>`__. This module
builds functionality around ``par``, expressing more elaborate patterns
of parallel computation, such as parallel ``map``.

.. _dph:

Data Parallel Haskell
---------------------

GHC includes experimental support for Data Parallel Haskell (DPH). This
code is highly unstable and is only provided as a technology preview.
More information can be found on the corresponding
`DPH wiki page <http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell>`__.
