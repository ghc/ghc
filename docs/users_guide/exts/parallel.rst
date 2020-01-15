.. _lang-parallel:

Parallel and Concurrent
=======================

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

.. toctree::
    :maxdepth: 1

    concurrent
    stm
    static_pointers
