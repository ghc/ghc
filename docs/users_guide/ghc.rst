:orphan:

GHC
===

Synopsis
--------

::

    ghc [option|filename]
    ghci [option|filename]


Description
-----------

This manual page documents briefly the ``ghc`` and ``ghci`` commands. Note that
``ghci`` is not yet available on all architectures. Extensive documentation is
available in various other formats including PDF and HTML; see below.

Each of GHC's command line options is classified as either *static* or
*dynamic*. A static flag may only be specified on the command line, whereas a
dynamic flag may also be given in an ``OPTIONS`` pragma in a source file or
set from the GHCi command-line with ``:set`` .

As a rule of thumb, all the language options are dynamic, as are the
warning options and the debugging options.

The rest are static, with the notable exceptions of
``-v``, ``-cpp``, ``-fasm``, ``-fvia-C``, ``-fllvm``, and
``-#include``.
The OPTIONS sections lists the status of each flag.

Common suffixes of file names for Haskell are:

``.hs``
    Haskell source code; preprocess, compile

``.lhs``
    literate Haskell source; unlit, preprocess, compile

``.hi``
    Interface file; contains information about exported symbols

``.hc``
    intermediate C files

``.⟨way⟩_o``
    object files for "way" ⟨way⟩; common ways are:

    ``dyn``
        dynamically-linked
    ``p``
        built with profiling

``.⟨way⟩_hi``
    interface files for "way" ⟨way⟩; common ways are:

.. _options-ref:

Options
-------

.. include:: all-flags.gen.rst

Copyright
---------

Copyright 2015. The University Court of the University of Glasgow.
All rights reserved.
