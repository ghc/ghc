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

.. flag-print::
    :type: summary
    :category: codegen


.. flag-print::
    :type: summary
    :category: debugging


.. flag-print::
    :type: summary
    :category: cpp


.. flag-print::
    :type: summary
    :category: search-path


.. flag-print::
    :type: summary
    :category: interactive


.. flag-print::
    :type: summary
    :category: interface-files


.. flag-print::
    :type: summary
    :category: keep-intermediates


.. flag-print::
    :type: summary
    :category: language


.. flag-print::
    :type: summary
    :category: linking


.. flag-print::
    :type: summary
    :category: misc


.. flag-print::
    :type: summary
    :category: modes


.. flag-print::
    :type: summary
    :category: optimization


.. flag-print::
    :type: summary
    :category: optimization-levels


.. flag-print::
    :type: summary
    :category: packages


.. flag-print::
    :type: summary
    :category: phases


.. flag-print::
    :type: summary
    :category: phase-programs


.. flag-print::
    :type: summary
    :category: phase-options


.. flag-print::
    :type: summary
    :category: platform-options


.. flag-print::
    :type: summary
    :category: plugins


.. flag-print::
    :type: summary
    :category: profiling


.. flag-print::
    :type: summary
    :category: coverage


.. flag-print::
    :type: summary
    :category: recompilation


.. flag-print::
    :type: summary
    :category: redirect-output


.. flag-print::
    :type: summary
    :category: temp-files


.. flag-print::
    :type: summary
    :category: verbosity


.. flag-print::
    :type: summary
    :category: warnings


Code generation
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: codegen

Debugging the compiler
~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: debugging

C pre-processor
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: cpp

Finding imports
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: search-path

Interactive mode
~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: interactive

Interface files
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: interface-files

Keeping intermediate files
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: keep-intermediates

Language options
~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: language

Linking options
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: linking

Miscellaneous options
~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: misc

Modes of operation
~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: modes

Individual optimizations
~~~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: optimization

Optimization levels
~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: optimization-levels

Package options
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: packages

Phases of compilation
~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: phases

Overriding external programs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: phase-programs

Phase-specific options
~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: phase-options

Platform-specific options
~~~~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: platform-options

Compiler plugins
~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: plugins

Profiling
~~~~~~~~~

.. flag-print::
    :type: list
    :category: profiling

Program coverage
~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: coverage

Recompilation checking
~~~~~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: recompilation

Redirecting output
~~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: redirect-output

Temporary files
~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: temp-files

Verbosity options
~~~~~~~~~~~~~~~~~

.. flag-print::
    :type: list
    :category: verbosity

Warnings
~~~~~~~~

.. flag-print::
    :type: list
    :category: warnings

Copyright
---------

Copyright 2015. The University Court of the University of Glasgow.
All rights reserved.
