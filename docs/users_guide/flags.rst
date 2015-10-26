.. _flag-reference:

Flag reference
==============

This section is a quick-reference for GHC's command-line flags. For each
flag, we also list its static/dynamic status (see
:ref:`static-dynamic-flags`), and the flag's opposite (if available).

Verbosity options
-----------------

More details in :ref:`options-help`

.. include:: flags-verbosity.gen.rst

Alternative modes of operation
------------------------------

More details in :ref:`modes`

.. include:: flags-modes.gen.rst

Which phases to run
-------------------

More details in :ref:`options-order`

.. include:: flags-phases.gen.rst

Redirecting output
------------------

More details in :ref:`options-output`

.. include:: flags-redirecting-output.gen.rst

Keeping intermediate files
--------------------------

More details in :ref:`keeping-intermediates`

.. include:: flags-keeping-intermediates.gen.rst

Temporary files
---------------

More details in :ref:`temp-files`

.. include:: flags-temporary-files.gen.rst

Finding imports
---------------

More details in :ref:`search-path`

.. include:: flags-finding-imports.gen.rst

Interface file options
----------------------

More details in :ref:`hi-options`

.. include:: flags-interface-files.gen.rst

Recompilation checking
----------------------

More details in :ref:`recomp`

.. include:: flags-recompilation-checking.gen.rst

.. _interactive-mode-options:

Interactive-mode options
------------------------

More details in :ref:`ghci-dot-files`

.. include:: flags-interactive.gen.rst

Packages
--------

More details in :ref:`packages`

.. include:: flags-packages.gen.rst

Language options
----------------

Language options can be enabled either by a command-line option
``-Xblah``, or by a ``{-# LANGUAGE blah #-}`` pragma in the file itself.
See :ref:`options-language`. Some options are enabled using ``-f*``
flags.

.. include:: flags-language.gen.rst

Warnings
--------

More details in :ref:`options-sanity`

.. include:: flags-warnings.gen.rst

Optimisation levels
-------------------

These options are described in more detail in :ref:`options-optimise`.

See :ref:`options-f-compact` for a list of optimisations enabled on
level 1 and level 2.

.. include:: flags-optimization-levels.gen.rst

.. _options-f-compact:

Individual optimisations
------------------------

These options are described in more detail in :ref:`options-f`. If a
flag is implied by ``-O`` then it is also implied by ``-O2`` (unless
flag description explicitly says otherwise). If a flag is implied by
``-O0`` only then the flag is not implied by ``-O`` and ``-O2``.

.. include:: flags-optimization.gen.rst

Profiling options
-----------------

More details in :ref:`profiling`

.. include:: flags-profiling.gen.rst

Program coverage options
------------------------

More details in :ref:`hpc`

.. include:: flags-program-coverage.gen.rst

C pre-processor options
-----------------------

More details in :ref:`c-pre-processor`

.. include:: flags-cpp.gen.rst

Code generation options
-----------------------

More details in :ref:`options-codegen`

.. include:: flags-codegen.gen.rst

Linking options
---------------

More details in :ref:`options-linker`

.. include:: flags-linking.gen.rst

Plugin options
--------------

More details in :ref:`compiler-plugins`

.. include:: flags-plugin.gen.rst

Replacing phases
----------------

More details in :ref:`replacing-phases`

.. include:: flags-phase-programs.gen.rst

.. index::
   single: -pgmL
   single: -pgmP
   single: -pgmc
   single: -pgmlo
   single: -pgmlc
   single: -pgma
   single: -pgml
   single: -pgmdll
   single: -pgmF

Forcing options to particular phases
------------------------------------

More details in :ref:`forcing-options-through`

.. include:: flags-phase-specific.gen.rst

Platform-specific options
-------------------------

More details in :ref:`options-platform`

.. include:: flags-platform-specific.gen.rst

Compiler debugging options
--------------------------

More details in :ref:`options-debugging`

.. include:: flags-compiler-debugging.gen.rst

Miscellaneous compiler options
------------------------------

.. include:: flags-misc.gen.rst
