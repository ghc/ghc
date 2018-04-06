.. _runghc:

Using runghc
============

.. index::
   single: runghc

``runghc`` allows you to run Haskell programs without first having to
compile them.

.. _runghc-introduction:

Usage
-----

The ``runghc`` command-line looks like:

.. code-block:: none

    runghc [runghc flags] [GHC flags] module [program args]

Any flags not recognized by runghc are automatically passed to GHC.
If a flag is recognized by both runghc and GHC but you want to
pass it to GHC then you can place it after a ``--`` separator. Flags after the
separator are treated as GHC only flags. Alternatively you can use the runghc
option ``--ghc-arg=<arg>`` to pass any flag or argument directly to GHC.

``module`` could be a Haskell source filename with or without the extension.
If for some reason the filename starts with a ``-`` you can use a second
``--`` to indicate the end of flags. Anything following a second
``--`` will be considered a program file or module name followed by its
arguments. For example:

- ``runghc -- -- -hello.hs``

runghc flags
------------

runghc accepts the following flags:

- ``-f /path/to/ghc``: tell runghc the path of GHC executable to use to run the program. By default runghc will search for GHC in the directories in the system search path.
- ``--ghc-arg=<arg>``: Pass an option or argument to GHC
- ``--help``: print usage information.
- ``--version``: print version information.

GHC Flags
---------

As discussed earlier, use ``--`` or ``--ghc-arg=<arg>`` to disambiguate GHC
flags when needed. For example, ``-f`` is recognized by runghc, therefore to
pass ``-fliberate-case`` to GHC use any of the following:

- ``runghc -- -fliberate-case``
- ``runghc --ghc-arg=-fliberate-case``

Note that any non-flag arguments are never passed to GHC. An unused non-flag
argument will be considered as the name of the program to run. If a GHC flag
takes an argument use ``--ghc-arg=<arg>`` to pass the argument to GHC.
For example, if you want to pass ``-package foo`` to GHC use any of the
following:

- ``runghc -package --ghc-arg=foo Main.hs``
- ``runghc --ghc-arg=-package --ghc-arg=foo Main.hs``
