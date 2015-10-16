.. _runghc:

Using runghc
============

.. index::
   single: runghc

``runghc`` allows you to run Haskell programs without first having to
compile them.

.. _runghc-introduction:

Flags
-----

The ``runghc`` command-line looks like:

::

    runghc [runghc flags] [GHC flags] module [program args]

The runghc flags are ``-f /path/to/ghc``, which tells runghc which GHC
to use to run the program, and ``--help``, which prints usage
information. If it is not given then runghc will search for GHC in the
directories in the system search path.

runghc will try to work out where the boundaries between
``[runghc flags]`` and ``[GHC flags]``, and ``[program args]`` and
``module`` are, but you can use a ``--`` flag if it doesn't get it
right. For example, ``runghc -- -fwarn-unused-bindings Foo`` means
runghc won't try to use ``warn-unused-bindings`` as the path to GHC, but
instead will pass the flag to GHC. If a GHC flag doesn't start with a
dash then you need to prefix it with ``--ghc-arg=`` or runghc will think
that it is the program to run, e.g.
``runghc -package-db --ghc-arg=foo.conf Main.hs``.
