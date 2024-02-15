Introduction
============

GHC implements several variants of the Haskell language, along with many
extensions. They can all be enabled or disabled by command line flags or
:pragma:`LANGUAGE` pragmas.

Some of the extensions serve to give you access to the
underlying facilities with which we implement Haskell. Thus, you can get
at the Raw Iron, if you are willing to write some non-portable code at a
more primitive level. You need not be “stuck” on performance because of
the implementation costs of Haskell's "high-level" features—you can
always code "under" them. In an extreme case, you can write all your
time-critical code in C, and then just glue it together with Haskell!


.. toctree::
    :maxdepth: 1

    control
    table
    stolen_syntax
