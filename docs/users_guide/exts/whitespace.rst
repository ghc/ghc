.. _whitespace:

Whitespace
==========

.. index::
   single: Whitespace

As in the Haskell Language Report, Haskell comments are valid whitespace. In
addition, lines (which must end with a line feed character) that begin as
follows are valid whitespace in source code, except immediately after a
``where``, ``let``, ``do`` or ``of`` keyword:

-  ``#!``. This accommodates 'shebang' interpreter directives in scripts on
   Unix-like operating systems.

-  ``<space>#!``, where ``<space>`` is an initial space character before the
   'shebang'.

-  ``#pragma``. This accommodates the use of a directive that passes additional
   information to a compiler.

-  ``#line <line> "<file>"``, where ``<line>`` is a positive integer and
   ``<file>`` can comprise zero or more characters. This accommodates a compiler
   directive that resets the numbering of lines of source code, and the
   identification of the source code file name, in compiler messages.

-  ``#<line> "<file>"``, where ``<line>`` is a positive integer and ``<file>``
   can comprise zero or more characters.
