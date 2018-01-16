.. _utils:

Other Haskell utility programs
==============================

.. index::
   single: utilities, Haskell

This section describes other program(s) which we distribute, that help
with the Great Haskell Programming Task.

.. _happy:

"Yacc for Haskell": ``happy``
-----------------------------

.. index::
   single: Happy
   single: Yacc for Haskell
   single: parser generator for Haskell
   single: happy parser generator

Andy Gill and Simon Marlow have written a parser-generator for Haskell,
called ``happy``. ``Happy`` is to Haskell what ``Yacc`` is to C.

You can get ``happy`` from `the Happy
Homepage <http://www.haskell.org/happy/>`__.

``Happy`` is at its shining best when compiled by GHC.

.. _hsc2hs:

Writing Haskell interfaces to C code: ``hsc2hs``
------------------------------------------------

.. index::
   single: hsc2hs

The ``hsc2hs`` command can be used to automate some parts of the process
of writing Haskell bindings to C code. It reads an almost-Haskell source
with embedded special constructs, and outputs a real Haskell file with
these constructs processed, based on information taken from some C
headers. The extra constructs deal with accessing C data from Haskell.

It may also output a C file which contains additional C functions to be
linked into the program, together with a C header that gets included
into the C code to which the Haskell module will be compiled (when
compiled via C) and into the C file. These two files are created when
the ``#def`` construct is used (see below).

Actually ``hsc2hs`` does not output the Haskell file directly. It
creates a C program that includes the headers, gets automatically
compiled and run. That program outputs the Haskell code.

In the following, "Haskell file" is the main output (usually a ``.hs``
file), "compiled Haskell file" is the Haskell file after ``ghc`` has
compiled it to C (i.e. a ``.hc`` file), "C program" is the program that
outputs the Haskell file, "C file" is the optionally generated C file,
and "C header" is its header file.

command line syntax
~~~~~~~~~~~~~~~~~~~

``hsc2hs`` takes input files as arguments, and flags that modify its
behavior:

``-o FILE``, ``--output=FILE``
    Name of the Haskell file.

``-t FILE``, ``--template=FILE``
    The template file (see below).

``-c PROG``, ``--cc=PROG``
    The C compiler to use (default: ``gcc``)

``-l PROG``, ``--ld=PROG``
    The linker to use (default: ``gcc``).

``-C FLAG``, ``--cflag=FLAG``
    An extra flag to pass to the C compiler.

``-I DIR``
    Passed to the C compiler.

``-L FLAG``, ``--lflag=FLAG``
    An extra flag to pass to the linker.

``-i FILE``, ``--include=FILE``
    As if the appropriate ``#include`` directive was placed in the
    source.

``-D NAME[=VALUE]``, ``--define=NAME[=VALUE]``
    As if the appropriate ``#define`` directive was placed in the
    source.

``--no-compile``
    Stop after writing out the intermediate C program to disk. The file
    name for the intermediate C program is the input file name with
    ``.hsc`` replaced with ``_hsc_make.c``.

``-k``, ``--keep-files``
    Proceed as normal, but do not delete any intermediate files.

``-x``, ``--cross-compile``
    Activate cross-compilation mode (see :ref:`hsc2hs_cross`).

``--cross-safe``
    Restrict the .hsc directives to those supported by the
    ``--cross-compile`` mode (see :ref:`hsc2hs_cross`). This should be
    useful if your ``.hsc`` files must be safely cross-compiled and you
    wish to keep non-cross-compilable constructs from creeping into
    them.

``-?``, ``--help``
    Display a summary of the available flags and exit successfully.

``-V``, ``--version``
    Output version information and exit successfully.

The input file should end with .hsc (it should be plain Haskell source
only; literate Haskell is not supported at the moment). Output files by
default get names with the ``.hsc`` suffix replaced:

+--------------+----------------+
| ``.hs``      | Haskell file   |
+--------------+----------------+
| ``_hsc.h``   | C header       |
+--------------+----------------+
| ``_hsc.c``   | C file         |
+--------------+----------------+

The C program is compiled using the Haskell compiler. This provides the
include path to ``HsFFI.h`` which is automatically included into the C
program.

Input syntax
~~~~~~~~~~~~

All special processing is triggered by the ``#`` operator. To output a
literal ``#``, write it twice: ``##``. Inside string literals and
comments ``#`` characters are not processed.

A ``#`` is followed by optional spaces and tabs, an alphanumeric keyword
that describes the kind of processing, and its arguments. Arguments look
like C expressions separated by commas (they are not written inside
parens). They extend up to the nearest unmatched ``)``, ``]`` or ``}``,
or to the end of line if it occurs outside any ``() [] {} '' "" /**/``
and is not preceded by a backslash. Backslash-newline pairs are
stripped.

In addition ``#{stuff}`` is equivalent to ``#stuff`` except that it's
self-delimited and thus needs not to be placed at the end of line or in
some brackets.

Meanings of specific keywords:

``#include <file.h>``, ``#include "file.h"``
    The specified file gets included into the C program, the compiled
    Haskell file, and the C header. ``<HsFFI.h>`` is included
    automatically.

``#define ⟨name⟩``, ``#define ⟨name ⟨value⟩``, ``#undef ⟨name⟩``
    Similar to ``#include``. Note that ``#includes`` and ``#defines``
    may be put in the same file twice so they should not assume
    otherwise.

``#let ⟨name⟩ ⟨parameters⟩ = "⟨definition⟩"``
    Defines a macro to be applied to the Haskell source. Parameter names
    are comma-separated, not inside parens. Such macro is invoked as
    other ``#``-constructs, starting with ``#name``. The definition will
    be put in the C program inside parens as arguments of ``printf``. To
    refer to a parameter, close the quote, put a parameter name and open
    the quote again, to let C string literals concatenate. Or use
    ``printf``'s format directives. Values of arguments must be given as
    strings, unless the macro stringifies them itself using the C
    preprocessor's ``#parameter`` syntax.

``#def ⟨C_definition⟩``
    The definition (of a function, variable, struct or typedef) is
    written to the C file, and its prototype or extern declaration to
    the C header. Inline functions are handled correctly. struct
    definitions and typedefs are written to the C program too. The
    ``inline``, ``struct`` or ``typedef`` keyword must come just after
    ``def``.

``#if ⟨condition⟩``, ``#ifdef ⟨name⟩``, ``#ifndef ⟨name⟩``, ``#elif ⟨condition⟩``, ``#else``, ``#endif``, ``#error ⟨message⟩``, ``#warning ⟨message⟩``
    Conditional compilation directives are passed unmodified to the C
    program, C file, and C header. Putting them in the C program means
    that appropriate parts of the Haskell file will be skipped.

``#const ⟨C_expression⟩``
    The expression must be convertible to ``long`` or ``unsigned long``.
    Its value (literal or negated literal) will be output.

``#const_str ⟨C_expression⟩``
    The expression must be convertible to const char pointer. Its value
    (string literal) will be output.

``#type ⟨C_type⟩``
    A Haskell equivalent of the C numeric type will be output. It will
    be one of ``{Int,Word}{8,16,32,64}``, ``Float``, ``Double``,
    ``LDouble``.

``#peek ⟨struct_type⟩, ⟨field⟩``
    A function that peeks a field of a C struct will be output. It will
    have the type ``Storable b => Ptr a -> IO b``. The intention is that
    ``#peek`` and ``#poke`` can be used for implementing the operations
    of class ``Storable`` for a given C struct (see the
    ``Foreign.Storable`` module in the library documentation).

``#poke ⟨struct_type⟩, ⟨field⟩``
    Similarly for poke. It will have the type
    ``Storable b => Ptr a -> b -> IO ()``.

``#ptr ⟨struct_type⟩, ⟨field⟩``
    Makes a pointer to a field struct. It will have the type
    ``Ptr a -> Ptr b``.

``#offset ⟨struct_type⟩, ⟨field⟩``
    Computes the offset, in bytes, of ``field`` in ``struct_type``. It
    will have type ``Int``.

``#size ⟨struct_type⟩``
    Computes the size, in bytes, of ``struct_type``. It will have type
    ``Int``.

``#alignment ⟨struct_type⟩``
    Computes the alignment, in bytes, of ``struct_type``. It will have type
    ``Int``.

``#enum ⟨type⟩, ⟨constructor⟩, ⟨value⟩, ⟨value⟩, ...``
    A shortcut for multiple definitions which use ``#const``. Each
    ``value`` is a name of a C integer constant, e.g. enumeration value.
    The name will be translated to Haskell by making each letter
    following an underscore uppercase, making all the rest lowercase,
    and removing underscores. You can supply a different translation by
    writing ``hs_name = c_value`` instead of a ``value``, in which case
    ``c_value`` may be an arbitrary expression. The ``hs_name`` will be
    defined as having the specified ``type``. Its definition is the
    specified ``constructor`` (which in fact may be an expression or be
    empty) applied to the appropriate integer value. You can have
    multiple ``#enum`` definitions with the same ``type``; this
    construct does not emit the type definition itself.

Custom constructs
~~~~~~~~~~~~~~~~~

``#const``, ``#type``, ``#peek``, ``#poke`` and ``#ptr`` are not
hardwired into the ``hsc2hs``, but are defined in a C template that is
included in the C program: ``template-hsc.h``. Custom constructs and
templates can be used too. Any ``#``\-construct with unknown key is
expected to be handled by a C template.

A C template should define a macro or function with name prefixed by
``hsc_`` that handles the construct by emitting the expansion to stdout.
See ``template-hsc.h`` for examples.

Such macros can also be defined directly in the source. They are useful
for making a ``#let``\-like macro whose expansion uses other ``#let``
macros. Plain ``#let`` prepends ``hsc_`` to the macro name and wraps the
definition in a ``printf`` call.

.. _hsc2hs_cross:

Cross-compilation
~~~~~~~~~~~~~~~~~

``hsc2hs`` normally operates by creating, compiling, and running a C
program. That approach doesn't work when cross-compiling — in this
case, the C compiler's generates code for the target machine, not the
host machine. For this situation, there's a special mode
``hsc2hs --cross-compile`` which can generate the .hs by extracting
information from compilations only — specifically, whether or not
compilation fails.

Only a subset of ``.hsc`` syntax is supported by ``--cross-compile``.
The following are unsupported:

-  ``#{const_str}``
-  ``#{let}``
-  ``#{def}``
-  Custom constructs
