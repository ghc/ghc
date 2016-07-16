.. _options-debugging:

Debugging the compiler
======================

.. index::
   single: debugging options (for GHC)

HACKER TERRITORY. HACKER TERRITORY. (You were warned.)

.. _dumping-output:

Dumping out compiler intermediate structures
--------------------------------------------

.. index::
   single: dumping GHC intermediates
   single: intermediate passes, output

``-ddump-`` ⟨pass⟩
    .. index::
       single: -ddump options

    Make a debugging dump after pass ``<pass>`` (may be common enough to
    need a short form…). You can get all of these at once (*lots* of
    output) by using ``-v5``, or most of them with ``-v4``. You can
    prevent them from clogging up your standard output by passing
    :ghc-flag:`-ddump-to-file`. Some of the most useful ones are:

    .. ghc-flag:: -ddump-to-file

        Causes the output from all of the flags listed below to be dumped
        to a file. The file name depends upon the output produced; for instance,
        output from :ghc-flag:`-ddump-simpl` will end up in
        :file:`{module}.dump-simpl`.

    .. ghc-flag:: -ddump-parsed

        Dump parser output

    .. ghc-flag:: -ddump-rn

        Dump renamer output

    .. ghc-flag:: -ddump-tc

        Dump typechecker output

    .. ghc-flag:: -ddump-splices

        Dump Template Haskell expressions that we splice in, and what
        Haskell code the expression evaluates to.

    .. ghc-flag:: -dth-dec-file=<file>

        Dump expansions of all top-level Template Haskell splices into ⟨file⟩.

    .. ghc-flag:: -ddump-types

        Dump a type signature for each value defined at the top level of
        the module. The list is sorted alphabetically. Using
        :ghc-flag:`-dppr-debug` dumps a type signature for all the imported and
        system-defined things as well; useful for debugging the
        compiler.

    .. ghc-flag:: -ddump-deriv

        Dump derived instances

    .. ghc-flag:: -ddump-ds

        Dump desugarer output

    .. ghc-flag:: -ddump-spec

        Dump output of specialisation pass

    .. ghc-flag:: -ddump-rules

        Dumps all rewrite rules specified in this module; see
        :ref:`controlling-rules`.

    .. ghc-flag:: -ddump-rule-firings

        Dumps the names of all rules that fired in this module

    .. ghc-flag:: -ddump-rule-rewrites

        Dumps detailed information about all rules that fired in this
        module

    .. ghc-flag:: -ddump-vect

        Dumps the output of the vectoriser.

    .. ghc-flag:: -ddump-simpl

        Dump simplifier output (Core-to-Core passes)

    .. ghc-flag:: -ddump-inlinings

        Dumps inlining info from the simplifier

    .. ghc-flag:: -ddump-stranal

        Dump strictness analyser output

    .. ghc-flag:: -ddump-str-signatures

        Dump strictness signatures

    .. ghc-flag:: -ddump-cse

        Dump common subexpression elimination (CSE) pass output

    .. ghc-flag:: -ddump-worker-wrapper

        Dump worker/wrapper split output

    .. ghc-flag:: -ddump-occur-anal

        Dump "occurrence analysis" output

    .. ghc-flag:: -ddump-prep

        Dump output of Core preparation pass

    .. ghc-flag:: -ddump-stg

        Dump output of STG-to-STG passes

    .. ghc-flag:: -ddump-cmm

        Dump the result of the C-- pipeline processing

    .. ghc-flag:: -ddump-cmm-from-stg

        Dump the result of STG-to-C-- conversion

    .. ghc-flag:: -ddump-cmm-verbose

        Dump output from all C-- pipeline stages. In case of
        ``.cmm`` compilation this also dumps the result of
        file parsing.

    .. ghc-flag:: -ddump-opt-cmm

        Dump the results of C-- to C-- optimising passes.

    .. ghc-flag:: -ddump-asm

        Dump assembly language produced by the :ref:`native code
        generator <native-code-gen>`

    .. ghc-flag:: -ddump-llvm

        :implies: :ghc-flag:`-fllvm`

        LLVM code from the :ref:`LLVM code generator <llvm-code-gen>`

    .. ghc-flag:: -ddump-bcos

        Dump byte-code compiler output

    .. ghc-flag:: -ddump-foreign

        dump foreign export stubs

.. ghc-flag:: -ddump-simpl-iterations

    Show the output of each *iteration* of the simplifier (each run of
    the simplifier has a maximum number of iterations, normally 4). This
    outputs even more information than ``-ddump-simpl-phases``.

.. ghc-flag:: -ddump-simpl-stats

    Dump statistics about how many of each kind of transformation too
    place. If you add ``-dppr-debug`` you get more detailed information.

.. ghc-flag:: -ddump-if-trace

    Make the interface loader be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-tc-trace

    Make the type checker be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-vt-trace

    Make the vectoriser be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-rn-trace

    Make the renamer be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-rn-stats

    Print out summary of what kind of information the renamer had to
    bring in.

.. ghc-flag:: -dverbose-core2core
              -dverbose-stg2stg

    Show the output of the intermediate Core-to-Core and STG-to-STG
    passes, respectively. (*lots* of output!) So: when we're really
    desperate:

    .. code-block:: sh

        % ghc -noC -O -ddump-simpl -dverbose-core2core -dcore-lint Foo.hs

.. ghc-flag:: -dshow-passes

    Print out each pass name, its runtime and heap allocations as it happens.
    Note that this may come at a slight performance cost as the compiler will
    be a bit more eager in forcing pass results to more accurately account for
    their costs.

    Two types of messages are produced: Those beginning with ``***`` are
    denote the beginning of a compilation phase whereas those starting with
    ``!!!`` mark the end of a pass and are accompanied by allocation and
    runtime statistics.

.. ghc-flag:: -ddump-core-stats

    Print a one-line summary of the size of the Core program at the end
    of the optimisation pipeline.

.. ghc-flag:: -dfaststring-stats

    Show statistics on the usage of fast strings by the compiler.

.. ghc-flag:: -dppr-debug

    Debugging output is in one of several "styles." Take the printing of
    types, for example. In the "user" style (the default), the
    compiler's internal ideas about types are presented in Haskell
    source-level syntax, insofar as possible. In the "debug" style
    (which is the default for debugging output), the types are printed
    in with explicit foralls, and variables have their unique-id
    attached (so you can check for things that look the same but
    aren't). This flag makes debugging output appear in the more verbose
    debug style.

.. _formatting dumps:

Formatting dumps
----------------

.. index::
   single: formatting dumps

.. ghc-flag:: -dppr-user-length

    In error messages, expressions are printed to a certain "depth",
    with subexpressions beyond the depth replaced by ellipses. This flag
    sets the depth. Its default value is 5.

.. ghc-flag:: -dppr-cols=N

    Set the width of debugging output. Use this if your code is wrapping
    too much. For example: ``-dppr-cols=200``.

.. ghc-flag:: -dppr-case-as-let

    Print single alternative case expressions as though they were strict
    let expressions. This is helpful when your code does a lot of
    unboxing.

.. ghc-flag:: -dno-debug-output

    Suppress any unsolicited debugging output. When GHC has been built
    with the ``DEBUG`` option it occasionally emits debug output of
    interest to developers. The extra output can confuse the testing
    framework and cause bogus test failures, so this flag is provided to
    turn it off.

.. _suppression:

Suppressing unwanted information
--------------------------------

.. index::
   single: suppression; of unwanted dump output

Core dumps contain a large amount of information. Depending on what you
are doing, not all of it will be useful. Use these flags to suppress the
parts that you are not interested in.

.. ghc-flag:: -dsuppress-all

    Suppress everything that can be suppressed, except for unique ids as
    this often makes the printout ambiguous. If you just want to see the
    overall structure of the code, then start here.

.. ghc-flag:: -dsuppress-uniques

    Suppress the printing of uniques. This may make the printout
    ambiguous (e.g. unclear where an occurrence of 'x' is bound), but it
    makes the output of two compiler runs have many fewer gratuitous
    differences, so you can realistically apply ``diff``. Once ``diff``
    has shown you where to look, you can try again without
    :ghc-flag:`-dsuppress-uniques`

.. ghc-flag:: -dsuppress-idinfo

    Suppress extended information about identifiers where they are
    bound. This includes strictness information and inliner templates.
    Using this flag can cut the size of the core dump in half, due to
    the lack of inliner templates

.. ghc-flag:: -dsuppress-unfoldings

    Suppress the printing of the stable unfolding of a variable at its
    binding site.

.. ghc-flag:: -dsuppress-module-prefixes

    Suppress the printing of module qualification prefixes. This is the
    ``Data.List`` in ``Data.List.length``.

.. ghc-flag:: -dsuppress-type-signatures

    Suppress the printing of type signatures.

.. ghc-flag:: -dsuppress-type-applications

    Suppress the printing of type applications.

.. ghc-flag:: -dsuppress-coercions

    Suppress the printing of type coercions.

.. _checking-consistency:

Checking for consistency
------------------------

.. index::
   single: consistency checks
   single: lint

.. ghc-flag:: -dcore-lint

    Turn on heavyweight intra-pass sanity-checking within GHC, at Core
    level. (It checks GHC's sanity, not yours.)

.. ghc-flag:: -dstg-lint

    Ditto for STG level. (note: currently doesn't work).

.. ghc-flag:: -dcmm-lint

    Ditto for C-- level.

.. ghc-flag:: -fllvm-fill-undef-with-garbage

    Instructs the LLVM code generator to fill dead STG registers with garbage
    instead of ``undef`` in calls. This makes it easier to catch subtle
    code generator and runtime system bugs (e.g. see :ghc-ticket:`11487`).

.. _checking-determinism:

Checking for determinism
------------------------

.. index::
   single: deterministic builds

.. ghc-flag:: -dinitial-unique=⟨s⟩

    Start ``UniqSupply`` allocation from ⟨s⟩.

.. ghc-flag:: -dunique-increment=⟨i⟩

    Set the increment for the generated ``Unique``'s to ⟨i⟩.

    This is useful in combination with :ghc-flag:`-dinitial-unique` to test if the
    generated files depend on the order of ``Unique``'s.

    Some interesting values:

    * ``-dinitial-unique=0 -dunique-increment=1`` - current sequential
      ``UniqSupply``
    * ``-dinitial-unique=16777215 -dunique-increment=-1`` - ``UniqSupply`` that
      generates in decreasing order
    * ``-dinitial-unique=1 -dunique-increment=PRIME`` - where PRIME big enough
      to overflow often - nonsequential order
