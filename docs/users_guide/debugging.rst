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
    ``-ddump-to-file``. Some of the most useful ones are:

    ``-ddump-parsed``
        .. index::
           single: -ddump-parsed

        Dump parser output

    ``-ddump-rn``
        .. index::
           single: -ddump-rn

        Dump renamer output

    ``-ddump-tc``
        .. index::
           single: -ddump-tc

        Dump typechecker output

    ``-ddump-splices``
        .. index::
           single: -ddump-splices

        Dump Template Haskell expressions that we splice in, and what
        Haskell code the expression evaluates to.

    ``-ddump-types``
        .. index::
           single: -ddump-types

        Dump a type signature for each value defined at the top level of
        the module. The list is sorted alphabetically. Using
        ``-dppr-debug`` dumps a type signature for all the imported and
        system-defined things as well; useful for debugging the
        compiler.

    ``-ddump-deriv``
        .. index::
           single: -ddump-deriv

        Dump derived instances

    ``-ddump-ds``
        .. index::
           single: -ddump-ds

        Dump desugarer output

    ``-ddump-spec``
        .. index::
           single: -ddump-spec

        Dump output of specialisation pass

    ``-ddump-rules``
        .. index::
           single: -ddump-rules

        Dumps all rewrite rules specified in this module; see
        :ref:`controlling-rules`.

    ``-ddump-rule-firings``
        .. index::
           single: -ddump-rule-firings

        Dumps the names of all rules that fired in this module

    ``-ddump-rule-rewrites``
        .. index::
           single: -ddump-rule-rewrites

        Dumps detailed information about all rules that fired in this
        module

    ``-ddump-vect``
        .. index::
           single: -ddump-vect

        Dumps the output of the vectoriser.

    ``-ddump-simpl``
        .. index::
           single: -ddump-simpl

        Dump simplifier output (Core-to-Core passes)

    ``-ddump-inlinings``
        .. index::
           single: -ddump-inlinings

        Dumps inlining info from the simplifier

    ``-ddump-stranal``
        .. index::
           single: -ddump-stranal

        Dump strictness analyser output

    ``-ddump-strsigs``
        .. index::
           single: -ddump-strsigs

        Dump strictness signatures

    ``-ddump-cse``
        .. index::
           single: -ddump-cse

        Dump common subexpression elimination (CSE) pass output

    ``-ddump-worker-wrapper``
        .. index::
           single: -ddump-worker-wrapper

        Dump worker/wrapper split output

    ``-ddump-occur-anal``
        .. index::
           single: -ddump-occur-anal

        Dump "occurrence analysis" output

    ``-ddump-prep``
        .. index::
           single: -ddump-prep

        Dump output of Core preparation pass

    ``-ddump-stg``
        .. index::
           single: -ddump-stg

        Dump output of STG-to-STG passes

    ``-ddump-cmm``
        .. index::
           single: -ddump-cmm

        Print the C-- code out.

    ``-ddump-opt-cmm``
        .. index::
           single: -ddump-opt-cmm

        Dump the results of C-- to C-- optimising passes.

    ``-ddump-asm``
        .. index::
           single: -ddump-asm

        Dump assembly language produced by the :ref:`native code
        generator <native-code-gen>`

    ``-ddump-llvm``
        .. index::
           single: -ddump-llvm

        LLVM code from the :ref:`LLVM code generator <llvm-code-gen>`

    ``-ddump-bcos``
        .. index::
           single: -ddump-bcos

        Dump byte-code compiler output

    ``-ddump-foreign``
        .. index::
           single: -ddump-foreign

        dump foreign export stubs

``-ddump-simpl-iterations``
    .. index::
       single: -ddump-simpl-iterations

    Show the output of each *iteration* of the simplifier (each run of
    the simplifier has a maximum number of iterations, normally 4). This
    outputs even more information than ``-ddump-simpl-phases``.

``-ddump-simpl-stats``
    .. index::
       single: -ddump-simpl-stats option

    Dump statistics about how many of each kind of transformation too
    place. If you add ``-dppr-debug`` you get more detailed information.

``-ddump-if-trace``
    .. index::
       single: -ddump-if-trace

    Make the interface loader be *real* chatty about what it is up to.

``-ddump-tc-trace``
    .. index::
       single: -ddump-tc-trace

    Make the type checker be *real* chatty about what it is up to.

``-ddump-vt-trace``
    .. index::
       single: -ddump-tv-trace

    Make the vectoriser be *real* chatty about what it is up to.

``-ddump-rn-trace``
    .. index::
       single: -ddump-rn-trace

    Make the renamer be *real* chatty about what it is up to.

``-ddump-rn-stats``
    .. index::
       single: -dshow-rn-stats

    Print out summary of what kind of information the renamer had to
    bring in.

``-dverbose-core2core``, ``-dverbose-stg2stg``
    .. index::
       single: -dverbose-core2core
       single: -dverbose-stg2stg

    Show the output of the intermediate Core-to-Core and STG-to-STG
    passes, respectively. (*lots* of output!) So: when we're really
    desperate:

    ::

        % ghc -noC -O -ddump-simpl -dverbose-core2core -dcore-lint Foo.hs

``-dshow-passes``
    .. index::
       single: -dshow-passes

    Print out each pass name as it happens.

``-ddump-core-stats``
    .. index::
       single: -ddump-core-stats

    Print a one-line summary of the size of the Core program at the end
    of the optimisation pipeline.

``-dfaststring-stats``
    .. index::
       single: -dfaststring-stats

    Show statistics on the usage of fast strings by the compiler.

``-dppr-debug``
    .. index::
       single: -dppr-debug

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

``-dppr-user-length``
    .. index::
       single: -dppr-user-length

    In error messages, expressions are printed to a certain "depth",
    with subexpressions beyond the depth replaced by ellipses. This flag
    sets the depth. Its default value is 5.

``-dppr-colsNNN``
    .. index::
       single: -dppr-colsNNN

    Set the width of debugging output. Use this if your code is wrapping
    too much. For example: ``-dppr-cols200``.

``-dppr-case-as-let``
    .. index::
       single: -dppr-case-as-let

    Print single alternative case expressions as though they were strict
    let expressions. This is helpful when your code does a lot of
    unboxing.

``-dno-debug-output``
    .. index::
       single: -dno-debug-output

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

``-dsuppress-all``
    .. index::
       single: -dsuppress-all

    Suppress everything that can be suppressed, except for unique ids as
    this often makes the printout ambiguous. If you just want to see the
    overall structure of the code, then start here.

``-dsuppress-uniques``
    .. index::
       single: -dsuppress-uniques

    Suppress the printing of uniques. This may make the printout
    ambiguous (e.g. unclear where an occurrence of 'x' is bound), but it
    makes the output of two compiler runs have many fewer gratuitous
    differences, so you can realistically apply ``diff``. Once ``diff``
    has shown you where to look, you can try again without
    ``-dsuppress-uniques``

``-dsuppress-idinfo``
    .. index::
       single: -dsuppress-idinfo

    Suppress extended information about identifiers where they are
    bound. This includes strictness information and inliner templates.
    Using this flag can cut the size of the core dump in half, due to
    the lack of inliner templates

``-dsuppress-unfoldings``
    .. index::
       single: -dsuppress-unfoldings

    Suppress the printing of the stable unfolding of a variable at its
    binding site.

``-dsuppress-module-prefixes``
    .. index::
       single: -dsuppress-module-prefixes

    Suppress the printing of module qualification prefixes. This is the
    ``Data.List`` in ``Data.List.length``.

``-dsuppress-type-signatures``
    .. index::
       single: -dsuppress-type-signatures

    Suppress the printing of type signatures.

``-dsuppress-type-applications``
    .. index::
       single: -dsuppress-type-applications

    Suppress the printing of type applications.

``-dsuppress-coercions``
    .. index::
       single: -dsuppress-coercions

    Suppress the printing of type coercions.

.. _checking-consistency:

Checking for consistency
------------------------

.. index::
   single: consistency checks
   single: lint

``-dcore-lint``
    .. index::
       single: -dcore-lint

    Turn on heavyweight intra-pass sanity-checking within GHC, at Core
    level. (It checks GHC's sanity, not yours.)

``-dstg-lint``
    .. index::
       single: -dstg-lint

    Ditto for STG level. (note: currently doesn't work).

``-dcmm-lint``
    .. index::
       single: -dcmm-lint

    Ditto for C-- level.

.. _checking-determinism:

Checking for determinism
------------------------

.. index::
   single: deterministic builds

``-dinitial-unique=⟨s⟩``
    .. index::
       single: -dinitial-unique

    Start ``UniqSupply`` allocation from ⟨s⟩.

``-dunique-increment=⟨i⟩``
    .. index::
       single: -dunique-increment

    Set the increment for the generated ``Unique``'s to ⟨i⟩.

    This is useful in combination with ``-dinitial-unique`` to test if the
    generated files depend on the order of ``Unique``'s.

    Some interesting values:

    * ``-dinitial-unique=0 -dunique-increment=1`` - current sequential
      ``UniqSupply``
    * ``-dinitial-unique=16777215 -dunique-increment=-1`` - ``UniqSupply`` that
      generates in decreasing order
    * ``-dinitial-unique=1 -dunique-increment=PRIME`` - where PRIME big enough
      to overflow often - nonsequential order
