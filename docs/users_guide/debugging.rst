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
        :shortdesc: Dump to files instead of stdout
        :type: dynamic
        :category:

        Causes the output from all of the flags listed below to be dumped
        to a file. The file name depends upon the output produced; for instance,
        output from :ghc-flag:`-ddump-simpl` will end up in
        :file:`{module}.dump-simpl`.

    .. ghc-flag:: -ddump-parsed
        :shortdesc: Dump parse tree
        :type: dynamic
        :category:

        Dump parser output

    .. ghc-flag:: -ddump-parsed-ast
        :shortdesc: Dump parser output as a syntax tree
        :type: dynamic
        :category:

        Dump parser output as a syntax tree

    .. ghc-flag:: -ddump-rn
        :shortdesc: Dump renamer output
        :type: dynamic
        :category:

        Dump renamer output

    .. ghc-flag:: -ddump-rn-ast
        :shortdesc: Dump renamer output as a syntax tree
        :type: dynamic
        :category:

        Dump renamer output as a syntax tree

    .. ghc-flag:: -ddump-tc
        :shortdesc: Dump typechecker output
        :type: dynamic
        :category:

        Dump typechecker output

    .. ghc-flag:: -ddump-tc-ast
        :shortdesc: Dump typechecker output as a syntax tree
        :type: dynamic
        :category:

        Dump typechecker output as a syntax tree

    .. ghc-flag:: -ddump-splices
        :shortdesc: Dump TH spliced expressions, and what they evaluate to
        :type: dynamic
        :category:

        Dump Template Haskell expressions that we splice in, and what
        Haskell code the expression evaluates to.

    .. ghc-flag:: -dth-dec-file=⟨file⟩
        :shortdesc: Show evaluated TH declarations in a .th.hs file
        :type: dynamic
        :category:

        Dump expansions of all top-level Template Haskell splices into ⟨file⟩.

    .. ghc-flag:: -ddump-types
        :shortdesc: Dump type signatures
        :type: dynamic
        :category:

        Dump a type signature for each value defined at the top level of
        the module. The list is sorted alphabetically. Using
        :ghc-flag:`-dppr-debug` dumps a type signature for all the imported and
        system-defined things as well; useful for debugging the
        compiler.

    .. ghc-flag:: -ddump-deriv
        :shortdesc: Dump deriving output
        :type: dynamic
        :category:

        Dump derived instances

    .. ghc-flag:: -ddump-ds
        :shortdesc: Dump desugarer output
        :type: dynamic
        :category:

        Dump desugarer output

    .. ghc-flag:: -ddump-spec
        :shortdesc: Dump specialiser output
        :type: dynamic
        :category:

        Dump output of specialisation pass

    .. ghc-flag:: -ddump-rules
        :shortdesc: Dump rewrite rules
        :type: dynamic
        :category:

        Dumps all rewrite rules specified in this module; see
        :ref:`controlling-rules`.

    .. ghc-flag:: -ddump-rule-firings
        :shortdesc: Dump rule firing info
        :type: dynamic
        :category:

        Dumps the names of all rules that fired in this module

    .. ghc-flag:: -ddump-rule-rewrites
        :shortdesc: Dump detailed rule firing info
        :type: dynamic
        :category:

        Dumps detailed information about all rules that fired in this
        module

    .. ghc-flag:: -ddump-vect
        :shortdesc: Dump vectoriser input and output
        :type: dynamic
        :category:

        Dumps the output of the vectoriser.

    .. ghc-flag:: -ddump-simpl
        :shortdesc: Dump final simplifier output
        :type: dynamic
        :category:

        Dump simplifier output (Core-to-Core passes)

    .. ghc-flag:: -ddump-inlinings
        :shortdesc: Dump inlining info
        :type: dynamic
        :category:

        Dumps inlining info from the simplifier. Note that if used in
        conjunction with :ghc-flag:`-dverbose-core2core` the compiler will
        also dump the inlinings that it considers but passes up, along with
        its rationale.

    .. ghc-flag:: -ddump-stranal
        :shortdesc: Dump strictness analyser output
        :type: dynamic
        :category:

        Dump strictness analyser output

    .. ghc-flag:: -ddump-str-signatures
        :shortdesc: Dump strictness signatures
        :type: dynamic
        :category:

        Dump strictness signatures

    .. ghc-flag:: -ddump-cse
        :shortdesc: Dump CSE output
        :type: dynamic
        :category:

        Dump common subexpression elimination (CSE) pass output

    .. ghc-flag:: -ddump-worker-wrapper
        :shortdesc: Dump worker-wrapper output
        :type: dynamic
        :category:

        Dump worker/wrapper split output

    .. ghc-flag:: -ddump-occur-anal
        :shortdesc: Dump occurrence analysis output
        :type: dynamic
        :category:

        Dump "occurrence analysis" output

    .. ghc-flag:: -ddump-prep
        :shortdesc: Dump prepared core
        :type: dynamic
        :category:

        Dump output of Core preparation pass

    .. ghc-flag:: -ddump-stg
        :shortdesc: Dump final STG
        :type: dynamic
        :category:

        Dump output of STG-to-STG passes

    .. ghc-flag:: -ddump-cmm
        :shortdesc: Dump the final C-- output
        :type: dynamic
        :category:

        Dump the result of the C-- pipeline processing

    .. ghc-flag:: -ddump-cmm-from-stg
        :shortdesc: Dump STG-to-C-- output
        :type: dynamic
        :category:

        Dump the result of STG-to-C-- conversion

    .. ghc-flag:: -ddump-cmm-verbose
        :shortdesc: Show output from each C-- pipeline pass
        :type: dynamic
        :category:

        Dump output from all C-- pipeline stages. In case of
        ``.cmm`` compilation this also dumps the result of
        file parsing.

    .. ghc-flag:: -ddump-opt-cmm
        :shortdesc: Dump the results of C-- to C-- optimising passes
        :type: dynamic
        :category:

        Dump the results of C-- to C-- optimising passes.

    .. ghc-flag:: -ddump-asm
        :shortdesc: Dump assembly
        :type: dynamic
        :category:

        Dump assembly language produced by the :ref:`native code
        generator <native-code-gen>`

    .. ghc-flag:: -ddump-llvm
        :shortdesc: Dump LLVM intermediate code.
            Implies :ghc-flag:`-fllvm`.
        :type: dynamic
        :category:

        :implies: :ghc-flag:`-fllvm`

        LLVM code from the :ref:`LLVM code generator <llvm-code-gen>`

    .. ghc-flag:: -ddump-bcos
        :shortdesc: Dump interpreter byte code
        :type: dynamic
        :category:

        Dump byte-code compiler output

    .. ghc-flag:: -ddump-foreign
        :shortdesc: Dump ``foreign export`` stubs
        :type: dynamic
        :category:

        dump foreign export stubs

    .. ghc-flag:: -ddump-json
        :shortdesc: Dump error messages as JSON documents
        :type: dynamic
        :category:

         Dump error messages as JSON documents. This is intended to be consumed
         by external tooling. A good way to use it is in conjunction with
         :ghc-flag:`-ddump-to-file`.

.. ghc-flag:: -ddump-simpl-iterations
    :shortdesc: Dump output from each simplifier iteration
    :type: dynamic
    :category:

    Show the output of each *iteration* of the simplifier (each run of
    the simplifier has a maximum number of iterations, normally 4). This
    outputs even more information than ``-ddump-simpl-phases``.

.. ghc-flag:: -ddump-simpl-stats
    :shortdesc: Dump simplifier stats
    :type: dynamic
    :category:

    Dump statistics about how many of each kind of transformation too
    place. If you add ``-dppr-debug`` you get more detailed information.

.. ghc-flag:: -ddump-if-trace
    :shortdesc: Trace interface files
    :type: dynamic
    :category:

    Make the interface loader be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-tc-trace
    :shortdesc: Trace typechecker
    :type: dynamic
    :category:

    Make the type checker be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-vt-trace
    :shortdesc: Trace vectoriser
    :type: dynamic
    :category:

    Make the vectoriser be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-rn-trace
    :shortdesc: Trace renamer
    :type: dynamic
    :category:

    Make the renamer be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-ec-trace
    :shortdesc: Trace exhaustiveness checker
    :type: dynamic
    :category:

    Make the pattern match exhaustiveness checker be *real* chatty about
    what it is up to.

.. ghc-flag:: -ddump-rn-stats
    :shortdesc: Renamer stats
    :type: dynamic
    :category:

    Print out summary of what kind of information the renamer had to
    bring in.

.. ghc-flag:: -dverbose-core2core
    :shortdesc: Show output from each core-to-core pass
    :type: dynamic
    :category:

    Show the output of the intermediate Core-to-Core pass. (*lots* of output!)
    So: when we're really desperate:

    .. code-block:: sh

        % ghc -noC -O -ddump-simpl -dverbose-core2core -dcore-lint Foo.hs

.. ghc-flag:: -dverbose-stg2stg
    :shortdesc: Show output from each STG-to-STG pass
    :type: dynamic
    :category:

    Show the output of the intermediate STG-to-STG pass. (*lots* of output!)

.. ghc-flag:: -dshow-passes
    :shortdesc: Print out each pass name as it happens
    :type: dynamic
    :category:

    Print out each pass name, its runtime and heap allocations as it happens.
    Note that this may come at a slight performance cost as the compiler will
    be a bit more eager in forcing pass results to more accurately account for
    their costs.

    Two types of messages are produced: Those beginning with ``***`` are
    denote the beginning of a compilation phase whereas those starting with
    ``!!!`` mark the end of a pass and are accompanied by allocation and
    runtime statistics.

.. ghc-flag:: -ddump-core-stats
    :shortdesc: Print a one-line summary of the size of the Core program at the
        end of the optimisation pipeline
    :type: dynamic
    :category:

    Print a one-line summary of the size of the Core program at the end
    of the optimisation pipeline.

.. ghc-flag:: -dfaststring-stats
    :shortdesc: Show statistics for fast string usage when finished
    :type: dynamic
    :category:

    Show statistics on the usage of fast strings by the compiler.

.. ghc-flag:: -dppr-debug
    :shortdesc: Turn on debug printing (more verbose)
    :type: dynamic
    :category:

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
    :shortdesc: Set the depth for printing expressions in error msgs
    :type: dynamic
    :category:

    In error messages, expressions are printed to a certain "depth",
    with subexpressions beyond the depth replaced by ellipses. This flag
    sets the depth. Its default value is 5.

.. ghc-flag:: -dppr-cols=⟨n⟩
    :shortdesc: Set the width of debugging output. For example ``-dppr-cols200``
    :type: dynamic
    :category:

    Set the width of debugging output. Use this if your code is wrapping
    too much. For example: ``-dppr-cols=200``.

.. ghc-flag:: -dppr-case-as-let
    :shortdesc: Print single alternative case expressions as strict lets.
    :type: dynamic
    :category:

    Print single alternative case expressions as though they were strict
    let expressions. This is helpful when your code does a lot of
    unboxing.

.. ghc-flag:: -dno-debug-output
    :shortdesc: Suppress unsolicited debugging output
    :type: dynamic
    :reverse: -ddebug-output
    :category:

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
    :shortdesc: In core dumps, suppress everything (except for uniques) that is
        suppressible.
    :type: dynamic
    :category:

    Suppress everything that can be suppressed, except for unique ids as
    this often makes the printout ambiguous. If you just want to see the
    overall structure of the code, then start here.

.. ghc-flag:: -dsuppress-ticks
    :shortdesc: Suppress "ticks" in the pretty-printer output.
    :type: dynamic
    :category:

    Suppress "ticks" in the pretty-printer output.

.. ghc-flag:: -dsuppress-uniques
    :shortdesc: Suppress the printing of uniques in debug output (easier to use
        ``diff``)
    :type: dynamic
    :category:

    Suppress the printing of uniques. This may make the printout
    ambiguous (e.g. unclear where an occurrence of 'x' is bound), but it
    makes the output of two compiler runs have many fewer gratuitous
    differences, so you can realistically apply ``diff``. Once ``diff``
    has shown you where to look, you can try again without
    :ghc-flag:`-dsuppress-uniques`

.. ghc-flag:: -dsuppress-idinfo
    :shortdesc: Suppress extended information about identifiers where they
        are bound
    :type: dynamic
    :category:

    Suppress extended information about identifiers where they are
    bound. This includes strictness information and inliner templates.
    Using this flag can cut the size of the core dump in half, due to
    the lack of inliner templates

.. ghc-flag:: -dsuppress-unfoldings
    :shortdesc: Suppress the printing of the stable unfolding of a variable at
        its binding site
    :type: dynamic
    :category:

    Suppress the printing of the stable unfolding of a variable at its
    binding site.

.. ghc-flag:: -dsuppress-module-prefixes
    :shortdesc: Suppress the printing of module qualification prefixes
    :type: dynamic
    :category:

    Suppress the printing of module qualification prefixes. This is the
    ``Data.List`` in ``Data.List.length``.

.. ghc-flag:: -dsuppress-type-signatures
    :shortdesc: Suppress type signatures
    :type: dynamic
    :category:

    Suppress the printing of type signatures.

.. ghc-flag:: -dsuppress-type-applications
    :shortdesc: Suppress type applications
    :type: dynamic
    :category:

    Suppress the printing of type applications.

.. ghc-flag:: -dsuppress-coercions
    :shortdesc: Suppress the printing of coercions in Core dumps to make them
        shorter
    :type: dynamic
    :category:

    Suppress the printing of type coercions.

.. _checking-consistency:

Checking for consistency
------------------------

.. index::
   single: consistency checks
   single: lint

.. ghc-flag:: -dcore-lint
    :shortdesc: Turn on internal sanity checking
    :type: dynamic
    :category:

    Turn on heavyweight intra-pass sanity-checking within GHC, at Core
    level. (It checks GHC's sanity, not yours.)

.. ghc-flag:: -dstg-lint
    :shortdesc: STG pass sanity checking
    :type: dynamic
    :category:

    Ditto for STG level. (note: currently doesn't work).

.. ghc-flag:: -dcmm-lint
    :shortdesc: C-- pass sanity checking
    :type: dynamic
    :category:

    Ditto for C-- level.

.. ghc-flag:: -fllvm-fill-undef-with-garbage
    :shortdesc: Intruct LLVM to fill dead STG registers with garbage
    :type: dynamic
    :category:

    Instructs the LLVM code generator to fill dead STG registers with garbage
    instead of ``undef`` in calls. This makes it easier to catch subtle
    code generator and runtime system bugs (e.g. see :ghc-ticket:`11487`).

.. ghc-flag:: -fcatch-bottoms
    :shortdesc: Insert ``error`` expressions after bottoming expressions; useful
        when debugging the compiler.
    :type: dynamic
    :category:

    Instructs the simplifier to emit ``error`` expressions in the continuation
    of empty case analyses (which should bottom and consequently not return).
    This is helpful when debugging demand analysis bugs which can sometimes
    manifest as segmentation faults.

.. _checking-determinism:

Checking for determinism
------------------------

.. index::
   single: deterministic builds

.. ghc-flag:: -dinitial-unique=⟨s⟩
    :shortdesc: Start ``UniqSupply`` allocation from ⟨s⟩.
    :type: dynamic
    :category:

    Start ``UniqSupply`` allocation from ⟨s⟩.

.. ghc-flag:: -dunique-increment=⟨i⟩
    :shortdesc: Set the increment for the generated ``Unique``'s to ⟨i⟩.
    :type: dynamic
    :category:

    Set the increment for the generated ``Unique``'s to ⟨i⟩.

    This is useful in combination with :ghc-flag:`-dinitial-unique=⟨s⟩` to test
    if the generated files depend on the order of ``Unique``'s.

    Some interesting values:

    * ``-dinitial-unique=0 -dunique-increment=1`` - current sequential
      ``UniqSupply``
    * ``-dinitial-unique=16777215 -dunique-increment=-1`` - ``UniqSupply`` that
      generates in decreasing order
    * ``-dinitial-unique=1 -dunique-increment=PRIME`` - where PRIME big enough
      to overflow often - nonsequential order
