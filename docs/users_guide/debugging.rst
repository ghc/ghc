.. _options-debugging:

Debugging the compiler
======================

.. index::
   single: debugging options (for GHC)

..
    It is not necessary to provide :category: tags for ``ghc-flag:``s defined in
    this file; a default is specified in ``flags.py``.

HACKER TERRITORY. HACKER TERRITORY. (You were warned.)

.. contents:: Dump flags

.. _dumping-output:

Dumping out compiler intermediate structures
--------------------------------------------

.. index::
   single: dumping GHC intermediates
   single: intermediate passes, output

.. ghc-flag:: -ddump-to-file
    :shortdesc: Dump to files instead of stdout
    :type: dynamic

    Causes the output from each of flags starting with "-ddump", to be
    dumped to a file or files. If you want to have all the output from one
    single flag saved to one file, use :ghc-flag:`-ddump-file-prefix=⟨str⟩`
    (see descriptions below). Otherwise, the output will go to several
    files, including one for non-module specific and several for module
    specific. The suffix of a dump file depends on the flag turned on, for
    instance, output from :ghc-flag:`-ddump-simpl` will end up in
    :file:`prefix.dump-simpl`.

.. ghc-flag:: -ddump-file-prefix=⟨str⟩
    :shortdesc: Set the prefix of the filenames used for debugging output.
    :type: dynamic

    Set the prefix of the filenames used for debugging output. For example,
    ``-ddump-file-prefix=Foo`` will cause the output from
    :ghc-flag:`-ddump-simpl` to be dumped to :file:`Foo.dump-simpl`.

.. ghc-flag:: -fdump-with-ways
    :shortdesc: Include the tag of the enabled ways in the extension of dump files.
    :type: dynamic

    :default: enabled


    When compiling Main.hs with profiling and without this will now produce
    ``Main.p.dump-simpl`` and ``Main.dump-simpl`` instead of overwriting the
    output of one way with the output of another.

.. ghc-flag:: -ddump-json
    :shortdesc: *(deprecated)* Use :ghc-flag:`-fdiagnostics-as-json` instead
    :type: dynamic

    This flag was previously used to generated JSON formatted GHC diagnostics,
    but has been deprecated. Instead, use :ghc-flag:`-fdiagnostics-as-json`.

.. ghc-flag:: -dshow-passes
    :shortdesc: Print out each pass name as it happens
    :type: dynamic

    Print out each pass name, its runtime and heap allocations as it happens.
    Note that this may come at a slight performance cost as the compiler will
    be a bit more eager in forcing pass results to more accurately account for
    their costs.

    Two types of messages are produced: Those beginning with ``***`` do
    denote the beginning of a compilation phase whereas those starting with
    ``!!!`` mark the end of a pass and are accompanied by allocation and
    runtime statistics.

.. ghc-flag:: -dipe-stats
    :shortdesc: Show statistics about IPE information
    :type: dynamic

    For each module, show some simple statistics about which info tables have
    IPE information, and how many info tables with IPE information each closure
    type has. This is useful, for example, for verifying that ``STACK`` info
    tables are being appropriately omitted or included from the info table map.

.. ghc-flag:: -dfaststring-stats
    :shortdesc: Show statistics for fast string usage when finished
    :type: dynamic

    Show statistics on the usage of fast strings by the compiler.

.. ghc-flag:: -ddump-faststrings
    :shortdesc: Dump the whole FastString table when finished
    :type: dynamic

    Dump the whole FastString table when finished. Consider using
    :ghc-flag:`-ddump-file-prefix=⟨str⟩` to dump it into a file.

.. ghc-flag:: -dppr-debug
    :shortdesc: Turn on debug printing (more verbose)
    :type: dynamic

    Debugging output is in one of several "styles." Take the printing of
    types, for example. In the "user" style (the default), the
    compiler's internal ideas about types are presented in Haskell
    source-level syntax, insofar as possible. In the "debug" style
    (which is the default for debugging output), the types are printed
    in with explicit foralls, and variables have their unique-id
    attached (so you can check for things that look the same but
    aren't). This flag makes debugging output appear in the more verbose
    debug style.

.. ghc-flag:: -ddump-timings
    :shortdesc: Dump per-pass timing and allocation statistics
    :type: dynamic

    Show allocation and runtime statistics for various stages of compilation.
    Allocations are measured in bytes. Timings are measured in milliseconds.

GHC is a large program consisting of a number of stages. You can tell GHC to
dump information from various stages of compilation using the ``-ddump-⟨pass⟩``
flags listed below. Note that some of these tend to produce a lot of output.
You can prevent them from clogging up your standard output by passing
:ghc-flag:`-ddump-to-file`.

Front-end
~~~~~~~~~

These flags dump various information from GHC's frontend. This includes the
parser and interface file reader.

.. ghc-flag:: -ddump-parsed
    :shortdesc: Dump parse tree
    :type: dynamic

    Dump parser output

.. ghc-flag:: -ddump-parsed-ast
    :shortdesc: Dump parser output as a syntax tree
    :type: dynamic

    Dump parser output as a syntax tree

.. ghc-flag:: -dkeep-comments
    :shortdesc: Include comments in the parser.  Useful in combination with :ghc-flag:`-ddump-parsed-ast`.
    :type: dynamic

    Include comments in the parser.  Useful in combination with :ghc-flag:`-ddump-parsed-ast`.


.. ghc-flag:: -ddump-if-trace
    :shortdesc: Trace interface files
    :type: dynamic

    Make the interface loader be *real* chatty about what it is up to.


Type-checking and renaming
~~~~~~~~~~~~~~~~~~~~~~~~~~

These flags dump various information from GHC's typechecker and renamer.

.. ghc-flag:: -ddump-tc-trace
    :shortdesc: Trace typechecker
    :type: dynamic

    Make the type checker be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-rn-trace
    :shortdesc: Trace renamer
    :type: dynamic

    Make the renamer be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-ec-trace
    :shortdesc: Trace exhaustiveness checker
    :type: dynamic

    Make the pattern match exhaustiveness checker be *real* chatty about
    what it is up to.

.. ghc-flag:: -ddump-cs-trace
    :shortdesc: Trace constraint solver
    :type: dynamic

    Make the constraint solver be *real* chatty about what it is up to.

.. ghc-flag:: -ddump-rn-stats
    :shortdesc: Renamer stats
    :type: dynamic

    Print out summary of what kind of information the renamer had to
    bring in.

.. ghc-flag:: -ddump-rn
    :shortdesc: Dump renamer output
    :type: dynamic

    Dump renamer output

.. ghc-flag:: -ddump-rn-ast
    :shortdesc: Dump renamer output as a syntax tree
    :type: dynamic

    Dump renamer output as a syntax tree

.. ghc-flag:: -ddump-tc
    :shortdesc: Dump typechecker output
    :type: dynamic

    Dump typechecker output. Note that this hides a great deal of detail by
    default; you might consider using this with
    :ghc-flag:`-fprint-typechecker-elaboration`.

.. ghc-flag:: -ddump-tc-ast
    :shortdesc: Dump typechecker output as a syntax tree
    :type: dynamic

    Dump typechecker output as a syntax tree

.. ghc-flag:: -ddump-hie
    :shortdesc: Dump the hie file syntax tree
    :type: dynamic

    Dump the hie file syntax tree if we are generating extended interface files

.. ghc-flag:: -ddump-splices
    :shortdesc: Dump TH spliced expressions, and what they evaluate to
    :type: dynamic

    Dump Template Haskell expressions that we splice in, and what
    Haskell code the expression evaluates to.

.. ghc-flag:: -dth-dec-file
    :shortdesc: Dump evaluated TH declarations into `*.th.hs` files
    :type: dynamic

    Dump expansions of all top-level Template Haskell splices into
    :file:`{module}.th.hs` for each file :file:`{module}.hs`.

.. ghc-flag:: -ddump-types
    :shortdesc: Dump type signatures
    :type: dynamic

    Dump a type signature for each value defined at the top level of
    the module. The list is sorted alphabetically. Using
    :ghc-flag:`-dppr-debug` dumps a type signature for all the imported and
    system-defined things as well; useful for debugging the
    compiler.

.. ghc-flag:: -ddump-deriv
    :shortdesc: Dump deriving output
    :type: dynamic

    Dump derived instances


Core representation and simplification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These flags dump various phases of GHC's Core-to-Core pipeline. This begins with
the desugarer and includes the simplifier, worker-wrapper transformation, the
rule engine, the specialiser, the strictness/occurrence analyser, and a common
subexpression elimination pass.

.. ghc-flag:: -ddump-call-arity
    :shortdesc: Dump output of the call arity analysis pass.
    :type: dynamic

    Dump output of the call arity analysis pass (:ghc-flag:`-fcall-arity`).

.. ghc-flag:: -ddump-core-stats
    :shortdesc: Print a one-line summary of the size of the Core program at the
        end of the optimisation pipeline
    :type: dynamic

    Print a one-line summary of the size of the Core program at the end
    of the optimisation pipeline.

.. ghc-flag:: -ddump-ds
              -ddump-ds-preopt
    :shortdesc: Dump desugarer output.
    :type: dynamic

    Dump desugarer output. :ghc-flag:`-ddump-ds` dumps the output after the very
    simple optimiser has run (which discards a lot of clutter and hence is a
    sensible default. :ghc-flag:`-ddump-ds-preopt` shows the output after
    desugaring but before the very simple optimiser.

.. ghc-flag:: -ddump-exitify
    :shortdesc: Dump output of the exitification pass.
    :type: dynamic

    Dump output of the exitification pass (:ghc-flag:`-fexitification`),
    which tries to pull out code out of recursive functions.

.. ghc-flag:: -ddump-simpl-iterations
    :shortdesc: Dump output from each simplifier iteration
    :type: dynamic

    Show the output of each *iteration* of the simplifier (each run of
    the simplifier has a maximum number of iterations, normally 4).

.. ghc-flag:: -ddump-simpl-stats
    :shortdesc: Dump simplifier stats
    :type: dynamic

    Dump statistics about how many of each kind of transformation took
    place. If you add :ghc-flag:`-dppr-debug` you get more detailed information.

.. ghc-flag:: -ddump-simpl-trace
    :shortdesc: Dump trace messages in simplifier
    :type: dynamic

    Dump trace messages from various functions of the simplifier.
    Produces quite a lot of output.

.. ghc-flag:: -dverbose-core2core
    :shortdesc: Show output from each core-to-core pass
    :type: dynamic

    Show the output of the intermediate Core-to-Core pass. (*lots* of output!)
    So: when we're really desperate:

    .. code-block:: sh

        % ghc -noC -O -ddump-simpl -dverbose-core2core -dcore-lint Foo.hs

.. ghc-flag:: -ddump-spec
    :shortdesc: Dump specialiser output
    :type: dynamic

    Dump output of typeclass specialisation pass

.. ghc-flag:: -ddump-spec-constr
    :shortdesc: Dump specialiser output from SpecConstr
    :type: dynamic

    :since: 9.8.1

    Dump output of the SpecConstr specialisation pass

.. ghc-flag:: -ddump-rules
    :shortdesc: Dump rewrite rules
    :type: dynamic

    Dumps all rewrite rules specified in this module; see
    :ref:`controlling-rules`.

.. ghc-flag:: -ddump-rule-firings
    :shortdesc: Dump rule firing info
    :type: dynamic

    Dumps the names of all rules that fired in this module

.. ghc-flag:: -ddump-rule-rewrites
    :shortdesc: Dump detailed rule firing info
    :type: dynamic

    Dumps detailed information about all rules that fired in this
    module

.. ghc-flag:: -drule-check=⟨str⟩
    :shortdesc: Dump information about potential rule application
    :type: dynamic

    This flag is useful for debugging why a rule you expect to be firing isn't.

    Rules are filtered by the user provided string, a rule is kept if a prefix
    of its name matches the string.
    The pass then checks whether any of these rules could apply to
    the program but which didn't fire for some reason. For example, specifying
    ``-drule-check=SPEC`` will check whether there are any applications which
    might be subject to a rule created by specialisation.

.. ghc-flag:: -dinline-check=⟨str⟩
    :shortdesc: Dump information about inlining decisions
    :type: dynamic

    This flag is useful for debugging why a definition is not inlined.

    When a string is passed to this flag we report information
    about all functions whose name shares a prefix with the string.

    For example, if you are inspecting the core of your program and you observe
    that ``foo`` is not being inlined. You can pass ``-dinline-check foo`` and
    you will see a report about why ``foo`` is not inlined.

.. ghc-flag:: -ddump-simpl
    :shortdesc: Dump final simplifier output
    :type: dynamic

    Dump simplifier output (Core-to-Core passes)

.. ghc-flag:: -ddump-inlinings
    :shortdesc: Dump inlinings performed by the simplifier.
    :type: dynamic

    Dumps inlinings performed by the simplifier.

.. ghc-flag:: -ddump-verbose-inlinings
    :shortdesc: Dump all considered inlinings
    :type: dynamic

    Dumps all inlinings considered by the simplifier, even those ultimately not
    performed. This output includes various information that the simplifier uses
    to determine whether the inlining is beneficial.

.. ghc-flag:: -ddump-stranal
    :shortdesc: *(deprecated)* Alias for :ghc-flag:`-ddump-dmdanal`
    :type: dynamic

    Has been renamed to :ghc-flag:`-ddump-dmdanal`.

.. ghc-flag:: -ddump-dmdanal
    :shortdesc: Dump demand analysis output
    :type: dynamic

    Dump demand analysis output.

    See :ghc-flag:`-fstrictness` for the syntax and semantics of demand
    annotations.

.. ghc-flag:: -ddump-str-signatures
    :shortdesc: *(deprecated)* Alias for :ghc-flag:`-ddump-dmd-signatures`
    :type: dynamic

    Has been renamed to :ghc-flag:`-ddump-dmd-signatures`.

.. ghc-flag:: -ddump-dmd-signatures
    :shortdesc: Dump top-level demand signatures
    :type: dynamic

    Dump top-level demand signatures as produced by demand analysis.

    See :ghc-flag:`-fstrictness` for the syntax and semantics of demand
    annotations.

.. ghc-flag:: -ddump-cpranal
    :shortdesc: Dump CPR analysis output
    :type: dynamic

    Dump Constructed Product Result analysis output

.. ghc-flag:: -ddump-cpr-signatures
    :shortdesc: Dump CPR signatures
    :type: dynamic

    Dump Constructed Product Result signatures

.. ghc-flag:: -ddump-cse
    :shortdesc: Dump CSE output
    :type: dynamic

    Dump common subexpression elimination (CSE) pass output

.. ghc-flag:: -ddump-full-laziness
              -ddump-float-out
    :shortdesc: Dump full laziness pass output
    :type: dynamic

    Dump full laziness pass (also known as float-out) output (see :ghc-flag:`-ffull-laziness`)

.. ghc-flag:: -ddump-float-in
    :shortdesc: Dump float in output
    :type: dynamic

    Dump float-in pass output (see :ghc-flag:`-ffloat-in`)

.. ghc-flag:: -ddump-liberate-case
    :shortdesc: Dump liberate case output
    :type: dynamic

    Dump liberate case pass output (see :ghc-flag:`-fliberate-case`)

.. ghc-flag:: -ddump-static-argument-transformation
    :shortdesc: Dump static argument transformation output
    :type: dynamic

    Dump static argument transformation pass output (see :ghc-flag:`-fstatic-argument-transformation`)

.. ghc-flag:: -ddump-worker-wrapper
    :shortdesc: Dump worker-wrapper output
    :type: dynamic

    Dump worker/wrapper split output

.. ghc-flag:: -ddump-occur-anal
    :shortdesc: Dump occurrence analysis output
    :type: dynamic

    Dump "occurrence analysis" output

.. ghc-flag:: -ddump-prep
    :shortdesc: Dump prepared core
    :type: dynamic

    Dump output of Core preparation pass

.. ghc-flag:: -ddump-late-cc
    :shortdesc: Dump core with late cost centres added
    :type: dynamic

    Dump output of LateCC pass after cost centres have been added.

.. ghc-flag:: -ddump-view-pattern-commoning
    :shortdesc: Dump commoned view patterns
    :type: dynamic

    Print the view patterns that are commoned.

STG representation
~~~~~~~~~~~~~~~~~~

These flags dump various phases of GHC's STG pipeline.

.. ghc-flag:: -ddump-stg-from-core
    :shortdesc: Show CoreToStg output
    :type: dynamic

    Show the output of CoreToStg pass.

.. ghc-flag:: -dverbose-stg2stg
    :shortdesc: Show output from each STG-to-STG pass
    :type: dynamic

    Show the output of the intermediate STG-to-STG pass. (*lots* of output!)

.. ghc-flag:: -ddump-stg-unarised
    :shortdesc: Show unarised STG
    :type: dynamic

    Show the output of the unarise pass.

.. ghc-flag:: -ddump-stg-cg
    :shortdesc: Show output after Stg2Stg
    :type: dynamic

    Show the output of the STG after Stg2Stg. This is the result after
    applying the Stg2Stg optimization passes.

.. ghc-flag:: -ddump-stg-tags
    :shortdesc: Show output of the tag inference pass.
    :type: dynamic

    Show the output of the tag inference pass.

.. ghc-flag:: -ddump-stg-final
    :shortdesc: Show output of last STG pass.
    :type: dynamic

    Show the output of the last STG pass before we generate Cmm.

.. ghc-flag:: -ddump-stg
    :shortdesc: *(deprecated)* Alias for :ghc-flag:`-ddump-stg-from-core`
    :type: dynamic

    Alias for :ghc-flag:`-ddump-stg-from-core`. Deprecated in favor of more explicit
    flags: :ghc-flag:`-ddump-stg-from-core`, :ghc-flag:`-ddump-stg-final`, etc.

.. ghc-flag:: -ddump-stg-from-js-sinker
    :shortdesc: Show JavaScript sinker output
    :type: dynamic

    Show the output of JavaScript Sinker pass.

C-\\- representation
~~~~~~~~~~~~~~~~~~~~

These flags dump various phases of GHC's C-\\- pipeline.

.. ghc-flag:: -ddump-cmm-verbose-by-proc
    :shortdesc: Show output from main C-\\- pipeline passes (grouped by proc)
    :type: dynamic

    Dump output from main C-\\- pipeline stages. In case of
    ``.cmm`` compilation this also dumps the result of
    file parsing. Not included are passes run by
    the chosen backend. Currently only the NCG backends runs
    additional passes ( :ghc-flag:`-ddump-opt-cmm` ).

    Cmm dumps don't include unreachable blocks since we print
    blocks in reverse post-order.

.. ghc-flag:: -ddump-cmm-verbose
    :shortdesc: Write output from main C-\\- pipeline passes to files
    :type: dynamic

    If used in conjunction with :ghc-flag:`-ddump-to-file`, writes dump
    output from main C-\\- pipeline stages to files (each stage per file).

.. ghc-flag:: -ddump-cmm-from-stg
    :shortdesc: Dump STG-to-C-\\- output
    :type: dynamic

    Dump the result of STG-to-C-\\- conversion

.. ghc-flag:: -ddump-cmm-raw
    :shortdesc: Dump raw C-\\-
    :type: dynamic

    Dump the “raw” C-\\-.

.. ghc-flag:: -ddump-cmm-cfg
    :shortdesc: Dump the results of the C-\\- control flow optimisation pass.
    :type: dynamic

    Dump the results of the C-\\- control flow optimisation pass.

.. ghc-flag:: -ddump-cmm-thread-sanitizer
    :shortdesc: Dump the results of the C-\\- ThreadSanitizer elaboration pass.
    :type: dynamic

    Dump the results of the C-\\- pass responsible for adding instrumentation
    added by :ghc-flag:`-fcmm-thread-sanitizer`.

.. ghc-flag:: -ddump-cmm-cbe
    :shortdesc: Dump the results of common block elimination
    :type: dynamic

    Dump the results of the C-\\- Common Block Elimination (CBE) pass.

.. ghc-flag:: -ddump-cmm-switch
    :shortdesc: Dump the results of switch lowering passes
    :type: dynamic

    Dump the results of the C-\\- switch lowering pass.

.. ghc-flag:: -ddump-cmm-proc
    :shortdesc: Dump the results of proc-point analysis
    :type: dynamic

    Dump the results of the C-\\- proc-point analysis pass.

.. ghc-flag:: -ddump-cmm-sp
    :shortdesc: Dump the results of the C-\\- stack layout pass.
    :type: dynamic

    Dump the results of the C-\\- stack layout pass.

.. ghc-flag:: -ddump-cmm-sink
    :shortdesc: Dump the results of the C-\\- sinking pass.
    :type: dynamic

    Dump the results of the C-\\- sinking pass.

.. ghc-flag:: -ddump-cmm-caf
    :shortdesc: Dump the results of the C-\\- CAF analysis pass.
    :type: dynamic

    Dump the results of the C-\\- CAF analysis pass.

.. ghc-flag:: -ddump-cmm-procmap
    :shortdesc: Dump the results of the C-\\- proc-point map pass.
    :type: dynamic

    Dump the results of the C-\\- proc-point map pass.

.. ghc-flag:: -ddump-cmm-split
    :shortdesc: Dump the results of the C-\\- proc-point splitting pass.
    :type: dynamic

    Dump the results of the C-\\- proc-point splitting pass.

.. ghc-flag:: -ddump-cmm-info
    :shortdesc: Dump the results of the C-\\- info table augmentation pass.
    :type: dynamic

    Dump the results of the C-\\- info table augmentation pass.

.. ghc-flag:: -ddump-cmm-cps
    :shortdesc: Dump the results of the CPS pass
    :type: dynamic

    Dump the results of the CPS pass.

.. ghc-flag:: -ddump-cmm
    :shortdesc: Dump the final C-\\- output
    :type: dynamic

    Dump the result of the C-\\- pipeline processing

.. ghc-flag:: -ddump-cfg-weights
    :shortdesc: Dump the assumed weights of the CFG.
    :type: dynamic

    Dumps the CFG with weights used by the new block layout code.
    Each CFG is dumped in dot format graph making it easy
    to visualize them.

LLVM code generator
~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -ddump-llvm
    :shortdesc: Dump LLVM intermediate code.
    :type: dynamic

    :implies: :ghc-flag:`-fllvm`

    LLVM code from the :ref:`LLVM code generator <llvm-code-gen>`

C code generator
~~~~~~~~~~~~~~~~

.. ghc-flag:: -ddump-c-backend
    :shortdesc: Dump C code produced by the C (unregisterised) backend.
    :type: dynamic

    :shortdesc: Dump C code produced by the C (unregisterised) backend.

Native code generator
~~~~~~~~~~~~~~~~~~~~~

These flags dump various stages of the :ref:`native code generator's
<native-code-gen>` pipeline, which starts with C-\\- and produces native
assembler.

.. ghc-flag:: -ddump-cmm-opt
    :shortdesc: Dump the results of C-\\- to C-\\- optimising passes
    :type: dynamic

    Dump the results of C-\\- to C-\\- optimising passes performed by the NCG.

.. ghc-flag:: -ddump-opt-cmm
    :shortdesc: Dump the results of C-\\- to C-\\- optimising passes
    :type: dynamic

    Alias for :ghc-flag:`-ddump-cmm-opt`

.. ghc-flag:: -ddump-asm-conflicts
    :shortdesc: Dump register conflicts from the register allocator.
    :type: dynamic

    Dump (virtual) register conflicts ("interferences") from the
    graph coloring register allocator (:ghc-flag:`-fregs-graph`).

.. ghc-flag:: -ddump-asm-native
    :shortdesc: Dump initial assembly
    :type: dynamic

    Dump the initial assembler output produced from C-\\-.

.. ghc-flag:: -ddump-asm-liveness
    :shortdesc: Dump assembly augmented with register liveness
    :type: dynamic

    Dump the result of the register liveness pass.

.. ghc-flag:: -ddump-asm-regalloc
    :shortdesc: Dump the result of register allocation
    :type: dynamic

    Dump the result of the register allocation pass.

.. ghc-flag:: -ddump-asm-regalloc-stages
    :shortdesc: Dump the build/spill stages of the :ghc-flag:`-fregs-graph`
                register allocator.
    :type: dynamic

    Dump the build/spill stages of the :ghc-flag:`-fregs-graph` register
    allocator.

.. ghc-flag:: -ddump-asm-stats
    :shortdesc: Dump statistics from the register allocator.
    :type: dynamic

    Dump statistics from the register allocator.

.. ghc-flag:: -ddump-asm
    :shortdesc: Dump final assembly
    :type: dynamic

    Dump the final assembly produced by the native code generator.

.. ghc-flag:: -ddump-js
    :shortdesc: Dump final JavaScript code
    :type: dynamic

    Dump the final JavaScript code produced by the JavaScript code generator.


JavaScript code generator
~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -ddisable-js-minifier
   :shortdesc: Generate pretty-printed JavaScript code instead of minified (compacted) code.
   :type: dynamic

   Include human-readable spacing and indentation when generating JavaScript.

.. ghc-flag:: -ddisable-js-c-sources
   :shortdesc: Disable the link with C sources compiled to JavaScript
   :type: dynamic

   For debugging it can be useful to avoid linking with C sources compiled to
   JavaScript with Emscripten. This also avoids linking with Emcscripten's RTS.
   Note that code that calls into this C code or that uses Emscripten's
   primitives will fail at runtime (e.g. undefined function errors).

Miscellaneous backend dumps
~~~~~~~~~~~~~~~~~~~~~~~~~~~

These flags dump various bits of information from other backends.

.. ghc-flag:: -ddump-bcos
    :shortdesc: Dump interpreter byte code
    :type: dynamic

    Dump byte-code objects (BCOs) produced for the GHC's byte-code interpreter.

.. ghc-flag:: -ddump-debug
    :shortdesc: Dump generated DWARF debug information
    :type: dynamic

    Dump generated debug information (DWARF) produced with the :ghc-flag:`-g` flag.

.. ghc-flag:: -ddump-rtti
    :shortdesc: Trace runtime type inference
    :type: dynamic

    Trace runtime type inference done by various interpreter commands.

.. ghc-flag:: -ddump-foreign
    :shortdesc: Dump ``foreign export`` stubs
    :type: dynamic

    Dump foreign export stubs.

.. ghc-flag:: -ddump-ticked
    :shortdesc: Dump the code instrumented by HPC (:ref:`hpc`).
    :type: dynamic

    Dump the code instrumented by HPC (:ref:`hpc`).

.. ghc-flag:: -ddump-hpc
    :shortdesc: An alias for :ghc-flag:`-ddump-ticked`.
    :type: dynamic

    An alias for :ghc-flag:`-ddump-ticked`.

.. ghc-flag:: -ddump-mod-map
    :shortdesc: Dump the state of the module mapping database.
    :type: dynamic

    Dump a mapping of modules to where they come from, and how:

    - ``(hidden module)``: Module is hidden, and thus will never be available for
      import.

    - ``(unusable module)``: Module is unavailable because the package is unusable.

    - ``(hidden package)``: This module is in someone's exported-modules list,
      but that package is hidden.

    - ``(exposed package)``: Module is available for import.

    - ``(reexport by <PACKAGES>)``: This module is available from a reexport
      of some set of exposed packages.

    - ``(hidden reexport by <PACKAGES>)``: This module is available from a reexport
      of some set of hidden packages.

    - ``(package flag)``: This module export comes from a package flag.

.. _formatting dumps:

Formatting dumps
----------------

.. index::
   single: formatting dumps

.. ghc-flag:: -dppr-user-length
    :shortdesc: Set the depth for printing expressions in error msgs
    :type: dynamic

    In error messages, expressions are printed to a certain "depth",
    with subexpressions beyond the depth replaced by ellipses. This flag
    sets the depth. Its default value is 5.

.. ghc-flag:: -dppr-cols=⟨n⟩
    :shortdesc: Set the width of debugging output. For example ``-dppr-cols200``
    :type: dynamic

    Set the width of debugging output. Use this if your code is wrapping
    too much. For example: ``-dppr-cols=200``.

.. ghc-flag:: -dppr-case-as-let
    :shortdesc: Print single alternative case expressions as strict lets.
    :type: dynamic

    Print single alternative case expressions as though they were strict
    let expressions. This is helpful when your code does a lot of
    unboxing.

.. ghc-flag:: -dhex-word-literals
    :shortdesc: Print values of type `Word#` in hexadecimal.
    :type: dynamic

    Print values of type `Word#` and `Word64#` (but not values of
    type `Int#` and `Int64#`) in hexadecimal instead of decimal.
    The hexadecimal is zero-padded to make the length of the
    representation a power of two. For example: `0x0A0A##`,
    `0x000FFFFF##`, `0xC##`. This flag may be helpful when you
    are producing a bit pattern that to expect to work correctly on a 32-bit
    or a 64-bit architecture. Dumping hexadecimal literals after
    optimizations and constant folding makes it easier to confirm
    that the generated bit pattern is correct.

.. ghc-flag:: -dno-debug-output
    :shortdesc: Suppress unsolicited debugging output
    :type: dynamic
    :reverse: -ddebug-output

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
    :shortdesc: In dumps, suppress everything (except for uniques) that is
        suppressible.
    :type: dynamic

    Suppress everything that can be suppressed, except for unique ids as
    this often makes the printout ambiguous. If you just want to see the
    overall structure of the code, then start here.

.. ghc-flag:: -dsuppress-ticks
    :shortdesc: Suppress "ticks" in the pretty-printer output.
    :type: dynamic

    Suppress "ticks" in the pretty-printer output.

.. ghc-flag:: -dsuppress-uniques
    :shortdesc: Suppress the printing of uniques in debug output (easier to use
        ``diff``)
    :type: dynamic

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

    Suppress extended information about identifiers where they are
    bound. This includes strictness information and inliner templates.
    Using this flag can cut the size of the core dump in half, due to
    the lack of inliner templates

.. ghc-flag:: -dsuppress-unfoldings
    :shortdesc: Suppress the printing of the stable unfolding of a variable at
        its binding site
    :type: dynamic

    Suppress the printing of the stable unfolding of a variable at its
    binding site.

.. ghc-flag:: -dsuppress-module-prefixes
    :shortdesc: Suppress the printing of module qualification prefixes
    :type: dynamic

    Suppress the printing of module qualification prefixes. This is the
    ``Data.List`` in ``Data.List.length``.

.. ghc-flag:: -dsuppress-timestamps
    :shortdesc: Suppress timestamps in dumps
    :type: dynamic

    Suppress the printing of timestamps.
    This makes it easier to diff dumps.

.. ghc-flag:: -dsuppress-type-signatures
    :shortdesc: Suppress type signatures
    :type: dynamic

    Suppress the printing of type signatures.

.. ghc-flag:: -dsuppress-type-applications
    :shortdesc: Suppress type applications
    :type: dynamic

    Suppress the printing of type applications.

.. ghc-flag:: -dsuppress-coercions
    :shortdesc: Suppress the printing of coercions in Core dumps to make them
        shorter
    :type: dynamic

    Suppress the printing of type coercions.

.. ghc-flag:: -dsuppress-coercion-types
    :shortdesc: Suppress the printing of coercion types in Core dumps to make them
        shorter
    :type: dynamic

.. ghc-flag:: -dsuppress-var-kinds
    :shortdesc: Suppress the printing of variable kinds
    :type: dynamic

    Suppress the printing of variable kinds

.. ghc-flag:: -dsuppress-stg-free-vars
    :shortdesc: Suppress the printing of closure free variable lists in STG output
    :type: dynamic

    Suppress the printing of closure free variable lists in STG output

.. ghc-flag:: -dsuppress-core-sizes
    :shortdesc: Suppress the printing of core size stats per binding (since 9.4)
    :type: dynamic

    :since: 9.4.1

    Suppress the printing of core size stats per binding

.. ghc-flag:: -dsuppress-stg-reps
    :shortdesc: Suppress rep annotations on STG args.
    :type: dynamic

    :since: 9.6.1

    default: enabled

    Disabling this will annoate certain stg arguments with their prim rep.


.. _checking-consistency:

Checking for consistency
------------------------

.. index::
   single: consistency checks
   single: lint

.. ghc-flag:: -dlint
    :shortdesc: Enable several common internal sanity checkers
    :type: dynamic

    :implies: -dcore-lint, -dstg-lint, -dcmm-lint, -dasm-lint, -fllvm-fill-undef-with-garbage, -fcatch-nonexhaustive-cases, -debug
    :since: 9.4.1

    Turn on various heavy-weight intra-pass sanity-checking measures within GHC
    and its runtime system.  Notably, this does not include
    :ghc-flag:`-falignment-sanitisation` as it incurs a rather hefty runtime
    cost.

.. ghc-flag:: -dcore-lint
    :shortdesc: Turn on internal sanity checking
    :type: dynamic

    :implies: -fno-zap-casts

    Turn on heavyweight intra-pass sanity-checking within GHC, at Core
    level. (It checks GHC's sanity, not yours.)

.. ghc-flag:: -dlinear-core-lint
    :shortdesc: Turn on internal sanity checking
    :type: dynamic

    Turn on linearity checking in GHC. Currently, some optimizations
    in GHC might not preserve linearity and there are valid programs
    that fail Linear Core Lint.
    In the near future, this option will be removed and folded into
    normal Core Lint.

.. ghc-flag:: -dstg-lint
    :shortdesc: STG pass sanity checking
    :type: dynamic

    Ditto for STG level.

.. ghc-flag:: -dcmm-lint
    :shortdesc: C-\\- pass sanity checking
    :type: dynamic

    Ditto for C-\\- level.

.. ghc-flag:: -dasm-lint
    :shortdesc: ASM pass sanity checking
    :type: dynamic

    Turn on intra-pass sanity-checking within GHC, at the
    code generator level.

.. ghc-flag:: -fllvm-fill-undef-with-garbage
    :shortdesc: Intruct LLVM to fill dead STG registers with garbage
    :type: dynamic

    Instructs the LLVM code generator to fill dead STG registers with garbage
    instead of ``undef`` in calls. This makes it easier to catch subtle
    code generator and runtime system bugs (e.g. see :ghc-ticket:`11487`).

.. ghc-flag:: -falignment-sanitisation
    :shortdesc: Compile with alignment checks for all info table dereferences.
    :type: dynamic

    Compile with alignment checks for all info table dereferences. This can be
    useful when finding pointer tagging issues.

.. ghc-flag:: -fproc-alignment
    :shortdesc: Align functions at given boundary.
    :type: dynamic

    :since: 8.6.1

    Align functions to multiples of the given value. Only valid values are powers
    of two.

    ``-fproc-alignment=64`` can be used to limit alignment impact on performance
    as each function will start at a cache line.
    However forcing larger alignments in general reduces performance.

.. ghc-flag:: -fcatch-nonexhaustive-cases
    :shortdesc: Add a default ``error`` alternative to case expressions without
        a default alternative.
    :type: dynamic

    GHC generates case expressions without a default alternative in some cases:

    - When the demand analysis thinks that the scrutinee does not return (i.e. a
      bottoming expression)

    - When the scrutinee is a GADT and its type rules out some constructors, and
      others constructors are already handled by the case expression.

    With this flag GHC generates a default alternative with ``error`` in these
    cases. This is helpful when debugging demand analysis or type checker bugs
    which can sometimes manifest as segmentation faults.

.. ghc-flag:: -forig-thunk-info
    :shortdesc: Generate ``stg_orig_thunk_info`` stack frames on thunk entry
    :type: dynamic

    When debugging cyclic thunks it can be helpful to know the original
    info table of a thunk being evaluated. This flag enables code generation logic
    to facilitate this, producing a ``stg_orig_thunk_info`` stack frame alongside
    the usual update frame; such ``orig_thunk`` frames have no operational
    effect but capture the original info table of the updated thunk for inspection
    by debugging tools. See ``Note [Original thunk info table frames]`` in
    ``GHC.StgToCmm.Bind`` for details.

.. ghc-flag:: -fcheck-prim-bounds
    :shortdesc: Instrument array primops with bounds checks.
    :type: dynamic

    Typically primops operations like ``writeArray#`` exhibit unsafe behavior,
    relying on the user to perform any bounds checking. This flag instructs the
    code generator to instrument such operations with bound checking logic
    which aborts the program when an out-of-bounds access is detected.

    Note that this is only intended to be used as a debugging measure, not as
    the primary means of catching out-of-bounds accesses.

.. ghc-flag:: -fcmm-thread-sanitizer
    :shortdesc: Enable ThreadSanitizer instrumentation of memory accesses.
    :type: dynamic

    This enables generation of `ThreadSanitizer
    <https://github.com/google/sanitizers/wiki/ThreadSanitizerCppManual>`
    instrumentation of memory accesses. Requires use of ``-fsanitize=thread``
    or similar when compiling and linking.

.. _checking-determinism:

Checking for determinism
------------------------

.. index::
   single: deterministic builds

.. ghc-flag:: -dinitial-unique=⟨s⟩
    :shortdesc: Start ``UniqSupply`` allocation from ⟨s⟩.
    :type: dynamic

    Start ``UniqSupply`` allocation from ⟨s⟩.

.. ghc-flag:: -dunique-increment=⟨i⟩
    :shortdesc: Set the increment for the generated ``Unique``'s to ⟨i⟩.
    :type: dynamic

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

Other
-----

.. ghc-flag:: -fzap-casts
    :shortdesc: Discard coercion proofs from casts
    :type: dynamic

    :since: TODO

    Reduce the size of Core terms by discarding coercion proofs that are needed
    only for debugging the compiler.  This usually helps improve compile-time
    performance for some programs that make heavy use of type families.

    When this flag is enabled, Core Lint will be less effective at verifying the
    correctness of Core programs involving casts. Hence this is automatically
    switched off by :ghc-flag:`-dcore-lint`.

.. ghc-flag:: -dno-typeable-binds
    :shortdesc: Don't generate bindings for Typeable methods
    :type: dynamic

    This avoids generating Typeable-related bindings for modules and types. This
    is useful when debugging because it gives smaller modules and dumps, but the
    compiler will panic if you try to use Typeable instances of things that you
    built with this flag.

.. ghc-flag:: -dtag-inference-checks
    :shortdesc: Affirm tag inference results are correct at runtime.
    :type: dynamic

    When tag inference tells as a specific value is supposed to be tagged then
    generate code to check this at runtime. If the check fails the program will
    be terminated. This helps narrowing down if an issue is due to tag inference
    if things go wrong. Which would otherwise be quite difficult.

.. ghc-flag:: -funoptimized-core-for-interpreter
    :shortdesc: Disable optimizations with the interpreter
    :reverse: -fno-unoptimized-core-for-interpreter
    :type: dynamic

    :since: 9.8.1
    :default: enabled

    At the moment, ghci disables optimizations, because not all passes
    are compatible with the interpreter.
    This option can be used to override this check, e.g.
    ``ghci -O2 -fno-unoptimized-core-for-interpreter``.
    It is not recommended for normal use and can cause a compiler panic.

    Note that this has an effect on the debugger interface: With optimizations
    in play, free variables in breakpoints may now be substituted with complex
    expressions.
    Those cannot be stored in breakpoints, so any free variable that refers to
    optimized code will not be inspectable when this flag is enabled.

.. ghc-flag:: -fadd-bco-name
    :shortdesc: Add ``BCO_NAME`` instructions in generated bytecode.
    :reverse: -fno-add-bco-name
    :type: dynamic

    :since: 9.14.1

    Prefix every generated bytecode object with a ``BCO_NAME`` instruction
    containing the STG name of the binding from which the BCO originated.
    These are printed by the bytecode disassembler, aiding in correlating
    bytecode with STG.

