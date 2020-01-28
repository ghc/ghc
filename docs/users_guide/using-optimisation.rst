.. _options-optimise:

Optimisation (code improvement)
-------------------------------

.. index::
   single: optimisation
   single: improvement, code

The ``-O*`` options specify convenient "packages" of optimisation flags;
the ``-f*`` options described later on specify *individual*
optimisations to be turned on/off; the ``-m*`` options specify
*machine-specific* optimisations to be turned on/off.

Most of these options are boolean and have options to turn them both "on" and
"off" (beginning with the prefix ``no-``). For instance, while ``-fspecialise``
enables specialisation, ``-fno-specialise`` disables it. When multiple flags for
the same option appear in the command-line they are evaluated from left to
right. For instance, ``-fno-specialise -fspecialise`` will enable
specialisation.

It is important to note that the ``-O*`` flags are roughly equivalent to
combinations of ``-f*`` flags. For this reason, the effect of the
``-O*`` and ``-f*`` flags is dependent upon the order in which they
occur on the command line.

For instance, take the example of ``-fno-specialise -O1``. Despite the
``-fno-specialise`` appearing in the command line, specialisation will
still be enabled. This is the case as ``-O1`` implies ``-fspecialise``,
overriding the previous flag. By contrast, ``-O1 -fno-specialise`` will
compile without specialisation, as one would expect.

.. _optimise-pkgs:

``-O*``: convenient “packages” of optimisation flags.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are *many* options that affect the quality of code produced by
GHC. Most people only have a general goal, something like "Compile
quickly" or "Make my program run like greased lightning." The following
"packages" of optimisations (or lack thereof) should suffice.

Note that higher optimisation levels cause more cross-module
optimisation to be performed, which can have an impact on how much of
your program needs to be recompiled when you change something. This is
one reason to stick to no-optimisation when developing code.

**No ``-O*``-type option specified:** This is taken to mean “Please
compile quickly; I'm not over-bothered about compiled-code quality.”
So, for example, ``ghc -c Foo.hs``

.. ghc-flag:: -O0
    :shortdesc: Disable optimisations (default)
    :type: dynamic
    :category: optimization-levels

    Means "turn off all optimisation", reverting to the same settings as
    if no ``-O`` options had been specified. Saying ``-O0`` can be
    useful if e.g. ``make`` has inserted a ``-O`` on the command line
    already.

.. ghc-flag:: -O
              -O1
    :shortdesc: Enable level 1 optimisations
    :type: dynamic
    :reverse: -O0
    :category: optimization-levels

    .. index::
       single: optimise; normally

    Means: "Generate good-quality code without taking too long about
    it." Thus, for example: ``ghc -c -O Main.lhs``

.. ghc-flag:: -O2
    :shortdesc: Enable level 2 optimisations
    :type: dynamic
    :reverse: -O0
    :category: optimization-levels

    .. index::
       single: optimise; aggressively

    Means: "Apply every non-dangerous optimisation, even if it means
    significantly longer compile times."

    The avoided "dangerous" optimisations are those that can make
    runtime or space *worse* if you're unlucky. They are normally turned
    on or off individually.

.. ghc-flag:: -O⟨n⟩
    :shortdesc: Any -On where n > 2 is the same as -O2.
    :type: dynamic
    :reverse: -O0
    :category: optimization-levels

    .. index::
       single: optimise; aggressively

    Any -On where n > 2 is the same as -O2.

We don't use a ``-O*`` flag for day-to-day work. We use ``-O`` to get
respectable speed; e.g., when we want to measure something. When we want
to go for broke, we tend to use ``-O2`` (and we go for lots of coffee
breaks).

The easiest way to see what ``-O`` (etc.) “really mean” is to run with
:ghc-flag:`-v`, then stand back in amazement.

.. _options-f:

``-f*``: platform-independent flags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: -f\* options (GHC)
   single: -fno-\* options (GHC)

These flags turn on and off individual optimisations. Flags marked as
on by default are enabled by ``-O``, and as such you shouldn't
need to set any of them explicitly. A flag ``-fwombat`` can be negated
by saying ``-fno-wombat``.

.. ghc-flag:: -fcase-merge
    :shortdesc: Enable case-merging. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-case-merge
    :category:

    :default: on

    Merge immediately-nested case expressions that scrutinise the same variable.
    For example, ::

          case x of
             Red -> e1
             _   -> case x of
                      Blue -> e2
                      Green -> e3

    Is transformed to, ::

          case x of
             Red -> e1
             Blue -> e2
             Green -> e2

.. ghc-flag:: -fcase-folding
    :shortdesc: Enable constant folding in case expressions. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-case-folding
    :category:

    :default: on

    Allow constant folding in case expressions that scrutinise some primops:
    For example, ::

          case x `minusWord#` 10## of
             10## -> e1
             20## -> e2
             v    -> e3

    Is transformed to, ::

          case x of
             20## -> e1
             30## -> e2
             _    -> let v = x `minusWord#` 10## in e3

.. ghc-flag:: -fcall-arity
    :shortdesc: Enable call-arity optimisation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-call-arity
    :category:

    :default: on

    Enable call-arity analysis.

.. ghc-flag:: -fexitification
    :shortdesc: Enables exitification optimisation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-exitification
    :category:

    :default: on

    Enables the floating of exit paths out of recursive functions.

.. ghc-flag:: -fcmm-elim-common-blocks
    :shortdesc: Enable Cmm common block elimination. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cmm-elim-common-blocks
    :category:

    :default: on

    Enables the common block elimination optimisation
    in the code generator. This optimisation attempts to find identical
    Cmm blocks and eliminate the duplicates.

.. ghc-flag:: -fcmm-sink
    :shortdesc: Enable Cmm sinking. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cmm-sink
    :category:

    :default: on

    Enables the sinking pass in the code generator.
    This optimisation attempts to find identical Cmm blocks and
    eliminate the duplicates attempts to move variable bindings closer
    to their usage sites. It also inlines simple expressions like
    literals or registers.

.. ghc-flag:: -fasm-shortcutting
    :shortdesc: Enable shortcutting on assembly. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-asm-shortcutting
    :category:

    :default: off

    This enables shortcutting at the assembly stage of the code generator.
    In simpler terms shortcutting means if a block of instructions A only consists
    of a unconditionally jump, we replace all jumps to A by jumps to the successor
    of A.

    This is mostly done during Cmm passes. However this can miss corner cases. So at -O2
    we run the pass again at the asm stage to catch these.

.. ghc-flag:: -fblock-layout-cfg
    :shortdesc: Use the new cfg based block layout algorithm.
    :type: dynamic
    :reverse: -fno-block-layout-cfg
    :category:

    :default: off but enabled with :ghc-flag:`-O`.

    The new algorithm considers all outgoing edges of a basic blocks for
    code layout instead of only the last jump instruction.
    It also builds a control flow graph for functions, tries to find
    hot code paths and place them sequentially leading to better cache utilization
    and performance.

    This is expected to improve performance on average, but actual performance
    difference can vary.

    If you find cases of significant performance regressions, which can
    be traced back to obviously bad code layout please open a ticket.

.. ghc-flag:: -fblock-layout-weights
    :shortdesc: Sets edge weights used by the new code layout algorithm.
    :type: dynamic
    :category:

    This flag is hacker territory. The main purpose of this flag is to make
    it easy to debug and tune the new code layout algorithm. There is no
    guarantee that values giving better results now won't be worse with
    the next release.

    If you feel your code warrants modifying these settings please consult
    the source code for default values and documentation. But I strongly
    advise against this.

.. ghc-flag:: -fblock-layout-weightless
    :shortdesc: Ignore cfg weights for code layout.
    :type: dynamic
    :reverse: -fno-block-layout-weightless
    :category:

    :default: off

    When not using the cfg based blocklayout layout is determined either
    by the last jump in a basic block or the heaviest outgoing edge of the
    block in the cfg.

    With this flag enabled we use the last jump instruction in blocks.
    Without this flags the old algorithm also uses the heaviest outgoing
    edge.

    When this flag is enabled and :ghc-flag:`-fblock-layout-cfg` is disabled
    block layout behaves the same as in 8.6 and earlier.

.. ghc-flag:: -fcpr-anal
    :shortdesc: Turn on CPR analysis in the demand analyser. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cpr-anal
    :category:

    :default: on

    Turn on CPR analysis in the demand analyser.

.. ghc-flag:: -fcse
    :shortdesc: Enable common sub-expression elimination. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cse
    :category:

    :default: on

    Enables the common-sub-expression elimination
    optimisation. Switching this off can be useful if you have some
    ``unsafePerformIO`` expressions that you don't want commoned-up.

.. ghc-flag:: -fstg-cse
    :shortdesc: Enable common sub-expression elimination on the STG
        intermediate language
    :type: dynamic
    :reverse: -fno-stg-cse
    :category:

    :default: on

    Enables the common-sub-expression elimination optimisation on the STG
    intermediate language, where it is able to common up some subexpressions
    that differ in their types, but not their representation.

.. ghc-flag:: -fdicts-cheap
    :shortdesc: Make dictionary-valued expressions seem cheap to the optimiser.
    :type: dynamic
    :reverse: -fno-dicts-cheap
    :category:

    :default: off

    A very experimental flag that makes dictionary-valued expressions
    seem cheap to the optimiser.

.. ghc-flag:: -fdicts-strict
    :shortdesc: Make dictionaries strict
    :type: dynamic
    :reverse: -fno-dicts-strict
    :category:

    :default: on

    Make dictionaries strict.

.. ghc-flag:: -fdmd-tx-dict-sel
    :shortdesc: Use a special demand transformer for dictionary selectors.
        Always enabled by default.
    :type: dynamic
    :reverse: -fno-dmd-tx-dict-sel
    :category:

    :default: on

    Use a special demand transformer for dictionary selectors.

.. ghc-flag:: -fdo-eta-reduction
    :shortdesc: Enable eta-reduction. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-do-eta-reduction
    :category:

    :default: on

    Eta-reduce lambda expressions, if doing so gets rid of a whole group of
    lambdas.

.. ghc-flag:: -fdo-lambda-eta-expansion
    :shortdesc: Enable lambda eta-expansion. Always enabled by default.
    :type: dynamic
    :reverse: -fno-do-lambda-eta-expansion
    :category:

    :default: on

    Eta-expand let-bindings to increase their arity.

.. ghc-flag:: -feager-blackholing
    :shortdesc: Turn on :ref:`eager blackholing <parallel-compile-options>`
    :type: dynamic
    :category:

    :default: off

    Usually GHC black-holes a thunk only when it switches threads. This
    flag makes it do so as soon as the thunk is entered. See `Haskell on
    a shared-memory
    multiprocessor <http://community.haskell.org/~simonmar/papers/multiproc.pdf>`__.

    See :ref:`parallel-compile-options` for a discussion on its use.

.. ghc-flag:: -fexcess-precision
    :shortdesc: Enable excess intermediate precision
    :type: dynamic
    :reverse: -fno-excess-precision
    :category:

    :default: off

    When this option is given, intermediate floating point values can
    have a *greater* precision/range than the final type. Generally this
    is a good thing, but some programs may rely on the exact
    precision/range of ``Float``/``Double`` values and should not use
    this option for their compilation.

    Note that the 32-bit x86 native code generator only supports
    excess-precision mode, so neither ``-fexcess-precision`` nor
    ``-fno-excess-precision`` has any effect. This is a known bug, see
    :ref:`bugs-ghc`.

.. ghc-flag:: -fexpose-all-unfoldings
    :shortdesc: Expose all unfoldings, even for very large or recursive functions.
    :type: dynamic
    :reverse: -fno-expose-all-unfoldings
    :category:

    :default: off

    An experimental flag to expose all unfoldings, even for very large
    or recursive functions. This allows for all functions to be inlined
    while usually GHC would avoid inlining larger functions.

.. ghc-flag:: -ffloat-in
    :shortdesc: Turn on the float-in transformation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-float-in
    :category:

    :default: on

    Float let-bindings inwards, nearer their binding
    site. See `Let-floating: moving bindings to give faster programs
    (ICFP'96) <http://research.microsoft.com/en-us/um/people/simonpj/papers/float.ps.gz>`__.

    This optimisation moves let bindings closer to their use site. The
    benefit here is that this may avoid unnecessary allocation if the
    branch the let is now on is never executed. It also enables other
    optimisation passes to work more effectively as they have more
    information locally.

    This optimisation isn't always beneficial though (so GHC applies
    some heuristics to decide when to apply it). The details get
    complicated but a simple example is that it is often beneficial to
    move let bindings outwards so that multiple let bindings can be
    grouped into a larger single let binding, effectively batching their
    allocation and helping the garbage collector and allocator.

.. ghc-flag:: -ffull-laziness
    :shortdesc: Turn on full laziness (floating bindings outwards).
        Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-full-laziness
    :category:

    :default: on

    Run the full laziness optimisation (also known as
    let-floating), which floats let-bindings outside enclosing lambdas,
    in the hope they will be thereby be computed less often. See
    `Let-floating: moving bindings to give faster programs
    (ICFP'96) <http://research.microsoft.com/en-us/um/people/simonpj/papers/float.ps.gz>`__.
    Full laziness increases sharing, which can lead to increased memory
    residency.

    .. note::
        GHC doesn't implement complete full laziness. Although GHC's
        full-laziness optimisation does enable some transformations
        which would be performed by a fully lazy implementation (such as
        extracting repeated computations from loops), these
        transformations are not applied consistently, so don't rely on
        them.

.. ghc-flag:: -ffun-to-thunk
    :shortdesc: Allow worker-wrapper to convert a function closure into a thunk
        if the function does not use any of its arguments. Off by default.
    :type: dynamic
    :reverse: -fno-fun-to-thunk
    :category:

    :default: off

    Worker-wrapper removes unused arguments, but usually we do not
    remove them all, lest it turn a function closure into a thunk,
    thereby perhaps creating a space leak and/or disrupting inlining.
    This flag allows worker/wrapper to remove *all* value lambdas.

.. ghc-flag:: -fignore-asserts
    :shortdesc: Ignore assertions in the source. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-ignore-asserts
    :category:

    :default: on

    Causes GHC to ignore uses of the function ``Exception.assert`` in source
    code (in other words, rewriting ``Exception.assert p e`` to ``e`` (see
    :ref:`assertions`).

.. ghc-flag:: -fignore-interface-pragmas
    :shortdesc: Ignore pragmas in interface files. Implied by :ghc-flag:`-O0` only.
    :type: dynamic
    :reverse: -fno-ignore-interface-pragmas
    :category:

    :default: off

    Tells GHC to ignore all inessential information when reading
    interface files. That is, even if :file:`M.hi` contains unfolding or
    strictness information for a function, GHC will ignore that
    information.

.. ghc-flag:: -flate-dmd-anal
    :shortdesc: Run demand analysis again, at the end of the
        simplification pipeline
    :type: dynamic
    :reverse: -fno-late-dmd-anal
    :category:

    :default: off

    Run demand analysis again, at the end of the simplification
    pipeline. We found some opportunities for discovering strictness
    that were not visible earlier; and optimisations like
    :ghc-flag:`-fspec-constr` can create functions with unused arguments which
    are eliminated by late demand analysis. Improvements are modest, but
    so is the cost. See notes on the :ghc-wiki:`wiki page <late-dmd>`.

.. ghc-flag:: -fliberate-case
    :shortdesc: Turn on the liberate-case transformation. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-liberate-case
    :category:

    :default: off but enabled with :ghc-flag:`-O2`.

    Turn on the liberate-case transformation. This unrolls recursive function
    once in its own RHS, to avoid repeated case analysis of free variables. It's
    a bit like the call-pattern specialiser (:ghc-flag:`-fspec-constr`) but for
    free variables rather than arguments.

.. ghc-flag:: -fliberate-case-threshold=⟨n⟩
    :shortdesc: *default: 2000.* Set the size threshold for the liberate-case
        transformation to ⟨n⟩
    :type: dynamic
    :reverse: -fno-liberate-case-threshold
    :category:

    :default: 2000

    Set the size threshold for the liberate-case transformation.

.. ghc-flag:: -floopification
    :shortdesc: Turn saturated self-recursive tail-calls into local jumps in the
        generated assembly. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-loopification
    :category:

    :default: on

    When this optimisation is enabled the code generator will turn all
    self-recursive saturated tail calls into local jumps rather than
    function calls.

.. ghc-flag:: -fllvm-pass-vectors-in-regs
    :shortdesc: Pass vector value in vector registers for function calls
    :type: dynamic
    :reverse: -fno-llvm-pass-vectors-in-regs
    :category:

    :default: on

    Instructs GHC to use the platform's native vector registers to pass vector
    arguments during function calls. As with all vector support, this requires
    :ghc-flag:`-fllvm`.

.. ghc-flag:: -fmax-inline-alloc-size=⟨n⟩
    :shortdesc: *default: 128.* Set the maximum size of inline array allocations
        to ⟨n⟩ bytes (default: 128).
    :type: dynamic
    :category:

    :default: 128

    Set the maximum size of inline array allocations to n bytes.
    GHC will allocate non-pinned arrays of statically known size in the current
    nursery block if they're no bigger than n bytes, ignoring GC overheap. This
    value should be quite a bit smaller than the block size (typically: 4096).

.. ghc-flag:: -fmax-inline-memcpy-insns=⟨n⟩
    :shortdesc: *default: 32.* Inline ``memcpy`` calls if they would generate no
        more than ⟨n⟩ pseudo instructions.
    :type: dynamic
    :category:

    :default: 32

    Inline ``memcpy`` calls if they would generate no more than ⟨n⟩ pseudo-instructions.

.. ghc-flag:: -fmax-inline-memset-insns=⟨n⟩
    :shortdesc: *default: 32.* Inline ``memset`` calls if they would generate no
        more than ⟨n⟩ pseudo instructions
    :type: dynamic
    :category:

    :default: 32

    Inline ``memset`` calls if they would generate no more than n pseudo
    instructions.

.. ghc-flag:: -fmax-relevant-binds=⟨n⟩
    :shortdesc: *default: 6.* Set the maximum number of bindings to display in
        type error messages.
    :type: dynamic
    :reverse: -fno-max-relevant-binds
    :category: verbosity

    :default: 6

    The type checker sometimes displays a fragment of the type
    environment in error messages, but only up to some maximum number,
    set by this flag. Turning it off with
    ``-fno-max-relevant-binds`` gives an unlimited number.
    Syntactically top-level bindings are also usually excluded (since
    they may be numerous), but ``-fno-max-relevant-binds`` includes
    them too.

.. ghc-flag:: -fmax-uncovered-patterns=⟨n⟩
    :shortdesc: *default: 4.* Set the maximum number of patterns to display in
        warnings about non-exhaustive ones.
    :type: dynamic
    :category:

    :default: 4

    Maximum number of unmatched patterns to be shown in warnings generated by
    :ghc-flag:`-Wincomplete-patterns` and :ghc-flag:`-Wincomplete-uni-patterns`.

.. ghc-flag:: -fmax-simplifier-iterations=⟨n⟩
    :shortdesc: *default: 4.* Set the max iterations for the simplifier.
    :type: dynamic
    :category:

    :default: 4

    Sets the maximal number of iterations for the simplifier.

.. ghc-flag:: -fmax-worker-args=⟨n⟩
    :shortdesc: *default: 10.* If a worker has that many arguments, none will
        be unpacked anymore.
    :type: dynamic
    :category:

    :default: 10

    If a worker has that many arguments, none will be unpacked anymore.

.. ghc-flag:: -fno-opt-coercion
    :shortdesc: Turn off the coercion optimiser
    :type: dynamic
    :category:

    :default: coercion optimisation enabled.

    Turn off the coercion optimiser.

.. ghc-flag:: -fno-pre-inlining
    :shortdesc: Turn off pre-inlining
    :type: dynamic
    :category:

    :default: pre-inlining enabled

    Turn off pre-inlining.

.. ghc-flag:: -fno-state-hack
    :shortdesc: Turn off the \state hack\ whereby any lambda with a real-world
        state token as argument is considered to be single-entry. Hence
        OK to inline things inside it.
    :type: dynamic
    :category:

    :default: state hack is enabled

    Turn off the "state hack" whereby any lambda with a ``State#`` token
    as argument is considered to be single-entry, hence it is considered
    okay to inline things inside it. This can improve performance of IO
    and ST monad code, but it runs the risk of reducing sharing.

.. ghc-flag:: -fomit-interface-pragmas
    :shortdesc: Don't generate interface pragmas. Implied by :ghc-flag:`-O0` only.
    :type: dynamic
    :reverse: -fno-omit-interface-pragmas
    :category:

    :default: Implied by :ghc-flag:`-O0`, otherwise off.

    Tells GHC to omit all inessential information from the interface
    file generated for the module being compiled (say M). This means
    that a module importing M will see only the *types* of the functions
    that M exports, but not their unfoldings, strictness info, etc.
    Hence, for example, no function exported by M will be inlined into
    an importing module. The benefit is that modules that import M will
    need to be recompiled less often (only when M's exports change their
    type, not when they change their implementation).

.. ghc-flag:: -fomit-yields
    :shortdesc: Omit heap checks when no allocation is being performed.
    :type: dynamic
    :reverse: -fno-omit-yields
    :category:

    :default: yield points enabled

    Tells GHC to omit heap checks when no allocation is
    being performed. While this improves binary sizes by about 5%, it
    also means that threads run in tight non-allocating loops will not
    get preempted in a timely fashion. If it is important to always be
    able to interrupt such threads, you should turn this optimization
    off. Consider also recompiling all libraries with this optimization
    turned off, if you need to guarantee interruptibility.

.. ghc-flag:: -fpedantic-bottoms
    :shortdesc: Make GHC be more precise about its treatment of bottom (but see
        also :ghc-flag:`-fno-state-hack`). In particular, GHC will not
        eta-expand through a case expression.
    :type: dynamic
    :reverse: -fno-pedantic-bottoms
    :category:

    :default: off

    Make GHC be more precise about its treatment of bottom (but see also
    :ghc-flag:`-fno-state-hack`). In particular, stop GHC eta-expanding through
    a case expression, which is good for performance, but bad if you are
    using ``seq`` on partial applications.

.. ghc-flag:: -fregs-graph
    :shortdesc: Use the graph colouring register allocator for register
        allocation in the native code generator. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-regs-graph
    :category:

    :default: off due to a performance regression bug (:ghc-ticket:`7679`)

    *Only applies in combination with the native code generator.* Use the graph
    colouring register allocator for register allocation in the native code
    generator. By default, GHC uses a simpler, faster linear register allocator.
    The downside being that the linear register allocator usually generates
    worse code.

    Note that the graph colouring allocator is a bit experimental and may fail
    when faced with code with high register pressure :ghc-ticket:`8657`.

.. ghc-flag:: -fregs-iterative
    :shortdesc: Use the iterative coalescing graph colouring register allocator
        in the native code generator.
    :type: dynamic
    :reverse: -fno-regs-iterative
    :category:

    :default: off

    *Only applies in combination with the native code generator.* Use the
    iterative coalescing graph colouring register allocator for register
    allocation in the native code generator. This is the same register allocator
    as the :ghc-flag:`-fregs-graph` one but also enables iterative coalescing
    during register allocation.

.. ghc-flag:: -fsimplifier-phases=⟨n⟩
    :shortdesc: *default: 2.* Set the number of phases for the simplifier.
        Ignored with :ghc-flag:`-O0`.
    :type: dynamic
    :category:

    :default: 2

    Set the number of phases for the simplifier. Ignored with ``-O0``.

.. ghc-flag:: -fsimpl-tick-factor=⟨n⟩
    :shortdesc: *default: 100.* Set the percentage factor for simplifier ticks.
    :type: dynamic
    :category:

    :default: 100

    GHC's optimiser can diverge if you write rewrite rules
    (:ref:`rewrite-rules`) that don't terminate, or (less satisfactorily)
    if you code up recursion through data types (:ref:`bugs-ghc`). To
    avoid making the compiler fall into an infinite loop, the optimiser
    carries a "tick count" and stops inlining and applying rewrite rules
    when this count is exceeded. The limit is set as a multiple of the
    program size, so bigger programs get more ticks. The
    ``-fsimpl-tick-factor`` flag lets you change the multiplier. The
    default is 100; numbers larger than 100 give more ticks, and numbers
    smaller than 100 give fewer.

    If the tick-count expires, GHC summarises what simplifier steps it
    has done; you can use ``-fddump-simpl-stats`` to generate a much
    more detailed list. Usually that identifies the loop quite
    accurately, because some numbers are very large.

.. ghc-flag:: -fspec-constr
    :shortdesc: Turn on the SpecConstr transformation. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-spec-constr
    :category:

    :default: off but enabled by :ghc-flag:`-O2`.

    Turn on call-pattern specialisation; see `Call-pattern specialisation for
    Haskell programs
    <https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions-2/>`__.

    This optimisation specializes recursive functions according to their
    argument "shapes". This is best explained by example so consider: ::

        last :: [a] -> a
        last [] = error "last"
        last (x : []) = x
        last (x : xs) = last xs

    In this code, once we pass the initial check for an empty list we
    know that in the recursive case this pattern match is redundant. As
    such ``-fspec-constr`` will transform the above code to: ::

        last :: [a] -> a
        last []       = error "last"
        last (x : xs) = last' x xs
            where
              last' x []       = x
              last' x (y : ys) = last' y ys

    As well avoid unnecessary pattern matching it also helps avoid
    unnecessary allocation. This applies when a argument is strict in
    the recursive call to itself but not on the initial entry. As strict
    recursive branch of the function is created similar to the above
    example.

    It is also possible for library writers to instruct GHC to perform
    call-pattern specialisation extremely aggressively. This is
    necessary for some highly optimized libraries, where we may want to
    specialize regardless of the number of specialisations, or the size
    of the code. As an example, consider a simplified use-case from the
    ``vector`` library: ::

        import GHC.Types (SPEC(..))

        foldl :: (a -> b -> a) -> a -> Stream b -> a
        {-# INLINE foldl #-}
        foldl f z (Stream step s _) = foldl_loop SPEC z s
          where
            foldl_loop !sPEC z s = case step s of
                                    Yield x s' -> foldl_loop sPEC (f z x) s'
                                    Skip       -> foldl_loop sPEC z s'
                                    Done       -> z

    Here, after GHC inlines the body of ``foldl`` to a call site, it
    will perform call-pattern specialisation very aggressively on
    ``foldl_loop`` due to the use of ``SPEC`` in the argument of the
    loop body. ``SPEC`` from ``GHC.Types`` is specifically recognised by
    the compiler.

    (NB: it is extremely important you use ``seq`` or a bang pattern on
    the ``SPEC`` argument!)

    In particular, after inlining this will expose ``f`` to the loop
    body directly, allowing heavy specialisation over the recursive
    cases.

.. ghc-flag:: -fspec-constr-keen
    :shortdesc: Specialize a call with an explicit constructor argument,
        even if the argument is not scrutinised in the body of the function
    :type: dynamic
    :reverse: -fno-spec-constr-keen
    :category:

    :default: off

    If this flag is on, call-pattern specialisation will specialise a call
    ``(f (Just x))`` with an explicit constructor argument, even if the argument
    is not scrutinised in the body of the function. This is sometimes
    beneficial; e.g. the argument might be given to some other function
    that can itself be specialised.

.. ghc-flag:: -fspec-constr-count=⟨n⟩
    :shortdesc: default: 3.* Set to ⟨n⟩ the maximum number of specialisations that
        will be created for any one function by the SpecConstr
        transformation.
    :type: dynamic
    :reverse: -fno-spec-constr-count
    :category:

    :default: 3

    Set the maximum number of specialisations that will be created for
    any one function by the SpecConstr transformation.

.. ghc-flag:: -fspec-constr-threshold=⟨n⟩
    :shortdesc: *default: 2000.* Set the size threshold for the SpecConstr
        transformation to ⟨n⟩.
    :type: dynamic
    :reverse: -fno-spec-constr-threshold
    :category:

    :default: 2000

    Set the size threshold for the SpecConstr transformation.

.. ghc-flag:: -fspecialise
    :shortdesc: Turn on specialisation of overloaded functions. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-specialise
    :category:

    :default: on

    Specialise each type-class-overloaded function
    defined in this module for the types at which it is called in this
    module. If :ghc-flag:`-fcross-module-specialise` is set imported functions
    that have an INLINABLE pragma (:ref:`inlinable-pragma`) will be
    specialised as well.

.. ghc-flag:: -fspecialise-aggressively
    :shortdesc: Turn on specialisation of overloaded functions regardless of
        size, if unfolding is available
    :type: dynamic
    :reverse: -fno-specialise-aggressively
    :category:

    :default: off

    By default only type class methods and methods marked ``INLINABLE`` or
    ``INLINE`` are specialised. This flag will specialise any overloaded function
    regardless of size if its unfolding is available. This flag is not
    included in any optimisation level as it can massively increase code
    size. It can be used in conjunction with :ghc-flag:`-fexpose-all-unfoldings`
    if you want to ensure all calls are specialised.


.. ghc-flag:: -fcross-module-specialise
    :shortdesc: Turn on specialisation of overloaded functions imported from
        other modules.
    :type: dynamic
    :reverse: -fno-cross-module-specialise
    :category:

    :default: on

    Specialise ``INLINABLE`` (:ref:`inlinable-pragma`)
    type-class-overloaded functions imported from other modules for the types at
    which they are called in this module. Note that specialisation must be
    enabled (by ``-fspecialise``) for this to have any effect.

.. ghc-flag:: -flate-specialise
    :shortdesc: Run a late specialisation pass
    :type: dynamic
    :reverse: -fno-late-specialise
    :category:

    :default: off

    Runs another specialisation pass towards the end of the optimisation
    pipeline. This can catch specialisation opportunities which arose from
    the previous specialisation pass or other inlining.

    You might want to use this if you are you have a type class method
    which returns a constrained type. For example, a type class where one
    of the methods implements a traversal.

.. ghc-flag:: -fsolve-constant-dicts
    :shortdesc: When solving constraints, try to eagerly solve
        super classes using available dictionaries.
    :type: dynamic
    :reverse: -fno-solve-constant-dicts
    :category:

    :default: on

    When solving constraints, try to eagerly solve
    super classes using available dictionaries.

    For example::

      class M a b where m :: a -> b

      type C a b = (Num a, M a b)

      f :: C Int b => b -> Int -> Int
      f _ x = x + 1

    The body of `f` requires a `Num Int` instance. We could solve this
    constraint from the context  because we have `C Int b` and that provides us
    a
    solution for `Num Int`. However, we can often produce much better code
    by directly solving for an available `Num Int` dictionary we might have at
    hand. This removes potentially many layers of indirection and crucially
    allows other optimisations to fire as the dictionary will be statically
    known and selector functions can be inlined.

    The optimisation also works for GADTs which bind dictionaries. If we
    statically know which class dictionary we need then we will solve it
    directly rather than indirectly using the one passed in at run time.



.. ghc-flag:: -fstatic-argument-transformation
    :shortdesc: Turn on the static argument transformation.
    :type: dynamic
    :reverse: -fno-static-argument-transformation
    :category:

    :default: off

    Turn on the static argument transformation, which turns a recursive function
    into a non-recursive one with a local recursive loop. See Chapter 7 of
    `Andre Santos's PhD thesis
    <https://www.microsoft.com/en-us/research/publication/compilation-transformation-non-strict-functional-languages/>`__.

.. ghc-flag:: -fstg-lift-lams
    :shortdesc: Enable late lambda lifting on the STG intermediate
        language. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-stg-lift-lams
    :category:

    :default: on

    Enables the late lambda lifting optimisation on the STG
    intermediate language. This selectively lifts local functions to
    top-level by converting free variables into function parameters.

.. ghc-flag:: -fstg-lift-lams-known
    :shortdesc: Allow turning known into unknown calls while performing
        late lambda lifting.
    :type: dynamic
    :reverse: -fno-stg-lift-lams-known
    :category:

    :default: off

    Allow turning known into unknown calls while performing
    late lambda lifting. This is deemed non-beneficial, so it's
    off by default.

.. ghc-flag:: -fstg-lift-lams-non-rec-args
    :shortdesc: Create top-level non-recursive functions with at most <n>
        parameters while performing late lambda lifting.
    :type: dynamic
    :reverse: -fno-stg-lift-lams-non-rec-args-any
    :category:

    :default: 5

    Create top-level non-recursive functions with at most <n> parameters
    while performing late lambda lifting. The default is 5, the number of
    available parameter registers on x86_64.

.. ghc-flag:: -fstg-lift-lams-rec-args
    :shortdesc: Create top-level recursive functions with at most <n>
        parameters while performing late lambda lifting.
    :type: dynamic
    :reverse: -fno-stg-lift-lams-rec-args-any
    :category:

    :default: 5

    Create top-level recursive functions with at most <n> parameters
    while performing late lambda lifting. The default is 5, the number of
    available parameter registers on x86_64.

.. ghc-flag:: -fstrictness
    :shortdesc: Turn on strictness analysis.
        Implied by :ghc-flag:`-O`. Implies :ghc-flag:`-fworker-wrapper`
    :type: dynamic
    :reverse: -fno-strictness
    :category:

    :default: on

    Switch on the strictness analyser. The implementation is described in the
    paper `Theory and Practice of Demand Analysis in Haskell
    <https://www.microsoft.com/en-us/research/wp-content/uploads/2017/03/demand-jfp-draft.pdf>`__.

    The strictness analyser figures out when arguments and variables in
    a function can be treated 'strictly' (that is they are always
    evaluated in the function at some point). This allow GHC to apply
    certain optimisations such as unboxing that otherwise don't apply as
    they change the semantics of the program when applied to lazy
    arguments.

.. ghc-flag:: -fstrictness-before=⟨n⟩
    :shortdesc: Run an additional strictness analysis before simplifier phase ⟨n⟩
    :type: dynamic
    :category:

    Run an additional strictness analysis before simplifier phase ⟨n⟩.

.. ghc-flag:: -funbox-small-strict-fields
    :shortdesc: Flatten strict constructor fields with a pointer-sized
        representation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-unbox-small-strict-fields
    :category:

    :default: on

    .. index::
       single: strict constructor fields
       single: constructor fields, strict

    This option causes all constructor fields which
    are marked strict (i.e. “!”) and which representation is smaller or
    equal to the size of a pointer to be unpacked, if possible. It is
    equivalent to adding an ``UNPACK`` pragma (see :ref:`unpack-pragma`)
    to every strict constructor field that fulfils the size restriction.

    For example, the constructor fields in the following data types ::

        data A = A !Int
        data B = B !A
        newtype C = C B
        data D = D !C

    would all be represented by a single ``Int#`` (see
    :ref:`primitives`) value with ``-funbox-small-strict-fields``
    enabled.

    This option is less of a sledgehammer than
    ``-funbox-strict-fields``: it should rarely make things worse. If
    you use ``-funbox-small-strict-fields`` to turn on unboxing by
    default you can disable it for certain constructor fields using the
    ``NOUNPACK`` pragma (see :ref:`nounpack-pragma`).

    Note that for consistency ``Double``, ``Word64``, and ``Int64``
    constructor fields are unpacked on 32-bit platforms, even though
    they are technically larger than a pointer on those platforms.

.. ghc-flag:: -funbox-strict-fields
    :shortdesc: Flatten strict constructor fields
    :type: dynamic
    :reverse: -fno-unbox-strict-fields
    :category:

    :default: off

    .. index::
       single: strict constructor fields
       single: constructor fields, strict

    This option causes all constructor fields which are marked strict
    (i.e. ``!``) to be unpacked if possible. It is equivalent to adding an
    ``UNPACK`` pragma to every strict constructor field (see
    :ref:`unpack-pragma`).

    This option is a bit of a sledgehammer: it might sometimes make
    things worse. Selectively unboxing fields by using ``UNPACK``
    pragmas might be better. An alternative is to use
    ``-funbox-strict-fields`` to turn on unboxing by default but disable
    it for certain constructor fields using the ``NOUNPACK`` pragma (see
    :ref:`nounpack-pragma`).

    Alternatively you can use :ghc-flag:`-funbox-small-strict-fields` to only
    unbox strict fields which are "small".

.. ghc-flag:: -funfolding-creation-threshold=⟨n⟩
    :shortdesc: *default: 750.* Tweak unfolding settings.
    :type: dynamic
    :category:

    :default: 750

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    Governs the maximum size that GHC will allow a
    function unfolding to be. (An unfolding has a “size” that reflects
    the cost in terms of “code bloat” of expanding (aka inlining) that
    unfolding at a call site. A bigger function would be assigned a
    bigger cost.)

    Consequences:

    a. nothing larger than this will be inlined (unless it has an ``INLINE`` pragma)
    b. nothing larger than this will be spewed into an interface file.

    Increasing this figure is more likely to result in longer compile times
    than faster code. The :ghc-flag:`-funfolding-use-threshold=⟨n⟩` is more
    useful.

.. ghc-flag:: -funfolding-dict-discount=⟨n⟩
    :shortdesc: *default: 30.* Tweak unfolding settings.
    :type: dynamic
    :category:

    :default: 30

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    How eager should the compiler be to inline dictionaries?

.. ghc-flag:: -funfolding-fun-discount=⟨n⟩
    :shortdesc: *default: 60.* Tweak unfolding settings.
    :type: dynamic
    :category:

    :default: 60

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    How eager should the compiler be to inline functions?

.. ghc-flag:: -funfolding-keeness-factor=⟨n⟩
    :shortdesc: *default: 1.5.* Tweak unfolding settings.
    :type: dynamic
    :category:

    :default: 1.5

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    How eager should the compiler be to inline functions?

.. ghc-flag:: -funfolding-use-threshold=⟨n⟩
    :shortdesc: *default: 60.* Tweak unfolding settings.
    :type: dynamic
    :category:

    :default: 60

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    This is the magic cut-off figure for unfolding (aka
    inlining): below this size, a function definition will be unfolded
    at the call-site, any bigger and it won't. The size computed for a
    function depends on two things: the actual size of the expression
    minus any discounts that apply depending on the context into which
    the expression is to be inlined.

    The difference between this and
    :ghc-flag:`-funfolding-creation-threshold=⟨n⟩` is that this one determines
    if a function definition will be inlined *at a call site*. The other option
    determines if a function definition will be kept around at all for
    potential inlining.

.. ghc-flag:: -fworker-wrapper
    :shortdesc: Enable the worker-wrapper transformation.
    :type: dynamic
    :category:

    Enable the worker-wrapper transformation after a strictness
    analysis pass. Implied by :ghc-flag:`-O`, and by :ghc-flag:`-fstrictness`.
    Disabled by :ghc-flag:`-fno-strictness`. Enabling :ghc-flag:`-fworker-wrapper`
    while strictness analysis is disabled (by :ghc-flag:`-fno-strictness`)
    has no effect.

.. ghc-flag:: -fbinary-blob-threshold=⟨n⟩
    :shortdesc: *default: 500K.* Tweak assembly generator for binary blobs.
    :type: dynamic
    :category: optimization

    :default: 500000

    The native code-generator can either dump binary blobs (e.g. string
    literals) into the assembly file (by using ".asciz" or ".string" assembler
    directives) or it can dump them as binary data into a temporary file which
    is then included by the assembler (using the ".incbin" assembler directive).

    This flag sets the size (in bytes) threshold above which the second approach
    is used. You can disable the second approach entirely by setting the
    threshold to 0.
