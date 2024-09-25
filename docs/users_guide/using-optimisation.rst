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
*on* by default are enabled at all optimisation levels by default, and
as such you shouldn't need to set any of them explicitly. A flag
``-fwombat`` can be negated by saying ``-fno-wombat``.

.. ghc-flag:: -fcore-constant-folding
    :shortdesc: Enable constant folding in Core. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-core-constant-folding
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enables Core-level constant folding, i.e. propagation of values
    that can be computed at compile time.

.. ghc-flag:: -fcase-merge
    :shortdesc: Enable case-merging. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-case-merge
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

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

    :default: off but enabled by :ghc-flag:`-O`.

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

    :default: off but enabled by :ghc-flag:`-O`.

    Enable call-arity analysis.

.. ghc-flag:: -fexitification
    :shortdesc: Enables exitification optimisation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-exitification
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enables the floating of exit paths out of recursive functions.

.. ghc-flag:: -fcmm-elim-common-blocks
    :shortdesc: Enable Cmm common block elimination. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cmm-elim-common-blocks
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enables the common block elimination optimisation
    in the code generator. This optimisation attempts to find identical
    Cmm blocks and eliminate the duplicates.

.. ghc-flag:: -fcmm-sink
    :shortdesc: Enable Cmm sinking. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cmm-sink
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enables the sinking pass in the code generator.
    This optimisation attempts to find identical Cmm blocks and
    eliminate the duplicates attempts to move variable bindings closer
    to their usage sites. It also inlines simple expressions like
    literals or registers.

.. ghc-flag:: -fcmm-static-pred
    :shortdesc: Enable static control flow prediction. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cmm-static-pred
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    This enables static control flow prediction on the final Cmm
    code. If enabled GHC will apply certain heuristics to identify
    loops and hot code paths. This information is then used by the
    register allocation and code layout passes.

.. ghc-flag:: -fcmm-control-flow
    :shortdesc: Enable control flow optimisation in the Cmm backend. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cmm-control-flow
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enables some control flow optimisations in the Cmm code
    generator, merging basic blocks and avoiding jumps right after jumps.

.. ghc-flag:: -fasm-shortcutting
    :shortdesc: Enable shortcutting on assembly. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-asm-shortcutting
    :category:

    :default: off but enabled by :ghc-flag:`-O2`.

    This enables shortcutting at the assembly stage of the code generator.
    In simpler terms shortcutting means if a block of instructions A only consists
    of a unconditionally jump, we replace all jumps to A by jumps to the successor
    of A.

    This is mostly done during Cmm passes. However this can miss corner cases.
    So at ``-O2`` this flag runs the pass again at the assembly stage to catch
    these. Note that due to platform limitations (:ghc-ticket:`21972`) this flag
    does nothing on macOS.

.. ghc-flag:: -fblock-layout-cfg
    :shortdesc: Use the new cfg based block layout algorithm. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-block-layout-cfg
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

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
    :shortdesc: Turn on Constructed Product Result analysis. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cpr-anal
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Turn on CPR analysis, which enables the worker/wrapper transformation (cf.
    :ghc-flag:`-fworker-wrapper`) to unbox the result of a function, such as ::

         sum :: [Int] -> Int
         sum []     = 0
         sum (x:xs) = x + sum xs

    CPR analysis will see that each code path produces a *constructed product*
    such as ``I# 0#`` in the first branch (where ``GHC.Exts.I#`` is the data
    constructor of ``Int``, boxing up the primitive integer literal ``0#``
    of type ``Int#``) and optimise to ::

         sum xs = I# ($wsum xs)
         $wsum []        = 0#
         $wsum (I# x:xs) = x# +# $wsum xs

    and then ``sum`` can inline to potentially cancel away the ``I#`` box.

    Here's an example of the function that *does not* return a constructed product: ::

         f :: [Int] -> (Int -> Int) -> Int
         f []     g = g 0
         f (x:xs) g = x + f xs g

    The expression ``g 0`` is not a constructed product, because we don't know
    anything about ``g``.

    CPR analysis also works *nestedly*, for example ::

        sumIO :: [Int] -> IO Int
        sumIO []     = return 0
        sumIO (x:xs) = do
          r <- sumIO xs
          return $! x + r

    Note the use of ``$!``: Without it, GHC would be unable to see that evaluation
    of ``r`` and ``x`` terminates (and rapidly, at that). An alternative would be to
    evaluate both with a bang pattern or a ``seq``, but the ``return $! <res>``
    idiom should work more reliably and needs less thinking. The above example
    will be optimised to ::

        sumIO :: [Int] -> IO Int
        sumIO xs = IO $ \s -> case $wsum xs s of
          (# s', r #) -> (# s', I# r #)
        $wsumIO :: [Int] -> (# State# RealWorld, Int# #)
        $wsumIO []        s = (# s, 0# #)
        $wsumIO (I# x:xs) s = case $wsumIO xs of
          (# s', r #) -> (# s', x +# r#)

    And the latter can inline ``sumIO`` and cancel away the ``I#`` constructor.
    Unboxing the result of a ``State`` action should work similarly.

.. ghc-flag:: -fcse
    :shortdesc: Enable common sub-expression elimination. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cse
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enables the common-sub-expression elimination
    optimisation. Switching this off can be useful if you have some
    ``unsafePerformIO`` expressions that you don't want commoned-up.

.. ghc-flag:: -fstg-cse
    :shortdesc: Enable common sub-expression elimination on the STG
        intermediate language. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-stg-cse
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

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
    :shortdesc: Make dictionaries strict. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-dicts-strict
    :category:

    :default: off but enabled by :ghc-flag:`-O2`.

    Make dictionaries strict.

    This enables WW to fire on dictionary constraints which usually results
    in better runtime. In niche cases it can lead to significant compile time
    regressions because of changed inlining behaviour. Rarely this can also affect
    runtime negatively.

    If enabling this flag leads to regressions try increasing the unfolding
    threshold using :ghc-flag:`-funfolding-use-threshold=⟨n⟩` by a modest amount (~30)
    as this is likely a result of a known limitation described in `#18421`.

.. ghc-flag:: -fdmd-tx-dict-sel
    :shortdesc: *(deprecated)* Use a special demand transformer for dictionary selectors.
    :type: dynamic
    :reverse: -fno-dmd-tx-dict-sel
    :category:

    :default: on

    Use a special demand transformer for dictionary selectors.
    Behaviour is unconditionally enabled starting with 9.2

.. ghc-flag:: -fdo-eta-reduction
    :shortdesc: Enable eta-reduction. Always enabled by default.
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

.. ghc-flag:: -fdo-clever-arg-eta-expansion
    :shortdesc: Enable sophisticated argument eta-expansion. Implied by :ghc-flag:`-O2`.
    :type: dynamic
    :reverse: -fno-do-clever-arg-eta-expansion
    :category:

    :default: off

    Eta-expand arguments to increase their arity to avoid allocating unnecessary
    thunks for them.

.. ghc-flag:: -feager-blackholing
    :shortdesc: Turn on :ref:`eager blackholing <parallel-compile-options>`
    :type: dynamic
    :category:

    :default: off

    Usually GHC black-holes a thunk only when it switches threads. This
    flag makes it do so as soon as the thunk is entered. See `Haskell on
    a shared-memory
    multiprocessor <https://simonmar.github.io/bib/papers/multiproc.pdf>`__.

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

    A flag to expose all unfoldings, even for very large or recursive functions.

    However GHC will still use the usual heuristics to make inlining
    and specialization choices. This means further measures are needed to
    get benefits at use sites. Usually this involves one of:

    * :ghc-flag:`-fspecialise-aggressively` to force as much specialization
      as possible.
    * `{-# SPECIALIZE #-}` pragmas to ensure specialization to specific types.
    * Use of the magic `inline` function to force inlining.

.. ghc-flag:: -fexpose-overloaded-unfoldings
    :shortdesc: Expose unfoldings carrying constraints, even for very large or recursive functions.
    :type: dynamic
    :reverse: -fno-expose-overloaded-unfoldings
    :category:

    :default: off

    This experimental flag is a slightly less heavy weight alternative
    to :ghc-flag:`-fexpose-all-unfoldings`.

    Instead of exposing all functions it only aims at exposing constrained functions.
    This is intended to be used for cases where specialization is considered
    crucial but :ghc-flag:`-fexpose-all-unfoldings` imposes too much compile
    time cost.

    Currently this won't expose unfoldings where a type class is hidden under a
    newtype. That is for cases like: ::

        newtype NT a = NT (Integral a => a)

        foo :: NT a -> T1 -> TR

    GHC won't recognise `foo` as specialisable and won't expose the unfolding
    even with :ghc-flag:`-fexpose-overloaded-unfoldings` enabled.

    All the other caveats about :ghc-flag:`-fexpose-overloaded-unfoldings`
    still apply, so please see there for more details.

.. ghc-flag:: -ffloat-in
    :shortdesc: Turn on the float-in transformation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-float-in
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Float let-bindings inwards, nearer their binding
    site. See `Let-floating: moving bindings to give faster programs
    (ICFP'96) <https://www.microsoft.com/en-us/research/publication/let-floating-moving-bindings-to-give-faster-programs/>`__.

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

    :default: off but enabled by :ghc-flag:`-O`.

    Run the full laziness optimisation (also known as
    let-floating), which floats let-bindings outside enclosing lambdas,
    in the hope they will be thereby be computed less often. See
    `Let-floating: moving bindings to give faster programs
    (ICFP'96) <https://research.microsoft.com/en-us/um/people/simonpj/papers/float.ps.gz>`__.
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
    :shortdesc: *(deprecated)* superseded by -ffull-laziness.
    :type: dynamic
    :reverse: -fno-fun-to-thunk
    :category:

    :default: off

    Worker/wrapper removes unused arguments, but usually we do not
    remove them all, lest it turn a function closure into a thunk,
    thereby perhaps creating a space leak and/or disrupting inlining.
    This flag allows worker/wrapper to remove *all* value lambdas.

    This flag was ineffective in the presence of :ghc-flag:`-ffull-laziness`,
    which would flout a thunk out of a constant worker function *even though*
    :ghc-flag:`-ffun-to-thunk` was off.

    Hence use of this flag is deprecated since GHC 9.4.1 and we rather suggest
    to pass ``-fno-full-laziness`` instead. That implies there's no way for
    worker/wrapper to turn a function into a thunk in the presence of
    ``-fno-full-laziness``. If that is inconvenient for you, please leave a
    comment `on the issue tracker (#21204) <https://gitlab.haskell.org/ghc/ghc/-/issues/21204>`__.

.. ghc-flag:: -fignore-asserts
    :shortdesc: Ignore assertions in the source. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-ignore-asserts
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Causes GHC to ignore uses of the function ``Exception.assert`` in source
    code (in other words, rewriting ``Exception.assert p e`` to ``e`` (see
    :ref:`assertions`).

.. ghc-flag:: -fignore-interface-pragmas
    :shortdesc: Ignore pragmas in interface files. Implied by :ghc-flag:`-O0` only.
    :type: dynamic
    :reverse: -fno-ignore-interface-pragmas
    :category:

    :default: Implied by :ghc-flag:`-O0`, otherwise off.

    Tells GHC to ignore all inessential information when reading
    interface files. That is, even if :file:`M.hi` contains unfolding or
    strictness information for a function, GHC will ignore that
    information.

.. ghc-flag:: -fkeep-auto-rules
    :shortdesc: Keep all "auto" rules, generated by specialisation
    :type: dynamic
    :reverse: -fno-keep-auto-rules
    :category:

    :default: off
    :since: 9.10.1

    The type-class specialiser and call-pattern specialisation both
    generate so-called "auto" RULES.  These rules are usually exposed
    to importing modules in the interface file. But when an auto rule is the
    sole reason for keeping a function alive, both the rule and the function
    are discarded, by default. That reduces code bloat, but risks the same
    function being specialised again in an importing module.

    You can change this behaviour with :ghc-flag:`-fkeep-auto-rules`. Switching
    it on keeps all auto-generated rules.

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

    :default: off but enabled by :ghc-flag:`-O2`.

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

    :default: off but enabled by :ghc-flag:`-O`.

    When this optimisation is enabled the code generator will turn all
    self-recursive saturated tail calls into local jumps rather than
    function calls.

.. ghc-flag:: -fllvm-pass-vectors-in-regs
    :shortdesc: *(deprecated)* Does nothing
    :type: dynamic
    :category:

    :default: on

    This flag has no effect since GHC 8.8 - its behavior is always on.
    It used to instruct GHC to use the platform's native vector registers
    to pass vector arguments during function calls.

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

.. ghc-flag:: -flocal-float-out
    :shortdesc: Enable local floating definitions out of let-binds.
    :type: dynamic
    :reverse: -fno-local-float-out
    :category:

    :default: on

    Enable local floating of bindings from the RHS of a let(rec) in the
    simplifier. For example ::

        let x = let y = rhs_y in rhs_x in blah
        ==>
        let y = rhs_y in let x = rhs_x in blah

    See the paper "Let-floating: moving bindings to give faster programs", Partain, Santos, and Peyton Jones; ICFP 1996.
    https://www.microsoft.com/en-us/research/publication/let-floating-moving-bindings-to-give-faster-programs/

    .. note::
      This is distinct from the global floating pass which can be disabled with
      :ghc-flag:`-fno-full-laziness`.

.. ghc-flag:: -flocal-float-out-top-level
    :shortdesc: Enable local floating to float top-level bindings
    :type: dynamic
    :reverse: -fno-local-float-out-top-level
    :category:

    :default: on

    Enable local floating of top-level bindings from the RHS of a let(rec) in
    the simplifier. For example

      x = let y = e in (a,b)
      ===>
      y = e; x = (a,b)


    See the paper "Let-floating: moving bindings to give faster programs", Partain, Santos, and Peyton Jones; ICFP 1996.
    https://www.microsoft.com/en-us/research/publication/let-floating-moving-bindings-to-give-faster-programs/

    Note that if :ghc-flag:`-fno-local-float-out` is set, that will take
    precedence.

    .. note::
      This is distinct from the global floating pass which can be disabled with
      :ghc-flag:`-fno-full-laziness`.

.. ghc-flag:: -fmax-worker-args=⟨n⟩
    :shortdesc: *default: 10.* Maximum number of value arguments for a worker.
    :type: dynamic
    :category:

    :default: 10

    A function will not be split into worker and wrapper if the number of
    value arguments of the resulting worker exceeds both that of the original
    function and this setting.

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

    :default: on (yields are *not* inserted)

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
        allocation in the native code generator.
    :type: dynamic
    :reverse: -fno-regs-graph
    :category:

    :default: off due to a performance regression bug (:ghc-ticket:`7679`)

    *Only applies in combination with the native code generator.* Use the graph
    colouring register allocator for register allocation in the native code
    generator. By default, GHC uses a simpler, faster linear register allocator.
    The downside being that the linear register allocator usually generates
    worse code.

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

.. ghc-flag:: -fdmd-unbox-width=⟨n⟩
    :shortdesc: *default: 3.* Boxity analysis pretends that returned records
                              with this many fields can be unboxed.
    :type: dynamic
    :category:

    :default: 3

    Boxity analysis optimistically pretends that a function returning a record
    with at most ``-fdmd-unbox-width`` fields has only call sites that don't
    need the box of the returned record. That may in turn allow more argument
    unboxing to happen. Set to 0 to be completely conservative (which guarantees
    that no reboxing will happen due to this mechanism).

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
    unnecessary allocation. This applies when an argument is strict in
    the recursive call to itself but not on the initial entry. A strict
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

    :default: off but enabled by :ghc-flag:`-O`.

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

    This flag controls the specialisation of *imported* functions only.  By default, an imported function
    is only specialised if it is marked ``INLINEABLE`` or ``INLINE``.
    But with :ghc-flag:`-fspecialise-aggressively`, an imported function is specialised
    if its unfolding is available in the interface file.  (Use :ghc-flag:`-fexpose-all-unfoldings`
    or :ghc-flag:`-fexpose-overloaded-unfoldings` to ensure that the unfolding is put into the interface file.)

    :ghc-flag:`-fspecialise-aggressively` is not included in any optimisation level
    as it can massively increase code size.

.. ghc-flag:: -fcross-module-specialise
    :shortdesc: Turn on specialisation of overloaded functions imported from
        other modules. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-cross-module-specialise
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Specialise ``INLINABLE`` (:ref:`inlinable-pragma`)
    type-class-overloaded functions imported from other modules for the types at
    which they are called in this module. Note that specialisation must be
    enabled (by ``-fspecialise``) for this to have any effect.

.. ghc-flag:: -fpolymorphic-specialisation
    :shortdesc: Allow specialisation to abstract over free type variables
    :type: dynamic
    :reverse: -fno-polymorphic-specialisation
    :category:

    :default: off

    Warning, this feature is highly experimental and may lead to incorrect runtime
    results. Use at your own risk (:ghc-ticket:`23469`, :ghc-ticket:`23109`, :ghc-ticket:`21229`, :ghc-ticket:`23445`).

    Enable specialisation of function calls to known dictionaries with free type variables.
    The created specialisation will abstract over the type variables free in the dictionary.


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


.. ghc-flag:: -fspecialise-incoherents
    :shortdesc: Enable specialisation on incoherent instances
    :type: dynamic
    :reverse: -fno-specialise-incoherents
    :category:

    :default: on

    Enable specialisation of overloaded functions in cases when the
    selected instance is incoherent. This makes the choice of instance
    non-deterministic, so it is only safe to do if there is no
    observable runtime behaviour difference between potentially
    unifying instances. Turning this flag off ensures the incoherent
    instance selection adheres to the algorithm described in
    :extension:`IncoherentInstances` at the cost of optimisation
    opportunities arising from specialisation.


.. ghc-flag:: -finline-generics
    :shortdesc: Annotate methods of derived Generic and Generic1 instances with
        INLINE[1] pragmas based on heuristics. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-inline-generics
    :category:

    :default: off but enabled by :ghc-flag:`-O`.
    :since: 9.2.1

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    Annotate methods of derived Generic and Generic1 instances with INLINE[1]
    pragmas based on heuristics dependent on the size of the data type in
    question. Improves performance of generics-based algorithms as GHC is able
    to optimize away intermediate representation more often.

.. ghc-flag:: -finline-generics-aggressively
    :shortdesc: Annotate methods of all derived Generic and Generic1 instances
        with INLINE[1] pragmas.
    :type: dynamic
    :reverse: -fno-inline-generics-aggressively
    :category:

    :default: off
    :since: 9.2.1

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    Annotate methods of all derived Generic and Generic1 instances with
    INLINE[1] pragmas.

    This flag should only be used in modules deriving Generic instances that
    weren't considered appropriate for INLINE[1] annotations by heuristics of
    :ghc-flag:`-finline-generics`, yet you know that doing so would be
    beneficial.

    When enabled globally it will most likely lead to worse compile times and
    code size blowup without runtime performance gains.

.. ghc-flag:: -fsolve-constant-dicts
    :shortdesc: When solving constraints, try to eagerly solve
        super classes using available dictionaries. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-solve-constant-dicts
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

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

    :default: off but enabled by :ghc-flag:`-O2`.

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
    :reverse: -fstg-lift-lams-non-rec-args-any
    :category:

    :default: 5

    Create top-level non-recursive functions with at most <n> parameters
    while performing late lambda lifting. The default is 5, the number of
    available parameter registers on x86_64.

.. ghc-flag:: -fstg-lift-lams-rec-args
    :shortdesc: Create top-level recursive functions with at most <n>
        parameters while performing late lambda lifting.
    :type: dynamic
    :reverse: -fstg-lift-lams-rec-args-any
    :category:

    :default: 5

    Create top-level recursive functions with at most <n> parameters
    while performing late lambda lifting. The default is 5, the number of
    available parameter registers on x86_64.

.. ghc-flag:: -fstrictness
    :shortdesc: Turn on demand analysis.
        Implied by :ghc-flag:`-O`. Implies :ghc-flag:`-fworker-wrapper`
    :type: dynamic
    :reverse: -fno-strictness
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Turn on demand analysis.

    A *Demand* describes an evaluation context of an expression.  *Demand
    analysis* tries to find out what demands a function puts on its arguments
    when called: If an argument is scrutinised on every code path, the function
    is strict in that argument and GHC is free to use the more efficient
    call-by-value calling convention, as well as pass parameters unboxed.

    Apart from *strictness analysis*, demand analysis also performs *usage
    analysis*: Where *strict* translates to "evaluated at least once", usage
    analysis asks whether arguments and bindings are "evaluated at most once"
    or not at all ("evaluated at most zero times"), e.g. *absent*. For the
    former, GHC may use call-by-name instead of call-by-need, effectively
    turning thunks into non-memoised functions. For the latter, no code needs
    to be generated at all: An absent argument can simply be replaced by a
    dummy value at the call site or omitted altogether.

    The worker/wrapper transformation (:ghc-flag:`-fworker-wrapper`) is
    responsible for exploiting unboxing opportunities and replacing absent
    arguments by dummies. For arguments that can't be unboxed, opportunities
    for call-by-value and call-by-name are exploited in CorePrep when
    translating to STG.

    It's not only interesting to look at how often a binding is *evaluated*,
    but also how often a function *is called*. If a function is called at most
    once, we may freely eta-expand it, even if doing so destroys shared work
    if the function was called multiple times. This information translates
    into ``OneShotInfo`` annotations that the Simplifier acts on.

    **Notation**

    So demand analysis is about conservatively inferring lower and upper
    bounds about how many times something is evaluated/called. We call the
    "how many times" part a *cardinality*. In the compiler and debug output
    we differentiate the following cardinality intervals as approximations
    to cardinality:

    +----------+------------------------------+--------+---------------------------------------+
    | Interval | Set of denoted cardinalities | Syntax | Explanation tying syntax to semantics |
    +==========+==============================+========+=======================================+
    | [1,0]    | {}                           | ``B``  | Bottom element                        |
    +----------+------------------------------+--------+---------------------------------------+
    | [0,0]    | {0}                          | ``A``  | Absent                                |
    +----------+------------------------------+--------+---------------------------------------+
    | [0,1]    | {0,1}                        | ``M``  | Used at most once ("Maybe")           |
    +----------+------------------------------+--------+---------------------------------------+
    | [0,ω]    | {0,1,ω}                      | ``L``  | Lazy. Top element, no information,    |
    |          |                              |        | used at least 0, at most many times   |
    +----------+------------------------------+--------+---------------------------------------+
    | [1,1]    | {1}                          | ``1``  | Strict, used exactly once             |
    +----------+------------------------------+--------+---------------------------------------+
    | [1,ω]    | {1,ω}                        | ``S``  | Strict, used possibly many times      |
    +----------+------------------------------+--------+---------------------------------------+

    Note that it's never interesting to differentiate between a cardinality
    of 2 and 3, or even 4232123. We just approximate the >1 case with ω,
    standing for "many times".

    Apart from the cardinality describing *how often* an argument is evaluated,
    a demand also carries a *sub-demand*, describing *how deep* something
    is evaluated beyond a simple ``seq``-like evaluation.

    This is the full syntax for cardinalities, demands and sub-demands in BNF:

    .. code-block:: none

        card ::= B | A | M | L | 1 | S    semantics as in the table above

        d    ::= card sd                  card = how often, sd = how deep
              |  card                     abbreviation: Same as "card card"

        sd   ::= card                     polymorphic sub-demand, card at every level
              |  P(d,d,..)                product sub-demand
              |  C(card,sd)               call sub-demand

    For example, ``fst`` is strict in its argument, and also in the first
    component of the argument.  It will not evaluate the argument's second
    component. That is expressed by the demand ``1P(1L,A)``. The ``P`` is for
    "product sub-demand", which has a *demand* for each product field. The
    notation ``1L`` just says "evaluated strictly (``1``), with everything
    nested inside evaluated according to ``L``" -- e.g., no information,
    because that would depend on the evaluation context of the call site of
    ``fst``. The role of ``L`` in ``1L`` is that of a *polymorphic* sub-demand,
    being semantically equivalent to the sub-demand ``P(LP(..))``, which we
    simply abbreviate by the (consequently overloaded) cardinality notation
    ``L``.

    For another example, the expression ``x + 1`` evaluates ``x`` according to
    demand ``1P(L)``. We have seen single letters stand for cardinalities and
    polymorphic sub-demands, but what does the single letter ``L`` mean for a
    *demand*? Such a single letter demand simply expands to a cardinality and
    a polymorphic sub-demand of the same letter: E.g. ``L`` is equivalent to
    ``LL`` by expansion of the single letter demand, which is equivalent to
    ``LP(LP(..))``, so ``L``\s all the way down. It is always clear from
    context whether we talk about about a cardinality, sub-demand or demand.

    **Demand signatures**

    We summarise a function's demand properties in its *demand signature*.
    This is the general syntax:

    .. code-block:: none

        {x->dx,y->dy,z->dz...}<d1><d2><d3>...<dn>div
                ^              ^   ^   ^      ^   ^
                |              |   |   |      |   |
                |              \---+---+------/   |
                |                  |              |
           demand on free        demand on      divergence
             variables           arguments      information
         (omitted if empty)                     (omitted if
                                              no information)

    We summarise ``fst``'s demand properties in its *demand signature*
    ``<1P(1L,A)>``, which just says "If ``fst`` is applied to one argument,
    that argument is evaluated according to ``1P(1L,A)``". For another
    example, the demand signature of ``seq`` would be ``<1A><1L>`` and that of
    ``+`` would be ``<1P(L)><1P(L)>``.

    If not omitted, the divergence information can be ``b`` (surely diverges)
    or ``x`` (surely diverges or throws a precise exception).  For example,
    ``error`` has demand signature ``<S>b`` and ``throwIO`` (which is the
    only way to throw precise exceptions) has demand signature ``<_><L><L>x``
    (leaving out the complicated demand on the ``Exception`` dictionary).

    **Call sub-demands**

    Consider ``maybe``: ::

        maybe :: b -> (a -> b) -> Maybe a -> b
        maybe n _ Nothing  = n
        maybe _ s (Just a) = s a

    We give it demand signature ``<L><MC(M,L)><1L>``.  The ``C(M,L)`` is a *call
    sub-demand* that says "Called at most once, where the result is used
    according to ``L``". The expression ``f `seq` f 1`` puts ``f`` under
    demand ``SC(1,L)`` and serves as an example where the upper bound on
    evaluation cardinality doesn't coincide with that of the call cardinality.

    Cardinality is always relative to the enclosing call cardinality, so
    ``g 1 2 + g 3 4`` puts ``g`` under demand ``SC(S,C(1,L))``, which says
    "called multiple times (``S``), but every time it is called with one
    argument, it is applied exactly once to another argument (``1``)".

.. ghc-flag:: -fstrictness-before=⟨n⟩
    :shortdesc: Run an additional demand analysis before simplifier phase ⟨n⟩
    :type: dynamic
    :category:

    Run an additional demand analysis before simplifier phase ⟨n⟩.

.. ghc-flag:: -funbox-small-strict-fields
    :shortdesc: Flatten strict constructor fields with a pointer-sized
        representation. Implied by :ghc-flag:`-O`.
    :type: dynamic
    :reverse: -fno-unbox-small-strict-fields
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

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

    Note that for consistency constructor fields are unpacked on 32-bit platforms
    as if it we were compiling for a 64-bit target even if fields are larger
    than a pointer on those platforms.

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
    :shortdesc: This has been deprecated in GHC 9.0.1.
    :type: dynamic
    :category:

    This factor was deprecated in GHC 9.0.1. See :ghc-ticket:`15304` for
    details. Users who need to control inlining should rather consider
    :ghc-flag:`-funfolding-use-threshold=⟨n⟩`.

.. ghc-flag:: -funfolding-use-threshold=⟨n⟩
    :shortdesc: *default: 90.* Tweak unfolding settings.
    :type: dynamic
    :category:

    :default: 80

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

.. ghc-flag:: -funfolding-case-threshold=⟨n⟩
    :shortdesc: *default: 2.* Reduce inlining for cases nested deeper than n.
    :type: dynamic
    :category:

    :default: 2

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    GHC is in general quite eager to inline small functions. However sometimes
    these functions will be expanded by more inlining after inlining. Since
    they are now applied to "interesting" arguments. Even worse, their expanded
    form might reference again a small function, which will be inlined and expanded
    afterwards. This can repeat often and lead to explosive growth of programs.

    As it happened in #18730.

    Starting with GHC 9.0 we will be less eager to inline deep into nested cases.
    We achieve this by applying a inlining penalty that increases as the nesting
    gets deeper. However sometimes a specific (maybe quite high!) threshold of nesting
    is to be expected.

    In such cases this flag can be used to ignore the first ⟨n⟩ levels of nesting
    when computing the penalty.

    This flag in combination with :ghc-flag:`-funfolding-case-scaling=⟨n⟩` can
    be used to break inlining loops without disabling inlining completely. For
    this purpose a smaller value is more likely to break such loops although
    often adjusting the scaling is enough and preferably.

.. ghc-flag:: -funfolding-case-scaling=⟨n⟩
    :shortdesc: *default: 30.* Apply a penalty of (inlining_cost * `1/n`) for each level of case nesting.
    :type: dynamic
    :category:

    :default: 30

    .. index::
       single: inlining, controlling
       single: unfolding, controlling

    GHC is in general quite eager to inline small functions. However sometimes
    these functions will be expanded by more inlining after inlining. Since
    they are now applied to "interesting" arguments. Even worse, their expanded
    form might reference again a small function, which will be inlined and expanded
    afterwards. This can repeat often and lead to explosive growth of programs.

    As it happened in #18730.

    Starting with GHC 9.0 we will be less eager to inline deep into nested cases.
    We achieve this by applying a inlining penalty that increases as the nesting
    gets deeper. However sometimes we are ok with inlining a lot in the name of
    performance.

    In such cases this flag can be used to tune how hard we penalize inlining into
    deeply nested cases beyond the threshold set by :ghc-flag:`-funfolding-case-threshold=⟨n⟩`.
    Cases are only counted against the nesting level if they have more than one alternative.

    We use 1/n to scale the penalty. That is a higher value gives a lower penalty.

    This can be used to break inlining loops. For this purpose a lower value is
    recommended. Values in the range 10 <= n <= 20 allow some inlining to take place
    while still allowing GHC to compile modules containing such inlining loops.


.. ghc-flag:: -fworker-wrapper
    :shortdesc: Enable the worker/wrapper transformation. Implied by :ghc-flag:`-O`
        and by :ghc-flag:`-fstrictness`.
    :type: dynamic
    :category:

    :default: off but enabled by :ghc-flag:`-O`.

    Enable the worker/wrapper transformation after a demand analysis pass.

    Exploits strictness and absence information by unboxing strict arguments
    and replacing absent fields by dummy values in a wrapper function that
    will inline in all relevant scenarios and thus expose a specialised,
    unboxed calling convention of the worker function.

    Implied by :ghc-flag:`-O`, and by :ghc-flag:`-fstrictness`.
    Disabled by :ghc-flag:`-fno-strictness`. Enabling :ghc-flag:`-fworker-wrapper`
    while demand analysis is disabled (by :ghc-flag:`-fno-strictness`)
    has no effect.

.. ghc-flag:: -fworker-wrapper-cbv
    :shortdesc: Enable w/w splits for wrappers whos sole purpose is evaluating arguments.
    :type: dynamic
    :category: optimization

    Disabling this flag prevents a W/W split if the only benefit would be call-by-value
    for some arguments.

    Otherwise this exploits strictness information by passing strict value arguments
    call-by-value to the functions worker. Even for functions who would
    otherwise not get a worker.

    This avoids (potentially repeated) checks for evaluatedness of arguments in
    the rhs of the worker by pushing this check to the call site.
    If the argument is statically visible to be a value at the call site the
    overhead for the check disappears completely.

    This can cause slight codesize increases. It will also cause many more functions
    to get a worker/wrapper split which can play badly with rules (see :ghc-ticket:`20364`)
    which is why it's currently disabled by default.
    In particular if you depend on rules firing on functions marked as NOINLINE without
    marking use sites of these functions as INLINE or INLINEABLE then things will break
    unless this flag is disabled.

    While WorkerWrapper is disabled this has no effect.

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

.. ghc-flag:: -fwrite-if-compression=⟨n⟩
    :shortdesc: *default: 2.* Tweak the level of interface file compression.
    :type: dynamic
    :category: optimization

    :default: 2

    This flag defines the level of compression of interface files when writing to disk.
    The higher the flag, the more we deduplicate the interface file, at the cost of a higher compilation time.
    Deduplication (when applied to :ghc-flag:`--make` mode and :ghc-flag:`--interactive` mode) decreases the size of interface files as well as reducing
    the overall memory usage of GHC.

    Compression cannot be fully turned off, GHC always compresses interface files to a certain degree.
    Currently, we support values of ``1``, ``2`` and ``3``.
    Lower or higher values are clamped to ``1`` and ``3`` respectively.

    * ``1``: Compress as little as possible. No run-time impact, at the cost of interface file size and memory usage.
    * ``2``: Apply compression with minimal run-time overhead, reducing the interface file size and memory usage.
    * ``3``: Apply all possible compressions, minimal interface file sizes and memory usage, at the cost of run-time overhead.
