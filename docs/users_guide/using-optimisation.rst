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

``-O*``
    .. index::
       single: -O\* not specified

    This is taken to mean: “Please compile quickly; I'm not
    over-bothered about compiled-code quality.” So, for example:
    ``ghc -c Foo.hs``

``-O0``
    .. index::
       single: -O0

    Means "turn off all optimisation", reverting to the same settings as
    if no ``-O`` options had been specified. Saying ``-O0`` can be
    useful if eg. ``make`` has inserted a ``-O`` on the command line
    already.

``-O``, ``-O1``
    .. index::
       single: -O option
       single: -O1 option
       single: optimise; normally

    Means: "Generate good-quality code without taking too long about
    it." Thus, for example: ``ghc -c -O Main.lhs``

``-O2``
    .. index::
       single: -O2 option
       single: optimise; aggressively

    Means: "Apply every non-dangerous optimisation, even if it means
       significantly longer compile times."

    The avoided "dangerous" optimisations are those that can make
    runtime or space *worse* if you're unlucky. They are normally turned
    on or off individually.

    At the moment, ``-O2`` is *unlikely* to produce better code than
    ``-O``.

``-Odph``
    .. index::
       single: -Odph
       single: optimise; DPH

    Enables all ``-O2`` optimisation, sets
    ``-fmax-simplifier-iterations=20`` and ``-fsimplifier-phases=3``.
    Designed for use with :ref:`Data Parallel Haskell (DPH) <dph>`.

We don't use a ``-O*`` flag for day-to-day work. We use ``-O`` to get
respectable speed; e.g., when we want to measure something. When we want
to go for broke, we tend to use ``-O2`` (and we go for lots of coffee
breaks).

The easiest way to see what ``-O`` (etc.) “really mean” is to run with
``-v``, then stand back in amazement.

.. _options-f:

``-f*``: platform-independent flags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: -f\* options (GHC)
   single: -fno-\* options (GHC)

These flags turn on and off individual optimisations. Flags marked as
*Enabled by default* are enabled by ``-O``, and as such you shouldn't
need to set any of them explicitly. A flag ``-fwombat`` can be negated
by saying ``-fno-wombat``. See :ref:`options-f-compact` for a compact
list.

``-fcase-merge``
    .. index::
       single: -fcase-merge

    *On by default.* Merge immediately-nested case expressions that
    scrutinse the same variable. For example,

    ::

          case x of
             Red -> e1
             _   -> case x of 
                      Blue -> e2
                      Green -> e3

    Is transformed to,

    ::
          case x of
             Red -> e1
             Blue -> e2
             Green -> e2

``-fcall-arity``
    .. index::
       single: -fcall-arity

    *On by default.*.

``-fcmm-elim-common-blocks``
    .. index::
       single: -felim-common-blocks

    *On by default.*. Enables the common block elimination optimisation
    in the code generator. This optimisation attempts to find identical
    Cmm blocks and eliminate the duplicates.

``-fcmm-sink``
    .. index::
       single: -fcmm-sink

    *On by default.*. Enables the sinking pass in the code generator.
    This optimisation attempts to find identical Cmm blocks and
    eliminate the duplicates attempts to move variable bindings closer
    to their usage sites. It also inlines simple expressions like
    literals or registers.

``-fcpr-off``
    .. index::
       single: -fcpr-Off

    Switch off CPR analysis in the demand analyser.

``-fcse``
    .. index::
       single: -fcse

    *On by default.*. Enables the common-sub-expression elimination
    optimisation. Switching this off can be useful if you have some
    ``unsafePerformIO`` expressions that you don't want commoned-up.

``-fdicts-cheap``
    .. index::
       single: -fdicts-cheap

    A very experimental flag that makes dictionary-valued expressions
    seem cheap to the optimiser.

``-fdicts-strict``
    .. index::
       single: -fdicts-strict

    Make dictionaries strict.

``-fdmd-tx-dict-sel``
    .. index::
       single: -fdmd-tx-dict-sel

    *On by default for ``-O0``, ``-O``, ``-O2``.*

    Use a special demand transformer for dictionary selectors.

``-fdo-eta-reduction``
    .. index::
       single: -fdo-eta-reduction

    *On by default.* Eta-reduce lambda expressions, if doing so gets rid
    of a whole group of lambdas.

``-fdo-lambda-eta-expansion``
    .. index::
       single: -fdo-lambda-eta-expansion

    *On by default.* Eta-expand let-bindings to increase their arity.

``-feager-blackholing``
    .. index::
       single: -feager-blackholing

    Usually GHC black-holes a thunk only when it switches threads. This
    flag makes it do so as soon as the thunk is entered. See `Haskell on
    a shared-memory
    multiprocessor <http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/>`__.

``-fexcess-precision``
    .. index::
       single: -fexcess-precision

    When this option is given, intermediate floating point values can
    have a *greater* precision/range than the final type. Generally this
    is a good thing, but some programs may rely on the exact
    precision/range of ``Float``/``Double`` values and should not use
    this option for their compilation.

    Note that the 32-bit x86 native code generator only supports
    excess-precision mode, so neither ``-fexcess-precision`` nor
    ``-fno-excess-precision`` has any effect. This is a known bug, see
    :ref:`bugs-ghc`.

``-fexpose-all-unfoldings``
    .. index::
       single: -fexpose-all-unfoldings

    An experimental flag to expose all unfoldings, even for very large
    or recursive functions. This allows for all functions to be inlined
    while usually GHC would avoid inlining larger functions.

``-ffloat-in``
    .. index::
       single: -ffloat-in

    *On by default.* Float let-bindings inwards, nearer their binding
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

``-ffull-laziness``
    .. index::
       single: -ffull-laziness

    *On by default.* Run the full laziness optimisation (also known as
    let-floating), which floats let-bindings outside enclosing lambdas,
    in the hope they will be thereby be computed less often. See
    `Let-floating: moving bindings to give faster programs
    (ICFP'96) <http://research.microsoft.com/en-us/um/people/simonpj/papers/float.ps.gz>`__.
    Full laziness increases sharing, which can lead to increased memory
    residency.

    .. note::
       GHC doesn't implement complete full-laziness. When
       optimisation in on, and ``-fno-full-laziness`` is not given, some
       transformations that increase sharing are performed, such as
       extracting repeated computations from a loop. These are the same
       transformations that a fully lazy implementation would do, the
       difference is that GHC doesn't consistently apply full-laziness, so
       don't rely on it.

``-ffun-to-thunk``
    .. index::
       single: -ffun-to-thunk

    Worker-wrapper removes unused arguments, but usually we do not
    remove them all, lest it turn a function closure into a thunk,
    thereby perhaps creating a space leak and/or disrupting inlining.
    This flag allows worker/wrapper to remove *all* value lambdas. Off
    by default.

``-fignore-asserts``
    .. index::
       single: -fignore-asserts

    *On by default.*. Causes GHC to ignore uses of the function
    ``Exception.assert`` in source code (in other words, rewriting
    ``Exception.assert p e`` to ``e`` (see :ref:`assertions`).

``-fignore-interface-pragmas``
    .. index::
       single: -fignore-interface-pragmas

    Tells GHC to ignore all inessential information when reading
    interface files. That is, even if ``M.hi`` contains unfolding or
    strictness information for a function, GHC will ignore that
    information.

``-flate-dmd-anal``
    .. index::
       single: -flate-dmd-anal

    Run demand analysis again, at the end of the simplification
    pipeline. We found some opportunities for discovering strictness
    that were not visible earlier; and optimisations like
    ``-fspec-constr`` can create functions with unused arguments which
    are eliminated by late demand analysis. Improvements are modest, but
    so is the cost. See notes on the :ghc-wiki:`Trac wiki page <LateDmd>`.

``-fliberate-case``
    .. index::
       single: -fliberate-case

    *Off by default, but enabled by -O2.* Turn on the liberate-case
    transformation. This unrolls recursive function once in its own RHS,
    to avoid repeated case analysis of free variables. It's a bit like
    the call-pattern specialiser (``-fspec-constr``) but for free
    variables rather than arguments.

``-fliberate-case-threshold=n``
    .. index::
       single: -fliberate-case-threshold

    *default: 2000.* Set the size threshold for the liberate-case
    transformation.

``-floopification``
    .. index::
       single: -floopification

    *On by default.*

    When this optimisation is enabled the code generator will turn all
    self-recursive saturated tail calls into local jumps rather than
    function calls.

``-fmax-inline-alloc-size=n``
    .. index::
       single: -fmax-inline-alloc-size

    *default: 128.* Set the maximum size of inline array allocations to n bytes.
    GHC will allocate non-pinned arrays of statically known size in the current
    nursery block if they're no bigger than n bytes, ignoring GC overheap. This
    value should be quite a bit smaller than the block size (typically: 4096).

``-fmax-inline-memcpy-insn=n``
    .. index::
       single: -fmax-inline-memcpy-insn

    *default: 32.* Inline ``memcpy`` calls if they would generate no more than n pseudo
    instructions.

``-fmax-inline-memset-insns=n``
    .. index::
       single: -fmax-inline-memset-insns

    *default: 32.* Inline ``memset`` calls if they would generate no more than n pseudo
    instructions.

``-fmax-relevant-binds=n``
    .. index::
       single: -fmax-relevant-bindings

    The type checker sometimes displays a fragment of the type
    environment in error messages, but only up to some maximum number,
    set by this flag. The default is 6. Turning it off with
    ``-fno-max-relevant-bindings`` gives an unlimited number.
    Syntactically top-level bindings are also usually excluded (since
    they may be numerous), but ``-fno-max-relevant-bindings`` includes
    them too.

``-fmax-simplifier-iterations=n``
    .. index::
       single: -fmax-simplifier-iterations

    *default: 4.* Sets the maximal number of iterations for the simplifier.

``-fmax-worker-args=n``
    .. index::
       single: -fmax-worker-args

    *default: 10.* If a worker has that many arguments, none will be unpacked
     anymore.

``-fno-opt-coercion``
    .. index::
       single: -fno-opt-coercion

    Turn off the coercion optimiser.

``-fno-pre-inlining``
    .. index::
       single: -fno-pre-inlining

    Turn off pre-inlining.

``-fno-state-hack``
    .. index::
       single: -fno-state-hack

    Turn off the "state hack" whereby any lambda with a ``State#`` token
    as argument is considered to be single-entry, hence it is considered
    OK to inline things inside it. This can improve performance of IO
    and ST monad code, but it runs the risk of reducing sharing.

``-fomit-interface-pragmas``
    .. index::
       single: -fomit-interface-pragmas

    Tells GHC to omit all inessential information from the interface
    file generated for the module being compiled (say M). This means
    that a module importing M will see only the *types* of the functions
    that M exports, but not their unfoldings, strictness info, etc.
    Hence, for example, no function exported by M will be inlined into
    an importing module. The benefit is that modules that import M will
    need to be recompiled less often (only when M's exports change their
    type, not when they change their implementation).

``-fomit-yields``
    .. index::
       single: -fomit-yields

    *On by default.* Tells GHC to omit heap checks when no allocation is
    being performed. While this improves binary sizes by about 5%, it
    also means that threads run in tight non-allocating loops will not
    get preempted in a timely fashion. If it is important to always be
    able to interrupt such threads, you should turn this optimization
    off. Consider also recompiling all libraries with this optimization
    turned off, if you need to guarantee interruptibility.

``-fpedantic-bottoms``
    .. index::
       single: -fpedantic-bottoms

    Make GHC be more precise about its treatment of bottom (but see also
    ``-fno-state-hack``). In particular, stop GHC eta-expanding through
    a case expression, which is good for performance, but bad if you are
    using ``seq`` on partial applications.

``-fregs-graph``
    .. index::
       single: -fregs-graph

    *Off by default due to a performance regression bug. Only applies in
    combination with the native code generator.* Use the graph colouring
    register allocator for register allocation in the native code
    generator. By default, GHC uses a simpler, faster linear register
    allocator. The downside being that the linear register allocator
    usually generates worse code.

``-fregs-iterative``
    .. index::
       single: -fregs-iterative

    *Off by default, only applies in combination with the native code
    generator.* Use the iterative coalescing graph colouring register
    allocator for register allocation in the native code generator. This
    is the same register allocator as the ``-fregs-graph`` one but also
    enables iterative coalescing during register allocation.

``-fsimplifier-phases=n``
    .. index::
       single: -fsimplifier-phases

    *default: 2.* Set the number of phases for the simplifier. Ignored
    with -O0.

``-fsimpl-tick-factor=n``
    .. index::
       single: -fsimpl-tick-factor

    *default: 100.* GHC's optimiser can diverge if you write rewrite rules
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

``-fspec-constr``
    .. index::
       single: -fspec-constr

    *Off by default, but enabled by -O2.* Turn on call-pattern
    specialisation; see `Call-pattern specialisation for Haskell
    programs <http://research.microsoft.com/en-us/um/people/simonpj/papers/spec-constr/index.htm>`__.

    This optimisation specializes recursive functions according to their
    argument "shapes". This is best explained by example so consider:

    ::

        last :: [a] -> a
        last [] = error "last"
        last (x : []) = x
        last (x : xs) = last xs

    In this code, once we pass the initial check for an empty list we
    know that in the recursive case this pattern match is redundant. As
    such ``-fspec-constr`` will transform the above code to:

    ::

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
    ``vector`` library:

    ::

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

``-fspec-constr-count=n``
    .. index::
       single: -fspec-constr-count

    *default: 3.* Set the maximum number of specialisations that will be created for
    any one function by the SpecConstr transformation.

``-fspec-constr-threshold=n``
    .. index::
       single: -fspec-constr-threshold

    *default: 2000.* Set the size threshold for the SpecConstr transformation.

``-fspecialise``
    .. index::
       single: -fspecialise

    *On by default.* Specialise each type-class-overloaded function
    defined in this module for the types at which it is called in this
    module. If ``-fcross-module-specialise`` is set imported functions
    that have an INLINABLE pragma (:ref:`inlinable-pragma`) will be
    specialised as well.

``-fcross-module-specialise``
    .. index::
       single: -fcross-module-specialise

    *On by default.* Specialise ``INLINABLE`` (:ref:`inlinable-pragma`)
    type-class-overloaded functions imported from other modules for the types at
    which they are called in this module. Note that specialisation must be
    enabled (by ``-fspecialise``) for this to have any effect.

``-fstatic-argument-transformation``
    .. index::
       single: -fstatic-argument-transformation

    Turn on the static argument transformation, which turns a recursive
    function into a non-recursive one with a local recursive loop. See
    Chapter 7 of `Andre Santos's PhD
    thesis <http://research.microsoft.com/en-us/um/people/simonpj/papers/santos-thesis.ps.gz>`__

``-fstrictness``
    .. index::
       single: -fstrictness

    *On by default.*. Switch on the strictness analyser. There is a very
    old paper about GHC's strictness analyser, `Measuring the
    effectiveness of a simple strictness
    analyser <http://research.microsoft.com/en-us/um/people/simonpj/papers/simple-strictnes-analyser.ps.gz>`__,
    but the current one is quite a bit different.

    The strictness analyser figures out when arguments and variables in
    a function can be treated 'strictly' (that is they are always
    evaluated in the function at some point). This allow GHC to apply
    certain optimisations such as unboxing that otherwise don't apply as
    they change the semantics of the program when applied to lazy
    arguments.

``-fstrictness-before=⟨n⟩``
    .. index::
       single: -fstrictness-before

    Run an additional strictness analysis before simplifier phase ⟨n⟩.

``-funbox-small-strict-fields``
    .. index::
       single: -funbox-small-strict-fields
       single: strict constructor fields
       single: constructor fields, strict

    *On by default.*. This option causes all constructor fields which
    are marked strict (i.e. “!”) and which representation is smaller or
    equal to the size of a pointer to be unpacked, if possible. It is
    equivalent to adding an ``UNPACK`` pragma (see :ref:`unpack-pragma`)
    to every strict constructor field that fulfils the size restriction.

    For example, the constructor fields in the following data types

    ::

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

``-funbox-strict-fields``
    .. index::
       single: -funbox-strict-fields
       single: strict constructor fields
       single: constructor fields, strict

    This option causes all constructor fields which are marked strict
    (i.e. “!”) to be unpacked if possible. It is equivalent to adding an
    ``UNPACK`` pragma to every strict constructor field (see
    :ref:`unpack-pragma`).

    This option is a bit of a sledgehammer: it might sometimes make
    things worse. Selectively unboxing fields by using ``UNPACK``
    pragmas might be better. An alternative is to use
    ``-funbox-strict-fields`` to turn on unboxing by default but disable
    it for certain constructor fields using the ``NOUNPACK`` pragma (see
    :ref:`nounpack-pragma`).

``-funfolding-creation-threshold=n``
    .. index::
       single: -funfolding-creation-threshold
       single: inlining, controlling
       single: unfolding, controlling

    *default: 750.* Governs the maximum size that GHC will allow a
    function unfolding to be. (An unfolding has a “size” that reflects
    the cost in terms of “code bloat” of expanding (aka inlining) that
    unfolding at a call site. A bigger function would be assigned a
    bigger cost.)

    Consequences: (a) nothing larger than this will be inlined (unless
    it has an INLINE pragma); (b) nothing larger than this will be
    spewed into an interface file.

    Increasing this figure is more likely to result in longer compile
    times than faster code. The ``-funfolding-use-threshold`` is more
    useful.

``-funfolding-dict-discount=n``
    .. index::
       single: -funfolding-dict-discount
       single: inlining, controlling
       single: unfolding, controlling

    Default: 30

``-funfolding-fun-discount=n``
    .. index::
       single: -funfolding-fun-discount
       single: inlining, controlling
       single: unfolding, controlling

    Default: 60

``-funfolding-keeness-factor=n``
    .. index::
       single: -funfolding-keeness-factor
       single: inlining, controlling
       single: unfolding, controlling

    Default: 1.5

``-funfolding-use-threshold=n``
    .. index::
       single: -funfolding-use-threshold
       single: inlining, controlling
       single: unfolding, controlling

    *default: 60.* This is the magic cut-off figure for unfolding (aka
    inlining): below this size, a function definition will be unfolded
    at the call-site, any bigger and it won't. The size computed for a
    function depends on two things: the actual size of the expression
    minus any discounts that apply depending on the context into which
    the expression is to be inlined.

    The difference between this and ``-funfolding-creation-threshold``
    is that this one determines if a function definition will be inlined
    *at a call site*. The other option determines if a function
    definition will be kept around at all for potential inlining.

``-fvectorisation-avoidance``
    .. index::
       single: -fvectorisation-avoidance

    Part of :ref:`Data Parallel Haskell (DPH) <dph>`.

    *On by default.* Enable the *vectorisation* avoidance optimisation.
    This optimisation only works when used in combination with the
    ``-fvectorise`` transformation.

    While vectorisation of code using DPH is often a big win, it can
    also produce worse results for some kinds of code. This optimisation
    modifies the vectorisation transformation to try to determine if a
    function would be better of unvectorised and if so, do just that.

``-fvectorise``
    .. index::
       single: -fvectorise

    Part of :ref:`Data Parallel Haskell (DPH) <dph>`.

    *Off by default.* Enable the *vectorisation* optimisation
    transformation. This optimisation transforms the nested data
    parallelism code of programs using DPH into flat data parallelism.
    Flat data parallel programs should have better load balancing,
    enable SIMD parallelism and friendlier cache behaviour.
