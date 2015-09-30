.. _sooner-faster-quicker:

Advice on: sooner, faster, smaller, thriftier
=============================================

Please advise us of other "helpful hints" that should go here!

.. _sooner:

Sooner: producing a program more quickly
----------------------------------------

.. index::
   single: compiling faster
   single: faster compiling

Don't use ``-O`` or (especially) ``-O2``:
    By using them, you are telling GHC that you are willing to suffer
    longer compilation times for better-quality code.

    GHC is surprisingly zippy for normal compilations without ``-O``!

Use more memory:
    Within reason, more memory for heap space means less garbage
    collection for GHC, which means less compilation time. If you use
    the ``-Rghc-timing`` option, you'll get a garbage-collector report.
    (Again, you can use the cheap-and-nasty ``+RTS -S -RTS`` option to
    send the GC stats straight to standard error.)

    .. index::
       single: -H; RTS option

    If it says you're using more than 20% of total time in garbage
    collecting, then more memory might help: use the ``-H⟨size⟩``
    option. Increasing the default allocation area size used by the
    compiler's RTS might also help: use the ``+RTS -A⟨size⟩ -RTS``
    option.

    .. index::
       single: -A⟨size⟩; RTS option

    If GHC persists in being a bad memory citizen, please report it as a
    bug.

Don't use too much memory!
    As soon as GHC plus its “fellow citizens” (other processes on your
    machine) start using more than the *real memory* on your machine,
    and the machine starts “thrashing,” *the party is over*. Compile
    times will be worse than terrible! Use something like the csh
    builtin ``time`` command to get a report on how many page faults
    you're getting.

    If you don't know what virtual memory, thrashing, and page faults
    are, or you don't know the memory configuration of your machine,
    *don't* try to be clever about memory use: you'll just make your
    life a misery (and for other people, too, probably).

Try to use local disks when linking:
    Because Haskell objects and libraries tend to be large, it can take
    many real seconds to slurp the bits to/from a remote filesystem.

    It would be quite sensible to *compile* on a fast machine using
    remotely-mounted disks; then *link* on a slow machine that had your
    disks directly mounted.

Don't derive/use ``Read`` unnecessarily:
    It's ugly and slow.

GHC compiles some program constructs slowly:
    We'd rather you reported such behaviour as a bug, so that we can try
    to correct it.

    .. index::
       single: -v; GHC option

    To figure out which part of the compiler is badly behaved, the
    ``-v2`` option is your friend.

.. _faster:

Faster: producing a program that runs quicker
---------------------------------------------

.. index::
   single: faster programs, how to produce

The key tool to use in making your Haskell program run faster are GHC's
profiling facilities, described separately in :ref:`profiling`. There is
*no substitute* for finding where your program's time/space is *really*
going, as opposed to where you imagine it is going.

Another point to bear in mind: By far the best way to improve a
program's performance *dramatically* is to use better algorithms. Once
profiling has thrown the spotlight on the guilty time-consumer(s), it
may be better to re-think your program than to try all the tweaks listed
below.

Another extremely efficient way to make your program snappy is to use
library code that has been Seriously Tuned By Someone Else. You *might*
be able to write a better quicksort than the one in ``Data.List``, but
it will take you much longer than typing ``import Data.List``.

Please report any overly-slow GHC-compiled programs. Since GHC doesn't
have any credible competition in the performance department these days
it's hard to say what overly-slow means, so just use your judgement! Of
course, if a GHC compiled program runs slower than the same program
compiled with NHC or Hugs, then it's definitely a bug.

Optimise, using ``-O`` or ``-O2``:
    This is the most basic way to make your program go faster.
    Compilation time will be slower, especially with ``-O2``.

    At present, ``-O2`` is nearly indistinguishable from ``-O``.

Compile via LLVM:
    The :ref:`LLVM code generator <llvm-code-gen>` can sometimes do a far
    better job at producing fast code than the :ref:`native code
    generator <native-code-gen>`. This is not universal and depends
    on the code. Numeric heavy code seems to show the best improvement
    when compiled via LLVM. You can also experiment with passing
    specific flags to LLVM with the ``-optlo`` and ``-optlc`` flags. Be
    careful though as setting these flags stops GHC from setting its
    usual flags for the LLVM optimiser and compiler.

Overloaded functions are not your friend:
    Haskell's overloading (using type classes) is elegant, neat, etc.,
    etc., but it is death to performance if left to linger in an inner
    loop. How can you squash it?

Give explicit type signatures:
    Signatures are the basic trick; putting them on exported, top-level
    functions is good software-engineering practice, anyway. (Tip: using
    ``-fwarn-missing-signatures``-fwarn-missing-signatures option can
    help enforce good signature-practice).

    The automatic specialisation of overloaded functions (with ``-O``)
    should take care of overloaded local and/or unexported functions.

Use ``SPECIALIZE`` pragmas:
    .. index::
       single: SPECIALIZE pragma

    .. index::
       single: overloading, death to

    Specialize the overloading on key functions in your program. See
    :ref:`specialize-pragma` and :ref:`specialize-instance-pragma`.

"But how do I know where overloading is creeping in?"
    A low-tech way: grep (search) your interface files for overloaded
    type signatures. You can view interface files using the
    ``--show-iface`` option (see :ref:`hi-options`).

    ::

                    % ghc --show-iface Foo.hi | egrep '^[a-z].*::.*=>'

Strict functions are your dear friends:
    And, among other things, lazy pattern-matching is your enemy.

    (If you don't know what a "strict function" is, please consult a
    functional-programming textbook. A sentence or two of explanation
    here probably would not do much good.)

    Consider these two code fragments:

    ::

                    f (Wibble x y) =  ... # strict

                    f arg = let { (Wibble x y) = arg } in ... # lazy

    The former will result in far better code.

    A less contrived example shows the use of ``cases`` instead of
    ``lets`` to get stricter code (a good thing):

    ::

                    f (Wibble x y)  # beautiful but slow
                    = let
                    (a1, b1, c1) = unpackFoo x
                    (a2, b2, c2) = unpackFoo y
                    in ...

                    f (Wibble x y)  # ugly, and proud of it
                    = case (unpackFoo x) of { (a1, b1, c1) ->
                    case (unpackFoo y) of { (a2, b2, c2) ->
                    ...
                    }}

GHC loves single-constructor data-types:
    It's all the better if a function is strict in a single-constructor
    type (a type with only one data-constructor; for example, tuples are
    single-constructor types).

Newtypes are better than datatypes:
    If your datatype has a single constructor with a single field, use a
    ``newtype`` declaration instead of a ``data`` declaration. The
    ``newtype`` will be optimised away in most cases.

"How do I find out a function's strictness?"
    Don't guess—look it up.

    Look for your function in the interface file, then for the third
    field in the pragma; it should say ``Strictness: ⟨string⟩``. The
    ⟨string⟩ gives the strictness of the function's arguments: see
    :ghc-wiki:`the GHC Commentary <Commentary/Compiler/Demand>`
    for a description of the strictness notation.

    For an "unpackable" ``U(...)`` argument, the info inside tells the
    strictness of its components. So, if the argument is a pair, and it
    says ``U(AU(LSS))``, that means “the first component of the pair
    isn't used; the second component is itself unpackable, with three
    components (lazy in the first, strict in the second \\& third).”

    If the function isn't exported, just compile with the extra flag
    ``-ddump-simpl``; next to the signature for any binder, it will
    print the self-same pragmatic information as would be put in an
    interface file. (Besides, Core syntax is fun to look at!)

Force key functions to be ``INLINE``\ d (esp. monads):
    Placing ``INLINE`` pragmas on certain functions that are used a lot
    can have a dramatic effect. See :ref:`inline-pragma`.

Explicit ``export`` list:
    If you do not have an explicit export list in a module, GHC must
    assume that everything in that module will be exported. This has
    various pessimising effects. For example, if a bit of code is
    actually *unused* (perhaps because of unfolding effects), GHC will
    not be able to throw it away, because it is exported and some other
    module may be relying on its existence.

    GHC can be quite a bit more aggressive with pieces of code if it
    knows they are not exported.

Look at the Core syntax!
    (The form in which GHC manipulates your code.) Just run your
    compilation with ``-ddump-simpl`` (don't forget the ``-O``).

    If profiling has pointed the finger at particular functions, look at
    their Core code. ``lets`` are bad, ``cases`` are good, dictionaries
    (``d.⟨Class⟩.⟨Unique⟩``) [or anything overloading-ish] are bad,
    nested lambdas are bad, explicit data constructors are good,
    primitive operations (e.g., ``eqInt#``) are good, ...

Use strictness annotations:
    Putting a strictness annotation (``!``) on a constructor field helps
    in two ways: it adds strictness to the program, which gives the
    strictness analyser more to work with, and it might help to reduce
    space leaks.

    It can also help in a third way: when used with
    ``-funbox-strict-fields`` (see :ref:`options-f`), a strict field can
    be unpacked or unboxed in the constructor, and one or more levels of
    indirection may be removed. Unpacking only happens for
    single-constructor datatypes (``Int`` is a good candidate, for
    example).

    Using ``-funbox-strict-fields`` is only really a good idea in
    conjunction with ``-O``, because otherwise the extra packing and
    unpacking won't be optimised away. In fact, it is possible that
    ``-funbox-strict-fields`` may worsen performance even *with* ``-O``,
    but this is unlikely (let us know if it happens to you).

Use unboxed types (a GHC extension):
    When you are *really* desperate for speed, and you want to get right
    down to the “raw bits.” Please see :ref:`glasgow-unboxed` for some
    information about using unboxed types.

    Before resorting to explicit unboxed types, try using strict
    constructor fields and ``-funbox-strict-fields`` first (see above).
    That way, your code stays portable.

Use ``foreign import`` (a GHC extension) to plug into fast libraries:
    This may take real work, but… There exist piles of massively-tuned
    library code, and the best thing is not to compete with it, but link
    with it.

    :ref:`ffi` describes the foreign function interface.

Don't use ``Float``\s:
    If you're using ``Complex``, definitely use ``Complex Double``
    rather than ``Complex Float`` (the former is specialised heavily,
    but the latter isn't).

    ``Floats`` (probably 32-bits) are almost always a bad idea, anyway,
    unless you Really Know What You Are Doing. Use ``Double``\s.
    There's rarely a speed disadvantage—modern machines will use the
    same floating-point unit for both. With ``Double``\s, you are much
    less likely to hang yourself with numerical errors.

    One time when ``Float`` might be a good idea is if you have a *lot*
    of them, say a giant array of ``Float``\s. They take up half the
    space in the heap compared to ``Doubles``. However, this isn't true
    on a 64-bit machine.

Use unboxed arrays (``UArray``)
    GHC supports arrays of unboxed elements, for several basic
    arithmetic element types including ``Int`` and ``Char``: see the
    ``Data.Array.Unboxed`` library for details. These arrays are likely
    to be much faster than using standard Haskell 98 arrays from the
    ``Data.Array`` library.

Use a bigger heap!
    If your program's GC stats (``-S`` RTS option) indicate that
    it's doing lots of garbage-collection (say, more than 20% of execution
    time), more memory might help — with the ``-H⟨size⟩`` or ``-A⟨size⟩`` RTS
    options (see :ref:`rts-options-gc`). As a rule of thumb, try setting
    ``-H⟨size⟩`` to the amount of memory you're willing to let your process
    consume, or perhaps try passing ``-H`` without any argument to let GHC
    calculate a value based on the amount of live data.

.. _smaller:

Smaller: producing a program that is smaller
--------------------------------------------

.. index::
   single: smaller programs, how to produce
   single: -funfolding-use-threshold0 option

Decrease the “go-for-it” threshold for unfolding smallish expressions.
Give a ``-funfolding-use-threshold0`` option for the extreme case.
(“Only unfoldings with zero cost should proceed.”) Warning: except in
certain specialised cases (like Happy parsers) this is likely to
actually *increase* the size of your program, because unfolding
generally enables extra simplifying optimisations to be performed.

Avoid ``Read``.

Use ``strip`` on your executables.

.. _thriftier:

Thriftier: producing a program that gobbles less heap space
-----------------------------------------------------------

.. index::
   single: memory, using less heap
   single: space-leaks, avoiding
   single: heap space, using less

"I think I have a space leak..."

Re-run your program with ``+RTS -S``, and remove all doubt! (You'll see the
heap usage get bigger and bigger...) [Hmmm... this might be even easier with
the ``-G1`` RTS option; so... ``./a.out +RTS -S -G1``...]

.. index::
    single: -G RTS option
    single: -S RTS option

Once again, the profiling facilities (:ref:`profiling`) are the basic
tool for demystifying the space behaviour of your program.

Strict functions are good for space usage, as they are for time, as
discussed in the previous section. Strict functions get right down to
business, rather than filling up the heap with closures (the system's
notes to itself about how to evaluate something, should it eventually be
required).
