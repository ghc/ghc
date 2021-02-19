.. _sooner-faster-quicker:

Hints
=====

Please advise us of other "helpful hints" that should go here!

.. _sooner:

Sooner: producing a program more quickly
----------------------------------------

.. index::
   single: compiling faster
   single: faster compiling

Don't use :ghc-flag:`-O` or (especially) :ghc-flag:`-O2`:
    By using them, you are telling GHC that you are willing to suffer
    longer compilation times for better-quality code.

    GHC is surprisingly zippy for normal compilations without :ghc-flag:`-O`!

Use more memory:
    Within reason, more memory for heap space means less garbage
    collection for GHC, which means less compilation time. If you use
    the ``-Rghc-timing`` option, you'll get a garbage-collector report.
    (Again, you can use the cheap-and-nasty ``+RTS -S -RTS`` option to
    send the GC stats straight to standard error.)

    .. index::
       single: -H; RTS option

    If it says you're using more than 20% of total time in garbage collecting,
    then more memory might help: use the ``-H⟨size⟩`` (see
    :rts-flag:`-H [⟨size⟩]`) option. Increasing the default allocation area
    size used by the compiler's RTS might also help: use the ``+RTS -A⟨size⟩
    -RTS`` option (see :rts-flag:`-A ⟨size⟩`).

    .. index::
       single: -A⟨size⟩; RTS option

    If GHC persists in being a bad memory citizen, please report it as a
    bug.

Don't use too much memory!
    As soon as GHC plus its “fellow citizens” (other processes on your
    machine) start using more than the *real memory* on your machine,
    and the machine starts “thrashing,” *the party is over*. Compile
    times will be worse than terrible! Use something like the csh
    builtin :command:`time` command to get a report on how many page faults
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
    better job at producing fast code than the :ref:`native code generator
    <native-code-gen>`. This is not universal and depends on the code. Numeric
    heavy code seems to show the best improvement when compiled via LLVM. You
    can also experiment with passing specific flags to LLVM with the
    :ghc-flag:`-optlo ⟨option⟩` and :ghc-flag:`-optlc ⟨option⟩` flags. Be
    careful though as setting these flags stops GHC from setting its usual
    flags for the LLVM optimiser and compiler.

Overloaded functions are not your friend:
    Haskell's overloading (using type classes) is elegant, neat, etc.,
    etc., but it is death to performance if left to linger in an inner
    loop. How can you squash it?

Give explicit type signatures:
    Signatures are the basic trick; putting them on exported, top-level
    functions is good software-engineering practice, anyway. (Tip: using
    the :ghc-flag:`-Wmissing-signatures` option can
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
    :ghc-flag:`--show-iface ⟨file⟩` option (see :ref:`hi-options`).

    .. code-block:: sh

        $ ghc --show-iface Foo.hi | egrep '^[a-z].*::.*=>'

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

    A less contrived example shows the use of ``BangPatterns`` on
    ``lets`` to get stricter code (a good thing):

    ::

        f (Wibble x y)
              = let
                    !(a1, b1, c1) = unpackFoo x
                    !(a2, b2, c2) = unpackFoo y
                in ...

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
    :ghc-wiki:`the GHC Commentary <commentary/compiler/demand>`
    for a description of the strictness notation.

    For an "unpackable" ``U(...)`` argument, the info inside tells the
    strictness of its components. So, if the argument is a pair, and it
    says ``U(AU(LSS))``, that means “the first component of the pair
    isn't used; the second component is itself unpackable, with three
    components (lazy in the first, strict in the second \\& third).”

    If the function isn't exported, just compile with the extra flag
    :ghc-flag:`-ddump-simpl`; next to the signature for any binder, it will
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
    compilation with :ghc-flag:`-ddump-simpl` (don't forget the :ghc-flag:`-O`).

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
    :ghc-flag:`-funbox-strict-fields` (see :ref:`options-f`), a strict field can
    be unpacked or unboxed in the constructor, and one or more levels of
    indirection may be removed. Unpacking only happens for
    single-constructor datatypes (``Int`` is a good candidate, for
    example).

    Using :ghc-flag:`-funbox-strict-fields` is only really a good idea in
    conjunction with :ghc-flag:`-O`, because otherwise the extra packing and
    unpacking won't be optimised away. In fact, it is possible that
    :ghc-flag:`-funbox-strict-fields` may worsen performance even *with* :ghc-flag:`-O`,
    but this is unlikely (let us know if it happens to you).

Use unboxed types (a GHC extension):
    When you are *really* desperate for speed, and you want to get right
    down to the “raw bits.” Please see :ref:`glasgow-unboxed` for some
    information about using unboxed types.

    Before resorting to explicit unboxed types, try using strict
    constructor fields and :ghc-flag:`-funbox-strict-fields` first (see above).
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
    :array-ref:`Data.Array.Unboxed.` library for details. These arrays are
    likely to be much faster than using standard Haskell 98 arrays from the
    :array-ref:`Data.Array.` library.

Use a bigger heap!
    If your program's GC stats (:rts-flag:`-S [⟨file⟩]` RTS option) indicate
    that it's doing lots of garbage-collection (say, more than 20% of execution
    time), more memory might help — with the :rts-flag:`-H [⟨size⟩]` or
    :rts-flag:`-A ⟨size⟩` RTS options (see :ref:`rts-options-gc`). As a rule
    of thumb, try setting :rts-flag:`-H [⟨size⟩]` to the amount of memory
    you're willing to let your process consume, or perhaps try passing
    :rts-flag:`-H [⟨size⟩]` without any argument to let GHC calculate a value
    based on the amount of live data.

Compact your data:
    The :ghc-compact-ref:`GHC.Compact.` module
    provides a way to make garbage collection more efficient for
    long-lived data structures. Compacting a data structure collects
    the objects together in memory, where they are treated as a single
    object by the garbage collector and not traversed individually.

.. _smaller:

Smaller: producing a program that is smaller
--------------------------------------------

.. index::
   single: smaller programs, how to produce
   single: -funfolding-use-threshold0 option

Decrease the "go-for-it" threshold for unfolding smallish expressions.
Give a :ghc-flag:`-funfolding-use-threshold=0 <-funfolding-use-threshold=⟨n⟩>`
option for the extreme case. (“Only unfoldings with zero cost should proceed.”)
Warning: except in certain specialised cases (like Happy parsers) this is likely
to actually *increase* the size of your program, because unfolding generally
enables extra simplifying optimisations to be performed.

Avoid :base-ref:`Prelude.Read`.

Use :command:`strip` on your executables.

.. _thriftier:

Thriftier: producing a program that gobbles less heap space
-----------------------------------------------------------

.. index::
   single: memory, using less heap
   single: space-leaks, avoiding
   single: heap space, using less

"I think I have a space leak..."

Re-run your program with :rts-flag:`+RTS -S <-S [⟨file⟩]>`, and remove all
doubt! (You'll see the heap usage get bigger and bigger...) (Hmmm... this might
be even easier with the :rts-flag:`-G1 <-G ⟨generations⟩>` RTS option; so...
``./a.out +RTS -S -G1``)

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

.. _control-inlining:

Controlling inlining via optimisation flags.
--------------------------------------------

.. index::
    single: inlining, controlling
    single: unfolding, controlling

Inlining is one of the major optimizations GHC performs. Partially
because inlining often allows other optimizations to be triggered.
Sadly this is also a double edged sword. While inlining can often
cut through runtime overheads this usually comes at the cost
of not just program size, but also compiler performance. In
extreme cases making it impossible to compile certain code.

For this reason GHC offers various ways to tune inlining
behaviour.

Unfolding creation
~~~~~~~~~~~~~~~~~~

In order for a function from a different module to be inlined
GHC requires the functions unfolding. The following flags can
be used to control unfolding creation. Making their creation more
or less likely:

* :ghc-flag:`-fexpose-all-unfoldings`
* :ghc-flag:`-funfolding-creation-threshold=⟨n⟩`

Inlining decisions
~~~~~~~~~~~~~~~~~~

If a unfolding is available the following flags can impact GHC's
decision about inlining a specific binding.

* :ghc-flag:`-funfolding-use-threshold=⟨n⟩`
* :ghc-flag:`-funfolding-case-threshold=⟨n⟩`
* :ghc-flag:`-funfolding-case-scaling=⟨n⟩`
* :ghc-flag:`-funfolding-dict-discount=⟨n⟩`
* :ghc-flag:`-funfolding-fun-discount=⟨n⟩`

Should the simplifier run out of ticks because of a inlining loop
users are encouraged to try decreasing :ghc-flag:`-funfolding-case-threshold=⟨n⟩`
or :ghc-flag:`-funfolding-case-scaling=⟨n⟩` to limit inlining into
deeply nested expressions while allowing a higher tick factor.

The defaults of these are tuned such that we don't expect regressions for most
user programs. Using a :ghc-flag:`-funfolding-case-threshold=⟨n⟩` of 1-2 with a
:ghc-flag:`-funfolding-case-scaling=⟨n⟩` of 15-25 can cause usually small runtime
regressions but will prevent most inlining loops from getting out of control.

In extreme cases lowering scaling and treshold further can be useful, but at that
point it's very likely that beneficial inlining is prevented as well resulting
in significant runtime regressions.

In such cases it's recommended to move the problematic piece of code into it's own
module and changing inline parameters for the offending module only.

Inlining generics
~~~~~~~~~~~~~~~~~

There are also flags specific to the inlining of generics:

* :ghc-flag:`-finline-generics`
* :ghc-flag:`-finline-generics-aggressively`


.. _hints-os-memory:

Understanding how OS memory usage corresponds to live data
----------------------------------------------------------

A confusing aspect about the RTS is the sometimes big difference between
OS reported memory usage and
the amount of live data reported by heap profiling or ``GHC.Stats``.

There are two main factors which determine OS memory usage.

Firstly the collection strategy used by the oldest generation. By default a copying
strategy is used which requires at least 2 times the amount of currently live
data in order to perform a major collection. For example, if your program's live data
is 1G then you would expect the OS to report at minimum 2G.

If instead you are using the compacting (:rts-flag:`-c`) or nonmoving (:rts-flag:`-xn`) strategies
for the
oldest generation then less overhead is required as the strategy immediately
reuses already allocated memory by overwriting. For a program with heap size
1G then you might expect the OS to report at minimum a small percentage above 1G.

Secondly, after doing some allocation GHC is quite reluctant to return
the memory to the OS. This is because after performing a major collection the program might
still be allocating a lot and it costs to have to request
more memory. Therefore the RTS keeps an extra amount to reuse which
depends on the :rts-flag:`-F ⟨factor⟩` option. By default
the RTS will keep up to ``(2 + F) * live_bytes`` after performing a major collection due to
exhausting the available heap. The default value is ``F = 2`` so you
can see OS memory usage reported to be as high as 4 times the amount used by your
program.

Without further intervention, once your program has topped out at this high
threshold, no more memory would be returned to the OS so memory usage would always remain
at 4 times the live data. If you had a server with 1.5G live data, then if there was a memory
spike up to 6G for a short period, then OS reported memory would never dip below 6G. This
is what happened before GHC 9.2. In GHC 9.2 memory is gradually returned to the OS so OS memory
usage returns closer to the theoretical minimums.

The :rts-flag:`-Fd ⟨factor⟩` option controls the rate at which memory is returned to
the OS. On consecutive major collections which are not triggered by heap overflows, a
counter (``t``) is increased and the ``F`` factor is inversly scaled according to the
value of ``t`` and ``Fd``. The factor is scaled by the equation:

.. math::

  \texttt{F}' = \texttt{F} \times {2 ^ \frac{- \texttt{t}}{\texttt{Fd}}}

By default ``Fd = 4``, increasing ``Fd`` decreases the rate memory is returned.

Major collections which are not triggered by heap overflows arise mainly in two ways.

  1. Idle collections (controlled by :rts-flag:`-I  ⟨seconds⟩`)
  2. Explicit trigger using ``performMajorGC``.

For example, idle collections happen by default after 0.3 seconds of inactivity.
If you are running your application and have also set ``-Iw30``, so that the minimum
period between idle GCs is 30 seconds, then say you do a small amount of work every 5 seconds,
there will be about 10 idle collections about 5 minutes. This number of consecutive
idle collections will scale the ``F`` factor as follows:

.. math::

  \texttt{F}' = 2 \times {2^{\frac{-10}{4}}} \approx 0.35

and hence we will only retain ``(0.35 + 2) * live_bytes``
rather than the original 4 times. If you want less frequent idle collections then
you should also decrease ``Fd`` so that more memory is returned each time
a collection takes place.

If you set ``-Fd0`` then GHC will not attempt to return memory, which corresponds
with the behaviour from releases prior to 9.2. You probably don't want to do this as
unless you have idle periods in your program the behaviour will be similar anyway.
If you want to retain a specific amount of memory then it's better to set ``-H1G``
in order to communicate that you are happy with a heap size of ``1G``. If you do this
then OS reported memory will never decrease below this amount if it ever reaches this
threshold.

The collecting strategy also affects the fragmentation of the heap and hence how easy
it is to return memory to a theoretical baseline. Memory is allocated firstly
in the unit of megablocks which is then further divided into blocks. Block-level
fragmentation is how much unused space within the allocated megablocks there is.
In a fragmented heap there will be many megablocks which are only partially full.

In theory the compacting
strategy has a lower memory baseline but practically it can be hard to reach the
baseline due to how compacting never defragments. On the other hand, the copying
collecting has a higher theoretical baseline but we can often get very close to
it because the act of copying leads to lower fragmentation.

There are some other flags which affect the amount of retained memory as well.
Setting the maximum heap size using :rts-flag:`-M ⟨size⟩` will make sure we don't try
and retain more memory than the maximum size and explicitly setting :rts-flag:`-H [⟨size⟩]`
will mean that we will always try and retain at least ``H`` bytes irrespective of
the amount of live data.
