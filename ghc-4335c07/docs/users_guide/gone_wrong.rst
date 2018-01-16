.. _wrong:

What to do when something goes wrong
====================================

.. index::
   single: problems

If you still have a problem after consulting this section, then you may
have found a *bug*—please report it! See :ref:`bug-reporting` for
details on how to report a bug and a list of things we'd like to know
about your bug. If in doubt, send a report — we love mail from irate users
:-!

(:ref:`vs-Haskell-defn`, which describes Glasgow Haskell's shortcomings
vs. the Haskell language definition, may also be of interest.)

.. _wrong-compiler:

When the compiler “does the wrong thing”
----------------------------------------

.. index::
   single: compiler problems
   single: problems with the compiler

"Help! The compiler crashed (or panic'd)!"
    These events are *always* bugs in the GHC system—please report them.

"This is a terrible error message."
    If you think that GHC could have produced a better error message,
    please report it as a bug.

"What about this warning from the C compiler?"
    For example: ``…warning: \`Foo' declared \`static' but never
    defined.`` Unsightly, but shouldn't be a problem.

Sensitivity to ``.hi`` interface files
    GHC is very sensitive about interface files. For example, if it
    picks up a non-standard ``Prelude.hi`` file, pretty terrible things
    will happen. If you turn on
    ``-XNoImplicitPrelude``-XNoImplicitPrelude option, the compiler will
    almost surely die, unless you know what you are doing.

    Furthermore, as sketched below, you may have big problems running
    programs compiled using unstable interfaces.

"I think GHC is producing incorrect code"
    Unlikely :-) A useful be-more-paranoid option to give to GHC is
    ``-dcore-lint``-dcore-lint option; this causes a “lint” pass to
    check for errors (notably type errors) after each Core-to-Core
    transformation pass. We run with ``-dcore-lint`` on all the time; it
    costs about 5% in compile time.

Why did I get a link error?
    If the linker complains about not finding ``_<something>_fast``,
    then something is inconsistent: you probably didn't compile modules
    in the proper dependency order.

"Is this line number right?"
    On this score, GHC usually does pretty well, especially if you
    "allow" it to be off by one or two. In the case of an instance or
    class declaration, the line number may only point you to the
    declaration, not to a specific method.

    Please report line-number errors that you find particularly
    unhelpful.

.. _wrong-compile:

When your program “does the wrong thing”
----------------------------------------

.. index::
   single: problems running your program

(For advice about overly slow or memory-hungry Haskell programs, please
see :ref:`sooner-faster-quicker`).

"Help! My program crashed!"
    (e.g., a "segmentation fault" or "core dumped") segmentation fault

    If your program has no foreign calls in it, and no calls to
    known-unsafe functions (such as ``unsafePerformIO``) then a crash is
    always a BUG in the GHC system, except in one case: If your program
    is made of several modules, each module must have been compiled
    after any modules on which it depends (unless you use ``.hi-boot``
    files, in which case these *must* be correct with respect to the
    module source).

    For example, if an interface is lying about the type of an imported
    value then GHC may well generate duff code for the importing module.
    *This applies to pragmas inside interfaces too!* If the pragma is
    lying (e.g., about the “arity” of a value), then duff code may
    result. Furthermore, arities may change even if types do not.

    In short, if you compile a module and its interface changes, then
    all the modules that import that interface *must* be re-compiled.

    A useful option to alert you when interfaces change is
    ``-ddump-hi-diffs`` option. It will run ``diff`` on
    the changed interface file, before and after, when applicable.

    .. index::
       single: -ddump-hi-diffs

    If you are using ``make``, GHC can automatically generate the
    dependencies required in order to make sure that every module *is*
    up-to-date with respect to its imported interfaces. Please see
    :ref:`makefile-dependencies`.

    If you are down to your last-compile-before-a-bug-report, we would
    recommend that you add a ``-dcore-lint`` option (for extra checking)
    to your compilation options.

    So, before you report a bug because of a core dump, you should
    probably:

    ::

        % rm *.o        # scrub your object files
        % make my_prog  # re-make your program; use -ddump-hi-diffs to highlight changes;
                        # as mentioned above, use -dcore-lint to be more paranoid
        % ./my_prog ... # retry...

    Of course, if you have foreign calls in your program then all bets
    are off, because you can trash the heap, the stack, or whatever.

"My program entered an 'absent' argument."
    This is definitely caused by a bug in GHC. Please report it (see
    :ref:`bug-reporting`).

"What's with this arithmetic (or floating-point) exception?"
    ``Int``, ``Float``, and ``Double`` arithmetic is *unchecked*.
    Overflows, underflows and loss of precision are either silent or
    reported as an exception by the operating system (depending on the
    platform). Divide-by-zero *may* cause an untrapped exception (please
    report it if it does).
