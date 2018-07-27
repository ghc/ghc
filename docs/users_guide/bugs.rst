.. _bugs-and-infelicities:

Known bugs and infelicities
===========================

.. _vs-Haskell-defn:

Haskell standards vs. Glasgow Haskell: language non-compliance
--------------------------------------------------------------

.. index::
   single: GHC vs the Haskell standards
   single: Haskell standards vs GHC

This section lists Glasgow Haskell infelicities in its implementation of
Haskell 98 and Haskell 2010. See also the “when things go wrong” section
(:ref:`wrong`) for information about crashes, space leaks, and other
undesirable phenomena.

The limitations here are listed in Haskell Report order (roughly).

.. _haskell-standards-divergence:

Divergence from Haskell 98 and Haskell 2010
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, GHC mainly aims to behave (mostly) like a Haskell 2010
compiler, although you can tell it to try to behave like a particular
version of the language with the :ghc-flag:`-XHaskell98` and
:ghc-flag:`-XHaskell2010` flags. The known deviations from the standards are
described below. Unless otherwise stated, the deviation applies in Haskell 98,
Haskell 2010 and the default modes.

.. _infelicities-lexical:

Lexical syntax
^^^^^^^^^^^^^^

-  Certain lexical rules regarding qualified identifiers are slightly
   different in GHC compared to the Haskell report. When you have
   ⟨module⟩\ ``.``\ ⟨reservedop⟩, such as ``M.\``, GHC will interpret it
   as a single qualified operator rather than the two lexemes ``M`` and
   ``.\``.

.. _infelicities-syntax:

Context-free syntax
^^^^^^^^^^^^^^^^^^^

-  In Haskell 98 mode and by default (but not in Haskell 2010 mode), GHC
   is a little less strict about the layout rule when used in ``do``
   expressions. Specifically, the restriction that "a nested context
   must be indented further to the right than the enclosing context" is
   relaxed to allow the nested context to be at the same level as the
   enclosing context, if the enclosing context is a ``do`` expression.

   For example, the following code is accepted by GHC: ::

       main = do args <- getArgs
                 if null args then return [] else do
                 ps <- mapM process args
                 mapM print ps

   This behaviour is controlled by the ``NondecreasingIndentation``
   extension.

-  GHC doesn't do the fixity resolution in expressions during parsing as
   required by Haskell 98 (but not by Haskell 2010). For example,
   according to the Haskell 98 report, the following expression is
   legal: ::

           let x = 42 in x == 42 == True

   and parses as: ::

           (let x = 42 in x == 42) == True

   because according to the report, the ``let`` expression “extends as
   far to the right as possible”. Since it can't extend past the second
   equals sign without causing a parse error (``==`` is non-fix), the
   ``let``\-expression must terminate there. GHC simply gobbles up the
   whole expression, parsing like this: ::

           (let x = 42 in x == 42 == True)

.. _infelicities-exprs-pats:

Expressions and patterns
^^^^^^^^^^^^^^^^^^^^^^^^

In its default mode, GHC makes some programs slightly more defined than
they should be. For example, consider ::

    f :: [a] -> b -> b
    f [] = error "urk"
    f (x:xs) = \v -> v

    main = print (f [] `seq` True)

This should call ``error`` but actually prints ``True``. Reason: GHC
eta-expands ``f`` to

::

    f :: [a] -> b -> b
    f []     v = error "urk"
    f (x:xs) v = v

This improves efficiency slightly but significantly for most programs,
and is bad for only a few. To suppress this bogus "optimisation" use
``-fpedantic-bottoms``.

.. _infelicities-decls:

Declarations and bindings
^^^^^^^^^^^^^^^^^^^^^^^^^

In its default mode, GHC does not accept datatype contexts, as it has
been decided to remove them from the next version of the language
standard. This behaviour can be controlled with the ``DatatypeContexts``
extension. See :ref:`datatype-contexts`.

.. _infelicities-recursive-groups:

Typechecking of recursive binding groups
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Haskell Report specifies that a group of bindings (at top level, or
in a ``let`` or ``where``) should be sorted into strongly-connected
components, and then type-checked in dependency order
(`Haskell Report, Section
4.5.1 <http://www.haskell.org/onlinereport/decls.html#sect4.5.1>`__). As
each group is type-checked, any binders of the group that have an
explicit type signature are put in the type environment with the
specified polymorphic type, and all others are monomorphic until the
group is generalised (`Haskell Report, Section
4.5.2 <http://www.haskell.org/onlinereport/decls.html#sect4.5.2>`__).

Following a suggestion of Mark Jones, in his paper `Typing Haskell in
Haskell <https://web.cecs.pdx.edu/~mpj/thih/>`__, GHC implements a
more general scheme. In GHC *the dependency analysis ignores references to
variables that have an explicit type signature*. As a result of this refined
dependency analysis, the dependency groups are smaller, and more bindings will
typecheck. For example, consider: ::

      f :: Eq a => a -> Bool
      f x = (x == x) || g True || g "Yes"

      g y = (y <= y) || f True

This is rejected by Haskell 98, but under Jones's scheme the definition
for ``g`` is typechecked first, separately from that for ``f``, because
the reference to ``f`` in ``g``\'s right hand side is ignored by the
dependency analysis. Then ``g``\'s type is generalised, to get ::

      g :: Ord a => a -> Bool

Now, the definition for ``f`` is typechecked, with this type for ``g``
in the type environment.

The same refined dependency analysis also allows the type signatures of
mutually-recursive functions to have different contexts, something that is
illegal in Haskell 98 (Section 4.5.2, last sentence). GHC only insists that the
type signatures of a *refined* group have identical type signatures; in practice
this means that only variables bound by the same pattern binding must have the
same context. For example, this is fine: ::

      f :: Eq a => a -> Bool
      f x = (x == x) || g True

      g :: Ord a => a -> Bool
      g y = (y <= y) || f True

.. _infelicities-Modules:

Module system and interface files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GHC requires the use of ``hs-boot`` files to cut the recursive loops
among mutually recursive modules as described in
:ref:`mutual-recursion`. This more of an infelicity than a bug: the
Haskell Report says (`Section
5.7 <http://haskell.org/onlinereport/modules.html#sect5.7>`__)

    "Depending on the Haskell implementation used, separate compilation of
    mutually recursive modules may require that imported modules contain
    additional information so that they may be referenced before they are
    compiled. Explicit type signatures for all exported values may be
    necessary to deal with mutual recursion. The precise details of separate
    compilation are not defined by this Report."

.. _infelicities-numbers:

Numbers, basic types, and built-in classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Num`` superclasses
    The ``Num`` class does not have ``Show`` or ``Eq`` superclasses.


    You can make code that works with both Haskell98/Haskell2010 and GHC
    by:

    -  Whenever you make a ``Num`` instance of a type, also make
        ``Show`` and ``Eq`` instances, and

    -  Whenever you give a function, instance or class a ``Num t``
        constraint, also give it ``Show t`` and ``Eq t`` constraints.

``Bits`` superclass
    The ``Bits`` class does not have a ``Num`` superclass. It
    therefore does not have default methods for the ``bit``, ``testBit``
    and ``popCount`` methods.

    You can make code that works with both Haskell 2010 and GHC by:

    -  Whenever you make a ``Bits`` instance of a type, also make a
        ``Num`` instance, and

    -  Whenever you give a function, instance or class a ``Bits t``
        constraint, also give it a ``Num t`` constraint, and

    -  Always define the ``bit``, ``testBit`` and ``popCount`` methods
        in ``Bits`` instances.

``Read`` class methods
    The ``Read`` class has two extra methods, ``readPrec`` and
    ``readListPrec``, that are not found in the Haskell 2010 since they rely
    on the ``ReadPrec`` data type, which requires the :ghc-flag:`-XRankNTypes`
    extension. GHC also derives ``Read`` instances by implementing ``readPrec``
    instead of ``readsPrec``, and relies on a default implementation of
    ``readsPrec`` that is defined in terms of ``readPrec``. GHC adds these two
    extra methods simply because ``ReadPrec`` is more efficient than ``ReadS``
    (the type on which ``readsPrec`` is based).

``Monad`` superclass
    The ``Monad`` class has an ``Applicative`` superclass. You cannot write
    ``Monad`` instances that work for GHC and also for a Haskell 2010
    implementation that does not define ``Applicative``.

Extra instances
    The following extra instances are defined: ::

        instance Functor ((->) r)
        instance Monad ((->) r)
        instance Functor ((,) a)
        instance Functor (Either a)
        instance Monad (Either e)

Multiply-defined array elements not checked
    This code fragment should elicit a fatal error, but it does not: ::

        main = print (array (1,1) [(1,2), (1,3)])

    GHC's implementation of ``array`` takes the value of an array slot
    from the last (index,value) pair in the list, and does no checking
    for duplicates. The reason for this is efficiency, pure and simple.

.. _infelicities-Prelude:

In ``Prelude`` support
^^^^^^^^^^^^^^^^^^^^^^

Arbitrary-sized tuples
    Tuples are currently limited to size 100. However, standard
    instances for tuples (``Eq``, ``Ord``, ``Bounded``, ``Ix``, ``Read``,
    and ``Show``) are available *only* up to 16-tuples.

    This limitation is easily subvertible, so please ask if you get
    stuck on it.

``splitAt`` semantics
    ``Data.List.splitAt`` is more strict than specified in the Report.
    Specifically, the Report specifies that ::

       splitAt n xs = (take n xs, drop n xs)

    which implies that ::

       splitAt undefined undefined = (undefined, undefined)

    but GHC's implementation is strict in its first argument, so ::

       splitAt undefined [] = undefined

``Show``\ ing records
    The Haskell 2010 definition of ``Show`` stipulates that the rendered
    string should only include parentheses which are necessary to unambiguously
    parse the result. For historical reasons, ``Show`` instances derived by GHC
    include parentheses around records despite the fact that record syntax
    binds more tightly than function application; e.g., ::

        data Hello = Hello { aField :: Int } deriving (Show)

        -- GHC produces...
        show (Just (Hello {aField=42})) == "Just (Hello {aField=42})"

        -- whereas Haskell 2010 calls for...
        show (Just (Hello {aField=42})) == "Just Hello {aField=42}"

``Read``\ ing integers
    GHC's implementation of the ``Read`` class for integral types
    accepts hexadecimal and octal literals (the code in the Haskell 98
    report doesn't). So, for example, ::

        read "0xf00" :: Int

    works in GHC.

    A possible reason for this is that ``readLitChar`` accepts hex and
    octal escapes, so it seems inconsistent not to do so for integers
    too.

``isAlpha``
    The Haskell 98 definition of ``isAlpha`` is: ::

        isAlpha c = isUpper c || isLower c

    GHC's implementation diverges from the Haskell 98 definition in the
    sense that Unicode alphabetic characters which are neither upper nor
    lower case will still be identified as alphabetic by ``isAlpha``.

``hGetContents``
    Lazy I/O throws an exception if an error is encountered, in contrast
    to the Haskell 98 spec which requires that errors are discarded (see
    Section 21.2.2 of the Haskell 98 report). The exception thrown is
    the usual IO exception that would be thrown if the failing IO
    operation was performed in the IO monad, and can be caught by
    ``System.IO.Error.catch`` or ``Control.Exception.catch``.

.. _infelicities-ffi:

The Foreign Function Interface
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``hs_init()``, ``hs_exit()``
    The FFI spec requires the implementation to support re-initialising
    itself after being shut down with ``hs_exit()``, but GHC does not
    currently support that. See :ghc-ticket:`13693`.

    .. index::
        single: hs_init
        single: hs_exit

.. _infelicities-operator-sections:

Operator sections
^^^^^^^^^^^^^^^^^

The Haskell Report demands that, for infix operators ``%``, the following
identities hold:

::

    (% expr) = \x -> x % expr
    (expr %) = \x -> expr % x

However, the second law is violated in the presence of undefined operators,

::

    (%) = error "urk"
    (() %)         `seq` () -- urk
    (\x -> () % x) `seq` () -- OK, result ()

The operator section is treated like function application of an undefined
function, while the lambda form is in WHNF that contains an application of an
undefined function.

.. _haskell-98-2010-undefined:

GHC's interpretation of undefined behaviour in Haskell 98 and Haskell 2010
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This section documents GHC's take on various issues that are left
undefined or implementation specific in Haskell 98.

``Char``
    .. index::
       single: Char; size of

    Following the ISO-10646 standard, ``maxBound :: Char`` in GHC is
    ``0x10FFFF``.

``Int``
    .. index::
       single: Int; size of
       single: fromInteger function
       single: fromIntegral function

    In GHC the ``Int`` type follows the size of an address on the host
    architecture; in other words it holds 32 bits on a 32-bit machine,
    and 64-bits on a 64-bit machine.

    Arithmetic on ``Int`` is unchecked for overflowoverflow\ ``Int``, so
    all operations on ``Int`` happen modulo 2\ :sup:`⟨n⟩` where ⟨n⟩ is
    the size in bits of the ``Int`` type.

    The ``fromInteger`` (and hence also ``fromIntegral``) is a special case when
    converting to ``Int``. The value of ``fromIntegral x :: Int`` is
    given by taking the lower ⟨n⟩ bits of ``(abs x)``, multiplied by the
    sign of ``x`` (in 2's complement ⟨n⟩-bit arithmetic). This behaviour
    was chosen so that for example writing ``0xffffffff :: Int``
    preserves the bit-pattern in the resulting ``Int``.

    Negative literals, such as ``-3``, are specified by (a careful
    reading of) the Haskell Report as meaning
    ``Prelude.negate (Prelude.fromInteger 3)``. So ``-2147483648`` means
    ``negate (fromInteger 2147483648)``. Since ``fromInteger`` takes the
    lower 32 bits of the representation,
    ``fromInteger (2147483648::Integer)``, computed at type ``Int`` is
    ``-2147483648::Int``. The ``negate`` operation then overflows, but
    it is unchecked, so ``negate (-2147483648::Int)`` is just
    ``-2147483648``. In short, one can write ``minBound::Int`` as a
    literal with the expected meaning (but that is not in general
    guaranteed).

    The ``fromIntegral`` function also preserves bit-patterns when
    converting between the sized integral types (``Int8``, ``Int16``,
    ``Int32``, ``Int64`` and the unsigned ``Word`` variants), see the
    modules ``Data.Int`` and ``Data.Word`` in the library documentation.

Unchecked floating-point arithmetic
    Operations on ``Float`` and ``Double`` numbers are *unchecked* for
    overflow, underflow, and other sad occurrences. (note, however, that
    some architectures trap floating-point overflow and
    loss-of-precision and report a floating-point exception, probably
    terminating the program)

    .. index::
        single: floating-point exceptions.

.. _bugs:

Known bugs or infelicities
--------------------------

The bug tracker lists bugs that have been reported in GHC but not yet
fixed: see the `GHC Trac <http://ghc.haskell.org/trac/ghc/>`__. In
addition to those, GHC also has the following known bugs or
infelicities. These bugs are more permanent; it is unlikely that any of
them will be fixed in the short term.

.. _bugs-ghc:

Bugs in GHC
~~~~~~~~~~~

-  GHC's runtime system implements cooperative multitasking, with
   context switching potentially occurring only when a program
   allocates. This means that programs that do not allocate may never
   context switch. This is especially true of programs using STM, which
   may deadlock after observing inconsistent state. See :ghc-ticket:`367`
   for further discussion.

   If you are hit by this, you may want to compile the affected module
   with :ghc-flag:`-fno-omit-yields <-fomit-yields>` (see :ref:`options-f`).
   This flag ensures that yield points are inserted at every function entrypoint
   (at the expense of a bit of performance).

-  GHC does not allow you to have a data type with a context that
   mentions type variables that are not data type parameters. For
   example:

   ::

         data C a b => T a = MkT a

   so that ``MkT``\'s type is

   ::

         MkT :: forall a b. C a b => a -> T a

   In principle, with a suitable class declaration with a functional
   dependency, it's possible that this type is not ambiguous; but GHC
   nevertheless rejects it. The type variables mentioned in the context
   of the data type declaration must be among the type parameters of the
   data type.

-  GHC's inliner can be persuaded into non-termination using the
   standard way to encode recursion via a data type:

   ::

         data U = MkU (U -> Bool)

         russel :: U -> Bool
         russel u@(MkU p) = not $ p u

         x :: Bool
         x = russel (MkU russel)

   The non-termination is reported like this:

   .. code-block:: none

       ghc: panic! (the 'impossible' happened)
         (GHC version 8.2.1 for x86_64-unknown-linux):
           Simplifier ticks exhausted
         When trying UnfoldingDone x_alB
         To increase the limit, use -fsimpl-tick-factor=N (default 100)

   with the panic being reported no matter how high a
   :ghc-flag:`-fsimpl-tick-factor` you supply.

   We have never found another class of programs, other than this
   contrived one, that makes GHC diverge, and fixing the problem would
   impose an extra overhead on every compilation. So the bug remains
   un-fixed. There is more background in `Secrets of the GHC
   inliner <http://research.microsoft.com/~simonpj/Papers/inlining/>`__.

-  On 32-bit x86 platforms when using the native code generator, the
   :ghc-flag:`-fexcess-precision` option is always on.
   This means that floating-point calculations are non-deterministic,
   because depending on how the program is compiled (optimisation
   settings, for example), certain calculations might be done at 80-bit
   precision instead of the intended 32-bit or 64-bit precision.
   Floating-point results may differ when optimisation is turned on. In
   the worst case, referential transparency is violated, because for
   example ``let x = E1 in E2`` can evaluate to a different value than
   ``E2[E1/x]``.

   .. index::
      single: -msse2 option

   One workaround is to use the :ghc-flag:`-msse2` option (see
   :ref:`options-platform`), which generates code to use the SSE2
   instruction set instead of the x87 instruction set. SSE2 code uses
   the correct precision for all floating-point operations, and so gives
   deterministic results. However, note that this only works with
   processors that support SSE2 (Intel Pentium 4 or AMD Athlon 64 and
   later), which is why the option is not enabled by default. The
   libraries that come with GHC are probably built without this option,
   unless you built GHC yourself.

-  The :ghc-flag:`state hack <-fstate-hack>` optimization can result in
   non-obvious changes in evaluation ordering which may hide exceptions, even
   with :ghc-flag:`-fpedantic-bottoms` (see, e.g., :ghc-ticket:`7411`). For
   instance, ::

     import Control.Exception
     import Control.DeepSeq
     main = do
         evaluate (('a' : undefined) `deepseq` return () :: IO ())
         putStrLn "Hello"

   Compiling this program with ``-O`` results in ``Hello`` to be printed,
   despite the fact that ``evaluate`` should have bottomed. Compiling
   with ``-O -fno-state-hack`` results in the exception one would expect.

-  Programs compiled with :ghc-flag:`-fdefer-type-errors` may fail a bit
   more eagerly than one might expect. For instance, ::

     {-# OPTIONS_GHC -fdefer-type-errors #-}
     main = do
       putStrLn "Hi there."
       putStrLn True

   Will emit no output, despite the fact that the ill-typed term appears
   after the well-typed ``putStrLn "Hi there."``. See :ghc-ticket:`11197`.

-  Despite appearances ``*`` and ``Constraint`` aren't really distinct kinds
   in the compiler's internal representation and can be unified producing
   unexpected results. See :ghc-ticket:`11715` for one example.

-  Because of a toolchain limitation we are unable to support full Unicode paths
   on Windows. On Windows we support up to Latin-1. See :ghc-ticket:`12971` for more.

.. _bugs-ghci:

Bugs in GHCi (the interactive GHC)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  GHCi does not respect the ``default`` declaration in the module whose
   scope you are in. Instead, for expressions typed at the command line,
   you always get the default default-type behaviour; that is,
   ``default(Int,Double)``.

   It would be better for GHCi to record what the default settings in
   each module are, and use those of the 'current' module (whatever that
   is).

-  On Windows, there's a GNU ld/BFD bug whereby it emits bogus PE object
   files that have more than 0xffff relocations. When GHCi tries to load
   a package affected by this bug, you get an error message of the form

   .. code-block:: none

       Loading package javavm ... linking ... WARNING: Overflown relocation field (# relocs found: 30765)

   The last time we looked, this bug still wasn't fixed in the BFD
   codebase, and there wasn't any noticeable interest in fixing it when
   we reported the bug back in 2001 or so.

   The workaround is to split up the .o files that make up your package
   into two or more .o's, along the lines of how the ``base`` package does
   it.
