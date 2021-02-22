.. _pragmas:

Pragmas
=======

.. index::
   single: pragma

GHC supports several pragmas, or instructions to the compiler placed in
the source code. Pragmas don't normally affect the meaning of the
program, but they might affect the efficiency of the generated code.

Pragmas all take the form ``{-# word ... #-}`` where ⟨word⟩ indicates
the type of pragma, and is followed optionally by information specific
to that type of pragma. Case is ignored in ⟨word⟩. The various values
for ⟨word⟩ that GHC understands are described in the following sections;
any pragma encountered with an unrecognised ⟨word⟩ is ignored. The
layout rule applies in pragmas, so the closing ``#-}`` should start in a
column to the right of the opening ``{-#``.

Certain pragmas are *file-header pragmas*:

-  A file-header pragma must precede the ``module`` keyword in the file.

-  There can be as many file-header pragmas as you please, and they can
   be preceded or followed by comments.

-  File-header pragmas are read once only, before pre-processing the
   file (e.g. with cpp).

-  The file-header pragmas are: ``{-# LANGUAGE #-}``,
   ``{-# OPTIONS_GHC #-}``, and ``{-# INCLUDE #-}``.

.. _language-pragma:

``LANGUAGE`` pragma
-------------------

.. pragma:: LANGUAGE ⟨ext⟩, ⟨ext⟩, ...

    :where: file header

    Enable or disable a set of language extensions.

The ``LANGUAGE`` pragma allows language extensions to be enabled in a
portable way. It is the intention that all Haskell compilers support the
``LANGUAGE`` pragma with the same syntax, although not all extensions
are supported by all compilers, of course. The ``LANGUAGE`` pragma
should be used instead of ``OPTIONS_GHC``, if possible.

For example, to enable the FFI and preprocessing with CPP: ::

    {-# LANGUAGE ForeignFunctionInterface, CPP #-}

``LANGUAGE`` is a file-header pragma (see :ref:`pragmas`).

Every language extension can also be turned into a command-line flag by
prefixing it with "``-X``"; for example ``-XForeignFunctionInterface``.
(Similarly, all "``-X``" flags can be written as ``LANGUAGE`` pragmas.)

A list of all supported language extensions can be obtained by invoking
``ghc --supported-extensions`` (see :ghc-flag:`--supported-extensions`).

Any extension from the ``Extension`` type defined in
:cabal-ref:`Language.Haskell.Extension.` may be used. GHC will report an error
if any of the requested extensions are not supported.

.. _options-pragma:

``OPTIONS_GHC`` pragma
----------------------

.. pragma:: OPTIONS_GHC ⟨flags⟩

    :where: file header

The ``OPTIONS_GHC`` pragma is used to specify additional options that
are given to the compiler when compiling this source file. See
:ref:`source-file-options` for details.

Previous versions of GHC accepted ``OPTIONS`` rather than
``OPTIONS_GHC``, but that is now deprecated.

``OPTIONS_GHC`` is a file-header pragma (see :ref:`pragmas`).

.. _include-pragma:

``INCLUDE`` pragma
------------------

The ``INCLUDE`` used to be necessary for specifying header files to be
included when using the FFI and compiling via C. It is no longer
required for GHC, but is accepted (and ignored) for compatibility with
other compilers.

.. _warning-deprecated-pragma:

``WARNING`` and ``DEPRECATED`` pragmas
--------------------------------------

.. pragma:: WARNING

    :where: declaration

    The ``WARNING`` pragma allows you to attach an arbitrary warning to a
    particular function, class, or type.

.. pragma:: DEPRECATED

    :where: declaration

    A ``DEPRECATED`` pragma lets you specify that a particular function, class,
    or type is deprecated.

There are two ways of using these pragmas.

-  You can work on an entire module thus: ::

          module Wibble {-# DEPRECATED "Use Wobble instead" #-} where
            ...

   Or: ::

          module Wibble {-# WARNING "This is an unstable interface." #-} where
            ...

   When you compile any module that import ``Wibble``, GHC will print
   the specified message.

-  You can attach a warning to a function, class, type, or data
   constructor, with the following top-level declarations: ::

          {-# DEPRECATED f, C, T "Don't use these" #-}
          {-# WARNING unsafePerformIO "This is unsafe; I hope you know what you're doing" #-}

   When you compile any module that imports and uses any of the
   specified entities, GHC will print the specified message.

   You can only attach to entities declared at top level in the module
   being compiled, and you can only use unqualified names in the list of
   entities. A capitalised name, such as ``T`` refers to *either* the
   type constructor ``T`` *or* the data constructor ``T``, or both if
   both are in scope. If both are in scope, there is currently no way to
   specify one without the other (c.f. fixities :ref:`infix-tycons`).

Also note that the argument to ``DEPRECATED`` and ``WARNING`` can also be a list
of strings, in which case the strings will be presented on separate lines in the
resulting warning message, ::

    {-# DEPRECATED foo, bar ["Don't use these", "Use gar instead"] #-}

Warnings and deprecations are not reported for (a) uses within the
defining module, (b) defining a method in a class instance, and (c) uses
in an export list. The latter reduces spurious complaints within a
library in which one module gathers together and re-exports the exports
of several others.

You can suppress the warnings with the flag
:ghc-flag:`-Wno-warnings-deprecations <-Wwarnings-deprecations>`.

.. _minimal-pragma:

``MINIMAL`` pragma
------------------

.. pragma:: MINIMAL ⟨name⟩ | ⟨name⟩ , ...

    :where: in class body

    Define the methods needed for a minimal complete instance of a class.

The ``MINIMAL`` pragma is used to specify the minimal complete definition of
a class, i.e. specify which methods must be implemented by all
instances. If an instance does not satisfy the minimal complete
definition, then a warning is generated. This can be useful when a class
has methods with circular defaults. For example ::

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)
        {-# MINIMAL (==) | (/=) #-}

Without the ``MINIMAL`` pragma no warning would be generated for an instance
that implements neither method.

The syntax for minimal complete definition is: ::

    mindef ::= name
            |  '(' mindef ')'
            |  mindef '|' mindef
            |  mindef ',' mindef

A vertical bar denotes disjunction, i.e. one of the two sides is
required. A comma denotes conjunction, i.e. both sides are required.
Conjunction binds stronger than disjunction.

If no ``MINIMAL`` pragma is given in the class declaration, it is just as if
a pragma ``{-# MINIMAL op1, op2, ..., opn #-}`` was given, where the
``opi`` are the methods that lack a default method in the class
declaration (c.f. :ghc-flag:`-Wmissing-methods`, :ref:`options-sanity`).

This warning can be turned off with the flag
:ghc-flag:`-Wno-missing-methods <-Wmissing-methods>`.

.. _inline-noinline-pragma:

``INLINE`` and ``NOINLINE`` pragmas
-----------------------------------

These pragmas control the inlining of function definitions.

.. _inline-pragma:

``INLINE`` pragma
~~~~~~~~~~~~~~~~~

.. pragma:: INLINE ⟨name⟩

    :where: top-level

    Force GHC to inline a value.

GHC (with :ghc-flag:`-O`, as always) tries to inline (or "unfold")
functions/values that are "small enough," thus avoiding the call
overhead and possibly exposing other more-wonderful optimisations. GHC
has a set of heuristics, tuned over a long period of time using many
benchmarks, that decide when it is beneficial to inline a function at
its call site. The heuristics are designed to inline functions when it
appears to be beneficial to do so, but without incurring excessive code
bloat. If a function looks too big, it won't be inlined, and functions
larger than a certain size will not even have their definition exported
in the interface file. Some of the thresholds that govern these
heuristic decisions can be changed using flags, see :ref:`options-f`.

Normally GHC will do a reasonable job of deciding by itself when it is a
good idea to inline a function. However, sometimes you might want to
override the default behaviour. For example, if you have a key function
that is important to inline because it leads to further optimisations,
but GHC judges it to be too big to inline.

The sledgehammer you can bring to bear is the ``INLINE`` pragma, used thusly: ::

    key_function :: Int -> String -> (Bool, Double)
    {-# INLINE key_function #-}

The major effect of an ``INLINE`` pragma is to declare a function's
"cost" to be very low. The normal unfolding machinery will then be very
keen to inline it. However, an ``INLINE`` pragma for a function "``f``"
has a number of other effects:

-  While GHC is keen to inline the function, it does not do so blindly.
   For example, if you write ::

       map key_function xs

   there really isn't any point in inlining ``key_function`` to get ::

       map (\x -> body) xs

   In general, GHC only inlines the function if there is some reason (no
   matter how slight) to suppose that it is useful to do so.

-  Moreover, GHC will only inline the function if it is *fully applied*,
   where "fully applied" means applied to as many arguments as appear
   (syntactically) on the LHS of the function definition. For example: ::

       comp1 :: (b -> c) -> (a -> b) -> a -> c
       {-# INLINE comp1 #-}
       comp1 f g = \x -> f (g x)

       comp2 :: (b -> c) -> (a -> b) -> a -> c
       {-# INLINE comp2 #-}
       comp2 f g x = f (g x)

   The two functions ``comp1`` and ``comp2`` have the same semantics,
   but ``comp1`` will be inlined when applied to *two* arguments, while
   ``comp2`` requires *three*. This might make a big difference if you
   say ::

       map (not `comp1` not) xs

   which will optimise better than the corresponding use of ``comp2``.

-  It is useful for GHC to optimise the definition of an INLINE function
   ``f`` just like any other non-``INLINE`` function, in case the
   non-inlined version of ``f`` is ultimately called. But we don't want
   to inline the *optimised* version of ``f``; a major reason for ``INLINE``
   pragmas is to expose functions in ``f``\'s RHS that have rewrite
   rules, and it's no good if those functions have been optimised away.

   So *GHC guarantees to inline precisely the code that you wrote*, no
   more and no less. It does this by capturing a copy of the definition
   of the function to use for inlining (we call this the "inline-RHS"),
   which it leaves untouched, while optimising the ordinarily RHS as
   usual. For externally-visible functions the inline-RHS (not the
   optimised RHS) is recorded in the interface file.

-  An ``INLINE`` function is not worker/wrappered by strictness analysis.
   It's going to be inlined wholesale instead.

GHC ensures that inlining cannot go on forever: every mutually-recursive
group is cut by one or more *loop breakers* that is never inlined (see
`Secrets of the GHC inliner, JFP 12(4) July
2002 <http://research.microsoft.com/%7Esimonpj/Papers/inlining/index.htm>`__).
GHC tries not to select a function with an ``INLINE`` pragma as a loop
breaker, but when there is no choice even an INLINE function can be
selected, in which case the ``INLINE`` pragma is ignored. For example, for a
self-recursive function, the loop breaker can only be the function
itself, so an ``INLINE`` pragma is always ignored.

Syntactically, an ``INLINE`` pragma for a function can be put anywhere
its type signature could be put.

``INLINE`` pragmas are a particularly good idea for the
``then``/``return`` (or ``bind``/``unit``) functions in a monad. For
example, in GHC's own ``UniqueSupply`` monad code, we have: ::

    {-# INLINE thenUs #-}
    {-# INLINE returnUs #-}

See also the ``NOINLINE`` (:ref:`noinline-pragma`) and ``INLINABLE``
(:ref:`inlinable-pragma`) pragmas.

.. _inlinable-pragma:

``INLINABLE`` pragma
~~~~~~~~~~~~~~~~~~~~

.. pragma:: INLINABLE ⟨name⟩

    :where: top-level

    Suggest that the compiler always consider inlining ``name``.

An ``{-# INLINABLE f #-}`` pragma on a function ``f`` has the following
behaviour:

-  While ``INLINE`` says "please inline me", the ``INLINABLE`` says
   "feel free to inline me; use your discretion". In other words the
   choice is left to GHC, which uses the same rules as for pragma-free
   functions. Unlike ``INLINE``, that decision is made at the *call
   site*, and will therefore be affected by the inlining threshold,
   optimisation level etc.

-  Like ``INLINE``, the ``INLINABLE`` pragma retains a copy of the
   original RHS for inlining purposes, and persists it in the interface
   file, regardless of the size of the RHS.

-  One way to use ``INLINABLE`` is in conjunction with the special
   function ``inline`` (:ref:`special-ids`). The call ``inline f`` tries
   very hard to inline ``f``. To make sure that ``f`` can be inlined, it
   is a good idea to mark the definition of ``f`` as ``INLINABLE``, so
   that GHC guarantees to expose an unfolding regardless of how big it
   is. Moreover, by annotating ``f`` as ``INLINABLE``, you ensure that
   ``f``\'s original RHS is inlined, rather than whatever random
   optimised version of ``f`` GHC's optimiser has produced.

-  The ``INLINABLE`` pragma also works with ``SPECIALISE``: if you mark
   function ``f`` as ``INLINABLE``, then you can subsequently
   ``SPECIALISE`` in another module (see :ref:`specialize-pragma`).

-  Unlike ``INLINE``, it is OK to use an ``INLINABLE`` pragma on a
   recursive function. The principal reason do to so to allow later use
   of ``SPECIALISE``

The alternative spelling ``INLINEABLE`` is also accepted by GHC.

.. _noinline-pragma:

``NOINLINE`` pragma
~~~~~~~~~~~~~~~~~~~

.. index::
   single: NOTINLINE

.. pragma:: NOINLINE ⟨name⟩

    :where: top-level

    Instructs the compiler not to inline a value.

The :pragma:`NOINLINE` pragma does exactly what you'd expect: it stops the
named function from being inlined by the compiler. You shouldn't ever
need to do this, unless you're very cautious about code size.

``NOTINLINE`` is a synonym for ``NOINLINE`` (``NOINLINE`` is specified
by Haskell 98 as the standard way to disable inlining, so it should be
used if you want your code to be portable).

.. _conlike-pragma:

``CONLIKE`` modifier
~~~~~~~~~~~~~~~~~~~~

.. pragma:: CONLIKE

    :where: modifies :pragma:`INLINE` or :pragma:`NOINLINE` pragma

    Instructs GHC to consider a value to be especially cheap to inline.

An :pragma:`INLINE` or :pragma:`NOINLINE` pragma may have a :pragma:`CONLIKE` modifier, which affects
matching in :pragma:`RULE <RULES>`\s (only). See :ref:`conlike`.

.. _phase-control:

Phase control
~~~~~~~~~~~~~

Sometimes you want to control exactly when in GHC's pipeline the :pragma:`INLINE`
pragma is switched on. Inlining happens only during runs of the
*simplifier*. Each run of the simplifier has a different *phase number*;
the phase number decreases towards zero. If you use
:ghc-flag:`-dverbose-core2core` you will see the sequence of phase numbers for
successive runs of the simplifier. In an :pragma:`INLINE` pragma you can
optionally specify a phase number, thus:

-  "``INLINE[k] f``" means: do not inline ``f`` until phase ``k``, but
   from phase ``k`` onwards be very keen to inline it.

-  "``INLINE[~k] f``" means: be very keen to inline ``f`` until phase
   ``k``, but from phase ``k`` onwards do not inline it.

-  "``NOINLINE[k] f``" means: do not inline ``f`` until phase ``k``, but
   from phase ``k`` onwards be willing to inline it (as if there was no
   pragma).

-  "``NOINLINE[~k] f``" means: be willing to inline ``f`` until phase
   ``k``, but from phase ``k`` onwards do not inline it.

The same information is summarised here:

.. code-block:: none

                               -- Before phase 2     Phase 2 and later
      {-# INLINE   [2]  f #-}  --      No                 Yes
      {-# INLINE   [~2] f #-}  --      Yes                No
      {-# NOINLINE [2]  f #-}  --      No                 Maybe
      {-# NOINLINE [~2] f #-}  --      Maybe              No

      {-# INLINE   f #-}       --      Yes                Yes
      {-# NOINLINE f #-}       --      No                 No

By "Maybe" we mean that the usual heuristic inlining rules apply (if the
function body is small, or it is applied to interesting-looking
arguments etc). Another way to understand the semantics is this:

-  For both :pragma:`INLINE` and :pragma:`NOINLINE`, the phase number says when
   inlining is allowed at all.

-  The :pragma:`INLINE` pragma has the additional effect of making the function
   body look small, so that when inlining is allowed it is very likely
   to happen.

The same phase-numbering control is available for :pragma:`RULE <RULES>`\s
(:ref:`rewrite-rules`).

.. _line-pragma:

``LINE`` pragma
---------------

.. pragma:: LINE ⟨lineno⟩ "⟨file⟩"

    :where: anywhere

    Generated by preprocessors to convey source line numbers of the original
    source.

This pragma is similar to C's ``#line`` pragma, and is mainly for use in
automatically generated Haskell code. It lets you specify the line
number and filename of the original code; for example

::

    {-# LINE 42 "Foo.vhs" #-}

if you'd generated the current file from something called ``Foo.vhs``
and this line corresponds to line 42 in the original. GHC will adjust
its error messages to refer to the line/file named in the ``LINE``
pragma.

``LINE`` pragmas generated from Template Haskell set the file and line
position for the duration of the splice and are limited to the splice.
Note that because Template Haskell splices abstract syntax, the file
positions are not automatically advanced.

.. _column-pragma:

``COLUMN`` pragma
-----------------

.. index::
   single: COLUMN; pragma
   single: pragma; COLUMN

This is the analogue of the ``LINE`` pragma and is likewise intended for
use in automatically generated Haskell code. It lets you specify the
column number of the original code; for example

::

    foo = do
      {-# COLUMN 42 #-}pure ()
      pure ()

This adjusts all column numbers immediately after the pragma to start
at 42.  The presence of this pragma only affects the quality of the
diagnostics and does not change the syntax of the code itself.

.. _rules:

``RULES`` pragma
----------------

The :pragma:`RULES` pragma lets you specify rewrite rules. It is described in
:ref:`rewrite-rules`.

.. _specialize-pragma:

``SPECIALIZE`` pragma
---------------------

.. index::
   single: SPECIALIZE pragma
   single: pragma, SPECIALIZE
   single: overloading, death to

.. pragma:: SPECIALIZE ⟨name⟩ :: ⟨type⟩

    Ask that GHC specialize a polymorphic value to a particular type.

(UK spelling also accepted.) For key overloaded functions, you can
create extra versions (NB: at the cost of larger code) specialised to particular
types. Thus, if you have an overloaded function:

::

      hammeredLookup :: Ord key => [(key, value)] -> key -> value

If it is heavily used on lists with ``Widget`` keys, you could
specialise it as follows:

::

      {-# SPECIALIZE hammeredLookup :: [(Widget, value)] -> Widget -> value #-}

-  A ``SPECIALIZE`` pragma for a function can be put anywhere its type
   signature could be put. Moreover, you can also ``SPECIALIZE`` an
   *imported* function provided it was given an ``INLINABLE`` pragma at
   its definition site (:ref:`inlinable-pragma`).

-  A ``SPECIALIZE`` has the effect of generating (a) a specialised
   version of the function and (b) a rewrite rule (see
   :ref:`rewrite-rules`) that rewrites a call to the un-specialised
   function into a call to the specialised one. Moreover, given a
   ``SPECIALIZE`` pragma for a function ``f``, GHC will automatically
   create specialisations for any type-class-overloaded functions called
   by ``f``, if they are in the same module as the ``SPECIALIZE``
   pragma, or if they are ``INLINABLE``; and so on, transitively.

-  You can add phase control (:ref:`phase-control`) to the RULE
   generated by a ``SPECIALIZE`` pragma, just as you can if you write a
   ``RULE`` directly. For example:

   ::

         {-# SPECIALIZE [0] hammeredLookup :: [(Widget, value)] -> Widget -> value #-}

   generates a specialisation rule that only fires in Phase 0 (the final
   phase). If you do not specify any phase control in the ``SPECIALIZE``
   pragma, the phase control is inherited from the inline pragma (if
   any) of the function. For example:

   ::

         foo :: Num a => a -> a
         foo = ...blah...
         {-# NOINLINE [0] foo #-}
         {-# SPECIALIZE foo :: Int -> Int #-}

   The ``NOINLINE`` pragma tells GHC not to inline ``foo`` until Phase
   0; and this property is inherited by the specialisation RULE, which
   will therefore only fire in Phase 0.

   The main reason for using phase control on specialisations is so that
   you can write optimisation RULES that fire early in the compilation
   pipeline, and only *then* specialise the calls to the function. If
   specialisation is done too early, the optimisation rules might fail
   to fire.

-  The type in a ``SPECIALIZE`` pragma can be any type that is less
   polymorphic than the type of the original function. In concrete
   terms, if the original function is ``f`` then the pragma

   ::

         {-# SPECIALIZE f :: <type> #-}

   is valid if and only if the definition

   ::

         f_spec :: <type>
         f_spec = f

   is valid. Here are some examples (where we only give the type
   signature for the original function, not its code):

   ::

         f :: Eq a => a -> b -> b
         {-# SPECIALISE f :: Int -> b -> b #-}

         g :: (Eq a, Ix b) => a -> b -> b
         {-# SPECIALISE g :: (Eq a) => a -> Int -> Int #-}

         h :: Eq a => a -> a -> a
         {-# SPECIALISE h :: (Eq a) => [a] -> [a] -> [a] #-}

   The last of these examples will generate a RULE with a
   somewhat-complex left-hand side (try it yourself), so it might not
   fire very well. If you use this kind of specialisation, let us know
   how well it works.

.. _specialize-inline:

``SPECIALIZE INLINE``
~~~~~~~~~~~~~~~~~~~~~

.. pragma:: SPECIALIZE INLINE ⟨name⟩ :: ⟨type⟩

    :where: top-level

A ``SPECIALIZE`` pragma can optionally be followed with a ``INLINE`` or
``NOINLINE`` pragma, optionally followed by a phase, as described in
:ref:`inline-noinline-pragma`. The ``INLINE`` pragma affects the
specialised version of the function (only), and applies even if the
function is recursive. The motivating example is this: ::

    -- A GADT for arrays with type-indexed representation
    data Arr e where
      ArrInt :: !Int -> ByteArray# -> Arr Int
      ArrPair :: !Int -> Arr e1 -> Arr e2 -> Arr (e1, e2)

    (!:) :: Arr e -> Int -> e
    {-# SPECIALISE INLINE (!:) :: Arr Int -> Int -> Int #-}
    {-# SPECIALISE INLINE (!:) :: Arr (a, b) -> Int -> (a, b) #-}
    (ArrInt _ ba)     !: (I# i) = I# (indexIntArray# ba i)
    (ArrPair _ a1 a2) !: i      = (a1 !: i, a2 !: i)

Here, ``(!:)`` is a recursive function that indexes arrays of type
``Arr e``. Consider a call to ``(!:)`` at type ``(Int,Int)``. The second
specialisation will fire, and the specialised function will be inlined.
It has two calls to ``(!:)``, both at type ``Int``. Both these calls
fire the first specialisation, whose body is also inlined. The result is
a type-based unrolling of the indexing function.

You can add explicit phase control (:ref:`phase-control`) to
``SPECIALISE INLINE`` pragma, just like on an :pragma:`INLINE` pragma; if
you do so, the same phase is used for the rewrite rule and the INLINE control
of the specialised function.

.. warning:: You can make GHC diverge by using ``SPECIALISE INLINE`` on an
             ordinarily-recursive function.

``SPECIALIZE`` for imported functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generally, you can only give a :pragma:`SPECIALIZE` pragma for a function
defined in the same module. However if a function ``f`` is given an
:pragma:`INLINABLE` pragma at its definition site, then it can subsequently be
specialised by importing modules (see :ref:`inlinable-pragma`). For example ::

    module Map( lookup, blah blah ) where
      lookup :: Ord key => [(key,a)] -> key -> Maybe a
      lookup = ...
      {-# INLINABLE lookup #-}

    module Client where
      import Map( lookup )

      data T = T1 | T2 deriving( Eq, Ord )
      {-# SPECIALISE lookup :: [(T,a)] -> T -> Maybe a

Here, ``lookup`` is declared :pragma:`INLINABLE`, but it cannot be specialised
for type ``T`` at its definition site, because that type does not exist
yet. Instead a client module can define ``T`` and then specialise
``lookup`` at that type.

Moreover, every module that imports ``Client`` (or imports a module that
imports ``Client``, transitively) will "see", and make use of, the
specialised version of ``lookup``. You don't need to put a
:pragma:`SPECIALIZE` pragma in every module.

Moreover you often don't even need the :pragma:`SPECIALIZE` pragma in the
first place. When compiling a module ``M``, GHC's optimiser (when given the
:ghc-flag:`-O` flag) automatically considers each top-level overloaded function declared
in ``M``, and specialises it for the different types at which it is called in
``M``. The optimiser *also* considers each *imported* :pragma:`INLINABLE`
overloaded function, and specialises it for the different types at which
it is called in ``M``. So in our example, it would be enough for ``lookup``
to be called at type ``T``:

::

    module Client where
      import Map( lookup )

      data T = T1 | T2 deriving( Eq, Ord )

      findT1 :: [(T,a)] -> Maybe a
      findT1 m = lookup m T1   -- A call of lookup at type T

However, sometimes there are no such calls, in which case the pragma can
be useful.

.. _specialize-instance-pragma:

``SPECIALIZE`` instance pragma
------------------------------

.. index::
   single: instance, specializing
   single: overloading, death to

.. pragma:: SPECIALIZE instance ⟨instance head⟩

   :where: instance body

Same idea, except for instance declarations. For example:

::

    instance (Eq a) => Eq (Foo a) where {
       {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
       ... usual stuff ...
     }

The pragma must occur inside the ``where`` part of the instance
declaration.

.. _unpack-pragma:

``UNPACK`` pragma
-----------------

.. pragma:: UNPACK

    :where: data constructor field

    Instructs the compiler to unpack the contents of a constructor field into
    the constructor itself.

The ``UNPACK`` indicates to the compiler that it should unpack the
contents of a constructor field into the constructor itself, removing a
level of indirection. For example: ::

    data T = T {-# UNPACK #-} !Float
               {-# UNPACK #-} !Float

will create a constructor ``T`` containing two unboxed floats. This may
not always be an optimisation: if the ``T`` constructor is scrutinised
and the floats passed to a non-strict function for example, they will
have to be reboxed (this is done automatically by the compiler).

Unpacking constructor fields should only be used in conjunction with
:ghc-flag:`-O` [1]_, in order to expose unfoldings to the compiler so the
reboxing can be removed as often as possible. For example: ::

    f :: T -> Float
    f (T f1 f2) = f1 + f2

The compiler will avoid reboxing ``f1`` and ``f2`` by inlining ``+`` on
floats, but only when :ghc-flag:`-O` is on.

Any single-constructor data is eligible for unpacking; for example ::

    data T = T {-# UNPACK #-} !(Int,Int)

will store the two ``Int``\ s directly in the ``T`` constructor, by
flattening the pair. Multi-level unpacking is also supported: ::

    data T = T {-# UNPACK #-} !S
    data S = S {-# UNPACK #-} !Int {-# UNPACK #-} !Int

will store two unboxed ``Int#``\ s directly in the ``T`` constructor.
The unpacker can see through newtypes, too.

See also the :ghc-flag:`-funbox-strict-fields` flag, which essentially has the
effect of adding ``{-# UNPACK #-}`` to every strict constructor field.

.. [1]
   In fact, :pragma:`UNPACK` has no effect without :ghc-flag:`-O`, for technical
   reasons (see :ghc-ticket:`5252`).

.. _nounpack-pragma:

``NOUNPACK`` pragma
-------------------

.. pragma:: NOUNPACK

    :where: top-level

    Instructs the compiler not to unpack a constructor field.

The ``NOUNPACK`` pragma indicates to the compiler that it should not
unpack the contents of a constructor field. Example: ::

    data T = T {-# NOUNPACK #-} !(Int,Int)

Even with the flags :ghc-flag:`-funbox-strict-fields` and :ghc-flag:`-O`, the
field of the constructor ``T`` is not unpacked.

.. _source-pragma:

``SOURCE`` pragma
-----------------

.. pragma:: SOURCE

    :where: after ``import`` statement

    Import a module by ``hs-boot`` file to break a module loop.

The ``{-# SOURCE #-}`` pragma is used only in ``import`` declarations,
to break a module loop. It is described in detail in
:ref:`mutual-recursion`.

.. _complete-pragma:

``COMPLETE`` pragmas
--------------------

.. pragma:: COMPLETE

    :where: at top level

    Specify the set of constructors or pattern synonyms which constitute a total
    match.

The ``COMPLETE`` pragma is used to inform the pattern match checker that a
certain set of patterns is complete and that any function which matches
on all the specified patterns is total.

The most common usage of ``COMPLETE`` pragmas is with
:ref:`pattern-synonyms`.
On its own, the checker is very naive and assumes that any match involving
a pattern synonym will fail. As a result, any pattern match on a
pattern synonym is regarded as
incomplete unless the user adds a catch-all case.

For example, the data types ``2 * A`` and ``A + A`` are isomorphic but some
computations are more naturally expressed in terms of one or the other. To
get the best of both worlds, we can choose one as our implementation and then
provide a set of pattern synonyms so that users can use the other representation
if they desire. We can then specify a ``COMPLETE`` pragma in order to
inform the pattern match checker that a function which matches on both ``LeftChoice``
and ``RightChoice`` is total. ::

    data Choice a = Choice Bool a

    pattern LeftChoice :: a -> Choice a
    pattern LeftChoice a = Choice False a

    pattern RightChoice :: a -> Choice a
    pattern RightChoice a = Choice True a

    {-# COMPLETE LeftChoice, RightChoice #-}

    foo :: Choice Int -> Int
    foo (LeftChoice n) = n * 2
    foo (RightChoice n) = n - 2

``COMPLETE`` pragmas are only used by the pattern match checker. If a function
definition matches on all the constructors specified in the pragma then the
compiler will produce no warning.

``COMPLETE`` pragmas can contain any data constructors or pattern
synonyms which are in scope, but must mention at least one data
constructor or pattern synonym defined in the same module.
``COMPLETE`` pragmas may only appear at the top level of a module.
Once defined, they are automatically imported and exported from
modules. ``COMPLETE`` pragmas should be thought of as asserting a
universal truth about a set of patterns and as a result, should not be
used to silence context specific incomplete match warnings.

It is also possible to restrict the types to which a ``COMPLETE`` pragma applies
by putting a double colon ``::`` after the list of constructors, followed by a
result type constructor, which will be used to restrict the cases in which the
pragma applies. GHC will compare the annotated result type constructor with the
type constructor in the head of the scrutinee type in a pattern match to see if
the ``COMPLETE`` pragma is meant to apply to it.

This is especially useful in cases that the constructors specified are
polymorphic, e.g.::

    data Proxy a = Proxy

    class IsEmpty a where
      isEmpty :: a -> Bool

    class IsCons a where
      type Elt a
      isCons :: a -> Maybe (Elt a, a)

    pattern Empty :: IsEmpty a => a
    pattern Empty <- (isEmpty -> True)

    pattern Cons :: IsCons a => Elt a -> a -> a
    pattern Cons x xs <- (isCons -> Just (x,xs))

    instance IsEmpty (Proxy a) where
      isEmpty Proxy = True

    instance IsEmpty [a] where
      isEmpty = null

    instance IsCons [a] where
      type Elt [a] = a
      isCons [] = Nothing
      isCons (x:xs) = Just (x,xs)

    {-# COMPLETE Empty :: Proxy #-}
    {-# COMPLETE Empty, Cons :: [] #-}

    foo :: Proxy a -> Int
    foo Empty = 0

    bar :: [a] -> Int
    bar Empty = 0
    bar (Cons _ _) = 1

    baz :: [a] -> Int
    baz Empty = 0

In this example, ``foo`` and ``bar`` will not be warned about, as their
pattern matches are covered by the two ``COMPLETE`` pragmas above, but
``baz`` will be warned about as incomplete.

.. _overlap-pragma:

``OVERLAPPING``, ``OVERLAPPABLE``, ``OVERLAPS``, and ``INCOHERENT`` pragmas
---------------------------------------------------------------------------

.. index::
   single: OVERLAPPING
   single: pragma; OVERLAPPING
   single: OVERLAPPABLE
   single: pragma; OVERLAPPABLE
   single: OVERLAPS
   single: pragma; OVERLAPS
   single: INCOHERENT
   single: pragma; INCOHERENT

.. pragma:: OVERLAPPING
.. pragma:: OVERLAPPABLE
.. pragma:: OVERLAPS
.. pragma:: INCOHERENT

    :where: on instance head

The pragmas ``OVERLAPPING``, ``OVERLAPPABLE``, ``OVERLAPS``,
``INCOHERENT`` are used to specify the overlap behavior for individual
instances, as described in Section :ref:`instance-overlap`. The pragmas
are written immediately after the ``instance`` keyword, like this:

::

    instance {-# OVERLAPPING #-} C t where ...


