.. _template-haskell:

Template Haskell
================

Template Haskell allows you to do compile-time meta-programming in
Haskell. The background to the main technical innovations is discussed
in "`Template Meta-programming for
Haskell <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/meta-haskell.pdf>`__"
(Proc Haskell Workshop 2002).
The first example from that paper is set out below (:ref:`th-example`)
as a worked example to help get you started.

.. _th-syntax:

Syntax
------

.. extension:: TemplateHaskell
    :shortdesc: Allow Template Haskell's splice and quotation syntax.

    :implies: :extension:`TemplateHaskellQuotes`
    :since: 6.0. Typed splices introduced in GHC 7.8.1.

    Enable Template Haskell's splice and quotation syntax.

.. extension:: TemplateHaskellQuotes
    :shortdesc: Allow :ref:`Template Haskell <template-haskell>`'s quotation syntax.

    :since: 8.0.1

    Enable Template Haskell's quotation syntax.

Template Haskell has the following new syntactic constructions. You need to use
the extension :extension:`TemplateHaskell` to switch these syntactic extensions on.
Alternatively, the :extension:`TemplateHaskellQuotes` extension can be used to
enable the quotation subset of Template Haskell (i.e. without top-level splices).

-  A expression quotation is written in Oxford brackets, thus:

   -  ``[| ... |]``, or ``[e| ... |]``, where the "..." is an
      expression; the quotation has type ``Quote m => m Exp``.

   -  ``[d| ... |]``, where the "..." is a list of top-level
      declarations; the quotation has type ``Quote m => m [Dec]``.

   -  ``[t| ... |]``, where the "..." is a type; the quotation has type
      ``Quote m => m Type``.

   -  ``[p| ... |]``, where the "..." is a pattern; the quotation has
      type ``Quote m => m Pat``.

   The ``Quote`` type class (:th-ref:`Language.Haskell.TH.Syntax.Quote`) is
   the minimal interface necessary to implement the desugaring of quotations.
   The ``Q`` monad is an instance of ``Quote`` but contains many more
   operations which are not needed for defining quotations.

   See :ref:`pts-where` for using partial type signatures in quotations.

-  A splice is written ``$x``, where ``x`` is an arbitrary expression.
   There must be no space between the "$" and the expression.

   A top-level splice can occur in place of

   -  an expression; the spliced expression must have type ``Q Exp``

   -  a pattern; the spliced pattern must have type ``Q Pat``

   -  a type; the spliced expression must have type ``Q Type``

   -  a list of declarations at top level; the spliced expression must
      have type ``Q [Dec]``

   Note that declaration splices are not allowed anywhere except at top level
   (outside any other declarations).

   The ``Q`` monad is a monad defined in :th-ref:`Language.Haskell.TH.Syntax.` which
   supports several useful operations during code generation such as reporting
   errors or looking up identifiers in the environment.

   This use of ``$`` overrides its meaning as an infix operator, just as ``M.x``
   overrides the meaning of ``.`` as an infix operator. If you want the
   infix operator, put spaces around it.


-  Splices can be nested inside quotation brackets. For example the fragment
   representing ``1 + 2`` can be constructed using nested splices::

    oneC, twoC, plusC  :: Quote m => m Exp
    oneC = [| 1 |]

    twoC = [| 2 |]

    plusC = [| $oneC + $twoC |]

-  A *typed* expression quotation is written as ``[|| ... ||]``, or
   ``[e|| ... ||]``, where the "..." is an expression; if the "..."
   expression has type ``a``, then the quotation has type
   ``Quote m => Code m a``.

   It is possible to extract a value of type ``m Exp`` from ``Code m a``
   using the ``unTypeCode :: Code m a -> m Exp`` function.

-  A *typed* expression splice is written ``$$x``, where ``x`` is
   is an arbitrary expression.

   A top-level typed expression splice can occur in place of an expression; the
   spliced expression must have type ``Code Q a``

   **NOTE**: Currently typed splices may inhibit the unused identifier warning for
   identifiers in scope. See :ghc-ticket:`16524`.


-  A quasi-quotation can appear in a pattern, type, expression, or
   declaration context and is also written in Oxford brackets:

   -  ``[varid| ... |]``, where the "..." is an arbitrary string; a full
      description of the quasi-quotation facility is given in
      :ref:`th-quasiquotation`.

-  A name can be quoted with either one or two prefix single quotes:

   -  ``'f`` has type ``Name``, and names the function ``f``. Similarly
      ``'C`` has type ``Name`` and names the data constructor ``C``. In
      general ``'``\ ⟨thing⟩ interprets ⟨thing⟩ in an expression
      context.

      A name whose second character is a single quote cannot be quoted in
      exactly this way, because it will be parsed instead as a quoted
      character. For example, if the function is called ``f'7`` (which is a
      legal Haskell identifier), an attempt to quote it as ``'f'7`` would be
      parsed as the character literal ``'f'`` followed by the numeric literal
      ``7``. As for promoted constructors (:ref:`promotion-syntax`), the
      workaround is to add a space between the quote and the name. The name of
      the function ``f'7`` is thus written ``' f'7``.

   -  ``''T`` has type ``Name``, and names the type constructor ``T``.
      That is, ``''``\ ⟨thing⟩ interprets ⟨thing⟩ in a type context.

   These ``Names`` can be used to construct Template Haskell
   expressions, patterns, declarations etc. They may also be given as an
   argument to the ``reify`` function.

-  The precise type of a quotation depends on the types of the nested splices inside it::

      -- Add a redundant constraint to demonstrate that constraints on the
      -- monad used to build the representation are propagated when using nested
      -- splices.
      f :: (Quote m, C m) => m Exp
      f = [| 5 | ]

      -- f is used in a nested splice so the constraint on f, namely C, is propagated
      -- to a constraint on the whole representation.
      g :: (Quote m, C m) => m Exp
      g = [| $f + $f |]

   Remember, a top-level splice still requires its argument to be of type ``Q Exp``.
   So then splicing in ``g`` will cause ``m`` to be instantiated to ``Q``::

      h :: Int
      h = $(g) -- m ~ Q

Levels and Stages
------------------

Template Haskell executes code at both compile time and runtime, which requires
understanding two key concepts: **levels** and **stages**.

**Levels** are a concept the typechecker uses to ensure that code is well-staged -
that is, the compiler can execute compile-time operations before runtime operations.
**Stages** are the actual moments when code is compiled and executed. Levels are a semantic
concept used by the typechecker, whilst stages are operational, a property of evaluation.

Understanding Levels
~~~~~~~~~~~~~~~~~~~~

Every expression in a program exists at a specific integer level:

* Level 0: Normal top-level declarations in a module
* Level -1: Code inside a top-level splice (code that runs at compile time)
* Level 1: Code inside a quotation (code that is quoted for runtime)

The level changes when entering quotes and splices:

* Inside a quote ``[| e |]``, the level increases by 1
* Inside a splice ``$( e )``, the level decreases by 1

Thus, the level can be calculated as the number of surrounding quotes minus the
number of surrounding splices. For example:

.. code-block:: haskell

    -- foo is at level 0
    foo = $(let
             -- bar is at level -1
             bar = $(let
                      -- baz is at level -2
                      baz = [|
                              -- qux is at level -1
                              qux = [|
                                      -- quux is at level 0
                                      quux = [|
                                              -- quuz is at level 1
                                              quuz = 0
                                             |]
                                    |]
                            |]
                   in baz)
          in bar)

Top-level splices (which define where compile-time evaluation happens) are
characterized by having their body at a negative level.

* Top-level declarations introduce variables at level 1.
* Imports introduce variables at level 1.
* Local variables are introduced at the level of their expression. For example,
  the ``x`` in [| let x = 0 in ... |] is at level 2.


Cross-Stage Persistence
~~~~~~~~~~~~~~~~~~~~~~~

In normal Template Haskell, **cross-stage persistence (CSP)** allows identifiers
to be used at levels different from where they were defined. There are two
mechanisms for this:

1. **Path-based persistence**: This allows a global definition at one level to be
   used at a different level in two cases:

   * Any global identifier can be used at a later level (i.e. inside a quotation).
   * An imported identifier can be used at an earlier level (i.e. in a splice)

   The :extension:`ImplicitStagePersistence` extension controls whether
   path-based persistence is enabled. It is enabled by default in all current
   language editions.

2. **Serialisation-based persistence**: This allows locally-bound variables to be
   used at higher levels through the ``Lift`` typeclass:

   .. code-block:: haskell

       tardy x = [| x |]  -- This is elaborated to [| $(lift x) |]

   When the compiler sees a level error where a variable used one level higher than
   it is defined, it will automatically insert a ``lift`` to serialise the variable
   at the required level.

   This functionality is exposed to the
   user as the ``Lift`` typeclass in the ``Language.Haskell.TH.Syntax``
   module. If a type has a ``Lift`` instance, then any of its values can be
   lifted to a Template Haskell expression: ::

       class Lift t where
           lift :: Quote m => t -> m Exp
           liftTyped :: Quote m => t -> Code m t


   ``Lift`` is defined for most built-in types and can be
   derived using the :extension:`DeriveLift` extension.
   See :ref:`deriving-lift` for more information.

Path-based persistence explains why this code works:

.. code-block:: haskell

    module M where

    suc :: Int -> Int
    suc = (+1)

    one :: Q Exp
    one = [| \x -> suc x |]  -- suc is used at level 1, defined at level 0

    two = $(one)  -- one is used at level -1, defined at level 0

With :extension:`ExplicitLevelImports` and :extension:`NoImplicitStagePersistence`,
path-based persistence is disabled, requiring explicit indication of which
identifiers can be used at which levels.

Stages and Compilation
~~~~~~~~~~~~~~~~~~~~~~

While levels are a typechecker concept, **stages** refer to the actual moments
when modules are compiled and executed:

* Stage C (Compile time): Code that runs during compilation
* Stage R (Runtime): Code that runs when the compiled program is executed

The compiler may need to compile code differently depending on the stage.
For example, if you are using :ghc-flag:`-fno-code`, no code is needed for the R stage
but code generation will be needed for the C stage. If your compiler is dynamically
linked then the C stage code will need to be dynamically linked, but the R stage
may be statically linked.

The cross-stage persistence rules admitted by a language arise from assumptions
made about the stage structure. For GHC, with :extension:`ImplicitStagePersistence`,
it must be assumed that a module will be available at all stages. This is a strong
requirement.

Declaration Groups
------------------

Top-level declaration splices break up a source file into
*declaration groups*. A *declaration group* is the group of
declarations created by a top-level declaration splice, plus those
following it, down to but not including the next top-level
declaration splice. N.B. only top-level splices delimit declaration
groups, not expression splices. The first declaration group in a module
includes all top-level definitions down to but not including the first
top-level declaration splice.

Each group is compiled just like a separately compiled module. That is:

- Later groups can "see" declarations, and instance declarations, from
  earlier groups;

- But earlier groups cannot "see" declarations, or instance declarations,
  from later groups.

Each declaration group is mutually recursive only within the group.
Declaration groups can refer to definitions within previous groups,
but not later ones.

Accordingly, the type environment seen by ``reify`` includes all the
top-level declarations up to the end of the immediately preceding
declaration group, but no more.

Unlike normal declaration splices, declaration quasiquoters do not
cause a break. These quasiquoters are expanded before the rest of the
declaration group is processed, and the declarations they generate
are merged into the surrounding declaration group. Consequently, the
type environment seen by ``reify`` from a declaration quasiquoter
will not include anything from the quasiquoter's declaration group.

Concretely, consider the following code ::

    module M where

    import ...

    f x = x

    $(th1 4)

    h y = k y y $(blah1)

    [qq|blah|]

    k x y z = x + y + z

    $(th2 10)

    w z = $(blah2)

In this example, a ``reify`` inside...

1. The splice ``$(th1 ...)`` would see the definition of ``f`` - the
   splice is top-level and thus all definitions in the previous
   declaration group are visible (that is, all definitions in the module
   up-to, but not including, the splice itself).

2. The splice ``$(blah1)`` cannot refer to the function ``w`` - ``w`` is
   part of a later declaration group, and thus invisible, similarly,
   ``$(blah1)`` cannot see the definition of ``h`` (since it is part of
   the same declaration group as ``$(blah1)``. However, the splice
   ``$(blah1)`` can see the definition of ``f`` (since it is in the
   immediately preceding declaration group).

3. The splice ``$(th2 ...)`` would see the definition of ``f``, all the
   bindings created by ``$(th1 ...)``, the definition of ``h`` and all
   bindings created by ``[qq|blah|]`` (they are all in previous
   declaration groups).

4. The body of ``h`` *can* refer to the function ``k`` appearing on the
   other side of the declaration quasiquoter, as quasiquoters do not
   cause a declaration group to be broken up.

5. The ``qq`` quasiquoter would be able to see the definition of ``f``
   from the preceding declaration group, but not the definitions of
   ``h`` or ``k``, or any definitions from subsequent declaration
   groups.

6. The splice ``$(blah2)`` would see the same definitions as the splice
   ``$(th2 ...)`` (but *not* any bindings it creates).

Note that since an expression splice is unable to refer to declarations
in the same declaration group, we can introduce a top-level (empty)
splice to break up the declaration group ::

    module M where

    data D = C1 | C2

    f1 = $(th1 ...)

    $(return [])

    f2 = $(th2 ...)

Here

1. The splice ``$(th1 ...)`` *cannot* refer to ``D`` - it is in the same
   declaration group.
2. The declaration group containing ``D`` is terminated by the empty
   top-level declaration splice ``$(return [])`` (recall, ``Q`` is a
   Monad, so we may simply ``return`` the empty list of declarations).
3. Since the declaration group containing ``D`` is in the previous
   declaration group, the splice ``$(th2 ...)`` *can* refer to ``D``.

Note that in some cases, the presence or absence of top-level declaration
splices can affect the *runtime* behavior of the surrounding code, because
the resolution of instances may differ depending on their visiblity. One
case where this arises is with
:ref:`incoherent instances <instance-overlap>` ::

    module Main where

    main :: IO ()
    main = do
      let i :: Int
          i = 42
      putStrLn (m1 i)
      putStrLn (m2 i)

    class C1 a where
      m1 :: a -> String

    instance {-# INCOHERENT #-} C1 a where
      m1 _ = "C1 incoherent"

    instance C1 Int where
      m1 = show

    class C2 a where
      m2 :: a -> String

    instance {-# INCOHERENT #-} C2 a where
      m2 _ = "C2 incoherent"

    $(return [])

    instance C2 Int where
      m2 = show

Here, ``C1`` and ``C2`` are the same classes with nearly identical
instances. The only significant differences between ``C1`` and ``C2``, aside
from the minor name change, is that all of ``C1``'s instances are defined
within the same declaration group, whereas the ``C2 Int`` instance is put in
a separate declaration group from the incoherent ``C2 a`` instance. This has
an impact on the runtime behavior of the ``main`` function ::

    $ runghc Main.hs
    42
    C2 incoherent

Note that ``m1 i`` returns ``"42"``, but ``m2 i`` returns
``"C2 incoherent"``. When each of these expressions are typechecked, GHC
must figure out which ``C1 Int`` and ``C2 Int`` instances to use:

1. When resolving the ``C1 Int`` instance, GHC discovers two possible
   instances in the same declaration group: the incoherent ``C1 a`` instance
   and the non-incoherent ``C1 Int`` instance. According to the instance
   search rules described in :ref:`instance-overlap`, because there is
   exactly one non-incoherent instance to pick, GHC will choose the
   ``C1 Int`` instance. As a result, ``m1 i`` will be equivalent to
   ``show i`` (i.e., ``"42"``).
2. When resolving the ``C2 Int`` instance, GHC only discovers one instance
   in the same declaration group: the incoherent ``C2 a`` instance. Note
   that GHC does *not* see the ``C2 Int`` instance, as that is in a later
   declaration group that is made separate by the intervening declaration
   splice. As a result, GHC will choose the ``C2 a`` instance, making
   ``m2 i`` equivalent to ``"C2 incoherent"``.

Miscellaneous other features
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section the other features and issues of Template Haskell are
discussed.

-  You may omit the ``$(...)`` in a top-level declaration splice. Simply
   writing an expression (rather than a declaration) implies a splice.
   For example, you can write ::

       module Foo where
       import Bar

       f x = x

       $(deriveStuff 'f)   -- Uses the $(...) notation

       g y = y+1

       deriveStuff 'g      -- Omits the $(...)

       h z = z-1

   This abbreviation makes top-level declaration slices quieter and less
   intimidating.

-  Pattern splices introduce variable binders but scoping of variables in
   expressions inside the pattern's scope is only checked when a splice is
   run.  Note that pattern splices that occur outside of any quotation
   brackets are run at compile time.  Pattern splices occurring inside a
   quotation bracket are *not* run at compile time; they are run when the
   bracket is spliced in, sometime later.  For example, ::

       mkPat :: Quote m => m Pat
       mkPat = [p| (x, y) |]

       -- in another module:
       foo :: (Char, String) -> String
       foo $(mkPat) = x : z

       bar :: Quote m => m Exp
       bar = [| \ $(mkPat) -> x : w |]

   will fail with ``z`` being out of scope in the definition of ``foo`` but it
   will *not* fail with ``w`` being out of scope in the definition of ``bar``.
   That will only happen when ``bar`` is spliced.

-  A pattern quasiquoter *may* generate binders that scope over the
   right-hand side of a definition because these binders are in scope
   lexically. For example, given a quasiquoter ``haskell`` that parses
   Haskell, in the following code, the ``y`` in the right-hand side of
   ``f`` refers to the ``y`` bound by the ``haskell`` pattern
   quasiquoter, *not* the top-level ``y = 7``. ::

       y :: Int
       y = 7

       f :: Int -> Int -> Int
       f n = \ [haskell|y|] -> y+n



- The :extension:`TemplateHaskellQuotes` extension is considered safe under
  :ref:`safe-haskell` while :extension:`TemplateHaskell` is not.

-  Expression quotations accept most Haskell language constructs.
   However, there are some GHC-specific extensions which expression
   quotations currently do not support, including

   -  Type holes in typed splices (see :ghc-ticket:`10945` and
      :ghc-ticket:`10946`)

(Compared to the original paper, there are many differences of detail.
The syntax for a declaration splice uses "``$``" not "``splice``". The type of
the enclosed expression must be ``Quote m => m [Dec]``, not ``[Q Dec]``. Typed expression
splices and quotations are supported.)

.. ghc-flag:: -fenable-th-splice-warnings
    :shortdesc: Generate warnings for Template Haskell splices
    :type: dynamic
    :reverse: -fno-enable-th-splice-warnings
    :category: warnings

    Template Haskell splices won't be checked for warnings, because the code
    causing the warning might originate from a third-party library and possibly
    was not written by the user. If you want to have warnings for splices
    anyway, pass :ghc-flag:`-fenable-th-splice-warnings`.

Explicit Level Imports
----------------------

The :extension:`ExplicitLevelImports` extension, along with
:extension:`ImplicitStagePersistence`, gives programmers fine-grained control
over which modules are needed at each stage of execution.

For a detailed description of the extension, see the paper
`Explicit Level Imports <https://mpickering.github.io/papers/explicit-level-imports.pdf>`_.

.. extension:: ExplicitLevelImports
    :shortdesc: Allow explicit level imports in Template Haskell.

    :implies: :extension:`NoImplicitStagePersistence`
    :since: 9.14.1

    Enable explicit level imports for Template Haskell, allowing programmers to
    specify which modules are needed at which level.

    This introduces the ``splice`` and ``quote`` import modifiers which allow
    a user to precisely express the level of identifiers introduced by an import.

.. extension:: ImplicitStagePersistence
    :shortdesc: Allow identifiers to be used at different levels from where they are defined.

    :default: on
    :since: 9.14.1

    Allow identifiers to be used at different levels than where they're defined,
    using path-based persistence.

Syntax and Usage
~~~~~~~~~~~~~~~~

:extension:`ExplicitLevelImports` adds two new import modifiers:

* ``import splice M (...)`` - imports identifiers at level -1 (for use in splices)
* ``import quote M (...)`` - imports identifiers at level 1 (for use in quotations)
* ``import M (...)`` - imports identifiers at level 0 (normal code)

The syntax supports both options for placement of the level keywords:

.. code-block:: haskell

    import splice M          -- before the module name
    import M splice          -- after the module name
    import splice qualified M as MB -- with qualified
    import splice M qualified as MB -- with -XImportQualifiedPost
    import M splice qualified as MB -- with -XImportQualifiedPost

Basic Examples
~~~~~~~~~~~~~~

Explicit level imports allow you to be more precise about which modules are needed at which level.

.. code-block:: haskell

    {-# LANGUAGE TemplateHaskell #-}
    module Main where

    import Control.Lens.TH (makeLenses)
    import OtherModule (someFunction)

    data User = User { _name :: String, _age :: Int }

    $(makeLenses ''User)

    main = print (someFunction (User "John" 30))

In this version, both ``Control.Lens.TH`` and ``OtherModule`` are imported
normally. GHC must compile both modules before it can start type-checking Main,
because it can't tell in advance which imports might be needed when evaluating
the ``makeLenses`` splice. Even though only ``makeLenses`` is actually used in
the splice, GHC must assume that any imported identifier might be needed.

If you use :extension:`ExplicitLevelImports`, you can be more precise about which
modules are needed at which level. For example, ::

.. code-block:: haskell

    {-# LANGUAGE TemplateHaskell, ExplicitLevelImports #-}
    module Main where

    import splice Control.Lens.TH (makeLenses)
    import OtherModule (someFunction)

    data User = User { _name :: String, _age :: Int }

    $(makeLenses ''User)

    main = print (someFunction (User "John" 30))

With explicit level imports, we've marked ``Control.Lens.TH`` with the
``splice`` keyword, which tells GHC that this module is needed at compile-time
for evaluating splices. This provides GHC with crucial information:

1. ``Control.Lens.TH`` must be compiled to object code before type-checking ``Main``
2. ``OtherModule`` only needs to be type-checked before ``Main``, with code generation potentially happening in parallel
3. ``Control.Lens.TH`` won't be needed at runtime (assuming there are no other references to it)

This distinction brings several benefits:

* GHC doesn't need to wait for ``OtherModule`` to be fully compiled before starting on ``Main``
* ``Control.Lens.TH`` won't be linked into the final executable since it's only needed at compile-time
* The staging structure of the program is more explicit

Another example showing different import levels:

.. code-block:: haskell

    {-# LANGUAGE TemplateHaskell, ExplicitLevelImports #-}
    module Advanced where

    import splice A (makeFunction)   -- Used in splices (level -1)
    import B (normalFunction)        -- Used in normal code (level 0)
    import quote C (runtimeValue)    -- Used in quotes (level 1)

    -- This generates a function at compile time
    $(makeFunction "generatedFunction")

    -- This uses a normal function at runtime
    result = normalFunction 42

    -- This creates a quotation containing code that will use runtimeValue
    quotation = [| runtimeValue * 2 |]

In this example, we're explicitly marking each import with its intended level:
* ``A`` provides code that runs at compile time (in splices)
* ``B`` provides code that runs at normal runtime
* ``C`` provides values that will be referenced in quoted code

Level Rules and Errors
~~~~~~~~~~~~~~~~~~~~~~

With :extension:`NoImplicitStagePersistence`:

* Functions imported at level 0 can only be used at level 0
* Functions imported with ``splice`` can only be used inside top-level splices
* Functions imported with ``quote`` can only be used inside quotes

Errors will occur if you use an identifier at the wrong level:

.. code-block:: haskell

    import splice A (foo)       -- foo at level -1
    import B (bar)              -- bar at level 0
    import quote C (baz)        -- baz at level 1

    x = $(foo 42)               -- OK: foo used at level -1
    y = $(bar 42)               -- Error: bar imported at level 0 but used at level -1
    z = [| baz 42 |]            -- OK: baz used at level 1
    w = [| bar 42 |]            -- Error: bar imported at level 0 but used at level 1

Class Instances and Levels
~~~~~~~~~~~~~~~~~~~~~~~~~~

Class instances are also subject to level checking. Instances must be available at the level where they're used:

* Instances from the current module are at level 0
* Instances from normally imported modules are at level 0
* Instances from splice-imported modules are at level -1
* Instances from quote-imported modules are at level 1

Since classes are imported transitively, the typechecker ensures that there is a
well-levelled path to access any instance. For example, if an instance is needed
at level -1, then the instance must come from the transitive closure of splice
imported modules.

Prelude Imports
~~~~~~~~~~~~~~~

The implicit ``Prelude`` import only brings identifiers into scope at level 0.
If you need ``Prelude`` functions in splices or quotes, you must explicitly
import them:

.. code-block:: haskell

    import splice Prelude (map, filter)  -- Use these in splices
    import quote Prelude (show, (+))     -- Use these in quotes

Notes and Limitations
~~~~~~~~~~~~~~~~~~~~~

* Local definitions (those defined in the same module) are still subject to
  level rules - you can't use a function in a splice if it's defined in the
  same module
* :extension:`ExplicitLevelImports` works best when most Template Haskell
  usage is isolated to a few modules
* Defining ``Lift`` instances requires special handling since the datatype must
  be available at both compile-time and runtime

.. _th-usage:

Using Template Haskell
----------------------

-  The data types and monadic constructor functions for Template Haskell
   are in the library :th-ref:`Language.Haskell.TH.Syntax.`.

-  You can only run a function at compile time if it is imported from
   another module *that is not part of a mutually-recursive group of
   modules that includes the module currently being compiled*.
   Furthermore, all of the modules of the mutually-recursive group must
   be reachable by non-SOURCE imports from the module where the splice
   is to be run.

   For example, when compiling module A, you can only run Template
   Haskell functions imported from B if B does not import A (directly or
   indirectly). The reason should be clear: to run B we must compile and
   run A, but we are currently type-checking A.

-  If you are building GHC from source, you need at least a stage-2
   bootstrap compiler to run Template Haskell splices and quasi-quotes.
   A stage-1 compiler will only accept regular quotes of Haskell.
   Reason: TH splices and quasi-quotes compile and run a program, and
   then looks at the result. So it's important that the program it
   compiles produces results whose representations are identical to
   those of the compiler itself.

.. _th-view-gen-code:

Viewing Template Haskell generated code
---------------------------------------

The flag :ghc-flag:`-ddump-splices` shows the expansion of all top-level
declaration splices, both typed and untyped, as they happen. As with all
dump flags, the default is for this output to be sent to stdout. For a
non-trivial program, you may be interested in combining this with the
:ghc-flag:`-ddump-to-file` flag (see :ref:`dumping-output`. For each file using
Template Haskell, this will show the output in a ``.dump-splices`` file.

The flag :ghc-flag:`-dth-dec-file` dumps the expansions of all top-level
TH declaration splices, both typed and untyped, in the file :file:`M.th.hs`
for each module `M` being compiled. Note that other types of
splices (expressions, types, and patterns) are not shown. Application
developers can check this into their repository so that they can grep for
identifiers that were defined in Template Haskell. This is similar to using
:ghc-flag:`-ddump-to-file` with :ghc-flag:`-ddump-splices` but it always
generates a file instead of being coupled to :ghc-flag:`-ddump-to-file`. The
format is also different: it does not show code from the original file, instead
it only shows generated code and has a comment for the splice location of the
original file.

Below is a sample output of :ghc-flag:`-ddump-splices` ::

    TH_pragma.hs:(6,4)-(8,26): Splicing declarations
      [d| foo :: Int -> Int
          foo x = x + 1 |]
    ======>
      foo :: Int -> Int
      foo x = (x + 1)

Below is the output of the same sample using :ghc-flag:`-dth-dec-file` ::

    -- TH_pragma.hs:(6,4)-(8,26): Splicing declarations
    foo :: Int -> Int
    foo x = (x + 1)

.. _th-example:

A Template Haskell Worked Example
---------------------------------

To help you get over the confidence barrier, try out this skeletal
worked example. First cut and paste the two modules below into :file:`Main.hs`
and :file:`Printf.hs`:

::


    {- Main.hs -}
    module Main where

    -- Import our template "pr"
    import Printf ( pr )

    -- The splice operator $ takes the Haskell source code
    -- generated at compile time by "pr" and splices it into
    -- the argument of "putStrLn".
    main = putStrLn ( $(pr "Hello") )


    {- Printf.hs -}
    module Printf where

    -- Skeletal printf from the paper.
    -- It needs to be in a separate module to the one where
    -- you intend to use it.

    -- Import some Template Haskell syntax
    import Language.Haskell.TH

    -- Describe a format string
    data Format = D | S | L String

    -- Parse a format string.  This is left largely to you
    -- as we are here interested in building our first ever
    -- Template Haskell program and not in building printf.
    parse :: String -> [Format]
    parse s   = [ L s ]

    -- Generate Haskell source code from a parsed representation
    -- of the format string.  This code will be spliced into
    -- the module which calls "pr", at compile time.
    gen :: Quote m => [Format] -> m Exp
    gen [D]   = [| \n -> show n |]
    gen [S]   = [| \s -> s |]
    gen [L s] = stringE s

    -- Here we generate the Haskell code for the splice
    -- from an input format string.
    pr :: Quote m => String -> m Exp
    pr s = gen (parse s)

Now run the compiler,

.. code-block:: none

    $ ghc --make -XTemplateHaskell main.hs -o main

Run :file:`main` and here is your output:

.. code-block:: none

    $ ./main
    Hello

.. _th-rs:

Template Haskell quotes and Rebindable Syntax
---------------------------------------------

Rebindable syntax does not play well with untyped TH quotes:
applying the rebindable syntax rules would go against the lax
nature of untyped quotes that are accepted even in the presence of
unbound identifiers (see :ghc-ticket:`18102`). Applying the rebindable syntax
rules to them would force the code that defines the said quotes to have all
the necessary functions (e.g ``ifThenElse`` or ``fromInteger``) in scope,
instead of delaying the resolution of those symbols to the code that splices
the quoted Haskell syntax, as is usually done with untyped TH. For this reason,
even if a module has untyped TH quotes with ``RebindableSyntax`` enabled, GHC
turns off rebindable syntax while processing the quotes. The code that splices
the quotes is however free to turn on ``RebindableSyntax`` to have the usual
rules applied to the resulting code.

Typed TH quotes on the other hand are perfectly compatible with the eager
application of rebindable syntax rules, and GHC will therefore process any
such quotes according to the rebindable syntax rules whenever the
``RebindableSyntax`` extension is turned on in the modules where such quotes
appear.

.. _th-profiling:

Using Template Haskell with Profiling
-------------------------------------

.. index::
   single: profiling; with Template Haskell

Template Haskell relies on GHC's built-in bytecode compiler and
interpreter to run the splice expressions. The bytecode interpreter runs
the compiled expression on top of the same runtime on which GHC itself
is running; this means that the compiled code referred to by the
interpreted expression must be compatible with this runtime, and in
particular this means that object code that is compiled for profiling
*cannot* be loaded and used by a splice expression, because profiled
object code is only compatible with the profiling version of the
runtime.

This causes difficulties if you have a multi-module program containing
Template Haskell code and you need to compile it for profiling, because
GHC cannot load the profiled object code and use it when executing the
splices.

Fortunately GHC provides two workarounds.

The first option is to compile the program twice:

1. Compile the program or library first the normal way, without
   :ghc-flag:`-prof`.

2. Then compile it again with :ghc-flag:`-prof`, and additionally use ``-osuf
   p_o`` to name the object files differently (you can choose any suffix that
   isn't the normal object suffix here). GHC will automatically load the object
   files built in the first step when executing splice expressions. If you omit
   the :ghc-flag:`-osuf ⟨suffix⟩` flag when building with :ghc-flag:`-prof` and
   Template Haskell is used, GHC will emit an error message.

   .. index::
      single : -osuf; using with profiling

The second option is to add the flag :ghc-flag:`-fexternal-interpreter` (see
:ref:`external-interpreter`), which runs the interpreter in a separate
process, wherein it can load and run the profiled code directly.
There's no need to compile the code twice, just add
:ghc-flag:`-fexternal-interpreter` and it should just work.  (this option is
experimental in GHC 8.0.x, but it may become the default in future
releases).

.. _th-quasiquotation:

Template Haskell Quasi-quotation
--------------------------------

.. extension:: QuasiQuotes
    :shortdesc: Allow quasiquotation syntax.

    :since: 6.10.1

    Enable Template Haskell Quasi-quotation syntax.

Quasi-quotation allows patterns and expressions to be written using
programmer-defined concrete syntax; the motivation behind the extension
and several examples are documented in "`Why It's Nice to be Quoted:
Quasiquoting for
Haskell <https://www.cs.tufts.edu/comp/150FP/archive/geoff-mainland/quasiquoting.pdf>`__"
(Proc Haskell Workshop 2007). The example below shows how to write a
quasiquoter for a simple expression language.

Here are the salient features

-  A quasi-quote has the form ``[quoter| string |]``.

   -  The ⟨quoter⟩ must be the name of an imported quoter, either
      qualified or unqualified; it cannot be an arbitrary expression.

   -  The ⟨quoter⟩ cannot be "``e``", "``t``", "``d``", or "``p``",
      since those overlap with Template Haskell quotations.

   -  There must be no spaces in the token ``[quoter|``.

   -  The quoted ⟨string⟩ can be arbitrary, and may contain newlines.

   -  The quoted ⟨string⟩ finishes at the first occurrence of the
      two-character sequence ``"|]"``. Absolutely no escaping is
      performed. If you want to embed that character sequence in the
      string, you must invent your own escape convention (such as, say,
      using the string ``"|~]"`` instead), and make your quoter function
      interpret ``"|~]"`` as ``"|]"``. One way to implement this is to
      compose your quoter with a pre-processing pass to perform your
      escape conversion. See the discussion in :ghc-ticket:`5348` for details.

-  A quasiquote may appear in place of

   -  An expression

   -  A pattern

   -  A type

   -  A top-level declaration

   (Only the first two are described in the paper.)

-  A quoter is a value of type
   :th-ref:`Language.Haskell.TH.Quote.QuasiQuoter`, which is defined thus: ::

       data QuasiQuoter = QuasiQuoter { quoteExp  :: String -> Q Exp,
                                        quotePat  :: String -> Q Pat,
                                        quoteType :: String -> Q Type,
                                        quoteDec  :: String -> Q [Dec] }

   That is, a quoter is a tuple of four parsers, one for each of the
   contexts in which a quasi-quote can occur.

-  A quasi-quote is expanded by applying the appropriate parser to the
   string enclosed by the Oxford brackets. The context of the
   quasi-quote (expression, pattern, type, declaration) determines which
   of the parsers is called.

-  Unlike normal declaration splices of the form ``$(...)``, declaration
   quasi-quotes do not cause a declaration group break. See
   :ref:`th-syntax` for more information.

.. _quasi-quotes-list-comprehension-ambiguity:

.. warning::

    .. index::
        single: quasi-quotes; ambiguity with list comprehensions
        single: list comprehensions; ambiguity with quasi-quotes

    :extension:`QuasiQuotes` introduces an unfortunate ambiguity with list
    comprehension syntax. Consider the following, ::

        let x = [v| v <- [0..10]]

    Without :extension:`QuasiQuotes` this is parsed as a list comprehension.
    With :extension:`QuasiQuotes` this is parsed as a quasi-quote; however,
    this parse will fail due to the lack of a closing ``|]``. See
    :ghc-ticket:`11679`.

The example below shows quasi-quotation in action. The quoter ``expr``
is bound to a value of type ``QuasiQuoter`` defined in module ``Expr``.
The example makes use of an antiquoted variable ``n``, indicated by the
syntax ``'int:n`` (this syntax for anti-quotation was defined by the
parser's author, *not* by GHC). This binds ``n`` to the integer value
argument of the constructor ``IntExpr`` when pattern matching. Please
see the referenced paper for further details regarding anti-quotation as
well as the description of a technique that uses SYB to leverage a
single parser of type ``String -> a`` to generate both an expression
parser that returns a value of type ``Q Exp`` and a pattern parser that
returns a value of type ``Q Pat``.

Quasiquoters must obey the same level restrictions as Template Haskell,
e.g., in the example, ``expr`` cannot be defined in ``Main.hs`` where it
is used, but must be imported.

::

    {- ------------- file Main.hs --------------- -}
    module Main where

    import Expr

    main :: IO ()
    main = do { print $ eval [expr|1 + 2|]
              ; case IntExpr 1 of
                  { [expr|'int:n|] -> print n
                  ;  _              -> return ()
                  }
              }


    {- ------------- file Expr.hs --------------- -}
    module Expr where

    import qualified Language.Haskell.TH as TH
    import Language.Haskell.TH.Quote

    data Expr  =  IntExpr Integer
               |  AntiIntExpr String
               |  BinopExpr BinOp Expr Expr
               |  AntiExpr String
        deriving(Show, Typeable, Data)

    data BinOp  =  AddOp
                |  SubOp
                |  MulOp
                |  DivOp
        deriving(Show, Typeable, Data)

    eval :: Expr -> Integer
    eval (IntExpr n)        = n
    eval (BinopExpr op x y) = (opToFun op) (eval x) (eval y)
      where
        opToFun AddOp = (+)
        opToFun SubOp = (-)
        opToFun MulOp = (*)
        opToFun DivOp = div

    expr = QuasiQuoter { quoteExp = parseExprExp, quotePat =  parseExprPat }

    -- Parse an Expr, returning its representation as
    -- either a Q Exp or a Q Pat. See the referenced paper
    -- for how to use SYB to do this by writing a single
    -- parser of type String -> Expr instead of two
    -- separate parsers.

    parseExprExp :: String -> Q Exp
    parseExprExp ...

    parseExprPat :: String -> Q Pat
    parseExprPat ...

Now run the compiler:

.. code-block:: none

    $ ghc --make -XQuasiQuotes Main.hs -o main

Run "main" and here is your output:

.. code-block:: none

    $ ./main
    3
    1



