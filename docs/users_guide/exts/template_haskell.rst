.. _template-haskell:

Template Haskell
================

Template Haskell allows you to do compile-time meta-programming in
Haskell. The background to the main technical innovations is discussed
in "`Template Meta-programming for
Haskell <http://research.microsoft.com/~simonpj/papers/meta-haskell/>`__"
(Proc Haskell Workshop 2002).

The `Template Haskell <http://www.haskell.org/haskellwiki/Template_Haskell>`__
page on the GHC Wiki has a wealth of information. You may also consult the
Haddock reference documentation :th-ref:`Language.Haskell.TH.`.
Many changes to the original
design are described in `Notes on Template Haskell version
2 <https://www.haskell.org/ghc/docs/papers/th2.ps>`__.
Not all of these changes are in GHC, however.

The first example from that paper is set out below (:ref:`th-example`)
as a worked example to help get you started.

The documentation here describes the realisation of Template Haskell in
GHC. It is not detailed enough to understand Template Haskell; see the
`Wiki page <http://haskell.org/haskellwiki/Template_Haskell>`__.

.. _th-syntax:

Syntax
------

.. extension:: TemplateHaskell
    :shortdesc: Enable Template Haskell.

    :implies: :extension:`TemplateHaskellQuotes`
    :since: 6.0. Typed splices introduced in GHC 7.8.1.

    Enable Template Haskell's splice and quotation syntax.

.. extension:: TemplateHaskellQuotes
    :shortdesc: Enable quotation subset of
        :ref:`Template Haskell <template-haskell>`.

    :since: 8.0.1

    Enable only Template Haskell's quotation syntax.

Template Haskell has the following new syntactic constructions. You need to use
the extension :extension:`TemplateHaskell` to switch these syntactic extensions on.
Alternatively, the :extension:`TemplateHaskellQuotes` extension can be used to
enable the quotation subset of Template Haskell (i.e. without top-level splices).
The :extension:`TemplateHaskellQuotes` extension is considered safe under
:ref:`safe-haskell` while :extension:`TemplateHaskell` is not.

-  A splice is written ``$x``, where ``x`` is an arbitrary expression.
   There must be no space between the "$" and the expression.
   This use of "$" overrides its meaning as an infix operator, just as "M.x"
   overrides the meaning of "." as an infix operator. If you want the
   infix operator, put spaces around it.

   A top-level splice can occur in place of

   -  an expression; the spliced expression must have type ``Q Exp``

   -  a pattern; the spliced pattern must have type ``Q Pat``

   -  a type; the spliced expression must have type ``Q Type``

   -  a list of declarations at top level; the spliced expression must
      have type ``Q [Dec]``

   Inside a splice you can only call functions defined in imported
   modules, not functions defined elsewhere in the same module. Note
   that declaration splices are not allowed anywhere except at top level
   (outside any other declarations).

   The ``Q`` monad is a monad defined in :th-ref:`Language.Haskell.TH.Syntax.` which
   supports several useful operations during code generation such as reporting
   errors or looking up identifiers in the environment.

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

-  Splices can be nested inside quotation brackets. For example the fragment
   representing ``1 + 2`` can be constructed using nested splices::

    oneC, twoC, plusC  :: Quote m => m Exp
    oneC = [| 1 |]

    twoC = [| 2 |]

    plusC = [| $oneC + $twoC |]

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

-  A *typed* expression splice is written ``$$x``, where ``x`` is
   is an arbitrary expression.

   A top-level typed expression splice can occur in place of an expression; the
   spliced expression must have type ``Code Q a``

-  A *typed* expression quotation is written as ``[|| ... ||]``, or
   ``[e|| ... ||]``, where the "..." is an expression; if the "..."
   expression has type ``a``, then the quotation has type
   ``Quote m => Code m a``.

   It is possible to extract a value of type ``m Exp`` from ``Code m a``
   using the ``unTypeCode :: Code m a -> m Exp`` function.

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

      A name whose second character is a single quote (sadly) cannot be
      quoted in this way, because it will be parsed instead as a quoted
      character. For example, if the function is called ``f'7`` (which
      is a legal Haskell identifier), an attempt to quote it as ``'f'7``
      would be parsed as the character literal ``'f'`` followed by the
      numeric literal ``7``. There is no current escape mechanism in
      this (unusual) situation.

   -  ``''T`` has type ``Name``, and names the type constructor ``T``.
      That is, ``''``\ ⟨thing⟩ interprets ⟨thing⟩ in a type context.

   These ``Names`` can be used to construct Template Haskell
   expressions, patterns, declarations etc. They may also be given as an
   argument to the ``reify`` function.

-  It is possible for a splice to expand to an expression that contain
   names which are not in scope at the site of the splice. As an
   example, consider the following code: ::

       module Bar where

       import Language.Haskell.TH

       add1 :: Quote m => Int -> m Exp
       add1 x = [| x + 1 |]

   Now consider a splice using ``add1`` in a separate
   module: ::

       module Foo where

       import Bar

       two :: Int
       two = $(add1 1)

   Template Haskell cannot know what the argument to ``add1`` will be at the
   function's definition site, so a lifting mechanism is used to promote
   ``x`` into a value of type ``Quote m => m Exp``. This functionality is exposed to the
   user as the ``Lift`` typeclass in the ``Language.Haskell.TH.Syntax``
   module. If a type has a ``Lift`` instance, then any of its values can be
   lifted to a Template Haskell expression: ::

       class Lift t where
           lift :: Quote m => t -> m Exp
           liftTyped :: Quote m => t -> Code m t

   In general, if GHC sees an expression within Oxford brackets (e.g., ``[|
   foo bar |]``, then GHC looks up each name within the brackets. If a name
   is global (e.g., suppose ``foo`` comes from an import or a top-level
   declaration), then the fully qualified name is used directly in the
   quotation. If the name is local (e.g., suppose ``bar`` is bound locally in
   the function definition ``mkFoo bar = [| foo bar |]``), then GHC uses
   ``lift`` on it (so GHC pretends ``[| foo bar |]`` actually contains ``[|
   foo $(lift bar) |]``). Local names, which are not in scope at splice
   locations, are actually evaluated when the quotation is processed.

   The ``template-haskell`` library provides ``Lift`` instances for many
   common data types. Furthermore, it is possible to derive ``Lift``
   instances automatically by using the :extension:`DeriveLift` language extension.
   See :ref:`deriving-lift` for more information.

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

-  Top-level declaration splices break up a source file into
   *declaration groups*. A *declaration group* is the group of
   declarations created by a top-level declaration splice, plus those
   following it, down to but not including the next top-level
   declaration splice. N.B. only top-level splices delimit declaration
   groups, not expression splices. The first declaration group in a module
   includes all top-level definitions down to but not including the first
   top-level declaration splice.

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

.. _th-usage:

Using Template Haskell
----------------------

-  The data types and monadic constructor functions for Template Haskell
   are in the library :th-ref:`Language.Haskell.TH.Syntax.`.

-  You can only run a function at compile time if it is imported from
   another module. That is, you can't define a function in a module, and
   call it from within a splice in the same module. (It would make sense
   to do so, but it's hard to implement.)

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

Template Haskell works in any mode (:ghc-flag:`--make`,
:ghc-flag:`--interactive`, or file-at-a-time). There used to be a restriction to
the former two, but that restriction has been lifted.

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
    :shortdesc: Enable quasiquotation.

    :since: 6.10.1

    Enable Template Haskell Quasi-quotation syntax.

Quasi-quotation allows patterns and expressions to be written using
programmer-defined concrete syntax; the motivation behind the extension
and several examples are documented in "`Why It's Nice to be Quoted:
Quasiquoting for
Haskell <http://www.cs.tufts.edu/comp/150FP/archive/geoff-mainland/quasiquoting.pdf>`__"
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

Quasiquoters must obey the same stage restrictions as Template Haskell,
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


