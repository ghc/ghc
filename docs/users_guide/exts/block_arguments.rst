.. _block-arguments:

More liberal syntax for function arguments
------------------------------------------

.. extension:: BlockArguments
    :shortdesc: Allow ``do`` blocks and other constructs as function arguments.

    :since: 8.6.1

    Allow ``do`` expressions, lambda expressions, etc. to be directly used as
    a function argument.

In Haskell 2010, certain kinds of expressions can be used without parentheses
as an argument to an operator, but not as an argument to a function.
They include ``do``, lambda, ``if``, ``case``, and ``let``
expressions. Some GHC extensions also define language constructs of this type:
``mdo`` (:ref:`recursive-do-notation`), ``\case`` (:ref:`lambda-case`), and
``proc`` (:ref:`arrow-notation`).

The :extension:`BlockArguments` extension allows these constructs to be directly
used as a function argument. For example::

    when (x > 0) do
      print x
      exitFailure

will be parsed as::

    when (x > 0) (do
      print x
      exitFailure)

and

::

    withForeignPtr fptr \ptr -> c_memcpy buf ptr size

will be parsed as::

    withForeignPtr fptr (\ptr -> c_memcpy buf ptr size)

Changes to the grammar
~~~~~~~~~~~~~~~~~~~~~~

The Haskell report `defines
<https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-220003>`_
the ``lexp`` nonterminal thus (``*`` indicates a rule of interest)

.. code-block:: none

    lexp  →  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
          |  let decls in exp                  (let expression)             *
          |  if exp [;] then exp [;] else exp  (conditional)                *
          |  case exp of { alts }              (case expression)            *
          |  do { stmts }                      (do expression)              *
          |  fexp

    fexp  →  [fexp] aexp                       (function application)

    aexp  →  qvar                              (variable)
          |  gcon                              (general constructor)
          |  literal
          |  ( exp )                           (parenthesized expression)
          |  qcon { fbind1 … fbindn }          (labeled construction)
          |  aexp { fbind1 … fbindn }          (labelled update)
          |  …

The :extension:`BlockArguments` extension moves these production rules under
``aexp``

.. code-block:: none

    lexp  →  fexp

    fexp  →  [fexp] aexp                       (function application)

    aexp  →  qvar                              (variable)
          |  gcon                              (general constructor)
          |  literal
          |  ( exp )                           (parenthesized expression)
          |  qcon { fbind1 … fbindn }          (labeled construction)
          |  aexp { fbind1 … fbindn }          (labelled update)
          |  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
          |  let decls in exp                  (let expression)             *
          |  if exp [;] then exp [;] else exp  (conditional)                *
          |  case exp of { alts }              (case expression)            *
          |  do { stmts }                      (do expression)              *
          |  …

Now the ``lexp`` nonterminal is redundant and can be dropped from the grammar.

Note that this change relies on an existing meta-rule to resolve ambiguities:

    The grammar is ambiguous regarding the extent of lambda abstractions, let
    expressions, and conditionals. The ambiguity is resolved by the meta-rule
    that each of these constructs extends as far to the right as possible.

For example, ``f \a -> a b`` will be parsed as ``f (\a -> a b)``, not as ``f
(\a -> a) b``.


