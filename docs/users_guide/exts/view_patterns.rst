.. _view-patterns:

View patterns
-------------

.. extension:: ViewPatterns
    :shortdesc: Enable view patterns.

    :since: 6.10.1

    Allow use of view pattern syntax.

View patterns are enabled by the language extension :extension:`ViewPatterns`. More
information and examples of view patterns can be found on the
:ghc-wiki:`Wiki page <view-patterns>`.

View patterns are somewhat like pattern guards that can be nested inside
of other patterns. They are a convenient way of pattern-matching against
values of abstract types. For example, in a programming language
implementation, we might represent the syntax of the types of the
language as follows: ::

    type Typ

    data TypView = Unit
                 | Arrow Typ Typ

    view :: Typ -> TypView

    -- additional operations for constructing Typ's ...

The representation of Typ is held abstract, permitting implementations
to use a fancy representation (e.g., hash-consing to manage sharing).
Without view patterns, using this signature is a little inconvenient: ::

    size :: Typ -> Integer
    size t = case view t of
      Unit -> 1
      Arrow t1 t2 -> size t1 + size t2

It is necessary to iterate the case, rather than using an equational
function definition. And the situation is even worse when the matching
against ``t`` is buried deep inside another pattern.

View patterns permit calling the view function inside the pattern and
matching against the result: ::

    size (view -> Unit) = 1
    size (view -> Arrow t1 t2) = size t1 + size t2

That is, we add a new form of pattern, written ⟨expression⟩ ``->``
⟨pattern⟩ that means "apply the expression to whatever we're trying to
match against, and then match the result of that application against the
pattern". The expression can be any Haskell expression of function type,
and view patterns can be used wherever patterns are used.

The semantics of a pattern ``(`` ⟨exp⟩ ``->`` ⟨pat⟩ ``)`` are as
follows:

-  Scoping:
   The variables bound by the view pattern are the variables bound by
   ⟨pat⟩.

   Any variables in ⟨exp⟩ are bound occurrences, but variables bound "to
   the left" in a pattern are in scope. This feature permits, for
   example, one argument to a function to be used in the view of another
   argument. For example, the function ``clunky`` from
   :ref:`pattern-guards` can be written using view patterns as follows: ::

       clunky env (lookup env -> Just val1) (lookup env -> Just val2) = val1 + val2
       ...other equations for clunky...

   More precisely, the scoping rules are:

   -  In a single pattern, variables bound by patterns to the left of a
      view pattern expression are in scope. For example: ::

          example :: Maybe ((String -> Integer,Integer), String) -> Bool
          example (Just ((f,_), f -> 4)) = True

      Additionally, in function definitions, variables bound by matching
      earlier curried arguments may be used in view pattern expressions
      in later arguments: ::

          example :: (String -> Integer) -> String -> Bool
          example f (f -> 4) = True

      That is, the scoping is the same as it would be if the curried
      arguments were collected into a tuple.

   -  In mutually recursive bindings, such as ``let``, ``where``, or the
      top level, view patterns in one declaration may not mention
      variables bound by other declarations. That is, each declaration
      must be self-contained. For example, the following program is not
      allowed: ::

          let {(x -> y) = e1 ;
               (y -> x) = e2 } in x

   (For some amplification on this design choice see :ghc-ticket:`4061`.

-  Typing: If ⟨exp⟩ has type ⟨T1⟩ ``->`` ⟨T2⟩ and ⟨pat⟩ matches a ⟨T2⟩,
   then the whole view pattern matches a ⟨T1⟩.

-  Matching: To the equations in Section 3.17.3 of the `Haskell 98
   Report <http://www.haskell.org/onlinereport/>`__, add the following: ::

       case v of { (e -> p) -> e1 ; _ -> e2 }
        =
       case (e v) of { p -> e1 ; _ -> e2 }

   That is, to match a variable ⟨v⟩ against a pattern ``(`` ⟨exp⟩ ``->``
   ⟨pat⟩ ``)``, evaluate ``(`` ⟨exp⟩ ⟨v⟩ ``)`` and match the result
   against ⟨pat⟩.

-  Efficiency: When the same view function is applied in multiple
   branches of a function definition or a case expression (e.g., in
   ``size`` above), GHC makes an attempt to collect these applications
   into a single nested case expression, so that the view function is
   only applied once. Pattern compilation in GHC follows the matrix
   algorithm described in Chapter 4 of `The Implementation of Functional
   Programming
   Languages <http://research.microsoft.com/~simonpj/Papers/slpj-book-1987/>`__.
   When the top rows of the first column of a matrix are all view
   patterns with the "same" expression, these patterns are transformed
   into a single nested case. This includes, for example, adjacent view
   patterns that line up in a tuple, as in

   ::

       f ((view -> A, p1), p2) = e1
       f ((view -> B, p3), p4) = e2

   The current notion of when two view pattern expressions are "the
   same" is very restricted: it is not even full syntactic equality.
   However, it does include variables, literals, applications, and
   tuples; e.g., two instances of ``view ("hi", "there")`` will be
   collected. However, the current implementation does not compare up to
   alpha-equivalence, so two instances of ``(x, view x -> y)`` will not
   be coalesced.


