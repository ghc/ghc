.. _gadt:

Generalised Algebraic Data Types (GADTs)
----------------------------------------

.. extension:: GADTs
    :shortdesc: Enable generalised algebraic data types.
        Implies :extension:`GADTSyntax` and :extension:`MonoLocalBinds`.

    :implies: :extension:`MonoLocalBinds`, :extension:`GADTSyntax`
    :since: 6.8.1

    Allow use of Generalised Algebraic Data Types (GADTs).

Generalised Algebraic Data Types generalise ordinary algebraic data
types by allowing constructors to have richer return types. Here is an
example: ::

      data Term a where
          Lit    :: Int -> Term Int
          Succ   :: Term Int -> Term Int
          IsZero :: Term Int -> Term Bool
          If     :: Term Bool -> Term a -> Term a -> Term a
          Pair   :: Term a -> Term b -> Term (a,b)

Notice that the return type of the constructors is not always
``Term a``, as is the case with ordinary data types. This generality
allows us to write a well-typed ``eval`` function for these ``Terms``: ::

      eval :: Term a -> a
      eval (Lit i)      = i
      eval (Succ t)     = 1 + eval t
      eval (IsZero t)   = eval t == 0
      eval (If b e1 e2) = if eval b then eval e1 else eval e2
      eval (Pair e1 e2) = (eval e1, eval e2)

The key point about GADTs is that *pattern matching causes type
refinement*. For example, in the right hand side of the equation ::

      eval :: Term a -> a
      eval (Lit i) =  ...

the type ``a`` is refined to ``Int``. That's the whole point! A precise
specification of the type rules is beyond what this user manual aspires
to, but the design closely follows that described in the paper `Simple
unification-based type inference for
GADTs <http://research.microsoft.com/%7Esimonpj/papers/gadt/>`__, (ICFP
2006). The general principle is this: *type refinement is only carried
out based on user-supplied type annotations*. So if no type signature is
supplied for ``eval``, no type refinement happens, and lots of obscure
error messages will occur. However, the refinement is quite general. For
example, if we had: ::

      eval :: Term a -> a -> a
      eval (Lit i) j =  i+j

the pattern match causes the type ``a`` to be refined to ``Int``
(because of the type of the constructor ``Lit``), and that refinement
also applies to the type of ``j``, and the result type of the ``case``
expression. Hence the addition ``i+j`` is legal.

These and many other examples are given in papers by Hongwei Xi, and Tim
Sheard. There is a longer introduction `on the
wiki <http://www.haskell.org/haskellwiki/GADT>`__, and Ralf Hinze's `Fun
with phantom
types <http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf>`__ also
has a number of examples. Note that papers may use different notation to
that implemented in GHC.

The rest of this section outlines the extensions to GHC that support
GADTs. The extension is enabled with :extension:`GADTs`. The :extension:`GADTs` extension
also sets :extension:`GADTSyntax` and :extension:`MonoLocalBinds`.

-  A GADT can only be declared using GADT-style syntax
   (:ref:`gadt-style`); the old Haskell 98 syntax for data declarations
   always declares an ordinary data type. The result type of each
   constructor must begin with the type constructor being defined, but
   for a GADT the arguments to the type constructor can be arbitrary
   monotypes. For example, in the ``Term`` data type above, the type of
   each constructor must end with ``Term ty``, but the ``ty`` need not
   be a type variable (e.g. the ``Lit`` constructor).

-  It is permitted to declare an ordinary algebraic data type using
   GADT-style syntax. What makes a GADT into a GADT is not the syntax,
   but rather the presence of data constructors whose result type is not
   just ``T a b``.

-  You cannot use a ``deriving`` clause for a GADT; only for an ordinary
   data type.

-  As mentioned in :ref:`gadt-style`, record syntax is supported. For
   example:

   ::

         data Term a where
             Lit    :: { val  :: Int }      -> Term Int
             Succ   :: { num  :: Term Int } -> Term Int
             Pred   :: { num  :: Term Int } -> Term Int
             IsZero :: { arg  :: Term Int } -> Term Bool
             Pair   :: { arg1 :: Term a
                       , arg2 :: Term b
                       }                    -> Term (a,b)
             If     :: { cnd  :: Term Bool
                       , tru  :: Term a
                       , fls  :: Term a
                       }                    -> Term a

   However, for GADTs there is the following additional constraint:
   every constructor that has a field ``f`` must have the same result
   type (modulo alpha conversion) Hence, in the above example, we cannot
   merge the ``num`` and ``arg`` fields above into a single name.
   Although their field types are both ``Term Int``, their selector
   functions actually have different types:

   ::

         num :: Term Int -> Term Int
         arg :: Term Bool -> Term Int

   See :ref:`field-selectors-and-type-applications` for a full description of
   how the types of top-level field selectors are determined.

-  When pattern-matching against data constructors drawn from a GADT,
   for example in a ``case`` expression, the following rules apply:

   -  The type of the scrutinee must be rigid.

   -  The type of the entire ``case`` expression must be rigid.

   -  The type of any free variable mentioned in any of the ``case``
      alternatives must be rigid.

   A type is "rigid" if it is completely known to the compiler at its
   binding site. The easiest way to ensure that a variable a rigid type
   is to give it a type signature. For more precise details see `Simple
   unification-based type inference for
   GADTs <http://research.microsoft.com/%7Esimonpj/papers/gadt/>`__. The
   criteria implemented by GHC are given in the Appendix.

-  When GHC typechecks multiple patterns in a function clause, it typechecks
   each pattern in order from left to right. This has consequences for patterns
   that match on GADTs, such as in this example: ::

       data U a where
         MkU :: U ()

       v1 :: U a -> a -> a
       v1 MkU () = ()

       v2 :: a -> U a -> a
       v2 () MkU = ()

   Although ``v1`` and ``v2`` may appear to be the same function but with
   differently ordered arguments, GHC will only typecheck ``v1``. This is
   because in ``v1``, GHC will first typecheck the ``MkU`` pattern, which
   causes ``a`` to be refined to ``()``. This refinement is what allows the
   subsequent ``()`` pattern to typecheck at type ``a``. In ``v2``, however,
   GHC first tries to typecheck the ``()`` pattern, and because ``a`` has not
   been refined to ``()`` yet, GHC concludes that ``()`` is not of type ``a``.
   ``v2`` can be made to typecheck by matching on ``MkU`` before ``()``, like
   so: ::

       v2 :: a -> U a -> a
       v2 x MkU = case x of () -> ()

-  Not only does GHC typecheck patterns from left to right, it also typechecks
   them from the outside in. This can be seen in this example: ::

       data F x y where
         MkF :: y -> F (Maybe z) y

       g :: F a a -> a
       g (MkF Nothing) = Nothing

   In the function clause for ``g``, GHC first checks ``MkF``, the outermost
   pattern, followed by the inner ``Nothing`` pattern. This outside-in order
   can interact somewhat counterintuitively with :ref:`pattern-type-sigs`.
   Consider the following variation of ``g``: ::

       g2 :: F a a -> a
       g2 (MkF Nothing :: F (Maybe z) (Maybe z)) = Nothing @z

   The ``g2`` function attempts to use the pattern type signature
   ``F (Maybe z) (Maybe z)`` to bring the type variable ``z`` into scope so
   that it can be used on the right-hand side of the definition with
   :ref:`visible-type-application`. However, GHC will reject the pattern type
   signature in ``g2``: ::

       • Couldn't match type ‘a’ with ‘Maybe z’
         Expected: F a a
           Actual: F (Maybe z) (Maybe z)

   Again, this is because of the outside-in order GHC uses when typechecking
   patterns. GHC first tries to check the pattern type signature
   ``F (Maybe z) (Maybe z)``, but at that point, GHC has not refined ``a`` to
   be ``Maybe z``, so GHC is unable to conclude that ``F a a`` is equal to
   ``F (Maybe z) (Maybe z)``. Here, the ``MkF`` pattern is considered to be
   inside of the pattern type signature, so GHC cannot use the type refinement
   from the ``MkF`` pattern when typechecking the pattern type signature.

   There are two possible ways to repair ``g2``. One way is to use a ``case``
   expression to write a pattern signature *after* matching on ``MkF``, like
   so: ::

       g3 :: F a a -> a
       g3 f@(MkF Nothing) =
         case f of
           (_ :: F (Maybe z) (Maybe z)) -> Nothing @z

   Another way is to use :ref:`type-applications-in-patterns` instead of a
   pattern type signature: ::

       g4 :: F a a -> a
       g4 (MkF @(Maybe z) Nothing) = Nothing @z

   Here, the visible type argument ``@(Maybe z)`` indicates that the ``y``
   in the type of ``MkF :: y -> F (Maybe z) y`` should be instantiated to
   ``Maybe z``. In addition, ``@(Maybe z)`` also brings ``z`` into scope.
   Although ``g4`` no longer uses a pattern type signature, it accomplishes the
   same end result, as the right-hand side ``Nothing @z`` will typecheck
   successfully.
