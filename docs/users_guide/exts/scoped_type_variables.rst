.. _universal-quantification:

.. _scoped-type-variables:

Lexically scoped type variables
===============================

.. extension:: ScopedTypeVariables
    :shortdesc: Enable lexically-scoped type variables.

    :implies: :extension:`ExplicitForAll`
    :since: 6.8.1

    Enable lexical scoping of type variables explicitly introduced with
    ``forall``.

.. tip::

    :extension:`ScopedTypeVariables` breaks GHC's usual rule that explicit ``forall`` is optional and doesn't affect semantics.
    For the :ref:`decl-type-sigs` (or :ref:`exp-type-sigs`) examples in this section,
    the explicit ``forall`` is required.
    (If omitted, usually the program will not compile; in a few cases it will compile but the functions get a different signature.)
    To trigger those forms of :extension:`ScopedTypeVariables`, the ``forall`` must appear against the top-level signature (or outer expression)
    but *not* against nested signatures referring to the same type variables.

    Explicit ``forall`` is not always required -- see :ref:`pattern signature equivalent <pattern-equiv-form>` for the example in this section, or :ref:`pattern-type-sigs`.

GHC supports *lexically scoped type variables*, without which some type
signatures are simply impossible to write. For example: ::

    f :: forall a. [a] -> [a]
    f xs = ys ++ ys
         where
           ys :: [a]
           ys = reverse xs

The type signature for ``f`` brings the type variable ``a`` into scope,
because of the explicit ``forall`` (:ref:`decl-type-sigs`). The type
variables bound by a ``forall`` scope over the entire definition of the
accompanying value declaration. In this example, the type variable ``a``
scopes over the whole definition of ``f``, including over the type
signature for ``ys``. In Haskell 98 it is not possible to declare a type
for ``ys``; a major benefit of scoped type variables is that it becomes
possible to do so.

.. _pattern-equiv-form:

An equivalent form for that example, avoiding explicit ``forall`` uses :ref:`pattern-type-sigs`: ::

    f :: [a] -> [a]
    f (xs :: [aa]) = xs ++ ys
      where
        ys :: [aa]
        ys = reverse xs

Unlike the ``forall`` form, type variable ``a`` from ``f``'s signature is not scoped over ``f``'s equation(s).
Type variable ``aa`` bound by the pattern signature is scoped over the right-hand side of ``f``'s equation.
(Therefore there is no need to use a distinct type variable; using ``a`` would be equivalent.)


Overview
--------

The design follows the following principles

-  A scoped type variable stands for a type *variable*, and not for a
   *type*. (This is a change from GHC's earlier design.)

-  Furthermore, distinct lexical type variables stand for distinct type
   variables. This means that every programmer-written type signature
   (including one that contains free scoped type variables) denotes a
   *rigid* type; that is, the type is fully known to the type checker,
   and no inference is involved.

-  Lexical type variables may be alpha-renamed freely, without changing
   the program.

A *lexically scoped type variable* can be bound by:

-  A declaration type signature (:ref:`decl-type-sigs`)

-  An expression type signature (:ref:`exp-type-sigs`)

-  A pattern type signature (:ref:`pattern-type-sigs`)

-  Class and instance declarations (:ref:`cls-inst-scoped-tyvars`)

In Haskell, a programmer-written type signature is implicitly quantified
over its free type variables (`Section
4.1.2 <http://www.haskell.org/onlinereport/decls.html#sect4.1.2>`__ of
the Haskell Report). Lexically scoped type variables affect this
implicit quantification rules as follows: any type variable that is in
scope is *not* universally quantified. For example, if type variable
``a`` is in scope, then ::

      (e :: a -> a)     means     (e :: a -> a)
      (e :: b -> b)     means     (e :: forall b. b->b)
      (e :: a -> b)     means     (e :: forall b. a->b)

.. _decl-type-sigs:

Declaration type signatures
---------------------------

A declaration type signature that has *explicit* quantification (using
``forall``) brings into scope the explicitly-quantified type variables,
in the definition of the named function. For example: ::

      f :: forall a. [a] -> [a]
      f (x:xs) = xs ++ [ x :: a ]

The "``forall a``" brings "``a``" into scope in the definition of
"``f``".

This only happens if:

-  The quantification in ``f``\'s type signature is explicit. For
   example: ::

         g :: [a] -> [a]
         g (x:xs) = xs ++ [ x :: a ]

   This program will be rejected, because "``a``" does not scope over
   the definition of "``g``", so "``x::a``" means "``x::forall a. a``"
   by Haskell's usual implicit quantification rules.

-  The type variable is quantified by the single, syntactically visible,
   outermost ``forall`` of the type signature. For example, GHC will reject
   all of the following examples: ::

         f1 :: forall a. forall b. a -> [b] -> [b]
         f1 _ (x:xs) = xs ++ [ x :: b ]

         f2 :: forall a. a -> forall b. [b] -> [b]
         f2 _ (x:xs) = xs ++ [ x :: b ]

         type Foo = forall b. [b] -> [b]

         f3 :: Foo
         f3 (x:xs) = xs ++ [ x :: b ]

   In ``f1`` and ``f2``, the type variable ``b`` is not quantified by the
   outermost ``forall``, so it is not in scope over the bodies of the
   functions. Neither is ``b`` in scope over the body of ``f3``, as the
   ``forall`` is tucked underneath the ``Foo`` type synonym.

-  The signature gives a type for a function binding or a bare variable
   binding, not a pattern binding. For example: ::

         f1 :: forall a. [a] -> [a]
         f1 (x:xs) = xs ++ [ x :: a ]   -- OK

         f2 :: forall a. [a] -> [a]
         f2 = \(x:xs) -> xs ++ [ x :: a ]   -- OK

         f3 :: forall a. [a] -> [a]
         Just f3 = Just (\(x:xs) -> xs ++ [ x :: a ])   -- Not OK!

   ``f1`` is a function binding, and ``f2`` binds a bare variable;
   in both cases the type signature brings ``a`` into scope.
   However the binding for ``f3`` is a pattern binding,
   and so ``f3`` is a fresh variable brought into scope by the pattern,
   not connected with top level ``f3``.
   Then type variable ``a`` is not in scope of the right-hand side of ``Just f3 = ...``.

.. _exp-type-sigs:

Expression type signatures
--------------------------

An expression type signature that has *explicit* quantification (using
``forall``) brings into scope the explicitly-quantified type variables,
in the annotated expression. For example: ::

    f = runST ( (op >>= \(x :: STRef s Int) -> g x) :: forall s. ST s Bool )

Here, the type signature ``forall s. ST s Bool`` brings the type
variable ``s`` into scope, in the annotated expression
``(op >>= \(x :: STRef s Int) -> g x)``.

.. _pattern-type-sigs:

Pattern type signatures
-----------------------

A type signature may occur in any pattern; this is a *pattern type
signature*. For example: ::

    -- f and g assume that 'a' is already in scope
    f = \(x::Int, y::a) -> x

    g (x::a) = x

    h ((x,y) :: (Int,Bool)) = (y,x)

In the case where all the type variables in the pattern type signature
are already in scope (i.e. bound by the enclosing context), matters are
simple: the signature simply constrains the type of the pattern in the
obvious way.

Unlike expression and declaration type signatures, pattern type
signatures are not implicitly generalised. The pattern in a *pattern
binding* may only mention type variables that are already in scope. For
example: ::

    f :: forall a. [a] -> (Int, [a])
    f xs = (n, zs)
      where
        (ys::[a], n) = (reverse xs, length xs) -- OK
        (zs::[a])    = xs ++ ys                     -- OK

        Just (v::b)  = ...  -- Not OK; b is not in scope

Here, the pattern signatures for ``ys`` and ``zs`` are fine, but the one
for ``v`` is not because ``b`` is not in scope.

However, in all patterns *other* than pattern bindings, a pattern type
signature may mention a type variable that is not in scope; in this
case, *the signature brings that type variable into scope*. For example: ::

    -- same f and g as above, now assuming that 'a' is not already in scope
    f = \(x::Int, y::a) -> x           -- 'a' is in scope on RHS of ->

    g (x::a) = x :: a

    hh (Just (v :: b)) = v :: b

The pattern type signature makes the type variable available on the right-hand side of the equation.

Bringing type variables into scope is particularly important
for existential data constructors. For example: ::

    data T = forall a. MkT [a]

    k :: T -> T
    k (MkT [t::a]) =
        MkT t3
      where
        (t3::[a]) = [t,t,t]

Here, the pattern type signature ``[t::a]`` mentions a lexical type
variable that is not already in scope. Indeed, it *must not* already be in
scope, because it is bound by the pattern match.
The effect is to bring it into scope,
standing for the existentially-bound type variable.

It does seem odd that the existentially-bound type variable *must not*
be already in scope. Contrast that usually name-bindings merely shadow
(make a 'hole') in a same-named outer variable's scope.
But we must have *some* way to bring such type variables into scope,
else we could not name existentially-bound type variables
in subsequent type signatures.

Compare the two (identical) definitions for examples ``f``, ``g``;
they are both legal whether or not ``a`` is already in scope.
They differ in that *if* ``a`` is already in scope, the signature constrains
the pattern, rather than the pattern binding the variable.

.. _cls-inst-scoped-tyvars:

Class and instance declarations
-------------------------------

:extension:`ScopedTypeVariables` allow the type variables bound by the top of a
``class`` or ``instance`` declaration to scope over the methods defined in the
``where`` part. Unlike :ref`decl-type-sigs`, type variables from class and
instance declarations can be lexically scoped without an explicit ``forall``
(although you are allowed an explicit ``forall`` in an ``instance``
declaration; see :ref:`explicit-foralls`). For example: ::

      class C a where
        op :: [a] -> a

        op xs = let ys::[a]
                    ys = reverse xs
                in
                head ys

      instance C b => C [b] where
        op xs = reverse (head (xs :: [[b]]))

      -- Alternatively, one could write the instance above as:
      instance forall b. C b => C [b] where
        op xs = reverse (head (xs :: [[b]]))

While :extension:`ScopedTypeVariables` is required for type variables from the
top of a class or instance declaration to scope over the /bodies/ of the
methods, it is not required for the type variables to scope over the /type
signatures/ of the methods. For example, the following will be accepted without
explicitly enabling :extension:`ScopedTypeVariables`: ::

      class D a where
        m :: [a] -> a

      instance D [a] where
        m :: [a] -> [a]
        m = reverse

Note that writing ``m :: [a] -> [a]`` requires the use of the
:extension:`InstanceSigs` extension.

Similarly, :extension:`ScopedTypeVariables` is not required for type variables
from the top of the class or instance declaration to scope over associated type
families, which only requires the :extension:`TypeFamilies` extension. For
instance, the following will be accepted without explicitly enabling
:extension:`ScopedTypeVariables`: ::

      class E a where
        type T a

      instance E [a] where
        type T [a] = a

See :ref:`scoping-class-params` for further information.
