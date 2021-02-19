.. _partial-type-signatures:

Partial Type Signatures
=======================

.. extension:: PartialTypeSignatures
    :shortdesc: Enable partial type signatures.

    :since: 7.10.1

    Type checker will allow inferred types for holes.

A partial type signature is a type signature containing special
placeholders called *wildcards*. A wildcard is written as an underscore (e.g. "``_``")
or, if :extension:`NamedWildCards` is enabled, any identifier with a leading
underscore (e.g. "``_foo``", "``_bar``"). Partial type signatures are to type
signatures what :ref:`typed-holes` are to expressions. During compilation these
wildcards or holes will generate an error message that describes which type was
inferred at the hole's location, and information about the origin of any free
type variables. GHC reports such error messages by default.

Unlike :ref:`typed-holes`, which make the program incomplete and will
generate errors when they are evaluated, this needn't be the case for
holes in type signatures. The type checker is capable (in most cases) of
type-checking a binding with or without a type signature. A partial type
signature bridges the gap between the two extremes, the programmer can
choose which parts of a type to annotate and which to leave over to the
type-checker to infer.

By default, the type-checker will report an error message for each hole
in a partial type signature, informing the programmer of the inferred
type. When the :extension:`PartialTypeSignatures` extension is enabled, the
type-checker will accept the inferred type for each hole, generating
warnings instead of errors. Additionally, these warnings can be silenced
with the :ghc-flag:`-Wno-partial-type-signatures <-Wpartial-type-signatures>`
flag.

However, because GHC must *infer* the type when part of a type is left
out, it is unable to use polymorphic recursion. The same restriction
takes place when the type signature is omitted completely.

A partial type signature als makes GHC generalise the binding even if
:extension:`MonoLocalBinds` is on; see :ref:`mono-local-binds`.

.. _pts-syntax:

Syntax
------

A (partial) type signature has the following form:
``forall a b .. . (C1, C2, ..) => tau``. It consists of three parts:

-  The type variables:
   ``a b ..``
-  The constraints:
   ``(C1, C2, ..)``
-  The (mono)type:
   ``tau``

We distinguish three kinds of wildcards.

.. _type-wildcards:

Type Wildcards
~~~~~~~~~~~~~~

Wildcards occurring within the monotype (tau) part of the type signature
are *type wildcards* ("type" is often omitted as this is the default
kind of wildcard). Type wildcards can be instantiated to any monotype
like ``Bool`` or ``Maybe [Bool]``, including functions and higher-kinded
types like ``(Int -> Bool)`` or ``Maybe``.

::

    not' :: Bool -> _
    not' x = not x
    -- Inferred: Bool -> Bool

    maybools :: _
    maybools = Just [True]
    -- Inferred: Maybe [Bool]

    just1 :: _ Int
    just1 = Just 1
    -- Inferred: Maybe Int

    filterInt :: _ -> _ -> [Int]
    filterInt = filter -- has type forall a. (a -> Bool) -> [a] -> [a]
    -- Inferred: (Int -> Bool) -> [Int] -> [Int]

For instance, the first wildcard in the type signature ``not'`` would
produce the following error message:

.. code-block:: none

    Test.hs:4:17: error:
        • Found type wildcard ‘_’ standing for ‘Bool’
          To use the inferred type, enable PartialTypeSignatures
        • In the type signature:
            not' :: Bool -> _
        • Relevant bindings include
            not' :: Bool -> Bool (bound at Test.hs:5:1)


When a wildcard is not instantiated to a monotype, it will be
generalised over, i.e. replaced by a fresh type variable, e.g.

::

    foo :: _ -> _
    foo x = x
    -- Inferred: forall t. t -> t

    filter' :: _
    filter' = filter -- has type forall a. (a -> Bool) -> [a] -> [a]
    -- Inferred: (a -> Bool) -> [a] -> [a]

.. _named-wildcards:

Named Wildcards
~~~~~~~~~~~~~~~

.. extension:: NamedWildCards
    :shortdesc: Enable named wildcards.

    :since: 7.10.1

    Allow naming of wildcards (e.g. ``_x``) in type signatures.

Type wildcards can also be named by giving the underscore an identifier
as suffix, i.e. ``_a``. These are called *named wildcards*. All
occurrences of the same named wildcard within one type signature will
unify to the same type. For example: ::

    f :: _x -> _x
    f ('c', y) = ('d', error "Urk")
    -- Inferred: forall t. (Char, t) -> (Char, t)

The named wildcard forces the argument and result types to be the same.
Lacking a signature, GHC would have inferred
``forall a b. (Char, a) -> (Char, b)``. A named wildcard can be
mentioned in constraints, provided it also occurs in the monotype part
of the type signature to make sure that it unifies with something: ::

    somethingShowable :: Show _x => _x -> _
    somethingShowable x = show x
    -- Inferred type: Show a => a -> String

    somethingShowable' :: Show _x => _x -> _
    somethingShowable' x = show (not x)
    -- Inferred type: Bool -> String

Besides an extra-constraints wildcard (see
:ref:`extra-constraints-wildcard`), only named wildcards can occur in
the constraints, e.g. the ``_x`` in ``Show _x``.

When :extension:`ScopedTypeVariables` is on, the named wildcards of a
function signature scope over the function body just like
explicitly-forall'd type variables (:ref:`scoped-type-variables`),
even though there is no explicit forall.  For example: ::

  f :: _a -> _a
  f x = let g :: _a -> _a
            g = ...
        in ...

Here the named wildcard ``_a`` scopes over the body of ``f``, thereby
binding the occurrences of ``_a`` in the signature of ``g``.  All
four occurrences stand for the same type.

Named wildcards *should not be confused with type variables*. Even
though syntactically similar, named wildcards can unify with monotypes
as well as be generalised over (and behave as type variables).

In the first example above, ``_x`` is generalised over (and is
effectively replaced by a fresh type variable ``a``). In the second
example, ``_x`` is unified with the ``Bool`` type, and as ``Bool``
implements the ``Show`` type class, the constraint ``Show Bool`` can be
simplified away.

By default, GHC (as the Haskell 2010 standard prescribes) parses
identifiers starting with an underscore in a type as type variables. To
treat them as named wildcards, the :extension:`NamedWildCards` extension should be
enabled. The example below demonstrated the effect. ::

    foo :: _a -> _a
    foo _ = False

Compiling this program without enabling :extension:`NamedWildCards` produces
the following error message complaining about the type variable ``_a``
no matching the actual type ``Bool``.

.. code-block:: none

    Test.hs:5:9: error:
        • Couldn't match expected type ‘_a’ with actual type ‘Bool’
          ‘_a’ is a rigid type variable bound by
            the type signature for:
              foo :: forall _a. _a -> _a
            at Test.hs:4:8
        • In the expression: False
          In an equation for ‘foo’: foo _ = False
        • Relevant bindings include foo :: _a -> _a (bound at Test.hs:5:1)

Compiling this program with :extension:`NamedWildCards` (as well as
:extension:`PartialTypeSignatures`) enabled produces the following error
message reporting the inferred type of the named wildcard ``_a``.

.. code-block:: none

    Test.hs:4:8: warning: [-Wpartial-type-signatures]
        • Found type wildcard ‘_a’ standing for ‘Bool’
        • In the type signature:
            foo :: _a -> _a
        • Relevant bindings include
            foo :: Bool -> Bool (bound at Test.hs:5:1)


.. _extra-constraints-wildcard:

Extra-Constraints Wildcard
~~~~~~~~~~~~~~~~~~~~~~~~~~

The third kind of wildcard is the *extra-constraints wildcard*. The
presence of an extra-constraints wildcard indicates that an arbitrary
number of extra constraints may be inferred during type checking and
will be added to the type signature. In the example below, the
extra-constraints wildcard is used to infer three extra constraints.

::

    arbitCs :: _ => a -> String
    arbitCs x = show (succ x) ++ show (x == x)
    -- Inferred:
    --   forall a. (Enum a, Eq a, Show a) => a -> String
    -- Error:
    Test.hs:5:12: error:
        Found constraint wildcard ‘_’ standing for ‘(Show a, Eq a, Enum a)’
        To use the inferred type, enable PartialTypeSignatures
        In the type signature:
          arbitCs :: _ => a -> String

An extra-constraints wildcard shouldn't prevent the programmer from
already listing the constraints they know or want to annotate, e.g.

::

    -- Also a correct partial type signature:
    arbitCs' :: (Enum a, _) => a -> String
    arbitCs' x = arbitCs x
    -- Inferred:
    --   forall a. (Enum a, Show a, Eq a) => a -> String
    -- Error:
    Test.hs:9:22: error:
        Found constraint wildcard ‘_’ standing for ‘()’
        To use the inferred type, enable PartialTypeSignatures
        In the type signature:
          arbitCs' :: (Enum a, _) => a -> String

An extra-constraints wildcard can also lead to zero extra constraints to
be inferred, e.g.

::

    noCs :: _ => String
    noCs = "noCs"
    -- Inferred: String
    -- Error:
    Test.hs:13:9: error:
        Found constraint wildcard ‘_’ standing for ‘()’
        To use the inferred type, enable PartialTypeSignatures
        In the type signature:
          noCs :: _ => String

As a single extra-constraints wildcard is enough to infer any number of
constraints, only one is allowed in a type signature and it should come
last in the list of constraints.

Extra-constraints wildcards cannot be named.

.. _pts-where:

Where can they occur?
---------------------

Partial type signatures are allowed for bindings, pattern and expression
signatures, except that extra-constraints
wildcards are not supported in pattern or expression signatures.
In the following example a wildcard is used in each of the three possible contexts.
::

    {-# LANGUAGE ScopedTypeVariables #-}
    foo :: _
    foo (x :: _) = (x :: _)
    -- Inferred: forall w_. w_ -> w_

Anonymous and named wildcards *can* occur on the left hand side of a
type or data instance declaration;
see :ref:`type-wildcards-lhs`.

Anonymous wildcards are also allowed in visible type applications/ visible kind
applications (:ref:`visible-type-application`). If you want to specify only the
second type argument to ``wurble``, then you can say ``wurble @_ @Int`` where
the first argument is a wildcard.

Standalone ``deriving`` declarations permit the use of a single,
extra-constraints wildcard, like so: ::

   deriving instance _ => Eq (Foo a)

This denotes a derived ``Eq (Foo a)`` instance where the context is inferred,
in much the same way that ordinary ``deriving`` clauses do. Any other use of
wildcards in a standalone ``deriving`` declaration is prohibited.

In all other contexts, type wildcards are disallowed, and a named wildcard is treated
as an ordinary type variable.  For example: ::

   class C _ where ...          -- Illegal
   instance Eq (T _)            -- Illegal (currently; would actually make sense)
   instance Eq _a => Eq (T _a)  -- Perfectly fine, same as  Eq a => Eq (T a)

Partial type signatures can also be used in :ref:`template-haskell`
splices.

-  Declaration splices: partial type signature are fully supported.
   ::

       {-# LANGUAGE TemplateHaskell, NamedWildCards #-}
       $( [d| foo :: _ => _a -> _a -> _
              foo x y = x == y|] )

-  Expression splices: anonymous and named wildcards can be used in
   expression signatures. Extra-constraints wildcards are not supported,
   just like in regular expression signatures.
   ::

       {-# LANGUAGE TemplateHaskell, NamedWildCards #-}
       $( [e| foo = (Just True :: _m _) |] )

-  Typed expression splices: the same wildcards as in (untyped)
   expression splices are supported.

-  Pattern splices: anonymous and named wildcards can be used in pattern
   signatures. Note that :extension:`ScopedTypeVariables` has to be enabled
   to allow pattern signatures. Extra-constraints wildcards are not supported,
   just like in regular pattern signatures.
   ::

       {-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
       foo $( [p| (x :: _) |] ) = x

-  Type splices: only anonymous wildcards are supported in type splices.
   Named and extra-constraints wildcards are not. ::

       {-# LANGUAGE TemplateHaskell #-}
       foo :: $( [t| _ |] ) -> a
       foo x = x


