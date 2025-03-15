Required type arguments
=======================

.. extension:: RequiredTypeArguments
    :shortdesc: Allow use of required type argument syntax in terms.

    :since: 9.10.1
    :status: Experimental

    Allow visible dependent quantification ``forall x ->`` in types of terms.

**This feature is only partially implemented in GHC.** In this section we
describe the implemented subset, while the full specification can be found in
`GHC Proposal #281 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst>`__.

The :extension:`RequiredTypeArguments` extension enables the use of visible
dependent quantification in types of terms::

  id     :: forall a.   a -> a         -- invisible dependent quantification
  id_vdq :: forall a -> a -> a         --   visible dependent quantification

The arrow in ``forall a ->`` is part of the syntax and not a function arrow,
just like the dot in ``forall a.`` is not a type operator.

The choice between ``forall a.`` and ``forall a ->`` does not have any effect on
program execution. Both quantifiers introduce type variables, which are erased
during compilation. Rather, the main difference is in the syntax used at call
sites::

  x1 = id       True     -- invisible forall, the type argument is inferred by GHC
  x2 = id @Bool True     -- invisible forall, the type argument is supplied by the programmer

  x3 = id_vdq _    True  --   visible forall, the type argument is inferred by GHC
  x4 = id_vdq Bool True  --   visible forall, the type argument is supplied by the programmer

.. _dependent-quantifier:

Terminology: Dependent quantifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Both ``forall a.`` and ``forall a ->`` are said to be "dependent" because the
result type depends on the supplied type argument: ::

  id @Integer :: Integer -> Integer
  id @String  :: String  -> String

  id_vdq Integer :: Integer -> Integer
  id_vdq String  :: String  -> String

Notice how the RHS of the signature is influenced by the LHS.

This is in contrast to the function arrow ``->``, which is a non-dependent
quantifier::

  putStrLn "Hello" :: IO ()
  putStrLn "World" :: IO ()

The type of ``putStrLn`` is ``String -> IO ()``. No matter what string we pass
as input, the result type ``IO ()`` does not depend on it.

This notion of dependence is weaker than the one used in dependently-typed
languages (see :ref:`pi-types`).

Terminology: Visible quantifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We say that ``forall a.`` is an *invisible* quantifier and ``forall a ->`` is a
*visible* quantifier. This notion of "visibility" is unrelated to implicit
quantification, which happens when the quantifier is omitted: ::

  id     ::             a -> a    -- implicit quantification, invisible forall
  id     :: forall a.   a -> a    -- explicit quantification, invisible forall
  id_vdq :: forall a -> a -> a    -- explicit quantification,   visible forall

The property of "visibility" actually describes whether the corresponding type
argument is visible at the definition site and at call sites: ::

  -- Invisible quantification
  id :: forall a. a -> a
  id x = x                -- defn site: `a` is not mentioned
  call_id = id True       -- call site: `a` is invisibly instantiated to `Bool`

  -- Visible quantification
  id_vdq :: forall a -> a -> a
  id_vdq t x = x                  -- defn site: `a` is visibly bound to `t`
  call_id_vdq = id_vdq Bool True  -- call site: `a` is visibly instantiated to `Bool`

In the equation for ``id`` there is just one binder on the LHS, ``x``, and it
corresponds to the value argument, not to the type argument. Compare that with
the definition of ``id_vdq``::

  id_vdq :: forall a -> a -> a
  id_vdq t x = x

This time we have two binders on the LHS:

* ``t``, corresponding to ``forall a ->`` in the signature
* ``x``, corresponding to ``a ->`` in the signature

The bound ``t`` can be used in subsequent patterns, as well as on the right-hand
side of the equation::

  id_vdq :: forall a -> a -> a
  id_vdq t (x :: t) = x :: t
  --     ↑       ↑         ↑
  --   bound    used      used

We use the terms "visible type argument" and "required type argument"
interchangeably.

Relation to :extension:`TypeApplications`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:extension:`RequiredTypeArguments` are similar to :extension:`TypeApplications`
in that we pass a type to a function as an explicit argument. The difference is
that type applications are optional: it is up to the caller whether to write
``id @Bool True`` or ``id True``. By default, the compiler infers that the
type variable is instantiated to ``Bool``. The existence of a type argument is
not reflected syntactically in the expression, it is invisible unless we use a
*visibility override*, i.e. ``@``.

Required type arguments are compulsory. They must appear syntactically at call
sites::

  x1 = id_vdq Bool True    -- OK
  x2 = id_vdq      True    -- not OK

You may use an underscore to infer a required type argument::

  x3 = id_vdq _ True       -- OK

That is, it is mostly a matter of syntax whether to use ``forall a.`` with type
applications or ``forall a ->``. One advantage of required type arguments is that
they are never ambiguous. Consider the type of ``Foreign.Storable.sizeOf``::

  sizeOf :: forall a. Storable a => a -> Int

The value parameter is not actually used, its only purpose is to drive type
inference. At call sites, one might write ``sizeOf (undefined :: Bool)`` or
``sizeOf @Bool undefined``. Either way, the ``undefined`` is entirely
superfluous and exists only to avoid an ambiguous type variable.

With :extension:`RequiredTypeArguments`, we can imagine a slightly different API::

  sizeOf :: forall a -> Storable a => Int

If ``sizeOf`` had this type, we could write ``sizeOf Bool`` without
passing a dummy value.

Required type arguments are erased during compilation. While the source program
appears to bind and pass required type arguments alongside value arguments, the
compiled program does not. There is no runtime overhead associated with required
type arguments relative to the usual, invisible type arguments.

Relation to :extension:`ExplicitNamespaces`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A required type argument is syntactically indistinguishable from a value
argument. In a function call ``f arg1 arg2 arg3``, it is impossible to tell,
without looking at the type of ``f``, which of the three arguments are required
type arguments, if any.

At the same time, one of the design goals of GHC is to be able to perform name
resolution (find the binding sites of identifiers) without involving the type
system. Consider: ::

  data Ty = Int | Double | String deriving Show
  main = print Int

In this example, there are two constructors named ``Int`` in scope:

* The **type constructor** ``Int`` of kind ``Type`` (imported from ``Prelude``)
* The **data constructor** ``Int`` of type ``Ty`` (defined locally)

How does the compiler or someone reading the code know that ``print Int`` is
supposed to refer to the data constructor, not the type constructor?  In GHC,
this is resolved as follows. Each identifier is said to occur either in
**type syntax** or **term syntax**, depending on the surrounding syntactic
context::

  -- Examples of X in type syntax
  type T = X      -- RHS of a type synonym
  data D = MkD X  -- field of a data constructor declaration
  a :: X          -- RHS of a type signature
  b = f (c :: X)  -- RHS of a type signature (in expressions)
  f (x :: X) = x  -- RHS of a type signature (in patterns)

  -- Examples of X in term syntax
  c X = a         -- LHS of a function equation
  c a = X         -- RHS of a function equation

One could imagine the entire program "zoned" into type syntax and term syntax,
each zone having its own rules for name resolution:

* In type syntax, type constructors take precedence over data constructors.
* In term syntax, data constructors take precedence over type constructors.

This means that in the ``print Int`` example, the data constructor is selected
solely based on the fact that the ``Int`` occurs in term syntax. This is firmly
determined before GHC attempts to type-check the expression, so the type of
``print`` does not influence which of the two ``Int``\s is passed to it.

This may not be the desired behavior in a required type argument. Consider::

  vshow :: forall a -> Show a => a -> String
  vshow t x = show (x :: t)

  s1 = vshow Int    42      -- "42"
  s2 = vshow Double 42      -- "42.0"

The function calls ``vshow Int 42`` and ``vshow Double 42`` are written in
*term* syntax, while the intended referents of ``Int`` and ``Double`` are the
respective *type* constructors. As long as there are no data constructors named
``Int`` or ``Double`` in scope, the example works as intended. However, if such
clashing constructor names are introduced, they may disrupt name resolution::

  data Ty = Int | Double | String

  vshow :: forall a -> Show a => a -> String
  vshow t x = show (x :: t)

  s1 = vshow Int    42      -- error: Expected a type, but ‘Int’ has kind ‘Ty’
  s2 = vshow Double 42      -- error: Expected a type, but ‘Double’ has kind ‘Ty’

In this example the intent was to refer to ``Int`` and ``Double`` as types, but
the names were resolved in favor of data constructors, resulting in type errors.

The example can be fixed with the help of :extension:`ExplicitNamespaces`, which
allows embedding type syntax into term syntax using the ``type`` keyword::

  s1 = vshow (type Int)    42
  s2 = vshow (type Double) 42

A similar problem occurs with list and tuple syntax. In type syntax, ``[a]`` is
the type of a list, i.e. ``Data.List.List a``. In term syntax, ``[a]`` is a
singleton list, i.e. ``a : []``. A naive attempt to use the list type as a
required type argument will result in a type error::

  s3 = vshow [Int] [1,2,3]  -- error: Expected a type, but ‘[Int]’ has kind ‘[Type]’

The problem is that GHC assumes ``[Int]`` to stand for ``Int : []`` instead of
the intended ``Data.List.List Int``. This, too, can be solved using the ``type`` keyword::

  s3 = vshow (type [Int]) [1,2,3]

Since the ``type`` keyword is merely a namespace disambiguation mechanism, it
need not apply to the entire type argument. Using it to disambiguate only a part
of the type argument is also valid::

  f :: forall a -> ...   -- `f`` is a function that expects a required type argument

  r1 = f (type (Either () Int))           -- `type` applied to the entire type argument
  r2 = f (Either (type ()) Int)           -- `type` applied to one part of it
  r3 = f (Either (type ()) (type Int))    -- `type` applied to multiple parts

That is, the expression ``Either (type ()) (type Int)`` does *not* indicate that
``Either`` is applied to two type arguments; rather, the entire expression is a
single type argument and ``type`` is used to disambiguate parts of it.

Outside a required type argument, it is illegal to use ``type``:
::

  r4 = type Int  -- illegal use of ‘type’

Types in terms
~~~~~~~~~~~~~~

**Since:** GHC 9.12

:extension:`RequiredTypeArguments` extends the grammar of term-level
expressions with syntax that is typically found only in types:

* function types: ``a -> b``, ``a ⊸ b``, ``a %m -> b``
* constrained types: ``ctx => t``
* universally quantified types: ``forall tvs. t``, ``forall tvs -> t``

These so-called "types in terms" make it possible to pass any types as required
type arguments::

  a1 = f (Int -> Bool)                       -- function type
  a2 = f (Int %1 -> String)                  -- linear function type
  a3 = f (Read T => T)                       -- constrained type
  a4 = f (forall a. a)                       -- universally quantified type
  a5 = f (forall a. Read a => String -> a)   -- a combination of the above

A few limitations apply:

* The ``*`` syntax of :extension:`StarIsType` is not available due to a
  conflict with the multiplication operator.
  What to do instead: use ``Type`` from the ``Data.Kind`` module.

* The ``'`` syntax of :extension:`DataKinds` is not available due to a conflict
  with :extension:`TemplateHaskell` name quotation.
  What to do instead: simply omit the ``'``.

Effect on implicit quantification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Implicit quantification is said to occur when GHC inserts an implicit ``forall``
to bind type variables::

  const :: a -> b -> a               -- implicit quantification
  const :: forall a b. a -> b -> a   -- explicit quantification

Normally, implicit quantification is unaffected by term variables in scope: ::

  f a = ...  -- the LHS binds `a`
    where const :: a -> b -> a
             -- implicit quantification over `a` takes place
             -- despite the `a` bound on the LHS of `f`

When :extension:`RequiredTypeArguments` is in effect, names bound in term syntax
are not implicitly quantified. This allows us to accept the following example: ::

  readshow :: forall a -> (Read a, Show a) => String -> String
  readshow t s = show (read s :: t)

  s1 = readshow Int    "42"      -- "42"
  s2 = readshow Double "42"      -- "42.0"

Note how ``t`` is bound on the LHS of a function equation (term syntax), and
then used in a type annotation (type syntax). Under the usual rules for implicit
quantification, the ``t`` would have been implicitly quantified: ::

  -- RequiredTypeArguments
  readshow t s = show (read s :: t)   -- the `t` is captured
  --       ↑                     ↑
  --      bound                 used

  -- NoRequiredTypeArguments
  readshow t s = show (read s :: t)   -- the `t` is implicitly quantified as follows:
  readshow t s = show (read s :: forall t. t)
  --       ↑                            ↑  ↑
  --      bound                      bound used

On the one hand, taking the current scope into account allows us to accept
programs like the one above. On the other hand, some existing programs will no
longer compile: ::

  a = 42
  f :: a -> a    -- RequiredTypeArguments: the top-level `a` is captured

Because of that, merely enabling :extension:`RequiredTypeArguments` might lead
to type errors of this form::

  Term variable ‘a’ cannot be used here
    (term variables cannot be promoted)

There are two possible ways to fix this error::

  a = 42
  f1 :: b -> b              -- (1) use a different variable name
  f2 :: forall a. a -> a    -- (2) use an explicit forall

If you are converting a large codebase to be compatible with
:extension:`RequiredTypeArguments`, consider using
:ghc-flag:`-Wterm-variable-capture` during the migration. It is a warning that
detects instances of implicit quantification incompatible with
:extension:`RequiredTypeArguments`: ::

  The type variable ‘a’ is implicitly quantified,
  even though another variable of the same name is in scope:
    ‘a’ defined at ...

.. _pi-types:

Relation to Π-types
~~~~~~~~~~~~~~~~~~~

Both ``forall a.`` and ``forall a ->`` are dependent quantifiers in the narrow
sense defined in :ref:`dependent-quantifier`. However, neither of them
constitutes a dependent function type (Π-type) that might be familiar to users
coming from dependently-typed languages or proof assistants.

* Haskell has always had functions whose result *value* depends on
  the argument *value*::

    not True  = False   -- argument value: True;  result value: False
    (*2) 5    = 10      -- argument value: 5;     result value: 10

  This captures the usual idea of a function, denoted ``a -> b``.

* Haskell also has functions whose result *type* depends on the argument *type*:
  ::

    id    @Int  :: Int  -> Int    -- argument type: Int;  result type: Int  -> Int
    id_vdq Bool :: Bool -> Bool   -- argument type: Bool; result type: Bool -> Bool

  This captures the idea of parametric polymorphism, denoted ``forall a. b`` or
  ``forall a -> b``.

* Furthermore, Haskell has functions whose result *value* depends on the
  argument *type*::

    maxBound @Int8   = 127    -- argument type: Int8;  result value: 127
    maxBound @Int16  = 32767  -- argument type: Int16; result value: 32767

  This captures the idea of ad-hoc (class-based) polymorphism,
  denoted ``C a => b``.

* However, Haskell does **not** have direct support for functions whose result
  *type* depends on the argument *value*. In the literature, these are often
  called "dependent functions", or "Π-types".

  Consider: ::

    type F :: Bool -> Bool
    type family F b where
      F True  = ...
      F False = ...

    f :: Bool -> Bool
    f True  = ...
    f False = ...

  In this example, we define a type family ``F`` to pattern-match on ``b`` at
  the type level; and a function ``f`` to pattern-match on ``b`` at the term
  level. However, it is impossible to quantify over ``b`` in such a way that
  both ``F`` and ``f`` could be applied to it::

    depfun :: forall (b :: Bool) -> F b  -- Allowed
    depfun b = ... (f b) ...             -- Not allowed

  It is illegal to pass ``b`` to ``f`` because ``b`` does not exist at runtime.
  Types and type arguments are erased before runtime.

The :extension:`RequiredTypeArguments` extension does not add dependent
functions, which would be a much bigger step. Rather :extension:`RequiredTypeArguments`
just makes it possible for the type arguments of a function to be compulsory.

.. _visible-forall-in-gadts:

Visible forall in GADTs
~~~~~~~~~~~~~~~~~~~~~~~

**Since:** GHC 9.14

When :extension:`RequiredTypeArguments` is in effect, the use of ``forall a ->``
in data constructor declarations is allowed: ::

  data T a where
    Typed :: forall a -> a -> T a

There are no restrictions placed on the flavour of the parent declaration:
the data constructor may be introduced as part of a ``data``, ``data instance``,
``newtype``, or ``newtype instance`` declaration.

The use of visible forall in the type of a data constructor imposes no
restrictions on where the data constructor may occur.  Examples:

* in expressions ::

      t1 = Typed Int 42
      t2 = Typed String "hello"
      t3 = Typed (Int -> Bool) even

* in patterns ::

      f1 (Typed a x) = x :: a
      f2 (Typed Int n) = n*2
      f3 (Typed ((->) w Bool) g) = not . g

* in types (with :extension:`DataKinds`) ::

      type T1 = Typed Nat 42
      type T2 = Typed Symbol "hello"
      type T3 = Typed (Type -> Constraint) Num

At the moment, all foralls in the type of a data constructor (including visible
foralls) must occur before constraints or value arguments: ::

  data D x where
    -- OK (one forall at the front of the type)
    MkD1 :: forall a b ->
            a ->
            b ->
            D (a, b)

    -- OK (multpile foralls at the front of the type)
    MkD2 :: forall a.
            forall b ->
            a ->
            b ->
            D (a, b)

    -- Rejected (forall after a value argument)
    MkD3 :: forall a ->
            a ->
            forall b ->
            b ->
            D (a, b)

This restriction is intended to be lifted in the future (:ghc-ticket:`18389`).

The use of visible forall instead of invisible forall has no effect on how data
is represented on the heap.
