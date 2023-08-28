Required type arguments
=======================

.. extension:: RequiredTypeArguments
    :shortdesc: Enable required type arguments in terms.

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

Note that the arrow in ``forall a ->`` is part of the syntax and not a function
arrow, just like the dot in ``forall a.`` is not a type operator. The essence of
a ``forall`` is the same regardless of whether it is followed by a dot or an
arrow: it introduces a type variable. But the way we bind and specify this type
variable at the term level differs.

When we define ``id``, we can use a lambda to bind a variable that stands for
the function argument::

  -- For reference: id :: forall a. a -> a
  id = \x -> x

At the same time, there is no mention of ``a`` in this definition at all. It is
bound by the compiler behind the scenes, and that is why we call the ordinary
``forall a.`` an *invisible* quantifier. Compare that to ``forall a ->``, which
is considered *visible*::

  -- For reference: id_vdq :: forall a -> a -> a
  id_vdq = \(type t) x -> x

This time we have two binders in the lambda:
* ``type t``, corresponding to ``forall a ->`` in the signature
* ``x``, corresponding to ``a ->`` in the signature

And of course, now we also have the option of using the bound ``t`` in a
subsequent pattern, as well as on the right-hand side of the lambda::

  -- For reference: id_vdq :: forall a -> a -> a
  id_vdq = \(type t) (x :: t) -> x :: t
  --              ↑        ↑          ↑
  --            bound     used       used

At use sites, we also instantiate this type variable explicitly::

  n = id_vdq (type Integer) 42
  s = id_vdq (type String)  "Hello"

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

  x1 = id_vdq (type Bool) True    -- OK
  x2 = id_vdq             True    -- not OK

You may use an underscore to infer a required type argument::

  x3 = id_vdq (type _) True       -- OK

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

If ``sizeOf`` had this type, we could write ``sizeOf (type Bool)`` without
passing a dummy value.

Relation to :extension:`ExplicitNamespaces`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``type`` keyword that we used in the examples is not actually part of
:extension:`RequiredTypeArguments`. It is guarded behind
:extension:`ExplicitNamespaces`. As described in the proposal, required type
arguments can be passed without a syntactic marker, making them syntactically
indistinguishble from ordinary function arguments::

  n = id_vdq Integer 42

In this example we pass ``Integer`` as opposed to ``(type Integer)``.
This means that :extension:`RequiredTypeArguments` is not tied to the ``type``
syntax, which belongs to :extension:`ExplicitNamespaces`.