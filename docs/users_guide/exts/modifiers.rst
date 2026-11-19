.. _modifiers:

Modifiers
=========

.. extension:: Modifiers
    :shortdesc: Allow experimental modifier syntax.

    :since: 10.0
    :status: Experimental

    Enable modifier syntax in various places, such as arrows (``a %m -> b``) and
    instance declarations (``%m instance C a``).

**This extension is currently considered experimental. Expect bugs,
warts, and bad error messages; everything down to the syntax is
subject to change**. We encourage you to experiment
with this extension and report issues in `the GHC
bug tracker <https://gitlab.haskell.org/ghc/ghc/issues>`__, adding the
tag ``Modifiers``.

Modifiers are a way to attach types to various places in the syntax tree.
They're intended as a general hook to allow new features without introducing new
syntax.

This fork recognises a modifier of kind ``Matchability`` on a function
arrow. With :extension:`LinearTypes`, GHC also recognises multiplicity
modifiers. GHC ignores unrecognised modifiers and warns about them with
``-Wunrecognised-modifiers``.

Import ``U`` and ``M`` from ``GHC.Exts`` to write an unmatchable or
matchable arrow. For example, ``(a %U -> b) -> [a] -> [b]`` accepts
an unmatchable callback, such as a type family. An arrow can have one
modifier of each kind. With :extension:`LinearTypes`, ``a %1 %U -> b``
and ``a %U %1 -> b`` describe the same arrow.

With ``-XModifiers`` enabled, modifiers of the form ``%t``, where ``t`` is a
type, may be placed in various positions. For example:

::

   foo :: a %m -> b
   %(); class C a
   data D = %m D Int

Use of ``%`` in a prefix position (like ``%x``, with no whitespace) is reserved
for modifiers. To use ``%`` as an operator, there must be whitespace in between
it and the following term (``% x``).

The kind of any modifier (that is, the kind of ``t`` in ``%t``) must be known.
If it's unknown or polymorphic, it produces an error.

Modifiers are currently accepted in the following positions:

- In front of arrows: ``a %m -> b``.
- In front of bindings: ``let %m a = b``.
- In front of patterns: ``case x of { %m y -> z }``.
- In front of the `::` of record field declarations:
  ``newtype D = D { d %m :: Int }``.
- In front of data constructor declarations: ``data D = %m D Int``,
  ``data D where { %m D :: Int -> D }``.
- In front of several top-level declarations (requiring a newline or semicolon):

  ::

     %m; type A = B
     %m; data A = B
     %m; newtype A = B Int
     %m; class C a where
     %m; instance C A where
     %m; default (Int)
     %m; foreign export ...
     %m; foreign import ...
     %m; foo :: A

In all other positions, modifier syntax is rejected.

Anywhere that one modifier is accepted, multiple modifiers are accepted too:

::

   foo :: %m1 %m2 Int

   %m1 %m2; %m3; data D

Interaction with LinearTypes
----------------------------
With :extension:`LinearTypes` enabled, there are small changes to the syntax of
modifiers:

* The modifier ``%1`` is handled as a special case. It's renamed and typechecked
  the same as ``%One`` (using the ``One :: GHC.Types.Multiplicity`` from
  ``base``), even if it appears somewhere that linear modifiers aren't expected.
  If you want to refer to the type ``1 :: Nat`` (with :extension:`DataKinds`),
  that can be written as either ``%(1 :: Nat)`` or ``%01``.

* The linear arrow ``a ⊸ b`` has the same meaning as ``a %1 -> b``. Other
  modifiers are accepted: ``a %X ⊸ b`` has the same meaning as ``a %X %1 -> b``.

Limitations
-----------

Modifier syntax is still considered experimental. Here is a list of known bugs
and limitations.

* In some cases, the warning caused by ``-Wunrecognised-modifiers`` is emitted
  twice.

* Modifiers of polymorphic kind (e.g. ``%Nothing``) do not produce an error.

* Modifiers in some positions aren't typechecked, which means that e.g.
  ``%[Maybe]; class A`` produces a warning while ``f :: Int %[Maybe] -> Int``
  correctly throws an error.

* The type ``Int %(m :: Multiplicity) -> Int %m -> Int`` is rejected, since the
  second ``%m`` has unknown kind. But the type
  ``Int %m -> Int %(m :: Multiplicity) -> Int`` is accepted, taking the first
  ``m`` as ``Multiplicity`` as well.

* In a quoted type, GHC uses the kind of each arrow modifier to construct
  the Template Haskell arrow. An explicitly kinded binder such as
  ``forall (m :: Matchability). Int %m -> Bool`` supplies that information.
  Give a compound modifier its own kind annotation in a raw quotation.
  Template Haskell has no general representation for modifiers in other
  positions.

* Haddock doesn't document non-multiplicity modifiers.

* It's not always clear which type variables should be exposed for use as
  modifiers (as in ``%a; data FV1 a`` or ``data FV2 a = %a FV2``). Generally,
  these situations have unspecified semantics.

* The behavior of non-multiplicity modifiers in hs-boot files is unspecified.

* With ``-XLinearTypes -XNoModifiers``, modifiers in patterns are incorrectly
  warned-about instead of rejected. Additionally, consider these lines:

  ::

     let %1   x:xs    = ... -- (1a)
     let %1  (x:xs)   = ... -- (1b)
     let %1 !(x:xs)   = ... -- (1c)
     let (%1 x):xs    = ... -- (1d)
     let %1   Just x  = ... -- (2a)
     let %1  (Just x) = ... -- (2b)
     let %1 !(Just x) = ... -- (2c)

  In 9.14, (1a) and (2a) parsed as (1b) and (2b) respectively. From 10.0, (1a)
  parses as (1d), and (2a) fails to parse.

  Note that linear bindings must be strict. (1c) and (2c) parse in 10.0 the same
  as in 9.14. But with ``-XStrict`` enabled, (1a) and (2a) would previously have
  been accepted, and are now rejected, even with
  ``-XLinearTypes -XNoModifiers``.
