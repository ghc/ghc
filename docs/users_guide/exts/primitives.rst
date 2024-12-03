.. _primitives:

Unboxed types and primitive operations
======================================

GHC is built on a raft of primitive data types and operations;
"primitive" in the sense that they cannot be defined in Haskell itself.
While you really can use this stuff to write fast code, we generally
find it a lot less painful, and more satisfying in the long run, to use
higher-level language features and libraries. With any luck, the code
you write will be optimised to the efficient unboxed version in any
case. And if it isn't, we'd like to know about it.

All these primitive data types and operations are exported by the
module :base-ref:`GHC.Exts.`.

If you want to mention any of the primitive data types or operations in
your program, you must first import ``GHC.Exts`` to bring them into
scope. Many of them have names ending in ``#``, and to mention such names
you need the :extension:`MagicHash` extension.

To enable defining literals for other primitive data types, see the
:extension:`ExtendedLiterals` extension.

The primops make extensive use of `unboxed types <#glasgow-unboxed>`__
and `unboxed tuples <#unboxed-tuples>`__, which we briefly summarise
here.

.. _glasgow-unboxed:

Unboxed types
-------------

Most types in GHC are boxed, which means that values of that type are
represented by a pointer to a heap object. The representation of a
Haskell ``Int``, for example, is a two-word heap object. An unboxed
type, however, is represented by the value itself, no pointers or heap
allocation are involved.

Unboxed types correspond to the “raw machine” types you would use in C:
``Int#`` (long int), ``Double#`` (double), ``Addr#`` (void \*), etc. The
*primitive operations* (PrimOps) on these types are what you might
expect; e.g., ``(+#)`` is addition on ``Int#``\ s, and is the
machine-addition that we all know and love—usually one instruction.

Primitive (unboxed) types cannot be defined in Haskell, and are
therefore built into the language and compiler. Primitive types are
always unlifted; that is, a value of a primitive type cannot be bottom.
(Note: a "boxed" type means that a value is represented by a pointer to a heap
object; a "lifted" type means that terms of that type may be bottom. See
the next paragraph for an example.)
We use the convention (but it is only a convention) that primitive
types, values, and operations have a ``#`` suffix (see
:ref:`magic-hash`). For some primitive types we have special syntax for
literals, also described in the `same section <#magic-hash>`__.

Primitive values are often represented by a simple bit-pattern, such as
``Int#``, ``Float#``, ``Double#``. But this is not necessarily the case:
a primitive value might be represented by a pointer to a heap-allocated
object. Examples include ``Array#``, the type of primitive arrays. Thus,
``Array#`` is an unlifted, boxed type. A
primitive array is heap-allocated because it is too big a value to fit
in a register, and would be too expensive to copy around; in a sense, it
is accidental that it is represented by a pointer. If a pointer
represents a primitive value, then it really does point to that value:
no unevaluated thunks, no indirections. Nothing can be at the other end
of the pointer than the primitive value. A numerically-intensive program
using unboxed types can go a *lot* faster than its “standard”
counterpart—we saw a threefold speedup on one example.

Unboxed type kinds
------------------

Because unboxed types are represented without the use of pointers, we
cannot store them in a polymorphic data type.
For example, the ``Just`` node
of ``Just 42#`` would have to be different from the ``Just`` node of
``Just 42``; the former stores an integer directly, while the latter
stores a pointer. GHC currently does not support this variety of ``Just``
nodes (nor for any other data type). Accordingly, the *kind* of an unboxed
type is different from the kind of a boxed type.

The Haskell Report describes that ``*`` (spelled ``Type`` and imported from
``Data.Kind`` in the GHC dialect of Haskell) is the kind of ordinary data types,
such as ``Int``. Furthermore, type constructors can have kinds with arrows; for
example, ``Maybe`` has kind ``Type -> Type``. Unboxed types have a kind that
specifies their runtime representation. For example, the type ``Int#`` has kind
``TYPE IntRep`` and ``Double#`` has kind ``TYPE DoubleRep``. These kinds say
that the runtime representation of an ``Int#`` is a machine integer, and the
runtime representation of a ``Double#`` is a machine double-precision floating
point. In contrast, the kind ``Type`` is actually just a synonym for ``TYPE
LiftedRep``. More details of the ``TYPE`` mechanisms appear in the `section
on runtime representation polymorphism <#runtime-rep>`__.

Given that ``Int#``'s kind is not ``Type``, then it follows that ``Maybe
Int#`` is disallowed. Similarly, because type variables tend to be of kind
``Type`` (for example, in ``(.) :: (b -> c) -> (a -> b) -> a -> c``, all the
type variables have kind ``Type``), polymorphism tends not to work over
primitive types. Stepping back, this makes some sense, because a polymorphic
function needs to manipulate the pointers to its data, and most primitive types
are unboxed.

There are some restrictions on the use of primitive types:

-  You cannot define a newtype whose representation type (the argument
   type of the data constructor) is an unboxed type. Thus, this is
   illegal:

   ::

         newtype A = MkA Int#

   However, this restriction can be relaxed by enabling
   :extension:`UnliftedNewtypes`.  The `section on unlifted newtypes
   <#unlifted-newtypes>`__ details the behavior of such types.

-  You cannot bind a variable with an unboxed type in a *top-level*
   binding.

-  You cannot bind a variable with an unboxed type in a *recursive*
   binding.

-  You may bind unboxed variables in a (non-recursive, non-top-level)
   pattern binding, but you must make any such pattern-match strict.
   (Failing to do so emits a warning :ghc-flag:`-Wunbanged-strict-patterns`.)
   For example, rather than:

   ::

         data Foo = Foo Int Int#

         f x = let (Foo a b, w) = ..rhs.. in ..body..

   you must write:

   ::

         data Foo = Foo Int Int#

         f x = let !(Foo a b, w) = ..rhs.. in ..body..

   since ``b`` has type ``Int#``.  See :ref:`recursive-and-polymorphic-let-bindings`.

.. _unboxed-tuples:

Unboxed tuples
--------------

.. extension:: UnboxedTuples
    :shortdesc: Allow the use of unboxed tuple syntax.

    :implies: :extension:`UnboxedSums`

    :since: 6.8.1

Unboxed tuples aren't really exported by ``GHC.Exts``; they are a
syntactic extension (:extension:`UnboxedTuples`). An
unboxed tuple looks like this: ::

    (# e_1, ..., e_n #)

where ``e_1..e_n`` are expressions of any type (primitive or
non-primitive). The type of an unboxed tuple looks the same.

Note that when unboxed tuples are enabled, ``(#`` is a single lexeme, so
for example when using operators like ``#`` and ``#-`` you need to write
``( # )`` and ``( #- )`` rather than ``(#)`` and ``(#-)``.

Unboxed tuples are used for functions that need to return multiple
values, but they avoid the heap allocation normally associated with
using fully-fledged tuples. When an unboxed tuple is returned, the
components are put directly into registers or on the stack; the unboxed
tuple itself does not have a composite representation. Many of the
primitive operations listed in ``primops.txt.pp`` return unboxed tuples.
In particular, the ``IO`` and ``ST`` monads use unboxed tuples to avoid
unnecessary allocation during sequences of operations.

The typical use of unboxed tuples is simply to return multiple
values, binding those multiple results with a ``case`` expression,
thus:

::

      f x y = (# x+1, y-1 #)
      g x = case f x x of { (# a, b #) -> a + b }

You can have an unboxed tuple in a pattern binding, thus

::

      f x = let (# p,q #) = h x in ..body..

If the types of ``p`` and ``q`` are not unboxed, the resulting
binding is lazy like any other Haskell pattern binding. The above
example desugars like this:

::

      f x = let t = case h x of { (# p,q #) -> (p,q) }
                p = fst t
                q = snd t
            in ..body..

Indeed, the bindings can even be recursive.  See :ref:`recursive-and-polymorphic-let-bindings`
for a more precise account.

To refer to the unboxed tuple type constructors themselves, e.g. if you
want to attach instances to them, use ``(# #)``, ``(#,#)``, ``(#,,#)``, etc.
This mirrors the syntax for boxed tuples ``()``, ``(,)``, ``(,,)``, etc.

.. _unboxed-sums:

Unboxed sums
------------

.. extension:: UnboxedSums
    :shortdesc: Allow the use of unboxed sum syntax.

    :implied by: :extension:`UnboxedTuples`
    :since: 8.2.1

    Enable the use of unboxed sum syntax.
    Implied by :extension:`UnboxedTuples`.

`-XUnboxedSums` enables new syntax for anonymous, unboxed sum types. The syntax
for an unboxed sum type with N alternatives is ::

    (# t_1 | t_2 | ... | t_N #)

where ``t_1`` ... ``t_N`` are types (which can be unlifted, including unboxed
tuples and sums).

Unboxed tuples can be used for multi-arity alternatives. For example: ::

    (# (# Int, String #) | Bool #)

The term level syntax is similar. Leading and preceding bars (`|`) indicate which
alternative it is. Here are two terms of the type shown above: ::

    (# (# 1, "foo" #) | #) -- first alternative

    (# | True #) -- second alternative

The pattern syntax reflects the term syntax: ::

    case x of
      (# (# i, str #) | #) -> ...
      (# | bool #) -> ...

Note that spaces are always required around bars. For example, ``(# | 1# | | #)``
is valid, but ``(# | 1# || #)`` and ``(#| 1# | | #)`` are both invalid.

The type constructors themselves can be written in prefix form as ``(# | #)``,
``(# | | #)``, ``(# | | | #)``, etc. Partial applications must also use prefix form,
i.e. ``(# | #) Int#``. Saturated applications can be written either way,
so that ``(# | #) Int# Float#`` is equivalent to ``(# Int# | Float# #)``.

Unboxed sums are "unboxed" in the sense that, instead of allocating sums in the
heap and representing values as pointers, unboxed sums are represented as their
components, just like unboxed tuples. These "components" depend on alternatives
of a sum type. Like unboxed tuples, unboxed sums are lazy in their lifted
components.

The code generator tries to generate as compact layout as possible for each
unboxed sum. In the best case, size of an unboxed sum is size of its biggest
alternative plus one word (for a tag). The algorithm for generating the memory
layout for a sum type works like this:

- All types are classified as one of these classes: 32bit word, 64bit word,
  32bit float, 64bit float, pointer.

- For each alternative of the sum type, a layout that consists of these fields
  is generated. For example, if an alternative has ``Int``, ``Float#`` and
  ``String`` fields, the layout will have an 32bit word, 32bit float and
  pointer fields.

- Layout fields are then overlapped so that the final layout will be as compact
  as possible. For example, suppose we have the unboxed sum: ::

    (# (# Word32#, String, Float# #)
    |  (# Float#, Float#, Maybe Int #) #)

  The final layout will be something like ::

    Int32, Float32, Float32, Word32, Pointer

  The first ``Int32`` is for the tag. There are two ``Float32`` fields because
  floating point types can't overlap with other types, because of limitations of
  the code generator that we're hoping to overcome in the future. The second
  alternative needs two ``Float32`` fields: The ``Word32`` field is for the
  ``Word32#`` in the first alternative. The ``Pointer`` field is shared between
  ``String`` and ``Maybe Int`` values of the alternatives.

  As another example, this is the layout for the unboxed version of ``Maybe a``
  type, ``(# (# #) | a #)``: ::

    Int32, Pointer

  The ``Pointer`` field is not used when tag says that it's ``Nothing``.
  Otherwise ``Pointer`` points to the value in ``Just``. As mentioned
  above, this type is lazy in its lifted field. Therefore, the type ::

    data Maybe' a = Maybe' (# (# #) | a #)

  is *precisely* isomorphic to the type ``Maybe a``, although its memory
  representation is different.

  In the degenerate case where all the alternatives have zero width, such
  as the ``Bool``-like ``(# (# #) | (# #) #)``, the unboxed sum layout only
  has an ``Int32`` tag field (i.e., the whole thing is represented by an integer).

.. _unlifted-newtypes:

Unlifted Newtypes
-----------------

.. extension:: UnliftedNewtypes
    :shortdesc: Allow definition of unlifted newtypes.

    :since: 8.10.1

    Enable the use of newtypes over types with non-lifted runtime representations.

GHC implements an :extension:`UnliftedNewtypes` extension as specified in
`the GHC Proposal #98 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0098-unlifted-newtypes.rst>`_.
:extension:`UnliftedNewtypes` relaxes the restrictions around what types can appear inside
of a ``newtype``. For example, the type ::

    newtype A = MkA Int#

is accepted when this extension is enabled. This creates a type
``A :: TYPE IntRep`` and a data constructor ``MkA :: Int# -> A``.
Although the kind of ``A`` is inferred by GHC, there is nothing visually
distinctive about this type that indicated that is it not of kind ``Type``
like newtypes typically are. `GADTSyntax <#gadt-style>`__ can be used to
provide a kind signature for additional clarity ::

    newtype A :: TYPE IntRep where
      MkA :: Int# -> A

The ``Coercible`` machinery works with unlifted newtypes just like it does with
lifted types. In either of the equivalent formulations of ``A`` given above,
users would additionally have access to a coercion between ``A`` and ``Int#``.

As a consequence of the
`representation-polymorphic binder restriction <#representation-polymorphism-restrictions>`__,
representation-polymorphic fields are disallowed in data constructors
of data types declared using ``data``. However, since ``newtype`` data
constructor application is implemented as a coercion instead of as function
application, this restriction does not apply to the field inside a ``newtype``
data constructor. Thus, the type checker accepts ::

    newtype Identity# :: forall (r :: RuntimeRep). TYPE r -> TYPE r where
      MkIdentity# :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Identity# a

And with `UnboxedSums <#unboxed-sums>`__ enabled ::

    newtype Maybe# :: forall (r :: RuntimeRep). TYPE r -> TYPE (SumRep '[r, TupleRep '[]]) where
      MkMaybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# a | (# #) #) -> Maybe# a

This extension also relaxes some of the restrictions around data family
instances. In particular, :extension:`UnliftedNewtypes` permits a
``newtype instance`` to be given a return kind of ``TYPE r``, not just
``Type``. For example, the following ``newtype instance`` declarations would be
permitted: ::

     class Foo a where
       data FooKey a :: TYPE IntRep
     class Bar (r :: RuntimeRep) where
       data BarType r :: TYPE r

     instance Foo Bool where
       newtype FooKey Bool = FooKeyBoolC Int#
     instance Bar WordRep where
       newtype BarType WordRep = BarTypeWordRepC Word#

It is worth noting that :extension:`UnliftedNewtypes` is *not* required to give
the data families themselves return kinds involving ``TYPE``, such as the
``FooKey`` and ``BarType`` examples above. The extension is
only required for ``newtype instance`` declarations, such as ``FooKeyBoolC``
and ``BarTypeWorkRepC`` above.

This extension impacts the determination of whether or not a newtype has
a Complete User-Supplied Kind (CUSK). The exact impact is specified
`the section on CUSKs <#complete-kind-signatures>`__.

Unlifted Datatypes
------------------

.. extension:: UnliftedDatatypes
    :shortdesc: Allow the definition of unlifted data types.

    :implies: :extension:`DataKinds`, :extension:`StandaloneKindSignatures`
    :since: 9.2.1

    Enable the declaration of data types with unlifted or levity-polymorphic
    result kind.

GHC implements the :extension:`UnliftedDatatypes` extension as specified in
`the GHC Proposal #265 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0265-unlifted-datatypes.rst>`_.
:extension:`UnliftedDatatypes` relaxes the restrictions around what result kinds
are allowed in data declarations. For example, the type ::

  data UList a :: UnliftedType where
    UCons :: a -> UList a -> UList a
    UNil :: UList a

defines a list type that lives in kind ``UnliftedType``
(e.g., ``TYPE (BoxedRep Unlifted)``). As such, each occurrence of a term of that
type is assumed to be evaluated (and the compiler makes sure that is indeed the
case). In other words: Unlifted data types behave like data types in strict
languages such as OCaml or Idris. However unlike :extension:`StrictData`,
this extension will not change whether the fields of a (perhaps unlifted)
data type are strict or lazy. For example, ``UCons`` is lazy in its first
argument as its field has kind ``Type``.

The fact that unlifted types are always evaluated allows GHC to elide
evaluatedness checks at runtime. See the Motivation section of the proposal
for how this can improve performance for some programs.

The above data declaration in GADT syntax correctly suggests that unlifted
data types are compatible with the full GADT feature set. Somewhat conversely,
you can also declare unlifted data types in Haskell98 syntax, which requires you
to specify the result kind via :extension:`StandaloneKindSignatures`: ::

  type UList :: Type -> UnliftedType
  data UList a = UCons a (UList a) | UNil

You may even declare levity-polymorphic data types: ::

  type PEither :: Type -> Type -> TYPE (BoxedRep l)
  data PEither l r = PLeft l | PRight r

  f :: PEither @Unlifted Int Bool -> Bool
  f (PRight b) = b
  f _          = False

While ``f`` above could reasonably be levity-polymorphic (as it evaluates its
argument either way), GHC currently disallows the more general type
``PEither @l Int Bool -> Bool``. This is a consequence of the
`representation-polymorphic binder restriction <#representation-polymorphism-restrictions>`__.

Pattern matching against an unlifted data type work just like that for lifted
types; but see :ref:`recursive-and-polymorphic-let-bindings` for the semantics of
pattern bindings involving unlifted data types.

Due to :ghc-ticket:`19487`, it is
not currently possible to declare levity-polymorphic data types with nullary
data constructors. There is a workaround, though: ::

  type T :: TYPE (BoxedRep l)
  data T where
    MkT :: forall l. (() :: Constraint) => T @l

The use of ``=>`` makes the type of ``MkT`` lifted.
If you want a zero-runtime-cost alternative, use ``MkT :: Proxy# () -> T @l``
instead and bear with the additional ``proxy#`` argument at construction sites.

This extension also relaxes some of the restrictions around data family
instances. In particular, :extension:`UnliftedDatatypes` permits a
``data instance`` to be given a return kind that unifies with
``TYPE (BoxedRep l)``, not just ``Type``. For example, the following ``data
instance`` declarations would be permitted: ::

  data family F a :: UnliftedType
  data instance F Int = FInt

  data family G a :: TYPE (BoxedRep l)
  data instance G Int = GInt Int -- defaults to Type
  data instance G Bool :: UnliftedType where
    GBool :: Bool -> G Bool
  data instance G Char :: Type where
    GChar :: Char -> G Char
  data instance G Double :: forall l. TYPE (BoxedRep l) where
    GDouble :: Int -> G @l Double

It is worth noting that :extension:`UnliftedDatatypes` is *not* required to give
the data families themselves return kinds involving ``TYPE``, such as the
``G`` example above. The extension is only required for ``data instance``
declarations, such as ``FInt`` and ``GBool`` above.

.. _primitive-string-literals:

Primitive string literals
-------------------------

Primitive string literals, also called unboxed string literals or ``Addr#`` literals,
are string literals with a postfix hash sign ``#``.
They are enabled by the :extension:`MagicHash` extension and have type ``Addr#``.

A primitive string literal represents a sequence of bytes.
You can embed a NUL byte like ``"foo\0bar"#``, which corresponds to the bytes
``[102,111,111,0,98,97,114] :: [Word8]`` (with an implicit NUL at the end).
Further, you can only embed a character whose code is less than 256; ``"\xFF"#`` is valid
and represents ``[255] :: [Word8]``, but ``"\x3B1"#`` is invalid.
In other words, it is encoded in Latin-1.

Like C strings, primitive string literals are implicitly terminated by a NUL byte.

Primitive string literals are immutable and attempting to write to their address results in undefined behavior.

.. _desugaring-of-string-literals:

Desugaring of string literals
-----------------------------

An ASCII string literal without a NUL byte is desugared into a call to
``GHC.CString.unpackCString#`` with the content as a primitive string literal.
For example, ``"some string"`` is translated into ``GHC.CString.unpackCString# "some string"#``.

If the string contains NUL bytes or non-ASCII characters (characters with code points >= 128),
the string is encoded in Modified UTF-8 and then passed to ``GHC.CString.unpackCStringUtf8#``.
Modified UTF-8 differs from standard UTF-8 in two ways:

- The NUL byte is encoded as ``"\xC0\x80"#`` (i.e. an over-long encoding of U+0000).
- Surrogate code points are allowed and encoded like other code points.

Examples:

- ``"Hello\0world!"`` is desugared to ``GHC.CString.unpackCStringUtf8# "Hello\xC0\x80world!"#``.
- ``"café"`` is desugared to ``GHC.CString.unpackCStringUtf8# "caf\xC3\xA9"#``.
- ``"\x732B\x1F431"`` is desugared to ``GHC.CString.unpackCStringUtf8# "\xE7\x8C\xAB\xF0\x9F\x90\xB1"#``.
- ``"\xDFFF\xD800"`` is desugared to ``GHC.CString.unpackCStringUtf8# "\xED\xBF\xBF\xED\xA0\x80"#``.
