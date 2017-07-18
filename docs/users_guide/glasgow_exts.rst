.. index::
   single: language, GHC extensions

As with all known Haskell systems, GHC implements some extensions to the
standard Haskell language. They can all be enabled or disabled by command line
flags or language pragmas. By default GHC understands the most recent Haskell
version it supports, plus a handful of extensions.

Some of the Glasgow extensions serve to give you access to the
underlying facilities with which we implement Haskell. Thus, you can get
at the Raw Iron, if you are willing to write some non-portable code at a
more primitive level. You need not be “stuck” on performance because of
the implementation costs of Haskell's "high-level" features—you can
always code "under" them. In an extreme case, you can write all your
time-critical code in C, and then just glue it together with Haskell!

Before you get too carried away working at the lowest level (e.g.,
sloshing ``MutableByteArray#``\ s around your program), you may wish to
check if there are libraries that provide a "Haskellised veneer" over
the features you want. The separate
`libraries documentation <../libraries/index.html>`__ describes all the
libraries that come with GHC.

.. _options-language:

Language options
================

.. index::
   single: language; option
   single: options; language
   single: extensions; options controlling

The language option flags control what variation of the language are
permitted.

Language options can be controlled in two ways:

-  Every language option can switched on by a command-line flag
   "``-X...``" (e.g. ``-XTemplateHaskell``), and switched off by the
   flag "``-XNo...``"; (e.g. ``-XNoTemplateHaskell``).

-  Language options recognised by Cabal can also be enabled using the
   ``LANGUAGE`` pragma, thus ``{-# LANGUAGE TemplateHaskell #-}`` (see
   :ref:`language-pragma`).


Although not recommended, the deprecated :ghc-flag:`-fglasgow-exts` flag enables
a large swath of the extensions supported by GHC at once.

.. ghc-flag:: -fglasgow-exts

    The flag ``-fglasgow-exts`` is equivalent to enabling the following extensions:

    .. include:: what_glasgow_exts_does.gen.rst

    Enabling these options is the *only* effect of ``-fglasgow-exts``. We are trying
    to move away from this portmanteau flag, and towards enabling features
    individually.

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
library ``GHC.Prim``, for which there is
:ghc-prim-ref:`detailed online documentation <GHC.Prim.>`. (This
documentation is generated from the file ``compiler/prelude/primops.txt.pp``.)

If you want to mention any of the primitive data types or operations in
your program, you must first import ``GHC.Prim`` to bring them into
scope. Many of them have names ending in ``#``, and to mention such names
you need the :ghc-flag:`-XMagicHash` extension (:ref:`magic-hash`).

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
cannot store them in use a polymorphic datatype at an unboxed type.
For example, the ``Just`` node
of ``Just 42#`` would have to be different from the ``Just`` node of
``Just 42``; the former stores an integer directly, while the latter
stores a pointer. GHC currently does not support this variety of ``Just``
nodes (nor for any other datatype). Accordingly, the *kind* of an unboxed
type is different from the kind of a boxed type.

The Haskell Report describes that ``*`` is the kind of ordinary datatypes,
such as ``Int``. Furthermore, type constructors can have kinds with arrows;
for example, ``Maybe`` has kind ``* -> *``. Unboxed types have a kind that
specifies their runtime representation. For example, the type ``Int#`` has
kind ``TYPE 'IntRep`` and ``Double#`` has kind ``TYPE 'DoubleRep``. These
kinds say that the runtime representation of an ``Int#`` is a machine integer,
and the runtime representation of a ``Double#`` is a machine double-precision
floating point. In contrast, the kind ``*`` is actually just a synonym
for ``TYPE 'PtrRepLifted``. More details of the ``TYPE`` mechanisms appear in
the `section on runtime representation polymorphism <#runtime-rep>`__.

Given that ``Int#``'s kind is not ``*``, it then it follows that
``Maybe Int#`` is disallowed. Similarly, because type variables tend
to be of kind ``*`` (for example, in ``(.) :: (b -> c) -> (a -> b) -> a -> c``,
all the type variables have kind ``*``), polymorphism tends not to work
over primitive types. Stepping back, this makes some sense, because
a polymorphic function needs to manipulate the pointers to its data,
and most primitive types are unboxed.

There are some restrictions on the use of primitive types:

-  You cannot define a newtype whose representation type (the argument
   type of the data constructor) is an unboxed type. Thus, this is
   illegal:

   ::

         newtype A = MkA Int#

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

   since ``b`` has type ``Int#``.

.. _unboxed-tuples:

Unboxed tuples
--------------

.. ghc-flag:: -XUnboxedTuples

    Enable the use of unboxed tuple syntax.

Unboxed tuples aren't really exported by ``GHC.Exts``; they are a
syntactic extension enabled by the language flag :ghc-flag:`-XUnboxedTuples`. An
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

There are some restrictions on the use of unboxed tuples:

-  The typical use of unboxed tuples is simply to return multiple
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

   Indeed, the bindings can even be recursive.

.. _unboxed-sums:

Unboxed sums
------------

.. ghc-flag:: -XUnboxedSums

    Enable the use of unboxed sum syntax.

`-XUnboxedSums` enables new syntax for anonymous, unboxed sum types. The syntax
for an unboxed sum type with N alternatives is ::

    (# t_1 | t_2 | ... | t_N #)

where `t_1` ... `t_N` are types (which can be unlifted, including unboxed tuple
and sums).

Unboxed tuples can be used for multi-arity alternatives. For example: ::

    (# (# Int, String #) | Bool #)

Term level syntax is similar. Leading and preceding bars (`|`) indicate which
alternative it is. Here is two terms of the type shown above: ::

    (# (# 1, "foo" #) | #) -- first alternative

    (# | True #) -- second alternative

Pattern syntax reflects the term syntax: ::

    case x of
      (# (# i, str #) | #) -> ...
      (# | bool #) -> ...

Unboxed sums are "unboxed" in the sense that, instead of allocating sums in the
heap and representing values as pointers, unboxed sums are represented as their
components, just like unboxed tuples. These "components" depend on alternatives
of a sum type. Code generator tries to generate as compact layout as possible.
In the best case, size of an unboxed sum is size of its biggest alternative +
one word (for tag). The algorithm for generating memory layout for a sum type
works like this:

- All types are classified as one of these classes: 32bit word, 64bit word,
  32bit float, 64bit float, pointer.

- For each alternative of the sum type, a layout that consists of these fields
  is generated. For example, if an alternative has `Int`, `Float#` and `String`
  fields, the layout will have an 32bit word, 32bit float and pointer fields.

- Layout fields are then overlapped so that the final layout will be as compact
  as possible. E.g. say two alternatives have these fields: ::

    Word32, String, Float#
    Float#, Float#, Maybe Int

  Final layout will be something like ::

    Int32, Float32, Float32, Word32, Pointer

  First `Int32` is for the tag. It has two `Float32` fields because floating
  point types can't overlap with other types, because of limitations of the code
  generator that we're hoping to overcome in the future, and second alternative
  needs two `Float32` fields. `Word32` field is for the `Word32` in the first
  alternative. `Pointer` field is shared between `String` and `Maybe Int` values
  of the alternatives.

  In the case of enumeration types (like `Bool`), the unboxed sum layout only
  has an `Int32` field (i.e. the whole thing is represented by an integer).

In the example above, a value of this type is thus represented as 5 values. As
an another example, this is the layout for unboxed version of `Maybe a` type: ::

    Int32, Pointer

The `Pointer` field is not used when tag says that it's `Nothing`. Otherwise
`Pointer` points to the value in `Just`.

.. _syntax-extns:

Syntactic extensions
====================

.. _unicode-syntax:

Unicode syntax
--------------

.. ghc-flag:: -XUnicodeSyntax

    Enable the use of Unicode characters in place of their equivalent ASCII
    sequences.

The language extension :ghc-flag:`-XUnicodeSyntax` enables
Unicode characters to be used to stand for certain ASCII character
sequences. The following alternatives are provided:

+--------------+---------------+-------------+-----------------------------------------+
| ASCII        | Unicode       | Code point  | Name                                    |
|              | alternative   |             |                                         |
+==============+===============+=============+=========================================+
| ``::``       | ∷             | 0x2237      | PROPORTION                              |
+--------------+---------------+-------------+-----------------------------------------+
| ``=>``       | ⇒             | 0x21D2      | RIGHTWARDS DOUBLE ARROW                 |
+--------------+---------------+-------------+-----------------------------------------+
| ``->``       | →             | 0x2192      | RIGHTWARDS ARROW                        |
+--------------+---------------+-------------+-----------------------------------------+
| ``<-``       | ←             | 0x2190      | LEFTWARDS ARROW                         |
+--------------+---------------+-------------+-----------------------------------------+
| ``>-``       | ⤚             | 0x291a      | RIGHTWARDS ARROW-TAIL                   |
+--------------+---------------+-------------+-----------------------------------------+
| ``-<``       | ⤙             | 0x2919      | LEFTWARDS ARROW-TAIL                    |
+--------------+---------------+-------------+-----------------------------------------+
| ``>>-``      | ⤜             | 0x291C      | RIGHTWARDS DOUBLE ARROW-TAIL            |
+--------------+---------------+-------------+-----------------------------------------+
| ``-<<``      | ⤛             | 0x291B      | LEFTWARDS DOUBLE ARROW-TAIL             |
+--------------+---------------+-------------+-----------------------------------------+
| ``*``        | ★             | 0x2605      | BLACK STAR                              |
+--------------+---------------+-------------+-----------------------------------------+
| ``forall``   | ∀             | 0x2200      | FOR ALL                                 |
+--------------+---------------+-------------+-----------------------------------------+
| ``(|``       | ⦇             | 0x2987      | Z NOTATION LEFT IMAGE BRACKET           |
+--------------+---------------+-------------+-----------------------------------------+
| ``|)``       | ⦈             | 0x2988      | Z NOTATION RIGHT IMAGE BRACKET          |
+--------------+---------------+-------------+-----------------------------------------+
| ``[|``       | ⟦             | 0x27E6      | MATHEMATICAL LEFT WHITE SQUARE BRACKET  |
+--------------+---------------+-------------+-----------------------------------------+
| ``|]``       | ⟧             | 0x27E7      | MATHEMATICAL RIGHT WHITE SQUARE BRACKET |
+--------------+---------------+-------------+-----------------------------------------+

.. _magic-hash:

The magic hash
--------------

.. ghc-flag:: -XMagicHash

    Enable the use of the hash character (``#``) as an identifier suffix.

The language extension :ghc-flag:`-XMagicHash` allows ``#`` as a postfix modifier
to identifiers. Thus, ``x#`` is a valid variable, and ``T#`` is a valid type
constructor or data constructor.

The hash sign does not change semantics at all. We tend to use variable
names ending in "#" for unboxed values or types (e.g. ``Int#``), but
there is no requirement to do so; they are just plain ordinary
variables. Nor does the :ghc-flag:`-XMagicHash` extension bring anything into
scope. For example, to bring ``Int#`` into scope you must import
``GHC.Prim`` (see :ref:`primitives`); the :ghc-flag:`-XMagicHash` extension then
allows you to *refer* to the ``Int#`` that is now in scope. Note that
with this option, the meaning of ``x#y = 0`` is changed: it defines a
function ``x#`` taking a single argument ``y``; to define the operator
``#``, put a space: ``x # y = 0``.

The :ghc-flag:`-XMagicHash` also enables some new forms of literals (see
:ref:`glasgow-unboxed`):

-  ``'x'#`` has type ``Char#``

-  ``"foo"#`` has type ``Addr#``

-  ``3#`` has type ``Int#``. In general, any Haskell integer lexeme
   followed by a ``#`` is an ``Int#`` literal, e.g. ``-0x3A#`` as well as
   ``32#``.

-  ``3##`` has type ``Word#``. In general, any non-negative Haskell
   integer lexeme followed by ``##`` is a ``Word#``.

-  ``3.2#`` has type ``Float#``.

-  ``3.2##`` has type ``Double#``

.. _negative-literals:

Negative literals
-----------------

.. ghc-flag:: -XNegativeLiterals

    :since: 7.8.1

    Enable the use of un-parenthesized negative numeric literals.

The literal ``-123`` is, according to Haskell98 and Haskell 2010,
desugared as ``negate (fromInteger 123)``. The language extension
:ghc-flag:`-XNegativeLiterals` means that it is instead desugared as
``fromInteger (-123)``.

This can make a difference when the positive and negative range of a
numeric data type don't match up. For example, in 8-bit arithmetic -128
is representable, but +128 is not. So ``negate (fromInteger 128)`` will
elicit an unexpected integer-literal-overflow message.

.. _num-decimals:

Fractional looking integer literals
-----------------------------------

.. ghc-flag:: -XNumDecimals

    :since: 7.8.1

    Allow the use of floating-point literal syntax for integral types.

Haskell 2010 and Haskell 98 define floating literals with the syntax
``1.2e6``. These literals have the type ``Fractional a => a``.

The language extension :ghc-flag:`-XNumDecimals` allows you to also use the
floating literal syntax for instances of ``Integral``, and have values
like ``(1.2e6 :: Num a => a)``

.. _binary-literals:

Binary integer literals
-----------------------

.. ghc-flag:: -XBinaryLiterals

    :since: 7.10.1

    Allow the use of binary notation in integer literals.

Haskell 2010 and Haskell 98 allows for integer literals to be given in
decimal, octal (prefixed by ``0o`` or ``0O``), or hexadecimal notation
(prefixed by ``0x`` or ``0X``).

The language extension :ghc-flag:`-XBinaryLiterals` adds support for expressing
integer literals in binary notation with the prefix ``0b`` or ``0B``. For
instance, the binary integer literal ``0b11001001`` will be desugared into
``fromInteger 201`` when :ghc-flag:`-XBinaryLiterals` is enabled.

.. _pattern-guards:

Pattern guards
--------------

.. ghc-flag:: -XNoPatternGuards

    :implied by: :ghc-flag:`-XHaskell98`
    :since: 6.8.1

Disable `pattern guards
<http://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13>`__.

.. _view-patterns:

View patterns
-------------

.. ghc-flag:: -XViewPatterns

    Allow use of view pattern syntax.

View patterns are enabled by the flag :ghc-flag:`-XViewPatterns`. More
information and examples of view patterns can be found on the
:ghc-wiki:`Wiki page <ViewPatterns>`.

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
          example Just ((f,_), f -> 4) = True

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

.. _n-k-patterns:

n+k patterns
------------

.. ghc-flag:: -XNPlusKPatterns

    :implied by: :ghc-flag:`-XHaskell98`
    :since: 6.12

    Enable use of ``n+k`` patterns.

.. _recursive-do-notation:

The recursive do-notation
-------------------------

.. ghc-flag:: -XRecursiveDo

    Allow the use of recursive ``do`` notation.

The do-notation of Haskell 98 does not allow *recursive bindings*, that
is, the variables bound in a do-expression are visible only in the
textually following code block. Compare this to a let-expression, where
bound variables are visible in the entire binding group.

It turns out that such recursive bindings do indeed make sense for a
variety of monads, but not all. In particular, recursion in this sense
requires a fixed-point operator for the underlying monad, captured by
the ``mfix`` method of the ``MonadFix`` class, defined in
``Control.Monad.Fix`` as follows: ::

    class Monad m => MonadFix m where
       mfix :: (a -> m a) -> m a

Haskell's ``Maybe``, ``[]`` (list), ``ST`` (both strict and lazy
versions), ``IO``, and many other monads have ``MonadFix`` instances. On
the negative side, the continuation monad, with the signature
``(a -> r) -> r``, does not.

For monads that do belong to the ``MonadFix`` class, GHC provides an
extended version of the do-notation that allows recursive bindings. The
:ghc-flag:`-XRecursiveDo` (language pragma: ``RecursiveDo``) provides the
necessary syntactic support, introducing the keywords ``mdo`` and
``rec`` for higher and lower levels of the notation respectively. Unlike
bindings in a ``do`` expression, those introduced by ``mdo`` and ``rec``
are recursively defined, much like in an ordinary let-expression. Due to
the new keyword ``mdo``, we also call this notation the *mdo-notation*.

Here is a simple (albeit contrived) example:

::

    {-# LANGUAGE RecursiveDo #-}
    justOnes = mdo { xs <- Just (1:xs)
                   ; return (map negate xs) }

or equivalently

::

    {-# LANGUAGE RecursiveDo #-}
    justOnes = do { rec { xs <- Just (1:xs) }
                  ; return (map negate xs) }

As you can guess ``justOnes`` will evaluate to ``Just [-1,-1,-1,...``.

GHC's implementation the mdo-notation closely follows the original
translation as described in the paper `A recursive do for
Haskell <http://leventerkok.github.io/papers/recdo.pdf>`__, which
in turn is based on the work `Value Recursion in Monadic
Computations <http://leventerkok.github.io/papers/erkok-thesis.pdf>`__.
Furthermore, GHC extends the syntax described in the former paper with a
lower level syntax flagged by the ``rec`` keyword, as we describe next.

Recursive binding groups
~~~~~~~~~~~~~~~~~~~~~~~~

The flag :ghc-flag:`-XRecursiveDo` also introduces a new keyword ``rec``, which
wraps a mutually-recursive group of monadic statements inside a ``do``
expression, producing a single statement. Similar to a ``let`` statement
inside a ``do``, variables bound in the ``rec`` are visible throughout
the ``rec`` group, and below it. For example, compare

::

        do { a <- getChar            do { a <- getChar
           ; let { r1 = f a r2          ; rec { r1 <- f a r2
           ;     ; r2 = g r1 }          ;     ; r2 <- g r1 }
           ; return (r1 ++ r2) }        ; return (r1 ++ r2) }

In both cases, ``r1`` and ``r2`` are available both throughout the
``let`` or ``rec`` block, and in the statements that follow it. The
difference is that ``let`` is non-monadic, while ``rec`` is monadic. (In
Haskell ``let`` is really ``letrec``, of course.)

The semantics of ``rec`` is fairly straightforward. Whenever GHC finds a
``rec`` group, it will compute its set of bound variables, and will
introduce an appropriate call to the underlying monadic value-recursion
operator ``mfix``, belonging to the ``MonadFix`` class. Here is an
example:

::

    rec { b <- f a c     ===>    (b,c) <- mfix (\ ~(b,c) -> do { b <- f a c
        ; c <- f b a }                                         ; c <- f b a
                                                               ; return (b,c) })

As usual, the meta-variables ``b``, ``c`` etc., can be arbitrary
patterns. In general, the statement ``rec ss`` is desugared to the
statement

::

    vs <- mfix (\ ~vs -> do { ss; return vs })

where ``vs`` is a tuple of the variables bound by ``ss``.

Note in particular that the translation for a ``rec`` block only
involves wrapping a call to ``mfix``: it performs no other analysis on
the bindings. The latter is the task for the ``mdo`` notation, which is
described next.

The ``mdo`` notation
~~~~~~~~~~~~~~~~~~~~

A ``rec``-block tells the compiler where precisely the recursive knot
should be tied. It turns out that the placement of the recursive knots
can be rather delicate: in particular, we would like the knots to be
wrapped around as minimal groups as possible. This process is known as
*segmentation*, and is described in detail in Section 3.2 of `A
recursive do for
Haskell <http://leventerkok.github.io/papers/recdo.pdf>`__.
Segmentation improves polymorphism and reduces the size of the recursive
knot. Most importantly, it avoids unnecessary interference caused by a
fundamental issue with the so-called *right-shrinking* axiom for monadic
recursion. In brief, most monads of interest (IO, strict state, etc.) do
*not* have recursion operators that satisfy this axiom, and thus not
performing segmentation can cause unnecessary interference, changing the
termination behavior of the resulting translation. (Details can be found
in Sections 3.1 and 7.2.2 of `Value Recursion in Monadic
Computations <http://leventerkok.github.io/papers/erkok-thesis.pdf>`__.)

The ``mdo`` notation removes the burden of placing explicit ``rec``
blocks in the code. Unlike an ordinary ``do`` expression, in which
variables bound by statements are only in scope for later statements,
variables bound in an ``mdo`` expression are in scope for all statements
of the expression. The compiler then automatically identifies minimal
mutually recursively dependent segments of statements, treating them as
if the user had wrapped a ``rec`` qualifier around them.

The definition is syntactic:

-  A generator ⟨g⟩ *depends* on a textually following generator ⟨g'⟩, if

   -  ⟨g'⟩ defines a variable that is used by ⟨g⟩, or

   -  ⟨g'⟩ textually appears between ⟨g⟩ and ⟨g''⟩, where ⟨g⟩ depends on
      ⟨g''⟩.

-  A *segment* of a given ``mdo``-expression is a minimal sequence of
   generators such that no generator of the sequence depends on an
   outside generator. As a special case, although it is not a generator,
   the final expression in an ``mdo``-expression is considered to form a
   segment by itself.

Segments in this sense are related to *strongly-connected components*
analysis, with the exception that bindings in a segment cannot be
reordered and must be contiguous.

Here is an example ``mdo``-expression, and its translation to ``rec``
blocks:

::

    mdo { a <- getChar      ===> do { a <- getChar
        ; b <- f a c                ; rec { b <- f a c
        ; c <- f b a                ;     ; c <- f b a }
        ; z <- h a b                ; z <- h a b
        ; d <- g d e                ; rec { d <- g d e
        ; e <- g a z                ;     ; e <- g a z }
        ; putChar c }               ; putChar c }

Note that a given ``mdo`` expression can cause the creation of multiple
``rec`` blocks. If there are no recursive dependencies, ``mdo`` will
introduce no ``rec`` blocks. In this latter case an ``mdo`` expression
is precisely the same as a ``do`` expression, as one would expect.

In summary, given an ``mdo`` expression, GHC first performs
segmentation, introducing ``rec`` blocks to wrap over minimal recursive
groups. Then, each resulting ``rec`` is desugared, using a call to
``Control.Monad.Fix.mfix`` as described in the previous section. The
original ``mdo``-expression typechecks exactly when the desugared
version would do so.

Here are some other important points in using the recursive-do notation:

-  It is enabled with the flag :ghc-flag:`-XRecursiveDo`, or the
   ``LANGUAGE RecursiveDo`` pragma. (The same flag enables both
   ``mdo``-notation, and the use of ``rec`` blocks inside ``do``
   expressions.)

-  ``rec`` blocks can also be used inside ``mdo``-expressions, which
   will be treated as a single statement. However, it is good style to
   either use ``mdo`` or ``rec`` blocks in a single expression.

-  If recursive bindings are required for a monad, then that monad must
   be declared an instance of the ``MonadFix`` class.

-  The following instances of ``MonadFix`` are automatically provided:
   List, Maybe, IO. Furthermore, the ``Control.Monad.ST`` and
   ``Control.Monad.ST.Lazy`` modules provide the instances of the
   ``MonadFix`` class for Haskell's internal state monad (strict and
   lazy, respectively).

-  Like ``let`` and ``where`` bindings, name shadowing is not allowed
   within an ``mdo``-expression or a ``rec``-block; that is, all the
   names bound in a single ``rec`` must be distinct. (GHC will complain
   if this is not the case.)

.. _applicative-do:

Applicative do-notation
-----------------------

.. index::
   single: Applicative do-notation
   single: do-notation; Applicative

.. ghc-flag:: -XApplicativeDo

    :since: 8.0.1

    Allow use of ``Applicative`` ``do`` notation.

The language option :ghc-flag:`-XApplicativeDo` enables an alternative translation for
the do-notation, which uses the operators ``<$>``, ``<*>``, along with ``join``
as far as possible. There are two main reasons for wanting to do this:

-  We can use do-notation with types that are an instance of ``Applicative`` and
   ``Functor``, but not ``Monad``
-  In some monads, using the applicative operators is more efficient than monadic
   bind. For example, it may enable more parallelism.

Applicative do-notation desugaring preserves the original semantics, provided
that the ``Applicative`` instance satisfies ``<*> = ap`` and ``pure = return``
(these are true of all the common monadic types). Thus, you can normally turn on
:ghc-flag:`-XApplicativeDo` without fear of breaking your program. There is one pitfall
to watch out for; see :ref:`applicative-do-pitfall`.

There are no syntactic changes with :ghc-flag:`-XApplicativeDo`. The only way it shows
up at the source level is that you can have a ``do`` expression that doesn't
require a ``Monad`` constraint. For example, in GHCi: ::

    Prelude> :set -XApplicativeDo
    Prelude> :t \m -> do { x <- m; return (not x) }
    \m -> do { x <- m; return (not x) }
      :: Functor f => f Bool -> f Bool

This example only requires ``Functor``, because it is translated into ``(\x ->
not x) <$> m``. A more complex example requires ``Applicative``, ::

    Prelude> :t \m -> do { x <- m 'a'; y <- m 'b'; return (x || y) }
    \m -> do { x <- m 'a'; y <- m 'b'; return (x || y) }
      :: Applicative f => (Char -> f Bool) -> f Bool

Here GHC has translated the expression into ::

    (\x y -> x || y) <$> m 'a' <*> m 'b'

It is possible to see the actual translation by using :ghc-flag:`-ddump-ds`, but be
warned, the output is quite verbose.

Note that if the expression can't be translated into uses of ``<$>``, ``<*>``
only, then it will incur a ``Monad`` constraint as usual. This happens when
there is a dependency on a value produced by an earlier statement in the
``do``-block: ::

    Prelude> :t \m -> do { x <- m True; y <- m x; return (x || y) }
    \m -> do { x <- m True; y <- m x; return (x || y) }
      :: Monad m => (Bool -> m Bool) -> m Bool

Here, ``m x`` depends on the value of ``x`` produced by the first statement, so
the expression cannot be translated using ``<*>``.

In general, the rule for when a ``do`` statement incurs a ``Monad`` constraint
is as follows. If the do-expression has the following form: ::

    do p1 <- E1; ...; pn <- En; return E

where none of the variables defined by ``p1...pn`` are mentioned in ``E1...En``,
and ``p1...pn`` are all variables or lazy patterns,
then the expression will only require ``Applicative``. Otherwise, the expression
will require ``Monad``. The block may return a pure expression ``E`` depending
upon the results ``p1...pn`` with either ``return`` or ``pure``.

Note: the final statement must match one of these patterns exactly:

- ``return E``
- ``return $ E``
- ``pure E``
- ``pure $ E``

otherwise GHC cannot recognise it as a ``return`` statement, and the
transformation to use ``<$>`` that we saw above does not apply.  In
particular, slight variations such as ``return . Just $ x`` or ``let x
= e in return x`` would not be recognised.

If the final statement is not of one of these forms, GHC falls back to
standard ``do`` desugaring, and the expression will require a
``Monad`` constraint.

When the statements of a ``do`` expression have dependencies between
them, and ``ApplicativeDo`` cannot infer an ``Applicative`` type, it
uses a heuristic algorithm to try to use ``<*>`` as much as possible.
This algorithm usually finds the best solution, but in rare complex
cases it might miss an opportunity.  There is an algorithm that finds
the optimal solution, provided as an option:

.. ghc-flag:: -foptimal-applicative-do

    :since: 8.0.1

    Enables an alternative algorithm for choosing where to use ``<*>``
    in conjunction with the ``ApplicativeDo`` language extension.
    This algorithm always finds the optimal solution, but it is
    expensive: ``O(n^3)``, so this option can lead to long compile
    times when there are very large ``do`` expressions (over 100
    statements).  The default ``ApplicativeDo`` algorithm is ``O(n^2)``.


.. _applicative-do-strict:

Strict patterns
~~~~~~~~~~~~~~~


A strict pattern match in a bind statement prevents
``ApplicativeDo`` from transforming that statement to use
``Applicative``.  This is because the transformation would change the
semantics by making the expression lazier.

For example, this code will require a ``Monad`` constraint::

    > :t \m -> do { (x:xs) <- m; return x }
    \m -> do { (x:xs) <- m; return x } :: Monad m => m [b] -> m b

but making the pattern match lazy allows it to have a ``Functor`` constraint::

    > :t \m -> do { ~(x:xs) <- m; return x }
    \m -> do { ~(x:xs) <- m; return x } :: Functor f => f [b] -> f b

A "strict pattern match" is any pattern match that can fail.  For
example, ``()``, ``(x:xs)``, ``!z``, and ``C x`` are strict patterns,
but ``x`` and ``~(1,2)`` are not.  For the purposes of
``ApplicativeDo``, a pattern match against a ``newtype`` constructor
is considered strict.

When there's a strict pattern match in a sequence of statements,
``ApplicativeDo`` places a ``>>=`` between that statement and the one
that follows it.  The sequence may be transformed to use ``<*>``
elsewhere, but the strict pattern match and the following statement
will always be connected with ``>>=``, to retain the same strictness
semantics as the standard do-notation.  If you don't want this, simply
put a ``~`` on the pattern match to make it lazy.

.. _applicative-do-existential:

Existential patterns and GADTs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When the pattern in a statement matches a constructor with
existential type variables and/or constraints, the transformation that
``ApplicativeDo`` performs may mean that the pattern does not scope
over the statements that follow it.  This is because the rearrangement
happens before the expression is typechecked.  For example, this
program does not typecheck::

    {-# LANGUAGE RankNTypes, GADTs, ApplicativeDo #-}

    data T where A :: forall a . Eq a => a -> T

    test = do
      A x <- undefined
      _ <- return 'a'
      _ <- return 'b'
      return (x == x)

The reason is that the ``Eq`` constraint that would be brought into
scope from the pattern match ``A x`` is not available when
typechecking the expression ``x == x``, because ``ApplicativeDo`` has
rearranged the expression to look like this::

    test =
      (\x _ -> x == x)
        <$> do A x <- undefined; _ <- return 'a'; return x
        <*> return 'b'

(Note that the ``return 'a'`` and ``return 'b'`` statements are needed
to make ``ApplicativeDo`` apply despite the restriction noted in
:ref:`applicative-do-strict`, because ``A x`` is a strict pattern match.)

Turning off ``ApplicativeDo`` lets the program typecheck.  This is
something to bear in mind when using ``ApplicativeDo`` in combination
with :ref:`existential-quantification` or :ref:`gadt`.

.. _applicative-do-pitfall:

Things to watch out for
~~~~~~~~~~~~~~~~~~~~~~~

Your code should just work as before when :ghc-flag:`-XApplicativeDo` is enabled,
provided you use conventional ``Applicative`` instances. However, if you define
a ``Functor`` or ``Applicative`` instance using do-notation, then it will likely
get turned into an infinite loop by GHC. For example, if you do this: ::

    instance Functor MyType where
        fmap f m = do x <- m; return (f x)

Then applicative desugaring will turn it into ::

    instance Functor MyType where
        fmap f m = fmap (\x -> f x) m

And the program will loop at runtime. Similarly, an ``Applicative`` instance
like this ::

    instance Applicative MyType where
        pure = return
        x <*> y = do f <- x; a <- y; return (f a)

will result in an infinte loop when ``<*>`` is called.

Just as you wouldn't define a ``Monad`` instance using the do-notation, you
shouldn't define ``Functor`` or ``Applicative`` instance using do-notation (when
using ``ApplicativeDo``) either. The correct way to define these instances in
terms of ``Monad`` is to use the ``Monad`` operations directly, e.g. ::

    instance Functor MyType where
        fmap f m = m >>= return . f

    instance Applicative MyType where
        pure = return
        (<*>) = ap


.. _parallel-list-comprehensions:

Parallel List Comprehensions
----------------------------

.. index::
   single: list comprehensions; parallel
   single: parallel list comprehensions

.. ghc-flag:: -XParallelListComp

    Allow parallel list comprehension syntax.

Parallel list comprehensions are a natural extension to list
comprehensions. List comprehensions can be thought of as a nice syntax
for writing maps and filters. Parallel comprehensions extend this to
include the ``zipWith`` family.

A parallel list comprehension has multiple independent branches of
qualifier lists, each separated by a ``|`` symbol. For example, the
following zips together two lists: ::

       [ (x, y) | x <- xs | y <- ys ]

The behaviour of parallel list comprehensions follows that of zip, in
that the resulting list will have the same length as the shortest
branch.

We can define parallel list comprehensions by translation to regular
comprehensions. Here's the basic idea:

Given a parallel comprehension of the form: ::

       [ e | p1 <- e11, p2 <- e12, ...
           | q1 <- e21, q2 <- e22, ...
           ...
       ]

This will be translated to: ::

       [ e | ((p1,p2), (q1,q2), ...) <- zipN [(p1,p2) | p1 <- e11, p2 <- e12, ...]
                                             [(q1,q2) | q1 <- e21, q2 <- e22, ...]
                                             ...
       ]

where ``zipN`` is the appropriate zip for the given number of branches.

.. _generalised-list-comprehensions:

Generalised (SQL-like) List Comprehensions
------------------------------------------

.. index::
   single: list comprehensions; generalised
   single: extended list comprehensions
   single: group
   single: SQL

.. ghc-flag:: -XTransformListComp

    Allow use of generalised list (SQL-like) comprehension syntax. This
    introduces the ``group``, ``by``, and ``using`` keywords.

Generalised list comprehensions are a further enhancement to the list
comprehension syntactic sugar to allow operations such as sorting and
grouping which are familiar from SQL. They are fully described in the
paper `Comprehensive comprehensions: comprehensions with "order by" and
"group by" <https://www.microsoft.com/en-us/research/wp-content/uploads/2007/09/list-comp.pdf>`__,
except that the syntax we use differs slightly from the paper.

The extension is enabled with the flag :ghc-flag:`-XTransformListComp`.

Here is an example:

::

    employees = [ ("Simon", "MS", 80)
                , ("Erik", "MS", 100)
                , ("Phil", "Ed", 40)
                , ("Gordon", "Ed", 45)
                , ("Paul", "Yale", 60) ]

    output = [ (the dept, sum salary)
             | (name, dept, salary) <- employees
             , then group by dept using groupWith
             , then sortWith by (sum salary)
             , then take 5 ]

In this example, the list ``output`` would take on the value:

::

    [("Yale", 60), ("Ed", 85), ("MS", 180)]

There are three new keywords: ``group``, ``by``, and ``using``. (The
functions ``sortWith`` and ``groupWith`` are not keywords; they are
ordinary functions that are exported by ``GHC.Exts``.)

There are five new forms of comprehension qualifier, all introduced by
the (existing) keyword ``then``:

-  ::

       then f

   This statement requires that
   f
   have the type
   forall a. [a] -> [a]
   . You can see an example of its use in the motivating example, as
   this form is used to apply
   take 5
   .
-  ::

       then f by e

   This form is similar to the previous one, but allows you to create a
   function which will be passed as the first argument to f. As a
   consequence f must have the type
   ``forall a. (a -> t) -> [a] -> [a]``. As you can see from the type,
   this function lets f "project out" some information from the elements
   of the list it is transforming.

   An example is shown in the opening example, where ``sortWith`` is
   supplied with a function that lets it find out the ``sum salary`` for
   any item in the list comprehension it transforms.

-  ::

       then group by e using f

   This is the most general of the grouping-type statements. In this
   form, f is required to have type
   ``forall a. (a -> t) -> [a] -> [[a]]``. As with the ``then f by e``
   case above, the first argument is a function supplied to f by the
   compiler which lets it compute e on every element of the list being
   transformed. However, unlike the non-grouping case, f additionally
   partitions the list into a number of sublists: this means that at
   every point after this statement, binders occurring before it in the
   comprehension refer to *lists* of possible values, not single values.
   To help understand this, let's look at an example:

   ::

       -- This works similarly to groupWith in GHC.Exts, but doesn't sort its input first
       groupRuns :: Eq b => (a -> b) -> [a] -> [[a]]
       groupRuns f = groupBy (\x y -> f x == f y)

       output = [ (the x, y)
       | x <- ([1..3] ++ [1..2])
       , y <- [4..6]
       , then group by x using groupRuns ]

   This results in the variable ``output`` taking on the value below:

   ::

       [(1, [4, 5, 6]), (2, [4, 5, 6]), (3, [4, 5, 6]), (1, [4, 5, 6]), (2, [4, 5, 6])]

   Note that we have used the ``the`` function to change the type of x
   from a list to its original numeric type. The variable y, in
   contrast, is left unchanged from the list form introduced by the
   grouping.

-  ::

       then group using f

   With this form of the group statement, f is required to simply have
   the type ``forall a. [a] -> [[a]]``, which will be used to group up
   the comprehension so far directly. An example of this form is as
   follows:

   ::

       output = [ x
       | y <- [1..5]
       , x <- "hello"
       , then group using inits]

   This will yield a list containing every prefix of the word "hello"
   written out 5 times:

   ::

       ["","h","he","hel","hell","hello","helloh","hellohe","hellohel","hellohell","hellohello","hellohelloh",...]

.. _monad-comprehensions:

Monad comprehensions
--------------------

.. index::
   single: monad comprehensions

.. ghc-flag:: -XMonadComprehensions

    :since: 7.2

    Enable list comprehension syntax for arbitrary monads.

Monad comprehensions generalise the list comprehension notation,
including parallel comprehensions (:ref:`parallel-list-comprehensions`)
and transform comprehensions (:ref:`generalised-list-comprehensions`) to
work for any monad.

Monad comprehensions support:

-  Bindings: ::

       [ x + y | x <- Just 1, y <- Just 2 ]

   Bindings are translated with the ``(>>=)`` and ``return`` functions
   to the usual do-notation: ::

       do x <- Just 1
          y <- Just 2
          return (x+y)

-  Guards: ::

       [ x | x <- [1..10], x <= 5 ]

   Guards are translated with the ``guard`` function, which requires a
   ``MonadPlus`` instance: ::

       do x <- [1..10]
          guard (x <= 5)
          return x

-  Transform statements (as with :ghc-flag:`-XTransformListComp`): ::

       [ x+y | x <- [1..10], y <- [1..x], then take 2 ]

   This translates to: ::

       do (x,y) <- take 2 (do x <- [1..10]
                              y <- [1..x]
                              return (x,y))
          return (x+y)

-  Group statements (as with :ghc-flag:`-XTransformListComp`):

   ::

       [ x | x <- [1,1,2,2,3], then group by x using GHC.Exts.groupWith ]
       [ x | x <- [1,1,2,2,3], then group using myGroup ]

-  Parallel statements (as with :ghc-flag:`-XParallelListComp`):

   ::

       [ (x+y) | x <- [1..10]
               | y <- [11..20]
               ]

   Parallel statements are translated using the ``mzip`` function, which
   requires a ``MonadZip`` instance defined in
   :base-ref:`Control.Monad.Zip.`:

   ::

       do (x,y) <- mzip (do x <- [1..10]
                            return x)
                        (do y <- [11..20]
                            return y)
          return (x+y)

All these features are enabled by default if the :ghc-flag:`-XMonadComprehensions`
extension is enabled. The types and more detailed examples on how to use
comprehensions are explained in the previous chapters
:ref:`generalised-list-comprehensions` and
:ref:`parallel-list-comprehensions`. In general you just have to replace
the type ``[a]`` with the type ``Monad m => m a`` for monad
comprehensions.

.. note::
    Even though most of these examples are using the list monad, monad
    comprehensions work for any monad. The ``base`` package offers all
    necessary instances for lists, which make :ghc-flag:`-XMonadComprehensions`
    backward compatible to built-in, transform and parallel list
    comprehensions.

More formally, the desugaring is as follows. We write ``D[ e | Q]`` to
mean the desugaring of the monad comprehension ``[ e | Q]``:

.. code-block:: none

    Expressions: e
    Declarations: d
    Lists of qualifiers: Q,R,S

    -- Basic forms
    D[ e | ]               = return e
    D[ e | p <- e, Q ]  = e >>= \p -> D[ e | Q ]
    D[ e | e, Q ]          = guard e >> \p -> D[ e | Q ]
    D[ e | let d, Q ]      = let d in D[ e | Q ]

    -- Parallel comprehensions (iterate for multiple parallel branches)
    D[ e | (Q | R), S ]    = mzip D[ Qv | Q ] D[ Rv | R ] >>= \(Qv,Rv) -> D[ e | S ]

    -- Transform comprehensions
    D[ e | Q then f, R ]                  = f D[ Qv | Q ] >>= \Qv -> D[ e | R ]

    D[ e | Q then f by b, R ]             = f (\Qv -> b) D[ Qv | Q ] >>= \Qv -> D[ e | R ]

    D[ e | Q then group using f, R ]      = f D[ Qv | Q ] >>= \ys ->
                                            case (fmap selQv1 ys, ..., fmap selQvn ys) of
                                             Qv -> D[ e | R ]

    D[ e | Q then group by b using f, R ] = f (\Qv -> b) D[ Qv | Q ] >>= \ys ->
                                            case (fmap selQv1 ys, ..., fmap selQvn ys) of
                                               Qv -> D[ e | R ]

    where  Qv is the tuple of variables bound by Q (and used subsequently)
           selQvi is a selector mapping Qv to the ith component of Qv

    Operator     Standard binding       Expected type
    --------------------------------------------------------------------
    return       GHC.Base               t1 -> m t2
    (>>=)        GHC.Base               m1 t1 -> (t2 -> m2 t3) -> m3 t3
    (>>)         GHC.Base               m1 t1 -> m2 t2         -> m3 t3
    guard        Control.Monad          t1 -> m t2
    fmap         GHC.Base               forall a b. (a->b) -> n a -> n b
    mzip         Control.Monad.Zip      forall a b. m a -> m b -> m (a,b)

The comprehension should typecheck when its desugaring would typecheck,
except that (as discussed in :ref:`generalised-list-comprehensions`) in the
"then ``f``" and "then group using ``f``" clauses, when the "by ``b``" qualifier
is omitted, argument ``f`` should have a polymorphic type. In particular, "then
``Data.List.sort``" and "then group using ``Data.List.group``" are
insufficiently polymorphic.

Monad comprehensions support rebindable syntax
(:ref:`rebindable-syntax`). Without rebindable syntax, the operators
from the "standard binding" module are used; with rebindable syntax, the
operators are looked up in the current lexical scope. For example,
parallel comprehensions will be typechecked and desugared using whatever
"``mzip``" is in scope.

The rebindable operators must have the "Expected type" given in the
table above. These types are surprisingly general. For example, you can
use a bind operator with the type

::

    (>>=) :: T x y a -> (a -> T y z b) -> T x z b

In the case of transform comprehensions, notice that the groups are
parameterised over some arbitrary type ``n`` (provided it has an
``fmap``, as well as the comprehension being over an arbitrary monad.

.. _monadfail-desugaring:

New monadic failure desugaring mechanism
----------------------------------------

.. ghc-flag:: -XMonadFailDesugaring

    :since: 8.0.1

    Use the ``MonadFail.fail`` instead of the legacy ``Monad.fail`` function
    when desugaring refutable patterns in ``do`` blocks.

The ``-XMonadFailDesugaring`` extension switches the desugaring of
``do``-blocks to use ``MonadFail.fail`` instead of ``Monad.fail``. This will
eventually be the default behaviour in a future GHC release, under the
`MonadFail Proposal (MFP)
<https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail>`__.

This extension is temporary, and will be deprecated in a future release. It is
included so that library authors have a hard check for whether their code
will work with future GHC versions.

.. _rebindable-syntax:

Rebindable syntax and the implicit Prelude import
-------------------------------------------------

.. ghc-flag:: -XNoImplicitPrelude

    Don't import ``Prelude`` by default.

GHC normally imports ``Prelude.hi`` files for
you. If you'd rather it didn't, then give it a ``-XNoImplicitPrelude``
option. The idea is that you can then import a Prelude of your own. (But
don't call it ``Prelude``; the Haskell module namespace is flat, and you
must not conflict with any Prelude module.)

.. ghc-flag:: -XRebindableSyntax

    :implies: :ghc-flag:`-XNoImplicitPrelude`
    :since: 7.0.1

    Enable rebinding of a variety of usually-built-in operations.

Suppose you are importing a Prelude of your own in order to define your
own numeric class hierarchy. It completely defeats that purpose if the
literal "1" means "``Prelude.fromInteger 1``", which is what the Haskell
Report specifies. So the :ghc-flag:`-XRebindableSyntax` flag causes the
following pieces of built-in syntax to refer to *whatever is in scope*,
not the Prelude versions:

-  An integer literal ``368`` means "``fromInteger (368::Integer)``",
   rather than "``Prelude.fromInteger (368::Integer)``".

-  Fractional literals are handed in just the same way, except that the
   translation is ``fromRational (3.68::Rational)``.

-  The equality test in an overloaded numeric pattern uses whatever
   ``(==)`` is in scope.

-  The subtraction operation, and the greater-than-or-equal test, in
   ``n+k`` patterns use whatever ``(-)`` and ``(>=)`` are in scope.

-  Negation (e.g. "``- (f x)``") means "``negate (f x)``", both in
   numeric patterns, and expressions.

-  Conditionals (e.g. "``if`` e1 ``then`` e2 ``else`` e3") means
   "``ifThenElse`` e1 e2 e3". However ``case`` expressions are
   unaffected.

-  "Do" notation is translated using whatever functions ``(>>=)``,
   ``(>>)``, and ``fail``, are in scope (not the Prelude versions). List
   comprehensions, ``mdo`` (:ref:`recursive-do-notation`), and parallel
   array comprehensions, are unaffected.

-  Arrow notation (see :ref:`arrow-notation`) uses whatever ``arr``,
   ``(>>>)``, ``first``, ``app``, ``(|||)`` and ``loop`` functions are
   in scope. But unlike the other constructs, the types of these
   functions must match the Prelude types very closely. Details are in
   flux; if you want to use this, ask!

-  List notation, such as ``[x,y]`` or ``[m..n]`` can also be treated
   via rebindable syntax if you use `-XOverloadedLists`;
   see :ref:`overloaded-lists`.

-  An overloaded label "``#foo``" means "``fromLabel @"foo"``", rather than
   "``GHC.OverloadedLabels.fromLabel @"foo"``" (see :ref:`overloaded-labels`).

:ghc-flag:`-XRebindableSyntax` implies :ghc-flag:`-XNoImplicitPrelude`.

In all cases (apart from arrow notation), the static semantics should be
that of the desugared form, even if that is a little unexpected. For
example, the static semantics of the literal ``368`` is exactly that of
``fromInteger (368::Integer)``; it's fine for ``fromInteger`` to have
any of the types: ::

    fromInteger :: Integer -> Integer
    fromInteger :: forall a. Foo a => Integer -> a
    fromInteger :: Num a => a -> Integer
    fromInteger :: Integer -> Bool -> Bool

Be warned: this is an experimental facility, with fewer checks than
usual. Use ``-dcore-lint`` to typecheck the desugared program. If Core
Lint is happy you should be all right.

Things unaffected by :ghc-flag:`-XRebindableSyntax`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:ghc-flag:`-XRebindableSyntax` does not apply to any code generated from a
``deriving`` clause or declaration. To see why, consider the following code: ::

    {-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
    newtype Text = Text String

    fromString :: String -> Text
    fromString = Text

    data Foo = Foo deriving Show

This will generate code to the effect of: ::

    instance Show Foo where
      showsPrec _ Foo = showString "Foo"

But because :ghc-flag:`-XRebindableSyntax` and :ghc-flag:`-XOverloadedStrings`
are enabled, the ``"Foo"`` string literal would now be of type ``Text``, not
``String``, which ``showString`` doesn't accept! This causes the generated
``Show`` instance to fail to typecheck. It's hard to imagine any scenario where
it would be desirable have :ghc-flag:`-XRebindableSyntax` behavior within
derived code, so GHC simply ignores :ghc-flag:`-XRebindableSyntax` entirely
when checking derived code.

.. _postfix-operators:

Postfix operators
-----------------

.. ghc-flag:: -XPostfixOperators

    Allow the use of post-fix operators

The :ghc-flag:`-XPostfixOperators` flag enables a small extension to the syntax
of left operator sections, which allows you to define postfix operators.
The extension is this: the left section ::

      (e !)

is equivalent (from the point of view of both type checking and
execution) to the expression ::

      ((!) e)

(for any expression ``e`` and operator ``(!)``. The strict Haskell 98
interpretation is that the section is equivalent to ::

      (\y -> (!) e y)

That is, the operator must be a function of two arguments. GHC allows it
to take only one argument, and that in turn allows you to write the
function postfix.

The extension does not extend to the left-hand side of function
definitions; you must define such a function in prefix form.

.. _tuple-sections:

Tuple sections
--------------

.. ghc-flag:: -XTupleSections

    :since: 6.12

    Allow the use of tuple section syntax

The :ghc-flag:`-XTupleSections` flag enables partially applied
tuple constructors. For example, the following program ::

      (, True)

is considered to be an alternative notation for the more unwieldy
alternative ::

      \x -> (x, True)

You can omit any combination of arguments to the tuple, as in the
following ::

      (, "I", , , "Love", , 1337)

which translates to ::

      \a b c d -> (a, "I", b, c, "Love", d, 1337)

If you have `unboxed tuples <#unboxed-tuples>`__ enabled, tuple sections
will also be available for them, like so ::

      (# , True #)

Because there is no unboxed unit tuple, the following expression ::

      (# #)

continues to stand for the unboxed singleton tuple data constructor.

.. _lambda-case:

Lambda-case
-----------

.. ghc-flag:: -XLambdaCase

    :since: 7.6.1

    Allow the use of lambda-case syntax.

The :ghc-flag:`-XLambdaCase` flag enables expressions of the form ::

      \case { p1 -> e1; ...; pN -> eN }

which is equivalent to ::

      \freshName -> case freshName of { p1 -> e1; ...; pN -> eN }

Note that ``\case`` starts a layout, so you can write ::

      \case
        p1 -> e1
        ...
        pN -> eN

.. _empty-case:

Empty case alternatives
-----------------------

.. ghc-flag:: -XEmptyCase

    :since: 7.8.1

    Allow empty case expressions.

The :ghc-flag:`-XEmptyCase` flag enables case expressions, or lambda-case
expressions, that have no alternatives, thus: ::

    case e of { }   -- No alternatives

or ::

    \case { }       -- -XLambdaCase is also required

This can be useful when you know that the expression being scrutinised
has no non-bottom values. For example:

::

      data Void
      f :: Void -> Int
      f x = case x of { }

With dependently-typed features it is more useful (see :ghc-ticket:`2431`). For
example, consider these two candidate definitions of ``absurd``:

::

    data a :==: b where
      Refl :: a :==: a

    absurd :: True :~: False -> a
    absurd x = error "absurd"    -- (A)
    absurd x = case x of {}      -- (B)

We much prefer (B). Why? Because GHC can figure out that
``(True :~: False)`` is an empty type. So (B) has no partiality and GHC
should be able to compile with :ghc-flag:`-Wincomplete-patterns`. (Though
the pattern match checking is not yet clever enough to do that.) On the
other hand (A) looks dangerous, and GHC doesn't check to make sure that,
in fact, the function can never get called.

.. _multi-way-if:

Multi-way if-expressions
------------------------

.. ghc-flag:: -XMultiWayIf

    :since: 7.6.1

    Allow the use of multi-way-``if`` syntax.

With :ghc-flag:`-XMultiWayIf` flag GHC accepts conditional expressions with
multiple branches: ::

      if | guard1 -> expr1
         | ...
         | guardN -> exprN

which is roughly equivalent to ::

      case () of
        _ | guard1 -> expr1
        ...
        _ | guardN -> exprN

Multi-way if expressions introduce a new layout context. So the example
above is equivalent to: ::

      if { | guard1 -> expr1
         ; | ...
         ; | guardN -> exprN
         }

The following behaves as expected: ::

      if | guard1 -> if | guard2 -> expr2
                        | guard3 -> expr3
         | guard4 -> expr4

because layout translates it as ::

      if { | guard1 -> if { | guard2 -> expr2
                          ; | guard3 -> expr3
                          }
         ; | guard4 -> expr4
         }

Layout with multi-way if works in the same way as other layout contexts,
except that the semi-colons between guards in a multi-way if are
optional. So it is not necessary to line up all the guards at the same
column; this is consistent with the way guards work in function
definitions and case expressions.

.. _local-fixity-declarations:

Local Fixity Declarations
-------------------------

A careful reading of the Haskell 98 Report reveals that fixity
declarations (``infix``, ``infixl``, and ``infixr``) are permitted to
appear inside local bindings such those introduced by ``let`` and
``where``. However, the Haskell Report does not specify the semantics of
such bindings very precisely.

In GHC, a fixity declaration may accompany a local binding: ::

    let f = ...
        infixr 3 `f`
    in
        ...

and the fixity declaration applies wherever the binding is in scope. For
example, in a ``let``, it applies in the right-hand sides of other
``let``-bindings and the body of the ``let``\ C. Or, in recursive ``do``
expressions (:ref:`recursive-do-notation`), the local fixity
declarations of a ``let`` statement scope over other statements in the
group, just as the bound name does.

Moreover, a local fixity declaration *must* accompany a local binding
of that name: it is not possible to revise the fixity of name bound
elsewhere, as in ::

    let infixr 9 $ in ...

Because local fixity declarations are technically Haskell 98, no flag is
necessary to enable them.

.. _package-imports:

Import and export extensions
----------------------------

Hiding things the imported module doesn't export
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Technically in Haskell 2010 this is illegal: ::

    module A( f ) where
      f = True

    module B where
      import A hiding( g )  -- A does not export g
      g = f

The ``import A hiding( g )`` in module ``B`` is technically an error
(`Haskell Report,
5.3.1 <http://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1020005.3.1>`__)
because ``A`` does not export ``g``. However GHC allows it, in the
interests of supporting backward compatibility; for example, a newer
version of ``A`` might export ``g``, and you want ``B`` to work in
either case.

The warning :ghc-flag:`-Wdodgy-imports`, which is off by default but included
with :ghc-flag:`-W`, warns if you hide something that the imported module does
not export.

.. _package-qualified-imports:

Package-qualified imports
~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XPackageImports

    Allow the use of package-qualified ``import`` syntax.

With the :ghc-flag:`-XPackageImports` flag, GHC allows import declarations to be
qualified by the package name that the module is intended to be imported
from. For example: ::

    import "network" Network.Socket

would import the module ``Network.Socket`` from the package ``network``
(any version). This may be used to disambiguate an import when the same
module is available from multiple packages, or is present in both the
current package being built and an external package.

The special package name ``this`` can be used to refer to the current
package being built.

.. note::
   You probably don't need to use this feature, it was added mainly so that we
   can build backwards-compatible versions of packages when APIs change. It can
   lead to fragile dependencies in the common case: modules occasionally move
   from one package to another, rendering any package-qualified imports broken.
   See also :ref:`package-thinning-and-renaming` for an alternative way of
   disambiguating between module names.

.. _safe-imports-ext:

Safe imports
~~~~~~~~~~~~

.. ghc-flag:: -XSafe
              -XTrustworthy
              -XUnsafe
    :noindex:

    :since: 7.2

    Declare the Safe Haskell state of the current module.

With the :ghc-flag:`-XSafe`, :ghc-flag:`-XTrustworthy` and :ghc-flag:`-XUnsafe`
language flags, GHC extends the import declaration syntax to take an optional
``safe`` keyword after the ``import`` keyword. This feature is part of the Safe
Haskell GHC extension. For example: ::

    import safe qualified Network.Socket as NS

would import the module ``Network.Socket`` with compilation only
succeeding if ``Network.Socket`` can be safely imported. For a description of
when a import is considered safe see :ref:`safe-haskell`.

.. _explicit-namespaces:

Explicit namespaces in import/export
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XExplicitNamespaces

    :since: 7.6.1

    Enable use of explicit namespaces in module export lists.

In an import or export list, such as ::

      module M( f, (++) ) where ...
        import N( f, (++) )
        ...

the entities ``f`` and ``(++)`` are *values*. However, with type
operators (:ref:`type-operators`) it becomes possible to declare
``(++)`` as a *type constructor*. In that case, how would you export or
import it?

The :ghc-flag:`-XExplicitNamespaces` extension allows you to prefix the name of
a type constructor in an import or export list with "``type``" to
disambiguate this case, thus: ::

      module M( f, type (++) ) where ...
        import N( f, type (++) )
        ...
      module N( f, type (++) ) where
        data family a ++ b = L a | R b

The extension :ghc-flag:`-XExplicitNamespaces` is implied by
:ghc-flag:`-XTypeOperators` and (for some reason) by :ghc-flag:`-XTypeFamilies`.

In addition, with :ghc-flag:`-XPatternSynonyms` you can prefix the name of a
data constructor in an import or export list with the keyword
``pattern``, to allow the import or export of a data constructor without
its parent type constructor (see :ref:`patsyn-impexp`).

.. _syntax-stolen:

Summary of stolen syntax
------------------------

Turning on an option that enables special syntax *might* cause working
Haskell 98 code to fail to compile, perhaps because it uses a variable
name which has become a reserved word. This section lists the syntax
that is "stolen" by language extensions. We use notation and nonterminal
names from the Haskell 98 lexical syntax (see the Haskell 98 Report). We
only list syntax changes here that might affect existing working
programs (i.e. "stolen" syntax). Many of these extensions will also
enable new context-free syntax, but in all cases programs written to use
the new syntax would not be compilable without the option enabled.

There are two classes of special syntax:

-  New reserved words and symbols: character sequences which are no
   longer available for use as identifiers in the program.

-  Other special syntax: sequences of characters that have a different
   meaning when this particular option is turned on.

The following syntax is stolen:

``forall``
    .. index::
       single: forall

    Stolen (in types) by: :ghc-flag:`-XExplicitForAll`, and hence by
    :ghc-flag:`-XScopedTypeVariables`, :ghc-flag:`-XLiberalTypeSynonyms`,
    :ghc-flag:`-XRankNTypes`, :ghc-flag:`-XExistentialQuantification`

``mdo``
    .. index::
       single: mdo

    Stolen by: :ghc-flag:`-XRecursiveDo`

``foreign``
    .. index::
       single: foreign

    Stolen by: :ghc-flag:`-XForeignFunctionInterface`

``rec``, ``proc``, ``-<``, ``>-``, ``-<<``, ``>>-``, ``(|``, ``|)``
    .. index::
       single: proc

    Stolen by: :ghc-flag:`-XArrows`

``?varid``
    .. index::
       single: implicit parameters

    Stolen by: :ghc-flag:`-XImplicitParams`

``[|``, ``[e|``, ``[p|``, ``[d|``, ``[t|``, ``[||``, ``[e||``
    .. index::
       single: Quasi-quotes

    Stolen by: :ghc-flag:`-XQuasiQuotes`. Moreover, this introduces an ambiguity
    with list comprehension syntax. See the
    :ref:`discussion on quasi-quoting <quasi-quotes-list-comprehension-ambiguity>`
    for details.

``$(``, ``$$(``, ``$varid``, ``$$varid``
    .. index::
       single: Template Haskell

    Stolen by: :ghc-flag:`-XTemplateHaskell`

``[varid|``
    .. index::
       single: quasi-quotation

    Stolen by: :ghc-flag:`-XQuasiQuotes`

⟨varid⟩, ``#``\ ⟨char⟩, ``#``, ⟨string⟩, ``#``, ⟨integer⟩, ``#``, ⟨float⟩, ``#``, ⟨float⟩, ``##``
    Stolen by: :ghc-flag:`-XMagicHash`

``(#``, ``#)``
    Stolen by: :ghc-flag:`-XUnboxedTuples`

⟨varid⟩, ``!``, ⟨varid⟩
    Stolen by: :ghc-flag:`-XBangPatterns`

``pattern``
    Stolen by: :ghc-flag:`-XPatternSynonyms`

.. _data-type-extensions:

Extensions to data types and type synonyms
==========================================

.. _nullary-types:

Data types with no constructors
-------------------------------

.. ghc-flag:: -XEmptyDataDecls

    Allow definition of empty ``data`` types.

With the :ghc-flag:`-XEmptyDataDecls` flag (or equivalent ``LANGUAGE`` pragma), GHC
lets you declare a data type with no constructors. For example: ::

      data S      -- S :: *
      data T a    -- T :: * -> *

Syntactically, the declaration lacks the "= constrs" part. The type can
be parameterised over types of any kind, but if the kind is not ``*``
then an explicit kind annotation must be used (see :ref:`kinding`).

Such data types have only one value, namely bottom. Nevertheless, they
can be useful when defining "phantom types".

.. _datatype-contexts:

Data type contexts
------------------

.. ghc-flag:: -XDatatypeContexts

    :since: 7.0.1

    Allow contexts on ``data`` types.

Haskell allows datatypes to be given contexts, e.g. ::

    data Eq a => Set a = NilSet | ConsSet a (Set a)

give constructors with types: ::

    NilSet :: Set a
    ConsSet :: Eq a => a -> Set a -> Set a

This is widely considered a misfeature, and is going to be removed from
the language. In GHC, it is controlled by the deprecated extension
``DatatypeContexts``.

.. _infix-tycons:

Infix type constructors, classes, and type variables
----------------------------------------------------

GHC allows type constructors, classes, and type variables to be
operators, and to be written infix, very much like expressions. More
specifically:

-  A type constructor or class can be any non-reserved operator.
   Symbols used in types are always like capitalized identifiers; they
   are never variables. Note that this is different from the lexical
   syntax of data constructors, which are required to begin with a
   ``:``.

-  Data type and type-synonym declarations can be written infix,
   parenthesised if you want further arguments. E.g. ::

         data a :*: b = Foo a b
         type a :+: b = Either a b
         class a :=: b where ...

         data (a :**: b) x = Baz a b x
         type (a :++: b) y = Either (a,b) y

-  Types, and class constraints, can be written infix. For example ::

         x :: Int :*: Bool
         f :: (a :=: b) => a -> b

-  Back-quotes work as for expressions, both for type constructors and
   type variables; e.g. ``Int `Either` Bool``, or ``Int `a` Bool``.
   Similarly, parentheses work the same; e.g. ``(:*:) Int Bool``.

-  Fixities may be declared for type constructors, or classes, just as
   for data constructors. However, one cannot distinguish between the
   two in a fixity declaration; a fixity declaration sets the fixity for
   a data constructor and the corresponding type constructor. For
   example: ::

         infixl 7 T, :*:

   sets the fixity for both type constructor ``T`` and data constructor
   ``T``, and similarly for ``:*:``. ``Int `a` Bool``.

-  Function arrow is ``infixr`` with fixity 0 (this might change; it's
   not clear what it should be).

.. _type-operators:

Type operators
--------------

.. ghc-flag:: -XTypeOperators

    :implies: :ghc-flag:`-XExplicitNamespaces`

    Allow the use and definition of types with operator names.

In types, an operator symbol like ``(+)`` is normally treated as a type
*variable*, just like ``a``. Thus in Haskell 98 you can say

::

    type T (+) = ((+), (+))
    -- Just like: type T a = (a,a)

    f :: T Int -> Int
    f (x,y)= x

As you can see, using operators in this way is not very useful, and
Haskell 98 does not even allow you to write them infix.

The language :ghc-flag:`-XTypeOperators` changes this behaviour:

-  Operator symbols become type *constructors* rather than type
   *variables*.

-  Operator symbols in types can be written infix, both in definitions
   and uses. For example: ::

       data a + b = Plus a b
       type Foo = Int + Bool

-  There is now some potential ambiguity in import and export lists; for
   example if you write ``import M( (+) )`` do you mean the *function*
   ``(+)`` or the *type constructor* ``(+)``? The default is the former,
   but with :ghc-flag:`-XExplicitNamespaces` (which is implied by
   :ghc-flag:`-XTypeOperators`) GHC allows you to specify the latter by
   preceding it with the keyword ``type``, thus: ::

       import M( type (+) )

   See :ref:`explicit-namespaces`.

-  The fixity of a type operator may be set using the usual fixity
   declarations but, as in :ref:`infix-tycons`, the function and type
   constructor share a single fixity.

.. _type-synonyms:

Liberalised type synonyms
-------------------------

.. ghc-flag:: -XLiberalTypeSynonyms

    :implies: :ghc-flag:`-XExplicitForAll`

    Relax many of the Haskell 98 rules on type synonym definitions.

Type synonyms are like macros at the type level, but Haskell 98 imposes
many rules on individual synonym declarations. With the
:ghc-flag:`-XLiberalTypeSynonyms` extension, GHC does validity checking on types
*only after expanding type synonyms*. That means that GHC can be very
much more liberal about type synonyms than Haskell 98.

-  You can write a ``forall`` (including overloading) in a type synonym,
   thus: ::

         type Discard a = forall b. Show b => a -> b -> (a, String)

         f :: Discard a
         f x y = (x, show y)

         g :: Discard Int -> (Int,String)    -- A rank-2 type
         g f = f 3 True

-  If you also use :ghc-flag:`-XUnboxedTuples`, you can write an unboxed tuple
   in a type synonym: ::

         type Pr = (# Int, Int #)

         h :: Int -> Pr
         h x = (# x, x #)

-  You can apply a type synonym to a forall type: ::

         type Foo a = a -> a -> Bool

         f :: Foo (forall b. b->b)

   After expanding the synonym, ``f`` has the legal (in GHC) type: ::

         f :: (forall b. b->b) -> (forall b. b->b) -> Bool

-  You can apply a type synonym to a partially applied type synonym: ::

         type Generic i o = forall x. i x -> o x
         type Id x = x

         foo :: Generic Id []

   After expanding the synonym, ``foo`` has the legal (in GHC) type: ::

         foo :: forall x. x -> [x]

GHC currently does kind checking before expanding synonyms (though even
that could be changed).

After expanding type synonyms, GHC does validity checking on types,
looking for the following malformedness which isn't detected simply by
kind checking:

-  Type constructor applied to a type involving for-alls (if
   :ghc-flag:`-XImpredicativeTypes` is off)

-  Partially-applied type synonym.

So, for example, this will be rejected: ::

      type Pr = forall a. a

      h :: [Pr]
      h = ...

because GHC does not allow type constructors applied to for-all types.

.. _existential-quantification:

Existentially quantified data constructors
------------------------------------------

.. ghc-flag:: -XExistentialQuantification

    :implies: :ghc-flag:`-XExplicitForAll`

    Allow existentially quantified type variables in types.

The idea of using existential quantification in data type declarations
was suggested by Perry, and implemented in Hope+ (Nigel Perry, *The
Implementation of Practical Functional Programming Languages*, PhD
Thesis, University of London, 1991). It was later formalised by Laufer
and Odersky (*Polymorphic type inference and abstract data types*,
TOPLAS, 16(5), pp. 1411-1430, 1994). It's been in Lennart Augustsson's
``hbc`` Haskell compiler for several years, and proved very useful.
Here's the idea. Consider the declaration: ::

      data Foo = forall a. MkFoo a (a -> Bool)
               | Nil

The data type ``Foo`` has two constructors with types: ::

      MkFoo :: forall a. a -> (a -> Bool) -> Foo
      Nil   :: Foo

Notice that the type variable ``a`` in the type of ``MkFoo`` does not
appear in the data type itself, which is plain ``Foo``. For example, the
following expression is fine: ::

      [MkFoo 3 even, MkFoo 'c' isUpper] :: [Foo]

Here, ``(MkFoo 3 even)`` packages an integer with a function ``even``
that maps an integer to ``Bool``; and ``MkFoo 'c'
isUpper`` packages a character with a compatible function. These two
things are each of type ``Foo`` and can be put in a list.

What can we do with a value of type ``Foo``? In particular, what
happens when we pattern-match on ``MkFoo``? ::

      f (MkFoo val fn) = ???

Since all we know about ``val`` and ``fn`` is that they are compatible,
the only (useful) thing we can do with them is to apply ``fn`` to
``val`` to get a boolean. For example: ::

      f :: Foo -> Bool
      f (MkFoo val fn) = fn val

What this allows us to do is to package heterogeneous values together
with a bunch of functions that manipulate them, and then treat that
collection of packages in a uniform manner. You can express quite a bit
of object-oriented-like programming this way.

.. _existential:

Why existential?
~~~~~~~~~~~~~~~~

What has this to do with *existential* quantification? Simply that
``MkFoo`` has the (nearly) isomorphic type ::

      MkFoo :: (exists a . (a, a -> Bool)) -> Foo

But Haskell programmers can safely think of the ordinary *universally*
quantified type given above, thereby avoiding adding a new existential
quantification construct.

.. _existential-with-context:

Existentials and type classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An easy extension is to allow arbitrary contexts before the constructor.
For example: ::

    data Baz = forall a. Eq a => Baz1 a a
             | forall b. Show b => Baz2 b (b -> b)

The two constructors have the types you'd expect: ::

    Baz1 :: forall a. Eq a => a -> a -> Baz
    Baz2 :: forall b. Show b => b -> (b -> b) -> Baz

But when pattern matching on ``Baz1`` the matched values can be compared
for equality, and when pattern matching on ``Baz2`` the first matched
value can be converted to a string (as well as applying the function to
it). So this program is legal: ::

      f :: Baz -> String
      f (Baz1 p q) | p == q    = "Yes"
                   | otherwise = "No"
      f (Baz2 v fn)            = show (fn v)

Operationally, in a dictionary-passing implementation, the constructors
``Baz1`` and ``Baz2`` must store the dictionaries for ``Eq`` and
``Show`` respectively, and extract it on pattern matching.

.. _existential-records:

Record Constructors
~~~~~~~~~~~~~~~~~~~

GHC allows existentials to be used with records syntax as well. For
example: ::

    data Counter a = forall self. NewCounter
        { _this    :: self
        , _inc     :: self -> self
        , _display :: self -> IO ()
        , tag      :: a
        }

Here ``tag`` is a public field, with a well-typed selector function
``tag :: Counter a -> a``. The ``self`` type is hidden from the outside;
any attempt to apply ``_this``, ``_inc`` or ``_display`` as functions
will raise a compile-time error. In other words, *GHC defines a record
selector function only for fields whose type does not mention the
existentially-quantified variables*. (This example used an underscore in
the fields for which record selectors will not be defined, but that is
only programming style; GHC ignores them.)

To make use of these hidden fields, we need to create some helper
functions: ::

    inc :: Counter a -> Counter a
    inc (NewCounter x i d t) = NewCounter
        { _this = i x, _inc = i, _display = d, tag = t }

    display :: Counter a -> IO ()
    display NewCounter{ _this = x, _display = d } = d x

Now we can define counters with different underlying implementations: ::

    counterA :: Counter String
    counterA = NewCounter
        { _this = 0, _inc = (1+), _display = print, tag = "A" }

    counterB :: Counter String
    counterB = NewCounter
        { _this = "", _inc = ('#':), _display = putStrLn, tag = "B" }

    main = do
        display (inc counterA)         -- prints "1"
        display (inc (inc counterB))   -- prints "##"

Record update syntax is supported for existentials (and GADTs): ::

    setTag :: Counter a -> a -> Counter a
    setTag obj t = obj{ tag = t }

The rule for record update is this:

    the types of the updated fields may mention only the universally-quantified
    type variables of the data constructor. For GADTs, the field may mention
    only types that appear as a simple type-variable argument in the
    constructor's result type.

For example: ::

    data T a b where { T1 { f1::a, f2::b, f3::(b,c) } :: T a b } -- c is existential
    upd1 t x = t { f1=x }   -- OK:   upd1 :: T a b -> a' -> T a' b
    upd2 t x = t { f3=x }   -- BAD   (f3's type mentions c, which is
                            --        existentially quantified)

    data G a b where { G1 { g1::a, g2::c } :: G a [c] }
    upd3 g x = g { g1=x }   -- OK:   upd3 :: G a b -> c -> G c b
    upd4 g x = g { g2=x }   -- BAD (f2's type mentions c, which is not a simple
                            --      type-variable argument in G1's result type)

Restrictions
~~~~~~~~~~~~

There are several restrictions on the ways in which existentially-quantified
constructors can be used.

-  When pattern matching, each pattern match introduces a new, distinct,
   type for each existential type variable. These types cannot be
   unified with any other type, nor can they escape from the scope of
   the pattern match. For example, these fragments are incorrect: ::

       f1 (MkFoo a f) = a

   Here, the type bound by ``MkFoo`` "escapes", because ``a`` is the
   result of ``f1``. One way to see why this is wrong is to ask what
   type ``f1`` has: ::

         f1 :: Foo -> a             -- Weird!

   What is this "``a``" in the result type? Clearly we don't mean this: ::

         f1 :: forall a. Foo -> a   -- Wrong!

   The original program is just plain wrong. Here's another sort of
   error ::

         f2 (Baz1 a b) (Baz1 p q) = a==q

   It's ok to say ``a==b`` or ``p==q``, but ``a==q`` is wrong because it
   equates the two distinct types arising from the two ``Baz1``
   constructors.

-  You can't pattern-match on an existentially quantified constructor in
   a ``let`` or ``where`` group of bindings. So this is illegal: ::

         f3 x = a==b where { Baz1 a b = x }

   Instead, use a ``case`` expression: ::

         f3 x = case x of Baz1 a b -> a==b

   In general, you can only pattern-match on an existentially-quantified
   constructor in a ``case`` expression or in the patterns of a function
   definition. The reason for this restriction is really an
   implementation one. Type-checking binding groups is already a
   nightmare without existentials complicating the picture. Also an
   existential pattern binding at the top level of a module doesn't make
   sense, because it's not clear how to prevent the
   existentially-quantified type "escaping". So for now, there's a
   simple-to-state restriction. We'll see how annoying it is.

-  You can't use existential quantification for ``newtype``
   declarations. So this is illegal: ::

         newtype T = forall a. Ord a => MkT a

   Reason: a value of type ``T`` must be represented as a pair of a
   dictionary for ``Ord t`` and a value of type ``t``. That contradicts
   the idea that ``newtype`` should have no concrete representation. You
   can get just the same efficiency and effect by using ``data`` instead
   of ``newtype``. If there is no overloading involved, then there is
   more of a case for allowing an existentially-quantified ``newtype``,
   because the ``data`` version does carry an implementation cost, but
   single-field existentially quantified constructors aren't much use.
   So the simple restriction (no existential stuff on ``newtype``)
   stands, unless there are convincing reasons to change it.

-  You can't use ``deriving`` to define instances of a data type with
   existentially quantified data constructors. Reason: in most cases it
   would not make sense. For example:; ::

       data T = forall a. MkT [a] deriving( Eq )

   To derive ``Eq`` in the standard way we would need to have equality
   between the single component of two ``MkT`` constructors: ::

       instance Eq T where
         (MkT a) == (MkT b) = ???

   But ``a`` and ``b`` have distinct types, and so can't be compared.
   It's just about possible to imagine examples in which the derived
   instance would make sense, but it seems altogether simpler simply to
   prohibit such declarations. Define your own instances!

.. _gadt-style:

Declaring data types with explicit constructor signatures
---------------------------------------------------------

.. ghc-flag:: -XGADTSyntax

    :since: 7.2

    Allow the use of GADT syntax in data type definitions (but not GADTs
    themselves; for this see :ghc-flag:`-XGADTs`)

When the ``GADTSyntax`` extension is enabled, GHC allows you to declare
an algebraic data type by giving the type signatures of constructors
explicitly. For example: ::

      data Maybe a where
          Nothing :: Maybe a
          Just    :: a -> Maybe a

The form is called a "GADT-style declaration" because Generalised
Algebraic Data Types, described in :ref:`gadt`, can only be declared
using this form.

Notice that GADT-style syntax generalises existential types
(:ref:`existential-quantification`). For example, these two declarations
are equivalent: ::

      data Foo = forall a. MkFoo a (a -> Bool)
      data Foo' where { MKFoo :: a -> (a->Bool) -> Foo' }

Any data type that can be declared in standard Haskell 98 syntax can
also be declared using GADT-style syntax. The choice is largely
stylistic, but GADT-style declarations differ in one important respect:
they treat class constraints on the data constructors differently.
Specifically, if the constructor is given a type-class context, that
context is made available by pattern matching. For example: ::

      data Set a where
        MkSet :: Eq a => [a] -> Set a

      makeSet :: Eq a => [a] -> Set a
      makeSet xs = MkSet (nub xs)

      insert :: a -> Set a -> Set a
      insert a (MkSet as) | a `elem` as = MkSet as
                          | otherwise   = MkSet (a:as)

A use of ``MkSet`` as a constructor (e.g. in the definition of
``makeSet``) gives rise to a ``(Eq a)`` constraint, as you would expect.
The new feature is that pattern-matching on ``MkSet`` (as in the
definition of ``insert``) makes *available* an ``(Eq a)`` context. In
implementation terms, the ``MkSet`` constructor has a hidden field that
stores the ``(Eq a)`` dictionary that is passed to ``MkSet``; so when
pattern-matching that dictionary becomes available for the right-hand
side of the match. In the example, the equality dictionary is used to
satisfy the equality constraint generated by the call to ``elem``, so
that the type of ``insert`` itself has no ``Eq`` constraint.

For example, one possible application is to reify dictionaries: ::

       data NumInst a where
         MkNumInst :: Num a => NumInst a

       intInst :: NumInst Int
       intInst = MkNumInst

       plus :: NumInst a -> a -> a -> a
       plus MkNumInst p q = p + q

Here, a value of type ``NumInst a`` is equivalent to an explicit
``(Num a)`` dictionary.

All this applies to constructors declared using the syntax of
:ref:`existential-with-context`. For example, the ``NumInst`` data type
above could equivalently be declared like this: ::

       data NumInst a
          = Num a => MkNumInst (NumInst a)

Notice that, unlike the situation when declaring an existential, there
is no ``forall``, because the ``Num`` constrains the data type's
universally quantified type variable ``a``. A constructor may have both
universal and existential type variables: for example, the following two
declarations are equivalent: ::

       data T1 a
        = forall b. (Num a, Eq b) => MkT1 a b
       data T2 a where
        MkT2 :: (Num a, Eq b) => a -> b -> T2 a

All this behaviour contrasts with Haskell 98's peculiar treatment of
contexts on a data type declaration (Section 4.2.1 of the Haskell 98
Report). In Haskell 98 the definition ::

      data Eq a => Set' a = MkSet' [a]

gives ``MkSet'`` the same type as ``MkSet`` above. But instead of
*making available* an ``(Eq a)`` constraint, pattern-matching on
``MkSet'`` *requires* an ``(Eq a)`` constraint! GHC faithfully
implements this behaviour, odd though it is. But for GADT-style
declarations, GHC's behaviour is much more useful, as well as much more
intuitive.

The rest of this section gives further details about GADT-style data
type declarations.

-  The result type of each data constructor must begin with the type
   constructor being defined. If the result type of all constructors has
   the form ``T a1 ... an``, where ``a1 ... an`` are distinct type
   variables, then the data type is *ordinary*; otherwise is a
   *generalised* data type (:ref:`gadt`).

-  As with other type signatures, you can give a single signature for
   several data constructors. In this example we give a single signature
   for ``T1`` and ``T2``: ::

         data T a where
           T1,T2 :: a -> T a
           T3 :: T a

-  The type signature of each constructor is independent, and is
   implicitly universally quantified as usual. In particular, the type
   variable(s) in the "``data T a where``" header have no scope, and
   different constructors may have different universally-quantified type
   variables: ::

         data T a where        -- The 'a' has no scope
           T1,T2 :: b -> T b   -- Means forall b. b -> T b
           T3 :: T a           -- Means forall a. T a

-  A constructor signature may mention type class constraints, which can
   differ for different constructors. For example, this is fine: ::

         data T a where
           T1 :: Eq b => b -> b -> T b
           T2 :: (Show c, Ix c) => c -> [c] -> T c

   When pattern matching, these constraints are made available to
   discharge constraints in the body of the match. For example: ::

         f :: T a -> String
         f (T1 x y) | x==y      = "yes"
                    | otherwise = "no"
         f (T2 a b)             = show a

   Note that ``f`` is not overloaded; the ``Eq`` constraint arising from
   the use of ``==`` is discharged by the pattern match on ``T1`` and
   similarly the ``Show`` constraint arising from the use of ``show``.

-  Unlike a Haskell-98-style data type declaration, the type variable(s)
   in the "``data Set a where``" header have no scope. Indeed, one can
   write a kind signature instead: ::

         data Set :: * -> * where ...

   or even a mixture of the two: ::

         data Bar a :: (* -> *) -> * where ...

   The type variables (if given) may be explicitly kinded, so we could
   also write the header for ``Foo`` like this: ::

         data Bar a (b :: * -> *) where ...

-  You can use strictness annotations, in the obvious places in the
   constructor type: ::

         data Term a where
             Lit    :: !Int -> Term Int
             If     :: Term Bool -> !(Term a) -> !(Term a) -> Term a
             Pair   :: Term a -> Term b -> Term (a,b)

-  You can use a ``deriving`` clause on a GADT-style data type
   declaration. For example, these two declarations are equivalent ::

         data Maybe1 a where {
             Nothing1 :: Maybe1 a ;
             Just1    :: a -> Maybe1 a
           } deriving( Eq, Ord )

         data Maybe2 a = Nothing2 | Just2 a
              deriving( Eq, Ord )

-  The type signature may have quantified type variables that do not
   appear in the result type: ::

         data Foo where
            MkFoo :: a -> (a->Bool) -> Foo
            Nil   :: Foo

   Here the type variable ``a`` does not appear in the result type of
   either constructor. Although it is universally quantified in the type
   of the constructor, such a type variable is often called
   "existential". Indeed, the above declaration declares precisely the
   same type as the ``data Foo`` in :ref:`existential-quantification`.

   The type may contain a class context too, of course: ::

         data Showable where
           MkShowable :: Show a => a -> Showable

-  You can use record syntax on a GADT-style data type declaration: ::

         data Person where
             Adult :: { name :: String, children :: [Person] } -> Person
             Child :: Show a => { name :: !String, funny :: a } -> Person

   As usual, for every constructor that has a field ``f``, the type of
   field ``f`` must be the same (modulo alpha conversion). The ``Child``
   constructor above shows that the signature may have a context,
   existentially-quantified variables, and strictness annotations, just
   as in the non-record case. (NB: the "type" that follows the
   double-colon is not really a type, because of the record syntax and
   strictness annotations. A "type" of this form can appear only in a
   constructor signature.)

-  Record updates are allowed with GADT-style declarations, only fields
   that have the following property: the type of the field mentions no
   existential type variables.

-  As in the case of existentials declared using the Haskell-98-like
   record syntax (:ref:`existential-records`), record-selector functions
   are generated only for those fields that have well-typed selectors.
   Here is the example of that section, in GADT-style syntax: ::

       data Counter a where
           NewCounter :: { _this    :: self
                         , _inc     :: self -> self
                         , _display :: self -> IO ()
                         , tag      :: a
                         } -> Counter a

   As before, only one selector function is generated here, that for
   ``tag``. Nevertheless, you can still use all the field names in
   pattern matching and record construction.

-  In a GADT-style data type declaration there is no obvious way to
   specify that a data constructor should be infix, which makes a
   difference if you derive ``Show`` for the type. (Data constructors
   declared infix are displayed infix by the derived ``show``.) So GHC
   implements the following design: a data constructor declared in a
   GADT-style data type declaration is displayed infix by ``Show`` iff
   (a) it is an operator symbol, (b) it has two arguments, (c) it has a
   programmer-supplied fixity declaration. For example

   ::

          infix 6 (:--:)
          data T a where
            (:--:) :: Int -> Bool -> T Int

.. _gadt:

Generalised Algebraic Data Types (GADTs)
----------------------------------------

.. ghc-flag:: -XGADTs

    :implies: :ghc-flag:`-XMonoLocalBinds`, :ghc-flag:`-XGADTSyntax`

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
GADTs. The extension is enabled with :ghc-flag:`-XGADTs`. The :ghc-flag:`-XGADTs` flag
also sets :ghc-flag:`-XGADTSyntax` and :ghc-flag:`-XMonoLocalBinds`.

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

.. _record-system-extensions:

Extensions to the record system
===============================

.. _traditional-record-syntax:

Traditional record syntax
-------------------------

.. ghc-flag:: -XNoTraditionalRecordSyntax

    :since: 7.4.1

    Disallow use of record syntax.

Traditional record syntax, such as ``C {f = x}``, is enabled by default.
To disable it, you can use the :ghc-flag:`-XNoTraditionalRecordSyntax` flag.

.. _disambiguate-fields:

Record field disambiguation
---------------------------

.. ghc-flag:: -XDisambiguateRecordFields

    Allow the compiler to automatically choose between identically-named
    record selectors based on type (if the choice is unambiguous).

In record construction and record pattern matching it is entirely
unambiguous which field is referred to, even if there are two different
data types in scope with a common field name. For example:

::

    module M where
      data S = MkS { x :: Int, y :: Bool }

    module Foo where
      import M

      data T = MkT { x :: Int }

      ok1 (MkS { x = n }) = n+1   -- Unambiguous
      ok2 n = MkT { x = n+1 }     -- Unambiguous

      bad1 k = k { x = 3 }        -- Ambiguous
      bad2 k = x k                -- Ambiguous

Even though there are two ``x``'s in scope, it is clear that the ``x``
in the pattern in the definition of ``ok1`` can only mean the field
``x`` from type ``S``. Similarly for the function ``ok2``. However, in
the record update in ``bad1`` and the record selection in ``bad2`` it is
not clear which of the two types is intended.

Haskell 98 regards all four as ambiguous, but with the
:ghc-flag:`-XDisambiguateRecordFields` flag, GHC will accept the former two. The
rules are precisely the same as those for instance declarations in
Haskell 98, where the method names on the left-hand side of the method
bindings in an instance declaration refer unambiguously to the method of
that class (provided they are in scope at all), even if there are other
variables in scope with the same name. This reduces the clutter of
qualified names when you import two records from different modules that
use the same field name.

Some details:

-  Field disambiguation can be combined with punning (see
   :ref:`record-puns`). For example: ::

       module Foo where
         import M
         x=True
         ok3 (MkS { x }) = x+1   -- Uses both disambiguation and punning

-  With :ghc-flag:`-XDisambiguateRecordFields` you can use *unqualified* field
   names even if the corresponding selector is only in scope *qualified*
   For example, assuming the same module ``M`` as in our earlier
   example, this is legal: ::

       module Foo where
         import qualified M    -- Note qualified

         ok4 (M.MkS { x = n }) = n+1   -- Unambiguous

   Since the constructor ``MkS`` is only in scope qualified, you must
   name it ``M.MkS``, but the field ``x`` does not need to be qualified
   even though ``M.x`` is in scope but ``x`` is not (In effect, it is
   qualified by the constructor).

.. _duplicate-record-fields:

Duplicate record fields
-----------------------

.. ghc-flag:: -XDuplicateRecordFields

    :implies: :ghc-flag:`-XDisambiguateRecordFields`
    :since: 8.0.1

    Allow definition of record types with identically-named fields.

Going beyond :ghc-flag:`-XDisambiguateRecordFields` (see :ref:`disambiguate-fields`),
the :ghc-flag:`-XDuplicateRecordFields` extension allows multiple datatypes to be
declared using the same field names in a single module. For example, it allows
this: ::

    module M where
      data S = MkS { x :: Int }
      data T = MkT { x :: Bool }

Uses of fields that are always unambiguous because they mention the constructor,
including construction and pattern-matching, may freely use duplicated field
names. For example, the following are permitted (just as with
:ghc-flag:`-XDisambiguateRecordFields`): ::

    s = MkS { x = 3 }

    f (MkT { x = b }) = b

Field names used as selector functions or in record updates must be unambiguous,
either because there is only one such field in scope, or because a type
signature is supplied, as described in the following sections.

Selector functions
~~~~~~~~~~~~~~~~~~

Fields may be used as selector functions only if they are unambiguous, so this
is still not allowed if both ``S(x)`` and ``T(x)`` are in scope: ::

    bad r = x r

An ambiguous selector may be disambiguated by the type being "pushed down" to
the occurrence of the selector (see :ref:`higher-rank-type-inference` for more
details on what "pushed down" means). For example, the following are permitted: ::

    ok1 = x :: S -> Int

    ok2 :: S -> Int
    ok2 = x

    ok3 = k x -- assuming we already have k :: (S -> Int) -> _

In addition, the datatype that is meant may be given as a type signature on the
argument to the selector: ::

    ok4 s = x (s :: S)

However, we do not infer the type of the argument to determine the datatype, or
have any way of deferring the choice to the constraint solver. Thus the
following is ambiguous: ::

    bad :: S -> Int
    bad s = x s

Even though a field label is duplicated in its defining module, it may be
possible to use the selector unambiguously elsewhere. For example, another
module could import ``S(x)`` but not ``T(x)``, and then use ``x`` unambiguously.

Record updates
~~~~~~~~~~~~~~

In a record update such as ``e { x = 1 }``, if there are multiple ``x`` fields
in scope, then the type of the context must fix which record datatype is
intended, or a type annotation must be supplied. Consider the following
definitions: ::

    data S = MkS { foo :: Int }
    data T = MkT { foo :: Int, bar :: Int }
    data U = MkU { bar :: Int, baz :: Int }

Without :ghc-flag:`-XDuplicateRecordFields`, an update mentioning ``foo`` will always be
ambiguous if all these definitions were in scope. When the extension is enabled,
there are several options for disambiguating updates:

- Check for types that have all the fields being updated. For example: ::

      f x = x { foo = 3, bar = 2 }

  Here ``f`` must be updating ``T`` because neither ``S`` nor ``U`` have both
  fields.

- Use the type being pushed in to the record update, as in the following: ::

      g1 :: T -> T
      g1 x = x { foo = 3 }

      g2 x = x { foo = 3 } :: T

      g3 = k (x { foo = 3 }) -- assuming we already have k :: T -> _

- Use an explicit type signature on the record expression, as in: ::

      h x = (x :: T) { foo = 3 }

The type of the expression being updated will not be inferred, and no
constraint-solving will be performed, so the following will be rejected as
ambiguous: ::

    let x :: T
        x = blah
    in x { foo = 3 }

    \x -> [x { foo = 3 },  blah :: T ]

    \ (x :: T) -> x { foo = 3 }

Import and export of record fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When :ghc-flag:`-XDuplicateRecordFields` is enabled, an ambiguous field must be exported
as part of its datatype, rather than at the top level. For example, the
following is legal: ::

    module M (S(x), T(..)) where
      data S = MkS { x :: Int }
      data T = MkT { x :: Bool }

However, this would not be permitted, because ``x`` is ambiguous: ::

    module M (x) where ...

Similar restrictions apply on import.

.. _record-puns:

Record puns
-----------

.. ghc-flag:: -XNamedFieldPuns

    Allow use of record puns.

Record puns are enabled by the flag :ghc-flag:`-XNamedFieldPuns`.

When using records, it is common to write a pattern that binds a
variable with the same name as a record field, such as: ::

    data C = C {a :: Int}
    f (C {a = a}) = a

Record punning permits the variable name to be elided, so one can simply
write ::

    f (C {a}) = a

to mean the same pattern as above. That is, in a record pattern, the
pattern ``a`` expands into the pattern ``a = a`` for the same name
``a``.

Note that:

-  Record punning can also be used in an expression, writing, for
   example, ::

       let a = 1 in C {a}

   instead of ::

       let a = 1 in C {a = a}

   The expansion is purely syntactic, so the expanded right-hand side
   expression refers to the nearest enclosing variable that is spelled
   the same as the field name.

-  Puns and other patterns can be mixed in the same record: ::

       data C = C {a :: Int, b :: Int}
       f (C {a, b = 4}) = a

-  Puns can be used wherever record patterns occur (e.g. in ``let``
   bindings or at the top-level).

-  A pun on a qualified field name is expanded by stripping off the
   module qualifier. For example: ::

       f (C {M.a}) = a

   means ::

       f (M.C {M.a = a}) = a

   (This is useful if the field selector ``a`` for constructor ``M.C``
   is only in scope in qualified form.)

.. _record-wildcards:

Record wildcards
----------------

.. ghc-flag:: -XRecordWildCards

    :implies: :ghc-flag:`-XDisambiguateRecordFields`.

    Allow the use of wildcards in record construction and pattern matching.

Record wildcards are enabled by the flag :ghc-flag:`-XRecordWildCards`. This
flag implies :ghc-flag:`-XDisambiguateRecordFields`.

For records with many fields, it can be tiresome to write out each field
individually in a record pattern, as in ::

    data C = C {a :: Int, b :: Int, c :: Int, d :: Int}
    f (C {a = 1, b = b, c = c, d = d}) = b + c + d

Record wildcard syntax permits a "``..``" in a record pattern, where
each elided field ``f`` is replaced by the pattern ``f = f``. For
example, the above pattern can be written as ::

    f (C {a = 1, ..}) = b + c + d

More details:

-  Record wildcards in patterns can be mixed with other patterns,
   including puns (:ref:`record-puns`); for example, in a pattern
   ``(C {a = 1, b, ..})``. Additionally, record wildcards can be used
   wherever record patterns occur, including in ``let`` bindings and at
   the top-level. For example, the top-level binding ::

       C {a = 1, ..} = e

   defines ``b``, ``c``, and ``d``.

-  Record wildcards can also be used in an expression, when constructing
   a record. For example, ::

       let {a = 1; b = 2; c = 3; d = 4} in C {..}

   in place of ::

       let {a = 1; b = 2; c = 3; d = 4} in C {a=a, b=b, c=c, d=d}

   The expansion is purely syntactic, so the record wildcard expression
   refers to the nearest enclosing variables that are spelled the same
   as the omitted field names.

-  Record wildcards may *not* be used in record *updates*. For example
   this is illegal: ::

       f r = r { x = 3, .. }

-  For both pattern and expression wildcards, the "``..``" expands to
   the missing *in-scope* record fields. Specifically the expansion of
   "``C {..}``" includes ``f`` if and only if:

   -  ``f`` is a record field of constructor ``C``.

   -  The record field ``f`` is in scope somehow (either qualified or
      unqualified).

   -  In the case of expressions (but not patterns), the variable ``f``
      is in scope unqualified, and is not imported or bound at top level.
      For example, ``f`` can be bound by an enclosing pattern match or
      let/where-binding.  (The motivation here is that it should be
      easy for the reader to figure out what the "``..``" expands to.)

   These rules restrict record wildcards to the situations in which the
   user could have written the expanded version. For example ::

       module M where
         data R = R { a,b,c :: Int }
       module X where
         import M( R(a,c) )
         f b = R { .. }

   The ``R{..}`` expands to ``R{M.a=a}``, omitting ``b`` since the
   record field is not in scope, and omitting ``c`` since the variable
   ``c`` is not in scope (apart from the binding of the record selector
   ``c``, of course).

-  Record wildcards cannot be used (a) in a record update construct, and
   (b) for data constructors that are not declared with record fields.
   For example: ::

       f x = x { v=True, .. }   -- Illegal (a)

       data T = MkT Int Bool
       g = MkT { .. }           -- Illegal (b)
       h (MkT { .. }) = True    -- Illegal (b)


.. _record-field-selector-polymorphism:

Record field selector polymorphism
----------------------------------

The module :base-ref:`GHC.Records.` defines the following: ::

  class HasField (x :: k) r a | x r -> a where
    getField :: r -> a

A ``HasField x r a`` constraint represents the fact that ``x`` is a
field of type ``a`` belonging to a record type ``r``.  The
``getField`` method gives the record selector function.

This allows definitions that are polymorphic over record types with a specified
field.  For example, the following works with any record type that has a field
``name :: String``: ::

  foo :: HasField "name" r String => r -> String
  foo r = reverse (getField @"name" r)

``HasField`` is a magic built-in typeclass (similar to ``Coercible``, for
example).  It is given special treatment by the constraint solver (see
:ref:`solving-hasfield-constraints`).  Users may define their own instances of
``HasField`` also (see :ref:`virtual-record-fields`).

.. _solving-hasfield-constraints:

Solving HasField constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the constraint solver encounters a constraint ``HasField x r a``
where ``r`` is a concrete datatype with a field ``x`` in scope, it
will automatically solve the constraint using the field selector as
the dictionary, unifying ``a`` with the type of the field if
necessary.  This happens irrespective of which extensions are enabled.

For example, if the following datatype is in scope ::

  data Person = Person { name :: String }

the end result is rather like having an instance ::

  instance HasField "name" Person String where
    getField = name

except that this instance is not actually generated anywhere, rather
the constraint is solved directly by the constraint solver.

A field must be in scope for the corresponding ``HasField`` constraint
to be solved.  This retains the existing representation hiding
mechanism, whereby a module may choose not to export a field,
preventing client modules from accessing or updating it directly.

Solving ``HasField`` constraints depends on the field selector functions that
are generated for each datatype definition:

-  If a record field does not have a selector function because its type would allow
   an existential variable to escape, the corresponding ``HasField`` constraint
   will not be solved.  For example, ::

     {-# LANGUAGE ExistentialQuantification #-}
     data Exists t = forall x . MkExists { unExists :: t x }

   does not give rise to a selector ``unExists :: Exists t -> t x`` and we will not
   solve ``HasField "unExists" (Exists t) a`` automatically.

-  If a record field has a polymorphic type (and hence the selector function is
   higher-rank), the corresponding ``HasField`` constraint will not be solved,
   because doing so would violate the functional dependency on ``HasField`` and/or
   require impredicativity.  For example, ::

     {-# LANGUAGE RankNTypes #-}
     data Higher = MkHigher { unHigher :: forall t . t -> t }

   gives rise to a selector ``unHigher :: Higher -> (forall t . t -> t)`` but does
   not lead to solution of the constraint ``HasField "unHigher" Higher a``.

-  A record GADT may have a restricted type for a selector function, which may lead
   to additional unification when solving ``HasField`` constraints.  For example, ::

     {-# LANGUAGE GADTs #-}
     data Gadt t where
       MkGadt :: { unGadt :: Maybe v } -> Gadt [v]

   gives rise to a selector ``unGadt :: Gadt [v] -> Maybe v``, so the solver will reduce
   the constraint ``HasField "unGadt" (Gadt t) b`` by unifying ``t ~ [v]`` and
   ``b ~ Maybe v`` for some fresh metavariable ``v``, rather as if we had an instance ::

     instance (t ~ [v], b ~ Maybe v) => HasField "unGadt" (Gadt t) b

-  If a record type has an old-fashioned datatype context, the ``HasField``
   constraint will be reduced to solving the constraints from the context.
   For example, ::

     {-# LANGUAGE DatatypeContexts #-}
     data Eq a => Silly a = MkSilly { unSilly :: a }

   gives rise to a selector ``unSilly :: Eq a => Silly a -> a``, so
   the solver will reduce the constraint ``HasField "unSilly" (Silly a) b`` to
   ``Eq a`` (and unify ``a`` with ``b``), rather as if we had an instance ::

     instance (Eq a, a ~ b) => HasField "unSilly" (Silly a) b

.. _virtual-record-fields:

Virtual record fields
~~~~~~~~~~~~~~~~~~~~~

Users may define their own instances of ``HasField``, provided they do
not conflict with the built-in constraint solving behaviour.  This
allows "virtual" record fields to be defined for datatypes that do not
otherwise have them.

For example, this instance would make the ``name`` field of ``Person``
accessible using ``#fullname`` as well: ::

  instance HasField "fullname" Person String where
    getField = name

More substantially, an anonymous records library could provide
``HasField`` instances for its anonymous records, and thus be
compatible with the polymorphic record selectors introduced by this
proposal.  For example, something like this makes it possible to use
``getField`` to access ``Record`` values with the appropriate
string in the type-level list of fields: ::

  data Record (xs :: [(k, Type)]) where
    Nil  :: Record '[]
    Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

  instance HasField x (Record ('(x, a) ': xs)) a where
    getField (Cons _ v _) = v
  instance HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
    getField (Cons _ _ r) = getField @x r

  r :: Record '[ '("name", String) ]
  r = Cons Proxy "R" Nil)

  x = getField @"name" r

Since representations such as this can support field labels with kinds other
than ``Symbol``, the ``HasField`` class is poly-kinded (even though the built-in
constraint solving works only at kind ``Symbol``).  In particular, this allows
users to declare scoped field labels such as in the following example: ::

  data PersonFields = Name

  s :: Record '[ '(Name, String) ]
  s = Cons Proxy "S" Nil

  y = getField @Name s

In order to avoid conflicting with the built-in constraint solving,
the following user-defined ``HasField`` instances are prohibited (in
addition to the usual rules, such as the prohibition on type
families appearing in instance heads):

-  ``HasField _ r _`` where ``r`` is a variable;

-  ``HasField _ (T ...) _`` if ``T`` is a data family (because it
   might have fields introduced later, using data instance declarations);

-  ``HasField x (T ...) _`` if ``x`` is a variable and ``T`` has any
   fields at all (but this instance is permitted if ``T`` has no fields);

-  ``HasField "foo" (T ...) _`` if ``T`` has a field ``foo`` (but this
   instance is permitted if it does not).

If a field has a higher-rank or existential type, the corresponding ``HasField``
constraint will not be solved automatically (as described above), but in the
interests of simplicity we do not permit users to define their own instances
either.  If a field is not in scope, the corresponding instance is still
prohibited, to avoid conflicts in downstream modules.


.. _deriving:

Extensions to the "deriving" mechanism
======================================

.. _deriving-inferred:

Inferred context for deriving clauses
-------------------------------------

The Haskell Report is vague about exactly when a ``deriving`` clause is
legal. For example: ::

      data T0 f a = MkT0 a         deriving( Eq )
      data T1 f a = MkT1 (f a)     deriving( Eq )
      data T2 f a = MkT2 (f (f a)) deriving( Eq )

The natural generated ``Eq`` code would result in these instance
declarations: ::

      instance Eq a         => Eq (T0 f a) where ...
      instance Eq (f a)     => Eq (T1 f a) where ...
      instance Eq (f (f a)) => Eq (T2 f a) where ...

The first of these is obviously fine. The second is still fine, although
less obviously. The third is not Haskell 98, and risks losing
termination of instances.

GHC takes a conservative position: it accepts the first two, but not the
third. The rule is this: each constraint in the inferred instance
context must consist only of type variables, with no repetitions.

This rule is applied regardless of flags. If you want a more exotic
context, you can write it yourself, using the `standalone deriving
mechanism <#stand-alone-deriving>`__.

.. _stand-alone-deriving:

Stand-alone deriving declarations
---------------------------------

.. ghc-flag:: -XStandaloneDeriving

    Allow the use of stand-alone ``deriving`` declarations.

GHC allows stand-alone ``deriving`` declarations, enabled by
:ghc-flag:`-XStandaloneDeriving`: ::

      data Foo a = Bar a | Baz String

      deriving instance Eq a => Eq (Foo a)

The syntax is identical to that of an ordinary instance declaration
apart from (a) the keyword ``deriving``, and (b) the absence of the
``where`` part.

However, standalone deriving differs from a ``deriving`` clause in a
number of important ways:

-  The standalone deriving declaration does not need to be in the same
   module as the data type declaration. (But be aware of the dangers of
   orphan instances (:ref:`orphan-modules`).

-  You must supply an explicit context (in the example the context is
   ``(Eq a)``), exactly as you would in an ordinary instance
   declaration. (In contrast, in a ``deriving`` clause attached to a
   data type declaration, the context is inferred.)

-  Unlike a ``deriving`` declaration attached to a ``data`` declaration,
   the instance can be more specific than the data type (assuming you
   also use :ghc-flag:`-XFlexibleInstances`, :ref:`instance-rules`). Consider
   for example ::

         data Foo a = Bar a | Baz String

         deriving instance Eq a => Eq (Foo [a])
         deriving instance Eq a => Eq (Foo (Maybe a))

   This will generate a derived instance for ``(Foo [a])`` and
   ``(Foo (Maybe a))``, but other types such as ``(Foo (Int,Bool))``
   will not be an instance of ``Eq``.

-  Unlike a ``deriving`` declaration attached to a ``data`` declaration,
   GHC does not restrict the form of the data type. Instead, GHC simply
   generates the appropriate boilerplate code for the specified class,
   and typechecks it. If there is a type error, it is your problem. (GHC
   will show you the offending code if it has a type error.)

   The merit of this is that you can derive instances for GADTs and
   other exotic data types, providing only that the boilerplate code
   does indeed typecheck. For example: ::

         data T a where
            T1 :: T Int
            T2 :: T Bool

         deriving instance Show (T a)

   In this example, you cannot say ``... deriving( Show )`` on the data
   type declaration for ``T``, because ``T`` is a GADT, but you *can*
   generate the instance declaration using stand-alone deriving.

   The down-side is that, if the boilerplate code fails to typecheck,
   you will get an error message about that code, which you did not
   write. Whereas, with a ``deriving`` clause the side-conditions are
   necessarily more conservative, but any error message may be more
   comprehensible.

-  Under most circumstances, you cannot use standalone deriving to create an
   instance for a data type whose constructors are not all in scope. This is
   because the derived instance would generate code that uses the constructors
   behind the scenes, which would break abstraction.

   The one exception to this rule is :ghc-flag:`-XDeriveAnyClass`, since
   deriving an instance via :ghc-flag:`-XDeriveAnyClass` simply generates
   an empty instance declaration, which does not require the use of any
   constructors. See the `deriving any class <#derive-any-class>`__ section
   for more details.

In other ways, however, a standalone deriving obeys the same rules as
ordinary deriving:

-  A ``deriving instance`` declaration must obey the same rules
   concerning form and termination as ordinary instance declarations,
   controlled by the same flags; see :ref:`instance-decls`.

-  The stand-alone syntax is generalised for newtypes in exactly the
   same way that ordinary ``deriving`` clauses are generalised
   (:ref:`newtype-deriving`). For example: ::

         newtype Foo a = MkFoo (State Int a)

         deriving instance MonadState Int Foo

   GHC always treats the *last* parameter of the instance (``Foo`` in
   this example) as the type whose instance is being derived.

.. _deriving-extra:

Deriving instances of extra classes (``Data``, etc.)
----------------------------------------------------

.. ghc-flag:: -XDeriveGeneric

    :since: 7.2

    Allow automatic deriving of instances for the ``Generic`` typeclass.

.. ghc-flag:: -XDeriveFunctor

    :since: 6.12

    Allow automatic deriving of instances for the ``Functor`` typeclass.

.. ghc-flag:: -XDeriveFoldable

    :since: 6.12

    Allow automatic deriving of instances for the ``Foldable`` typeclass.

.. ghc-flag:: -XDeriveTraversable

    :since: 6.12

    :implies: :ghc-flag:`-XDeriveFoldable`, :ghc-flag:`-XDeriveFunctor`

    Allow automatic deriving of instances for the ``Traversable`` typeclass.

Haskell 98 allows the programmer to add "``deriving( Eq, Ord )``" to a
data type declaration, to generate a standard instance declaration for
classes specified in the ``deriving`` clause. In Haskell 98, the only
classes that may appear in the ``deriving`` clause are the standard
classes ``Eq``, ``Ord``, ``Enum``, ``Ix``, ``Bounded``, ``Read``, and
``Show``.

GHC extends this list with several more classes that may be
automatically derived:

-  With :ghc-flag:`-XDeriveGeneric`, you can derive instances of the classes
   ``Generic`` and ``Generic1``, defined in ``GHC.Generics``. You can
   use these to define generic functions, as described in
   :ref:`generic-programming`.

-  With :ghc-flag:`-XDeriveFunctor`, you can derive instances of the class
   ``Functor``, defined in ``GHC.Base``. See :ref:`deriving-functor`.

-  With :ghc-flag:`-XDeriveDataTypeable`, you can derive instances of the class
   ``Data``, defined in ``Data.Data``. See :ref:`deriving-data`.

-  With :ghc-flag:`-XDeriveFoldable`, you can derive instances of the class
   ``Foldable``, defined in ``Data.Foldable``. See
   :ref:`deriving-foldable`.

-  With :ghc-flag:`-XDeriveTraversable`, you can derive instances of the class
   ``Traversable``, defined in ``Data.Traversable``. Since the
   ``Traversable`` instance dictates the instances of ``Functor`` and
   ``Foldable``, you'll probably want to derive them too, so
   :ghc-flag:`-XDeriveTraversable` implies :ghc-flag:`-XDeriveFunctor` and
   :ghc-flag:`-XDeriveFoldable`. See :ref:`deriving-traversable`.

-  With :ghc-flag:`-XDeriveLift`, you can derive instances of the class ``Lift``,
   defined in the ``Language.Haskell.TH.Syntax`` module of the
   ``template-haskell`` package. See :ref:`deriving-lift`.

You can also use a standalone deriving declaration instead (see
:ref:`stand-alone-deriving`).

In each case the appropriate class must be in scope before it can be
mentioned in the ``deriving`` clause.

.. _deriving-functor:

Deriving ``Functor`` instances
------------------------------

With :ghc-flag:`-XDeriveFunctor`, one can derive ``Functor`` instances for data types
of kind ``* -> *``. For example, this declaration::

    data Example a = Ex a Char (Example a) (Example Char)
      deriving Functor

would generate the following instance: ::

    instance Functor Example where
      fmap f (Ex a1 a2 a3 a4) = Ex (f a1) a2 (fmap f a3) a4

The basic algorithm for :ghc-flag:`-XDeriveFunctor` walks the arguments of each
constructor of a data type, applying a mapping function depending on the type
of each argument. If a plain type variable is found that is syntactically
equivalent to the last type parameter of the data type (``a`` in the above
example), then we apply the function ``f`` directly to it. If a type is
encountered that is not syntactically equivalent to the last type parameter
*but does mention* the last type parameter somewhere in it, then a recursive
call to ``fmap`` is made. If a type is found which doesn't mention the last
type parameter at all, then it is left alone.

The second of those cases, in which a type is unequal to the type parameter but
does contain the type parameter, can be surprisingly tricky. For example, the
following example compiles::

    newtype Right a = Right (Either Int a) deriving Functor

Modifying the code slightly, however, produces code which will not compile::

    newtype Wrong a = Wrong (Either a Int) deriving Functor

The difference involves the placement of the last type parameter, ``a``. In the
``Right`` case, ``a`` occurs within the type ``Either Int a``, and moreover, it
appears as the last type argument of ``Either``. In the ``Wrong`` case,
however, ``a`` is not the last type argument to ``Either``; rather, ``Int`` is.

This distinction is important because of the way :ghc-flag:`-XDeriveFunctor` works. The
derived ``Functor Right`` instance would be::

    instance Functor Right where
      fmap f (Right a) = Right (fmap f a)

Given a value of type ``Right a``, GHC must produce a value of type
``Right b``. Since the argument to the ``Right`` constructor has type
``Either Int a``, the code recursively calls ``fmap`` on it to produce a value
of type ``Either Int b``, which is used in turn to construct a final value of
type ``Right b``.

The generated code for the ``Functor Wrong`` instance would look exactly the
same, except with ``Wrong`` replacing every occurrence of ``Right``. The
problem is now that ``fmap`` is being applied recursively to a value of type
``Either a Int``. This cannot possibly produce a value of type
``Either b Int``, as ``fmap`` can only change the last type parameter! This
causes the generated code to be ill-typed.

As a general rule, if a data type has a derived ``Functor`` instance and its
last type parameter occurs on the right-hand side of the data declaration, then
either it must (1) occur bare (e.g., ``newtype Id a = a``), or (2) occur as the
last argument of a type constructor (as in ``Right`` above).

There are two exceptions to this rule:

#. Tuple types. When a non-unit tuple is used on the right-hand side of a data
   declaration, :ghc-flag:`-XDeriveFunctor` treats it as a product of distinct types.
   In other words, the following code::

       newtype Triple a = Triple (a, Int, [a]) deriving Functor

   Would result in a generated ``Functor`` instance like so::

       instance Functor Triple where
         fmap f (Triple a) =
           Triple (case a of
                        (a1, a2, a3) -> (f a1, a2, fmap f a3))

   That is, :ghc-flag:`-XDeriveFunctor` pattern-matches its way into tuples and maps
   over each type that constitutes the tuple. The generated code is
   reminiscient of what would be generated from
   ``data Triple a = Triple a Int [a]``, except with extra machinery to handle
   the tuple.

#. Function types. The last type parameter can appear anywhere in a function
   type as long as it occurs in a *covariant* position. To illustrate what this
   means, consider the following three examples::

       newtype CovFun1 a = CovFun1 (Int -> a) deriving Functor
       newtype CovFun2 a = CovFun2 ((a -> Int) -> a) deriving Functor
       newtype CovFun3 a = CovFun3 (((Int -> a) -> Int) -> a) deriving Functor

   All three of these examples would compile without issue. On the other hand::

       newtype ContraFun1 a = ContraFun1 (a -> Int) deriving Functor
       newtype ContraFun2 a = ContraFun2 ((Int -> a) -> Int) deriving Functor
       newtype ContraFun3 a = ContraFun3 (((a -> Int) -> a) -> Int) deriving Functor

   While these examples look similar, none of them would successfully compile.
   This is because all occurrences of the last type parameter ``a`` occur in *contravariant* positions, not covariant ones.

   Intuitively, a covariant type is *produced*, and a contravariant type is
   *consumed*. Most types in Haskell are covariant, but the function type is
   special in that the lefthand side of a function arrow reverses variance. If
   a function type ``a -> b`` appears in a covariant position (e.g.,
   ``CovFun1`` above), then ``a`` is in a contravariant position and ``b`` is
   in a covariant position. Similarly, if ``a -> b`` appears in a contravariant
   position (e.g., ``CovFun2`` above), then ``a`` is in ``a`` covariant
   position and ``b`` is in a contravariant position.

   To see why a data type with a contravariant occurrence of its last type
   parameter cannot have a derived ``Functor`` instance, let's suppose that a
   ``Functor ContraFun1`` instance exists. The implementation would look
   something like this::

       instance Functor ContraFun1 where
         fmap f (ContraFun g) = ContraFun (\x -> _)

   We have ``f :: a -> b``, ``g :: a -> Int``, and ``x :: b``. Using these, we
   must somehow fill in the hole (denoted with an underscore) with a value of
   type ``Int``. What are our options?

   We could try applying ``g`` to ``x``. This won't work though, as ``g``
   expects an argument of type ``a``, and ``x :: b``. Even worse, we can't turn
   ``x`` into something of type ``a``, since ``f`` also needs an argument of
   type ``a``! In short, there's no good way to make this work.

   On the other hand, a derived ``Functor`` instances for the ``CovFun``\ s are
   within the realm of possibility::

       instance Functor CovFun1 where
         fmap f (CovFun1 g) = CovFun1 (\x -> f (g x))

       instance Functor CovFun2 where
         fmap f (CovFun2 g) = CovFun2 (\h -> f (g (\x -> h (f x))))

       instance Functor CovFun3 where
         fmap f (CovFun3 g) = CovFun3 (\h -> f (g (\k -> h (\x -> f (k x)))))

There are some other scenarios in which a derived ``Functor`` instance will
fail to compile:

#. A data type has no type parameters (e.g., ``data Nothing = Nothing``).

#. A data type's last type variable is used in a :ghc-flag:`-XDatatypeContexts`
   constraint (e.g., ``data Ord a => O a = O a``).

#. A data type's last type variable is used in an
   :ghc-flag:`-XExistentialQuantification` constraint, or is refined in a GADT. For
   example, ::

       data T a b where
           T4 :: Ord b => b -> T a b
           T5 :: b -> T b b
           T6 :: T a (b,b)

       deriving instance Functor (T a)

   would not compile successfully due to the way in which ``b`` is constrained.

When the last type parameter has a phantom role (see :ref:`roles`), the derived
``Functor`` instance will not be produced using the usual algorithm. Instead,
the entire value will be coerced. ::

    data Phantom a = Z | S (Phantom a) deriving Functor

will produce the following instance: ::

    instance Functor Phantom where
      fmap _ = coerce

When a type has no constructors, the derived ``Functor`` instance will
simply force the (bottom) value of the argument using
:ghc-flag:`-XEmptyCase`. ::

    data V a deriving Functor
    type role V nominal

will produce

    instance Functor V where
      fmap _ z = case z of

.. _deriving-foldable:

Deriving ``Foldable`` instances
-------------------------------

With :ghc-flag:`-XDeriveFoldable`, one can derive ``Foldable`` instances for data types
of kind ``* -> *``. For example, this declaration::

    data Example a = Ex a Char (Example a) (Example Char)
      deriving Foldable

would generate the following instance::

    instance Foldable Example where
      foldr f z (Ex a1 a2 a3 a4) = f a1 (foldr f z a3)
      foldMap f (Ex a1 a2 a3 a4) = mappend (f a1) (foldMap f a3)

The algorithm for :ghc-flag:`-XDeriveFoldable` is adapted from the
:ghc-flag:`-XDeriveFunctor` algorithm, but it generates definitions for
``foldMap``, ``foldr``, and ``null`` instead of ``fmap``. In addition,
:ghc-flag:`-XDeriveFoldable` filters out all constructor arguments on the RHS
expression whose types do not mention the last type parameter, since those
arguments do not need to be folded over.

When the type parameter has a phantom role (see :ref:`roles`),
:ghc-flag:`-XDeriveFoldable` derives a trivial instance. For example, this
declaration: ::

    data Phantom a = Z | S (Phantom a)

will generate the following instance. ::

    instance Foldable Phantom where
      foldMap _ _ = mempty

Similarly, when the type has no constructors, :ghc-flag:`-XDeriveFoldable` will
derive a trivial instance: ::

    data V a deriving Foldable
    type role V nominal

will generate the following. ::

    instance Foldable V where
      foldMap _ _ = mempty

Here are the differences between the generated code for ``Functor`` and
``Foldable``:

#. When a bare type variable ``a`` is encountered, :ghc-flag:`-XDeriveFunctor`
would generate ``f a`` for an ``fmap`` definition. :ghc-flag:`-XDeriveFoldable`
would generate ``f a z`` for ``foldr``, ``f a`` for ``foldMap``, and ``False``
for ``null``.

#. When a type that is not syntactically equivalent to ``a``, but which does
   contain ``a``, is encountered, :ghc-flag:`-XDeriveFunctor` recursively calls
   ``fmap`` on it. Similarly, :ghc-flag:`-XDeriveFoldable` would recursively call
   ``foldr`` and ``foldMap``. Depending on the context, ``null`` may recursively
   call ``null`` or ``all null``. For example, given ::

       data F a = F (P a)
       data G a = G (P (a, Int))
       data H a = H (P (Q a))

   ``Foldable`` deriving will produce ::

       null (F x) = null x
       null (G x) = null x
       null (H x) = all null x

#. :ghc-flag:`-XDeriveFunctor` puts everything back together again at the end by
   invoking the constructor. :ghc-flag:`-XDeriveFoldable`, however, builds up a value
   of some type. For ``foldr``, this is accomplished by chaining applications
   of ``f`` and recursive ``foldr`` calls on the state value ``z``. For
   ``foldMap``, this happens by combining all values with ``mappend``. For ``null``,
   the values are usually combined with ``&&``. However, if any of the values is
   known to be ``False``, all the rest will be dropped. For example, ::

       data SnocList a = Nil | Snoc (SnocList a) a

   will not produce ::

       null (Snoc xs _) = null xs && False

   (which would walk the whole list), but rather ::

       null (Snoc _ _) = False

There are some other differences regarding what data types can have derived
``Foldable`` instances:

#. Data types containing function types on the right-hand side cannot have
   derived ``Foldable`` instances.

#. ``Foldable`` instances can be derived for data types in which the last type
   parameter is existentially constrained or refined in a GADT. For example,
   this data type::

       data E a where
           E1 :: (a ~ Int) => a   -> E a
           E2 ::              Int -> E Int
           E3 :: (a ~ Int) => a   -> E Int
           E4 :: (a ~ Int) => Int -> E a

       deriving instance Foldable E

   would have the following generated ``Foldable`` instance::

       instance Foldable E where
           foldr f z (E1 e) = f e z
           foldr f z (E2 e) = z
           foldr f z (E3 e) = z
           foldr f z (E4 e) = z

           foldMap f (E1 e) = f e
           foldMap f (E2 e) = mempty
           foldMap f (E3 e) = mempty
           foldMap f (E4 e) = mempty

   Notice how every constructor of ``E`` utilizes some sort of existential
   quantification, but only the argument of ``E1`` is actually "folded over".
   This is because we make a deliberate choice to only fold over universally
   polymorphic types that are syntactically equivalent to the last type
   parameter. In particular:

  -  We don't fold over the arguments of ``E1`` or ``E4`` beacause even though
     ``(a ~ Int)``, ``Int`` is not syntactically equivalent to ``a``.

  -  We don't fold over the argument of ``E3`` because ``a`` is not universally
     polymorphic. The ``a`` in ``E3`` is (implicitly) existentially quantified,
     so it is not the same as the last type parameter of ``E``.

.. _deriving-traversable:

Deriving ``Traversable`` instances
----------------------------------

With :ghc-flag:`-XDeriveTraversable`, one can derive ``Traversable`` instances for data
types of kind ``* -> *``. For example, this declaration::

    data Example a = Ex a Char (Example a) (Example Char)
      deriving (Functor, Foldable, Traversable)

would generate the following ``Traversable`` instance::

    instance Traversable Example where
      traverse f (Ex a1 a2 a3 a4)
        = fmap (\b1 b3 -> Ex b1 a2 b3 a4) (f a1) <*> traverse f a3

The algorithm for :ghc-flag:`-XDeriveTraversable` is adapted from the
:ghc-flag:`-XDeriveFunctor` algorithm, but it generates a definition for ``traverse``
instead of ``fmap``. In addition, :ghc-flag:`-XDeriveTraversable` filters out
all constructor arguments on the RHS expression whose types do not mention the
last type parameter, since those arguments do not produce any effects in a
traversal.

When the type parameter has a phantom role (see :ref:`roles`),
:ghc-flag:`-XDeriveTraversable` coerces its argument. For example, this
declaration::

    data Phantom a = Z | S (Phantom a) deriving Traversable

will generate the following instance::

    instance Traversable Phantom where
      traverse _ z = pure (coerce z)

When the type has no constructors, :ghc-flag:`-XDeriveTraversable` will
derive the laziest instance it can. ::

    data V a deriving Traversable
    type role V nominal

will generate the following, using :ghc-flag:`-XEmptyCase`: ::

    instance Traversable V where
      traverse _ z = pure (case z of)

Here are the differences between the generated code in each
extension:

#. When a bare type variable ``a`` is encountered, both :ghc-flag:`-XDeriveFunctor` and
   :ghc-flag:`-XDeriveTraversable` would generate ``f a`` for an ``fmap`` and
   ``traverse`` definition, respectively.

#. When a type that is not syntactically equivalent to ``a``, but which does
   contain ``a``, is encountered, :ghc-flag:`-XDeriveFunctor` recursively calls
   ``fmap`` on it. Similarly, :ghc-flag:`-XDeriveTraversable` would recursively call
   ``traverse``.

#. :ghc-flag:`-XDeriveFunctor` puts everything back together again at the end by
   invoking the constructor. :ghc-flag:`-XDeriveTraversable` does something similar,
   but it works in an ``Applicative`` context by chaining everything together
   with ``(<*>)``.

Unlike :ghc-flag:`-XDeriveFunctor`, :ghc-flag:`-XDeriveTraversable` cannot be used on data
types containing a function type on the right-hand side.

For a full specification of the algorithms used in :ghc-flag:`-XDeriveFunctor`,
:ghc-flag:`-XDeriveFoldable`, and :ghc-flag:`-XDeriveTraversable`, see
:ghc-wiki:`this wiki page <Commentary/Compiler/DeriveFunctor>`.

.. _deriving-data:

Deriving ``Data`` instances
-------------------------------

.. ghc-flag:: -XDeriveDataTypeable

    Enable automatic deriving of instances for the ``Data`` typeclass

.. _deriving-typeable:

Deriving ``Typeable`` instances
-------------------------------

The class ``Typeable`` is very special:

-  ``Typeable`` is kind-polymorphic (see :ref:`kind-polymorphism`).

-  GHC has a custom solver for discharging constraints that involve
   class ``Typeable``, and handwritten instances are forbidden. This
   ensures that the programmer cannot subvert the type system by writing
   bogus instances.

-  Derived instances of ``Typeable`` may be declared if the
   :ghc-flag:`-XDeriveDataTypeable` extension is enabled, but they are ignored,
   and they may be reported as an error in a later version of the compiler.

-  The rules for solving \`Typeable\` constraints are as follows:

   -  A concrete type constructor applied to some types. ::

          instance (Typeable t1, .., Typeable t_n) =>
            Typeable (T t1 .. t_n)

      This rule works for any concrete type constructor, including type
      constructors with polymorphic kinds. The only restriction is that
      if the type constructor has a polymorphic kind, then it has to be
      applied to all of its kinds parameters, and these kinds need to be
      concrete (i.e., they cannot mention kind variables).

   -  ::

          A type variable applied to some types.
          instance (Typeable f, Typeable t1, .., Typeable t_n) =>
            Typeable (f t1 .. t_n)

   -  ::

          A concrete type literal.
          instance Typeable 0       -- Type natural literals
          instance Typeable "Hello" -- Type-level symbols

.. _deriving-lift:

Deriving ``Lift`` instances
---------------------------

.. ghc-flag:: -XDeriveLift

    :since: 8.0.1

    Enable automatic deriving of instances for the ``Lift`` typeclass for
    Template Haskell.

The class ``Lift``, unlike other derivable classes, lives in
``template-haskell`` instead of ``base``. Having a data type be an instance of
``Lift`` permits its values to be promoted to Template Haskell expressions (of
type ``ExpQ``), which can then be spliced into Haskell source code.

Here is an example of how one can derive ``Lift``:

::

    {-# LANGUAGE DeriveLift #-}
    module Bar where

    import Language.Haskell.TH.Syntax

    data Foo a = Foo a | a :^: a deriving Lift

    {-
    instance (Lift a) => Lift (Foo a) where
        lift (Foo a)
        = appE
            (conE
                (mkNameG_d "package-name" "Bar" "Foo"))
            (lift a)
        lift (u :^: v)
        = infixApp
            (lift u)
            (conE
                (mkNameG_d "package-name" "Bar" ":^:"))
            (lift v)
    -}

    -----
    {-# LANGUAGE TemplateHaskell #-}
    module Baz where

    import Bar
    import Language.Haskell.TH.Lift

    foo :: Foo String
    foo = $(lift $ Foo "foo")

    fooExp :: Lift a => Foo a -> Q Exp
    fooExp f = [| f |]

:ghc-flag:`-XDeriveLift` also works for certain unboxed types (``Addr#``, ``Char#``,
``Double#``, ``Float#``, ``Int#``, and ``Word#``):

::

    {-# LANGUAGE DeriveLift, MagicHash #-}
    module Unboxed where

    import GHC.Exts
    import Language.Haskell.TH.Syntax

    data IntHash = IntHash Int# deriving Lift

    {-
    instance Lift IntHash where
        lift (IntHash i)
        = appE
            (conE
                (mkNameG_d "package-name" "Unboxed" "IntHash"))
            (litE
                (intPrimL (toInteger (I# i))))
    -}


.. _newtype-deriving:

Generalised derived instances for newtypes
------------------------------------------

.. ghc-flag:: -XGeneralisedNewtypeDeriving
              -XGeneralizedNewtypeDeriving

    Enable GHC's cunning generalised deriving mechanism for ``newtype``\s

When you define an abstract type using ``newtype``, you may want the new
type to inherit some instances from its representation. In Haskell 98,
you can inherit instances of ``Eq``, ``Ord``, ``Enum`` and ``Bounded``
by deriving them, but for any other classes you have to write an
explicit instance declaration. For example, if you define ::

      newtype Dollars = Dollars Int

and you want to use arithmetic on ``Dollars``, you have to explicitly
define an instance of ``Num``: ::

      instance Num Dollars where
        Dollars a + Dollars b = Dollars (a+b)
        ...

All the instance does is apply and remove the ``newtype`` constructor.
It is particularly galling that, since the constructor doesn't appear at
run-time, this instance declaration defines a dictionary which is
*wholly equivalent* to the ``Int`` dictionary, only slower!

.. _generalized-newtype-deriving:

Generalising the deriving clause
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC now permits such instances to be derived instead, using the flag
:ghc-flag:`-XGeneralizedNewtypeDeriving`, so one can write ::

      newtype Dollars = Dollars { getDollars :: Int } deriving (Eq,Show,Num)

and the implementation uses the *same* ``Num`` dictionary for
``Dollars`` as for ``Int``. In other words, GHC will generate something that
resembles the following code ::

      instance Num Int => Num Dollars

and then attempt to simplify the ``Num Int`` context as much as possible.
GHC knows that there is a ``Num Int`` instance in scope, so it is able to
discharge the ``Num Int`` constraint, leaving the code that GHC actually
generates ::

      instance Num Dollars

One can think of this instance being implemented with the same code as the
``Num Int`` instance, but with ``Dollars`` and ``getDollars`` added wherever
necessary in order to make it typecheck. (In practice, GHC uses a somewhat
different approach to code generation. See the :ref:`precise-gnd-specification`
section below for more details.)

We can also derive instances of constructor classes in a similar way.
For example, suppose we have implemented state and failure monad
transformers, such that ::

      instance Monad m => Monad (State s m)
      instance Monad m => Monad (Failure m)

In Haskell 98, we can define a parsing monad by ::

      type Parser tok m a = State [tok] (Failure m) a

which is automatically a monad thanks to the instance declarations
above. With the extension, we can make the parser type abstract, without
needing to write an instance of class ``Monad``, via ::

      newtype Parser tok m a = Parser (State [tok] (Failure m) a)
                             deriving Monad

In this case the derived instance declaration is of the form ::

      instance Monad (State [tok] (Failure m)) => Monad (Parser tok m)

Notice that, since ``Monad`` is a constructor class, the instance is a
*partial application* of the new type, not the entire left hand side. We
can imagine that the type declaration is "eta-converted" to generate the
context of the instance declaration.

We can even derive instances of multi-parameter classes, provided the
newtype is the last class parameter. In this case, a "partial
application" of the class appears in the ``deriving`` clause. For
example, given the class ::

      class StateMonad s m | m -> s where ...
      instance Monad m => StateMonad s (State s m) where ...

then we can derive an instance of ``StateMonad`` for ``Parser`` by ::

      newtype Parser tok m a = Parser (State [tok] (Failure m) a)
                             deriving (Monad, StateMonad [tok])

The derived instance is obtained by completing the application of the
class to the new type: ::

      instance StateMonad [tok] (State [tok] (Failure m)) =>
               StateMonad [tok] (Parser tok m)

As a result of this extension, all derived instances in newtype
declarations are treated uniformly (and implemented just by reusing the
dictionary for the representation type), *except* ``Show`` and ``Read``,
which really behave differently for the newtype and its representation.

.. _precise-gnd-specification:

A more precise specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A derived instance is derived only for declarations of these forms
(after expansion of any type synonyms) ::

      newtype T v1..vn                   = MkT (t vk+1..vn) deriving (C t1..tj)
      newtype instance T s1..sk vk+1..vn = MkT (t vk+1..vn) deriving (C t1..tj)

where

-  ``v1..vn`` are type variables, and ``t``, ``s1..sk``, ``t1..tj`` are
   types.

-  The ``(C t1..tj)`` is a partial applications of the class ``C``,
   where the arity of ``C`` is exactly ``j+1``. That is, ``C`` lacks
   exactly one type argument.

-  ``k`` is chosen so that ``C t1..tj (T v1...vk)`` is well-kinded. (Or,
   in the case of a ``data instance``, so that ``C t1..tj (T s1..sk)``
   is well kinded.)

-  The type ``t`` is an arbitrary type.

-  The type variables ``vk+1...vn`` do not occur in the types ``t``,
   ``s1..sk``, or ``t1..tj``.

-  ``C`` is not ``Read``, ``Show``, ``Typeable``, or ``Data``. These
   classes should not "look through" the type or its constructor. You
   can still derive these classes for a newtype, but it happens in the
   usual way, not via this new mechanism.

-  It is safe to coerce each of the methods of ``C``. That is, the
   missing last argument to ``C`` is not used at a nominal role in any
   of the ``C``'s methods. (See :ref:`roles`.)

- ``C`` is allowed to have associated type families, provided they meet the
  requirements laid out in the section on :ref:`GND and associated types
  <gnd-and-associated-types>`.

Then the derived instance declaration is of the form ::

      instance C t1..tj t => C t1..tj (T v1...vk)

Note that if ``C`` does not contain any class methods, the instance context
is wholly unnecessary, and as such GHC will instead generate: ::

      instance C t1..tj (T v1..vk)

As an example which does *not* work, consider ::

      newtype NonMonad m s = NonMonad (State s m s) deriving Monad

Here we cannot derive the instance ::

      instance Monad (State s m) => Monad (NonMonad m)

because the type variable ``s`` occurs in ``State s m``, and so cannot
be "eta-converted" away. It is a good thing that this ``deriving``
clause is rejected, because ``NonMonad m`` is not, in fact, a monad ---
for the same reason. Try defining ``>>=`` with the correct type: you
won't be able to.

Notice also that the *order* of class parameters becomes important,
since we can only derive instances for the last one. If the
``StateMonad`` class above were instead defined as ::

      class StateMonad m s | m -> s where ...

then we would not have been able to derive an instance for the
``Parser`` type above. We hypothesise that multi-parameter classes
usually have one "main" parameter for which deriving new instances is
most interesting.

Lastly, all of this applies only for classes other than ``Read``,
``Show``, ``Typeable``, and ``Data``, for which the stock derivation
applies (section 4.3.3. of the Haskell Report). (For the standard
classes ``Eq``, ``Ord``, ``Ix``, and ``Bounded`` it is immaterial
whether the stock method is used or the one described here.)

.. _gnd-and-associated-types:

Associated type families
~~~~~~~~~~~~~~~~~~~~~~~~

:ghc-flag:`-XGeneralizedNewtypeDeriving` also works for some type classes with
associated type families. Here is an example: ::

      class HasRing a where
        type Ring a

      newtype L1Norm a = L1Norm a
        deriving HasRing

The derived ``HasRing`` instance would look like ::

      instance HasRing (L1Norm a) where
        type Ring (L1Norm a) = Ring a

To be precise, if the class being derived is of the form ::

      class C c_1 c_2 ... c_m where
        type T1 t1_1 t1_2 ... t1_n
        ...
        type Tk tk_1 tk_2 ... tk_p

and the newtype is of the form ::

      newtype N n_1 n_2 ... n_q = MkN <rep-type>

then you can derive a ``C c_1 c_2 ... c_(m-1)`` instance for
``N n_1 n_2 ... n_q``, provided that:

- The type parameter ``c_m`` occurs once in each of the type variables of
  ``T1`` through ``Tk``. Imagine a class where this condition didn't hold.
  For example: ::

      class Bad a b where
        type B a

      instance Bad Int a where
        type B Int = Char

      newtype Foo a = Foo a
        deriving (Bad Int)

  For the derived ``Bad Int`` instance, GHC would need to generate something
  like this: ::

      instance Bad Int (Foo a) where
        type B Int = B ???

  Now we're stuck, since we have no way to refer to ``a`` on the right-hand
  side of the ``B`` family instance, so this instance doesn't really make sense
  in a :ghc-flag:`-XGeneralizedNewtypeDeriving` setting.

- ``C`` does not have any associated data families (only type families). To
  see why data families are forbidden, imagine the following scenario: ::

      class Ex a where
        data D a

      instance Ex Int where
        data D Int = DInt Bool

      newtype Age = MkAge Int deriving Ex

  For the derived ``Ex`` instance, GHC would need to generate something like
  this: ::

      instance Ex Age where
        data D Age = ???

  But it is not clear what GHC would fill in for ``???``, as each data family
  instance must generate fresh data constructors.

If both of these conditions are met, GHC will generate this instance: ::

      instance C c_1 c_2 ... c_(m-1) <rep-type> =>
               C c_1 c_2 ... c_(m-1) (N n_1 n_2 ... n_q) where
        type T1 t1_1 t1_2 ... (N n_1 n_2 ... n_q) ... t1_n
           = T1 t1_1 t1_2 ... <rep-type>          ... t1_n
        ...
        type Tk tk_1 tk_2 ... (N n_1 n_2 ... n_q) ... tk_p
           = Tk tk_1 tk_2 ... <rep-type>          ... tk_p

Again, if ``C`` contains no class methods, the instance context will be
redundant, so GHC will instead generate
``instance C c_1 c_2 ... c_(m-1) (N n_1 n_2 ... n_q)``.

Beware that in some cases, you may need to enable the
:ghc-flag:`-XUndecidableInstances` extension in order to use this feature.
Here's a pathological case that illustrates why this might happen: ::

      class C a where
        type T a

      newtype Loop = MkLoop Loop
        deriving C

This will generate the derived instance: ::

      instance C Loop where
        type T Loop = T Loop

Here, it is evident that attempting to use the type ``T Loop`` will throw the
typechecker into an infinite loop, as its definition recurses endlessly. In
other cases, you might need to enable :ghc-flag:`-XUndecidableInstances` even
if the generated code won't put the typechecker into a loop. For example: ::

      instance C Int where
        type C Int = Int

      newtype MyInt = MyInt Int
        deriving C

This will generate the derived instance: ::

      instance C MyInt where
        type T MyInt = T Int

Although typechecking ``T MyInt`` will terminate, GHC's termination checker
isn't sophisticated enough to determine this, so you'll need to enable
:ghc-flag:`-XUndecidableInstances` in order to use this derived instance. If
you do go down this route, make sure you can convince yourself that all of
the type family instances you're deriving will eventually terminate if used!

.. _derive-any-class:

Deriving any other class
------------------------

.. ghc-flag:: -XDeriveAnyClass

    :since: 7.10.1

    Allow use of any typeclass in ``deriving`` clauses.

With :ghc-flag:`-XDeriveAnyClass` you can derive any other class. The compiler
will simply generate an instance declaration with no explicitly-defined
methods.
This is
mostly useful in classes whose `minimal set <#minimal-pragma>`__ is
empty, and especially when writing
`generic functions <#generic-programming>`__.

As an example, consider a simple pretty-printer class ``SPretty``, which outputs
pretty strings: ::

    {-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}

    class SPretty a where
      sPpr :: a -> String
      default sPpr :: Show a => a -> String
      sPpr = show

If a user does not provide a manual implementation for ``sPpr``, then it will
default to ``show``. Now we can leverage the :ghc-flag:`-XDeriveAnyClass` extension to
easily implement a ``SPretty`` instance for a new data type: ::

    data Foo = Foo deriving (Show, SPretty)

The above code is equivalent to: ::

    data Foo = Foo deriving Show
    instance SPretty Foo

That is, an ``SPretty Foo`` instance will be created with empty implementations
for all methods. Since we are using :ghc-flag:`-XDefaultSignatures` in this example, a
default implementation of ``sPpr`` is filled in automatically.

Note the following details

- In case you try to derive some
  class on a newtype, and :ghc-flag:`-XGeneralizedNewtypeDeriving` is also on,
  :ghc-flag:`-XDeriveAnyClass` takes precedence.

- The instance context is determined by the type signatures of the derived
  class's methods. For instance, if the class is: ::

    class Foo a where
      bar :: a -> String
      default bar :: Show a => a -> String
      bar = show

      baz :: a -> a -> Bool
      default baz :: Ord a => a -> a -> Bool
      baz x y = compare x y == EQ

  And you attempt to derive it using :ghc-flag:`-XDeriveAnyClass`: ::

    instance Eq   a => Eq   (Option a) where ...
    instance Ord  a => Ord  (Option a) where ...
    instance Show a => Show (Option a) where ...

    data Option a = None | Some a deriving Foo

  Then the derived ``Foo`` instance will be: ::

    instance (Show a, Ord a) => Foo (Option a)

  Since the default type signatures for ``bar`` and ``baz`` require ``Show a``
  and ``Ord a`` constraints, respectively.

  Constraints on the non-default type signatures can play a role in inferring
  the instance context as well. For example, if you have this class: ::

    class HigherEq f where
      (==#) :: f a -> f a -> Bool
      default (==#) :: Eq (f a) => f a -> f a -> Bool
      x ==# y = (x == y)

  And you tried to derive an instance for it: ::

    instance Eq a => Eq (Option a) where ...
    data Option a = None | Some a deriving HigherEq

  Then it will fail with an error to the effect of: ::

    No instance for (Eq a)
        arising from the 'deriving' clause of a data type declaration

  That is because we require an ``Eq (Option a)`` instance from the default
  type signature for ``(==#)``, which in turn requires an ``Eq a`` instance,
  which we don't have in scope. But if you tweak the definition of
  ``HigherEq`` slightly: ::

    class HigherEq f where
      (==#) :: Eq a => f a -> f a -> Bool
      default (==#) :: Eq (f a) => f a -> f a -> Bool
      x ==# y = (x == y)

  Then it becomes possible to derive a ``HigherEq Option`` instance. Note that
  the only difference is that now the non-default type signature for ``(==#)``
  brings in an ``Eq a`` constraint. Constraints from non-default type
  signatures never appear in the derived instance context itself, but they can
  be used to discharge obligations that are demanded by the default type
  signatures. In the example above, the default type signature demanded an
  ``Eq a`` instance, and the non-default signature was able to satisfy that
  request, so the derived instance is simply: ::

    instance HigherEq Option

- :ghc-flag:`-XDeriveAnyClass` can be used with partially applied classes,
  such as ::

    data T a = MKT a deriving( D Int )

  which generates ::

    instance D Int a => D Int (T a) where {}

- :ghc-flag:`-XDeriveAnyClass` can be used to fill in default instances for
  associated type families: ::

    {-# LANGUAGE DeriveAnyClass, TypeFamilies #-}

    class Sizable a where
      type Size a
      type Size a = Int

    data Bar = Bar deriving Sizable

    doubleBarSize :: Size Bar -> Size Bar
    doubleBarSize s = 2*s

  The ``deriving( Sizable )`` is equivalent to saying ::

    instance Sizeable Bar where {}

  and then the normal rules for filling in associated types from the
  default will apply, making ``Size Bar`` equal to ``Int``.

.. _deriving-strategies:

Deriving strategies
-------------------

.. ghc-flag:: -XDerivingStrategies

    Allow multiple ``deriving``, each optionally qualified with a *strategy*.

In most scenarios, every ``deriving`` statement generates a typeclass instance
in an unambiguous fashion. There is a corner case, however, where
simultaneously enabling both the :ghc-flag:`-XGeneralizedNewtypeDeriving` and
:ghc-flag:`-XDeriveAnyClass` extensions can make deriving become ambiguous.
Consider the following example ::

    {-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}
    newtype Foo = MkFoo Bar deriving C

One could either pick the ``DeriveAnyClass`` approach to deriving ``C`` or the
``GeneralizedNewtypeDeriving`` approach to deriving ``C``, both of which would
be equally as valid. GHC defaults to favoring ``DeriveAnyClass`` in such a
dispute, but this is not a satisfying solution, since that leaves users unable
to use both language extensions in a single module.

To make this more robust, GHC has a notion of deriving strategies, which allow
the user to explicitly request which approach to use when deriving an instance.
To enable this feature, one must enable the :ghc-flag:`-XDerivingStrategies`
language extension. A deriving strategy can be specified in a deriving
clause ::

    newtype Foo = MkFoo Bar
      deriving newtype C

Or in a standalone deriving declaration ::

    deriving anyclass instance C Foo

:ghc-flag:`-XDerivingStrategies` also allows the use of multiple deriving
clauses per data declaration so that a user can derive some instance with
one deriving strategy and other instances with another deriving strategy.
For example ::

    newtype Baz = Baz Quux
      deriving          (Eq, Ord)
      deriving stock    (Read, Show)
      deriving newtype  (Num, Floating)
      deriving anyclass C

Currently, the deriving strategies are:

- ``stock``: Have GHC implement a "standard" instance for a data type,
  if possible (e.g., ``Eq``, ``Ord``, ``Generic``, ``Data``, ``Functor``, etc.)

- ``anyclass``: Use :ghc-flag:`-XDeriveAnyClass`

- ``newtype``: Use :ghc-flag:`-XGeneralizedNewtypeDeriving`

If an explicit deriving strategy is not given, GHC has an algorithm for
determining how it will actually derive an instance. For brevity, the algorithm
is omitted here. You can read the full algorithm on the
:ghc-wiki:`GHC Wiki <Commentary/Compiler/DerivingStrategies>`.

.. _pattern-synonyms:

Pattern synonyms
================

.. ghc-flag:: -XPatternSynonyms

    :since: 7.8.1

    Allow the definition of pattern synonyms.

Pattern synonyms are enabled by the flag :ghc-flag:`-XPatternSynonyms`, which is
required for defining them, but *not* for using them. More information and
examples of view patterns can be found on the `Wiki page <PatternSynonyms>`.

Pattern synonyms enable giving names to parametrized pattern schemes.
They can also be thought of as abstract constructors that don't have a
bearing on data representation. For example, in a programming language
implementation, we might represent types of the language as follows: ::

    data Type = App String [Type]

Here are some examples of using said representation. Consider a few
types of the ``Type`` universe encoded like this: ::

      App "->" [t1, t2]          -- t1 -> t2
      App "Int" []               -- Int
      App "Maybe" [App "Int" []] -- Maybe Int

This representation is very generic in that no types are given special
treatment. However, some functions might need to handle some known types
specially, for example the following two functions collect all argument
types of (nested) arrow types, and recognize the ``Int`` type,
respectively: ::

      collectArgs :: Type -> [Type]
      collectArgs (App "->" [t1, t2]) = t1 : collectArgs t2
      collectArgs _                   = []

      isInt :: Type -> Bool
      isInt (App "Int" []) = True
      isInt _              = False

Matching on ``App`` directly is both hard to read and error prone to
write. And the situation is even worse when the matching is nested: ::

      isIntEndo :: Type -> Bool
      isIntEndo (App "->" [App "Int" [], App "Int" []]) = True
      isIntEndo _                                       = False

Pattern synonyms permit abstracting from the representation to expose
matchers that behave in a constructor-like manner with respect to
pattern matching. We can create pattern synonyms for the known types we
care about, without committing the representation to them (note that
these don't have to be defined in the same module as the ``Type`` type): ::

      pattern Arrow t1 t2 = App "->"    [t1, t2]
      pattern Int         = App "Int"   []
      pattern Maybe t     = App "Maybe" [t]

Which enables us to rewrite our functions in a much cleaner style: ::

      collectArgs :: Type -> [Type]
      collectArgs (Arrow t1 t2) = t1 : collectArgs t2
      collectArgs _             = []

      isInt :: Type -> Bool
      isInt Int = True
      isInt _   = False

      isIntEndo :: Type -> Bool
      isIntEndo (Arrow Int Int) = True
      isIntEndo _               = False

In general there are three kinds of pattern synonyms. Unidirectional,
bidirectional and explicitly bidirectional. The examples given so far are
examples of bidirectional pattern synonyms. A bidirectional synonym
behaves the same as an ordinary data constructor. We can use it in a pattern
context to deconstruct values and in an expression context to construct values.
For example, we can construct the value `intEndo` using the pattern synonyms
`Arrow` and `Int` as defined previously. ::

      intEndo :: Type
      intEndo = Arrow Int Int

This example is equivalent to the much more complicated construction if we had
directly used the `Type` constructors. ::

      intEndo :: Type
      intEndo = App "->" [App "Int" [], App "Int" []]


Unidirectional synonyms can only be used in a pattern context and are
defined as follows:


::

      pattern Head x <- x:xs

In this case, ``Head`` ⟨x⟩ cannot be used in expressions, only patterns,
since it wouldn't specify a value for the ⟨xs⟩ on the right-hand side. However,
we can define an explicitly bidirectional pattern synonym by separately
specifying how to construct and deconstruct a type. The syntax for
doing this is as follows:

::

      pattern HeadC x <- x:xs where
        HeadC x = [x]

We can then use ``HeadC`` in both expression and pattern contexts. In a pattern
context it will match the head of any list with length at least one. In an
expression context it will construct a singleton list.

The table below summarises where each kind of pattern synonym can be used.

+---------------+----------------+---------------+---------------------------+
| Context       | Unidirectional | Bidirectional | Explicitly Bidirectional  |
+===============+================+===============+===========================+
| Pattern       | Yes            | Yes           | Yes                       |
+---------------+----------------+---------------+---------------------------+
| Expression    | No             | Yes (Inferred)| Yes (Explicit)            |
+---------------+----------------+---------------+---------------------------+

.. _record-patsyn:

Record Pattern Synonyms
-----------------------

It is also possible to define pattern synonyms which behave just like record
constructors. The syntax for doing this is as follows:

::

      pattern Point :: Int -> Int -> (Int, Int)
      pattern Point{x, y} = (x, y)

The idea is that we can then use ``Point`` just as if we had defined a new
datatype ``MyPoint`` with two fields ``x`` and ``y``.

::

    data MyPoint = Point { x :: Int, y :: Int }

Whilst a normal pattern synonym can be used in two ways, there are then seven
ways in which to use ``Point``. Precisely the ways in which a normal record
constructor can be used.

=======================================   ==================================
Usage                                     Example
=======================================   ==================================
As a constructor                          ``zero = Point 0 0``
As a constructor with record syntax       ``zero = Point { x = 0, y = 0}``
In a pattern context                      ``isZero (Point 0 0) = True``
In a pattern context with record syntax   ``isZero (Point { x = 0, y = 0 }``
In a pattern context with field puns      ``getX (Point {x}) = x``
In a record update                        ``(0, 0) { x = 1 } == (1,0)``
Using record selectors                    ``x (0,0) == 0``
=======================================   ==================================

For a unidirectional record pattern synonym we define record selectors but do
not allow record updates or construction.

The syntax and semantics of pattern synonyms are elaborated in the
following subsections.
There are also lots more details in the `paper
<https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/pattern-synonyms-Haskell16.pdf>`_.

See the :ghc-wiki:`Wiki page <PatternSynonyms>` for more
details.

Syntax and scoping of pattern synonyms
--------------------------------------

A pattern synonym declaration can be either unidirectional,
bidirectional or explicitly bidirectional.
The syntax for unidirectional pattern synonyms is: ::

      pattern pat_lhs <- pat

the syntax for bidirectional pattern synonyms is: ::

      pattern pat_lhs = pat

and the syntax for explicitly bidirectional pattern synonyms is: ::

      pattern pat_lhs <- pat where
        pat_lhs = expr

We can define either prefix, infix or record pattern synonyms by modifying
the form of `pat_lhs`. The syntax for these is as follows:

======= ============================
Prefix  ``Name args``
------- ----------------------------
Infix   ``arg1 `Name` arg2``
        or ``arg1 op arg2``
------- ----------------------------
Record  ``Name{arg1,arg2,...,argn}``
======= ============================


Pattern synonym declarations can only occur in the top level of a
module. In particular, they are not allowed as local definitions.

The variables in the left-hand side of the definition are bound by the
pattern on the right-hand side. For bidirectional pattern
synonyms, all the variables of the right-hand side must also occur on
the left-hand side; also, wildcard patterns and view patterns are not
allowed. For unidirectional and explicitly bidirectional pattern
synonyms, there is no restriction on the right-hand side pattern.

Pattern synonyms cannot be defined recursively.

:ref:`complete-pragma` can be specified in order to tell
the pattern match exhaustiveness checker that a set of pattern synonyms is
complete.

.. _patsyn-impexp:

Import and export of pattern synonyms
-------------------------------------

The name of the pattern synonym is in the same namespace as proper data
constructors. Like normal data constructors, pattern synonyms can be imported
and exported through association with a type constructor or independently.

To export them on their own, in an export or import specification, you must
prefix pattern names with the ``pattern`` keyword, e.g.: ::

      module Example (pattern Zero) where

      data MyNum = MkNum Int

      pattern Zero :: MyNum
      pattern Zero = MkNum 0

Without the ``pattern`` prefix, ``Zero`` would be interpreted as a
type constructor in the export list.

You may also use the ``pattern`` keyword in an import/export
specification to import or export an ordinary data constructor. For
example: ::

      import Data.Maybe( pattern Just )

would bring into scope the data constructor ``Just`` from the ``Maybe``
type, without also bringing the type constructor ``Maybe`` into scope.

To bundle a pattern synonym with a type constructor, we list the pattern
synonym in the export list of a module which exports the type constructor.
For example, to bundle ``Zero`` with ``MyNum`` we could write the following: ::

      module Example ( MyNum(Zero) ) where

If a module was then to import ``MyNum`` from ``Example``, it would also import
the pattern synonym ``Zero``.

It is also possible to use the special token ``..`` in an export list to mean
all currently bundled constructors. For example, we could write: ::

      module Example ( MyNum(.., Zero) ) where

in which case, ``Example`` would export the type constructor ``MyNum`` with
the data constructor ``MkNum`` and also the pattern synonym ``Zero``.

Bundled pattern synonyms are type checked to ensure that they are of the same
type as the type constructor which they are bundled with. A pattern synonym
``P`` can not be bundled with a type constructor ``T`` if ``P``\'s type is visibly
incompatible with ``T``.

A module which imports ``MyNum(..)`` from ``Example`` and then re-exports
``MyNum(..)`` will also export any pattern synonyms bundled with ``MyNum`` in
``Example``. A more complete specification can be found on the
:ghc-wiki:`wiki. <PatternSynonyms/AssociatingSynonyms>`


.. _patsyn-typing:

Typing of pattern synonyms
--------------------------

Given a pattern synonym definition of the form ::

      pattern P var1 var2 ... varN <- pat

it is assigned a *pattern type* of the form ::

      pattern P :: CReq => CProv => t1 -> t2 -> ... -> tN -> t

where ⟨CReq⟩ and ⟨CProv⟩ are type contexts, and ⟨t1⟩, ⟨t2⟩, ..., ⟨tN⟩
and ⟨t⟩ are types. Notice the unusual form of the type, with two
contexts ⟨CReq⟩ and ⟨CProv⟩:

-  ⟨CReq⟩ are the constraints *required* to match the pattern.

-  ⟨CProv⟩ are the constraints *made available (provided)* by a
   successful pattern match.

For example, consider ::

    data T a where
      MkT :: (Show b) => a -> b -> T a

    f1 :: (Num a, Eq a) => T a -> String
    f1 (MkT 42 x) = show x

    pattern ExNumPat :: (Num a, Eq a) => (Show b) => b -> T a
    pattern ExNumPat x = MkT 42 x

    f2 :: (Eq a, Num a) => T a -> String
    f2 (ExNumPat x) = show x

Here ``f1`` does not use pattern synonyms. To match against the numeric
pattern ``42`` *requires* the caller to satisfy the constraints
``(Num a, Eq a)``, so they appear in ``f1``'s type. The call to ``show``
generates a ``(Show b)`` constraint, where ``b`` is an existentially
type variable bound by the pattern match on ``MkT``. But the same
pattern match also *provides* the constraint ``(Show b)`` (see ``MkT``'s
type), and so all is well.

Exactly the same reasoning applies to ``ExNumPat``: matching against
``ExNumPat`` *requires* the constraints ``(Num a, Eq a)``, and
*provides* the constraint ``(Show b)``.

Note also the following points

-  In the common case where ``CProv`` is empty, (i.e., ``()``), it can be
   omitted altogether in the above pattern type signature for ``P``.

-  However, if ``CProv`` is non-empty, while ``CReq`` is, the above pattern type
   signature for ``P`` must be specified as ::

     P :: () => CProv => t1 -> t2 -> .. -> tN -> t

-  You may specify an explicit *pattern signature*, as we did for
   ``ExNumPat`` above, to specify the type of a pattern, just as you can
   for a function. As usual, the type signature can be less polymorphic
   than the inferred type. For example ::

         -- Inferred type would be 'a -> [a]'
         pattern SinglePair :: (a, a) -> [(a, a)]
         pattern SinglePair x = [x]

   Just like signatures on value-level bindings, pattern synonym signatures can
   apply to more than one pattern. For instance, ::

         pattern Left', Right' :: a -> Either a a
         pattern Left' x  = Left x
         pattern Right' x = Right x

-  The GHCi :ghci-cmd:`:info` command shows pattern types in this format.

-  For a bidirectional pattern synonym, a use of the pattern synonym as
   an expression has the type

   ::

         (CReq, CProv) => t1 -> t2 -> ... -> tN -> t

   So in the previous example, when used in an expression, ``ExNumPat``
   has type

   ::

         ExNumPat :: (Num a, Eq a, Show b) => b -> T t

   Notice that this is a tiny bit more restrictive than the expression
   ``MkT 42 x`` which would not require ``(Eq a)``.

-  Consider these two pattern synonyms: ::

       data S a where
          S1 :: Bool -> S Bool

       pattern P1 :: Bool -> Maybe Bool
       pattern P1 b = Just b

       pattern P2 :: () => (b ~ Bool) => Bool -> S b
       pattern P2 b = S1 b

       f :: Maybe a -> String
       f (P1 x) = "no no no"     -- Type-incorrect

       g :: S a -> String
       g (P2 b) = "yes yes yes"  -- Fine

   Pattern ``P1`` can only match against a value of type ``Maybe Bool``,
   so function ``f`` is rejected because the type signature is
   ``Maybe a``. (To see this, imagine expanding the pattern synonym.)

   On the other hand, function ``g`` works fine, because matching
   against ``P2`` (which wraps the GADT ``S``) provides the local
   equality ``(a~Bool)``. If you were to give an explicit pattern
   signature ``P2 :: Bool -> S Bool``, then ``P2`` would become less
   polymorphic, and would behave exactly like ``P1`` so that ``g`` would
   then be rejected.

   In short, if you want GADT-like behaviour for pattern synonyms, then
   (unlike concrete data constructors like ``S1``) you must write
   its type with explicit provided equalities. For a concrete data
   constructor like ``S1`` you can write its type signature as either
   ``S1 :: Bool -> S Bool`` or ``S1 :: (b~Bool) => Bool -> S b``; the
   two are equivalent. Not so for pattern synonyms: the two forms are
   different, in order to distinguish the two cases above. (See
   :ghc-ticket:`9953` for discussion of this choice.)

Matching of pattern synonyms
----------------------------

A pattern synonym occurrence in a pattern is evaluated by first matching
against the pattern synonym itself, and then on the argument patterns.
For example, in the following program, ``f`` and ``f'`` are equivalent: ::

    pattern Pair x y <- [x, y]

    f (Pair True True) = True
    f _                = False

    f' [x, y] | True <- x, True <- y = True
    f' _                              = False

Note that the strictness of ``f`` differs from that of ``g`` defined
below:

.. code-block:: none

    g [True, True] = True
    g _            = False

    *Main> f (False:undefined)
    *** Exception: Prelude.undefined
    *Main> g (False:undefined)
    False

.. _type-class-extensions:

Class and instances declarations
================================

.. _multi-param-type-classes:

Class declarations
------------------

This section, and the next one, documents GHC's type-class extensions.
There's lots of background in the paper `Type classes: exploring the
design
space <http://research.microsoft.com/~simonpj/Papers/type-class-design-space/>`__
(Simon Peyton Jones, Mark Jones, Erik Meijer).

Multi-parameter type classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XMultiParamTypeClasses

    :implies: :ghc-flag:`-XConstrainedClassMethods`

    Allow the definition of typeclasses with more than one parameter.

Multi-parameter type classes are permitted, with flag
:ghc-flag:`-XMultiParamTypeClasses`. For example: ::

      class Collection c a where
          union :: c a -> c a -> c a
          ...etc.

.. _superclass-rules:

The superclasses of a class declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XFlexibleContexts

    Allow the use of complex constraints in class declaration contexts.

In Haskell 98 the context of a class declaration (which introduces
superclasses) must be simple; that is, each predicate must consist of a
class applied to type variables. The flag :ghc-flag:`-XFlexibleContexts`
(:ref:`flexible-contexts`) lifts this restriction, so that the only
restriction on the context in a class declaration is that the class
hierarchy must be acyclic. So these class declarations are OK: ::

      class Functor (m k) => FiniteMap m k where
        ...

      class (Monad m, Monad (t m)) => Transform t m where
        lift :: m a -> (t m) a

As in Haskell 98, the class hierarchy must be acyclic. However, the
definition of "acyclic" involves only the superclass relationships. For
example, this is okay: ::

      class C a where
        op :: D b => a -> b -> b

      class C a => D a where ...

Here, ``C`` is a superclass of ``D``, but it's OK for a class operation
``op`` of ``C`` to mention ``D``. (It would not be OK for ``D`` to be a
superclass of ``C``.)

With the extension that adds a `kind of
constraints <#constraint-kind>`__, you can write more exotic superclass
definitions. The superclass cycle check is even more liberal in these
case. For example, this is OK: ::

      class A cls c where
        meth :: cls c => c -> c

      class A B c => B c where

A superclass context for a class ``C`` is allowed if, after expanding
type synonyms to their right-hand-sides, and uses of classes (other than
``C``) to their superclasses, ``C`` does not occur syntactically in the
context.

.. _class-method-types:

Constrained class method types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XConstrainedClassMethods

    Allows the definition of further constraints on individual class methods.

Haskell 98 prohibits class method types to mention constraints on the
class type variable, thus: ::

      class Seq s a where
        fromList :: [a] -> s a
        elem     :: Eq a => a -> s a -> Bool

The type of ``elem`` is illegal in Haskell 98, because it contains the
constraint ``Eq a``, which constrains only the class type variable (in
this case ``a``).
this case ``a``).  More precisely, a constraint in a class method signature is rejected if

- The constraint mentions at least one type variable.  So this is allowed: ::

     class C a where
       op1 :: HasCallStack => a -> a
       op2 :: (?x::Int) => Int -> a

- All of the type variables mentioned are bound by the class declaration, and none is locally quantified.  Examples: ::

     class C a where
       op3 :: Eq a => a -> a    -- Rejected: constrains class variable only
       op4 :: D b => a -> b     -- Accepted: constrains a locally-quantified variable `b`
       op5 :: D (a,b) => a -> b -- Accepted: constrains a locally-quantified variable `b`


GHC lifts this restriction with language extension
:ghc-flag:`-XConstrainedClassMethods`. The restriction is a pretty stupid one in
the first place, so :ghc-flag:`-XConstrainedClassMethods` is implied by
:ghc-flag:`-XMultiParamTypeClasses`.

.. _class-default-signatures:

Default method signatures
~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XDefaultSignatures

    :since: 7.2

    Allows the definition of default method signatures in class definitions.

Haskell 98 allows you to define a default implementation when declaring
a class: ::

      class Enum a where
        enum :: [a]
        enum = []

The type of the ``enum`` method is ``[a]``, and this is also the type of
the default method. You can lift this restriction and give another type
to the default method using the flag :ghc-flag:`-XDefaultSignatures`. For
instance, if you have written a generic implementation of enumeration in
a class ``GEnum`` with method ``genum`` in terms of ``GHC.Generics``,
you can specify a default method that uses that generic implementation: ::

      class Enum a where
        enum :: [a]
        default enum :: (Generic a, GEnum (Rep a)) => [a]
        enum = map to genum

We reuse the keyword ``default`` to signal that a signature applies to
the default method only; when defining instances of the ``Enum`` class,
the original type ``[a]`` of ``enum`` still applies. When giving an
empty instance, however, the default implementation ``(map to genum)`` is
filled-in, and type-checked with the type
``(Generic a, GEnum (Rep a)) => [a]``.

The type signature for a default method of a type class must take on the same
form as the corresponding main method's type signature. Otherwise, the
typechecker will reject that class's definition. By "take on the same form", we
mean that the default type signature should differ from the main type signature
only in their contexts. Therefore, if you have a method ``bar``: ::

      class Foo a where
        bar :: forall b. C => a -> b -> b

Then a default method for ``bar`` must take on the form: ::

      default bar :: forall b. C' => a -> b -> b

``C`` is allowed to be different from ``C'``, but the right-hand sides of the
type signatures must coincide. We require this because when you declare an
empty instance for a class that uses :ghc-flag:`-XDefaultSignatures`, GHC
implicitly fills in the default implementation like this: ::

      instance Foo Int where
        bar = default_bar @Int

Where ``@Int`` utilizes visible type application
(:ref:`visible-type-application`) to instantiate the ``b`` in
``default bar :: forall b. C' => a -> b -> b``. In order for this type
application to work, the default type signature for ``bar`` must have the same
type variable order as the non-default signature! But there is no obligation
for ``C`` and ``C'`` to be the same (see, for instance, the ``Enum`` example
above, which relies on this).

To further explain this example, the right-hand side of the default
type signature for ``bar`` must be something that is alpha-equivalent to
``forall b. a -> b -> b`` (where ``a`` is bound by the class itself, and is
thus free in the methods' type signatures). So this would also be an acceptable
default type signature: ::

      default bar :: forall x. C' => a -> x -> x

But not this (since the free variable ``a`` is in the wrong place): ::

      default bar :: forall b. C' => b -> a -> b

Nor this, since we can't match the type variable ``b`` with the concrete type
``Int``: ::

      default bar :: C' => a -> Int -> Int

That last one deserves a special mention, however, since ``a -> Int -> Int`` is
a straightforward instantiation of ``forall b. a -> b -> b``. You can still
write such a default type signature, but you now must use type equalities to
do so: ::

      default bar :: forall b. (C', b ~ Int) => a -> b -> b

We use default signatures to simplify generic programming in GHC
(:ref:`generic-programming`).

.. _nullary-type-classes:

Nullary type classes
~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XNullaryTypeClasses

    :since: 7.8.1

    Allows the use definition of type classes with no parameters. This flag
    has been replaced by :ghc-flag:`-XMultiParamTypeClasses`.


Nullary (no parameter) type classes are enabled with
:ghc-flag:`-XMultiParamTypeClasses`; historically, they were enabled with the
(now deprecated) :ghc-flag:`-XNullaryTypeClasses`. Since there are no available
parameters, there can be at most one instance of a nullary class. A nullary type
class might be used to document some assumption in a type signature (such as
reliance on the Riemann hypothesis) or add some globally configurable settings
in a program. For example, ::

      class RiemannHypothesis where
        assumeRH :: a -> a

      -- Deterministic version of the Miller test
      -- correctness depends on the generalised Riemann hypothesis
      isPrime :: RiemannHypothesis => Integer -> Bool
      isPrime n = assumeRH (...)

The type signature of ``isPrime`` informs users that its correctness depends on
an unproven conjecture. If the function is used, the user has to acknowledge the
dependence with: ::

      instance RiemannHypothesis where
        assumeRH = id

.. _functional-dependencies:

Functional dependencies
-----------------------

.. ghc-flag:: -XFunctionalDependencies

    :implies: :ghc-flag:`-XMultiParamTypeClasses`

    Allow use of functional dependencies in class declarations.

Functional dependencies are implemented as described by Mark Jones in
[Jones2000]_.

Functional dependencies are introduced by a vertical bar in the syntax
of a class declaration; e.g. ::

      class (Monad m) => MonadState s m | m -> s where ...

      class Foo a b c | a b -> c where ...

There should be more documentation, but there isn't (yet). Yell if you
need it.

.. [Jones2000]
    "`Type Classes with Functional
    Dependencies <http://citeseer.ist.psu.edu/jones00type.html>`__",
    Mark P. Jones, In *Proceedings of the 9th European Symposium on Programming*,
    ESOP 2000, Berlin, Germany, March 2000, Springer-Verlag LNCS 1782, .

Rules for functional dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a class declaration, all of the class type variables must be
reachable (in the sense mentioned in :ref:`flexible-contexts`) from the
free variables of each method type. For example: ::

      class Coll s a where
        empty  :: s
        insert :: s -> a -> s

is not OK, because the type of ``empty`` doesn't mention ``a``.
Functional dependencies can make the type variable reachable: ::

      class Coll s a | s -> a where
        empty  :: s
        insert :: s -> a -> s

Alternatively ``Coll`` might be rewritten ::

      class Coll s a where
        empty  :: s a
        insert :: s a -> a -> s a

which makes the connection between the type of a collection of ``a``'s
(namely ``(s a)``) and the element type ``a``. Occasionally this really
doesn't work, in which case you can split the class like this: ::

      class CollE s where
        empty  :: s

      class CollE s => Coll s a where
        insert :: s -> a -> s

Background on functional dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following description of the motivation and use of functional
dependencies is taken from the Hugs user manual, reproduced here (with
minor changes) by kind permission of Mark Jones.

Consider the following class, intended as part of a library for
collection types: ::

       class Collects e ce where
           empty  :: ce
           insert :: e -> ce -> ce
           member :: e -> ce -> Bool

The type variable ``e`` used here represents the element type, while ``ce`` is
the type of the container itself. Within this framework, we might want to define
instances of this class for lists or characteristic functions (both of which can
be used to represent collections of any equality type), bit sets (which can be
used to represent collections of characters), or hash tables (which can be used
to represent any collection whose elements have a hash function). Omitting
standard implementation details, this would lead to the following declarations: ::

       instance Eq e => Collects e [e] where ...
       instance Eq e => Collects e (e -> Bool) where ...
       instance Collects Char BitSet where ...
       instance (Hashable e, Collects a ce)
                  => Collects e (Array Int ce) where ...

All this looks quite promising; we have a class and a range of
interesting implementations. Unfortunately, there are some serious
problems with the class declaration. First, the empty function has an
ambiguous type: ::

       empty :: Collects e ce => ce

By "ambiguous" we mean that there is a type variable ``e`` that appears on
the left of the ``=>`` symbol, but not on the right. The problem with
this is that, according to the theoretical foundations of Haskell
overloading, we cannot guarantee a well-defined semantics for any term
with an ambiguous type.

We can sidestep this specific problem by removing the empty member from
the class declaration. However, although the remaining members, insert
and member, do not have ambiguous types, we still run into problems when
we try to use them. For example, consider the following two functions: ::

       f x y = insert x . insert y
       g     = f True 'a'

for which GHC infers the following types: ::

       f :: (Collects a c, Collects b c) => a -> b -> c -> c
       g :: (Collects Bool c, Collects Char c) => c -> c

Notice that the type for ``f`` allows the two parameters ``x`` and ``y`` to be
assigned different types, even though it attempts to insert each of the
two values, one after the other, into the same collection. If we're
trying to model collections that contain only one type of value, then
this is clearly an inaccurate type. Worse still, the definition for g is
accepted, without causing a type error. As a result, the error in this
code will not be flagged at the point where it appears. Instead, it will
show up only when we try to use ``g``, which might even be in a different
module.

An attempt to use constructor classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Faced with the problems described above, some Haskell programmers might
be tempted to use something like the following version of the class
declaration: ::

       class Collects e c where
          empty  :: c e
          insert :: e -> c e -> c e
          member :: e -> c e -> Bool

The key difference here is that we abstract over the type constructor ``c``
that is used to form the collection type ``c e``, and not over that
collection type itself, represented by ``ce`` in the original class
declaration. This avoids the immediate problems that we mentioned above:
empty has type ``Collects e c => c e``, which is not ambiguous.

The function ``f`` from the previous section has a more accurate type: ::

       f :: (Collects e c) => e -> e -> c e -> c e

The function ``g`` from the previous section is now rejected with a type
error as we would hope because the type of ``f`` does not allow the two
arguments to have different types. This, then, is an example of a
multiple parameter class that does actually work quite well in practice,
without ambiguity problems. There is, however, a catch. This version of
the ``Collects`` class is nowhere near as general as the original class
seemed to be: only one of the four instances for ``Collects`` given
above can be used with this version of Collects because only one of them—the
instance for lists—has a collection type that can be written in the form ``c
e``, for some type constructor ``c``, and element type ``e``.

Adding functional dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To get a more useful version of the ``Collects`` class, GHC provides a
mechanism that allows programmers to specify dependencies between the
parameters of a multiple parameter class (For readers with an interest
in theoretical foundations and previous work: The use of dependency
information can be seen both as a generalisation of the proposal for
"parametric type classes" that was put forward by Chen, Hudak, and
Odersky, or as a special case of Mark Jones's later framework for
"improvement" of qualified types. The underlying ideas are also
discussed in a more theoretical and abstract setting in a manuscript
[implparam], where they are identified as one point in a general design
space for systems of implicit parameterisation). To start with an
abstract example, consider a declaration such as: ::

       class C a b where ...

which tells us simply that ``C`` can be thought of as a binary relation on
types (or type constructors, depending on the kinds of ``a`` and ``b``). Extra
clauses can be included in the definition of classes to add information
about dependencies between parameters, as in the following examples: ::

       class D a b | a -> b where ...
       class E a b | a -> b, b -> a where ...

The notation ``a -> b`` used here between the ``|`` and ``where`` symbols —
not to be confused with a function type — indicates that the a
parameter uniquely determines the ``b`` parameter, and might be read as "``a``
determines ``b``." Thus ``D`` is not just a relation, but actually a (partial)
function. Similarly, from the two dependencies that are included in the
definition of ``E``, we can see that ``E`` represents a (partial) one-to-one
mapping between types.

More generally, dependencies take the form ``x1 ... xn -> y1 ... ym``,
where ``x1``, ..., ``xn``, and ``y1``, ..., ``yn`` are type variables with n>0 and m>=0,
meaning that the ``y`` parameters are uniquely determined by the ``x``
parameters. Spaces can be used as separators if more than one variable
appears on any single side of a dependency, as in ``t -> a b``. Note
that a class may be annotated with multiple dependencies using commas as
separators, as in the definition of ``E`` above. Some dependencies that we
can write in this notation are redundant, and will be rejected because
they don't serve any useful purpose, and may instead indicate an error
in the program. Examples of dependencies like this include ``a -> a``,
``a -> a a``, ``a ->``, etc. There can also be some redundancy if
multiple dependencies are given, as in ``a->b``, ``b->c``, ``a->c``, and
in which some subset implies the remaining dependencies. Examples like
this are not treated as errors. Note that dependencies appear only in
class declarations, and not in any other part of the language. In
particular, the syntax for instance declarations, class constraints, and
types is completely unchanged.

By including dependencies in a class declaration, we provide a mechanism
for the programmer to specify each multiple parameter class more
precisely. The compiler, on the other hand, is responsible for ensuring
that the set of instances that are in scope at any given point in the
program is consistent with any declared dependencies. For example, the
following pair of instance declarations cannot appear together in the
same scope because they violate the dependency for ``D``, even though either
one on its own would be acceptable: ::

       instance D Bool Int where ...
       instance D Bool Char where ...

Note also that the following declaration is not allowed, even by itself: ::

       instance D [a] b where ...

The problem here is that this instance would allow one particular choice
of ``[a]`` to be associated with more than one choice for ``b``, which
contradicts the dependency specified in the definition of ``D``. More
generally, this means that, in any instance of the form: ::

       instance D t s where ...

for some particular types ``t`` and ``s``, the only variables that can appear in
``s`` are the ones that appear in ``t``, and hence, if the type ``t`` is known,
then ``s`` will be uniquely determined.

The benefit of including dependency information is that it allows us to
define more general multiple parameter classes, without ambiguity
problems, and with the benefit of more accurate types. To illustrate
this, we return to the collection class example, and annotate the
original definition of ``Collects`` with a simple dependency: ::

       class Collects e ce | ce -> e where
          empty  :: ce
          insert :: e -> ce -> ce
          member :: e -> ce -> Bool

The dependency ``ce -> e`` here specifies that the type ``e`` of elements is
uniquely determined by the type of the collection ``ce``. Note that both
parameters of Collects are of kind ``*``; there are no constructor classes
here. Note too that all of the instances of ``Collects`` that we gave
earlier can be used together with this new definition.

What about the ambiguity problems that we encountered with the original
definition? The empty function still has type ``Collects e ce => ce``, but
it is no longer necessary to regard that as an ambiguous type: Although
the variable ``e`` does not appear on the right of the ``=>`` symbol, the
dependency for class ``Collects`` tells us that it is uniquely determined by
``ce``, which does appear on the right of the ``=>`` symbol. Hence the context
in which empty is used can still give enough information to determine
types for both ``ce`` and ``e``, without ambiguity. More generally, we need only
regard a type as ambiguous if it contains a variable on the left of the
``=>`` that is not uniquely determined (either directly or indirectly) by
the variables on the right.

Dependencies also help to produce more accurate types for user defined
functions, and hence to provide earlier detection of errors, and less
cluttered types for programmers to work with. Recall the previous
definition for a function ``f``: ::

       f x y = insert x y = insert x . insert y

for which we originally obtained a type: ::

       f :: (Collects a c, Collects b c) => a -> b -> c -> c

Given the dependency information that we have for ``Collects``, however, we
can deduce that ``a`` and ``b`` must be equal because they both appear as the
second parameter in a ``Collects`` constraint with the same first parameter
``c``. Hence we can infer a shorter and more accurate type for ``f``: ::

       f :: (Collects a c) => a -> a -> c -> c

In a similar way, the earlier definition of ``g`` will now be flagged as a
type error.

Although we have given only a few examples here, it should be clear that
the addition of dependency information can help to make multiple
parameter classes more useful in practice, avoiding ambiguity problems,
and allowing more general sets of instance declarations.

.. _instance-decls:

Instance declarations
---------------------

An instance declaration has the form ::

      instance ( assertion1, ..., assertionn) => class type1 ... typem where ...

The part before the "``=>``" is the *context*, while the part after the
"``=>``" is the *head* of the instance declaration.

.. _instance-resolution:

Instance resolution
~~~~~~~~~~~~~~~~~~~

When GHC tries to resolve, say, the constraint ``C Int Bool``, it tries
to match every instance declaration against the constraint, by
instantiating the head of the instance declaration. Consider these
declarations: ::

      instance context1 => C Int a     where ...  -- (A)
      instance context2 => C a   Bool  where ...  -- (B)

GHC's default behaviour is that *exactly one instance must match the
constraint it is trying to resolve*. For example, the constraint
``C Int Bool`` matches instances (A) and (B), and hence would be
rejected; while ``C Int Char`` matches only (A) and hence (A) is chosen.

Notice that

-  When matching, GHC takes no account of the context of the instance
   declaration (``context1`` etc).

-  It is fine for there to be a *potential* of overlap (by including
   both declarations (A) and (B), say); an error is only reported if a
   particular constraint matches more than one.

See also :ref:`instance-overlap` for flags that loosen the instance
resolution rules.

.. _flexible-instance-head:

Relaxed rules for the instance head
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XTypeSynonymInstances

    Allow definition of type class instances for type synonyms.

.. ghc-flag:: -XFlexibleInstances

    :implies: :ghc-flag:`-XTypeSynonymInstances`

    Allow definition of type class instances with arbitrary nested types in the
    instance head.

In Haskell 98 the head of an instance declaration must be of the form
``C (T a1 ... an)``, where ``C`` is the class, ``T`` is a data type
constructor, and the ``a1 ... an`` are distinct type variables. In the
case of multi-parameter type classes, this rule applies to each
parameter of the instance head (Arguably it should be okay if just one
has this form and the others are type variables, but that's the rules at
the moment).

GHC relaxes this rule in two ways:

-  With the :ghc-flag:`-XTypeSynonymInstances` flag, instance heads may use type
   synonyms. As always, using a type synonym is just shorthand for
   writing the RHS of the type synonym definition. For example: ::

         type Point a = (a,a)
         instance C (Point a)   where ...

   is legal. The instance declaration is equivalent to ::

         instance C (a,a) where ...

   As always, type synonyms must be fully applied. You cannot, for
   example, write: ::

         instance Monad Point where ...

-  The :ghc-flag:`-XFlexibleInstances` flag allows the head of the instance
   declaration to mention arbitrary nested types. For example, this
   becomes a legal instance declaration ::

         instance C (Maybe Int) where ...

   See also the `rules on overlap <#instance-overlap>`__.

   The :ghc-flag:`-XFlexibleInstances` flag implies
   :ghc-flag:`-XTypeSynonymInstances`.

However, the instance declaration must still conform to the rules for
instance termination: see :ref:`instance-termination`.

.. _instance-rules:

Relaxed rules for instance contexts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Haskell 98, the class constraints in the context of the instance
declaration must be of the form ``C a`` where ``a`` is a type variable
that occurs in the head.

The :ghc-flag:`-XFlexibleContexts` flag relaxes this rule, as well as relaxing
the corresponding rule for type signatures (see
:ref:`flexible-contexts`). Specifically, :ghc-flag:`-XFlexibleContexts`, allows
(well-kinded) class constraints of form ``(C t1 ... tn)`` in the context
of an instance declaration.

Notice that the flag does not affect equality constraints in an instance
context; they are permitted by :ghc-flag:`-XTypeFamilies` or :ghc-flag:`-XGADTs`.

However, the instance declaration must still conform to the rules for
instance termination: see :ref:`instance-termination`.

.. _instance-termination:

Instance termination rules
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XUndecidableInstances

    Permit definition of instances which may lead to type-checker non-termination.

Regardless of :ghc-flag:`-XFlexibleInstances` and :ghc-flag:`-XFlexibleContexts`,
instance declarations must conform to some rules that ensure that
instance resolution will terminate. The restrictions can be lifted with
:ghc-flag:`-XUndecidableInstances` (see :ref:`undecidable-instances`).

The rules are these:

1. The Paterson Conditions: for each class constraint ``(C t1 ... tn)``
   in the context

   1. No type variable has more occurrences in the constraint than in
      the head

   2. The constraint has fewer constructors and variables (taken
      together and counting repetitions) than the head

   3. The constraint mentions no type functions. A type function
      application can in principle expand to a type of arbitrary size,
      and so are rejected out of hand

2. The Coverage Condition. For each functional dependency,
   ⟨tvs⟩\ :sub:`left` ``->`` ⟨tvs⟩\ :sub:`right`, of the class, every
   type variable in S(⟨tvs⟩\ :sub:`right`) must appear in
   S(⟨tvs⟩\ :sub:`left`), where S is the substitution mapping each type
   variable in the class declaration to the corresponding type in the
   instance head.

These restrictions ensure that instance resolution terminates: each
reduction step makes the problem smaller by at least one constructor.
You can find lots of background material about the reason for these
restrictions in the paper `Understanding functional dependencies via
Constraint Handling
Rules <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf>`__.

For example, these are okay:

::

      instance C Int [a]          -- Multiple parameters
      instance Eq (S [a])         -- Structured type in head

          -- Repeated type variable in head
      instance C4 a a => C4 [a] [a]
      instance Stateful (ST s) (MutVar s)

          -- Head can consist of type variables only
      instance C a
      instance (Eq a, Show b) => C2 a b

          -- Non-type variables in context
      instance Show (s a) => Show (Sized s a)
      instance C2 Int a => C3 Bool [a]
      instance C2 Int a => C3 [a] b

But these are not:

::

          -- Context assertion no smaller than head
      instance C a => C a where ...
          -- (C b b) has more occurrences of b than the head
      instance C b b => Foo [b] where ...

The same restrictions apply to instances generated by ``deriving``
clauses. Thus the following is accepted:

::

      data MinHeap h a = H a (h a)
        deriving (Show)

because the derived instance

::

      instance (Show a, Show (h a)) => Show (MinHeap h a)

conforms to the above rules.

A useful idiom permitted by the above rules is as follows. If one allows
overlapping instance declarations then it's quite convenient to have a
"default instance" declaration that applies if something more specific
does not:

::

      instance C a where
        op = ... -- Default

.. _undecidable-instances:

Undecidable instances
~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: -XUndecidableInstances

Sometimes even the termination rules of :ref:`instance-termination` are
too onerous. So GHC allows you to experiment with more liberal rules: if
you use the experimental flag :ghc-flag:`-XUndecidableInstances`, both the Paterson
Conditions and the Coverage
Condition (described in :ref:`instance-termination`) are lifted.
Termination is still ensured by having a fixed-depth recursion stack. If
you exceed the stack depth you get a sort of backtrace, and the
opportunity to increase the stack depth with
``-freduction-depth=⟨n⟩``. However, if you should exceed the default
reduction depth limit, it is probably best just to disable depth
checking, with ``-freduction-depth=0``. The exact depth your program
requires depends on minutiae of your code, and it may change between
minor GHC releases. The safest bet for released code -- if you're sure
that it should compile in finite time -- is just to disable the check.

For example, sometimes you might want to use the following to get the
effect of a "class synonym":

::

      class (C1 a, C2 a, C3 a) => C a where { }

      instance (C1 a, C2 a, C3 a) => C a where { }

This allows you to write shorter signatures:

::

      f :: C a => ...

instead of

::

      f :: (C1 a, C2 a, C3 a) => ...

The restrictions on functional dependencies
(:ref:`functional-dependencies`) are particularly troublesome. It is
tempting to introduce type variables in the context that do not appear
in the head, something that is excluded by the normal rules. For
example:

::

      class HasConverter a b | a -> b where
         convert :: a -> b

      data Foo a = MkFoo a

      instance (HasConverter a b,Show b) => Show (Foo a) where
         show (MkFoo value) = show (convert value)

This is dangerous territory, however. Here, for example, is a program
that would make the typechecker loop:

::

      class D a
      class F a b | a->b
      instance F [a] [[a]]
      instance (D c, F a c) => D [a]   -- 'c' is not mentioned in the head

Similarly, it can be tempting to lift the coverage condition:

::

      class Mul a b c | a b -> c where
        (.*.) :: a -> b -> c

      instance Mul Int Int Int where (.*.) = (*)
      instance Mul Int Float Float where x .*. y = fromIntegral x * y
      instance Mul a b c => Mul a [b] [c] where x .*. v = map (x.*.) v

The third instance declaration does not obey the coverage condition; and
indeed the (somewhat strange) definition:

::

      f = \ b x y -> if b then x .*. [y] else y

makes instance inference go into a loop, because it requires the
constraint ``(Mul a [b] b)``.

The :ghc-flag:`-XUndecidableInstances` flag is also used to lift some of the
restrictions imposed on type family instances. See
:ref:`type-family-decidability`.

.. _instance-overlap:

Overlapping instances
~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XOverlappingInstances
              -XIncoherentInstances

    Deprecated flags to weaken checks intended to ensure instance resolution
    termination.

In general, as discussed in :ref:`instance-resolution`, *GHC requires
that it be unambiguous which instance declaration should be used to
resolve a type-class constraint*. GHC also provides a way to to loosen
the instance resolution, by allowing more than one instance to match,
*provided there is a most specific one*. Moreover, it can be loosened
further, by allowing more than one instance to match irrespective of
whether there is a most specific one. This section gives the details.

To control the choice of instance, it is possible to specify the overlap
behavior for individual instances with a pragma, written immediately
after the ``instance`` keyword. The pragma may be one of:
``{-# OVERLAPPING #-}``, ``{-# OVERLAPPABLE #-}``, ``{-# OVERLAPS #-}``,
or ``{-# INCOHERENT #-}``.

The matching behaviour is also influenced by two module-level language
extension flags: :ghc-flag:`-XOverlappingInstances` and
:ghc-flag:`-XIncoherentInstances`. These flags are now
deprecated (since GHC 7.10) in favour of the fine-grained per-instance
pragmas.

A more precise specification is as follows. The willingness to be
overlapped or incoherent is a property of the *instance declaration*
itself, controlled as follows:

-  An instance is *incoherent* if: it has an ``INCOHERENT`` pragma; or
   if the instance has no pragma and it appears in a module compiled
   with :ghc-flag:`-XIncoherentInstances`.

-  An instance is *overlappable* if: it has an ``OVERLAPPABLE`` or
   ``OVERLAPS`` pragma; or if the instance has no pragma and it appears
   in a module compiled with :ghc-flag:`-XOverlappingInstances`; or if the
   instance is incoherent.

-  An instance is *overlapping* if: it has an ``OVERLAPPING`` or
   ``OVERLAPS`` pragma; or if the instance has no pragma and it appears
   in a module compiled with :ghc-flag:`-XOverlappingInstances`; or if the
   instance is incoherent.

Now suppose that, in some client module, we are searching for an
instance of the *target constraint* ``(C ty1 .. tyn)``. The search works
like this:

-  Find all instances :math:`I` that *match* the target constraint; that is, the
   target constraint is a substitution instance of :math:`I`. These instance
   declarations are the *candidates*.

-  Eliminate any candidate :math:`IX` for which both of the following hold:

   -  There is another candidate :math:`IY` that is strictly more specific; that
      is, :math:`IY` is a substitution instance of :math:`IX` but not vice versa.

   -  Either :math:`IX` is *overlappable*, or :math:`IY` is *overlapping*. (This
      "either/or" design, rather than a "both/and" design, allow a
      client to deliberately override an instance from a library,
      without requiring a change to the library.)

-  If exactly one non-incoherent candidate remains, select it. If all
   remaining candidates are incoherent, select an arbitrary one.
   Otherwise the search fails (i.e. when more than one surviving
   candidate is not incoherent).

-  If the selected candidate (from the previous step) is incoherent, the
   search succeeds, returning that candidate.

-  If not, find all instances that *unify* with the target constraint,
   but do not *match* it. Such non-candidate instances might match when
   the target constraint is further instantiated. If all of them are
   incoherent, the search succeeds, returning the selected candidate; if
   not, the search fails.

Notice that these rules are not influenced by flag settings in the
client module, where the instances are *used*. These rules make it
possible for a library author to design a library that relies on
overlapping instances without the client having to know.

Errors are reported *lazily* (when attempting to solve a constraint),
rather than *eagerly* (when the instances themselves are defined).
Consider, for example ::

      instance C Int  b where ..
      instance C a Bool where ..

These potentially overlap, but GHC will not complain about the instance
declarations themselves, regardless of flag settings. If we later try to
solve the constraint ``(C Int Char)`` then only the first instance
matches, and all is well. Similarly with ``(C Bool Bool)``. But if we
try to solve ``(C Int Bool)``, both instances match and an error is
reported.

As a more substantial example of the rules in action, consider ::

      instance {-# OVERLAPPABLE #-} context1 => C Int b     where ...  -- (A)
      instance {-# OVERLAPPABLE #-} context2 => C a   Bool  where ...  -- (B)
      instance {-# OVERLAPPABLE #-} context3 => C a   [b]   where ...  -- (C)
      instance {-# OVERLAPPING  #-} context4 => C Int [Int] where ...  -- (D)

Now suppose that the type inference engine needs to solve the constraint
``C Int [Int]``. This constraint matches instances (A), (C) and (D), but
the last is more specific, and hence is chosen.

If (D) did not exist then (A) and (C) would still be matched, but
neither is most specific. In that case, the program would be rejected,
unless :ghc-flag:`-XIncoherentInstances` is enabled, in which case it would be
accepted and (A) or (C) would be chosen arbitrarily.

An instance declaration is *more specific* than another iff the head of
former is a substitution instance of the latter. For example (D) is
"more specific" than (C) because you can get from (C) to (D) by
substituting ``a := Int``.

GHC is conservative about committing to an overlapping instance. For
example: ::

      f :: [b] -> [b]
      f x = ...

Suppose that from the RHS of ``f`` we get the constraint ``C b [b]``.
But GHC does not commit to instance (C), because in a particular call of
``f``, ``b`` might be instantiate to ``Int``, in which case instance (D)
would be more specific still. So GHC rejects the program.

If, however, you add the flag :ghc-flag:`-XIncoherentInstances` when compiling
the module that contains (D), GHC will instead pick (C), without
complaining about the problem of subsequent instantiations.

Notice that we gave a type signature to ``f``, so GHC had to *check*
that ``f`` has the specified type. Suppose instead we do not give a type
signature, asking GHC to *infer* it instead. In this case, GHC will
refrain from simplifying the constraint ``C Int [b]`` (for the same
reason as before) but, rather than rejecting the program, it will infer
the type ::

      f :: C b [b] => [b] -> [b]

That postpones the question of which instance to pick to the call site
for ``f`` by which time more is known about the type ``b``. You can
write this type signature yourself if you use the
:ghc-flag:`-XFlexibleContexts` flag.

Exactly the same situation can arise in instance declarations
themselves. Suppose we have ::

      class Foo a where
         f :: a -> a
      instance Foo [b] where
         f x = ...

and, as before, the constraint ``C Int [b]`` arises from ``f``'s right
hand side. GHC will reject the instance, complaining as before that it
does not know how to resolve the constraint ``C Int [b]``, because it
matches more than one instance declaration. The solution is to postpone
the choice by adding the constraint to the context of the instance
declaration, thus: ::

      instance C Int [b] => Foo [b] where
         f x = ...

(You need :ghc-flag:`-XFlexibleInstances` to do this.)

.. warning::
    Overlapping instances must be used with care. They can give
    rise to incoherence (i.e. different instance choices are made in
    different parts of the program) even without :ghc-flag:`-XIncoherentInstances`.
    Consider: ::

        {-# LANGUAGE OverlappingInstances #-}
        module Help where

            class MyShow a where
            myshow :: a -> String

            instance MyShow a => MyShow [a] where
            myshow xs = concatMap myshow xs

            showHelp :: MyShow a => [a] -> String
            showHelp xs = myshow xs

        {-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
        module Main where
            import Help

            data T = MkT

            instance MyShow T where
            myshow x = "Used generic instance"

            instance MyShow [T] where
            myshow xs = "Used more specific instance"

            main = do { print (myshow [MkT]); print (showHelp [MkT]) }

    In function ``showHelp`` GHC sees no overlapping instances, and so uses
    the ``MyShow [a]`` instance without complaint. In the call to ``myshow``
    in ``main``, GHC resolves the ``MyShow [T]`` constraint using the
    overlapping instance declaration in module ``Main``. As a result, the
    program prints

    .. code-block:: none

        "Used more specific instance"
        "Used generic instance"

    (An alternative possible behaviour, not currently implemented, would be
    to reject module ``Help`` on the grounds that a later instance
    declaration might overlap the local one.)

.. _instance-sigs:

Instance signatures: type signatures in instance declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XInstanceSigs

    :since: 7.6.1

    Allow type signatures for members in instance definitions.

In Haskell, you can't write a type signature in an instance declaration,
but it is sometimes convenient to do so, and the language extension
:ghc-flag:`-XInstanceSigs` allows you to do so. For example: ::

      data T a = MkT a a
      instance Eq a => Eq (T a) where
        (==) :: T a -> T a -> Bool   -- The signature
        (==) (MkT x1 x2) (MkTy y1 y2) = x1==y1 && x2==y2

Some details

-  The type signature in the instance declaration must be more
   polymorphic than (or the same as) the one in the class declaration,
   instantiated with the instance type. For example, this is fine: ::

         instance Eq a => Eq (T a) where
            (==) :: forall b. b -> b -> Bool
            (==) x y = True

   Here the signature in the instance declaration is more polymorphic
   than that required by the instantiated class method.

-  The code for the method in the instance declaration is typechecked
   against the type signature supplied in the instance declaration, as
   you would expect. So if the instance signature is more polymorphic
   than required, the code must be too.

-  One stylistic reason for wanting to write a type signature is simple
   documentation. Another is that you may want to bring scoped type
   variables into scope. For example: ::

       class C a where
         foo :: b -> a -> (a, [b])

       instance C a => C (T a) where
         foo :: forall b. b -> T a -> (T a, [b])
         foo x (T y) = (T y, xs)
            where
              xs :: [b]
              xs = [x,x,x]

   Provided that you also specify :ghc-flag:`-XScopedTypeVariables`
   (:ref:`scoped-type-variables`), the ``forall b`` scopes over the
   definition of ``foo``, and in particular over the type signature for
   ``xs``.

.. _overloaded-strings:

Overloaded string literals
--------------------------

.. ghc-flag:: -XOverloadedStrings

    Enable overloaded string literals (e.g. string literals desugared via the
    ``IsString`` class).

GHC supports *overloaded string literals*. Normally a string literal has
type ``String``, but with overloaded string literals enabled (with
:ghc-flag:`-XOverloadedStrings`) a string literal has type
``(IsString a) => a``.

This means that the usual string syntax can be used, e.g., for
``ByteString``, ``Text``, and other variations of string like types.
String literals behave very much like integer literals, i.e., they can
be used in both expressions and patterns. If used in a pattern the
literal will be replaced by an equality test, in the same way as an
integer literal is.

The class ``IsString`` is defined as: ::

    class IsString a where
        fromString :: String -> a

The only predefined instance is the obvious one to make strings work as
usual: ::

    instance IsString [Char] where
        fromString cs = cs

The class ``IsString`` is not in scope by default. If you want to
mention it explicitly (for example, to give an instance declaration for
it), you can import it from module ``GHC.Exts``.

Haskell's defaulting mechanism (`Haskell Report, Section
4.3.4 <http://www.haskell.org/onlinereport/decls.html#sect4.3.4>`__) is
extended to cover string literals, when :ghc-flag:`-XOverloadedStrings` is
specified. Specifically:

-  Each type in a ``default`` declaration must be an instance of ``Num``
   *or* of ``IsString``.

-  If no ``default`` declaration is given, then it is just as if the
   module contained the declaration
   ``default( Integer, Double, String)``.

-  The standard defaulting rule is extended thus: defaulting applies
   when all the unresolved constraints involve standard classes *or*
   ``IsString``; and at least one is a numeric class *or* ``IsString``.

So, for example, the expression ``length "foo"`` will give rise to an
ambiguous use of ``IsString a0`` which, because of the above rules, will
default to ``String``.

A small example:

::

    module Main where

    import GHC.Exts( IsString(..) )

    newtype MyString = MyString String deriving (Eq, Show)
    instance IsString MyString where
        fromString = MyString

    greet :: MyString -> MyString
    greet "hello" = "world"
    greet other = other

    main = do
        print $ greet "hello"
        print $ greet "fool"

Note that deriving ``Eq`` is necessary for the pattern matching to work
since it gets translated into an equality comparison.

.. _overloaded-labels:

Overloaded labels
-----------------

.. ghc-flag:: -XOverloadedLabels

    :since: 8.0.1

    Enable use of the ``#foo`` overloaded label syntax.

GHC supports *overloaded labels*, a form of identifier whose interpretation may
depend both on its type and on its literal text.  When the
:ghc-flag:`-XOverloadedLabels` extension is enabled, an overloaded label can written
with a prefix hash, for example ``#foo``.  The type of this expression is
``IsLabel "foo" a => a``.

The class ``IsLabel`` is defined as:

::

    class IsLabel (x :: Symbol) a where
      fromLabel :: a

This is rather similar to the class ``IsString`` (see
:ref:`overloaded-strings`), but with an additional type parameter that makes the
text of the label available as a type-level string (see
:ref:`type-level-literals`).  Note that ``fromLabel`` had an extra ``Proxy# x``
argument in GHC 8.0, but this was removed in GHC 8.2 as a type application (see
:ref:`visible-type-application`) can be used instead.

There are no predefined instances of this class.  It is not in scope by default,
but can be brought into scope by importing
:base-ref:`GHC.OverloadedLabels.`.  Unlike
``IsString``, there are no special defaulting rules for ``IsLabel``.

During typechecking, GHC will replace an occurrence of an overloaded label like
``#foo`` with ``fromLabel @"foo"``.  This will have some type ``alpha`` and
require the solution of a class constraint ``IsLabel "foo" alpha``.

The intention is for ``IsLabel`` to be used to support overloaded record fields
and perhaps anonymous records.  Thus, it may be given instances for base
datatypes (in particular ``(->)``) in the future.

If :ghc-flag:`-XRebindableSyntax` is enabled, overloaded
labels will be desugared using whatever ``fromLabel`` function is in scope,
rather than always using ``GHC.OverloadedLabels.fromLabel``.

When writing an overloaded label, there must be no space between the hash sign
and the following identifier.  The :ghc-flag:`-XMagicHash` extension makes use
of postfix hash signs; if :ghc-flag:`-XOverloadedLabels` and
:ghc-flag:`-XMagicHash` are both enabled then ``x#y`` means ``x# y``, but if
only :ghc-flag:`-XOverloadedLabels` is enabled then it means ``x #y``.  The
:ghc-flag:`-XUnboxedTuples` extension makes ``(#`` a single lexeme, so when
:ghc-flag:`-XUnboxedTuples` is enabled you must write a space between an opening
parenthesis and an overloaded label.  To avoid confusion, you are strongly
encouraged to put a space before the hash when using
:ghc-flag:`-XOverloadedLabels`.

When using :ghc-flag:`-XOverloadedLabels` (or other extensions that make use of
hash signs) in a ``.hsc`` file (see :ref:`hsc2hs`), the hash signs must be
doubled (write ``##foo`` instead of ``#foo``) to avoid them being treated as
``hsc2hs`` directives.

Here is an extension of the record access example in :ref:`type-level-literals`
showing how an overloaded label can be used as a record selector:

::

    {-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses,
                 FunctionalDependencies, FlexibleInstances,
                 OverloadedLabels, ScopedTypeVariables #-}

    import GHC.OverloadedLabels (IsLabel(..))
    import GHC.TypeLits (Symbol)

    data Label (l :: Symbol) = Get

    class Has a l b | a l -> b where
      from :: a -> Label l -> b

    data Point = Point Int Int deriving Show

    instance Has Point "x" Int where from (Point x _) _ = x
    instance Has Point "y" Int where from (Point _ y) _ = y

    instance Has a l b => IsLabel l (a -> b) where
      fromLabel x = from x (Get :: Label l)

    example = #x (Point 1 2)


.. _overloaded-lists:

Overloaded lists
----------------

.. ghc-flag:: -XOverloadedLists

    :since: 7.8.1

    Enable overloaded list syntax (e.g. desugaring of lists via the
    ``IsList`` class).

GHC supports *overloading of the list notation*. Let us recap the
notation for constructing lists. In Haskell, the list notation can be
used in the following seven ways:

::

    []          -- Empty list
    [x]         -- x : []
    [x,y,z]     -- x : y : z : []
    [x .. ]     -- enumFrom x
    [x,y ..]    -- enumFromThen x y
    [x .. y]    -- enumFromTo x y
    [x,y .. z]  -- enumFromThenTo x y z

When the ``OverloadedLists`` extension is turned on, the aforementioned
seven notations are desugared as follows:

::

    []          -- fromListN 0 []
    [x]         -- fromListN 1 (x : [])
    [x,y,z]     -- fromListN 3 (x : y : z : [])
    [x .. ]     -- fromList (enumFrom x)
    [x,y ..]    -- fromList (enumFromThen x y)
    [x .. y]    -- fromList (enumFromTo x y)
    [x,y .. z]  -- fromList (enumFromThenTo x y z)

This extension allows programmers to use the list notation for
construction of structures like: ``Set``, ``Map``, ``IntMap``,
``Vector``, ``Text`` and ``Array``. The following code listing gives a
few examples:

::

    ['0' .. '9']             :: Set Char
    [1 .. 10]                :: Vector Int
    [("default",0), (k1,v1)] :: Map String Int
    ['a' .. 'z']             :: Text

List patterns are also overloaded. When the ``OverloadedLists``
extension is turned on, these definitions are desugared as follows

::

    f [] = ...          -- f (toList -> []) = ...
    g [x,y,z] = ...     -- g (toList -> [x,y,z]) = ...

(Here we are using view-pattern syntax for the translation, see
:ref:`view-patterns`.)

The ``IsList`` class
~~~~~~~~~~~~~~~~~~~~

In the above desugarings, the functions ``toList``, ``fromList`` and
``fromListN`` are all methods of the ``IsList`` class, which is itself
exported from the ``GHC.Exts`` module. The type class is defined as
follows:

::

    class IsList l where
      type Item l

      fromList :: [Item l] -> l
      toList   :: l -> [Item l]

      fromListN :: Int -> [Item l] -> l
      fromListN _ = fromList

The ``IsList`` class and its methods are intended to be used in
conjunction with the ``OverloadedLists`` extension.

-  The type function ``Item`` returns the type of items of the structure
   ``l``.

-  The function ``fromList`` constructs the structure ``l`` from the
   given list of ``Item l``.

-  The function ``fromListN`` takes the input list's length as a hint.
   Its behaviour should be equivalent to ``fromList``. The hint can be
   used for more efficient construction of the structure ``l`` compared
   to ``fromList``. If the given hint is not equal to the input list's
   length the behaviour of ``fromListN`` is not specified.

-  The function ``toList`` should be the inverse of ``fromList``.

It is perfectly fine to declare new instances of ``IsList``, so that
list notation becomes useful for completely new data types. Here are
several example instances:

::

    instance IsList [a] where
      type Item [a] = a
      fromList = id
      toList = id

    instance (Ord a) => IsList (Set a) where
      type Item (Set a) = a
      fromList = Set.fromList
      toList = Set.toList

    instance (Ord k) => IsList (Map k v) where
      type Item (Map k v) = (k,v)
      fromList = Map.fromList
      toList = Map.toList

    instance IsList (IntMap v) where
      type Item (IntMap v) = (Int,v)
      fromList = IntMap.fromList
      toList = IntMap.toList

    instance IsList Text where
      type Item Text = Char
      fromList = Text.pack
      toList = Text.unpack

    instance IsList (Vector a) where
      type Item (Vector a) = a
      fromList  = Vector.fromList
      fromListN = Vector.fromListN
      toList = Vector.toList

Rebindable syntax
~~~~~~~~~~~~~~~~~

When desugaring list notation with :ghc-flag:`-XOverloadedLists` GHC uses the
``fromList`` (etc) methods from module ``GHC.Exts``. You do not need to
import ``GHC.Exts`` for this to happen.

However if you use :ghc-flag:`-XRebindableSyntax`, then GHC instead uses
whatever is in scope with the names of ``toList``, ``fromList`` and
``fromListN``. That is, these functions are rebindable; c.f.
:ref:`rebindable-syntax`.

Defaulting
~~~~~~~~~~

Currently, the ``IsList`` class is not accompanied with defaulting
rules. Although feasible, not much thought has gone into how to specify
the meaning of the default declarations like: ::

    default ([a])

Speculation about the future
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The current implementation of the ``OverloadedLists`` extension can be
improved by handling the lists that are only populated with literals in
a special way. More specifically, the compiler could allocate such lists
statically using a compact representation and allow ``IsList`` instances
to take advantage of the compact representation. Equipped with this
capability the ``OverloadedLists`` extension will be in a good position
to subsume the ``OverloadedStrings`` extension (currently, as a special
case, string literals benefit from statically allocated compact
representation).

Undecidable (or recursive) superclasses
---------------------------------------

.. ghc-flag:: -XUndecidableSuperClasses

    :since: 8.0.1

    Allow all superclass constraints, including those that may result in
    non-termination of the typechecker.

The language extension :ghc-flag:`-XUndecidableSuperClasses` allows much more flexible
constraints in superclasses.

A class cannot generally have itself as a superclass. So this is illegal ::

    class C a => D a where ...
    class D a => C a where ...

GHC implements this test conservatively when type functions, or type variables,
are involved. For example ::

    type family F a :: Constraint
    class F a => C a where ...

GHC will complain about this, because you might later add ::

    type instance F Int = C Int

and now we'd be in a superclass loop.  Here's an example involving a type variable ::

   class f (C f) => C f
   class c       => Id c

If we expanded the superclasses of ``C Id`` we'd get first ``Id (C Id)`` and
thence ``C Id`` again.

But superclass constraints like these are sometimes useful, and the conservative
check is annoying where no actual recursion is involved.

Moreover genuninely-recursive superclasses are sometimes useful. Here's a real-life
example (Trac #10318) ::

     class (Frac (Frac a) ~ Frac a,
            Fractional (Frac a),
            IntegralDomain (Frac a))
         => IntegralDomain a where
      type Frac a :: *

Here the superclass cycle does terminate but it's not entirely straightforward
to see that it does.

With the language extension :ghc-flag:`-XUndecidableSuperClasses` GHC lifts all restrictions
on superclass constraints. If there really *is* a loop, GHC will only
expand it to finite depth.


.. _type-families:

Type families
=============

.. ghc-flag:: -XTypeFamilies

    :implies: :ghc-flag:`-XMonoLocalBinds`, :ghc-flag:`-XKindSignatures`,
              :ghc-flag:`-XExplicitNamespaces`

    Allow use and definition of indexed type and data families.

Indexed type families form an extension to facilitate type-level
programming. Type families are a generalisation of associated data types
[AssocDataTypes2005]_ and associated type synonyms
[AssocTypeSyn2005]_ Type families themselves are described in
Schrijvers 2008 [TypeFamilies2008]_. Type families essentially provide
type-indexed data types and named functions on types, which are useful for
generic programming and highly parameterised library interfaces as well as
interfaces with enhanced static information, much like dependent types. They
might also be regarded as an alternative to functional dependencies, but provide
a more functional style of type-level programming than the relational style of
functional dependencies.

Indexed type families, or type families for short, are type constructors
that represent sets of types. Set members are denoted by supplying the
type family constructor with type parameters, which are called type
indices. The difference between vanilla parametrised type constructors
and family constructors is much like between parametrically polymorphic
functions and (ad-hoc polymorphic) methods of type classes. Parametric
polymorphic functions behave the same at all type instances, whereas
class methods can change their behaviour in dependence on the class type
parameters. Similarly, vanilla type constructors imply the same data
representation for all type instances, but family constructors can have
varying representation types for varying type indices.

Indexed type families come in three flavours: data families, open type
synonym families, and closed type synonym families. They are the indexed
family variants of algebraic data types and type synonyms, respectively.
The instances of data families can be data types and newtypes.

Type families are enabled by the flag :ghc-flag:`-XTypeFamilies`. Additional
information on the use of type families in GHC is available on `the
Haskell wiki page on type
families <http://www.haskell.org/haskellwiki/GHC/Indexed_types>`__.

.. [AssocDataTypes2005]
    “`Associated Types with Class
    <http://www.cse.unsw.edu.au/~chak/papers/CKPM05.html>`__\ ”, M.
    Chakravarty, G. Keller, S. Peyton Jones,
    and S. Marlow. In Proceedings of “The 32nd Annual
    ACM SIGPLAN-SIGACT Symposium on Principles of
    Programming Languages (POPL'05)”, pages 1-13, ACM
    Press, 2005)

.. [AssocTypeSyn2005]
    “`Type Associated Type
    Synonyms <http://www.cse.unsw.edu.au/~chak/papers/CKP05.html>`__\ ”. M.
    Chakravarty, G. Keller, and S. Peyton Jones. In Proceedings of “The
    Tenth ACM SIGPLAN International Conference on Functional Programming”,
    ACM Press, pages 241-253, 2005).

.. [TypeFamilies2008]
    “\ `Type Checking with Open Type
    Functions <http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html>`__\ ”,
    T. Schrijvers, S. Peyton-Jones, M. Chakravarty, and M. Sulzmann, in
    Proceedings of “ICFP 2008: The 13th ACM SIGPLAN International Conference
    on Functional Programming”, ACM Press, pages 51-62, 2008.


.. _data-families:

Data families
-------------

Data families appear in two flavours: (1) they can be defined on the
toplevel or (2) they can appear inside type classes (in which case they
are known as associated types). The former is the more general variant,
as it lacks the requirement for the type-indexes to coincide with the
class parameters. However, the latter can lead to more clearly
structured code and compiler warnings if some type instances were -
possibly accidentally - omitted. In the following, we always discuss the
general toplevel form first and then cover the additional constraints
placed on associated types.

.. _data-family-declarations:

Data family declarations
~~~~~~~~~~~~~~~~~~~~~~~~

Indexed data families are introduced by a signature, such as ::

    data family GMap k :: * -> *

The special ``family`` distinguishes family from standard data
declarations. The result kind annotation is optional and, as usual,
defaults to ``*`` if omitted. An example is ::

    data family Array e

Named arguments can also be given explicit kind signatures if needed.
Just as with :ref:`GADT declarations <gadt>` named arguments are
entirely optional, so that we can declare ``Array`` alternatively with ::

    data family Array :: * -> *

.. _data-instance-declarations:

Data instance declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~

Instance declarations of data and newtype families are very similar to
standard data and newtype declarations. The only two differences are
that the keyword ``data`` or ``newtype`` is followed by ``instance`` and
that some or all of the type arguments can be non-variable types, but
may not contain forall types or type synonym families. However, data
families are generally allowed in type parameters, and type synonyms are
allowed as long as they are fully applied and expand to a type that is
itself admissible - exactly as this is required for occurrences of type
synonyms in class instance parameters. For example, the ``Either``
instance for ``GMap`` is ::

    data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)

In this example, the declaration has only one variant. In general, it
can be any number.

When the flag :ghc-flag:`-Wunused-type-patterns` is enabled, type
variables that are mentioned in the patterns on the left hand side, but not
used on the right hand side are reported. Variables that occur multiple times
on the left hand side are also considered used. To suppress the warnings,
unused variables should be either replaced or prefixed with underscores. Type
variables starting with an underscore (``_x``) are otherwise treated as
ordinary type variables.

This resembles the wildcards that can be used in
:ref:`partial-type-signatures`. However, there are some differences.
No error messages reporting the inferred types are generated, nor does
the flag :ghc-flag:`-XPartialTypeSignatures` have any effect.

Data and newtype instance declarations are only permitted when an
appropriate family declaration is in scope - just as a class instance
declaration requires the class declaration to be visible. Moreover, each
instance declaration has to conform to the kind determined by its family
declaration. This implies that the number of parameters of an instance
declaration matches the arity determined by the kind of the family.

A data family instance declaration can use the full expressiveness of
ordinary ``data`` or ``newtype`` declarations:

-  Although, a data family is *introduced* with the keyword "``data``",
   a data family *instance* can use either ``data`` or ``newtype``. For
   example: ::

       data family T a
       data    instance T Int  = T1 Int | T2 Bool
       newtype instance T Char = TC Bool

-  A ``data instance`` can use GADT syntax for the data constructors,
   and indeed can define a GADT. For example: ::

       data family G a b
       data instance G [a] b where
          G1 :: c -> G [Int] b
          G2 :: G [a] Bool

-  You can use a ``deriving`` clause on a ``data instance`` or
   ``newtype instance`` declaration.

Even if data families are defined as toplevel declarations, functions
that perform different computations for different family instances may
still need to be defined as methods of type classes. In particular, the
following is not possible: ::

    data family T a
    data instance T Int  = A
    data instance T Char = B
    foo :: T a -> Int
    foo A = 1
    foo B = 2

Instead, you would have to write ``foo`` as a class operation, thus: ::

    class Foo a where
      foo :: T a -> Int
    instance Foo Int where
      foo A = 1
    instance Foo Char where
      foo B = 2

Given the functionality provided by GADTs (Generalised Algebraic Data
Types), it might seem as if a definition, such as the above, should be
feasible. However, type families - in contrast to GADTs - are
*open;* i.e., new instances can always be added, possibly in other
modules. Supporting pattern matching across different data instances
would require a form of extensible case construct.

.. _data-family-overlap:

Overlap of data instances
~~~~~~~~~~~~~~~~~~~~~~~~~

The instance declarations of a data family used in a single program may
not overlap at all, independent of whether they are associated or not.
In contrast to type class instances, this is not only a matter of
consistency, but one of type safety.

.. _synonym-families:

Synonym families
----------------

Type families appear in three flavours: (1) they can be defined as open
families on the toplevel, (2) they can be defined as closed families on
the toplevel, or (3) they can appear inside type classes (in which case
they are known as associated type synonyms). Toplevel families are more
general, as they lack the requirement for the type-indexes to coincide
with the class parameters. However, associated type synonyms can lead to
more clearly structured code and compiler warnings if some type
instances were - possibly accidentally - omitted. In the following, we
always discuss the general toplevel forms first and then cover the
additional constraints placed on associated types. Note that closed
associated type synonyms do not exist.

.. _type-family-declarations:

Type family declarations
~~~~~~~~~~~~~~~~~~~~~~~~

Open indexed type families are introduced by a signature, such as ::

    type family Elem c :: *

The special ``family`` distinguishes family from standard type
declarations. The result kind annotation is optional and, as usual,
defaults to ``*`` if omitted. An example is ::

    type family Elem c

Parameters can also be given explicit kind signatures if needed. We call
the number of parameters in a type family declaration, the family's
arity, and all applications of a type family must be fully saturated
with respect to to that arity. This requirement is unlike ordinary type synonyms
and it implies that the kind of a type family is not sufficient to
determine a family's arity, and hence in general, also insufficient to
determine whether a type family application is well formed. As an
example, consider the following declaration: ::

    type family F a b :: * -> *   -- F's arity is 2,
                                  -- although its overall kind is * -> * -> * -> *

Given this declaration the following are examples of well-formed and
malformed types: ::

    F Char [Int]       -- OK!  Kind: * -> *
    F Char [Int] Bool  -- OK!  Kind: *
    F IO Bool          -- WRONG: kind mismatch in the first argument
    F Bool             -- WRONG: unsaturated application

The result kind annotation is optional and defaults to ``*`` (like
argument kinds) if omitted. Polykinded type families can be declared
using a parameter in the kind annotation: ::

    type family F a :: k

In this case the kind parameter ``k`` is actually an implicit parameter
of the type family.

.. _type-instance-declarations:

Type instance declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~

Instance declarations of type families are very similar to standard type
synonym declarations. The only two differences are that the keyword
``type`` is followed by ``instance`` and that some or all of the type
arguments can be non-variable types, but may not contain forall types or
type synonym families. However, data families are generally allowed, and
type synonyms are allowed as long as they are fully applied and expand
to a type that is admissible - these are the exact same requirements as
for data instances. For example, the ``[e]`` instance for ``Elem`` is ::

    type instance Elem [e] = e

Type arguments can be replaced with underscores (``_``) if the names of
the arguments don't matter. This is the same as writing type variables
with unique names. Unused type arguments can be replaced or prefixed
with underscores to avoid warnings when the
:ghc-flag:`-Wunused-type-patterns` flag is enabled. The same rules apply
as for :ref:`data-instance-declarations`.

Type family instance declarations are only legitimate when an
appropriate family declaration is in scope - just like class instances
require the class declaration to be visible. Moreover, each instance
declaration has to conform to the kind determined by its family
declaration, and the number of type parameters in an instance
declaration must match the number of type parameters in the family
declaration. Finally, the right-hand side of a type instance must be a
monotype (i.e., it may not include foralls) and after the expansion of
all saturated vanilla type synonyms, no synonyms, except family synonyms
may remain.

.. _closed-type-families:

Closed type families
~~~~~~~~~~~~~~~~~~~~

A type family can also be declared with a ``where`` clause, defining the
full set of equations for that family. For example: ::

    type family F a where
      F Int  = Double
      F Bool = Char
      F a    = String

A closed type family's equations are tried in order, from top to bottom,
when simplifying a type family application. In this example, we declare
an instance for ``F`` such that ``F Int`` simplifies to ``Double``,
``F Bool`` simplifies to ``Char``, and for any other type ``a`` that is
known not to be ``Int`` or ``Bool``, ``F a`` simplifies to ``String``.
Note that GHC must be sure that ``a`` cannot unify with ``Int`` or
``Bool`` in that last case; if a programmer specifies just ``F a`` in
their code, GHC will not be able to simplify the type. After all, ``a``
might later be instantiated with ``Int``.

A closed type family's equations have the same restrictions as the
equations for open type family instances.

A closed type family may be declared with no equations. Such closed type
families are opaque type-level definitions that will never reduce, are
not necessarily injective (unlike empty data types), and cannot be given
any instances. This is different from omitting the equations of a closed
type family in a ``hs-boot`` file, which uses the syntax ``where ..``,
as in that case there may or may not be equations given in the ``hs``
file.

.. _type-family-examples:

Type family examples
~~~~~~~~~~~~~~~~~~~~

Here are some examples of admissible and illegal type instances: ::

    type family F a :: *
    type instance F [Int]   = Int   -- OK!
    type instance F String  = Char  -- OK!
    type instance F (F a)   = a     -- WRONG: type parameter mentions a type family
    type instance
      F (forall a. (a, b))  = b     -- WRONG: a forall type appears in a type parameter
    type instance
      F Float = forall a.a          -- WRONG: right-hand side may not be a forall type
    type family H a where          -- OK!
      H Int  = Int
      H Bool = Bool
      H a    = String
    type instance H Char = Char    -- WRONG: cannot have instances of closed family
    type family K a where          -- OK!

    type family G a b :: * -> *
    type instance G Int            = (,)     -- WRONG: must be two type parameters
    type instance G Int Char Float = Double  -- WRONG: must be two type parameters

.. _type-family-overlap:

Compatibility and apartness of type family equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There must be some restrictions on the equations of type families, lest
we define an ambiguous rewrite system. So, equations of open type
families are restricted to be compatible. Two type patterns are
compatible if

1. all corresponding types and implicit kinds in the patterns are apart,
   or

2. the two patterns unify producing a substitution, and the right-hand
   sides are equal under that substitution.

Two types are considered apart if, for all possible substitutions, the
types cannot reduce to a common reduct.

The first clause of "compatible" is the more straightforward one. It
says that the patterns of two distinct type family instances cannot
overlap. For example, the following is disallowed: ::

    type instance F Int = Bool
    type instance F Int = Char

The second clause is a little more interesting. It says that two
overlapping type family instances are allowed if the right-hand sides
coincide in the region of overlap. Some examples help here: ::

    type instance F (a, Int) = [a]
    type instance F (Int, b) = [b]   -- overlap permitted

    type instance G (a, Int)  = [a]
    type instance G (Char, a) = [a]  -- ILLEGAL overlap, as [Char] /= [Int]

Note that this compatibility condition is independent of whether the
type family is associated or not, and it is not only a matter of
consistency, but one of type safety.

For a polykinded type family, the kinds are checked for apartness just
like types. For example, the following is accepted: ::

    type family J a :: k
    type instance J Int = Bool
    type instance J Int = Maybe

These instances are compatible because they differ in their implicit
kind parameter; the first uses ``*`` while the second uses ``* -> *``.

The definition for "compatible" uses a notion of "apart", whose
definition in turn relies on type family reduction. This condition of
"apartness", as stated, is impossible to check, so we use this
conservative approximation: two types are considered to be apart when
the two types cannot be unified, even by a potentially infinite unifier.
Allowing the unifier to be infinite disallows the following pair of
instances: ::

    type instance H x   x = Int
    type instance H [x] x = Bool

The type patterns in this pair equal if ``x`` is replaced by an infinite
nesting of lists. Rejecting instances such as these is necessary for
type soundness.

Compatibility also affects closed type families. When simplifying an
application of a closed type family, GHC will select an equation only
when it is sure that no incompatible previous equation will ever apply.
Here are some examples: ::

    type family F a where
      F Int = Bool
      F a   = Char

    type family G a where
      G Int = Int
      G a   = a

In the definition for ``F``, the two equations are incompatible -- their
patterns are not apart, and yet their right-hand sides do not coincide.
Thus, before GHC selects the second equation, it must be sure that the
first can never apply. So, the type ``F a`` does not simplify; only a
type such as ``F Double`` will simplify to ``Char``. In ``G``, on the
other hand, the two equations are compatible. Thus, GHC can ignore the
first equation when looking at the second. So, ``G a`` will simplify to
``a``.

However see :ref:`ghci-decls` for the overlap rules in GHCi.

.. _type-family-decidability:

Decidability of type synonym instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -XUndecidableInstances
    :noindex:

    Relax restrictions on the decidability of type synonym family instances.

In order to guarantee that type inference in the presence of type
families decidable, we need to place a number of additional restrictions
on the formation of type instance declarations (c.f., Definition 5
(Relaxed Conditions) of “\ `Type Checking with Open Type
Functions <http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html>`__\ ”).
Instance declarations have the general form ::

    type instance F t1 .. tn = t

where we require that for every type family application ``(G s1 .. sm)``
in ``t``,

1. ``s1 .. sm`` do not contain any type family constructors,

2. the total number of symbols (data type constructors and type
   variables) in ``s1 .. sm`` is strictly smaller than in ``t1 .. tn``,
   and

3. for every type variable ``a``, ``a`` occurs in ``s1 .. sm`` at most
   as often as in ``t1 .. tn``.

These restrictions are easily verified and ensure termination of type
inference. However, they are not sufficient to guarantee completeness of
type inference in the presence of, so called, ''loopy equalities'', such
as ``a ~ [F a]``, where a recursive occurrence of a type variable is
underneath a family application and data constructor application - see
the above mentioned paper for details.

If the option :ghc-flag:`-XUndecidableInstances` is passed to the compiler, the
above restrictions are not enforced and it is on the programmer to ensure
termination of the normalisation of type families during type inference.

.. _type-wildcards-lhs:

Wildcards on the LHS of data and type family instances
------------------------------------------------------

When the name of a type argument of a data or type instance
declaration doesn't matter, it can be replaced with an underscore
(``_``). This is the same as writing a type variable with a unique name. ::

    data family F a b :: *
    data instance F Int _ = Int
    -- Equivalent to  data instance F Int b = Int

    type family T a :: *
    type instance T (a,_) = a
    -- Equivalent to  type instance T (a,b) = a

This use of underscore for wildcard in a type pattern is exactly like
pattern matching in the term language, but is rather different to the
use of a underscore in a partial type signature (see :ref:`type-wildcards`).

A type variable beginning with an underscore is not treated specially in a
type or data instance declaration.  For example: ::

   data instance F Bool _a = _a -> Int
   -- Equivalent to  data instance F Bool a = a -> Int

Contrast this with the special treatment of named wildcards in
type signatures (:ref:`named-wildcards`).


.. _assoc-decl:

Associated data and type families
---------------------------------

A data or type synonym family can be declared as part of a type class,
thus: ::

    class GMapKey k where
      data GMap k :: * -> *
      ...

    class Collects ce where
      type Elem ce :: *
      ...

When doing so, we (optionally) may drop the "``family``" keyword.

The type parameters must all be type variables, of course, and some (but
not necessarily all) of then can be the class parameters. Each class
parameter may only be used at most once per associated type, but some
may be omitted and they may be in an order other than in the class head.
Hence, the following contrived example is admissible: ::

      class C a b c where
        type T c a x :: *

Here ``c`` and ``a`` are class parameters, but the type is also indexed
on a third parameter ``x``.

.. _assoc-data-inst:

Associated instances
~~~~~~~~~~~~~~~~~~~~

When an associated data or type synonym family instance is declared
within a type class instance, we (optionally) may drop the ``instance``
keyword in the family instance: ::

    instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
      data GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
      ...

    instance Eq (Elem [e]) => Collects [e] where
      type Elem [e] = e
      ...

The data or type family instance for an assocated type must follow
the rule that the type indexes corresponding to class parameters must have
precisely the same as type given in the instance head. For example: ::

    class Collects ce where
      type Elem ce :: *

    instance Eq (Elem [e]) => Collects [e] where
      -- Choose one of the following alternatives:
      type Elem [e] = e       -- OK
      type Elem [x] = x       -- BAD; '[x]' is different to '[e]' from head
      type Elem x   = x       -- BAD; 'x' is different to '[e]'
      type Elem [Maybe x] = x -- BAD: '[Maybe x]' is different to '[e]'

Note the following points:

-  An instance for an associated family can only appear as part of an
   instance declarations of the class in which the family was declared,
   just as with the equations of the methods of a class.

-  The variables on the right hand side of the type family equation
   must, as usual, be bound on the left hand side.

-  The instance for an associated type can be omitted in class
   instances. In that case, unless there is a default instance (see
   :ref:`assoc-decl-defs`), the corresponding instance type is not
   inhabited; i.e., only diverging expressions, such as ``undefined``,
   can assume the type.

-  Although it is unusual, there (currently) can be *multiple* instances
   for an associated family in a single instance declaration. For
   example, this is legitimate: ::

       instance GMapKey Flob where
         data GMap Flob [v] = G1 v
         data GMap Flob Int = G2 Int
         ...

   Here we give two data instance declarations, one in which the last
   parameter is ``[v]``, and one for which it is ``Int``. Since you
   cannot give any *subsequent* instances for ``(GMap Flob ...)``, this
   facility is most useful when the free indexed parameter is of a kind
   with a finite number of alternatives (unlike ``*``).

.. _assoc-decl-defs:

Associated type synonym defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible for the class defining the associated type to specify a
default for associated type instances. So for example, this is OK: ::

    class IsBoolMap v where
      type Key v
      type instance Key v = Int

      lookupKey :: Key v -> v -> Maybe Bool

    instance IsBoolMap [(Int, Bool)] where
      lookupKey = lookup

In an ``instance`` declaration for the class, if no explicit
``type instance`` declaration is given for the associated type, the
default declaration is used instead, just as with default class methods.

Note the following points:

-  The ``instance`` keyword is optional.

-  There can be at most one default declaration for an associated type
   synonym.

-  A default declaration is not permitted for an associated *data* type.

-  The default declaration must mention only type *variables* on the
   left hand side, and the right hand side must mention only type
   variables bound on the left hand side. However, unlike the associated
   type family declaration itself, the type variables of the default
   instance are independent of those of the parent class.

Here are some examples:

::

      class C a where
        type F1 a :: *
        type instance F1 a = [a]     -- OK
        type instance F1 a = a->a    -- BAD; only one default instance is allowed

        type F2 b a                  -- OK; note the family has more type
                                     --     variables than the class
        type instance F2 c d = c->d  -- OK; you don't have to use 'a' in the type instance

        type F3 a
        type F3 [b] = b              -- BAD; only type variables allowed on the LHS

        type F4 a
        type F4 b = a                -- BAD; 'a' is not in scope  in the RHS

.. _scoping-class-params:

Scoping of class parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The visibility of class parameters in the right-hand side of associated
family instances depends *solely* on the parameters of the family. As an
example, consider the simple class declaration ::

    class C a b where
      data T a

Only one of the two class parameters is a parameter to the data family.
Hence, the following instance declaration is invalid: ::

    instance C [c] d where
      data T [c] = MkT (c, d)    -- WRONG!!  'd' is not in scope

Here, the right-hand side of the data instance mentions the type
variable ``d`` that does not occur in its left-hand side. We cannot
admit such data instances as they would compromise type safety.

Instance contexts and associated type and data instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Associated type and data instance declarations do not inherit any
context specified on the enclosing instance. For type instance
declarations, it is unclear what the context would mean. For data
instance declarations, it is unlikely a user would want the context
repeated for every data constructor. The only place where the context
might likely be useful is in a ``deriving`` clause of an associated data
instance. However, even here, the role of the outer instance context is
murky. So, for clarity, we just stick to the rule above: the enclosing
instance context is ignored. If you need to use a non-trivial context on
a derived instance, use a :ghc-flag:`standalone deriving <-XStandaloneDeriving>`
clause (at the top level).

.. _data-family-import-export:

Import and export
-----------------

The rules for export lists (Haskell Report `Section
5.2 <http://www.haskell.org/onlinereport/modules.html#sect5.2>`__) needs
adjustment for type families:

-  The form ``T(..)``, where ``T`` is a data family, names the family
   ``T`` and all the in-scope constructors (whether in scope qualified
   or unqualified) that are data instances of ``T``.

-  The form ``T(.., ci, .., fj, ..)``, where ``T`` is a data family,
   names ``T`` and the specified constructors ``ci`` and fields ``fj``
   as usual. The constructors and field names must belong to some data
   instance of ``T``, but are not required to belong to the *same*
   instance.

-  The form ``C(..)``, where ``C`` is a class, names the class ``C`` and
   all its methods *and associated types*.

-  The form ``C(.., mi, .., type Tj, ..)``, where ``C`` is a class,
   names the class ``C``, and the specified methods ``mi`` and
   associated types ``Tj``. The types need a keyword "``type``" to
   distinguish them from data constructors.

-  Whenever there is no export list and a data instance is defined, the
   corresponding data family type constructor is exported along with
   the new data constructors, regardless of whether the data family
   is defined locally or in another module.

.. _data-family-impexp-examples:

Examples
~~~~~~~~

Recall our running ``GMapKey`` class example:

::

    class GMapKey k where
      data GMap k :: * -> *
      insert :: GMap k v -> k -> v -> GMap k v
      lookup :: GMap k v -> k -> Maybe v
      empty  :: GMap k v

    instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
      data GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
      ...method declarations...

Here are some export lists and their meaning:

-  ::

     module GMap( GMapKey )

   Exports just the class name.

-  ::

     module GMap( GMapKey(..) )

   Exports the class, the associated type ``GMap`` and the member functions
   ``empty``, ``lookup``, and ``insert``. The data constructors of ``GMap`` (in
   this case ``GMapEither``) are not exported.

-  ::

     module GMap( GMapKey( type GMap, empty, lookup, insert ) )

   Same as the previous item. Note the "``type``" keyword.

-  ::

     module GMap( GMapKey(..), GMap(..) )

   Same as previous item, but also exports all the data constructors for
   ``GMap``, namely
   ``GMapEither``.

-  ::

     module GMap ( GMapKey( empty, lookup, insert), GMap(..) )

   Same as previous item.

-  ::

     module GMap ( GMapKey, empty, lookup, insert, GMap(..) )

   Same as previous item.

Two things to watch out for:

-  You cannot write ``GMapKey(type GMap(..))`` — i.e., sub-component
   specifications cannot be nested. To specify ``GMap``\ 's data
   constructors, you have to list it separately.

-  Consider this example: ::

         module X where
           data family D

         module Y where
           import X
           data instance D Int = D1 | D2

   Module ``Y`` exports all the entities defined in ``Y``, namely the data
   constructors ``D1`` and ``D2``, and *implicitly* the data family ``D``,
   even though it's defined in ``X``.
   This means you can write ``import Y( D(D1,D2) )`` *without*
   giving an explicit export list like this: ::

            module Y( D(..) ) where ...
       or   module Y( module Y, D ) where ...

.. _data-family-impexp-instances:

Instances
~~~~~~~~~

Family instances are implicitly exported, just like class instances.
However, this applies only to the heads of instances, not to the data
constructors an instance defines.

.. _ty-fams-in-instances:

Type families and instance declarations
---------------------------------------

Type families require us to extend the rules for the form of instance
heads, which are given in :ref:`flexible-instance-head`. Specifically:

-  Data type families may appear in an instance head

-  Type synonym families may not appear (at all) in an instance head

The reason for the latter restriction is that there is no way to check
for instance matching. Consider

::

       type family F a
       type instance F Bool = Int

       class C a

       instance C Int
       instance C (F a)

Now a constraint ``(C (F Bool))`` would match both instances. The
situation is especially bad because the type instance for ``F Bool``
might be in another module, or even in a module that is not yet written.

However, type class instances of instances of data families can be
defined much like any other data type. For example, we can say

::

    data instance T Int = T1 Int | T2 Bool
    instance Eq (T Int) where
      (T1 i) == (T1 j) = i==j
      (T2 i) == (T2 j) = i==j
      _      == _      = False

Note that class instances are always for particular *instances* of a
data family and never for an entire family as a whole. This is for
essentially the same reasons that we cannot define a toplevel function
that performs pattern matching on the data constructors of *different*
instances of a single type family. It would require a form of extensible
case construct.

Data instance declarations can also have ``deriving`` clauses. For
example, we can write ::

    data GMap () v = GMapUnit (Maybe v)
                   deriving Show

which implicitly defines an instance of the form ::

    instance Show v => Show (GMap () v) where ...


.. _injective-ty-fams:

Injective type families
-----------------------

.. ghc-flag:: -XTypeFamilyDependencies

    :implies: :ghc-flag:`-XTypeFamilies`
    :since: 8.0.1

    Allow functional dependency annotations on type families. This allows one to
    define injective type families.

Starting with GHC 8.0 type families can be annotated with injectivity
information. This information is then used by GHC during type checking
to resolve type ambiguities in situations where a type variable appears
only under type family applications. Consider this contrived example: ::

    type family Id a
    type instance Id Int = Int
    type instance Id Bool = Bool

    id :: Id t -> Id t
    id x = x

Here the definition of ``id`` will be rejected because type variable ``t``
appears only under type family applications and is thus ambiguous.  But this
code will be accepted if we tell GHC that ``Id`` is injective, which means it
will be possible to infer ``t`` at call sites from the type of the argument: ::

    type family Id a = r | r -> a

Injective type families are enabled with ``-XTypeFamilyDependencies`` language
extension.  This extension implies ``-XTypeFamilies``.

For full details on injective type families refer to Haskell Symposium
2015 paper `Injective type families for
Haskell <http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity_extended.pdf>`__.

.. _injective-ty-fams-syntax:

Syntax of injectivity annotation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Injectivity annotation is added after type family head and consists of
two parts:

-  a type variable that names the result of a type family. Syntax:
   ``= tyvar`` or ``= (tyvar :: kind)``. Type variable must be fresh.

-  an injectivity annotation of the form ``| A -> B``, where ``A`` is the
   result type variable (see previous bullet) and ``B`` is a list of
   argument type and kind variables in which type family is injective.
   It is possible to omit some variables if type family is not injective
   in them.

Examples: ::

    type family Id a = result | result -> a where
    type family F a b c = d | d -> a c b
    type family G (a :: k) b c = foo | foo -> k b where

For open and closed type families it is OK to name the result but skip
the injectivity annotation. This is not the case for associated type
synonyms, where the named result without injectivity annotation will be
interpreted as associated type synonym default.

.. _injective-ty-fams-typecheck:

Verifying injectivity annotation against type family equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once the user declares type family to be injective GHC must verify that
this declaration is correct, ie. type family equations don't violate the
injectivity annotation. A general idea is that if at least one equation
(bullets (1), (2) and (3) below) or a pair of equations (bullets (4) and
(5) below) violates the injectivity annotation then a type family is not
injective in a way user claims and an error is reported. In the bullets
below *RHS* refers to the right-hand side of the type family equation
being checked for injectivity. *LHS* refers to the arguments of that
type family equation. Below are the rules followed when checking
injectivity of a type family:

1. If a RHS of a type family equation is a type family application GHC
   reports that the type family is not injective.

2. If a RHS of a type family equation is a bare type variable we require
   that all LHS variables (including implicit kind variables) are also
   bare. In other words, this has to be a sole equation of that type
   family and it has to cover all possible patterns. If the patterns are
   not covering GHC reports that the type family is not injective.

3. If a LHS type variable that is declared as injective is not mentioned
   on injective position
   in the RHS GHC reports that the type family is not injective.
   Injective position means either argument to a type constructor or
   injective argument to a type family.

4. *Open type families* Open type families are typechecked
   incrementally. This means that when a module is imported type family
   instances contained in that module are checked against instances
   present in already imported modules.

   A pair of an open type family equations is checked by attempting to
   unify their RHSs. If the RHSs don't unify this pair does not violate
   injectivity annotation. If unification succeeds with a substitution
   then LHSs of unified equations must be identical under that
   substitution. If they are not identical then GHC reports that the
   type family is not injective.

5. In a *closed type family* all equations are ordered and in one place.
   Equations are also checked pair-wise but this time an equation has to
   be paired with all the preceeding equations. Of course a
   single-equation closed type family is trivially injective (unless
   (1), (2) or (3) above holds).

   When checking a pair of closed type family equations GHC tried to
   unify their RHSs. If they don't unify this pair of equations does not
   violate injectivity annotation. If the RHSs can be unified under some
   substitution (possibly empty) then either the LHSs unify under the
   same substitution or the LHS of the latter equation is subsumed by
   earlier equations. If neither condition is met GHC reports that a
   type family is not injective.

Note that for the purpose of injectivity check in bullets (4) and (5)
GHC uses a special variant of unification algorithm that treats type
family applications as possibly unifying with anything.

.. _promotion:

Datatype promotion
==================

.. ghc-flag:: -XDataKinds

    :since: 7.4.1

    Allow promotion of data types to kind level.

This section describes *data type promotion*, an extension to the kind
system that complements kind polymorphism. It is enabled by
:ghc-flag:`-XDataKinds`, and described in more detail in the paper `Giving
Haskell a Promotion <http://dreixel.net/research/pdf/ghp.pdf>`__, which
appeared at TLDI 2012.

Motivation
----------

Standard Haskell has a rich type language. Types classify terms and
serve to avoid many common programming mistakes. The kind language,
however, is relatively simple, distinguishing only regular types (kind
``*``) and type constructors (e.g. kind ``* -> * -> *``).
In particular when using advanced type
system features, such as type families (:ref:`type-families`) or GADTs
(:ref:`gadt`), this simple kind system is insufficient, and fails to
prevent simple errors. Consider the example of type-level natural
numbers, and length-indexed vectors: ::

    data Ze
    data Su n

    data Vec :: * -> * -> * where
      Nil  :: Vec a Ze
      Cons :: a -> Vec a n -> Vec a (Su n)

The kind of ``Vec`` is ``* -> * -> *``. This means that, e.g.,
``Vec Int Char`` is a well-kinded type, even though this is not what we
intend when defining length-indexed vectors.

With :ghc-flag:`-XDataKinds`, the example above can then be rewritten to: ::

    data Nat = Ze | Su Nat

    data Vec :: * -> Nat -> * where
      Nil  :: Vec a 'Ze
      Cons :: a -> Vec a n -> Vec a ('Su n)

With the improved kind of ``Vec``, things like ``Vec Int Char`` are now
ill-kinded, and GHC will report an error.

Overview
--------

With :ghc-flag:`-XDataKinds`, GHC automatically promotes every datatype
to be a kind and its (value) constructors to be type constructors. The
following types ::

    data Nat = Zero | Succ Nat

    data List a = Nil | Cons a (List a)

    data Pair a b = Pair a b

    data Sum a b = L a | R b

give rise to the following kinds and type constructors (where promoted
constructors are prefixed by a tick ``'``): ::

    Nat :: *
    'Zero :: Nat
    'Succ :: Nat -> Nat

    List :: * -> *
    'Nil  :: forall k. List k
    'Cons :: forall k. k -> List k -> List k

    Pair  :: * -> * -> *
    'Pair :: forall k1 k2. k1 -> k2 -> Pair k1 k2

    Sum :: * -> * -> *
    'L :: k1 -> Sum k1 k2
    'R :: k2 -> Sum k1 k2

The following restrictions apply to promotion:

-  We promote ``data`` types and ``newtypes``; type synonyms and
   type/data families are not promoted (:ref:`type-families`).

-  We only promote types whose kinds are of the form
   ``* -> ... -> * -> *``. In particular, we do not promote
   higher-kinded datatypes such as ``data Fix f = In (f (Fix f))``, or
   datatypes whose kinds involve promoted types such as
   ``Vec :: * -> Nat -> *``.

-  We do not promote data constructors that are kind polymorphic,
   involve constraints, mention type or data families, or involve types
   that are not promotable.

The flag :ghc-flag:`-XTypeInType` (which implies :ghc-flag:`-XDataKinds`)
relaxes some of these restrictions, allowing:

-  Promotion of type synonyms and type families, but not data families.
   GHC's type theory just isn't up to the task of promoting data families,
   which requires full dependent types.

-  All datatypes, even those with rich kinds, get promoted. For example: ::

     data Proxy a = Proxy
     data App f a = MkApp (f a)   -- App :: forall k. (k -> *) -> k -> *
     x = Proxy :: Proxy ('MkApp ('Just 'True))

.. _promotion-syntax:

Distinguishing between types and constructors
---------------------------------------------

In the examples above, all promoted constructors are prefixed with a single
quote mark ``'``. This mark tells GHC to look in the data constructor namespace
for a name, not the type (constructor) namespace. Consider ::

    data P = MkP    -- 1

    data Prom = P   -- 2

We can thus distinguish the type ``P`` (which has a constructor ``MkP``)
from the promoted data constructor ``'P`` (of kind ``Prom``).

As a convenience, GHC allows you to omit the quote mark when the name is
unambiguous. However, our experience has shown that the quote mark helps
to make code more readable and less error-prone. GHC thus supports
:ghc-flag:`-Wunticked-promoted-constructors` that will warn you if you
use a promoted data constructor without a preceding quote mark.

Just as in the case of Template Haskell (:ref:`th-syntax`), GHC gets
confused if you put a quote mark before a data constructor whose second
character is a quote mark. In this case, just put a space between the
promotion quote and the data constructor: ::

  data T = A'
  type S = 'A'   -- ERROR: looks like a character
  type R = ' A'  -- OK: promoted `A'`

.. _promoted-lists-and-tuples:

Promoted list and tuple types
-----------------------------

With :ghc-flag:`-XDataKinds`, Haskell's list and tuple types are natively
promoted to kinds, and enjoy the same convenient syntax at the type
level, albeit prefixed with a quote: ::

    data HList :: [*] -> * where
      HNil  :: HList '[]
      HCons :: a -> HList t -> HList (a ': t)

    data Tuple :: (*,*) -> * where
      Tuple :: a -> b -> Tuple '(a,b)

    foo0 :: HList '[]
    foo0 = HNil

    foo1 :: HList '[Int]
    foo1 = HCons (3::Int) HNil

    foo2 :: HList [Int, Bool]
    foo2 = ...

For type-level lists of *two or more elements*, such as the signature of
``foo2`` above, the quote may be omitted because the meaning is unambiguous. But
for lists of one or zero elements (as in ``foo0`` and ``foo1``), the quote is
required, because the types ``[]`` and ``[Int]`` have existing meanings in
Haskell.

.. note::
    The declaration for ``HCons`` also requires :ghc-flag:`-XTypeOperators`
    because of infix type operator ``(:')``


.. _promotion-existentials:

Promoting existential data constructors
---------------------------------------

Note that we do promote existential data constructors that are otherwise
suitable. For example, consider the following: ::

    data Ex :: * where
      MkEx :: forall a. a -> Ex

Both the type ``Ex`` and the data constructor ``MkEx`` get promoted,
with the polymorphic kind ``'MkEx :: forall k. k -> Ex``. Somewhat
surprisingly, you can write a type family to extract the member of a
type-level existential: ::

    type family UnEx (ex :: Ex) :: k
    type instance UnEx (MkEx x) = x

At first blush, ``UnEx`` seems poorly-kinded. The return kind ``k`` is
not mentioned in the arguments, and thus it would seem that an instance
would have to return a member of ``k`` *for any* ``k``. However, this is
not the case. The type family ``UnEx`` is a kind-indexed type family.
The return kind ``k`` is an implicit parameter to ``UnEx``. The
elaborated definitions are as follows (where implicit parameters are
denoted by braces): ::

    type family UnEx {k :: *} (ex :: Ex) :: k
    type instance UnEx {k} (MkEx @k x) = x

Thus, the instance triggers only when the implicit parameter to ``UnEx``
matches the implicit parameter to ``MkEx``. Because ``k`` is actually a
parameter to ``UnEx``, the kind is not escaping the existential, and the
above code is valid.

See also :ghc-ticket:`7347`.

.. _type-in-type:
.. _kind-polymorphism:

Kind polymorphism and Type-in-Type
==================================

.. ghc-flag:: -XTypeInType

    :implies: :ghc-flag:`-XPolyKinds`, :ghc-flag:`-XDataKinds`, :ghc-flag:`-XKindSignatures`
    :since: 8.0.1

    Allow kinds to be as intricate as types, allowing explicit quantification
    over kind variables, higher-rank kinds, and the use of type synonyms and
    families in kinds, among other features.

.. ghc-flag:: -XPolyKinds

    :implies: :ghc-flag:`-XKindSignatures`
    :since: 7.4.1

    Allow kind polymorphic types.

This section describes GHC's kind system, as it appears in version 8.0 and beyond.
The kind system as described here is always in effect, with or without extensions,
although it is a conservative extension beyond standard Haskell. The extensions
above simply enable syntax and tweak the inference algorithm to allow users to
take advantage of the extra expressiveness of GHC's kind system.

The difference between :ghc-flag:`-XTypeInType` and :ghc-flag:`-XPolyKinds`
---------------------------------------------------------------------------

It is natural to consider :ghc-flag:`-XTypeInType` as an extension of
:ghc-flag:`-XPolyKinds`. The latter simply enables fewer features of GHC's
rich kind system than does the former. The need for two separate extensions
stems from their history: :ghc-flag:`-XPolyKinds` was introduced for GHC 7.4,
when it was experimental and temperamental. The wrinkles were smoothed out for
GHC 7.6. :ghc-flag:`-XTypeInType` was introduced for GHC 8.0, and is currently
experimental and temperamental, with the wrinkles to be smoothed out in due
course. The intent of having the two extensions is that users can rely on
:ghc-flag:`-XPolyKinds` to work properly while being duly sceptical of
:ghc-flag:`-XTypeInType`. In particular, we recommend enabling
:ghc-flag:`-dcore-lint` whenever using :ghc-flag:`-XTypeInType`; that flag
turns on a set of internal checks within GHC that will discover bugs in the
implementation of :ghc-flag:`-XTypeInType`. Please report bugs at `our bug
tracker <https://ghc.haskell.org/trac/ghc/wiki/ReportABug>`__.

Although we have tried to allow the new behavior only when
:ghc-flag:`-XTypeInType` is enabled, some particularly thorny cases may have
slipped through. It is thus possible that some construct is available in GHC
8.0 with :ghc-flag:`-XPolyKinds` that was not possible in GHC 7.x. If you spot
such a case, you are welcome to submit that as a bug as well. We flag
newly-available capabilities below.

Overview of kind polymorphism
-----------------------------

Consider inferring the kind for ::

  data App f a = MkApp (f a)

In Haskell 98, the inferred kind for ``App`` is ``(* -> *) -> * -> *``.
But this is overly specific, because another suitable Haskell 98 kind for
``App`` is ``((* -> *) -> *) -> (* -> *) -> *``, where the kind assigned
to ``a`` is ``* -> *``. Indeed, without kind signatures
(:ghc-flag:`-XKindSignatures`), it is necessary to use a dummy constructor
to get a Haskell compiler to infer the second kind. With kind polymorphism
(:ghc-flag:`-XPolyKinds`), GHC infers the kind ``forall k. (k -> *) -> k -> *``
for ``App``, which is its most general kind.

Thus, the chief benefit of kind polymorphism is that we can now infer these
most general kinds and use ``App`` at a variety of kinds: ::

  App Maybe Int   -- `k` is instantiated to *

  data T a = MkT (a Int)    -- `a` is inferred to have kind (* -> *)
  App T Maybe     -- `k` is instantiated to (* -> *)

Overview of Type-in-Type
------------------------

GHC 8 extends the idea of kind polymorphism by declaring that types and kinds
are indeed one and the same. Nothing within GHC distinguishes between types
and kinds. Another way of thinking about this is that the type ``Bool`` and
the "promoted kind" ``Bool`` are actually identical. (Note that term
``True`` and the type ``'True`` are still distinct, because the former can
be used in expressions and the latter in types.) This lack of distinction
between types and kinds is a hallmark of dependently typed languages.
Full dependently typed languages also remove the difference between expressions
and types, but doing that in GHC is a story for another day.

One simplification allowed by combining types and kinds is that the type
of ``*`` is just ``*``. It is true that the ``* :: *`` axiom can lead to
non-termination, but this is not a problem in GHC, as we already have other
means of non-terminating programs in both types and expressions. This
decision (among many, many others) *does* mean that despite the expressiveness
of GHC's type system, a "proof" you write in Haskell is not an irrefutable
mathematical proof. GHC promises only partial correctness, that if your
programs compile and run to completion, their results indeed have the types
assigned. It makes no claim about programs that do not finish in a finite
amount of time.

To learn more about this decision and the design of GHC under the hood
please see the `paper <http://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf>`__
introducing this kind system to GHC/Haskell.

Principles of kind inference
----------------------------

Generally speaking, when :ghc-flag:`-XPolyKinds` is on, GHC tries to infer the
most general kind for a declaration.
In this case the definition has a right-hand side to inform kind
inference. But that is not always the case. Consider ::

    type family F a

Type family declarations have no right-hand side, but GHC must still
infer a kind for ``F``. Since there are no constraints, it could infer
``F :: forall k1 k2. k1 -> k2``, but that seems *too* polymorphic. So
GHC defaults those entirely-unconstrained kind variables to ``*`` and we
get ``F :: * -> *``. You can still declare ``F`` to be kind-polymorphic
using kind signatures: ::

    type family F1 a                -- F1 :: * -> *
    type family F2 (a :: k)         -- F2 :: forall k. k -> *
    type family F3 a :: k           -- F3 :: forall k. * -> k
    type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2

The general principle is this:

-  *When there is a right-hand side, GHC infers the most polymorphic
   kind consistent with the right-hand side.* Examples: ordinary data
   type and GADT declarations, class declarations. In the case of a
   class declaration the role of "right hand side" is played by the
   class method signatures.

-  *When there is no right hand side, GHC defaults argument and result
   kinds to ``*``, except when directed otherwise by a kind signature*.
   Examples: data and open type family declarations.

This rule has occasionally-surprising consequences (see
:ghc-ticket:`10132`. ::

    class C a where    -- Class declarations are generalised
                       -- so C :: forall k. k -> Constraint
      data D1 a        -- No right hand side for these two family
      type F1 a        -- declarations, but the class forces (a :: k)
                       -- so   D1, F1 :: forall k. k -> *

    data D2 a   -- No right-hand side so D2 :: * -> *
    type F2 a   -- No right-hand side so F2 :: * -> *

The kind-polymorphism from the class declaration makes ``D1``
kind-polymorphic, but not so ``D2``; and similarly ``F1``, ``F1``.

.. index::
   single: CUSK
   single: complete user-supplied kind signature

.. _complete-kind-signatures:

Complete user-supplied kind signatures and polymorphic recursion
----------------------------------------------------------------

Just as in type inference, kind inference for recursive types can only
use *monomorphic* recursion. Consider this (contrived) example: ::

    data T m a = MkT (m a) (T Maybe (m a))
    -- GHC infers kind  T :: (* -> *) -> * -> *

The recursive use of ``T`` forced the second argument to have kind
``*``. However, just as in type inference, you can achieve polymorphic
recursion by giving a *complete user-supplied kind signature* (or CUSK)
for ``T``. A CUSK is present when all argument kinds and the result kind
are known, without any need for inference. For example: ::

    data T (m :: k -> *) :: k -> * where
      MkT :: m a -> T Maybe (m a) -> T m a

The complete user-supplied kind signature specifies the polymorphic kind
for ``T``, and this signature is used for all the calls to ``T``
including the recursive ones. In particular, the recursive use of ``T``
is at kind ``*``.

What exactly is considered to be a "complete user-supplied kind
signature" for a type constructor? These are the forms:

-  For a datatype, every type variable must be annotated with a kind. In
   a GADT-style declaration, there may also be a kind signature (with a
   top-level ``::`` in the header), but the presence or absence of this
   annotation does not affect whether or not the declaration has a
   complete signature. ::

       data T1 :: (k -> *) -> k -> *       where ...
       -- Yes;  T1 :: forall k. (k->*) -> k -> *

       data T2 (a :: k -> *) :: k -> *     where ...
       -- Yes;  T2 :: forall k. (k->*) -> k -> *

       data T3 (a :: k -> *) (b :: k) :: * where ...
       -- Yes;  T3 :: forall k. (k->*) -> k -> *

       data T4 (a :: k -> *) (b :: k)      where ...
       -- Yes;  T4 :: forall k. (k->*) -> k -> *

       data T5 a (b :: k) :: *             where ...
       -- No;  kind is inferred

       data T6 a b                         where ...
       -- No;  kind is inferred

-  For a datatype with a top-level ``::`` when :ghc-flag:`-XTypeInType`
   is in effect: all kind variables introduced after the ``::`` must
   be explicitly quantified. ::

     -- -XTypeInType is on
     data T1 :: k -> *            -- No CUSK: `k` is not explicitly quantified
     data T2 :: forall k. k -> *  -- CUSK: `k` is bound explicitly
     data T3 :: forall (k :: *). k -> *   -- still a CUSK

   Note that the first example would indeed have a CUSK without
   :ghc-flag:`-XTypeInType`.

-  For a class, every type variable must be annotated with a kind.

-  For a type synonym, every type variable and the result type must all
   be annotated with kinds: ::

       type S1 (a :: k) = (a :: k)    -- Yes   S1 :: forall k. k -> k
       type S2 (a :: k) = a           -- No    kind is inferred
       type S3 (a :: k) = Proxy a     -- No    kind is inferred

   Note that in ``S2`` and ``S3``, the kind of the right-hand side is
   rather apparent, but it is still not considered to have a complete
   signature -- no inference can be done before detecting the signature.

-  An un-associated open type or data family declaration *always* has a CUSK;
   un-annotated type variables default to
   kind ``*``: ::

       data family D1 a               -- D1 :: * -> *
       data family D2 (a :: k)        -- D2 :: forall k. k -> *
       data family D3 (a :: k) :: *   -- D3 :: forall k. k -> *
       type family S1 a :: k -> *     -- S1 :: forall k. * -> k -> *

-  An associated type or data family declaration has a CUSK precisely if
   its enclosing class has a CUSK. ::

       class C a where                -- no CUSK
         type AT a b                  -- no CUSK, b is defaulted

       class D (a :: k) where         -- yes CUSK
         type AT2 a b                 -- yes CUSK, b is defaulted

-  A closed type family has a complete signature when all of its type
   variables are annotated and a return kind (with a top-level ``::``)
   is supplied.

With :ghc-flag:`-XTypeInType` enabled, it is possible to write a datatype
that syntactically has a CUSK (according to the rules above)
but actually requires some inference. As a very contrived example, consider ::

  data Proxy a           -- Proxy :: forall k. k -> *
  data X (a :: Proxy k)

According to the rules above ``X`` has a CUSK. Yet, what is the kind of ``k``?
It is impossible to know. This code is thus rejected as masquerading as having
a CUSK, but not really. If you wish ``k`` to be polykinded, it is straightforward
to specify this: ::

  data X (a :: Proxy (k1 :: k2))

The above definition is indeed fully fixed, with no masquerade.

Kind inference in closed type families
--------------------------------------

Although all open type families are considered to have a complete
user-supplied kind signature, we can relax this condition for closed
type families, where we have equations on which to perform kind
inference. GHC will infer kinds for the arguments and result types of a
closed type family.

GHC supports *kind-indexed* type families, where the family matches both
on the kind and type. GHC will *not* infer this behaviour without a
complete user-supplied kind signature, as doing so would sometimes infer
non-principal types. Indeed, we can see kind-indexing as a form
of polymorphic recursion, where a type is used at a kind other than
its most general in its own definition.

For example: ::

    type family F1 a where
      F1 True  = False
      F1 False = True
      F1 x     = x
    -- F1 fails to compile: kind-indexing is not inferred

    type family F2 (a :: k) where
      F2 True  = False
      F2 False = True
      F2 x     = x
    -- F2 fails to compile: no complete signature

    type family F3 (a :: k) :: k where
      F3 True  = False
      F3 False = True
      F3 x     = x
    -- OK

Kind inference in class instance declarations
---------------------------------------------

Consider the following example of a poly-kinded class and an instance
for it: ::

    class C a where
      type F a

    instance C b where
      type F b = b -> b

In the class declaration, nothing constrains the kind of the type ``a``,
so it becomes a poly-kinded type variable ``(a :: k)``. Yet, in the
instance declaration, the right-hand side of the associated type
instance ``b -> b`` says that ``b`` must be of kind ``*``. GHC could
theoretically propagate this information back into the instance head,
and make that instance declaration apply only to type of kind ``*``, as
opposed to types of any kind. However, GHC does *not* do this.

In short: GHC does *not* propagate kind information from the members of
a class instance declaration into the instance declaration head.

This lack of kind inference is simply an engineering problem within GHC,
but getting it to work would make a substantial change to the inference
infrastructure, and it's not clear the payoff is worth it. If you want
to restrict ``b``\ 's kind in the instance above, just use a kind
signature in the instance head.

Kind inference in type signatures
---------------------------------

When kind-checking a type, GHC considers only what is written in that
type when figuring out how to generalise the type's kind.

For example,
consider these definitions (with :ghc-flag:`-XScopedTypeVariables`): ::

  data Proxy a    -- Proxy :: forall k. k -> *
  p :: forall a. Proxy a
  p = Proxy :: Proxy (a :: *)

GHC reports an error, saying that the kind of ``a`` should be a kind variable
``k``, not ``*``. This is because, by looking at the type signature
``forall a. Proxy a``, GHC assumes ``a``'s kind should be generalised, not
restricted to be ``*``. The function definition is then rejected for being
more specific than its type signature.

Explicit kind quantification
----------------------------

Enabled by :ghc-flag:`-XTypeInType`, GHC now supports explicit kind quantification,
as in these examples: ::

  data Proxy :: forall k. k -> *
  f :: (forall k (a :: k). Proxy a -> ()) -> Int

Note that the second example has a ``forall`` that binds both a kind ``k`` and
a type variable ``a`` of kind ``k``. In general, there is no limit to how
deeply nested this sort of dependency can work. However, the dependency must
be well-scoped: ``forall (a :: k) k. ...`` is an error.

For backward compatibility, kind variables *do not* need to be bound explicitly,
even if the type starts with ``forall``.

Accordingly, the rule for kind quantification in higher-rank contexts has
changed slightly. In GHC 7, if a kind variable was mentioned for the first
time in the kind of a variable bound in a non-top-level ``forall``, the kind
variable was bound there, too.
That is, in ``f :: (forall (a :: k). ...) -> ...``, the ``k`` was bound
by the same ``forall`` as the ``a``. In GHC 8, however, all kind variables
mentioned in a type are bound at the outermost level. If you want one bound
in a higher-rank ``forall``, include it explicitly.

Kind-indexed GADTs
------------------

Consider the type ::

  data G (a :: k) where
    GInt    :: G Int
    GMaybe  :: G Maybe

This datatype ``G`` is GADT-like in both its kind and its type. Suppose you
have ``g :: G a``, where ``a :: k``. Then pattern matching to discover that
``g`` is in fact ```GMaybe`` tells you both that ``k ~ (* -> *)`` and
``a ~ Maybe``. The definition for ``G`` requires that :ghc-flag:`-XTypeInType`
be in effect, but pattern-matching on ``G`` requires no extension beyond
:ghc-flag:`-XGADTs`. That this works is actually a straightforward extension
of regular GADTs and a consequence of the fact that kinds and types are the
same.

Note that the datatype ``G`` is used at different kinds in its body, and
therefore that kind-indexed GADTs use a form of polymorphic recursion.
It is thus only possible to use this feature if you have provided a
complete user-supplied kind signature
for the datatype (:ref:`complete-kind-signatures`).

Constraints in kinds
--------------------

As kinds and types are the same, kinds can now (with :ghc-flag:`-XTypeInType`)
contain type constraints. Only equality constraints are currently supported,
however. We expect this to extend to other constraints in the future.

Here is an example of a constrained kind: ::

  type family IsTypeLit a where
    IsTypeLit Nat    = 'True
    IsTypeLit Symbol = 'True
    IsTypeLit a      = 'False

  data T :: forall a. (IsTypeLit a ~ 'True) => a -> * where
    MkNat    :: T 42
    MkSymbol :: T "Don't panic!"

The declarations above are accepted. However, if we add ``MkOther :: T Int``,
we get an error that the equality constraint is not satisfied; ``Int`` is
not a type literal. Note that explicitly quantifying with ``forall a`` is
not necessary here.

The kind ``*``
--------------

The kind ``*`` classifies ordinary types. Without :ghc-flag:`-XTypeInType`,
this identifier is always in scope when writing a kind. However, with
:ghc-flag:`-XTypeInType`, a user may wish to use ``*`` in a type or a
type operator ``*`` in a kind. To make this all more manageable, ``*``
becomes an (almost) ordinary name with :ghc-flag:`-XTypeInType` enabled.
So as not to cause naming collisions, it is not imported by default;
you must ``import Data.Kind`` to get ``*`` (but only with :ghc-flag:`-XTypeInType`
enabled).

The only way ``*`` is unordinary is in its parsing. In order to be backward
compatible, ``*`` is parsed as if it were an alphanumeric idenfifier; note
that we do not write ``Int :: (*)`` but just plain ``Int :: *``. Due to the
bizarreness with which ``*`` is parsed--and the fact that it is the only such
operator in GHC--there are some corner cases that are
not handled. We are aware of three:

- In a Haskell-98-style data constructor, you must put parentheses around
  ``*``, like this: ::

    data Universe = Ty (*) | Num Int | ...

- In an import/export list, you must put parentheses around ``*``, like this: ::

    import Data.Kind ( type (*) )

  Note that the keyword ``type`` there is just to disambiguate the import
  from a term-level ``(*)``. (:ref:`explicit-namespaces`)

- In an instance declaration head (the part after the word ``instance``), you
  must parenthesize ``*``. This applies to all manners of instances, including
  the left-hand sides of individual equations of a closed type family.

The ``Data.Kind`` module also exports ``Type`` as a synonym for ``*``.
Now that type synonyms work in kinds, it is conceivable that we will deprecate
``*`` when there is a good migration story for everyone to use ``Type``.
If you like neither of these names, feel free to write your own synonym: ::

  type Set = *   -- silly Agda programmers...

All the affordances for ``*`` also apply to ``★``, the Unicode variant
of ``*``.

Inferring dependency in datatype declarations
---------------------------------------------

If a type variable ``a`` in a datatype, class, or type family declaration
depends on another such variable ``k`` in the same declaration, two properties
must hold:

-  ``a`` must appear after ``k`` in the declaration, and

-  ``k`` must appear explicitly in the kind of *some* type variable in that
   declaration.

The first bullet simply means that the dependency must be well-scoped. The
second bullet concerns GHC's ability to infer dependency. Inferring this
dependency is difficult, and GHC currently requires the dependency to be
made explicit, meaning that ``k`` must appear in the kind of a type variable,
making it obvious to GHC that dependency is intended. For example: ::

  data Proxy k (a :: k)            -- OK: dependency is "obvious"
  data Proxy2 k a = P (Proxy k a)  -- ERROR: dependency is unclear

In the second declaration, GHC cannot immediately tell that ``k`` should
be a dependent variable, and so the declaration is rejected.

It is conceivable that this restriction will be relaxed in the future,
but it is (at the time of writing) unclear if the difficulties around this
scenario are theoretical (inferring this dependency would mean our type
system does not have principal types) or merely practical (inferring this
dependency is hard, given GHC's implementation). So, GHC takes the easy
way out and requires a little help from the user.

Kind defaulting without :ghc-flag:`-XPolyKinds`
-----------------------------------------------

Without :ghc-flag:`-XPolyKinds` or :ghc-flag:`-XTypeInType` enabled, GHC
refuses to generalise over kind variables. It thus defaults kind variables
to ``*`` when possible; when this is not possible, an error is issued.

Here is an example of this in action: ::

  {-# LANGUAGE TypeInType #-}
  data Proxy a = P   -- inferred kind: Proxy :: k -> *
  data Compose f g x = MkCompose (f (g x))
    -- inferred kind: Compose :: (b -> *) -> (a -> b) -> a -> *

  -- separate module having imported the first
  {-# LANGUAGE NoPolyKinds, DataKinds #-}
  z = Proxy :: Proxy 'MkCompose

In the last line, we use the promoted constructor ``'MkCompose``, which has
kind ::

  forall (a :: *) (b :: *) (f :: b -> *) (g :: a -> b) (x :: a).
    f (g x) -> Compose f g x

Now we must infer a type for ``z``. To do so without generalising over kind
variables, we must default the kind variables of ``'MkCompose``. We can
easily default ``a`` and ``b`` to ``*``, but ``f`` and ``g`` would be ill-kinded
if defaulted. The definition for ``z`` is thus an error.

Pretty-printing in the presence of kind polymorphism
----------------------------------------------------

With kind polymorphism, there is quite a bit going on behind the scenes that
may be invisible to a Haskell programmer. GHC supports several flags that
control how types are printed in error messages and at the GHCi prompt.
See the :ref:`discussion of type pretty-printing options <pretty-printing-types>`
for further details. If you are using kind polymorphism and are confused as to
why GHC is rejecting (or accepting) your program, we encourage you to turn on
these flags, especially :ghc-flag:`-fprint-explicit-kinds`.

.. index::
   single: TYPE
   single: levity polymorphism

.. _runtime-rep:

Levity polymorphism
===================

In order to allow full flexibility in how kinds are used, it is necessary
to use the kind system to differentiate between boxed, lifted types
(normal, everyday types like ``Int`` and ``[Bool]``) and unboxed, primitive
types (:ref:`primitives`) like ``Int#``. We thus have so-called levity
polymorphism.

Here are the key definitions, all available from ``GHC.Exts``: ::

  TYPE :: RuntimeRep -> *   -- highly magical, built into GHC

  data RuntimeRep = LiftedRep     -- for things like `Int`
                  | UnliftedRep   -- for things like `Array#`
                  | IntRep        -- for `Int#`
		  | TupleRep [RuntimeRep]  -- unboxed tuples, indexed by the representations of the elements
		  | SumRep [RuntimeRep]    -- unboxed sums, indexed by the representations of the disjuncts
                  | ...

  type * = TYPE LiftedRep    -- * is just an ordinary type synonym

The idea is that we have a new fundamental type constant ``TYPE``, which
is parameterised by a ``RuntimeRep``. We thus get ``Int# :: TYPE 'IntRep``
and ``Bool :: TYPE 'LiftedRep``. Anything with a type of the form
``TYPE x`` can appear to either side of a function arrow ``->``. We can
thus say that ``->`` has type
``TYPE r1 -> TYPE r2 -> TYPE 'LiftedRep``. The result is always lifted
because all functions are lifted in GHC.

No levity-polymorphic variables or arguments
--------------------------------------------

If GHC didn't have to compile programs that run in the real world, that
would be the end of the story. But representation polymorphism can cause
quite a bit of trouble for GHC's code generator. Consider ::

  bad :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                (a :: TYPE r1) (b :: TYPE r2).
         (a -> b) -> a -> b
  bad f x = f x

This seems like a generalisation of the standard ``$`` operator. If we
think about compiling this to runnable code, though, problems appear.
In particular, when we call ``bad``, we must somehow pass ``x`` into
``bad``. How wide (that is, how many bits) is ``x``? Is it a pointer?
What kind of register (floating-point or integral) should ``x`` go in?
It's all impossible to say, because ``x``'s type, ``a :: TYPE r1`` is
levity polymorphic. We thus forbid such constructions, via the
following straightforward rule:

    No variable may have a levity-polymorphic type.

This eliminates ``bad`` because the variable ``x`` would have a
representation-polymorphic type.

However, not all is lost. We can still do this: ::

  ($) :: forall r (a :: *) (b :: TYPE r).
         (a -> b) -> a -> b
  f $ x = f x

Here, only ``b`` is levity polymorphic. There are no variables
with a levity-polymorphic type. And the code generator has no
trouble with this. Indeed, this is the true type of GHC's ``$`` operator,
slightly more general than the Haskell 98 version.

Because the code generator must store and move arguments as well
as variables, the logic above applies equally well to function arguments,
which may not be levity-polymorphic.


Levity-polymorphic bottoms
--------------------------

We can use levity polymorphism to good effect with ``error``
and ``undefined``, whose types are given here: ::

  undefined :: forall (r :: RuntimeRep) (a :: TYPE r).
               HasCallStack => a
  error :: forall (r :: RuntimeRep) (a :: TYPE r).
           HasCallStack => String -> a

These functions do not bind a levity-polymorphic variable, and
so are accepted. Their polymorphism allows users to use these to conveniently
stub out functions that return unboxed types.

Printing levity-polymorphic types
---------------------------------

.. ghc-flag:: -Wprint-explicit-runtime-rep

  Print ``RuntimeRep`` parameters as they appear; otherwise, they are
  defaulted to ``'LiftedRep``.

Most GHC users will not need to worry about levity polymorphism
or unboxed types. For these users, seeing the levity polymorphism
in the type of ``$`` is unhelpful. And thus, by default, it is suppressed,
by supposing all type variables of type ``RuntimeRep`` to be ``'LiftedRep``
when printing, and printing ``TYPE 'LiftedRep`` as ``*``.

Should you wish to see levity polymorphism in your types, enable
the flag :ghc-flag:`-fprint-explicit-runtime-reps`.

.. _type-level-literals:

Type-Level Literals
===================

GHC supports numeric and string literals at the type level, giving
convenient access to a large number of predefined type-level constants.
Numeric literals are of kind ``Nat``, while string literals are of kind
``Symbol``. This feature is enabled by the :ghc-flag:`-XDataKinds` language
extension.

The kinds of the literals and all other low-level operations for this
feature are defined in module ``GHC.TypeLits``. Note that the module
defines some type-level operators that clash with their value-level
counterparts (e.g. ``(+)``). Import and export declarations referring to
these operators require an explicit namespace annotation (see
:ref:`explicit-namespaces`).

Here is an example of using type-level numeric literals to provide a
safe interface to a low-level function: ::

    import GHC.TypeLits
    import Data.Word
    import Foreign

    newtype ArrPtr (n :: Nat) a = ArrPtr (Ptr a)

    clearPage :: ArrPtr 4096 Word8 -> IO ()
    clearPage (ArrPtr p) = ...

Here is an example of using type-level string literals to simulate
simple record operations: ::

    data Label (l :: Symbol) = Get

    class Has a l b | a l -> b where
      from :: a -> Label l -> b

    data Point = Point Int Int deriving Show

    instance Has Point "x" Int where from (Point x _) _ = x
    instance Has Point "y" Int where from (Point _ y) _ = y

    example = from (Point 1 2) (Get :: Label "x")


.. _typelit-runtime:

Runtime Values for Type-Level Literals
--------------------------------------

Sometimes it is useful to access the value-level literal associated with
a type-level literal. This is done with the functions ``natVal`` and
``symbolVal``. For example: ::

    GHC.TypeLits> natVal (Proxy :: Proxy 2)
    2

These functions are overloaded because they need to return a different
result, depending on the type at which they are instantiated. ::

    natVal :: KnownNat n => proxy n -> Integer

    -- instance KnownNat 0
    -- instance KnownNat 1
    -- instance KnownNat 2
    -- ...

GHC discharges the constraint as soon as it knows what concrete
type-level literal is being used in the program. Note that this works
only for *literals* and not arbitrary type expressions. For example, a
constraint of the form ``KnownNat (a + b)`` will *not* be simplified to
``(KnownNat a, KnownNat b)``; instead, GHC will keep the constraint as
is, until it can simplify ``a + b`` to a constant value.

It is also possible to convert a run-time integer or string value to the
corresponding type-level literal. Of course, the resulting type literal
will be unknown at compile-time, so it is hidden in an existential type.
The conversion may be performed using ``someNatVal`` for integers and
``someSymbolVal`` for strings: ::

    someNatVal :: Integer -> Maybe SomeNat
    SomeNat    :: KnownNat n => Proxy n -> SomeNat

The operations on strings are similar.

.. _typelit-tyfuns:

Computing With Type-Level Naturals
----------------------------------

GHC 7.8 can evaluate arithmetic expressions involving type-level natural
numbers. Such expressions may be constructed using the type-families
``(+), (*), (^)`` for addition, multiplication, and exponentiation.
Numbers may be compared using ``(<=?)``, which returns a promoted
boolean value, or ``(<=)``, which compares numbers as a constraint. For
example:

.. code-block:: none

    GHC.TypeLits> natVal (Proxy :: Proxy (2 + 3))
    5

At present, GHC is quite limited in its reasoning about arithmetic: it
will only evaluate the arithmetic type functions and compare the
results--- in the same way that it does for any other type function. In
particular, it does not know more general facts about arithmetic, such
as the commutativity and associativity of ``(+)``, for example.

However, it is possible to perform a bit of "backwards" evaluation. For
example, here is how we could get GHC to compute arbitrary logarithms at
the type level:

.. code-block:: none

    lg :: Proxy base -> Proxy (base ^ pow) -> Proxy pow
    lg _ _ = Proxy

    GHC.TypeLits> natVal (lg (Proxy :: Proxy 2) (Proxy :: Proxy 8))
    3

Constraints in types
====================

.. _equality-constraints:

Equality constraints
--------------------

A type context can include equality constraints of the form ``t1 ~ t2``,
which denote that the types ``t1`` and ``t2`` need to be the same. In
the presence of type families, whether two types are equal cannot
generally be decided locally. Hence, the contexts of function signatures
may include equality constraints, as in the following example: ::

    sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2

where we require that the element type of ``c1`` and ``c2`` are the
same. In general, the types ``t1`` and ``t2`` of an equality constraint
may be arbitrary monotypes; i.e., they may not contain any quantifiers,
independent of whether higher-rank types are otherwise enabled.

Equality constraints can also appear in class and instance contexts. The
former enable a simple translation of programs using functional
dependencies into programs using family synonyms instead. The general
idea is to rewrite a class declaration of the form ::

    class C a b | a -> b

to ::

    class (F a ~ b) => C a b where
      type F a

That is, we represent every functional dependency (FD) ``a1 .. an -> b``
by an FD type family ``F a1 .. an`` and a superclass context equality
``F a1 .. an ~ b``, essentially giving a name to the functional
dependency. In class instances, we define the type instances of FD
families in accordance with the class head. Method signatures are not
affected by that process.

.. index::
   pair: Type equality constraints; kind heterogeneous

Heterogeneous equality
----------------------

GHC also supports *kind-heterogeneous* equality, which relates two types of
potentially different kinds. Heterogeneous equality is spelled ``~~``. Here
are the kinds of ``~`` and ``~~`` to better understand their difference: ::

  (~)  :: forall k. k -> k -> Constraint
  (~~) :: forall k1 k2. k1 -> k2 -> Constraint

Users will most likely want ``~``, but ``~~`` is available if GHC cannot know,
a priori, that the two types of interest have the same kind. Evidence that
``(a :: k1) ~~ (b :: k2)`` tells GHC both that ``k1`` and ``k2`` are the same
and that ``a`` and ``b`` are the same.

Because ``~`` is the more common equality relation, GHC prints out ``~~`` like
``~`` unless :ghc-flag:`-fprint-equality-relations` is set.

Unlifted heterogeneous equality
-------------------------------

Internal to GHC is yet a third equality relation ``(~#)``. It is heterogeneous
(like ``~~``) and is used only internally. It may appear in error messages
and other output only when :ghc-flag:`-fprint-equality-relations` is enabled.

.. _coercible:

The ``Coercible`` constraint
----------------------------

The constraint ``Coercible t1 t2`` is similar to ``t1 ~ t2``, but
denotes representational equality between ``t1`` and ``t2`` in the sense
of Roles (:ref:`roles`). It is exported by :base-ref:`Data.Coerce.`, which also
contains the documentation. More details and discussion can be found in the
paper
`"Safe Coercions" <http://www.cis.upenn.edu/~eir/papers/2014/coercible/coercible.pdf>`__.

.. _constraint-kind:

The ``Constraint`` kind
-----------------------

.. ghc-flag:: -XConstraintKinds

    :since: 7.4.1

    Allow types of kind ``Constraint`` to be used in contexts.

Normally, *constraints* (which appear in types to the left of the ``=>``
arrow) have a very restricted syntax. They can only be:

-  Class constraints, e.g. ``Show a``

-  :ghc-flag:`Implicit parameter <-XImplicitParams>` constraints, e.g.
   ``?x::Int`` (with the :ghc-flag:`-XImplicitParams` flag)

-  :ref:`Equality constraints <equality-constraints>`, e.g. ``a ~ Int``
   (with the :ghc-flag:`-XTypeFamilies` or :ghc-flag:`-XGADTs` flag)

With the :ghc-flag:`-XConstraintKinds` flag, GHC becomes more liberal in what it
accepts as constraints in your program. To be precise, with this flag
any *type* of the new kind ``Constraint`` can be used as a constraint.
The following things have kind ``Constraint``:

-  Anything which is already valid as a constraint without the flag:
   saturated applications to type classes, implicit parameter and
   equality constraints.

- Tuples, all of whose component types have kind ``Constraint``. So for example
  the type ``(Show a, Ord a)`` is of kind ``Constraint``.

-  Anything whose form is not yet known, but the user has declared to
   have kind ``Constraint`` (for which they need to import it from
   ``GHC.Exts``). So for example
   ``type Foo (f :: \* -> Constraint) = forall b. f b => b -> b``
   is allowed, as well as examples involving type families: ::

       type family Typ a b :: Constraint
       type instance Typ Int  b = Show b
       type instance Typ Bool b = Num b

       func :: Typ a b => a -> b -> b
       func = ...

Note that because constraints are just handled as types of a particular
kind, this extension allows type constraint synonyms: ::

    type Stringy a = (Read a, Show a)
    foo :: Stringy a => a -> (String, String -> a)
    foo x = (show x, read)

Presently, only standard constraints, tuples and type synonyms for those
two sorts of constraint are permitted in instance contexts and
superclasses (without extra flags). The reason is that permitting more
general constraints can cause type checking to loop, as it would with
these two programs:

::

    type family Clsish u a
    type instance Clsish () a = Cls a
    class Clsish () a => Cls a where

::

    class OkCls a where

    type family OkClsish u a
    type instance OkClsish () a = OkCls a
    instance OkClsish () a => OkCls a where

You may write programs that use exotic sorts of constraints in instance
contexts and superclasses, but to do so you must use
:ghc-flag:`-XUndecidableInstances` to signal that you don't mind if the type
checker fails to terminate.

.. _extensions-to-type-signatures:

Extensions to type signatures
=============================

.. _explicit-foralls:

Explicit universal quantification (forall)
------------------------------------------

.. ghc-flag:: -XExplicitForAll

    :since: 6.12

    Allow use of the ``forall`` keyword in places where universal quantification
    is implicit.

Haskell type signatures are implicitly quantified. When the language
option :ghc-flag:`-XExplicitForAll` is used, the keyword ``forall`` allows us to
say exactly what this means. For example: ::

    g :: b -> b

means this: ::

    g :: forall b. (b -> b)

The two are treated identically, except that the latter may bring type variables
into scope (see :ref:`scoped-type-variables`).

Notes:

- With :ghc-flag:`-XExplicitForAll`, ``forall`` becomes a keyword; you can't use ``forall`` as a
  type variable any more!

- As well in type signatures, you can also use an explicit ``forall``
  in an instance declaration: ::

      instance forall a. Eq a => Eq [a] where ...

- If the :ghc-flag:`-Wunused-foralls` flag is enabled, a warning will be emitted
  when you write a type variable in an explicit ``forall`` statement that is
  otherwise unused. For instance: ::

    g :: forall a b. (b -> b)

  would warn about the unused type variable `a`.

.. _flexible-contexts:

The context of a type signature
-------------------------------

The :ghc-flag:`-XFlexibleContexts` flag lifts the Haskell 98 restriction that
the type-class constraints in a type signature must have the form *(class
type-variable)* or *(class (type-variable type1 type2 ... typen))*. With
:ghc-flag:`-XFlexibleContexts` these type signatures are perfectly okay
::

      g :: Eq [a] => ...
      g :: Ord (T a ()) => ...

The flag :ghc-flag:`-XFlexibleContexts` also lifts the corresponding restriction
on class declarations (:ref:`superclass-rules`) and instance
declarations (:ref:`instance-rules`).

.. _ambiguity:

Ambiguous types and the ambiguity check
---------------------------------------

.. ghc-flag:: -XAllowAmbiguousTypes

    :since: 7.8.1

    Allow type signatures which appear that they would result in
    an unusable binding.

Each user-written type signature is subjected to an *ambiguity check*.
The ambiguity check rejects functions that can never be called; for
example: ::

       f :: C a => Int

The idea is there can be no legal calls to ``f`` because every call will
give rise to an ambiguous constraint. Indeed, the *only* purpose of the
ambiguity check is to report functions that cannot possibly be called.
We could soundly omit the ambiguity check on type signatures entirely,
at the expense of delaying ambiguity errors to call sites. Indeed, the
language extension :ghc-flag:`-XAllowAmbiguousTypes` switches off the ambiguity
check.

Ambiguity can be subtle. Consider this example which uses functional
dependencies: ::

       class D a b | a -> b where ..
       h :: D Int b => Int

The ``Int`` may well fix ``b`` at the call site, so that signature
should not be rejected. Moreover, the dependencies might be hidden.
Consider ::

       class X a b where ...
       class D a b | a -> b where ...
       instance D a b => X [a] b where...
       h :: X a b => a -> a

Here ``h``\'s type looks ambiguous in ``b``, but here's a legal call: ::

       ...(h [True])...

That gives rise to a ``(X [Bool] beta)`` constraint, and using the
instance means we need ``(D Bool beta)`` and that fixes ``beta`` via
``D``\'s fundep!

Behind all these special cases there is a simple guiding principle.
Consider ::

      f :: type
      f = ...blah...

      g :: type
      g = f

You would think that the definition of ``g`` would surely typecheck!
After all ``f`` has exactly the same type, and ``g=f``. But in fact
``f``\'s type is instantiated and the instantiated constraints are solved
against the constraints bound by ``g``\ 's signature. So, in the case an
ambiguous type, solving will fail. For example, consider the earlier
definition ``f :: C a => Int``: ::

      f :: C a => Int
      f = ...blah...

      g :: C a => Int
      g = f

In ``g``\'s definition, we'll instantiate to ``(C alpha)`` and try to
deduce ``(C alpha)`` from ``(C a)``, and fail.

So in fact we use this as our *definition* of ambiguity: a type ``ty``
is ambiguous if and only if ``((undefined :: ty) :: ty)`` would fail to
typecheck. We use a very similar test for *inferred* types, to ensure
that they too are unambiguous.

*Switching off the ambiguity check.* Even if a function has an
ambiguous type according the "guiding principle", it is possible that
the function is callable. For example: ::

      class D a b where ...
      instance D Bool b where ...

      strange :: D a b => a -> a
      strange = ...blah...

      foo = strange True

Here ``strange``\'s type is ambiguous, but the call in ``foo`` is OK
because it gives rise to a constraint ``(D Bool beta)``, which is
soluble by the ``(D Bool b)`` instance.

Another way of getting rid of the ambiguity at the call site is to use
the :ghc-flag:`-XTypeApplications` flag to specify the types. For example: ::

      class D a b where
        h :: b
      instance D Int Int where ...

      main = print (h @Int @Int)

Here ``a`` is ambiguous in the definition of ``D`` but later specified
to be `Int` using type applications.

:ghc-flag:`-XAllowAmbiguousTypes` allows you to switch off the ambiguity check.
However, even with ambiguity checking switched off, GHC will complain about a
function that can *never* be called, such as this one: ::

      f :: (Int ~ Bool) => a -> a

.. note::
    *A historical note.* GHC used to impose some more restrictive and less
    principled conditions on type signatures. For type
    ``forall tv1..tvn (c1, ...,cn) => type`` GHC used to require

     a. that each universally quantified type variable ``tvi`` must be "reachable"
        from ``type``, and

     b. that every constraint ``ci`` mentions at least one of the universally
        quantified type variables ``tvi``. These ad-hoc restrictions are
        completely subsumed by the new ambiguity check.

.. _kinding:

Explicitly-kinded quantification
--------------------------------

.. ghc-flag:: -XKindSignatures

    Allow explicit kind signatures on type variables.

Haskell infers the kind of each type variable. Sometimes it is nice to
be able to give the kind explicitly as (machine-checked) documentation,
just as it is nice to give a type signature for a function. On some
occasions, it is essential to do so. For example, in his paper
"Restricted Data Types in Haskell" (Haskell Workshop 1999) John Hughes
had to define the data type: ::

    data Set cxt a = Set [a]
                   | Unused (cxt a -> ())

The only use for the ``Unused`` constructor was to force the correct
kind for the type variable ``cxt``.

GHC now instead allows you to specify the kind of a type variable
directly, wherever a type variable is explicitly bound, with the flag
:ghc-flag:`-XKindSignatures`.

This flag enables kind signatures in the following places:

-  ``data`` declarations: ::

         data Set (cxt :: * -> *) a = Set [a]

-  ``type`` declarations: ::

         type T (f :: * -> *) = f Int

-  ``class`` declarations: ::

         class (Eq a) => C (f :: * -> *) a where ...

-  ``forall``\'s in type signatures: ::

         f :: forall (cxt :: * -> *). Set cxt Int

The parentheses are required. Some of the spaces are required too, to
separate the lexemes. If you write ``(f::*->*)`` you will get a parse
error, because ``::*->*`` is a single lexeme in Haskell.

As part of the same extension, you can put kind annotations in types as
well. Thus: ::

       f :: (Int :: *) -> Int
       g :: forall a. a -> (a :: *)

The syntax is

.. code-block:: none

       atype ::= '(' ctype '::' kind ')

The parentheses are required.

.. _universal-quantification:

.. _scoped-type-variables:

Lexically scoped type variables
===============================

.. ghc-flag:: -XScopedTypeVariables

    :implies: :ghc-flag:`-XExplicitForAll`

    Enable lexical scoping of type variables explicitly introduced with
    ``forall``.

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

-  The signature gives a type for a function binding or a bare variable
   binding, not a pattern binding. For example: ::

         f1 :: forall a. [a] -> [a]
         f1 (x:xs) = xs ++ [ x :: a ]   -- OK

         f2 :: forall a. [a] -> [a]
         f2 = \(x:xs) -> xs ++ [ x :: a ]   -- OK

         f3 :: forall a. [a] -> [a]
         Just f3 = Just (\(x:xs) -> xs ++ [ x :: a ])   -- Not OK!

   The binding for ``f3`` is a pattern binding, and so its type
   signature does not bring ``a`` into scope. However ``f1`` is a
   function binding, and ``f2`` binds a bare variable; in both cases the
   type signature brings ``a`` into scope.

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
        zs::[a] = xs ++ ys                     -- OK

        Just (v::b) = ...  -- Not OK; b is not in scope

Here, the pattern signatures for ``ys`` and ``zs`` are fine, but the one
for ``v`` is not because ``b`` is not in scope.

However, in all patterns *other* than pattern bindings, a pattern type
signature may mention a type variable that is not in scope; in this
case, *the signature brings that type variable into scope*. This is
particularly important for existential data constructors. For example: ::

    data T = forall a. MkT [a]

    k :: T -> T
    k (MkT [t::a]) =
        MkT t3
      where
        t3::[a] = [t,t,t]

Here, the pattern type signature ``(t::a)`` mentions a lexical type
variable that is not already in scope. Indeed, it *cannot* already be in
scope, because it is bound by the pattern match. GHC's rule is that in
this situation (and only then), a pattern type signature can mention a
type variable that is not already in scope; the effect is to bring it
into scope, standing for the existentially-bound type variable.

When a pattern type signature binds a type variable in this way, GHC
insists that the type variable is bound to a *rigid*, or fully-known,
type variable. This means that any user-written type signature always
stands for a completely known type.

If all this seems a little odd, we think so too. But we must have *some*
way to bring such type variables into scope, else we could not name
existentially-bound type variables in subsequent type signatures.

This is (now) the *only* situation in which a pattern type signature is
allowed to mention a lexical variable that is not already in scope. For
example, both ``f`` and ``g`` would be illegal if ``a`` was not already
in scope.

.. _cls-inst-scoped-tyvars:

Class and instance declarations
-------------------------------

The type variables in the head of a ``class`` or ``instance``
declaration scope over the methods defined in the ``where`` part. You do
not even need an explicit ``forall`` (although you are allowed an explicit
``forall`` in an ``instance`` declaration; see :ref:`explicit-foralls`).
For example: ::

      class C a where
        op :: [a] -> a

        op xs = let ys::[a]
                    ys = reverse xs
                in
                head ys

      instance C b => C [b] where
        op xs = reverse (head (xs :: [[b]]))

Bindings and generalisation
===========================

.. _monomorphism:

Switching off the dreaded Monomorphism Restriction
--------------------------------------------------

.. ghc-flag:: -XNoMonomorphismRestriction

    :default: on

    Prevents the compiler from applying the monomorphism restriction to
    bindings lacking explicit type signatures.

Haskell's monomorphism restriction (see `Section
4.5.5 <http://www.haskell.org/onlinereport/decls.html#sect4.5.5>`__ of
the Haskell Report) can be completely switched off by
:ghc-flag:`-XNoMonomorphismRestriction`. Since GHC 7.8.1, the monomorphism
restriction is switched off by default in GHCi's interactive options
(see :ref:`ghci-interactive-options`).

.. _mono-local-binds:

Let-generalisation
------------------

.. ghc-flag:: -XMonoLocalBinds

    :since: 6.12

    Infer less polymorphic types for local bindings by default.

An ML-style language usually generalises the type of any ``let``\-bound or
``where``\-bound variable, so that it is as polymorphic as possible. With the
flag :ghc-flag:`-XMonoLocalBinds` GHC implements a slightly more conservative
policy, using the following rules:

-  A variable is *closed* if and only if

   -  the variable is let-bound

   -  one of the following holds:

      -  the variable has an explicit type signature that has no free
         type variables, or

      -  its binding group is fully generalised (see next bullet)

-  A binding group is *fully generalised* if and only if

   -  each of its free variables is either imported or closed, and

   -  the binding is not affected by the monomorphism restriction
      (`Haskell Report, Section
      4.5.5 <http://www.haskell.org/onlinereport/decls.html#sect4.5.5>`__)

For example, consider ::

    f x = x + 1
    g x = let h y = f y * 2
              k z = z+x
          in  h x + k x

Here ``f`` is generalised because it has no free variables; and its
binding group is unaffected by the monomorphism restriction; and hence
``f`` is closed. The same reasoning applies to ``g``, except that it has
one closed free variable, namely ``f``. Similarly ``h`` is closed, *even
though it is not bound at top level*, because its only free variable
``f`` is closed. But ``k`` is not closed, because it mentions ``x``
which is not closed (because it is not let-bound).

Notice that a top-level binding that is affected by the monomorphism
restriction is not closed, and hence may in turn prevent generalisation
of bindings that mention it.

The rationale for this more conservative strategy is given in `the
papers <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf>`__
"Let should not be generalised" and "Modular type inference with local
assumptions", and a related `blog post <http://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7>`__.

The flag :ghc-flag:`-XMonoLocalBinds` is implied by :ghc-flag:`-XTypeFamilies`
and :ghc-flag:`-XGADTs`. You can switch it off again with
:ghc-flag:`-XNoMonoLocalBinds <-XMonoLocalBinds>` but type inference becomes
less predicatable if you do so. (Read the papers!)

.. _kind-generalisation:

Kind generalisation
-------------------

Just as :ghc-flag:`-XMonoLocalBinds` places limitations on when the *type* of a
*term* is generalised (see :ref:`mono-local-binds`), it also limits when the
*kind* of a *type signature* is generalised. Here is an example involving
:ref:`type signatures on instance declarations <instance-sigs>`: ::

    data Proxy a = Proxy
    newtype Tagged s b = Tagged b

    class C b where
      c :: forall (s :: k). Tagged s b

    instance C (Proxy a) where
      c :: forall s. Tagged s (Proxy a)
      c = Tagged Proxy

With :ghc-flag:`-XMonoLocalBinds` enabled, this ``C (Proxy a)`` instance will
fail to typecheck. The reason is that the type signature for ``c`` captures
``a``, an outer-scoped type variable, which means the type signature is not
closed. Therefore, the inferred kind for ``s`` will *not* be generalised, and
as a result, it will fail to unify with the kind variable ``k`` which is
specified in the declaration of ``c``. This can be worked around by specifying
an explicit kind variable for ``s``, e.g., ::

    instance C (Proxy a) where
      c :: forall (s :: k). Tagged s (Proxy a)
      c = Tagged Proxy

or, alternatively: ::

    instance C (Proxy a) where
      c :: forall k (s :: k). Tagged s (Proxy a)
      c = Tagged Proxy

This declarations are equivalent using Haskell's implicit "add implicit
foralls" rules (see :ref:`implicit-quantification`). The implicit foralls rules
are purely syntactic and are quite separate from the kind generalisation
described here.

.. _visible-type-application:

Visible type application
========================

.. ghc-flag:: -XTypeApplications

    :since: 8.0.1

    Allow the use of type application syntax.

The :ghc-flag:`-XTypeApplications` extension allows you to use
*visible type application* in expressions. Here is an
example: ``show (read @Int "5")``. The ``@Int``
is the visible type application; it specifies the value of the type variable
in ``read``'s type.

A visible type application is preceded with an ``@``
sign. (To disambiguate the syntax, the ``@`` must be
preceded with a non-identifier letter, usually a space. For example,
``read@Int 5`` would not parse.) It can be used whenever
the full polymorphic type of the function is known. If the function
is an identifier (the common case), its type is considered known only when
the identifier has been given a type signature. If the identifier does
not have a type signature, visible type application cannot be used.

Here are the details:

- If an identifier's type signature does not include an
  explicit ``forall``, the type variable arguments appear
  in the left-to-right order in which the variables appear in the type.
  So, ``foo :: Monad m => a b -> m (a c)``
  will have its type variables
  ordered as ``m, a, b, c``.

- If any of the variables depend on other variables (that is, if some
  of the variables are *kind* variables), the variables are reordered
  so that kind variables come before type variables, preserving the
  left-to-right order as much as possible. That is, GHC performs a
  stable topological sort on the variables.

  For example: if we have ``bar :: Proxy (a :: (j, k)) -> b``, then
  the variables are ordered ``j``, ``k``, ``a``, ``b``.

- Visible type application is available to instantiate only user-specified
  type variables. This means that in ``data Proxy a = Proxy``, the unmentioned
  kind variable used in ``a``'s kind is *not* available for visible type
  application.

- Class methods' type arguments include the class type
  variables, followed by any variables an individual method is polymorphic
  in. So, ``class Monad m where return :: a -> m a`` means
  that ``return``'s type arguments are ``m, a``.

- With the :ghc-flag:`-XRankNTypes` extension
  (:ref:`universal-quantification`), it is possible to declare
  type arguments somewhere other than the beginning of a type. For example,
  we can have ``pair :: forall a. a -> forall b. b -> (a, b)``
  and then say ``pair @Bool True @Char`` which would have
  type ``Char -> (Bool, Char)``.

- Partial type signatures (:ref:`partial-type-signatures`)
  work nicely with visible type
  application. If you want to specify only the second type argument to
  ``wurble``, then you can say ``wurble @_ @Int``.
  The first argument is a wildcard, just like in a partial type signature.
  However, if used in a visible type application, it is *not*
  necessary to specify :ghc-flag:`-XPartialTypeSignatures` and your
  code will not generate a warning informing you of the omitted type.

- When printing types with :ghc-flag:`-fprint-explicit-foralls` enabled,
  type variables not available for visible type application are printed
  in braces. We can observe this behavior in a GHCi session: ::

    > :set -XTypeApplications -fprint-explicit-foralls
    > let myLength1 :: Foldable f => f a -> Int; myLength1 = length
    > :type +v myLength1
    myLength1 :: forall (f :: * -> *) a. Foldable f => f a -> Int
    > let myLength2 = length
    > :type +v myLength2
    myLength2 :: forall {a} {t :: * -> *}. Foldable t => t a -> Int
    > :type +v myLength2 @[]

    <interactive>:1:1: error:
        • Cannot apply expression of type ‘t0 a0 -> Int’
          to a visible type argument ‘[]’
        • In the expression: myLength2 @[]

  Notice that since ``myLength1`` was defined with an explicit type signature,
  :ghci-cmd:`:type +v` reports that all of its type variables are available
  for type application. On the other hand, ``myLength2`` was not given a type
  signature. As a result, all of its type variables are surrounded with braces,
  and trying to use visible type application with ``myLength2`` fails.

  Also note the use of :ghci-cmd:`:type +v` in the GHCi session above instead
  of :ghci-cmd:`:type`. This is because :ghci-cmd:`:type` gives you the type
  that would be inferred for a variable assigned to the expression provided
  (that is, the type of ``x`` in ``let x = <expr>``). As we saw above with
  ``myLength2``, this type will have no variables available to visible type
  application. On the other hand, :ghci-cmd:`:type +v` gives you the actual
  type of the expression provided. To illustrate this: ::

    > :type myLength1
    myLength1 :: forall {a} {f :: * -> *}. Foldable f => f a -> Int
    > :type myLength2
    myLength2 :: forall {a} {t :: * -> *}. Foldable t => t a -> Int

  Using :ghci-cmd:`:type` might lead one to conclude that none of the type
  variables in ``myLength1``'s type signature are available for type
  application. This isn't true, however! Be sure to use :ghci-cmd:`:type +v`
  if you want the most accurate information with respect to visible type
  application properties.

- Data constructors declared with GADT syntax follow different rules
  for the time being; it is expected that these will be brought in line
  with other declarations in the future. The rules for GADT
  data constructors are as follows:

     * All kind and type variables are considered specified and available for
       visible type application.

     * Universal variables always come first, in precisely the order they
       appear in the type declaration. Universal variables that are
       constrained by a GADT return type are not included in the data constructor.

     * Existential variables come next. Their order is determined by a user-
       written `forall`; or, if there is none, by taking the left-to-right order
       in the data constructor's type and doing a stable topological sort.

.. _implicit-parameters:

Implicit parameters
===================

.. ghc-flag:: -XImplicitParams

    Allow definition of functions expecting implicit parameters.

Implicit parameters are implemented as described in [Lewis2000]_ and enabled
with the option :ghc-flag:`-XImplicitParams`. (Most of the following, still rather
incomplete, documentation is due to Jeff Lewis.)

.. [Lewis2000]
    "Implicit parameters: dynamic scoping with static types",
    J Lewis, MB Shields, E Meijer, J Launchbury,
    *27th ACM Symposium on Principles of Programming Languages (POPL'00)*,
    Boston, Jan 2000.

A variable is called *dynamically bound* when it is bound by the calling
context of a function and *statically bound* when bound by the callee's
context. In Haskell, all variables are statically bound. Dynamic binding
of variables is a notion that goes back to Lisp, but was later discarded
in more modern incarnations, such as Scheme. Dynamic binding can be very
confusing in an untyped language, and unfortunately, typed languages, in
particular Hindley-Milner typed languages like Haskell, only support
static scoping of variables.

However, by a simple extension to the type class system of Haskell, we
can support dynamic binding. Basically, we express the use of a
dynamically bound variable as a constraint on the type. These
constraints lead to types of the form ``(?x::t') => t``, which says
"this function uses a dynamically-bound variable ``?x`` of type ``t'``".
For example, the following expresses the type of a sort function,
implicitly parameterised by a comparison function named ``cmp``. ::

      sort :: (?cmp :: a -> a -> Bool) => [a] -> [a]

The dynamic binding constraints are just a new form of predicate in the
type class system.

An implicit parameter occurs in an expression using the special form
``?x``, where ``x`` is any valid identifier (e.g. ``ord ?x`` is a valid
expression). Use of this construct also introduces a new dynamic-binding
constraint in the type of the expression. For example, the following
definition shows how we can define an implicitly parameterised sort
function in terms of an explicitly parameterised ``sortBy`` function: ::

      sortBy :: (a -> a -> Bool) -> [a] -> [a]

      sort   :: (?cmp :: a -> a -> Bool) => [a] -> [a]
      sort    = sortBy ?cmp

Implicit-parameter type constraints
-----------------------------------

Dynamic binding constraints behave just like other type class
constraints in that they are automatically propagated. Thus, when a
function is used, its implicit parameters are inherited by the function
that called it. For example, our ``sort`` function might be used to pick
out the least value in a list: ::

      least   :: (?cmp :: a -> a -> Bool) => [a] -> a
      least xs = head (sort xs)

Without lifting a finger, the ``?cmp`` parameter is propagated to become
a parameter of ``least`` as well. With explicit parameters, the default
is that parameters must always be explicit propagated. With implicit
parameters, the default is to always propagate them.

An implicit-parameter type constraint differs from other type class
constraints in the following way: All uses of a particular implicit
parameter must have the same type. This means that the type of
``(?x, ?x)`` is ``(?x::a) => (a,a)``, and not
``(?x::a, ?x::b) => (a, b)``, as would be the case for type class
constraints.

You can't have an implicit parameter in the context of a class or
instance declaration. For example, both these declarations are illegal: ::

      class (?x::Int) => C a where ...
      instance (?x::a) => Foo [a] where ...

Reason: exactly which implicit parameter you pick up depends on exactly
where you invoke a function. But the "invocation" of instance
declarations is done behind the scenes by the compiler, so it's hard to
figure out exactly where it is done. Easiest thing is to outlaw the
offending types.

Implicit-parameter constraints do not cause ambiguity. For example,
consider: ::

       f :: (?x :: [a]) => Int -> Int
       f n = n + length ?x

       g :: (Read a, Show a) => String -> String
       g s = show (read s)

Here, ``g`` has an ambiguous type, and is rejected, but ``f`` is fine.
The binding for ``?x`` at ``f``\ 's call site is quite unambiguous, and
fixes the type ``a``.

Implicit-parameter bindings
---------------------------

An implicit parameter is *bound* using the standard ``let`` or ``where``
binding forms. For example, we define the ``min`` function by binding
``cmp``. ::

      min :: Ord a => [a] -> a
      min  = let ?cmp = (<=) in least

A group of implicit-parameter bindings may occur anywhere a normal group
of Haskell bindings can occur, except at top level. That is, they can
occur in a ``let`` (including in a list comprehension, or do-notation,
or pattern guards), or a ``where`` clause. Note the following points:

-  An implicit-parameter binding group must be a collection of simple
   bindings to implicit-style variables (no function-style bindings, and
   no type signatures); these bindings are neither polymorphic or
   recursive.

-  You may not mix implicit-parameter bindings with ordinary bindings in
   a single ``let`` expression; use two nested ``let``\ s instead. (In
   the case of ``where`` you are stuck, since you can't nest ``where``
   clauses.)

-  You may put multiple implicit-parameter bindings in a single binding
   group; but they are *not* treated as a mutually recursive group (as
   ordinary ``let`` bindings are). Instead they are treated as a
   non-recursive group, simultaneously binding all the implicit
   parameter. The bindings are not nested, and may be re-ordered without
   changing the meaning of the program. For example, consider: ::

         f t = let { ?x = t; ?y = ?x+(1::Int) } in ?x + ?y

   The use of ``?x`` in the binding for ``?y`` does not "see" the
   binding for ``?x``, so the type of ``f`` is ::

         f :: (?x::Int) => Int -> Int

Implicit parameters and polymorphic recursion
---------------------------------------------

Consider these two definitions: ::

      len1 :: [a] -> Int
      len1 xs = let ?acc = 0 in len_acc1 xs

      len_acc1 [] = ?acc
      len_acc1 (x:xs) = let ?acc = ?acc + (1::Int) in len_acc1 xs

      ------------

      len2 :: [a] -> Int
      len2 xs = let ?acc = 0 in len_acc2 xs

      len_acc2 :: (?acc :: Int) => [a] -> Int
      len_acc2 [] = ?acc
      len_acc2 (x:xs) = let ?acc = ?acc + (1::Int) in len_acc2 xs

The only difference between the two groups is that in the second group
``len_acc`` is given a type signature. In the former case, ``len_acc1``
is monomorphic in its own right-hand side, so the implicit parameter
``?acc`` is not passed to the recursive call. In the latter case,
because ``len_acc2`` has a type signature, the recursive call is made to
the *polymorphic* version, which takes ``?acc`` as an implicit
parameter. So we get the following results in GHCi:

.. code-block:: none

      Prog> len1 "hello"
      0
      Prog> len2 "hello"
      5

Adding a type signature dramatically changes the result! This is a
rather counter-intuitive phenomenon, worth watching out for.

Implicit parameters and monomorphism
------------------------------------

GHC applies the dreaded Monomorphism Restriction (section 4.5.5 of the
Haskell Report) to implicit parameters. For example, consider: ::

     f :: Int -> Int
     f v = let ?x = 0     in
           let y = ?x + v in
           let ?x = 5     in
           y

Since the binding for ``y`` falls under the Monomorphism Restriction it
is not generalised, so the type of ``y`` is simply ``Int``, not
``(?x::Int) => Int``. Hence, ``(f 9)`` returns result ``9``. If you add
a type signature for ``y``, then ``y`` will get type
``(?x::Int) => Int``, so the occurrence of ``y`` in the body of the
``let`` will see the inner binding of ``?x``, so ``(f 9)`` will return
``14``.

Arbitrary-rank polymorphism
===========================

.. ghc-flag:: -XRankNTypes

    :implies: :ghc-flag:`-XExplicitForAll`

    Allow types of arbitrary rank.

.. ghc-flag:: -XRank2Types

    A deprecated alias of :ghc-flag:`-XRankNTypes`.

GHC's type system supports *arbitrary-rank* explicit universal
quantification in types. For example, all the following types are legal: ::

        f1 :: forall a b. a -> b -> a
        g1 :: forall a b. (Ord a, Eq  b) => a -> b -> a

        f2 :: (forall a. a->a) -> Int -> Int
        g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int

        f3 :: ((forall a. a->a) -> Int) -> Bool -> Bool

        f4 :: Int -> (forall a. a -> a)

Here, ``f1`` and ``g1`` are rank-1 types, and can be written in standard
Haskell (e.g. ``f1 :: a->b->a``). The ``forall`` makes explicit the
universal quantification that is implicitly added by Haskell.

The functions ``f2`` and ``g2`` have rank-2 types; the ``forall`` is on
the left of a function arrow. As ``g2`` shows, the polymorphic type on
the left of the function arrow can be overloaded.

The function ``f3`` has a rank-3 type; it has rank-2 types on the left
of a function arrow.

The language option :ghc-flag:`-XRankNTypes` (which implies
:ghc-flag:`-XExplicitForAll`) enables higher-rank
types. That is, you can nest ``forall``\ s arbitrarily deep in function
arrows. For example, a forall-type (also called a "type scheme"),
including a type-class context, is legal:

-  On the left or right (see ``f4``, for example) of a function arrow

-  As the argument of a constructor, or type of a field, in a data type
   declaration. For example, any of the ``f1, f2, f3, g1, g2`` above would
   be valid field type signatures.

-  As the type of an implicit parameter

-  In a pattern type signature (see :ref:`scoped-type-variables`)

The :ghc-flag:`-XRankNTypes` option is also required for any type with a
``forall`` or context to the right of an arrow (e.g.
``f :: Int -> forall a. a->a``, or ``g :: Int -> Ord a => a -> a``).
Such types are technically rank 1, but are clearly not Haskell-98, and
an extra flag did not seem worth the bother.

In particular, in ``data`` and ``newtype`` declarations the constructor
arguments may be polymorphic types of any rank; see examples in
:ref:`univ`. Note that the declared types are nevertheless always
monomorphic. This is important because by default GHC will not
instantiate type variables to a polymorphic type
(:ref:`impredicative-polymorphism`).

The obsolete language options :ghc-flag:`-XPolymorphicComponents` and
:ghc-flag:`-XRank2Types` are synonyms for :ghc-flag:`-XRankNTypes`. They used to
specify finer distinctions that GHC no longer makes. (They should really elicit
a deprecation warning, but they don't, purely to avoid the need to library
authors to change their old flags specifications.)

.. _univ:

Examples
--------

These are examples of ``data`` and ``newtype`` declarations whose data
constructors have polymorphic argument types: ::

    data T a = T1 (forall b. b -> b -> b) a

    data MonadT m = MkMonad { return :: forall a. a -> m a,
                              bind   :: forall a b. m a -> (a -> m b) -> m b
                            }

    newtype Swizzle = MkSwizzle (forall a. Ord a => [a] -> [a])

The constructors have rank-2 types: ::

    T1 :: forall a. (forall b. b -> b -> b) -> a -> T a

    MkMonad :: forall m. (forall a. a -> m a)
                      -> (forall a b. m a -> (a -> m b) -> m b)
                      -> MonadT m

    MkSwizzle :: (forall a. Ord a => [a] -> [a]) -> Swizzle

In earlier versions of GHC, it was possible to omit the ``forall`` in
the type of the constructor if there was an explicit context. For
example: ::

    newtype Swizzle' = MkSwizzle' (Ord a => [a] -> [a])

Since GHC 8.0 declarations such as ``MkSwizzle'`` will cause an out-of-scope
error.

As for type signatures, implicit quantification happens for
non-overloaded types too. So if you write this: ::

      f :: (a -> a) -> a

it's just as if you had written this: ::

      f :: forall a. (a -> a) -> a

That is, since the type variable ``a`` isn't in scope, it's implicitly
universally quantified.

You construct values of types ``T1, MonadT, Swizzle`` by applying the
constructor to suitable values, just as usual. For example, ::

        a1 :: T Int
        a1 = T1 (\xy->x) 3

        a2, a3 :: Swizzle
        a2 = MkSwizzle sort
        a3 = MkSwizzle reverse

        a4 :: MonadT Maybe
        a4 = let r x = Just x
             b m k = case m of
                   Just y -> k y
                   Nothing -> Nothing
             in
             MkMonad r b

        mkTs :: (forall b. b -> b -> b) -> a -> [T a]
        mkTs f x y = [T1 f x, T1 f y]

The type of the argument can, as usual, be more general than the type
required, as ``(MkSwizzle reverse)`` shows. (``reverse`` does not need
the ``Ord`` constraint.)

When you use pattern matching, the bound variables may now have
polymorphic types. For example: ::

        f :: T a -> a -> (a, Char)
        f (T1 w k) x = (w k x, w 'c' 'd')

        g :: (Ord a, Ord b) => Swizzle -> [a] -> (a -> b) -> [b]
        g (MkSwizzle s) xs f = s (map f (s xs))

        h :: MonadT m -> [m a] -> m [a]
        h m [] = return m []
        h m (x:xs) = bind m x          $ \y ->
                     bind m (h m xs)   $ \ys ->
                     return m (y:ys)

In the function ``h`` we use the record selectors ``return`` and
``bind`` to extract the polymorphic bind and return functions from the
``MonadT`` data structure, rather than using pattern matching.


.. _higher-rank-type-inference:

Type inference
--------------

In general, type inference for arbitrary-rank types is undecidable. GHC
uses an algorithm proposed by Odersky and Laufer ("Putting type
annotations to work", POPL'96) to get a decidable algorithm by requiring
some help from the programmer. We do not yet have a formal specification
of "some help" but the rule is this:

    For a lambda-bound or case-bound variable, x, either the programmer
    provides an explicit polymorphic type for x, or GHC's type inference
    will assume that x's type has no foralls in it.

What does it mean to "provide" an explicit type for x? You can do that
by giving a type signature for x directly, using a pattern type
signature (:ref:`scoped-type-variables`), thus: ::

    \ f :: (forall a. a->a) -> (f True, f 'c')

Alternatively, you can give a type signature to the enclosing context,
which GHC can "push down" to find the type for the variable: ::

    (\ f -> (f True, f 'c')) :: (forall a. a->a) -> (Bool,Char)

Here the type signature on the expression can be pushed inwards to give
a type signature for f. Similarly, and more commonly, one can give a
type signature for the function itself: ::

    h :: (forall a. a->a) -> (Bool,Char)
    h f = (f True, f 'c')

You don't need to give a type signature if the lambda bound variable is
a constructor argument. Here is an example we saw earlier: ::

    f :: T a -> a -> (a, Char)
    f (T1 w k) x = (w k x, w 'c' 'd')

Here we do not need to give a type signature to ``w``, because it is an
argument of constructor ``T1`` and that tells GHC all it needs to know.


.. _implicit-quantification:

Implicit quantification
-----------------------

GHC performs implicit quantification as follows. At the outermost level
(only) of user-written types, if and only if there is no explicit
``forall``, GHC finds all the type variables mentioned in the type that
are not already in scope, and universally quantifies them. For example,
the following pairs are equivalent: ::

      f :: a -> a
      f :: forall a. a -> a

      g (x::a) = let
                    h :: a -> b -> b
                    h x y = y
                 in ...
      g (x::a) = let
                    h :: forall b. a -> b -> b
                    h x y = y
                 in ...

Notice that GHC always adds implicit quantfiers *at the outermost level*
of a user-written type; it
does *not* find the inner-most possible quantification
point. For example: ::

      f :: (a -> a) -> Int
               -- MEANS
      f :: forall a. (a -> a) -> Int
               -- NOT
      f :: (forall a. a -> a) -> Int


      g :: (Ord a => a -> a) -> Int
               -- MEANS
      g :: forall a. (Ord a => a -> a) -> Int
               -- NOT
      g :: (forall a. Ord a => a -> a) -> Int

If you want the latter type, you can write
your ``forall``\s explicitly. Indeed, doing so is strongly advised for
rank-2 types.

Sometimes there *is* no "outermost level", in which case no
implicit quantification happens: ::

      data PackMap a b s t = PackMap (Monad f => (a -> f b) -> s -> f t)

This is rejected because there is no "outermost level" for the types on the RHS
(it would obviously be terrible to add extra parameters to ``PackMap``),
so no implicit quantification happens, and the declaration is rejected
(with "``f`` is out of scope").  Solution: use an explicit ``forall``: ::

      data PackMap a b s t = PackMap (forall f. Monad f => (a -> f b) -> s -> f t)

.. _impredicative-polymorphism:

Impredicative polymorphism
==========================

.. ghc-flag:: -XImpredicativeTypes

    :implies: :ghc-flag:`-XRankNTypes`

    Allow impredicative polymorphic types.

In general, GHC will only instantiate a polymorphic function at a
monomorphic type (one with no foralls). For example, ::

    runST :: (forall s. ST s a) -> a
    id :: forall b. b -> b

    foo = id runST   -- Rejected

The definition of ``foo`` is rejected because one would have to
instantiate ``id``\'s type with ``b := (forall s. ST s a) -> a``, and
that is not allowed. Instantiating polymorphic type variables with
polymorphic types is called *impredicative polymorphism*.

GHC has extremely flaky support for *impredicative polymorphism*,
enabled with :ghc-flag:`-XImpredicativeTypes`. If it worked, this would mean
that you *could* call a polymorphic function at a polymorphic type, and
parameterise data structures over polymorphic types. For example: ::

      f :: Maybe (forall a. [a] -> [a]) -> Maybe ([Int], [Char])
      f (Just g) = Just (g [3], g "hello")
      f Nothing  = Nothing

Notice here that the ``Maybe`` type is parameterised by the
*polymorphic* type ``(forall a. [a] -> [a])``. However *the extension
should be considered highly experimental, and certainly un-supported*.
You are welcome to try it, but please don't rely on it working
consistently, or working the same in subsequent releases. See
:ghc-wiki:`this wiki page <ImpredicativePolymorphism>` for more details.

If you want impredicative polymorphism, the main workaround is to use a
newtype wrapper. The ``id runST`` example can be written using theis
workaround like this: ::

    runST :: (forall s. ST s a) -> a
    id :: forall b. b -> b

    nwetype Wrap a = Wrap { unWrap :: (forall s. ST s a) -> a }

    foo :: (forall s. ST s a) -> a
    foo = unWrap (id (Wrap runST))
          -- Here id is called at monomorphic type (Wrap a)

.. _typed-holes:

Typed Holes
===========

Typed holes are a feature of GHC that allows special placeholders
written with a leading underscore (e.g., "``_``", "``_foo``",
"``_bar``"), to be used as expressions. During compilation these holes
will generate an error message that describes which type is expected at
the hole's location, information about the origin of any free type
variables, and a list of local bindings that might help fill the hole
with actual code. Typed holes are always enabled in GHC.

The goal of typed holes is to help with writing Haskell code rather than
to change the type system. Typed holes can be used to obtain extra
information from the type checker, which might otherwise be hard to get.
Normally, using GHCi, users can inspect the (inferred) type signatures
of all top-level bindings. However, this method is less convenient with
terms that are not defined on top-level or inside complex expressions.
Holes allow the user to check the type of the term they are about to
write.

For example, compiling the following module with GHC: ::

    f :: a -> a
    f x = _

will fail with the following error: ::

    hole.hs:2:7:
        Found hole `_' with type: a
        Where: `a' is a rigid type variable bound by
                   the type signature for f :: a -> a at hole.hs:1:6
        Relevant bindings include
          f :: a -> a (bound at hole.hs:2:1)
          x :: a (bound at hole.hs:2:3)
        In the expression: _
        In an equation for `f': f x = _

Here are some more details:

-  A "``Found hole``" error usually terminates compilation, like any
   other type error. After all, you have omitted some code from your
   program. Nevertheless, you can run and test a piece of code
   containing holes, by using the :ghc-flag:`-fdefer-typed-holes` flag. This
   flag defers errors produced by typed holes until runtime, and
   converts them into compile-time warnings. These warnings can in turn
   be suppressed entirely by :ghc-flag:`-Wno-typed-holes <-Wtyped-holes>`.

   The same behaviour for "``Variable out of scope``" errors, it terminates
   compilation by default. You can defer such errors by using the
   :ghc-flag:`-fdefer-out-of-scope-variables` flag. This flag defers errors
   produced by out of scope variables until runtime, and
   converts them into compile-time warnings. These warnings can in turn
   be suppressed entirely by :ghc-flag:`-Wno-deferred-out-of-scope-variables
   <-Wdeferred-out-of-scope-variables>`.

   The result is that a hole or a variable will behave like ``undefined``, but with
   the added benefits that it shows a warning at compile time, and will
   show the same message if it gets evaluated at runtime. This behaviour
   follows that of the :ghc-flag:`-fdefer-type-errors` option, which implies
   :ghc-flag:`-fdefer-typed-holes` and :ghc-flag:`-fdefer-out-of-scope-variables`.
   See :ref:`defer-type-errors`.

-  All unbound identifiers are treated as typed holes, *whether or not
   they start with an underscore*. The only difference is in the error
   message: ::

       cons z = z : True : _x : y

   yields the errors

   .. code-block:: none

       Foo.hs:5:15: error:
           Found hole: _x :: Bool
           Relevant bindings include
             p :: Bool (bound at Foo.hs:3:6)
             cons :: Bool -> [Bool] (bound at Foo.hs:3:1)

       Foo.hs:5:20: error:
           Variable not in scope: y :: [Bool]

   More information is given for explicit holes (i.e. ones that start
   with an underscore), than for out-of-scope variables, because the
   latter are often unintended typos, so the extra information is
   distracting. If you want the detailed information, use a leading
   underscore to make explicit your intent to use a hole.

-  Unbound identifiers with the same name are never unified, even within
   the same function, but shown individually. For example: ::

       cons = _x : _x

   results in the following errors:

   .. code-block:: none

       unbound.hs:1:8:
           Found hole '_x' with type: a
           Where: `a' is a rigid type variable bound by
                      the inferred type of cons :: [a] at unbound.hs:1:1
           Relevant bindings include cons :: [a] (bound at unbound.hs:1:1)
           In the first argument of `(:)', namely `_x'
           In the expression: _x : _x
           In an equation for `cons': cons = _x : _x

       unbound.hs:1:13:
           Found hole '_x' with type: [a]
           Arising from: an undeclared identifier `_x' at unbound.hs:1:13-14
           Where: `a' is a rigid type variable bound by
                      the inferred type of cons :: [a] at unbound.hs:1:1
           Relevant bindings include cons :: [a] (bound at unbound.hs:1:1)
           In the second argument of `(:)', namely `_x'
           In the expression: _x : _x
           In an equation for `cons': cons = _x : _x

   Notice the two different types reported for the two different
   occurrences of ``_x``.

-  No language extension is required to use typed holes. The lexeme
   "``_``" was previously illegal in Haskell, but now has a more
   informative error message. The lexeme "``_x``" is a perfectly legal
   variable, and its behaviour is unchanged when it is in scope. For
   example ::

       f _x = _x + 1

   does not elict any errors. Only a variable *that is not in scope*
   (whether or not it starts with an underscore) is treated as an error
   (which it always was), albeit now with a more informative error
   message.

-  Unbound data constructors used in expressions behave exactly as
   above. However, unbound data constructors used in *patterns* cannot
   be deferred, and instead bring compilation to a halt. (In
   implementation terms, they are reported by the renamer rather than
   the type checker.)

There's a flag for controlling the amount of context information shown for
typed holes:

.. ghc-flag:: -fshow-hole-constraints

    When reporting typed holes, also print constraints that are in scope.
    Example: ::

        f :: Eq a => a -> Bool
        f x = _

    results in the following message:

    .. code-block:: none

        show_constraints.hs:4:7: error:
            • Found hole: _ :: Bool
            • In the expression: _
              In an equation for ‘f’: f x = _
            • Relevant bindings include
                x :: a (bound at show_constraints.hs:4:3)
                f :: a -> Bool (bound at show_constraints.hs:4:1)
              Constraints include
                Eq a (from the type signature for:
                             f :: Eq a => a -> Bool
                      at show_constraints.hs:3:1-22)


.. _partial-type-signatures:

Partial Type Signatures
=======================

.. ghc-flag:: -XPartialTypeSignatures

    :since: 7.10.1

    Type checker will allow inferred types for holes.

A partial type signature is a type signature containing special
placeholders written with a leading underscore (e.g., "``_``",
"``_foo``", "``_bar``") called *wildcards*. Partial type signatures are
to type signatures what :ref:`typed-holes` are to expressions. During
compilation these wildcards or holes will generate an error message that
describes which type was inferred at the hole's location, and
information about the origin of any free type variables. GHC reports
such error messages by default.

Unlike :ref:`typed-holes`, which make the program incomplete and will
generate errors when they are evaluated, this needn't be the case for
holes in type signatures. The type checker is capable (in most cases) of
type-checking a binding with or without a type signature. A partial type
signature bridges the gap between the two extremes, the programmer can
choose which parts of a type to annotate and which to leave over to the
type-checker to infer.

By default, the type-checker will report an error message for each hole
in a partial type signature, informing the programmer of the inferred
type. When the :ghc-flag:`-XPartialTypeSignatures` flag is enabled, the
type-checker will accept the inferred type for each hole, generating
warnings instead of errors. Additionally, these warnings can be silenced
with the :ghc-flag:`-Wno-partial-type-signatures <-Wpartial-type-signatures>`
flag.

However, because GHC must *infer* the type when part of a type is left
out, it is unable to use polymorphic recursion. The same restriction
takes place when the type signature is omitted completely.

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

.. ghc-flag:: -XNamedWildCards

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
treat them as named wildcards, the :ghc-flag:`-XNamedWildCards` flag should be
enabled. The example below demonstrated the effect. ::

    foo :: _a -> _a
    foo _ = False

Compiling this program without enabling :ghc-flag:`-XNamedWildCards` produces
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

Compiling this program with :ghc-flag:`-XNamedWildCards` (as well as
:ghc-flag:`-XPartialTypeSignatures`) enabled produces the following error
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
already listing the constraints he knows or wants to annotate, e.g.

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

Anonymous wildcards are also allowed in visible type applications
(:ref:`visible-type-application`). If you want to specify only the second type
argument to ``wurble``, then you can say ``wurble @_ @Int`` where the first
argument is a wildcard.

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
   signatures. Note that :ghc-flag:`-XScopedTypeVariables` has to be enabled
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

.. _custom-errors:

Custom compile-time errors
==========================

When designing embedded domain specific languages in Haskell, it is useful to
have something like ``error`` at the type level. In this way, the EDSL designer
may show a type error that is specific to the DSL, rather than the standard GHC
type error.

For example, consider a type class that is not intended to be used with
functions, but the user accidentally used it at a function type, perhaps
because they missed an argument to some function. Then, instead of getting the
standard GHC message about a missing instance, it would be nicer to emit a more
friendly message specific to the EDSL. Similarly, the reduction of a type-level
function may get stuck due to an error, at which point it would be nice to
report an EDSL specific error, rather than a generic error about an ambiguous
type.

To solve this, GHC provides a single type-level function, ::

    type family TypeError (msg :: ErrorMessage) :: k

along with a small type-level language (via :ghc-flag:`-XDataKinds`)
for constructing pretty-printed error messages, ::

    -- ErrorMessage is intended to be used as a kind
    data ErrorMessage =
          Text Symbol                        -- Show this text as is
        | forall t. ShowType t               -- Pretty print a type
        | ErrorMessage :<>: ErrorMessage     -- Put two chunks of error message next to each other
        | ErrorMessage :$$: ErrorMessage     -- Put two chunks of error message above each other

in the :base-ref:`GHC.TypeLits.` module.

For instance, we might use this interface to provide a more useful error
message for applications of ``show`` on unsaturated functions like this, ::

    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE UndecidableInstances #-}

    import GHC.TypeLits

    instance TypeError (Text "Cannot 'Show' functions." :$$:
                        Text "Perhaps there is a missing argument?")
             => Show (a -> b) where
       showsPrec = error "unreachable"

    main = print negate

Which will produce the following compile-time error,

.. code-block:: none

    Test.hs:12:8: error:
        • Cannot 'Show' functions.
          Perhaps there is a missing argument?
        • In the expression: print negate
          In an equation for ‘main’: main = print negate


.. _defer-type-errors:

Deferring type errors to runtime
================================

While developing, sometimes it is desirable to allow compilation to
succeed even if there are type errors in the code. Consider the
following case: ::

    module Main where

    a :: Int
    a = 'a'

    main = print "b"

Even though ``a`` is ill-typed, it is not used in the end, so if all
that we're interested in is ``main`` it can be useful to be able to
ignore the problems in ``a``.

For more motivation and details please refer to the
:ghc-wiki:`Wiki <DeferErrorsToRuntime>` page or the `original
paper <http://dreixel.net/research/pdf/epdtecp.pdf>`__.

Enabling deferring of type errors
---------------------------------

The flag :ghc-flag:`-fdefer-type-errors` controls whether type errors are
deferred to runtime. Type errors will still be emitted as warnings, but
will not prevent compilation. You can use :ghc-flag:`-Wno-type-errors
<-Wtype-errors>` to suppress these warnings.

This flag implies the :ghc-flag:`-fdefer-typed-holes` and
:ghc-flag:`-fdefer-out-of-scope-variables` flags, which enables this behaviour
for `typed holes <#typed-holes>`__ and variables. Should you so wish, it is
possible to enable :ghc-flag:`-fdefer-type-errors` without enabling
:ghc-flag:`-fdefer-typed-holes` or :ghc-flag:`-fdefer-out-of-scope-variables`,
by explicitly specifying :ghc-flag:`-fno-defer-typed-holes
<-fdefer-typed-holes>` or :ghc-flag:`-fno-defer-out-of-scope-variables
<-fdefer-out-of-scope-variables>` on the command-line after the
:ghc-flag:`-fdefer-type-errors` flag.

At runtime, whenever a term containing a type error would need to be
evaluated, the error is converted into a runtime exception of type
``TypeError``. Note that type errors are deferred as much as possible
during runtime, but invalid coercions are never performed, even when
they would ultimately result in a value of the correct type. For
example, given the following code: ::

    x :: Int
    x = 0

    y :: Char
    y = x

    z :: Int
    z = y

evaluating ``z`` will result in a runtime ``TypeError``.

Deferred type errors in GHCi
----------------------------

The flag :ghc-flag:`-fdefer-type-errors` works in GHCi as well, with one
exception: for "naked" expressions typed at the prompt, type errors
don't get delayed, so for example: ::

    Prelude> fst (True, 1 == 'a')

    <interactive>:2:12:
        No instance for (Num Char) arising from the literal `1'
        Possible fix: add an instance declaration for (Num Char)
        In the first argument of `(==)', namely `1'
        In the expression: 1 == 'a'
        In the first argument of `fst', namely `(True, 1 == 'a')'

Otherwise, in the common case of a simple type error such as typing
``reverse True`` at the prompt, you would get a warning and then an
immediately-following type error when the expression is evaluated.

This exception doesn't apply to statements, as the following example
demonstrates:

.. code-block:: none

    Prelude> let x = (True, 1 == 'a')

    <interactive>:3:16: Warning:
        No instance for (Num Char) arising from the literal `1'
        Possible fix: add an instance declaration for (Num Char)
        In the first argument of `(==)', namely `1'
        In the expression: 1 == 'a'
        In the expression: (True, 1 == 'a')
    Prelude> fst x
    True

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
:th-ref:`Haddock reference documentation <Language.Haskell.TH.>`.
Many changes to the original
design are described in `Notes on Template Haskell version
2 <http://research.microsoft.com/~simonpj/papers/meta-haskell/notes2.ps>`__.
Not all of these changes are in GHC, however.

The first example from that paper is set out below (:ref:`th-example`)
as a worked example to help get you started.

The documentation here describes the realisation of Template Haskell in
GHC. It is not detailed enough to understand Template Haskell; see the
`Wiki page <http://haskell.org/haskellwiki/Template_Haskell>`__.

.. _th-syntax:

Syntax
------

.. ghc-flag:: -XTemplateHaskell

    :since: 6.0. Typed splices introduced in GHC 7.8.1.
    :implies: :ghc-flag:`-XTemplateHaskellQuotes`

    Enable Template Haskell's splice and quotation syntax.

.. ghc-flag:: -XTemplateHaskellQuotes

    :since: 8.0.1

    Enable only Template Haskell's quotation syntax.

Template Haskell has the following new syntactic constructions. You need to use
the flag :ghc-flag:`-XTemplateHaskell` to switch these syntactic extensions on.
Alternatively, the :ghc-flag:`-XTemplateHaskellQuotes` flag can be used to
enable the quotation subset of Template Haskell (i.e. without splice syntax).
The :ghc-flag:`-XTemplateHaskellQuotes` extension is considered safe under
:ref:`safe-haskell` while :ghc-flag:`-XTemplateHaskell` is not.

-  A splice is written ``$x``, where ``x`` is an identifier, or
   ``$(...)``, where the "..." is an arbitrary expression. There must be
   no space between the "$" and the identifier or parenthesis. This use
   of "$" overrides its meaning as an infix operator, just as "M.x"
   overrides the meaning of "." as an infix operator. If you want the
   infix operator, put spaces around it.

   A splice can occur in place of

   -  an expression; the spliced expression must have type ``Q Exp``

   -  a pattern; the spliced pattern must have type ``Q Pat``

   -  a type; the spliced expression must have type ``Q Type``

   -  a list of declarations at top level; the spliced expression must
      have type ``Q [Dec]``

   Inside a splice you can only call functions defined in imported
   modules, not functions defined elsewhere in the same module. Note
   that declaration splices are not allowed anywhere except at top level
   (outside any other declarations).

-  A expression quotation is written in Oxford brackets, thus:

   -  ``[| ... |]``, or ``[e| ... |]``, where the "..." is an
      expression; the quotation has type ``Q Exp``.

   -  ``[d| ... |]``, where the "..." is a list of top-level
      declarations; the quotation has type ``Q [Dec]``.

   -  ``[t| ... |]``, where the "..." is a type; the quotation has type
      ``Q Type``.

   -  ``[p| ... |]``, where the "..." is a pattern; the quotation has
      type ``Q Pat``.

   See :ref:`pts-where` for using partial type signatures in quotations.

-  A *typed* expression splice is written ``$$x``, where ``x`` is an
   identifier, or ``$$(...)``, where the "..." is an arbitrary
   expression.

   A typed expression splice can occur in place of an expression; the
   spliced expression must have type ``Q (TExp a)``

-  A *typed* expression quotation is written as ``[|| ... ||]``, or
   ``[e|| ... ||]``, where the "..." is an expression; if the "..."
   expression has type ``a``, then the quotation has type
   ``Q (TExp a)``.

   Values of type ``TExp a`` may be converted to values of type ``Exp``
   using the function ``unType :: TExp a -> Exp``.

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

       add1 :: Int -> Q Exp
       add1 x = [| x + 1 |]

   Now consider a splice using ``add1`` in a separate
   module: ::

       module Foo where

       import Bar

       two :: Int
       two = $(add1 1)

   Template Haskell cannot know what the argument to ``add1`` will be at the
   function's definition site, so a lifting mechanism is used to promote
   ``x`` into a value of type ``Q Exp``. This functionality is exposed to the
   user as the ``Lift`` typeclass in the ``Language.Haskell.TH.Syntax``
   module. If a type has a ``Lift`` instance, then any of its values can be
   lifted to a Template Haskell expression: ::

       class Lift t where
           lift :: t -> Q Exp

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
   instances automatically by using the :ghc-flag:`-XDeriveLift` language extension.
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

       mkPat :: Q Pat
       mkPat = [p| (x, y) |]

       -- in another module:
       foo :: (Char, String) -> String
       foo $(mkPat) = x : z

       bar :: Q Exp
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

   -  Recursive ``do``-statements (see :ghc-ticket:`1262`)

   -  Type holes in typed splices (see :ghc-ticket:`10945` and
      :ghc-ticket:`10946`)

(Compared to the original paper, there are many differences of detail.
The syntax for a declaration splice uses "``$``" not "``splice``". The type of
the enclosed expression must be ``Q [Dec]``, not ``[Q Dec]``. Typed expression
splices and quotations are supported.)

.. _th-usage:

Using Template Haskell
----------------------

-  The data types and monadic constructor functions for Template Haskell
   are in the library ``Language.Haskell.TH.Syntax``.

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

The flag :ghc-flag:`-dth-dec-file=⟨file⟩` shows the expansions of all top-level
TH declaration splices, both typed and untyped, in the file :file:`M.th.hs`
where M is the name of the module being compiled. Note that other types of
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

Below is the output of the same sample using :ghc-flag:`-dth-dec-file=⟨file⟩` ::

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
    gen :: [Format] -> Q Exp
    gen [D]   = [| \n -> show n |]
    gen [S]   = [| \s -> s |]
    gen [L s] = stringE s

    -- Here we generate the Haskell code for the splice
    -- from an input format string.
    pr :: String -> Q Exp
    pr s = gen (parse s)

Now run the compiler,

.. code-block:: none

    $ ghc --make -XTemplateHaskell main.hs -o main

Run :file:`main` and here is your output:

.. code-block:: none

    $ ./main
    Hello

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

.. ghc-flag:: -XQuasiQuotes

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
   ``Language.Haskell.TH.Quote.QuasiQuoter``, which is defined thus: ::

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

    :ghc-flag:`-XQuasiQuotes` introduces an unfortunate ambiguity with list
    comprehension syntax. Consider the following, ::

        let x = [v| v <- [0..10]]

    Without :ghc-flag:`-XQuasiQuotes` this is parsed as a list comprehension.
    With :ghc-flag:`-XQuasiQuotes` this is parsed as a quasi-quote; however,
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

.. _arrow-notation:

Arrow notation
==============

.. ghc-flag:: -XArrows

    Enable arrow notation.

Arrows are a generalisation of monads introduced by John Hughes. For
more details, see

-  “Generalising Monads to Arrows”, John Hughes, in Science of Computer
   Programming 37, pp. 67–111, May 2000. The paper that introduced arrows:
   a friendly introduction, motivated with programming examples.

-  “\ `A New Notation for
   Arrows <http://www.soi.city.ac.uk/~ross/papers/notation.html>`__\ ”,
   Ross Paterson, in ICFP, Sep 2001. Introduced the notation described
   here.

-  “\ `Arrows and
   Computation <http://www.soi.city.ac.uk/~ross/papers/fop.html>`__\ ”,
   Ross Paterson, in The Fun of Programming, Palgrave, 2003.

-  “\ `Programming with
   Arrows <http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf>`__\ ”, John
   Hughes, in 5th International Summer School on Advanced Functional
   Programming, Lecture Notes in Computer Science vol. 3622, Springer,
   2004. This paper includes another introduction to the notation, with
   practical examples.

-  “\ `Type and Translation Rules for Arrow Notation in
   GHC <http://www.haskell.org/ghc/docs/papers/arrow-rules.pdf>`__\ ”,
   Ross Paterson and Simon Peyton Jones, September 16, 2004. A terse
   enumeration of the formal rules used (extracted from comments in the
   source code).

-  The arrows web page at
   ``http://www.haskell.org/arrows/`` <http://www.haskell.org/arrows/>`__.

With the :ghc-flag:`-XArrows` flag, GHC supports the arrow notation described in
the second of these papers, translating it using combinators from the
:base-ref:`Control.Arrow.` module.
What follows is a brief introduction to the notation; it won't make much
sense unless you've read Hughes's paper.

The extension adds a new kind of expression for defining arrows:

.. code-block:: none

    exp10 ::= ...
           |  proc apat -> cmd

where ``proc`` is a new keyword. The variables of the pattern are bound
in the body of the ``proc``-expression, which is a new sort of thing
called a command. The syntax of commands is as follows:

.. code-block:: none

    cmd   ::= exp10 -<  exp
           |  exp10 -<< exp
           |  cmd0

with ⟨cmd⟩\ :sup:`0` up to ⟨cmd⟩\ :sup:`9` defined using infix operators
as for expressions, and

.. code-block:: none

    cmd10 ::= \ apat ... apat -> cmd
           |  let decls in cmd
           |  if exp then cmd else cmd
           |  case exp of { calts }
           |  do { cstmt ; ... cstmt ; cmd }
           |  fcmd

    fcmd  ::= fcmd aexp
           |  ( cmd )
           |  (| aexp cmd ... cmd |)

    cstmt ::= let decls
           |  pat <- cmd
           |  rec { cstmt ; ... cstmt [;] }
           |  cmd

where ⟨calts⟩ are like ⟨alts⟩ except that the bodies are commands
instead of expressions.

Commands produce values, but (like monadic computations) may yield more
than one value, or none, and may do other things as well. For the most
part, familiarity with monadic notation is a good guide to using
commands. However the values of expressions, even monadic ones, are
determined by the values of the variables they contain; this is not
necessarily the case for commands.

A simple example of the new notation is the expression ::

    proc x -> f -< x+1

We call this a procedure or arrow abstraction. As with a lambda
expression, the variable ``x`` is a new variable bound within the
``proc``-expression. It refers to the input to the arrow. In the above
example, ``-<`` is not an identifier but an new reserved symbol used for
building commands from an expression of arrow type and an expression to
be fed as input to that arrow. (The weird look will make more sense
later.) It may be read as analogue of application for arrows. The above
example is equivalent to the Haskell expression ::

    arr (\ x -> x+1) >>> f

That would make no sense if the expression to the left of ``-<``
involves the bound variable ``x``. More generally, the expression to the
left of ``-<`` may not involve any local variable, i.e. a variable bound
in the current arrow abstraction. For such a situation there is a
variant ``-<<``, as in ::

    proc x -> f x -<< x+1

which is equivalent to ::

    arr (\ x -> (f x, x+1)) >>> app

so in this case the arrow must belong to the ``ArrowApply`` class. Such
an arrow is equivalent to a monad, so if you're using this form you may
find a monadic formulation more convenient.

do-notation for commands
------------------------

Another form of command is a form of ``do``-notation. For example, you
can write ::

    proc x -> do
            y <- f -< x+1
            g -< 2*y
            let z = x+y
            t <- h -< x*z
            returnA -< t+z

You can read this much like ordinary ``do``-notation, but with commands
in place of monadic expressions. The first line sends the value of
``x+1`` as an input to the arrow ``f``, and matches its output against
``y``. In the next line, the output is discarded. The arrow ``returnA``
is defined in the :base-ref:`Control.Arrow.` module as ``arr id``. The above
example is treated as an abbreviation for ::

    arr (\ x -> (x, x)) >>>
            first (arr (\ x -> x+1) >>> f) >>>
            arr (\ (y, x) -> (y, (x, y))) >>>
            first (arr (\ y -> 2*y) >>> g) >>>
            arr snd >>>
            arr (\ (x, y) -> let z = x+y in ((x, z), z)) >>>
            first (arr (\ (x, z) -> x*z) >>> h) >>>
            arr (\ (t, z) -> t+z) >>>
            returnA

Note that variables not used later in the composition are projected out.
After simplification using rewrite rules (see :ref:`rewrite-rules`)
defined in the :base-ref:`Control.Arrow.` module, this reduces to ::

    arr (\ x -> (x+1, x)) >>>
            first f >>>
            arr (\ (y, x) -> (2*y, (x, y))) >>>
            first g >>>
            arr (\ (_, (x, y)) -> let z = x+y in (x*z, z)) >>>
            first h >>>
            arr (\ (t, z) -> t+z)

which is what you might have written by hand. With arrow notation, GHC
keeps track of all those tuples of variables for you.

Note that although the above translation suggests that ``let``-bound
variables like ``z`` must be monomorphic, the actual translation
produces Core, so polymorphic variables are allowed.

It's also possible to have mutually recursive bindings, using the new
``rec`` keyword, as in the following example: ::

    counter :: ArrowCircuit a => a Bool Int
    counter = proc reset -> do
            rec     output <- returnA -< if reset then 0 else next
                    next <- delay 0 -< output+1
            returnA -< output

The translation of such forms uses the ``loop`` combinator, so the arrow
concerned must belong to the ``ArrowLoop`` class.

Conditional commands
--------------------

In the previous example, we used a conditional expression to construct
the input for an arrow. Sometimes we want to conditionally execute
different commands, as in ::

    proc (x,y) ->
            if f x y
            then g -< x+1
            else h -< y+2

which is translated to ::

    arr (\ (x,y) -> if f x y then Left x else Right y) >>>
            (arr (\x -> x+1) >>> g) ||| (arr (\y -> y+2) >>> h)

Since the translation uses ``|||``, the arrow concerned must belong to
the ``ArrowChoice`` class.

There are also ``case`` commands, like ::

    case input of
        [] -> f -< ()
        [x] -> g -< x+1
        x1:x2:xs -> do
            y <- h -< (x1, x2)
            ys <- k -< xs
            returnA -< y:ys

The syntax is the same as for ``case`` expressions, except that the
bodies of the alternatives are commands rather than expressions. The
translation is similar to that of ``if`` commands.

Defining your own control structures
------------------------------------

As we're seen, arrow notation provides constructs, modelled on those for
expressions, for sequencing, value recursion and conditionals. But
suitable combinators, which you can define in ordinary Haskell, may also
be used to build new commands out of existing ones. The basic idea is
that a command defines an arrow from environments to values. These
environments assign values to the free local variables of the command.
Thus combinators that produce arrows from arrows may also be used to
build commands from commands. For example, the ``ArrowPlus`` class
includes a combinator ::

    ArrowPlus a => (<+>) :: a b c -> a b c -> a b c

so we can use it to build commands: ::

    expr' = proc x -> do
                    returnA -< x
            <+> do
                    symbol Plus -< ()
                    y <- term -< ()
                    expr' -< x + y
            <+> do
                    symbol Minus -< ()
                    y <- term -< ()
                    expr' -< x - y

(The ``do`` on the first line is needed to prevent the first ``<+> ...``
from being interpreted as part of the expression on the previous line.)
This is equivalent to ::

    expr' = (proc x -> returnA -< x)
            <+> (proc x -> do
                    symbol Plus -< ()
                    y <- term -< ()
                    expr' -< x + y)
            <+> (proc x -> do
                    symbol Minus -< ()
                    y <- term -< ()
                    expr' -< x - y)

We are actually using ``<+>`` here with the more specific type ::

    ArrowPlus a => (<+>) :: a (e,()) c -> a (e,()) c -> a (e,()) c

It is essential that this operator be polymorphic in ``e`` (representing
the environment input to the command and thence to its subcommands) and
satisfy the corresponding naturality property ::

    arr (first k) >>> (f <+> g) = (arr (first k) >>> f) <+> (arr (first k) >>> g)

at least for strict ``k``. (This should be automatic if you're not using
``seq``.) This ensures that environments seen by the subcommands are
environments of the whole command, and also allows the translation to
safely trim these environments. (The second component of the input pairs
can contain unnamed input values, as described in the next section.) The
operator must also not use any variable defined within the current arrow
abstraction.

We could define our own operator ::

    untilA :: ArrowChoice a => a (e,s) () -> a (e,s) Bool -> a (e,s) ()
    untilA body cond = proc x -> do
            b <- cond -< x
            if b then returnA -< ()
            else do
                    body -< x
                    untilA body cond -< x

and use it in the same way. Of course this infix syntax only makes sense
for binary operators; there is also a more general syntax involving
special brackets: ::

    proc x -> do
            y <- f -< x+1
            (|untilA (increment -< x+y) (within 0.5 -< x)|)

Primitive constructs
--------------------

Some operators will need to pass additional inputs to their subcommands.
For example, in an arrow type supporting exceptions, the operator that
attaches an exception handler will wish to pass the exception that
occurred to the handler. Such an operator might have a type ::

    handleA :: ... => a (e,s) c -> a (e,(Ex,s)) c -> a (e,s) c

where ``Ex`` is the type of exceptions handled. You could then use this
with arrow notation by writing a command ::

    body `handleA` \ ex -> handler

so that if an exception is raised in the command ``body``, the variable
``ex`` is bound to the value of the exception and the command
``handler``, which typically refers to ``ex``, is entered. Though the
syntax here looks like a functional lambda, we are talking about
commands, and something different is going on. The input to the arrow
represented by a command consists of values for the free local variables
in the command, plus a stack of anonymous values. In all the prior
examples, we made no assumptions about this stack. In the second
argument to ``handleA``, the value of the exception has been added to
the stack input to the handler. The command form of lambda merely gives
this value a name.

More concretely, the input to a command consists of a pair of an
environment and a stack. Each value on the stack is paired with the
remainder of the stack, with an empty stack being ``()``. So operators
like ``handleA`` that pass extra inputs to their subcommands can be
designed for use with the notation by placing the values on the stack
paired with the environment in this way. More precisely, the type of
each argument of the operator (and its result) should have the form ::

    a (e, (t1, ... (tn, ())...)) t

where ⟨e⟩ is a polymorphic variable (representing the environment) and
⟨ti⟩ are the types of the values on the stack, with ⟨t1⟩ being the
"top". The polymorphic variable ⟨e⟩ must not occur in ⟨a⟩, ⟨ti⟩ or ⟨t⟩.
However the arrows involved need not be the same. Here are some more
examples of suitable operators: ::

    bracketA :: ... => a (e,s) b -> a (e,(b,s)) c -> a (e,(c,s)) d -> a (e,s) d
    runReader :: ... => a (e,s) c -> a' (e,(State,s)) c
    runState :: ... => a (e,s) c -> a' (e,(State,s)) (c,State)

We can supply the extra input required by commands built with the last
two by applying them to ordinary expressions, as in ::

    proc x -> do
            s <- ...
            (|runReader (do { ... })|) s

which adds ``s`` to the stack of inputs to the command built using
``runReader``.

The command versions of lambda abstraction and application are analogous
to the expression versions. In particular, the beta and eta rules
describe equivalences of commands. These three features (operators,
lambda abstraction and application) are the core of the notation;
everything else can be built using them, though the results would be
somewhat clumsy. For example, we could simulate ``do``\-notation by
defining ::

    bind :: Arrow a => a (e,s) b -> a (e,(b,s)) c -> a (e,s) c
    u `bind` f = returnA &&& u >>> f

    bind_ :: Arrow a => a (e,s) b -> a (e,s) c -> a (e,s) c
    u `bind_` f = u `bind` (arr fst >>> f)

We could simulate ``if`` by defining ::

    cond :: ArrowChoice a => a (e,s) b -> a (e,s) b -> a (e,(Bool,s)) b
    cond f g = arr (\ (e,(b,s)) -> if b then Left (e,s) else Right (e,s)) >>> f ||| g

Differences with the paper
--------------------------

-  Instead of a single form of arrow application (arrow tail) with two
   translations, the implementation provides two forms ``-<``
   (first-order) and ``-<<`` (higher-order).

-  User-defined operators are flagged with banana brackets instead of a
   new ``form`` keyword.

-  In the paper and the previous implementation, values on the stack
   were paired to the right of the environment in a single argument, but
   now the environment and stack are separate arguments.

Portability
-----------

Although only GHC implements arrow notation directly, there is also a
preprocessor (available from the `arrows web
page <http://www.haskell.org/arrows/>`__) that translates arrow notation
into Haskell 98 for use with other Haskell systems. You would still want
to check arrow programs with GHC; tracing type errors in the
preprocessor output is not easy. Modules intended for both GHC and the
preprocessor must observe some additional restrictions:

-  The module must import :base-ref:`Control.Arrow.`.

-  The preprocessor cannot cope with other Haskell extensions. These
   would have to go in separate modules.

-  Because the preprocessor targets Haskell (rather than Core),
   ``let``\-bound variables are monomorphic.

.. _bang-patterns:

.. _strict-haskell:

Bang patterns and Strict Haskell
================================

.. index::
   single: strict haskell

.. index::
   single: Bang patterns

In high-performance Haskell code (e.g. numeric code) eliminating
thunks from an inner loop can be a huge win.
GHC supports three extensions to allow the programmer to specify
use of strict (call-by-value) evalution rather than lazy (call-by-need)
evaluation.

- Bang patterns (:ghc-flag:`-XBangPatterns`) makes pattern matching and
  let bindings stricter.

- Strict data types (:ghc-flag:`-XStrictData`) makes constructor fields
  strict by default, on a per-module basis.

- Strict pattern (:ghc-flag:`-XStrict`) makes all patterns and let bindings
  strict by default, on a per-module basis.

The latter two extensions are simply a way to avoid littering high-performance
code with bang patterns, making it harder to read.

Bang patterns and strict matching do not affect the type system in any way.

.. _bang-patterns-informal:

Bang patterns
-------------

.. ghc-flag:: -XBangPatterns

    Allow use of bang pattern syntax.

GHC supports an extension of pattern matching called *bang patterns*,
written ``!pat``. Bang patterns are under consideration for Haskell
Prime. The `Haskell prime feature
description <http://ghc.haskell.org/trac/haskell-prime/wiki/BangPatterns>`__
contains more discussion and examples than the material below.

The main idea is to add a single new production to the syntax of
patterns: ::

      pat ::= !pat

Matching an expression ``e`` against a pattern ``!p`` is done by first
evaluating ``e`` (to WHNF) and then matching the result against ``p``.
Example: ::

    f1 !x = True

This definition makes ``f1`` is strict in ``x``, whereas without the
bang it would be lazy. Bang patterns can be nested of course: ::

    f2 (!x, y) = [x,y]

Here, ``f2`` is strict in ``x`` but not in ``y``.

Note the following points:

- A bang only really has
  an effect if it precedes a variable or wild-card pattern: ::

    f3 !(x,y) = [x,y]
    f4 (x,y)  = [x,y]

  Here, ``f3`` and ``f4`` are identical; putting a bang before a pattern
  that forces evaluation anyway does nothing.

- A bang pattern is allowed in a let or where clause, and makes the binding
  strict.  For example: ::

    let !x = e in body
    let !(p,q) = e in body

  In both cases ``e`` is evaluated before starting to evaluate ``body``.

  However, *nested* bangs in a let/where pattern binding behave uniformly with all
  other forms of pattern matching. For example ::

    let (!x,[y]) = e in b

  is equivalent to this: ::

    let { t = case e of (x,[y]) -> x `seq` (x,y)
          x = fst t
          y = snd t }
    in b

  The binding is lazy, but when either ``x`` or ``y`` is evaluated by
  ``b`` the entire pattern is matched, including forcing the evaluation of
  ``x``.

  See :ref:`Semantics of let bindings with bang patterns <recursive-and-polymorphic-let-bindings>` for
  the detailed semantics.

- A pattern with a bang at the outermost level is not allowed at the top
  level of a module.

- Bang patterns work in ``case`` expressions too, of course: ::

    g5 x = let y = f x in body
    g6 x = case f x of { y -> body }
    g7 x = case f x of { !y -> body }

  The functions ``g5`` and ``g6`` mean exactly the same thing. But ``g7``
  evaluates ``(f x)``, binds ``y`` to the result, and then evaluates
  ``body``.

- There is one problem with syntactic ambiguity. Consider: ::

    f !x = 3

  Is this a definition of the infix function "``(!)``", or of the "``f``"
  with a bang pattern? GHC resolves this ambiguity in favour of the
  latter. If you want to define ``(!)`` with bang-patterns enabled, you
  have to do so using prefix notation: ::

    (!) f x = 3


.. _strict-data:

Strict-by-default data types
----------------------------

.. ghc-flag:: -XStrictData

    :since: 8.0.1

    Make fields of data types defined in the current module strict by default.

Informally the ``StrictData`` language extension switches data type
declarations to be strict by default allowing fields to be lazy by
adding a ``~`` in front of the field.

When the user writes ::

          data T = C a
          data T' = C' ~a

we interpret it as if they had written ::

          data T = C !a
          data T' = C' a

The extension only affects definitions in this module.


.. _strict:

Strict-by-default pattern bindings
----------------------------------

.. ghc-flag:: -XStrict

    :implies: :ghc-flag:`-XStrictData`
    :since: 8.0.1

    Make bindings in the current module strict by default.

Informally the ``Strict`` language extension switches functions, data
types, and bindings to be strict by default, allowing optional laziness
by adding ``~`` in front of a variable. This essentially reverses the
present situation where laziness is default and strictness can be
optionally had by adding ``!`` in front of a variable.

``Strict`` implies :ref:`StrictData <strict-data>`.

-  **Function definitions**

   When the user writes ::

       f x = ...

   we interpret it as if they had written ::

       f !x = ...

   Adding ``~`` in front of ``x`` gives the regular lazy behavior.

-  **Let/where bindings**

   When the user writes ::

     let x = ...
     let pat = ...

   we interpret it as if they had written ::

     let !x = ...
     let !pat = ...

   Adding ``~`` in front of ``x`` gives the regular lazy
   behavior.
   The general rule is that we add an implicit bang on the outermost pattern,
   unless disabled with ``~``.

-  **Pattern matching in case expressions, lambdas, do-notation, etc**

   The outermost pattern of all pattern matches gets an implicit bang,
   unless disabled with ``~``.
   This applies to case expressions, patterns in lambda, do-notation,
   list comprehension, and so on.
   For example ::

       case x of (a,b) -> rhs

   is interpreted as ::

       case x of !(a,b) -> rhs

   Since the semantics of pattern matching in case expressions is
   strict, this usually has no effect whatsoever. But it does make a
   difference in the degenerate case of variables and newtypes. So ::

       case x of y -> rhs

   is lazy in Haskell, but with ``Strict`` is interpreted as ::

       case x of !y -> rhs

   which evaluates ``x``. Similarly, if ``newtype Age = MkAge Int``, then ::

       case x of MkAge i -> rhs

   is lazy in Haskell; but with ``Strict`` the added bang makes it
   strict.

   Similarly ::

      \ x -> body
      do { x <- rhs; blah }
      [ e | x <- rhs; blah }

   all get implicit bangs on the ``x`` pattern.

-  **Nested patterns**

   Notice that we do *not* put bangs on nested patterns. For
   example ::

     let (p,q) = if flob then (undefined, undefined) else (True, False)
     in ...

   will behave like ::

     let !(p,q) = if flob then (undefined, undefined) else (True,False)
     in ...

   which will strictly evaluate the right hand side, and bind ``p``
   and ``q`` to the components of the pair. But the pair itself is
   lazy (unless we also compile the ``Prelude`` with ``Strict``; see
   :ref:`strict-modularity` below). So ``p`` and ``q`` may end up bound to
   undefined. See also :ref:`recursive-and-polymorphic-let-bindings` below.

-  **Top level bindings**

   are unaffected by ``Strict``. For example: ::

       x = factorial 20
       (y,z) = if x > 10 then True else False

   Here ``x`` and the pattern binding ``(y,z)`` remain lazy. Reason:
   there is no good moment to force them, until first use.

-  **Newtypes**

   There is no effect on newtypes, which simply rename existing types.
   For example: ::

       newtype T = C a
       f (C x)  = rhs1
       g !(C x) = rhs2

   In ordinary Haskell, ``f`` is lazy in its argument and hence in
   ``x``; and ``g`` is strict in its argument and hence also strict in
   ``x``. With ``Strict``, both become strict because ``f``'s argument
   gets an implicit bang.


.. _strict-modularity:

Modularity
----------

``Strict`` and ``StrictData`` only affects definitions in the module
they are used in. Functions and data types imported from other modules
are unaffected. For example, we won't evaluate the argument to
``Just`` before applying the constructor.  Similarly we won't evaluate
the first argument to ``Data.Map.findWithDefault`` before applying the
function.

This is crucial to preserve correctness. Entities defined in other
modules might rely on laziness for correctness (whether functional or
performance).

Tuples, lists, ``Maybe``, and all the other types from ``Prelude``
continue to have their existing, lazy, semantics.

.. _bang-patterns-sem:
.. _recursive-and-polymorphic-let-bindings:

Dynamic semantics of bang patterns
----------------------------------

The semantics of Haskell pattern matching is described in `Section
3.17.2 <http://www.haskell.org/onlinereport/exps.html#sect3.17.2>`__ of
the Haskell Report. To this description add one extra item 10, saying:

-  Matching the pattern ``!pat`` against a value ``v`` behaves as
   follows:

   -  if ``v`` is bottom, the match diverges

   -  otherwise, ``pat`` is matched against ``v``

Similarly, in Figure 4 of `Section
3.17.3 <http://www.haskell.org/onlinereport/exps.html#sect3.17.3>`__,
add a new case (t): ::

    case v of { !pat -> e; _ -> e' }
       = v `seq` case v of { pat -> e; _ -> e' }

That leaves let expressions, whose translation is given in `Section
3.12 <http://www.haskell.org/onlinereport/exps.html#sect3.12>`__ of the
Haskell Report.
Replace the "Translation" there with the following one.  Given
``let { bind1 ... bindn } in body``:

.. admonition:: FORCE

    Replace any binding ``!p = e`` with ``v = case e of p -> (x1, ..., xn); (x1, ..., xn) = v`` and replace
    ``body`` with ``v seq body``, where ``v`` is fresh. This translation works fine if
    ``p`` is already a variable ``x``, but can obviously be optimised by not
    introducing a fresh variable ``v``.

.. admonition:: SPLIT

    Replace any binding ``p = e``, where ``p`` is not a variable, with
    ``v = e; x1 = case v of p -> x1; ...; xn = case v of p -> xn``, where
    ``v`` is fresh and ``x1``.. ``xn`` are the bound variables of ``p``.
    Again if ``e`` is a variable, this can be optimised by not introducing a
    fresh variable.

The result will be a (possibly) recursive set of bindings, binding
only simple variables on the left hand side. (One could go one step
further, as in the Haskell Report and make the recursive bindings
non-recursive using ``fix``, but we do not do so in Core, and it only
obfuscates matters, so we do not do so here.)

The translation is carefully crafted to make bang patterns meaningful
for recursive and polymorphic bindings as well as straightforward
non-recursive bindings.

Here are some examples of how this translation works. The first
expression of each sequence is Haskell source; the subsequent ones are
Core.

Here is a simple non-recursive case: ::

    let x :: Int     -- Non-recursive
        !x = factorial y
    in body

    ===> (FORCE)
        let x = factorial y in x `seq` body

    ===> (inline seq)
        let x = factorial y in case x of x -> body

    ===> (inline x)
        case factorial y of x -> body

Same again, only with a pattern binding: ::

    let !(Just x, Left y) = e in body

    ===> (FORCE)
        let v = case e of (Just x, Left y) -> (x,y)
            (x,y) = v
        in v `seq` body

    ===> (SPLIT)
        let v = case e of (Just x, Left y) -> (x,y)
            x = case v of (x,y) -> x
            y = case v of (x,y) -> y
        in v `seq` body

    ===> (inline seq, float x,y bindings inwards)
        let v = case e of (Just x, Left y) -> (x,y)
        in case v of v -> let x = case v of (x,y) -> x
                              y = case v of (x,y) -> y
                          in body

    ===> (fluff up v's pattern; this is a standard Core optimisation)
        let v = case e of (Just x, Left y) -> (x,y)
        in case v of v@(p,q) -> let x = case v of (x,y) -> x
                                    y = case v of (x,y) -> y
                                in body

    ===> (case of known constructor)
        let v = case e of (Just x, Left y) -> (x,y)
        in case v of v@(p,q) -> let x = p
                                    y = q
                                in body

    ===> (inline x,y, v)
        case (case e of (Just x, Left y) -> (x,y) of
            (p,q) -> body[p/x, q/y]

    ===> (case of case)
        case e of (Just x, Left y) -> body[p/x, q/y]

The final form is just what we want: a simple case expression.

Here is a recursive case ::

    letrec xs :: [Int]  -- Recursive
            !xs = factorial y : xs
    in body

    ===> (FORCE)
        letrec xs = factorial y : xs in xs `seq` body

    ===> (inline seq)
        letrec xs = factorial y : xs in case xs of xs -> body

    ===> (eliminate case of value)
        letrec xs = factorial y : xs in body

and a polymorphic one: ::

    let f :: forall a. [a] -> [a]    -- Polymorphic
        !f = fst (reverse, True)
    in body

    ===> (FORCE)
        let f = /\a. fst (reverse a, True) in f `seq` body
    ===> (inline seq, inline f)
        case (/\a. fst (reverse a, True)) of f -> body

Notice that the ``seq`` is added only in the translation to Core
If we did it in Haskell source, thus ::

   let f = ... in f `seq` body

then ``f``\ 's polymorphic type would get instantiated, so the Core
translation would be ::

   let f = ... in f Any `seq` body


When overloading is involved, the results might be slightly counter
intuitive: ::

    let f :: forall a. Eq a => a -> [a] -> Bool    -- Overloaded
        !f = fst (member, True)
    in body

    ===> (FORCE)
        let f = /\a \(d::Eq a). fst (member, True) in f `seq` body

    ===> (inline seq, case of value)
        let f = /\a \(d::Eq a). fst (member, True) in body

Note that the bang has no effect at all in this case


.. _assertions:

Assertions
==========

.. index::
   single: Assertions

If you want to make use of assertions in your standard Haskell code, you
could define a function like the following: ::

    assert :: Bool -> a -> a
    assert False x = error "assertion failed!"
    assert _     x = x

which works, but gives you back a less than useful error message -- an
assertion failed, but which and where?

One way out is to define an extended ``assert`` function which also
takes a descriptive string to include in the error message and perhaps
combine this with the use of a pre-processor which inserts the source
location where ``assert`` was used.

GHC offers a helping hand here, doing all of this for you. For every use
of ``assert`` in the user's source: ::

    kelvinToC :: Double -> Double
    kelvinToC k = assert (k >= 0.0) (k+273.15)

GHC will rewrite this to also include the source location where the
assertion was made, ::

    assert pred val ==> assertError "Main.hs|15" pred val

The rewrite is only performed by the compiler when it spots applications
of ``Control.Exception.assert``, so you can still define and use your
own versions of ``assert``, should you so wish. If not, import
``Control.Exception`` to make use ``assert`` in your code.

.. index::
   pair: assertions; disabling

GHC ignores assertions when optimisation is turned on with the
:ghc-flag:`-O` flag. That is, expressions of the form ``assert pred e``
will be rewritten to ``e``. You can also disable assertions using the
:ghc-flag:`-fignore-asserts` option. The option
:ghc-flag:`-fno-ignore-asserts <-fignore-asserts>`
allows enabling assertions even when optimisation is turned on.

Assertion failures can be caught, see the documentation for the
:base-ref:`Control.Exception` library for the details.

.. _static-pointers:

Static pointers
===============

.. index::
   single: Static pointers

.. ghc-flag:: -XStaticPointers

    :since: 7.10.1

    Allow use of static pointer syntax.

The language extension :ghc-flag:`-XStaticPointers` adds a new syntactic form
``static e``, which stands for a reference to the closed expression ⟨e⟩.
This reference is stable and portable, in the sense that it remains
valid across different processes on possibly different machines. Thus, a
process can create a reference and send it to another process that can
resolve it to ⟨e⟩.

With this extension turned on, ``static`` is no longer a valid
identifier.

Static pointers were first proposed in the paper `Towards Haskell in the
cloud <http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf>`__,
Jeff Epstein, Andrew P. Black and Simon Peyton-Jones, Proceedings of the
4th ACM Symposium on Haskell, pp. 118-129, ACM, 2011.

.. _using-static-pointers:

Using static pointers
---------------------

Each reference is given a key which can be used to locate it at runtime
with :base-ref:`GHC.StaticPtr.unsafeLookupStaticPtr`
which uses a global and immutable table called the Static Pointer Table.
The compiler includes entries in this table for all static forms found
in the linked modules. The value can be obtained from the reference via
:base-ref:`GHC.StaticPtr.deRefStaticPtr`.

The body ``e`` of a ``static e`` expression must be a closed expression. Where
we say an expression is *closed* when all of its free (type) variables are
closed. And a variable is *closed* if it is let-bound to a *closed* expression
and its type is *closed* as well. And a type is *closed* if it has no free
variables.

All of the following are permissible: ::

    inc :: Int -> Int
    inc x = x + 1

    ref1 = static 1
    ref2 = static inc
    ref3 = static (inc 1)
    ref4 = static ((\x -> x + 1) (1 :: Int))
    ref5 y = static (let x = 1 in x)
    ref6 y = let x = 1 in static x

While the following definitions are rejected: ::

    ref7 y = let x = y in static x    -- x is not closed
    ref8 y = static (let x = 1 in y)  -- y is not let-bound
    ref8 (y :: a) = let x = undefined :: a
                     in static x      -- x has a non-closed type

.. note::

    While modules loaded in GHCi with the :ghci-cmd:`:load` command may use
    :ghc-flag:`-XStaticPointers` and ``static`` expressions, statements
    entered on the REPL may not. This is a limitation of GHCi; see
    :ghc-ticket:`12356` for details.

.. _typechecking-static-pointers:

Static semantics of static pointers
-----------------------------------

Informally, if we have a closed expression ::

    e :: forall a_1 ... a_n . t

the static form is of type ::

    static e :: (IsStatic p, Typeable a_1, ... , Typeable a_n) => p t


A static form determines a value of type ``StaticPtr t``, but just
like ``OverloadedLists`` and ``OverloadedStrings``, this literal
expression is overloaded to allow lifting a ``StaticPtr`` into another
type implicitly, via the ``IsStatic`` class: ::

    class IsStatic p where
        fromStaticPtr :: StaticPtr a -> p a

The only predefined instance is the obvious one that does nothing: ::

    instance IsStatic StaticPtr where
        fromStaticPtr sptr = sptr

See :base-ref:`GHC.StaticPtr.IsStatic`.

Furthermore, type ``t`` is constrained to have a ``Typeable`` instance.
The following are therefore illegal: ::

    static show                    -- No Typeable instance for (Show a => a -> String)
    static Control.Monad.ST.runST  -- No Typeable instance for ((forall s. ST s a) -> a)

That being said, with the appropriate use of wrapper datatypes, the
above limitations induce no loss of generality: ::

    {-# LANGUAGE ConstraintKinds           #-}
    {-# LANGUAGE ExistentialQuantification #-}
    {-# LANGUAGE Rank2Types                #-}
    {-# LANGUAGE StandaloneDeriving        #-}
    {-# LANGUAGE StaticPointers            #-}

    import Control.Monad.ST
    import Data.Typeable
    import GHC.StaticPtr

    data Dict c = c => Dict

    g1 :: Typeable a => StaticPtr (Dict (Show a) -> a -> String)
    g1 = static (\Dict -> show)

    data Rank2Wrapper f = R2W (forall s. f s)
      deriving Typeable
    newtype Flip f a s = Flip { unFlip :: f s a }
      deriving Typeable

    g2 :: Typeable a => StaticPtr (Rank2Wrapper (Flip ST a) -> a)
    g2 = static (\(R2W f) -> runST (unFlip f))

.. _pragmas:

Pragmas
=======

.. index::
   single: pragma

GHC supports several pragmas, or instructions to the compiler placed in
the source code. Pragmas don't normally affect the meaning of the
program, but they might affect the efficiency of the generated code.

Pragmas all take the form ``{-# word ... #-}`` where ⟨word⟩ indicates
the type of pragma, and is followed optionally by information specific
to that type of pragma. Case is ignored in ⟨word⟩. The various values
for ⟨word⟩ that GHC understands are described in the following sections;
any pragma encountered with an unrecognised ⟨word⟩ is ignored. The
layout rule applies in pragmas, so the closing ``#-}`` should start in a
column to the right of the opening ``{-#``.

Certain pragmas are *file-header pragmas*:

-  A file-header pragma must precede the ``module`` keyword in the file.

-  There can be as many file-header pragmas as you please, and they can
   be preceded or followed by comments.

-  File-header pragmas are read once only, before pre-processing the
   file (e.g. with cpp).

-  The file-header pragmas are: ``{-# LANGUAGE #-}``,
   ``{-# OPTIONS_GHC #-}``, and ``{-# INCLUDE #-}``.

.. _language-pragma:

``LANGUAGE`` pragma
-------------------

.. index::
   single: LANGUAGE; pragma
   single: pragma; LANGUAGE

The ``LANGUAGE`` pragma allows language extensions to be enabled in a
portable way. It is the intention that all Haskell compilers support the
``LANGUAGE`` pragma with the same syntax, although not all extensions
are supported by all compilers, of course. The ``LANGUAGE`` pragma
should be used instead of ``OPTIONS_GHC``, if possible.

For example, to enable the FFI and preprocessing with CPP: ::

    {-# LANGUAGE ForeignFunctionInterface, CPP #-}

``LANGUAGE`` is a file-header pragma (see :ref:`pragmas`).

Every language extension can also be turned into a command-line flag by
prefixing it with "``-X``"; for example ``-XForeignFunctionInterface``.
(Similarly, all "``-X``" flags can be written as ``LANGUAGE`` pragmas.)

A list of all supported language extensions can be obtained by invoking
``ghc --supported-extensions`` (see :ghc-flag:`--supported-extensions`).

Any extension from the ``Extension`` type defined in
:cabal-ref:`Language.Haskell.Extension.` may be used. GHC will report an error
if any of the requested extensions are not supported.

.. _options-pragma:

``OPTIONS_GHC`` pragma
----------------------

.. index::
   single: OPTIONS_GHC
   single: pragma; OPTIONS_GHC

The ``OPTIONS_GHC`` pragma is used to specify additional options that
are given to the compiler when compiling this source file. See
:ref:`source-file-options` for details.

Previous versions of GHC accepted ``OPTIONS`` rather than
``OPTIONS_GHC``, but that is now deprecated.

``OPTIONS_GHC`` is a file-header pragma (see :ref:`pragmas`).

.. _include-pragma:

``INCLUDE`` pragma
------------------

The ``INCLUDE`` used to be necessary for specifying header files to be
included when using the FFI and compiling via C. It is no longer
required for GHC, but is accepted (and ignored) for compatibility with
other compilers.

.. _warning-deprecated-pragma:

``WARNING`` and ``DEPRECATED`` pragmas
--------------------------------------

.. index::
   single: WARNING
   single: DEPRECATED

The ``WARNING`` pragma allows you to attach an arbitrary warning to a
particular function, class, or type. A ``DEPRECATED`` pragma lets you
specify that a particular function, class, or type is deprecated. There
are two ways of using these pragmas.

-  You can work on an entire module thus: ::

          module Wibble {-# DEPRECATED "Use Wobble instead" #-} where
            ...

   Or: ::

          module Wibble {-# WARNING "This is an unstable interface." #-} where
            ...

   When you compile any module that import ``Wibble``, GHC will print
   the specified message.

-  You can attach a warning to a function, class, type, or data
   constructor, with the following top-level declarations: ::

          {-# DEPRECATED f, C, T "Don't use these" #-}
          {-# WARNING unsafePerformIO "This is unsafe; I hope you know what you're doing" #-}

   When you compile any module that imports and uses any of the
   specified entities, GHC will print the specified message.

   You can only attach to entities declared at top level in the module
   being compiled, and you can only use unqualified names in the list of
   entities. A capitalised name, such as ``T`` refers to *either* the
   type constructor ``T`` *or* the data constructor ``T``, or both if
   both are in scope. If both are in scope, there is currently no way to
   specify one without the other (c.f. fixities :ref:`infix-tycons`).

Also note that the argument to ``DEPRECATED`` and ``WARNING`` can also be a list
of strings, in which case the strings will be presented on separate lines in the
resulting warning message, ::

    {-# DEPRECATED foo, bar ["Don't use these", "Use gar instead"] #-}

Warnings and deprecations are not reported for (a) uses within the
defining module, (b) defining a method in a class instance, and (c) uses
in an export list. The latter reduces spurious complaints within a
library in which one module gathers together and re-exports the exports
of several others.

You can suppress the warnings with the flag
:ghc-flag:`-Wno-warnings-deprecations <-Wwarnings-deprecations>`.

.. _minimal-pragma:

``MINIMAL`` pragma
------------------

.. index::
   single: MINIMAL
   single: pragma; MINIMAL

The ``MINIMAL`` pragma is used to specify the minimal complete definition of
a class, i.e. specify which methods must be implemented by all
instances. If an instance does not satisfy the minimal complete
definition, then a warning is generated. This can be useful when a class
has methods with circular defaults. For example ::

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)
        {-# MINIMAL (==) | (/=) #-}

Without the ``MINIMAL`` pragma no warning would be generated for an instance
that implements neither method.

The syntax for minimal complete definition is: ::

    mindef ::= name
            |  '(' mindef ')'
            |  mindef '|' mindef
            |  mindef ',' mindef

A vertical bar denotes disjunction, i.e. one of the two sides is
required. A comma denotes conjunction, i.e. both sides are required.
Conjunction binds stronger than disjunction.

If no ``MINIMAL`` pragma is given in the class declaration, it is just as if
a pragma ``{-# MINIMAL op1, op2, ..., opn #-}`` was given, where the
``opi`` are the methods (a) that lack a default method in the class
declaration, and (b) whose name that does not start with an underscore
(c.f. :ghc-flag:`-Wmissing-methods`, :ref:`options-sanity`).

This warning can be turned off with the flag
:ghc-flag:`-Wno-missing-methods <-Wmissing-methods>`.

.. _inline-noinline-pragma:

``INLINE`` and ``NOINLINE`` pragmas
-----------------------------------

These pragmas control the inlining of function definitions.

.. _inline-pragma:

``INLINE`` pragma
~~~~~~~~~~~~~~~~~

.. index::
   single: INLINE
   single: pragma; INLINE

GHC (with :ghc-flag:`-O`, as always) tries to inline (or "unfold")
functions/values that are "small enough," thus avoiding the call
overhead and possibly exposing other more-wonderful optimisations. GHC
has a set of heuristics, tuned over a long period of time using many
benchmarks, that decide when it is beneficial to inline a function at
its call site. The heuristics are designed to inline functions when it
appears to be beneficial to do so, but without incurring excessive code
bloat. If a function looks too big, it won't be inlined, and functions
larger than a certain size will not even have their definition exported
in the interface file. Some of the thresholds that govern these
heuristic decisions can be changed using flags, see :ref:`options-f`.

Normally GHC will do a reasonable job of deciding by itself when it is a
good idea to inline a function. However, sometimes you might want to
override the default behaviour. For example, if you have a key function
that is important to inline because it leads to further optimisations,
but GHC judges it to be too big to inline.

The sledgehammer you can bring to bear is the ``INLINE`` pragma, used thusly: ::

    key_function :: Int -> String -> (Bool, Double)
    {-# INLINE key_function #-}

The major effect of an ``INLINE`` pragma is to declare a function's
"cost" to be very low. The normal unfolding machinery will then be very
keen to inline it. However, an ``INLINE`` pragma for a function "``f``"
has a number of other effects:

-  While GHC is keen to inline the function, it does not do so blindly.
   For example, if you write ::

       map key_function xs

   there really isn't any point in inlining ``key_function`` to get ::

       map (\x -> body) xs

   In general, GHC only inlines the function if there is some reason (no
   matter how slight) to suppose that it is useful to do so.

-  Moreover, GHC will only inline the function if it is *fully applied*,
   where "fully applied" means applied to as many arguments as appear
   (syntactically) on the LHS of the function definition. For example: ::

       comp1 :: (b -> c) -> (a -> b) -> a -> c
       {-# INLINE comp1 #-}
       comp1 f g = \x -> f (g x)

       comp2 :: (b -> c) -> (a -> b) -> a -> c
       {-# INLINE comp2 #-}
       comp2 f g x = f (g x)

   The two functions ``comp1`` and ``comp2`` have the same semantics,
   but ``comp1`` will be inlined when applied to *two* arguments, while
   ``comp2`` requires *three*. This might make a big difference if you
   say ::

       map (not `comp1` not) xs

   which will optimise better than the corresponding use of ``comp2``.

-  It is useful for GHC to optimise the definition of an INLINE function
   ``f`` just like any other non-INLINE function, in case the
   non-inlined version of ``f`` is ultimately called. But we don't want
   to inline the *optimised* version of ``f``; a major reason for ``INLINE``
   pragmas is to expose functions in ``f``\'s RHS that have rewrite
   rules, and it's no good if those functions have been optimised away.

   So *GHC guarantees to inline precisely the code that you wrote*, no
   more and no less. It does this by capturing a copy of the definition
   of the function to use for inlining (we call this the "inline-RHS"),
   which it leaves untouched, while optimising the ordinarily RHS as
   usual. For externally-visible functions the inline-RHS (not the
   optimised RHS) is recorded in the interface file.

-  An INLINE function is not worker/wrappered by strictness analysis.
   It's going to be inlined wholesale instead.

GHC ensures that inlining cannot go on forever: every mutually-recursive
group is cut by one or more *loop breakers* that is never inlined (see
`Secrets of the GHC inliner, JFP 12(4) July
2002 <http://research.microsoft.com/%7Esimonpj/Papers/inlining/index.htm>`__).
GHC tries not to select a function with an ``INLINE`` pragma as a loop
breaker, but when there is no choice even an INLINE function can be
selected, in which case the ``INLINE`` pragma is ignored. For example, for a
self-recursive function, the loop breaker can only be the function
itself, so an ``INLINE`` pragma is always ignored.

Syntactically, an ``INLINE`` pragma for a function can be put anywhere
its type signature could be put.

``INLINE`` pragmas are a particularly good idea for the
``then``/``return`` (or ``bind``/``unit``) functions in a monad. For
example, in GHC's own ``UniqueSupply`` monad code, we have: ::

    {-# INLINE thenUs #-}
    {-# INLINE returnUs #-}

See also the ``NOINLINE`` (:ref:`noinline-pragma`) and ``INLINABLE``
(:ref:`inlinable-pragma`) pragmas.

.. _inlinable-pragma:

``INLINABLE`` pragma
~~~~~~~~~~~~~~~~~~~~

An ``{-# INLINABLE f #-}`` pragma on a function ``f`` has the following
behaviour:

-  While ``INLINE`` says "please inline me", the ``INLINABLE`` says
   "feel free to inline me; use your discretion". In other words the
   choice is left to GHC, which uses the same rules as for pragma-free
   functions. Unlike ``INLINE``, that decision is made at the *call
   site*, and will therefore be affected by the inlining threshold,
   optimisation level etc.

-  Like ``INLINE``, the ``INLINABLE`` pragma retains a copy of the
   original RHS for inlining purposes, and persists it in the interface
   file, regardless of the size of the RHS.

-  One way to use ``INLINABLE`` is in conjunction with the special
   function ``inline`` (:ref:`special-ids`). The call ``inline f`` tries
   very hard to inline ``f``. To make sure that ``f`` can be inlined, it
   is a good idea to mark the definition of ``f`` as ``INLINABLE``, so
   that GHC guarantees to expose an unfolding regardless of how big it
   is. Moreover, by annotating ``f`` as ``INLINABLE``, you ensure that
   ``f``\'s original RHS is inlined, rather than whatever random
   optimised version of ``f`` GHC's optimiser has produced.

-  The ``INLINABLE`` pragma also works with ``SPECIALISE``: if you mark
   function ``f`` as ``INLINABLE``, then you can subsequently
   ``SPECIALISE`` in another module (see :ref:`specialize-pragma`).

-  Unlike ``INLINE``, it is OK to use an ``INLINABLE`` pragma on a
   recursive function. The principal reason do to so to allow later use
   of ``SPECIALISE``

The alternative spelling ``INLINEABLE`` is also accepted by GHC.

.. _noinline-pragma:

``NOINLINE`` pragma
~~~~~~~~~~~~~~~~~~~

.. index::
   single: NOINLINE
   single: NOTINLINE

The ``NOINLINE`` pragma does exactly what you'd expect: it stops the
named function from being inlined by the compiler. You shouldn't ever
need to do this, unless you're very cautious about code size.

``NOTINLINE`` is a synonym for ``NOINLINE`` (``NOINLINE`` is specified
by Haskell 98 as the standard way to disable inlining, so it should be
used if you want your code to be portable).

.. _conlike-pragma:

``CONLIKE`` modifier
~~~~~~~~~~~~~~~~~~~~

.. index::
   single: CONLIKE

An ``INLINE`` or ``NOINLINE`` pragma may have a ``CONLIKE`` modifier, which affects
matching in RULEs (only). See :ref:`conlike`.

.. _phase-control:

Phase control
~~~~~~~~~~~~~

Sometimes you want to control exactly when in GHC's pipeline the ``INLINE``
pragma is switched on. Inlining happens only during runs of the
*simplifier*. Each run of the simplifier has a different *phase number*;
the phase number decreases towards zero. If you use
``-dverbose-core2core`` you'll see the sequence of phase numbers for
successive runs of the simplifier. In an ``INLINE`` pragma you can
optionally specify a phase number, thus:

-  "``INLINE[k] f``" means: do not inline ``f`` until phase ``k``, but
   from phase ``k`` onwards be very keen to inline it.

-  "``INLINE[~k] f``" means: be very keen to inline ``f`` until phase
   ``k``, but from phase ``k`` onwards do not inline it.

-  "``NOINLINE[k] f``" means: do not inline ``f`` until phase ``k``, but
   from phase ``k`` onwards be willing to inline it (as if there was no
   pragma).

-  "``NOINLINE[~k] f``" means: be willing to inline ``f`` until phase
   ``k``, but from phase ``k`` onwards do not inline it.

The same information is summarised here:

::

                               -- Before phase 2     Phase 2 and later
      {-# INLINE   [2]  f #-}  --      No                 Yes
      {-# INLINE   [~2] f #-}  --      Yes                No
      {-# NOINLINE [2]  f #-}  --      No                 Maybe
      {-# NOINLINE [~2] f #-}  --      Maybe              No

      {-# INLINE   f #-}       --      Yes                Yes
      {-# NOINLINE f #-}       --      No                 No

By "Maybe" we mean that the usual heuristic inlining rules apply (if the
function body is small, or it is applied to interesting-looking
arguments etc). Another way to understand the semantics is this:

-  For both ``INLINE`` and ``NOINLINE``, the phase number says when inlining is
   allowed at all.

-  The ``INLINE`` pragma has the additional effect of making the function
   body look small, so that when inlining is allowed it is very likely
   to happen.

The same phase-numbering control is available for RULES
(:ref:`rewrite-rules`).

.. _line-pragma:

``LINE`` pragma
---------------

.. index::
   single: LINE; pragma
   single: pragma; LINE

This pragma is similar to C's ``#line`` pragma, and is mainly for use in
automatically generated Haskell code. It lets you specify the line
number and filename of the original code; for example

::

    {-# LINE 42 "Foo.vhs" #-}

if you'd generated the current file from something called ``Foo.vhs``
and this line corresponds to line 42 in the original. GHC will adjust
its error messages to refer to the line/file named in the ``LINE``
pragma.

``LINE`` pragmas generated from Template Haskell set the file and line
position for the duration of the splice and are limited to the splice.
Note that because Template Haskell splices abstract syntax, the file
positions are not automatically advanced.

.. _column-pragma:

``COLUMN`` pragma
-----------------

.. index::
   single: COLUMN; pragma
   single: pragma; COLUMN

This is the analogue of the ``LINE`` pragma and is likewise intended for
use in automatically generated Haskell code. It lets you specify the
column number of the original code; for example

::

    foo = do
      {-# COLUMN 42 #-}pure ()
      pure ()

This adjusts all column numbers immediately after the pragma to start
at 42.  The presence of this pragma only affects the quality of the
diagnostics and does not change the syntax of the code itself.

.. _rules:

``RULES`` pragma
----------------

The ``RULES`` pragma lets you specify rewrite rules. It is described in
:ref:`rewrite-rules`.

.. _specialize-pragma:

``SPECIALIZE`` pragma
---------------------

.. index::
   single: SPECIALIZE pragma
   single: pragma, SPECIALIZE
   single: overloading, death to

(UK spelling also accepted.) For key overloaded functions, you can
create extra versions (NB: more code space) specialised to particular
types. Thus, if you have an overloaded function:

::

      hammeredLookup :: Ord key => [(key, value)] -> key -> value

If it is heavily used on lists with ``Widget`` keys, you could
specialise it as follows:

::

      {-# SPECIALIZE hammeredLookup :: [(Widget, value)] -> Widget -> value #-}

-  A ``SPECIALIZE`` pragma for a function can be put anywhere its type
   signature could be put. Moreover, you can also ``SPECIALIZE`` an
   *imported* function provided it was given an ``INLINABLE`` pragma at
   its definition site (:ref:`inlinable-pragma`).

-  A ``SPECIALIZE`` has the effect of generating (a) a specialised
   version of the function and (b) a rewrite rule (see
   :ref:`rewrite-rules`) that rewrites a call to the un-specialised
   function into a call to the specialised one. Moreover, given a
   ``SPECIALIZE`` pragma for a function ``f``, GHC will automatically
   create specialisations for any type-class-overloaded functions called
   by ``f``, if they are in the same module as the ``SPECIALIZE``
   pragma, or if they are ``INLINABLE``; and so on, transitively.

-  You can add phase control (:ref:`phase-control`) to the RULE
   generated by a ``SPECIALIZE`` pragma, just as you can if you write a
   ``RULE`` directly. For example:

   ::

         {-# SPECIALIZE [0] hammeredLookup :: [(Widget, value)] -> Widget -> value #-}

   generates a specialisation rule that only fires in Phase 0 (the final
   phase). If you do not specify any phase control in the ``SPECIALIZE``
   pragma, the phase control is inherited from the inline pragma (if
   any) of the function. For example:

   ::

         foo :: Num a => a -> a
         foo = ...blah...
         {-# NOINLINE [0] foo #-}
         {-# SPECIALIZE foo :: Int -> Int #-}

   The ``NOINLINE`` pragma tells GHC not to inline ``foo`` until Phase
   0; and this property is inherited by the specialisation RULE, which
   will therefore only fire in Phase 0.

   The main reason for using phase control on specialisations is so that
   you can write optimisation RULES that fire early in the compilation
   pipeline, and only *then* specialise the calls to the function. If
   specialisation is done too early, the optimisation rules might fail
   to fire.

-  The type in a ``SPECIALIZE`` pragma can be any type that is less
   polymorphic than the type of the original function. In concrete
   terms, if the original function is ``f`` then the pragma

   ::

         {-# SPECIALIZE f :: <type> #-}

   is valid if and only if the definition

   ::

         f_spec :: <type>
         f_spec = f

   is valid. Here are some examples (where we only give the type
   signature for the original function, not its code):

   ::

         f :: Eq a => a -> b -> b
         {-# SPECIALISE f :: Int -> b -> b #-}

         g :: (Eq a, Ix b) => a -> b -> b
         {-# SPECIALISE g :: (Eq a) => a -> Int -> Int #-}

         h :: Eq a => a -> a -> a
         {-# SPECIALISE h :: (Eq a) => [a] -> [a] -> [a] #-}

   The last of these examples will generate a RULE with a
   somewhat-complex left-hand side (try it yourself), so it might not
   fire very well. If you use this kind of specialisation, let us know
   how well it works.

.. _specialize-inline:

``SPECIALIZE INLINE``
~~~~~~~~~~~~~~~~~~~~~

A ``SPECIALIZE`` pragma can optionally be followed with a ``INLINE`` or
``NOINLINE`` pragma, optionally followed by a phase, as described in
:ref:`inline-noinline-pragma`. The ``INLINE`` pragma affects the
specialised version of the function (only), and applies even if the
function is recursive. The motivating example is this: ::

    -- A GADT for arrays with type-indexed representation
    data Arr e where
      ArrInt :: !Int -> ByteArray# -> Arr Int
      ArrPair :: !Int -> Arr e1 -> Arr e2 -> Arr (e1, e2)

    (!:) :: Arr e -> Int -> e
    {-# SPECIALISE INLINE (!:) :: Arr Int -> Int -> Int #-}
    {-# SPECIALISE INLINE (!:) :: Arr (a, b) -> Int -> (a, b) #-}
    (ArrInt _ ba)     !: (I# i) = I# (indexIntArray# ba i)
    (ArrPair _ a1 a2) !: i      = (a1 !: i, a2 !: i)

Here, ``(!:)`` is a recursive function that indexes arrays of type
``Arr e``. Consider a call to ``(!:)`` at type ``(Int,Int)``. The second
specialisation will fire, and the specialised function will be inlined.
It has two calls to ``(!:)``, both at type ``Int``. Both these calls
fire the first specialisation, whose body is also inlined. The result is
a type-based unrolling of the indexing function.

You can add explicit phase control (:ref:`phase-control`) to
``SPECIALISE INLINE`` pragma, just like on an ``INLINE`` pragma; if you
do so, the same phase is used for the rewrite rule and the INLINE
control of the specialised function.

.. warning:: You can make GHC diverge by using ``SPECIALISE INLINE`` on an
             ordinarily-recursive function.

``SPECIALIZE`` for imported functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generally, you can only give a ``SPECIALIZE`` pragma for a function
defined in the same module. However if a function ``f`` is given an
``INLINABLE`` pragma at its definition site, then it can subsequently be
specialised by importing modules (see :ref:`inlinable-pragma`). For example ::

    module Map( lookup, blah blah ) where
      lookup :: Ord key => [(key,a)] -> key -> Maybe a
      lookup = ...
      {-# INLINABLE lookup #-}

    module Client where
      import Map( lookup )

      data T = T1 | T2 deriving( Eq, Ord )
      {-# SPECIALISE lookup :: [(T,a)] -> T -> Maybe a

Here, ``lookup`` is declared ``INLINABLE``, but it cannot be specialised
for type ``T`` at its definition site, because that type does not exist
yet. Instead a client module can define ``T`` and then specialise
``lookup`` at that type.

Moreover, every module that imports ``Client`` (or imports a module that
imports ``Client``, transitively) will "see", and make use of, the
specialised version of ``lookup``. You don't need to put a
``SPECIALIZE`` pragma in every module.

Moreover you often don't even need the ``SPECIALIZE`` pragma in the
first place. When compiling a module ``M``, GHC's optimiser (when given the
:ghc-flag:`-O` flag) automatically considers each top-level overloaded function declared
in ``M``, and specialises it for the different types at which it is called in
``M``. The optimiser *also* considers each *imported* ``INLINABLE``
overloaded function, and specialises it for the different types at which
it is called in ``M``. So in our example, it would be enough for ``lookup``
to be called at type ``T``:

::

    module Client where
      import Map( lookup )

      data T = T1 | T2 deriving( Eq, Ord )

      findT1 :: [(T,a)] -> Maybe a
      findT1 m = lookup m T1   -- A call of lookup at type T

However, sometimes there are no such calls, in which case the pragma can
be useful.

Obsolete ``SPECIALIZE`` syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In earlier versions of GHC, it was possible to provide your own
specialised function for a given type:

::

    {-# SPECIALIZE hammeredLookup :: [(Int, value)] -> Int -> value = intLookup #-}

This feature has been removed, as it is now subsumed by the ``RULES``
pragma (see :ref:`rule-spec`).

.. _specialize-instance-pragma:

``SPECIALIZE`` instance pragma
------------------------------

.. index::
   single: SPECIALIZE pragma
   single: overloading, death to

Same idea, except for instance declarations. For example:

::

    instance (Eq a) => Eq (Foo a) where {
       {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
       ... usual stuff ...
     }

The pragma must occur inside the ``where`` part of the instance
declaration.

.. _unpack-pragma:

``UNPACK`` pragma
-----------------

.. index::
   single: UNPACK

The ``UNPACK`` indicates to the compiler that it should unpack the
contents of a constructor field into the constructor itself, removing a
level of indirection. For example: ::

    data T = T {-# UNPACK #-} !Float
               {-# UNPACK #-} !Float

will create a constructor ``T`` containing two unboxed floats. This may
not always be an optimisation: if the ``T`` constructor is scrutinised
and the floats passed to a non-strict function for example, they will
have to be reboxed (this is done automatically by the compiler).

Unpacking constructor fields should only be used in conjunction with
:ghc-flag:`-O` [1]_, in order to expose unfoldings to the compiler so the
reboxing can be removed as often as possible. For example: ::

    f :: T -> Float
    f (T f1 f2) = f1 + f2

The compiler will avoid reboxing ``f1`` and ``f2`` by inlining ``+`` on
floats, but only when :ghc-flag:`-O` is on.

Any single-constructor data is eligible for unpacking; for example ::

    data T = T {-# UNPACK #-} !(Int,Int)

will store the two ``Int``\ s directly in the ``T`` constructor, by
flattening the pair. Multi-level unpacking is also supported: ::

    data T = T {-# UNPACK #-} !S
    data S = S {-# UNPACK #-} !Int {-# UNPACK #-} !Int

will store two unboxed ``Int#``\ s directly in the ``T`` constructor.
The unpacker can see through newtypes, too.

See also the :ghc-flag:`-funbox-strict-fields` flag, which essentially has the
effect of adding ``{-# UNPACK #-}`` to every strict constructor field.

.. [1]
   in fact, UNPACK has no effect without
   -O
   , for technical reasons (see
   tick 5252
   )

.. _nounpack-pragma:

``NOUNPACK`` pragma
-------------------

.. index::
   single: NOUNPACK

The ``NOUNPACK`` pragma indicates to the compiler that it should not
unpack the contents of a constructor field. Example: ::

    data T = T {-# NOUNPACK #-} !(Int,Int)

Even with the flags :ghc-flag:`-funbox-strict-fields` and :ghc-flag:`-O`, the
field of the constructor ``T`` is not unpacked.

.. _source-pragma:

``SOURCE`` pragma
-----------------

.. index::
   single: SOURCE
   single: pragma; SOURCE

The ``{-# SOURCE #-}`` pragma is used only in ``import`` declarations,
to break a module loop. It is described in detail in
:ref:`mutual-recursion`.

.. _complete-pragma:

``COMPLETE`` pragmas
--------------------

The ``COMPLETE`` pragma is used to inform the pattern match checker that a
certain set of patterns is complete and that any function which matches
on all the specified patterns is total.

The most common usage of ``COMPLETE`` pragmas is with
:ref:`pattern-synonyms`.
On its own, the checker is very naive and assumes that any match involving
a pattern synonym will fail. As a result, any pattern match on a
pattern synonym is regarded as
incomplete unless the user adds a catch-all case.

For example, the data types ``2 * A`` and ``A + A`` are isomorphic but some
computations are more naturally expressed in terms of one or the other. To
get the best of both worlds, we can choose one as our implementation and then
provide a set of pattern synonyms so that users can use the other representation
if they desire. We can then specify a ``COMPLETE`` pragma in order to
inform the pattern match checker that a function which matches on both ``LeftChoice``
and ``RightChoice`` is total. ::

    data Choice a = Choice Bool a

    pattern LeftChoice :: a -> Choice a
    pattern LeftChoice a = Choice False a

    pattern RightChoice :: a -> Choice a
    pattern RightChoice a = Choice True a

    {-# COMPLETE LeftChoice, RightChoice #-}

    foo :: Choice Int -> Int
    foo (LeftChoice n) = n * 2
    foo (RightChoice n) = n - 2

``COMPLETE`` pragmas are only used by the pattern match checker. If a function
definition matches on all the constructors specified in the pragma then the
compiler will produce no warning.

``COMPLETE`` pragmas can contain any data constructors or pattern
synonyms which are in scope, but must mention at least one data
constructor or pattern synonym defined in the same module.
``COMPLETE`` pragmas may only appear at the top level of a module.
Once defined, they are automatically imported and exported from
modules. ``COMPLETE`` pragmas should be thought of as asserting a
universal truth about a set of patterns and as a result, should not be
used to silence context specific incomplete match warnings.

When specifing a ``COMPLETE`` pragma, the result types of all patterns must
be consistent with each other. This is a sanity check as it would be impossible
to match on all the patterns if the types were inconsistent.

The result type must also be unambiguous. Usually this can be inferred but
when all the pattern synonyms in a group are polymorphic in the constructor
the user must provide a type signature. ::

    class LL f where
      go :: f a -> ()

    instance LL [] where
      go _ = ()

    pattern T :: LL f => f a
    pattern T <- (go -> ())

    {-# COMPLETE T :: [] #-}

    -- No warning
    foo :: [a] -> Int
    foo T = 5

.. _overlap-pragma:

``OVERLAPPING``, ``OVERLAPPABLE``, ``OVERLAPS``, and ``INCOHERENT`` pragmas
---------------------------------------------------------------------------

.. index::
   single: OVERLAPPING
   single: pragma; OVERLAPPING
   single: OVERLAPPABLE
   single: pragma; OVERLAPPABLE
   single: OVERLAPS
   single: pragma; OVERLAPS
   single: INCOHERENT
   single: pragma; INCOHERENT

The pragmas ``OVERLAPPING``, ``OVERLAPPABLE``, ``OVERLAPS``,
``INCOHERENT`` are used to specify the overlap behavior for individual
instances, as described in Section :ref:`instance-overlap`. The pragmas
are written immediately after the ``instance`` keyword, like this:

::

    instance {-# OVERLAPPING #-} C t where ...

.. _rewrite-rules:

Rewrite rules
=============

.. index::
   single: RULES pragma
   single: pragma; RULES
   single: rewrite rules

The programmer can specify rewrite rules as part of the source program
(in a pragma). Here is an example: ::

      {-# RULES
      "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs
        #-}

Use the debug flag :ghc-flag:`-ddump-simpl-stats` to see what rules fired. If
you need more information, then :ghc-flag:`-ddump-rule-firings` shows you each
individual rule firing and :ghc-flag:`-ddump-rule-rewrites` also shows what the
code looks like before and after the rewrite.

.. ghc-flag:: -fenable-rewrite-rules

    Allow the compiler to apply rewrite rules to the source program.

Syntax
------

From a syntactic point of view:

-  There may be zero or more rules in a ``RULES`` pragma, separated by
   semicolons (which may be generated by the layout rule).

-  The layout rule applies in a pragma. Currently no new indentation
   level is set, so if you put several rules in single ``RULES`` pragma and
   wish to use layout to separate them, you must lay out the starting in
   the same column as the enclosing definitions. ::

         {-# RULES
         "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs
         "map/append" forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys
           #-}

   Furthermore, the closing ``#-}`` should start in a column to the
   right of the opening ``{-#``.

-  Each rule has a name, enclosed in double quotes. The name itself has
   no significance at all. It is only used when reporting how many times
   the rule fired.

-  A rule may optionally have a phase-control number (see
   :ref:`phase-control`), immediately after the name of the rule. Thus: ::

         {-# RULES
               "map/map" [2]  forall f g xs. map f (map g xs) = map (f.g) xs
           #-}

   The ``[2]`` means that the rule is active in Phase 2 and subsequent
   phases. The inverse notation ``[~2]`` is also accepted, meaning that
   the rule is active up to, but not including, Phase 2.

   Rules support the special phase-control notation ``[~]``, which means
   the rule is never active. This feature supports plugins (see
   :ref:`compiler-plugins`), by making it possible to define a RULE that
   is never run by GHC, but is nevertheless parsed, typechecked etc, so
   that it is available to the plugin.

-  Each variable mentioned in a rule must either be in scope (e.g.
   ``map``), or bound by the ``forall`` (e.g. ``f``, ``g``, ``xs``). The
   variables bound by the ``forall`` are called the *pattern* variables.
   They are separated by spaces, just like in a type ``forall``.

-  A pattern variable may optionally have a type signature. If the type
   of the pattern variable is polymorphic, it *must* have a type
   signature. For example, here is the ``foldr/build`` rule: ::

       "fold/build"  forall k z (g::forall b. (a->b->b) -> b -> b) .
                     foldr k z (build g) = g k z

   Since ``g`` has a polymorphic type, it must have a type signature.

-  The left hand side of a rule must consist of a top-level variable
   applied to arbitrary expressions. For example, this is *not* OK: ::

       "wrong1"   forall e1 e2.  case True of { True -> e1; False -> e2 } = e1
       "wrong2"   forall f.      f True = True
       "wrong3"   forall x.      Just x = Nothing

   In ``"wrong1"``, the LHS is not an application; in ``"wrong2"``, the
   LHS has a pattern variable in the head. In ``"wrong3"``, the LHS consists
   of a *constructor*, rather than a *variable*, applied to an argument.

-  A rule does not need to be in the same module as (any of) the
   variables it mentions, though of course they need to be in scope.

-  All rules are implicitly exported from the module, and are therefore
   in force in any module that imports the module that defined the rule,
   directly or indirectly. (That is, if A imports B, which imports C,
   then C's rules are in force when compiling A.) The situation is very
   similar to that for instance declarations.

-  Inside a RULE "``forall``" is treated as a keyword, regardless of any
   other flag settings. Furthermore, inside a RULE, the language
   extension :ghc-flag:`-XScopedTypeVariables` is automatically enabled; see
   :ref:`scoped-type-variables`.

-  Like other pragmas, ``RULE`` pragmas are always checked for scope errors,
   and are typechecked. Typechecking means that the LHS and RHS of a
   rule are typechecked, and must have the same type. However, rules are
   only *enabled* if the :ghc-flag:`-fenable-rewrite-rules` flag is on (see
   :ref:`rule-semantics`).

.. _rule-semantics:

Semantics
---------

From a semantic point of view:

-  Rules are enabled (that is, used during optimisation) by the
   :ghc-flag:`-fenable-rewrite-rules` flag. This flag is implied by
   :ghc-flag:`-O`, and may be switched off (as usual) by
   :ghc-flag:`-fno-enable-rewrite-rules <-fenable-rewrite-rules>`. (NB: enabling
   :ghc-flag:`-fenable-rewrite-rules` without :ghc-flag:`-O` may not do what you
   expect, though, because without :ghc-flag:`-O` GHC ignores all optimisation
   information in interface files; see :ghc-flag:`-fignore-interface-pragmas`).
   Note that :ghc-flag:`-fenable-rewrite-rules` is an
   *optimisation* flag, and has no effect on parsing or typechecking.

-  Rules are regarded as left-to-right rewrite rules. When GHC finds an
   expression that is a substitution instance of the LHS of a rule, it
   replaces the expression by the (appropriately-substituted) RHS. By "a
   substitution instance" we mean that the LHS can be made equal to the
   expression by substituting for the pattern variables.

-  GHC makes absolutely no attempt to verify that the LHS and RHS of a
   rule have the same meaning. That is undecidable in general, and
   infeasible in most interesting cases. The responsibility is entirely
   the programmer's!

-  GHC makes no attempt to make sure that the rules are confluent or
   terminating. For example: ::

         "loop"        forall x y.  f x y = f y x

   This rule will cause the compiler to go into an infinite loop.

-  If more than one rule matches a call, GHC will choose one arbitrarily
   to apply.

-  GHC currently uses a very simple, syntactic, matching algorithm for
   matching a rule LHS with an expression. It seeks a substitution which
   makes the LHS and expression syntactically equal modulo alpha
   conversion. The pattern (rule), but not the expression, is
   eta-expanded if necessary. (Eta-expanding the expression can lead to
   laziness bugs.) But not beta conversion (that's called higher-order
   matching).

   Matching is carried out on GHC's intermediate language, which
   includes type abstractions and applications. So a rule only matches
   if the types match too. See :ref:`rule-spec` below.

-  GHC keeps trying to apply the rules as it optimises the program. For
   example, consider: ::

         let s = map f
             t = map g
         in
         s (t xs)

   The expression ``s (t xs)`` does not match the rule ``"map/map"``,
   but GHC will substitute for ``s`` and ``t``, giving an expression
   which does match. If ``s`` or ``t`` was (a) used more than once, and
   (b) large or a redex, then it would not be substituted, and the rule
   would not fire.

.. _rules-inline:

How rules interact with ``INLINE``/``NOINLINE`` pragmas
-------------------------------------------------------

Ordinary inlining happens at the same time as rule rewriting, which may
lead to unexpected results. Consider this (artificial) example ::

    f x = x
    g y = f y
    h z = g True

    {-# RULES "f" f True = False #-}

Since ``f``\'s right-hand side is small, it is inlined into ``g``, to
give ::

    g y = y

Now ``g`` is inlined into ``h``, but ``f``\'s RULE has no chance to fire.
If instead GHC had first inlined ``g`` into ``h`` then there would have
been a better chance that ``f``\'s RULE might fire.

The way to get predictable behaviour is to use a ``NOINLINE`` pragma, or an
``INLINE[⟨phase⟩]`` pragma, on ``f``, to ensure that it is not inlined until
its RULEs have had a chance to fire. The warning flag
:ghc-flag:`-Winline-rule-shadowing` (see :ref:`options-sanity`) warns about
this situation.

.. _conlike:

How rules interact with ``CONLIKE`` pragmas
-------------------------------------------

GHC is very cautious about duplicating work. For example, consider ::

    f k z xs = let xs = build g
               in ...(foldr k z xs)...sum xs...
    {-# RULES "foldr/build" forall k z g. foldr k z (build g) = g k z #-}

Since ``xs`` is used twice, GHC does not fire the foldr/build rule.
Rightly so, because it might take a lot of work to compute ``xs``, which
would be duplicated if the rule fired.

Sometimes, however, this approach is over-cautious, and we *do* want the
rule to fire, even though doing so would duplicate redex. There is no
way that GHC can work out when this is a good idea, so we provide the
``CONLIKE`` pragma to declare it, thus: ::

    {-# INLINE CONLIKE [1] f #-}
    f x = blah

``CONLIKE`` is a modifier to an ``INLINE`` or ``NOINLINE`` pragma. It specifies that
an application of ``f`` to one argument (in general, the number of arguments
to the left of the ``=`` sign) should be considered cheap enough to
duplicate, if such a duplication would make rule fire. (The name
"CONLIKE" is short for "constructor-like", because constructors
certainly have such a property.) The ``CONLIKE`` pragma is a modifier to
INLINE/NOINLINE because it really only makes sense to match ``f`` on the
LHS of a rule if you are sure that ``f`` is not going to be inlined
before the rule has a chance to fire.

.. _rules-class-methods:

How rules interact with class methods
-------------------------------------

Giving a RULE for a class method is a bad idea: ::

    class C a where
      op :: a -> a -> a

    instance C Bool where
      op x y = ...rhs for op at Bool...

    {-# RULES "f" op True y = False #-}

In this example, ``op`` is not an ordinary top-level function; it is a
class method. GHC rapidly rewrites any occurrences of
``op``\-used-at-type-Bool to a specialised function, say ``opBool``,
where ::

    opBool :: Bool -> Bool -> Bool
    opBool x y = ..rhs for op at Bool...

So the RULE never has a chance to fire, for just the same reasons as in
:ref:`rules-inline`.

The solution is to define the instance-specific function yourself, with
a pragma to prevent it being inlined too early, and give a RULE for it: ::

    instance C Bool where
      op x y = opBool

    opBool :: Bool -> Bool -> Bool
    {-# NOINLINE [1] opBool #-}
    opBool x y = ..rhs for op at Bool...

    {-# RULES "f" opBool True y = False #-}

If you want a RULE that truly applies to the overloaded class method,
the only way to do it is like this: ::

    class C a where
      op_c :: a -> a -> a

    op :: C a => a -> a -> a
    {-# NOINLINE [1] op #-}
    op = op_c

    {-# RULES "reassociate" op (op x y) z = op x (op y z) #-}

Now the inlining of ``op`` is delayed until the rule has a chance to
fire. The down-side is that instance declarations must define ``op_c``,
but all other uses should go via ``op``.

List fusion
-----------

The RULES mechanism is used to implement fusion (deforestation) of
common list functions. If a "good consumer" consumes an intermediate
list constructed by a "good producer", the intermediate list should be
eliminated entirely.

The following are good producers:

-  List comprehensions

-  Enumerations of ``Int``, ``Integer`` and ``Char`` (e.g.
   ``['a'..'z']``).

-  Explicit lists (e.g. ``[True, False]``)

-  The cons constructor (e.g ``3:4:[]``)

-  ``++``

-  ``map``

-  ``take``, ``filter``

-  ``iterate``, ``repeat``

-  ``zip``, ``zipWith``

The following are good consumers:

-  List comprehensions

-  ``array`` (on its second argument)

-  ``++`` (on its first argument)

-  ``foldr``

-  ``map``

-  ``take``, ``filter``

-  ``concat``

-  ``unzip``, ``unzip2``, ``unzip3``, ``unzip4``

-  ``zip``, ``zipWith`` (but on one argument only; if both are good
   producers, ``zip`` will fuse with one but not the other)

-  ``partition``

-  ``head``

-  ``and``, ``or``, ``any``, ``all``

-  ``sequence_``

-  ``msum``

So, for example, the following should generate no intermediate lists: ::

    array (1,10) [(i,i*i) | i <- map (+ 1) [0..9]]

This list could readily be extended; if there are Prelude functions that
you use a lot which are not included, please tell us.

If you want to write your own good consumers or producers, look at the
Prelude definitions of the above functions to see how to do so.

.. _rule-spec:

Specialisation
--------------

Rewrite rules can be used to get the same effect as a feature present in
earlier versions of GHC. For example, suppose that: ::

    genericLookup :: Ord a => Table a b   -> a   -> b
    intLookup     ::          Table Int b -> Int -> b

where ``intLookup`` is an implementation of ``genericLookup`` that works
very fast for keys of type ``Int``. You might wish to tell GHC to use
``intLookup`` instead of ``genericLookup`` whenever the latter was
called with type ``Table Int b -> Int -> b``. It used to be possible to
write ::

    {-# SPECIALIZE genericLookup :: Table Int b -> Int -> b = intLookup #-}

This feature is no longer in GHC, but rewrite rules let you do the same
thing: ::

    {-# RULES "genericLookup/Int" genericLookup = intLookup #-}

This slightly odd-looking rule instructs GHC to replace
``genericLookup`` by ``intLookup`` *whenever the types match*. What is
more, this rule does not need to be in the same file as
``genericLookup``, unlike the ``SPECIALIZE`` pragmas which currently do
(so that they have an original definition available to specialise).

It is *Your Responsibility* to make sure that ``intLookup`` really
behaves as a specialised version of ``genericLookup``!!!

An example in which using ``RULES`` for specialisation will Win Big: ::

    toDouble :: Real a => a -> Double
    toDouble = fromRational . toRational

    {-# RULES "toDouble/Int" toDouble = i2d #-}
    i2d (I# i) = D# (int2Double# i) -- uses Glasgow prim-op directly

The ``i2d`` function is virtually one machine instruction; the default
conversion—via an intermediate ``Rational``\-is obscenely expensive by
comparison.

.. _controlling-rules:

Controlling what's going on in rewrite rules
--------------------------------------------

-  Use :ghc-flag:`-ddump-rules` to see the rules that are defined *in this
   module*. This includes rules generated by the specialisation pass,
   but excludes rules imported from other modules.

-  Use :ghc-flag:`-ddump-simpl-stats` to see what rules are being fired. If you
   add :ghc-flag:`-dppr-debug` you get a more detailed listing.

-  Use :ghc-flag:`-ddump-rule-firings` or :ghc-flag:`-ddump-rule-rewrites` to see in
   great detail what rules are being fired. If you add :ghc-flag:`-dppr-debug`
   you get a still more detailed listing.

-  The definition of (say) ``build`` in ``GHC/Base.hs`` looks like
   this: ::

               build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
               {-# INLINE build #-}
               build g = g (:) []

   Notice the ``INLINE``! That prevents ``(:)`` from being inlined when
   compiling ``PrelBase``, so that an importing module will “see” the
   ``(:)``, and can match it on the LHS of a rule. ``INLINE`` prevents
   any inlining happening in the RHS of the ``INLINE`` thing. I regret
   the delicacy of this.

-  In ``libraries/base/GHC/Base.hs`` look at the rules for ``map`` to
   see how to write rules that will do fusion and yet give an efficient
   program even if fusion doesn't happen. More rules in
   ``GHC/List.hs``.

.. _special-ids:

Special built-in functions
==========================

GHC has a few built-in functions with special behaviour. In particular:

-  :base-ref:`GHC.Exts.inline` allows control over inlining on a per-call-site basis.

-  :base-ref:`GHC.Exts.lazy` restrains the strictness analyser.

-  :base-ref:`GHC.Exts.oneShot` gives a hint to the compiler about how often a
   function is being called.

.. _generic-classes:

Generic classes
===============

GHC used to have an implementation of generic classes as defined in the
paper "Derivable type classes", Ralf Hinze and Simon Peyton Jones,
Haskell Workshop, Montreal Sept 2000, pp. 94-105. These have been removed
and replaced by the more general `support for generic
programming <#generic-programming>`__.

.. _generic-programming:

Generic programming
===================

Using a combination of :ghc-flag:`-XDeriveGeneric`,
:ghc-flag:`-XDefaultSignatures`, and :ghc-flag:`-XDeriveAnyClass`, you can
easily do datatype-generic programming using the :base-ref:`GHC.Generics.`
framework. This section gives a very brief overview of how to do it.

Generic programming support in GHC allows defining classes with methods
that do not need a user specification when instantiating: the method
body is automatically derived by GHC. This is similar to what happens
for standard classes such as ``Read`` and ``Show``, for instance, but
now for user-defined classes.

Deriving representations
------------------------

The first thing we need is generic representations. The ``GHC.Generics``
module defines a couple of primitive types that are used to represent
Haskell datatypes: ::

    -- | Unit: used for constructors without arguments
    data U1 p = U1

    -- | Constants, additional parameters and recursion of kind *
    newtype K1 i c p = K1 { unK1 :: c }

    -- | Meta-information (constructor names, etc.)
    newtype M1 i c f p = M1 { unM1 :: f p }

    -- | Sums: encode choice between constructors
    infixr 5 :+:
    data (:+:) f g p = L1 (f p) | R1 (g p)

    -- | Products: encode multiple arguments to constructors
    infixr 6 :*:
    data (:*:) f g p = f p :*: g p

The ``Generic`` and ``Generic1`` classes mediate between user-defined
datatypes and their internal representation as a sum-of-products: ::

    class Generic a where
      -- Encode the representation of a user datatype
      type Rep a :: * -> *
      -- Convert from the datatype to its representation
      from  :: a -> (Rep a) x
      -- Convert from the representation to the datatype
      to    :: (Rep a) x -> a

    class Generic1 (f :: k -> *) where
      type Rep1 f :: k -> *

      from1  :: f a -> Rep1 f a
      to1    :: Rep1 f a -> f a

``Generic1`` is used for functions that can only be defined over type
containers, such as ``map``. Note that ``Generic1`` ranges over types of kind
``* -> *`` by default, but if the :ghc-flag:`-XPolyKinds` extension is enabled,
then it can range of types of kind ``k -> *``, for any kind ``k``.

Instances of these classes can be derived by GHC with the
:ghc-flag:`-XDeriveGeneric` extension, and are necessary to be able to define
generic instances automatically.

For example, a user-defined datatype of trees ::

    data UserTree a = Node a (UserTree a) (UserTree a) | Leaf

in a ``Main`` module in a package named ``foo`` will get the following
representation: ::

    instance Generic (UserTree a) where
      -- Representation type
      type Rep (UserTree a) =
        M1 D ('MetaData "UserTree" "Main" "package-name" 'False) (
              M1 C ('MetaCons "Node" 'PrefixI 'False) (
                    M1 S ('MetaSel 'Nothing
                                   'NoSourceUnpackedness
                                   'NoSourceStrictness
                                   'DecidedLazy)
                         (K1 R a)
                :*: M1 S ('MetaSel 'Nothing
                                   'NoSourceUnpackedness
                                   'NoSourceStrictness
                                   'DecidedLazy)
                         (K1 R (UserTree a))
                :*: M1 S ('MetaSel 'Nothing
                                   'NoSourceUnpackedness
                                   'NoSourceStrictness
                                   'DecidedLazy)
                         (K1 R (UserTree a)))
          :+: M1 C ('MetaCons "Leaf" 'PrefixI 'False) U1)

      -- Conversion functions
      from (Node x l r) = M1 (L1 (M1 (M1 (K1 x) :*: M1 (K1 l) :*: M1 (K1 r))))
      from Leaf         = M1 (R1 (M1 U1))
      to (M1 (L1 (M1 (M1 (K1 x) :*: M1 (K1 l) :*: M1 (K1 r))))) = Node x l r
      to (M1 (R1 (M1 U1)))                                      = Leaf

This representation is generated automatically if a ``deriving Generic``
clause is attached to the datatype. `Standalone
deriving <#stand-alone-deriving>`__ can also be used.

Writing generic functions
-------------------------

A generic function is defined by creating a class and giving instances
for each of the representation types of ``GHC.Generics``. As an example
we show generic serialization: ::

    data Bin = O | I

    class GSerialize f where
      gput :: f a -> [Bin]

    instance GSerialize U1 where
      gput U1 = []

    instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
      gput (x :*: y) = gput x ++ gput y

    instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
      gput (L1 x) = O : gput x
      gput (R1 x) = I : gput x

    instance (GSerialize a) => GSerialize (M1 i c a) where
      gput (M1 x) = gput x

    instance (Serialize a) => GSerialize (K1 i a) where
      gput (K1 x) = put x

A caveat: this encoding strategy may not be reliable across different versions
of GHC. When deriving a ``Generic`` instance is free to choose any nesting of
``:+:`` and ``:*:`` it chooses, so if GHC chooses ``(a :+: b) :+: c``, then the
encoding for ``a`` would be ``[O, O]``, ``b`` would be ``[O, I]``, and ``c``
would be ``[I]``. However, if GHC chooses ``a :+: (b :+: c)``, then the
encoding for ``a`` would be ``[O]``, ``b`` would be ``[I, O]``, and ``c`` would
be ``[I, I]``. (In practice, the current implementation tries to produce a
more-or-less balanced nesting of ``:+:`` and ``:*:`` so that the traversal of
the structure of the datatype from the root to a particular component can be
performed in logarithmic rather than linear time.)

Typically this ``GSerialize`` class will not be exported, as it only makes
sense to have instances for the representation types.

Unlifted representation types
-----------------------------

The data family ``URec`` is provided to enable generic programming over
datatypes with certain unlifted arguments. There are six instances corresponding
to common unlifted types: ::

    data family URec a p

    data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#   }
    data instance URec Char     p = UChar   { uChar#   :: Char#   }
    data instance URec Double   p = UDouble { uDouble# :: Double# }
    data instance URec Int      p = UInt    { uInt#    :: Int#    }
    data instance URec Float    p = UFloat  { uFloat#  :: Float#  }
    data instance URec Word     p = UWord   { uWord#   :: Word#   }

Six type synonyms are provided for convenience: ::

    type UAddr   = URec (Ptr ())
    type UChar   = URec Char
    type UDouble = URec Double
    type UFloat  = URec Float
    type UInt    = URec Int
    type UWord   = URec Word

As an example, this data declaration: ::

    data IntHash = IntHash Int#
      deriving Generic

results in the following ``Generic`` instance: ::

    instance 'Generic' IntHash where
      type 'Rep' IntHash =
        'D1' ('MetaData "IntHash" "Main" "package-name" 'False)
          ('C1' ('MetaCons "IntHash" 'PrefixI 'False)
            ('S1' ('MetaSel 'Nothing
                            'NoSourceUnpackedness
                            'NoSourceStrictness
                            'DecidedLazy)
                  'UInt'))

A user could provide, for example, a ``GSerialize UInt`` instance so that a
``Serialize IntHash`` instance could be easily defined in terms of
``GSerialize``.

Generic defaults
----------------

The only thing left to do now is to define a "front-end" class, which is
exposed to the user: ::

    class Serialize a where
      put :: a -> [Bin]

      default put :: (Generic a, GSerialize (Rep a)) => a -> [Bin]
      put = gput . from

Here we use a `default signature <#class-default-signatures>`__ to
specify that the user does not have to provide an implementation for
``put``, as long as there is a ``Generic`` instance for the type to
instantiate. For the ``UserTree`` type, for instance, the user can just
write: ::

    instance (Serialize a) => Serialize (UserTree a)

The default method for ``put`` is then used, corresponding to the
generic implementation of serialization. If you are using
:ghc-flag:`-XDeriveAnyClass`, the same instance is generated by simply attaching
a ``deriving Serialize`` clause to the ``UserTree`` datatype
declaration. For more examples of generic functions please refer to the
`generic-deriving <http://hackage.haskell.org/package/generic-deriving>`__
package on Hackage.

More information
----------------

For more details please refer to the `Haskell Wiki
page <http://www.haskell.org/haskellwiki/GHC.Generics>`__ or the
original paper [Generics2010]_.

.. [Generics2010] Jose Pedro Magalhaes, Atze Dijkstra, Johan Jeuring, and Andres Loeh.
   `A generic deriving mechanism for Haskell
   <http://dreixel.net/research/pdf/gdmh.pdf>`__. Proceedings of
   the third ACM Haskell symposium on Haskell (Haskell'2010), pp. 37-48,
   ACM, 2010.

.. _roles:

Roles
=====

.. index::
   single: roles

Using :ghc-flag:`-XGeneralizedNewtypeDeriving`
(:ref:`generalized-newtype-deriving`), a programmer can take existing
instances of classes and "lift" these into instances of that class for a
newtype. However, this is not always safe. For example, consider the
following:

::

      newtype Age = MkAge { unAge :: Int }

      type family Inspect x
      type instance Inspect Age = Int
      type instance Inspect Int = Bool

      class BadIdea a where
        bad :: a -> Inspect a

      instance BadIdea Int where
        bad = (> 0)

      deriving instance BadIdea Age    -- not allowed!

If the derived instance were allowed, what would the type of its method
``bad`` be? It would seem to be ``Age -> Inspect Age``, which is
equivalent to ``Age -> Int``, according to the type family ``Inspect``.
Yet, if we simply adapt the implementation from the instance for
``Int``, the implementation for ``bad`` produces a ``Bool``, and we have
trouble.

The way to identify such situations is to have *roles* assigned to type
variables of datatypes, classes, and type synonyms.

Roles as implemented in GHC are a from a simplified version of the work
described in `Generative type abstraction and type-level
computation <http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf>`__,
published at POPL 2011.

.. _nominal-representational-and-phantom:

Nominal, Representational, and Phantom
--------------------------------------

.. index::
   single: representational; role
   single: nominal; role
   single: phantom; role

The goal of the roles system is to track when two types have the same
underlying representation. In the example above, ``Age`` and ``Int``
have the same representation. But, the corresponding instances of
``BadIdea`` would *not* have the same representation, because the types
of the implementations of ``bad`` would be different.

Suppose we have two uses of a type constructor, each applied to the same
parameters except for one difference. (For example, ``T Age Bool c`` and
``T Int Bool c`` for some type ``T``.) The role of a type parameter says
what we need to know about the two differing type arguments in order to
know that the two outer types have the same representation (in the
example, what must be true about ``Age`` and ``Int`` in order to show
that ``T Age Bool c`` has the same representation as ``T Int Bool c``).

GHC supports three different roles for type parameters: nominal,
representational, and phantom. If a type parameter has a nominal role,
then the two types that differ must not actually differ at all: they
must be identical (after type family reduction). If a type parameter has
a representational role, then the two types must have the same
representation. (If ``T``\'s first parameter's role is representational,
then ``T Age Bool c`` and ``T Int Bool c`` would have the same
representation, because ``Age`` and ``Int`` have the same
representation.) If a type parameter has a phantom role, then we need no
further information.

Here are some examples: ::

      data Simple a = MkSimple a          -- a has role representational

      type family F
      type instance F Int = Bool
      type instance F Age = Char

      data Complex a = MkComplex (F a)    -- a has role nominal

      data Phant a = MkPhant Bool         -- a has role phantom

The type ``Simple`` has its parameter at role representational, which is
generally the most common case. ``Simple Age`` would have the same
representation as ``Simple Int``. The type ``Complex``, on the other
hand, has its parameter at role nominal, because ``Simple Age`` and
``Simple Int`` are *not* the same. Lastly, ``Phant Age`` and
``Phant Bool`` have the same representation, even though ``Age`` and
``Bool`` are unrelated.

.. _role-inference:

Role inference
--------------

What role should a given type parameter should have? GHC performs role
inference to determine the correct role for every parameter. It starts
with a few base facts: ``(->)`` has two representational parameters;
``(~)`` has two nominal parameters; all type families' parameters are
nominal; and all GADT-like parameters are nominal. Then, these facts are
propagated to all places where these types are used. The default role
for datatypes and synonyms is phantom; the default role for classes is
nominal. Thus, for datatypes and synonyms, any parameters unused in the
right-hand side (or used only in other types in phantom positions) will
be phantom. Whenever a parameter is used in a representational position
(that is, used as a type argument to a constructor whose corresponding
variable is at role representational), we raise its role from phantom to
representational. Similarly, when a parameter is used in a nominal
position, its role is upgraded to nominal. We never downgrade a role
from nominal to phantom or representational, or from representational to
phantom. In this way, we infer the most-general role for each parameter.

Classes have their roles default to nominal to promote coherence of
class instances. If a ``C Int`` were stored in a datatype, it would be
quite bad if that were somehow changed into a ``C Age`` somewhere,
especially if another ``C Age`` had been declared!

There is one particularly tricky case that should be explained: ::

      data Tricky a b = MkTricky (a b)

What should ``Tricky``'s roles be? At first blush, it would seem that
both ``a`` and ``b`` should be at role representational, since both are
used in the right-hand side and neither is involved in a type family.
However, this would be wrong, as the following example shows: ::

      data Nom a = MkNom (F a)   -- type family F from example above

Is ``Tricky Nom Age`` representationally equal to ``Tricky Nom Int``?
No! The former stores a ``Char`` and the latter stores a ``Bool``. The
solution to this is to require all parameters to type variables to have
role nominal. Thus, GHC would infer role representational for ``a`` but
role nominal for ``b``.

.. _role-annotations:

Role annotations
----------------

.. ghc-flag:: -XRoleAnnotations

    :since: 7.8.1

    Allow role annotation syntax.

Sometimes the programmer wants to constrain the inference process. For
example, the base library contains the following definition: ::

      data Ptr a = Ptr Addr#

The idea is that ``a`` should really be a representational parameter,
but role inference assigns it to phantom. This makes some level of
sense: a pointer to an ``Int`` really is representationally the same as
a pointer to a ``Bool``. But, that's not at all how we want to use
``Ptr``\ s! So, we want to be able to say ::

      type role Ptr representational
      data Ptr a = Ptr Addr#

The ``type role`` (enabled with :ghc-flag:`-XRoleAnnotations`) declaration
forces the parameter ``a`` to be at role representational, not role
phantom. GHC then checks the user-supplied roles to make sure they don't
break any promises. It would be bad, for example, if the user could make
``BadIdea``\'s role be representational.

As another example, we can consider a type ``Set a`` that represents a
set of data, ordered according to ``a``\'s ``Ord`` instance. While it
would generally be type-safe to consider ``a`` to be at role
representational, it is possible that a ``newtype`` and its base type
have *different* orderings encoded in their respective ``Ord``
instances. This would lead to misbehavior at runtime. So, the author of
the ``Set`` datatype would like its parameter to be at role nominal.
This would be done with a declaration ::

      type role Set nominal

Role annotations can also be used should a programmer wish to write a
class with a representational (or phantom) role. However, as a class
with non-nominal roles can quickly lead to class instance incoherence,
it is necessary to also specify :ghc-flag:`-XIncoherentInstances` to allow
non-nominal roles for classes.

The other place where role annotations may be necessary are in
``hs-boot`` files (:ref:`mutual-recursion`), where the right-hand sides
of definitions can be omitted. As usual, the types/classes declared in
an ``hs-boot`` file must match up with the definitions in the ``hs``
file, including down to the roles. The default role for datatypes is
representational in ``hs-boot`` files, corresponding to the common use
case.

Role annotations are allowed on data, newtype, and class declarations. A
role annotation declaration starts with ``type role`` and is followed by
one role listing for each parameter of the type. (This parameter count
includes parameters implicitly specified by a kind signature in a
GADT-style data or newtype declaration.) Each role listing is a role
(``nominal``, ``representational``, or ``phantom``) or a ``_``. Using a
``_`` says that GHC should infer that role. The role annotation may go
anywhere in the same module as the datatype or class definition (much
like a value-level type signature). Here are some examples: ::

      type role T1 _ phantom
      data T1 a b = MkT1 a     -- b is not used; annotation is fine but unnecessary

      type role T2 _ phantom
      data T2 a b = MkT2 b     -- ERROR: b is used and cannot be phantom

      type role T3 _ nominal
      data T3 a b = MkT3 a     -- OK: nominal is higher than necessary, but safe

      type role T4 nominal
      data T4 a = MkT4 (a Int) -- OK, but nominal is higher than necessary

      type role C representational _   -- OK, with -XIncoherentInstances
      class C a b where ...    -- OK, b will get a nominal role

      type role X nominal
      type X a = ...           -- ERROR: role annotations not allowed for type synonyms

.. _hascallstack:

HasCallStack
============

``GHC.Stack.HasCallStack`` is a lightweight method of obtaining a
partial call-stack at any point in the program.

A function can request its call-site with the ``HasCallStack`` constraint.
For example, we can define ::

   errorWithCallStack :: HasCallStack => String -> a

as a variant of ``error`` that will get its call-site (as of GHC 8.0,
``error`` already gets its call-site, but let's assume for the sake of
demonstration that it does not). We can access the call-stack inside
``errorWithCallStack`` with ``GHC.Stack.callStack``. ::

   errorWithCallStack :: HasCallStack => String -> a
   errorWithCallStack msg = error (msg ++ "\n" ++ prettyCallStack callStack)

Thus, if we call ``errorWithCallStack`` we will get a formatted call-stack
alongside our error message.

.. code-block:: none

   ghci> errorWithCallStack "die"
   *** Exception: die
   CallStack (from HasCallStack):
     errorWithCallStack, called at <interactive>:2:1 in interactive:Ghci1

The ``CallStack`` will only extend as far as the types allow it, for
example ::

   myHead :: HasCallStack => [a] -> a
   myHead []     = errorWithCallStack "empty"
   myHead (x:xs) = x

   bad :: Int
   bad = myHead []

.. code-block:: none

   ghci> bad
   *** Exception: empty
   CallStack (from HasCallStack):
     errorWithCallStack, called at Bad.hs:8:15 in main:Bad
     myHead, called at Bad.hs:12:7 in main:Bad

includes the call-site of ``errorWithCallStack`` in ``myHead``, and of
``myHead`` in ``bad``, but not the call-site of ``bad`` at the GHCi
prompt.

GHC solves ``HasCallStack`` constraints in two steps:

1. If there is a ``CallStack`` in scope -- i.e. the enclosing definition
   has a ``HasCallStack`` constraint -- GHC will push the new call-site
   onto the existing ``CallStack``.

2. Otherwise GHC will solve the ``HasCallStack`` constraint for the
   singleton ``CallStack`` containing just the current call-site.

Importantly, GHC will **never** infer a ``HasCallStack`` constraint,
you must request it explicitly.

``CallStack`` is kept abstract, but GHC provides a function ::

   getCallStack :: CallStack -> [(String, SrcLoc)]

to access the individual call-sites in the stack. The ``String`` is the
name of the function that was called, and the ``SrcLoc`` provides the
package, module, and file name, as well as the line and column numbers.

``GHC.Stack`` additionally exports a function ``withFrozenCallStack`` that
allows users to freeze the current ``CallStack``, preventing any future push
operations from having an effect. This can be used by library authors
to prevent ``CallStack``\s from exposing unnecessary implementation
details. Consider the ``myHead`` example above, the ``errorWithCallStack`` line in
the printed stack is not particularly enlightening, so we might choose
to suppress it by freezing the ``CallStack`` that we pass to ``errorWithCallStack``. ::

   myHead :: HasCallStack => [a] -> a
   myHead []     = withFrozenCallStack (errorWithCallStack "empty")
   myHead (x:xs) = x

.. code-block:: none

   ghci> myHead []
   *** Exception: empty
   CallStack (from HasCallStack):
     myHead, called at Bad.hs:12:7 in main:Bad

**NOTE**: The intrepid user may notice that ``HasCallStack`` is just an
alias for an implicit parameter ``?callStack :: CallStack``. This is an
implementation detail and **should not** be considered part of the
``CallStack`` API, we may decide to change the implementation in the
future.

Compared with other sources of stack traces
-------------------------------------------

``HasCallStack`` does not interact with the RTS and does not require
compilation with ``-prof``. On the other hand, as the ``CallStack`` is
built up explicitly via the ``HasCallStack`` constraints, it will
generally not contain as much information as the simulated call-stacks
maintained by the RTS.
