.. _pattern-synonyms:

Pattern synonyms
================

.. extension:: PatternSynonyms
    :shortdesc: Enable pattern synonyms.

    :since: 7.8.1

    Allow the definition of pattern synonyms.

Pattern synonyms are enabled by the language extension :extension:`PatternSynonyms`, which is
required for defining them, but *not* for using them. More information and
examples of pattern synonyms can be found on the :ghc-wiki:`Wiki page <pattern-synonyms>`.

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

Explicitly bidirectional pattern synonyms offer greater flexibility than
implicitly bidirectional ones in terms of the syntax that is permitted. For
instance, the following is not a legal implicitly bidirectional pattern
synonym: ::

      pattern StrictJust a = Just !a

This is illegal because the use of :extension:`BangPatterns` on the right-hand
sides prevents it from being a well formed expression. However, constructing a
strict pattern synonym is quite possible with an explicitly bidirectional
pattern synonym: ::

      pattern StrictJust a <- Just !a where
        StrictJust !a = Just a

Constructing an explicitly bidirectional pattern synonym also:

- can create different data constructors from the underlying data type,
  not just the one appearing in the pattern match;

- can call any functions or conditional logic, especially validation,
  of course providing it constructs a result of the right type;

- can use guards on the lhs of the ``=``;

- can have multiple equations.

For example: ::

      data PosNeg = Pos Int | Neg Int
      pattern Smarter{ nonneg } <- Pos nonneg  where
        Smarter x = if x >= 0 then (Pos x) else (Neg x)

Or using guards: ::

      pattern Smarter{ nonneg } <- Pos nonneg  where
        Smarter x | x >= 0    = (Pos x)
                  | otherwise = (Neg x)

There is an extensive Haskell folk art of `smart constructors
<https://wiki.haskell.org/Smart_constructor>`_,
essentially functions that wrap validation around a constructor,
and avoid exposing its representation.
The downside is that the underlying constructor can't be used as a matcher.
Pattern synonyms can be used as genuinely smart constructors, for both validation and matching.

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

See the :ghc-wiki:`Wiki page <pattern-synonyms>` for more
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
        pat_lhs = expr                      -- lhs restricted, see below

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

The `pat_lhs` for explicitly bidirectional construction cannot use Record syntax.
(Because the rhs *expr* might be constructing different data constructors.)
It can use guards with multiple equations.

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

As of GHC 8.0.1 you may also "bundle" pattern synonyms with an exported type
constructor, making that pattern appear as a data constructor of that type. To
bundle a pattern synonym, we list the pattern synonym in the export list of a
module which exports the type constructor.  For example, to bundle ``Zero``
with ``MyNum`` we could write the following: ::

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
:ghc-wiki:`wiki. <pattern-synonyms/associating-synonyms>`


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

-  The GHCi :ghci-cmd:`:info` command shows pattern types in this format.

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

-  The rules for lexically-scoped type variables (see
   :ref:`scoped-type-variables`) apply to pattern-synonym signatures.
   As those rules specify, only the type variables from an explicit,
   syntactically-visible outer `forall` (the universals) scope over
   the definition of the pattern synonym; the existentials, bound by
   the inner forall, do not.  For example ::

         data T a where
            MkT :: Bool -> b -> (b->Int) -> a -> T a

         pattern P :: forall a. forall b. b -> (b->Int) -> a -> T a
         pattern P x y v <- MkT True x y (v::a)

   Here the universal type variable `a` scopes over the definition of `P`,
   but the existential `b` does not.  (c.f. discussion on #14998.)

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

More precisely, the semantics of pattern matching is given in
`Section 3.17 of the Haskell 2010 report <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-580003.17>`__.   To the informal semantics in Section 3.17.2 we add this extra rule:

* If the pattern is a constructor pattern ``(P p1 ... pn)``, where ``P`` is
  a pattern synonym defined by ``P x1 ... xn = p`` or ``P x1 ... xn <- p``, then:

  (a) Match the value ``v`` against ``p``. If this match fails or diverges,
      so does the whole (pattern synonym) match.   Otherwise the match
      against ``p`` must bind the variables ``x1 ... xn``; let them be bound to values ``v1 ... vn``.

  (b) Match ``v1`` against ``p1``, ``v2`` against ``p2`` and so on.
      If any of these matches fail or diverge, so does the whole match.

  (c) If all the matches against the ``pi`` succeed, the match succeeds,
      binding the variables bound by the ``pi`` . (The ``xi`` are not
      bound; they remain local to the pattern synonym declaration.)

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

Pragmas for pattern synonyms
----------------------------

The :ref:`inlinable-pragma`, :ref:`inline-pragma` and :ref:`noinline-pragma` are supported for pattern
synonyms. For example: ::

    patternInlinablePattern x = [x]
    {-# INLINABLE InlinablePattern #-}
    pattern InlinedPattern x = [x]
    {-# INLINE InlinedPattern #-}
    pattern NonInlinedPattern x = [x]
    {-# NOINLINE NonInlinedPattern #-}

As with other ``INLINABLE``, ``INLINE`` and ``NOINLINE`` pragmas, it's possible to specify
to which phase the pragma applies: ::

    pattern Q x = [x]
    {-# NOINLINE[1] Q #-}

The pragmas are applied both when the pattern is used as a matcher, and as a
data constructor. For explicitly bidirectional pattern synonyms, the pragma
must be at top level, not nested in the where clause. For example, this won't compile: ::

    pattern HeadC x <- x:xs where
      HeadC x = [x]
      {-# INLINE HeadC #-}

but this will: ::

    pattern HeadC x <- x:xs where
      HeadC x = [x]
    {-# INLINE HeadC #-}

When no pragma is provided for a pattern, the inlining decision is made by
GHC's own inlining heuristics.
