.. _type-families:

Type families
=============

.. extension:: TypeFamilies
    :shortdesc: Enable type families.
        Implies :extension:`ExplicitNamespaces`, :extension:`KindSignatures`,
        and :extension:`MonoLocalBinds`.

    :implies: :extension:`MonoLocalBinds`, :extension:`KindSignatures`,
              :extension:`ExplicitNamespaces`
    :since: 6.8.1

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

Type families are enabled by the language extension :extension:`TypeFamilies`. Additional
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
    Press, 2005.

.. [AssocTypeSyn2005]
    “`Type Associated Type
    Synonyms <http://www.cse.unsw.edu.au/~chak/papers/CKP05.html>`__\ ”. M.
    Chakravarty, G. Keller, and S. Peyton Jones. In Proceedings of “The
    Tenth ACM SIGPLAN International Conference on Functional Programming”,
    ACM Press, pages 241-253, 2005.

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

    data family GMap k :: Type -> Type

The special ``family`` distinguishes family from standard data
declarations. The result kind annotation is optional and, as usual,
defaults to ``Type`` if omitted. An example is ::

    data family Array e

Named arguments can also be given explicit kind signatures if needed.
Just as with :ref:`GADT declarations <gadt>` named arguments are
entirely optional, so that we can declare ``Array`` alternatively with ::

    data family Array :: Type -> Type

Unlike with ordinary data definitions, the result kind of a data family
does not need to be ``Type``. It can alternatively be:

* Of the form ``TYPE r`` for some ``r`` (see :ref:`runtime-rep`).
  For example: ::

    data family DF1 :: TYPE IntRep
    data family DF2 (r :: RuntimeRep)  :: TYPE r
    data family DF3 :: Type -> TYPE WordRep

* A bare kind variable (with :extension:`PolyKinds` enabled).
  For example: ::

    data family DF4 :: k
    data family DF5 (a :: k) :: k
    data family DF6 :: (k -> Type) -> k

Data instances' kinds must end in ``Type``, however. This restriction is
slightly relaxed when the :extension:`UnliftedNewtypes` extension is enabled,
as it permits a ``newtype instance``'s kind to end in ``TYPE r`` for some
``r``.

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

When :extension:`ExplicitForAll` is enabled, type and kind variables used on
the left hand side can be explicitly bound. For example: ::

    data instance forall a (b :: Proxy a). F (Proxy b) = FProxy Bool

When an explicit ``forall`` is present, *all* type and kind variables mentioned
which are not already in scope must be bound by the ``forall``: ::

    data instance forall   (a :: k). F a = FOtherwise  -- rejected: k not in scope
    data instance forall k (a :: k). F a = FOtherwise  -- accepted

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
the extension :extension:`PartialTypeSignatures` have any effect.

A type or kind variable explicitly bound using :extension:`ExplicitForAll` but
not used on the left hand side will generate an error, not a warning.

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

    type family Elem c :: Type

The special ``family`` distinguishes family from standard type
declarations. The result kind annotation is optional and, as usual,
defaults to ``Type`` if omitted. An example is ::

    type family Elem c

Parameters can also be given explicit kind signatures if needed. We call
the number of parameters in a type family declaration, the family's
arity, and all applications of a type family must be fully saturated
with respect to that arity. This requirement is unlike ordinary type synonyms
and it implies that the kind of a type family is not sufficient to
determine a family's arity, and hence in general, also insufficient to
determine whether a type family application is well formed. As an
example, consider the following declaration: ::

    type family F a b :: Type -> Type
      -- F's arity is 2,
      -- although its overall kind is Type -> Type -> Type -> Type

Given this declaration the following are examples of well-formed and
malformed types: ::

    F Char [Int]       -- OK!  Kind: Type -> Type
    F Char [Int] Bool  -- OK!  Kind: Type
    F IO Bool          -- WRONG: kind mismatch in the first argument
    F Bool             -- WRONG: unsaturated application

The result kind annotation is optional and defaults to ``Type`` (like
argument kinds) if omitted. Polykinded type families can be declared
using a parameter in the kind annotation: ::

    type family F a :: k

In this case the kind parameter ``k`` is actually an implicit parameter
of the type family.

At definition site, the arity determines what inputs can be matched on: ::

    data PT (a :: Type)

    type family F1 :: k -> Type
    type instance F1 = PT
      -- OK, 'k' can be matched on.

    type family F0 :: forall k. k -> Type
    type instance F0 = PT
      -- Error:
      --   • Expected kind ‘forall k. k -> Type’,
      --       but ‘PT’ has kind ‘Type -> Type’
      --   • In the type ‘PT’
      --     In the type instance declaration for ‘F0’

Both ``F1`` and ``F0`` have kind ``forall k. k -> Type``, but their arity
differs.

At use sites, the arity determines if the definition can be used in a
higher-rank scenario: ::

    type HRK (f :: forall k. k -> Type) = (f Int, f Maybe, f True)

    type H1 = HRK F0  -- OK
    type H2 = HRK F1
      -- Error:
      --   • Expected kind ‘forall k. k -> Type’,
      --       but ‘F1’ has kind ‘k0 -> Type’
      --   • In the first argument of ‘HRK’, namely ‘F1’
      --     In the type ‘HRK F1’
      --     In the type declaration for ‘H2’

This is a consequence of the requirement that all applications of a type family
must be fully saturated with respect to their arity.

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

Also in the same way as :ref:`data-instance-declarations`, when
:extension:`ExplicitForAll` is enabled, type and kind variables can be
explicitly bound in a type instance declaration.

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

A closed type family's equations have the same restrictions and extensions as
the equations for open type family instances. For instance, when
:extension:`ExplicitForAll` is enabled, type or kind variables used on the
left hand side of an equation can be explicitly bound, such as in: ::

  type family R a where
    forall t a. R (t a) = [a]
    forall a.   R a     = a

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

    type family F a :: Type
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

    type family G a b :: Type -> Type
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

These instances are compatible because they differ in their implicit kind
parameter; the first uses ``Type`` while the second uses ``Type -> Type``.

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

Incompatibilities between closed type family equations can be displayed
in :ghci-cmd:`:info` when :ghc-flag:`-fprint-axiom-incomps` is enabled.

However see :ref:`ghci-decls` for the overlap rules in GHCi.

.. _type-family-decidability:

Decidability of type synonym instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to guarantee that type inference in the presence of type
families is decidable, we need to place a number of additional restrictions
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

If the option :extension:`UndecidableInstances` is passed to the compiler
(see :ref:`undecidable-instances`), the above restrictions are not enforced
and it is on the programmer to ensure termination of the normalisation
of type families during type inference.

Reducing type family applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. ghc-flag:: -ffamily-application-cache
    :shortdesc: Use a cache when reducing type family applications
    :type: dynamic
    :reverse: -fno-family-application-cache
    :category:

    The flag :ghc-flag:`-ffamily-application-cache` (on by default) instructs
    GHC to use a cache when reducing type family applications. In most cases,
    this will speed up compilation. The use of this flag will not affect
    runtime behaviour.

When GHC encounters a type family application (like ``F Int a``) in a program,
it must often reduce it in order to complete type checking. Here is a simple
example::

  type family F a where
    F Int            = Bool
    F (Maybe Double) = Char

  g :: F Int -> Bool
  g = not

Despite the fact that ``g``\'s type mentions ``F Int``, GHC must recognize that
``g``\'s argument really has type ``Bool``. This is done by *reducing* ``F Int``
to become ``Bool``. Sometimes, there is not enough information to reduce a type
family application; we say such an application is *stuck*. Continuing this example,
an occurrence of ``F (Maybe a)`` (for some type variable ``a``) would be stuck, as
no equation applies.

During type checking, GHC uses heuristics to determine which type family application
to reduce next; there is no predictable ordering among different type family applications.
The non-determinism rarely matters in practice. In most programs, type family reduction
terminates, and so these choices are immaterial. However, if a type family application
does not terminate, it is possible that type-checking may unpredictably diverge. (GHC
will always take the same path for a given source program, but small changes in that
source program may induce GHC to take a different path. Compiling a given, unchanged
source program is still deterministic.)

In order to speed up type family reduction, GHC normally uses a cache, remembering what
type family applications it has previously reduced. This feature can be disabled with
:ghc-flag:`-fno-family-application-cache`.

.. _type-wildcards-lhs:

Wildcards on the LHS of data and type family instances
------------------------------------------------------

When the name of a type argument of a data or type instance
declaration doesn't matter, it can be replaced with an underscore
(``_``). This is the same as writing a type variable with a unique name. ::

    data family F a b :: Type
    data instance F Int _ = Int
    -- Equivalent to  data instance F Int b = Int

    type family T a :: Type
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
      data GMap k :: Type -> Type
      ...

    class Collects ce where
      type Elem ce :: Type
      ...

When doing so, we (optionally) may drop the "``family``" keyword.

The type parameters must all be type variables, of course, and some (but
not necessarily all) of then can be the class parameters. Each class
parameter may only be used at most once per associated type, but some
may be omitted and they may be in an order other than in the class head.
Hence, the following contrived example is admissible: ::

      class C a b c where
        type T c a x :: Type

Here ``c`` and ``a`` are class parameters, but the type is also indexed
on a third parameter ``x``.

.. _assoc-inst:

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

The data or type family instance for an associated type must follow
the rule that the type indexes corresponding to class parameters must be
precisely the same as types given in the instance head. For example: ::

    class Collects ce where
      type Elem ce :: Type

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

-  The type variables on the right hand side of the type family equation
   must, as usual, be explicitly bound by the left hand side. This restriction
   is relaxed for *kind* variables, however, as the right hand side is allowed
   to mention kind variables that are implicitly bound. For example, these are
   legitimate: ::

    data family Nat :: k -> k -> Type
    -- k is implicitly bound by an invisible kind pattern
    newtype instance Nat :: (k -> Type) -> (k -> Type) -> Type where
      Nat :: (forall xx. f xx -> g xx) -> Nat f g

    class Funct f where
      type Codomain f :: Type
    instance Funct ('KProxy :: KProxy o) where
      -- o is implicitly bound by the kind signature
      -- of the LHS type pattern ('KProxy)
      type Codomain 'KProxy = NatTr (Proxy :: o -> Type)

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
   with a finite number of alternatives (unlike ``Type``).

-  When :extension:`ExplicitForAll` is enabled, type and kind variables can be
   explicitly bound in associated data or type family instances in the same way
   (and with the same restrictions) as :ref:`data-instance-declarations` or
   :ref:`type-instance-declarations`. For example, adapting the above, the
   following is accepted: ::

     instance Eq (Elem [e]) => Collects [e] where
       type forall e. Elem [e] = e

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
   left hand side, and type variables may not be repeated on the left-hand
   side. The right hand side must mention only type
   variables that are explicitly bound on the left hand side. This restriction
   is relaxed for *kind* variables, however, as the right hand side is allowed
   to mention kind variables that are implicitly bound on the left hand side.

   Like with :ref:`assoc-inst`, it is possible to explicitly bind type and kind
   variables in default declarations with a ``forall`` by using the
   :extension:`ExplicitForAll` language extension.

-  Unlike the associated type family declaration itself, the type variables of
   the default instance are independent of those of the parent class.

Here are some examples:

::

      class C (a :: Type) where
        type F1 a :: Type
        type instance F1 a = [a]     -- OK
        type instance F1 a = a->a    -- BAD; only one default instance is allowed

        type F2 b a                  -- OK; note the family has more type
                                     --     variables than the class
        type instance F2 c d = c->d  -- OK; you don't have to use 'a' in the type instance

        type F3 a
        type F3 [b] = b              -- BAD; only type variables allowed on the
                                     --      LHS, and the argument to F3 is
                                     --      instantiated to [b], which is not
                                     --      a bare type variable

        type F4 x y
        type F4 x x = x              -- BAD; the type variable x is repeated on
                                     --      the LHS

        type F5 a
        type F5 b = a                -- BAD; 'a' is not in scope  in the RHS

        type F6 a :: [k]
        type F6 a = ('[] :: [x])     -- OK; the kind variable x is implicitly
                                     --     bound by an invisible kind pattern
                                     --     on the LHS

        type F7 a
        type F7 a =
          Proxy ('[] :: [x])         -- BAD; the kind variable x is not bound,
                                     --      even by an invisible kind pattern

        type F8 (x :: a) :: [a]
        type F8 x = ('[] :: [a])     -- OK; the kind variable a is implicitly
                                     --     bound by the kind signature of the
                                     --     LHS type pattern

        type F9 (a :: k)
        type F9 a = Maybe a          -- BAD; the kind variable k is
                                     --      instantiated to Type, which is not
                                     --      a bare kind variable

        type F10 (a :: j) (b :: k)
        type F10 (a :: z) (b :: z)
          = Proxy a                  -- BAD; the kind variable z is repeated,
                                     --      as both j and k are instantiated to z

        type F11 a b
        type forall a b. F11 a b = a -- OK; LHS type variables can be
                                     --     explicitly bound with 'forall'

        type F12 (a :: k)
        type F12 @k a = Proxy a      -- OK; visible kind application syntax is
                                     --     permitted in default declarations

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

Bear in mind that it is also possible for the *right*-hand side of an
associated family instance to contain *kind* parameters (by using the
:extension:`PolyKinds` extension). For instance, this class and instance are
perfectly admissible: ::

    class C k where
      type T :: k

    instance C (Maybe a) where
      type T = (Nothing :: Maybe a)

Here, although the right-hand side ``(Nothing :: Maybe a)`` mentions a kind
variable ``a`` which does not occur on the left-hand side, this is acceptable,
because ``a`` is *implicitly* bound by ``T``'s kind pattern.

A kind variable can also be bound implicitly in a LHS type pattern, as in this
example: ::

    class C a where
      type T (x :: a) :: [a]

    instance C (Maybe a) where
      type T x = ('[] :: [Maybe a])

In ``('[] :: [Maybe a])``, the kind variable ``a`` is implicitly bound by the
kind signature of the LHS type pattern ``x``.

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
5.2 <http://www.haskell.org/onlinereport/modules.html#sect5.2>`__) need
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
      data GMap k :: Type -> Type
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

.. extension:: TypeFamilyDependencies
    :shortdesc: Enable injective type families.
        Implies :extension:`TypeFamilies`.

    :implies: :extension:`TypeFamilies`
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

The injectivity annotation is added after the type family head and consists of
two parts:

-  a type variable that names the result of a type family. Syntax:
   ``= tyvar`` or ``= (tyvar :: kind)``. The type variable must be fresh.

-  an injectivity annotation of the form ``| A -> B``, where ``A`` is the
   result type variable (see previous bullet) and ``B`` is a list of
   argument type and kind variables in which type family is injective.
   It is possible to omit some variables if the type family is not injective
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

Verifying the injectivity annotation against type family equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once the user declares type family to be injective GHC must verify that
this declaration is correct, i.e., that type family equations don't violate the
injectivity annotation. A general idea is that if at least one equation
(bullets (1), (2) and (3) below) or a pair of equations (bullets (4) and
(5) below) violates the injectivity annotation then a type family is not
injective in a way the user claims and an error is reported. In the bullets
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
   injective argument to a type family. Type inference can potentially
   loop when looking under injective type families in the RHS, so this
   requires :extension:`UndecidableInstances`; GHC suggests enabling
   the flag when it is necessary.

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
   be paired with all the preceding equations. Of course a
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


