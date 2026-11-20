.. _experimental-dependent-types:

Experimental dependent types
============================

.. extension:: ExperimentalDependentTypes
    :shortdesc: Check restricted retained algebraic quantification.

    :since: 10.1
    :status: Experimental

    Enable retained algebraic quantification for an internal experiment.

This flag is local to the native dependent Haskell experiment.
This extension is disabled by default. It enables ``foreach`` in types and
checks a restricted subset of explicit value signatures. The experiment uses a
native dependent Core representation. The restricted module path supports
native top-level signatures and local retained functions. Interfaces encode
retained binders with scoped local Id names. GHCi can load these modules as
interpreted source and keep local declarations between commands.

Local definitions and functions in types
---------------------------------------

A type synonym can name an intermediate type with ``let``, select a
type by matching a constructor, or partially apply a type lambda::

    {-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers #-}
    {-# LANGUAGE StandaloneKindSignatures, TypeFamilies #-}
    module SurfaceGuide (PairUp, Payload, ByType, WithError, Result) where
    import Data.Kind (Type)
    import GHC.Exts (U)

    type PairUp a = let b = Maybe a in (b, b)

    type Payload b = case b of
      True  -> Int
      False -> Bool

    type ByType a = case a of
      Bool     -> Int
      Ordering -> Char

    type WithError :: Type %U -> Type
    type WithError = (\err result -> Either err result) String

    type Result = WithError (Payload 'True)

Here ``Result`` reduces to ``Either String Int``. A module importing the
synonym can use that equality without enabling the extension. The
unmatchable arrow in ``WithError`` uses :extension:`Modifiers`.

A type-level let binds one variable in the body. GHC checks its
right-hand side in the enclosing scope, even if the body ignores it.
The lambda and case forms currently require a top-level synonym on the
outside. A case alternative can bind distinct fields of a constructor.
A variable or wildcard at the root accepts the scrutinee when matching
reaches that row. An earlier constructor pattern blocks reduction on an
unknown input, even if both rows return the same type.

``ByType Bool`` selects ``Int`` directly at compile time. A bare branch
head names a data constructor if one is in scope. Otherwise GHC looks
it up in the type namespace. A promotion tick always selects a data
constructor. A synonym used as a pattern must still have a constructor
head after expansion.

Retained pairs and erased lengths
---------------------------------

A retained pair stores its first component at runtime. The type of its
second component can mention the first. An existential can instead hide
a type index chosen by the producer. For example, a vector filter chooses
its result length after it checks the elements::

    {-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
    {-# OPTIONS_GHC -O0 #-}
    module Filter where
    import GHC.Sigma (Sigma(Pair))

    data Nat = Z | S Nat
    data Vec a (n :: Nat) where
      Nil  :: Vec a 'Z
      Cons :: a -> Vec a n -> Vec a ('S n)

    type Filtered a = exists (n :: Nat). Vec a n

    filterVec :: (a -> Bool) -> Vec a n -> Filtered a
    filterVec _ Nil = Nil
    filterVec p (Cons x xs)
      | p x       = Cons x (filterVec p xs)
      | otherwise = filterVec p xs

    forgetLength :: (some (n :: Nat) | Vec a n) -> Filtered a
    forgetLength (Pair _ xs) = xs

In the passing branch, GHC opens the recursive result at a fresh length
and packs the new cell at its successor. Reading the first passing cell
can leave the tail unevaluated. The caller must establish that the result
is nonempty before it can use a function that requires a successor length.

The dot form infers package introduction and elimination. The bar form
(``exists (n :: Nat) | Vec a n``) uses explicit ``Pack`` expressions and
patterns from ``GHC.Exists``. Compile definitions with surviving erased
package operations at ``-O0``. A separately compiled ``-O1`` module can
call them.

Natural numerals in vector indices
----------------------------------

A library can give its index type an interpretation for nonnegative type
numerals. Import ``FromNatural`` from ``GHC.TypeNats.Experimental`` in the
package ``ghc-experimental``. Define an instance for the index kind. For
example, these declarations register a Peano kind::

    import GHC.TypeNats.Experimental (FromNatural)
    import qualified GHC.TypeNats as TN

    data Nat = Z | S Nat

    type family ToPeano (n :: TN.Nat) :: Nat where
      ToPeano 0 = 'Z
      ToPeano n = 'S (ToPeano (n TN.- 1))

    type instance FromNatural Nat n = ToPeano n

This definition of ``ToPeano`` requires :extension:`UndecidableInstances`.
In a module with :extension:`ExperimentalDependentTypes`, a numeral in
``Vec a 3`` now has the expected kind ``Nat``. The checked instance
determines its value. When GHC infers the kind of a numeral, or expects
``GHC.TypeNats.Nat``, the ordinary built-in literal rules apply.
Family instance patterns keep the ordinary rules for literals.
When a proof needs the full Peano representation of a numeral, each
successor takes a reduction step in this example. Large proofs can
reach GHC's family reduction depth limit.

The packed-vector example defines ``type n + m = plus n m`` and shares
the ordinary definition of ``plus`` with its ``Num Nat`` instance. It can
therefore write signatures such as these::

    three     :: Vec Bool 3
    safeHead  :: Vec a (1 + n) -> a
    appendOne :: Vec a n -> a -> Vec a (n + 1)

The first argument of ``plus`` controls reduction. In ``safeHead``,
``1 + n`` reduces to a successor without inspecting ``n``. Each caller
still uses the ordinary term-level ``fromInteger`` for its runtime literals.
An arbitrary ``Num`` instance cannot establish a runtime index equality.

The library also provides ``replicateAt 3 x :: Vec a 3``, using
:extension:`RequiredTypeArguments`. The first argument specifies the
length in the type. For a length ``n`` read at runtime, its retained
counterpart is ``replicateNative n x :: Vec a n``.

A retained argument can also be a sum of two lengths read from IO::

    import PackedVecLibrary (Nat, Vec, type (+), replicateNative)

    fillSum :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Bool (n + m)
    fillSum n m = replicateNative (n + m) True

The signature uses the library's type operator. The body uses the
standard overloaded ``Prelude.(+)``. The library defines ``(+) = plus``
in its ``Num Nat`` instance. Its interface records both that alias and
the checked equations for ``plus``. GHC checks that this call uses the
same dictionary before relying on the sum in the result type. The
runtime sum stays lazy.

A recursive equation can also call another checked function. For example,
``times (S n) m = plus m (times n m)`` uses the checked equations for
``plus``. The recursive call decreases the first argument of ``times``.
The vector library defines both the type operator ``(*)`` and the
ordinary ``Num Nat`` multiplication method using ``times``.
Enable ``NoStarIsType`` to write the type operator::

    flattenRows :: Vec (Vec a cols) rows -> Vec a (rows * cols)
    flattenRows VNil = VNil
    flattenRows (VCons row rest) = appendVec row (flattenRows rest)

A retained operand can contain several checked calls. Each call uses its
own function or method dictionary. For example::

    prefixed :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Bool (1 + rows * cols)
    prefixed rows cols = replicateNative (1 + rows * cols) True

The two operators have their ordinary fixities. A caller can also write
``plus 1 (times rows cols)``. The first cell can be produced before
evaluating the product.

A checked result can use the defining index of a lazy local name::

    prefixed rows cols =
        let cells = rows * cols
        in replicateNative (1 + cells) True

The compiler checks the ordinary binding first. If its actual ``Num Nat``
dictionary supplies the checked multiplication function, the body can use
``cells`` at the index ``times rows cols``. Each use shares the local value.
The same check applies to a numeral in a local formula, such as
``let cells = times rows 3``.

A lazy let expression can also name more than one intermediate length.
The bindings can have complete monomorphic signatures::

    prefixed rows cols =
        let total, cells :: Nat
            total = 1 + cells
            cells = rows * cols
        in replicateNative total True

GHC checks the signatures first and follows the dependencies between
bindings. Here the body can use ``total`` at the length
``1 + rows * cols``. This rule applies to nonrecursive value definitions
in a let expression with a checked dependent result or a fixed
monomorphic result. A function with one unguarded equation can also
use these definitions in its ``where`` clause::

    safeHead :: Vec a (1 + n) -> a
    safeHead (VCons x _) = x

    localHead :: foreach (r :: Nat) -> foreach (c :: Nat) -> Char
    localHead r c = safeHead (replicateNative total 'x')
      where
        total, area :: Nat
        total = 1 + area
        area = r * c

GHC can use a defining RHS to choose the omitted index for ``safeHead``.
It checks the original vector against the resulting argument type.
With the checked arithmetic from this library, either dimension can be
unknown at compile time. The outer successor suffices to take the head.
If the local length is ``0``, type checking fails at the vector argument.
The mismatch refers to the constructors ``Z`` and ``S`` of this ``Nat`` type.

Each RHS that supplies a length equality must yield a monomorphic value.
A different dictionary can still supply the ordinary runtime operation.
If the body needs the index equality, the compiler also checks rewrite
rules that can affect the selected method.

The vector library's ``Num Nat`` instance also supplies checked equations
for ``fromInteger``. A caller can use those equations at a retained
argument, including inside an addition::

    three :: Vec Bool 3
    three = replicateNative 3 True

    oneMore :: foreach (n :: Nat) -> Vec Bool (n + 1)
    oneMore n = replicateNative (n + 1) True

The checker recognises a ground instance whose ``fromInteger n``
first guards against ``n < 0``, returns the zero constructor for
``n == 0``, and otherwise applies the successor constructor to
``fromInteger (n - 1)``. Both guards use the standard ``Integer``
operations. The instance module publishes the constructors in its
interface. Use retained numerals in a client of that module. Each
successor counts against ``-freduction-depth``.
The ordinary solver must select the checked dictionary at the literal.
A result signature still uses ``FromNatural``. GHC checks
that the two interpretations agree when it checks the result type.

A consumer can infer the tail of a vector produced by another call::

    firstCopy :: foreach (n :: Nat) -> a -> a
    firstCopy n x = safeHead (replicateNative (1 + n) x)

The addition equations reduce the formal length ``1 + k`` of ``safeHead``
to ``S k`` and the argument length to ``S n``. GHC can then choose ``k``
to be the caller's runtime ``n``. The first element is available without
forcing that tail length. With an unknown left addend, ``n + 1`` needs a
proof of nonemptiness or a pattern match on the vector.

A constructor argument can determine the erased type of a retained parameter.
For example, the fields of a list cell determine its element type::

    firstCell :: a -> [a] -> a
    firstCell x xs = safeHead (sourceVector (x : xs))

The equation for ``sourceLength (x : xs)`` gives a successor for any tail
``xs``. The caller can therefore take the head without inspecting the tail.

A call can also use the source function directly, as in
``replicateNative (plus n m) True``. The function must be monomorphic
and fully applied. Its operands can be local values, supported constructors,
or further checked calls. A user-defined operator can supply equations in
the same way as a named function.

GHC can execute a call to an imported ordinary function while checking
a type. An unqualified name uses the existing scope rules. For example,
with :extension:`RequiredTypeArguments`, ``Vec (length [1,2,3]) Bool``
can refer to an imported ``length :: [Natural] -> Natural``. An explicit
type binder named ``length`` takes precedence.

GHC uses a published character equation for ``toUpper`` and executes
``isDigit``, ``sum``, ``reverse`` and ``odd`` in the following examples::

    {-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
    import qualified Data.Char
    import Data.Type.Equality ((:~:)(Refl))

    upper :: Data.Char.toUpper 'x' :~: 'X'
    upper = Refl

    digit :: Data.Char.isDigit '7' :~: 'True
    digit = Refl

    total :: Prelude.sum '[2, 3, 5] :~: 10
    total = Refl

    reversed :: Prelude.reverse '[2, 3, 5] :~: '[5, 3, 2]
    reversed = Refl

    nested :: Prelude.sum (Prelude.reverse '[2, 3, 5]) :~: 10
    nested = Refl

    evenSum :: Prelude.odd (Prelude.sum '[2, 3, 5]) :~: 'False
    evenSum = Refl

In a nested call, GHC checks the argument and uses the kind of its result
to specialize the outer function. Here the inner call returns a list of
``Natural``, so the outer call uses the list and ``Natural`` instances.
GHC checks the operand kinds and the required instances throughout the
chain before it executes any call. It then evaluates each inner argument
once. If that call fails, the error
points to the inner function, even when the outer function would ignore
its argument at runtime.

At the innermost call, write a natural or character literal, a Boolean
constructor, or a promoted list of those values. For example,
``Prelude.odd 5 :~: 'True`` and ``Prelude.and '[ 'True, 'False] :~: 'False``
both admit ``Refl``. GHC uses the declared function argument to select
``[Bool]`` for ``Prelude.and '[]``. A polymorphic ``Prelude.reverse '[]``
uses ``[Natural]`` as before.

Each function takes one unrestricted argument. An intermediate result
can have any of the same six kinds. An empty result list carries its
element kind to the next call. GHC forces
every result element before constructing the promoted list. The producer may use
ordinary Haskell. The compiler needs its compiled code and a target
interpreter. Safe Haskell rejects execution of these calls. If an imported
function diverges, checking its application can also diverge.

A literal can specialize an imported polymorphic function. In ``total``,
the list fixes the container to ``[]`` and the element type to
``Natural``. GHC uses ordinary instance resolution for ``Foldable []`` and
``Num Natural``. Each type parameter must be determined by the input.
A missing or ambiguous instance causes a type error at the call, even if
the module enables :ghc-flag:`-fdefer-type-errors`. An instance defined
in the module currently being compiled cannot supply executable code.

With :extension:`TypeApplications`, the expression
``charVal (Proxy @(Data.Char.toUpper 'x'))`` reflects the result as a
character. A visible type argument containing such a call must be complete.

A pattern can determine the input after GHC reads a partial signature.
For example, with :extension:`NamedWildCards`, :extension:`PartialTypeSignatures`,
:extension:`ScopedTypeVariables` and :extension:`TypeApplications`::

    import Data.Proxy (Proxy(..))
    import qualified Data.Char

    upper :: Proxy _c -> Proxy (Data.Char.toTitle _c)
    upper (_ :: Proxy 'a') = Proxy @'A'

GHC learns that ``_c`` is ``'a'`` from the pattern. Before it generalizes
the binding, it evaluates the call and checks that the body returns
``Proxy 'A'``. A body that returns ``Proxy 'Z'`` fails at the call in
the signature. A body that leaves ``_c`` unknown also fails there.
An importing module sees ``upper :: Proxy 'a' -> Proxy 'A'``.

This form requires a named wildcard in the value type of a partial
signature with no leading constraints. Each input must become a
character literal or a list of natural number literals within the binding
group. An equality available only in one GADT branch cannot determine
the input for the whole group. A local pattern binding with an explicit
multiplicity cannot postpone a call.

An imported polymorphic function can also occur in a type when its argument
is a rigid type variable. For example, this definition checks because
reducing the ordinary Prelude function ``id`` returns the type variable
``a`` unchanged::

    {-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}

    x :: forall a. Num a => id a
    x = 42

The compiler also accepts projections with several operands. For example::

    left :: forall a b. a -> b -> const a b
    left x _ = x

The imported function must have one or more invisible, lifted type
parameters. Each operand type and the result type must be one of those
parameters. Supply all the operands as rigid type variables of kind
``Type``. The function must return one of them during evaluation. If
evaluation reaches a case directly on a rigid operand, the error names
that operand at the source call.
A user-defined function can participate if its interface provides the
Core unfoldings that reduction needs. Compile the producer with
:ghc-flag:`-fno-omit-interface-pragmas` to retain available unfoldings.
The client records each definition it uses. The usual type variable
lookup takes precedence. For example, an explicit type binder named
``id`` still denotes that binder.

The function can select an operand by inspecting a known character.
For example, an ordinary worker can implement ``route 'x' l _ = l``
and ``route _ _ r = r``. If an imported ``routeLeft`` has type
``t -> t -> t`` and definition ``route 'x'``, then
``routeLeft a b`` reduces to ``a`` in the client signature.
The consumer records the worker definition it used. A producer loaded
as bytecode in GHCi can supply its unfoldings with
:ghc-flag:`-fno-unoptimized-core-for-interpreter` and
:ghc-flag:`-fno-omit-interface-pragmas`.

An exported equality can also mention a checked character function.
For example, module ``CharacterCase`` can define::

    upper :: Char -> Char
    upper 'x' = 'X'
    upper 'é' = 'É'
    upper c   = c

A second module can use that function in a signature::

    p3 :: forall c uc. (CharacterCase.upper c ~ uc, KnownChar uc)
       => Proxy c -> Char
    p3 _ = charVal (Proxy @uc)

Enable :extension:`ExperimentalDependentTypes` in those two modules.
A client with the extension switched off can call ``p3 (Proxy @'x')``
and get ``'X'``. Calling it at ``'!'`` returns ``'!'``. The producer's
interface carries the checked equations even with
:ghc-flag:`-fomit-interface-pragmas`. GHC can use the default equation
when it can rule out every earlier incompatible literal. This form
supports a finite set of character literals and an identity default.
An exported function can also forward to an imported function with checked
equations. The fork's in-tree build uses this for ``Data.Char.toUpper``.
Replacing ``CharacterCase.upper c`` with ``Data.Char.toUpper c`` in the
signature above lets an ordinary client reflect the Unicode mapping.

For a class method, the current implementation accepts a concrete
instance of a one-parameter class. The method must return its class
parameter and name a checked function as its complete definition.
The producer enables :extension:`ExperimentalDependentTypes` to
publish this information. A client reads it from the completed
interface. Within the instance module, use ``plus n m`` directly.
An abstract ``Num Nat`` dictionary passed to the client
cannot establish the addition equations.

Retained arguments
------------------

The syntax retains a value argument whose identity can occur in the
result type. For example::

    {-# LANGUAGE ExperimentalDependentTypes #-}

    keep :: foreach (b :: Bool) -> Select b -> Select b
    keep _ x = x

Here ``Select`` denotes a type family indexed by ``Bool``. The argument ``b`` is
an ordinary runtime Boolean whose identity can occur in the result type.
The wildcard leaves this argument unnamed in the equation. The payload
and result still have the same index. Under Haskell's usual lazy pattern
semantics, calling ``keep`` does not force the Boolean. In a module with
:extension:`Strict`, the wildcard has the same demand as a named argument.

Applications retain that value argument. They accept supported constructors,
named local values and checked source calls as described above. The
compiler checks their types without evaluating the runtime arguments.

In GHCi, a retained definition can be used in later commands. For example,
with the vector library loaded, enter the definition and use it at the
next prompt::

    ghci> let fill :: foreach (n :: Nat) -> Vec Bool n; fill n = replicateNative n True
    ghci> toList (fill 2)
    [True,True]

A multiline declaration group between ``:{`` and ``:}`` works as well.
Use ``:seti -XExperimentalDependentTypes`` to enable the syntax at the
prompt. A length obtained from IO can be bound in the same expression
that applies the retained function, as in
``do { n <- readIORef ref; print (toList (fill n)) }``.

Constructor equations can specialise the result type::

    choose :: foreach (b :: Bool) -> Select b
    choose True = 42
    choose False = False

The checker substitutes the constructor into later argument types and the
result type. A consumer can therefore receive the selected value::

    observe :: foreach (b :: Bool) -> Select b -> String
    observe True n = show (n :: Int)
    observe False p = if p then "True" else "False"

The domain can also be a user-defined closed algebraic type::

    {-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeFamilies #-}

    data Nat = Z | S Nat

    data Vec a (n :: Nat) where
      VNil :: Vec a Z
      VCons :: a -> Vec a n -> Vec a (S n)

    type family Add (n :: Nat) (m :: Nat) where
      Add Z m = m
      Add (S n) m = S (Add n m)

    replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
    replicateNative n x = case n of
      Z -> VNil
      S k -> VCons x (replicateNative k x)

    headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
    headNative n (VCons x _) = x

    successorHead :: foreach (n :: Nat) -> Int
    successorHead n = headNative n (replicateNative (S n) 42)

    appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
                 -> Vec a n -> Vec a m -> Vec a (Add n m)
    appendNative n m xs ys = appendVec xs ys

    appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat)
                         -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)
    appendAfterSuccessor n = appendNative @Int (S n)

    appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
    appendVec VNil ys = ys
    appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

The input Nat remains a runtime value. ``headNative`` does not inspect its
Nat argument. ``appendNative`` uses its vector arguments to select equations.
Nat constructor indices use ordinary DataKinds promotion.

The current subset requires a complete value signature. It permits outer
erased ``forall`` binders and multiple annotated retained binders. The domain
must be a closed, lifted, first-order algebraic type. Parameterised domains,
newtypes, existential or constrained constructors, unboxed fields, and
constructor wrappers remain unsupported.

One variable equation can bind the complete retained telescope and match an
ordinary GADT payload, as in ``headNative``. An equation with a complete signature
can also stop before the remaining retained arguments, as in
``appendAfterSuccessor``.
Unsigned aliases of retained functions still require a complete signature and
report GHC-83605. Constructor equations split one
leading retained index. Each constructor of that index must occur in an
unguarded equation. Later ordinary arguments can distinguish equations
for the same retained constructor. GHC checks the ordinary patterns for
coverage and redundancy using the usual pattern warnings. This adapter
checks a list against a requested length::

    fromListExact :: foreach (n :: Nat) -> [a] -> Maybe (Vec a n)
    fromListExact Z [] = Just VNil
    fromListExact Z (_ : _) = Nothing
    fromListExact (S _) [] = Nothing
    fromListExact (S n) (x : xs) = VCons x <$> fromListExact n xs

In each equation, matching the leading constructor refines dependent
argument and result types for that equation alone. Retained constructor
fields and later retained variables accept variable and wildcard patterns.
The arguments must have unrestricted multiplicity. The result must have
lifted kind, and the outer match wrapper must be identity. Ordinary rank
restrictions still apply.
An erased ``forall`` inside a retained binder's scope remains unsupported.
Index-dependent constraints are limited to reflexive
Boolean equalities. Quick Look inference and optimisation pragmas for retained
functions remain restricted. Dedicated rejection checks report GHC-83605.
Ordinary payload mismatches use the existing type error. Unsupported type
positions report GHC-43582.

Source ``case`` expressions can refine a named local retained index when the
result type is checked. The rule also refines referenced local values whose
types depend on that index. It requires one unguarded branch per constructor
and accepts variables or wildcards for its fields. The result must have lifted kind. Parentheses around
the scrutinee are accepted. Generalized locals require unrestricted
multiplicities. Guards, defaults, nested constructor patterns,
Template Haskell quotations, and general motive inference remain unsupported.
The case inspects its index even when the result is a function. Captured payloads
remain lazy until the selected branch uses them.

Calls can pass constructor arguments with fields, including ``S n`` and
``S (S n)``. These calls do not force ``n`` to check its index. CorePrep checks
transport of later payloads and the result when it names a constructor argument.
Transport through a remaining retained function is supported when its closed
algebraic domain and unrestricted multiplicity stay unchanged. This includes
partial applications such as ``appendNative @Int (S n)``. Later applications
retain the original index relationships. Structural Pi unification,
normalization with ambient Given evidence or plugins,
changed domains or multiplicities, and dependent erased binder kinds remain
unsupported. Strict wildcard matching on a partial function with named indices
preserves ordinary forcing behavior. It does not force an
unused index or the function body. An empty match on a function type remains
non-exhaustive. Producers and consumers that use local names must use the same
named index. The checker does not equate a name with its definition.

Global type-family equations can normalize beneath a retained binder with an
unchanged closed algebraic domain and unrestricted multiplicity. Complete
signatures can therefore check aliases with no source argument patterns::

    nativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
    nativeId n xs = xs

    leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n
    leftUnit = nativeId

    leftUnitAppend :: foreach (n :: Nat) (m :: Nat)
                   -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)
    leftUnitAppend = appendNative @Int

A constant family can remove the retained dependency::

    type family ConstantNat (n :: Nat) where
      ConstantNat n = Int

    ordinaryNat :: Nat -> Int
    ordinaryNat _ = 42

    constantAlias :: foreach (n :: Nat) -> ConstantNat n
    constantAlias = ordinaryNat

The normalized signature of ``constantAlias`` is an ordinary ``Nat -> Int``
arrow. This normalization does not evaluate a runtime index. A family that needs
an unknown constructor remains stuck. Solver normalization under the retained
binder excludes ambient Given evidence and plugin rewriting. It does not enable
unsigned aliases, structural retained unification, or a function eta law.
After normalization, identical runtime indices and alpha-equal retained
function types can satisfy reflexivity. This does not decompose a retained
function or assign a type meta-variable. Family axioms reduce at nominal and
representational roles. Existing phantom normalization leaves family heads
unreduced; ordinary forall binder kinds normalize at nominal role.

Use ``-O0 -fomit-interface-pragmas -fno-write-if-simplified-core`` for
object code and bytecode. The restricted module pipeline forces Core Lint
and STG Lint. GHCi can interpret a module in memory. A no-link build can
also write bytecode for a later GHCi session. Profiling, module plugins,
JavaScript code generation, and optimized module options remain unsupported.
Native-containing ``INLINE``, ``INLINABLE``, and class-default unfoldings also
remain unsupported. Their current builders run ordinary optimization during
desugaring.

Use libraries and packages built with a matching compiler. The configured
compiler at this revision is ``10.1.20261115.1``. Its ordinary interface
version is ``100120261115168``.

With :ghc-flag:`-fwrite-ide-info`, an extended interface file also records
the visibility and type of a retained binder, references to runtime
indices, and opaque applications of a bound function. Its version is
separate from the ordinary interface version. Regenerate older
``.hie`` files with this compiler before reading them.

The extension implies no other extensions. In particular, it does not imply
:extension:`KindSignatures`. The ``:: Bool`` annotation specifies the
type of a value binder. It does not make that binder a type variable.

When the extension is disabled, ``foreach`` remains an ordinary identifier::

    foreach :: a -> a
    foreach x = x

When the extension is enabled, ``foreach`` is a reserved keyword. Programs that
use it as an identifier must leave the extension disabled. The extension does
not enable general dependent Haskell, native module unfoldings, or the
ordinary optimizer for dependent Core.
