# R04: Dependent sums

The second field of a dependent pair has a type that depends on the value
of the first field. Serokell lists dependent Core (R05) and existential
quantification (R22) as dependencies. R04 retains the witness at runtime.
An erased existential can hide an index without providing its value.
[Serokell roadmap](https://ghc.serokell.io/dh#dependent-sums) and
[first-class existentials proposal](https://github.com/ghc-proposals/ghc-proposals/pull/473).

## What source programs can express

A package can abstract over the type of the witness as well as the payload
family. The ordinary client uses the same constructor at `Bool`, `Int`,
`Int -> Int` and `IO Int`. Reading a constant payload leaves its
witness unevaluated. At `Bool`, GHC rejects a payload indexed by
`False` when the supplied witness is `True`. A consumer can bind
the stored index and use the payload at that index.
[Pair and eliminator](../examples/packed-vector/DependentPair.hs)
and [separate client](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

The second projection has type `foreach (q :: DPair a p) -> p (first q)`.
The ordinary first projection supplies its equation through the pair
module's interface. The compiler substitutes the witness type into the
kind of the payload family. Changing the first projection to force
the payload invalidates the equation. [Second projection](../examples/packed-vector/DependentProjection.hs).

An older compiler that accepts `DPair` can leave its first projection
without an equation. The `--abstract-witness` upgrade check rebuilds
the pair owner and a separate worker that renames its type parameters.
A fixed-domain worker and an unrelated module keep their objects and
interfaces. A second build reuses all outputs.
[Upgrade test](../examples/closed-evaluation/check-readback-upgrade.sh).

A GADT can already store a runtime length and a vector indexed by that
length. The same encoding works for a family parameter `p`.

```haskell
data Retained (p :: Nat -> Type) where
  Retain :: foreach (n :: Nat) -> p n -> Retained p

withRetained :: Retained p -> (foreach (n :: Nat) -> p n -> r) -> r
withRetained (Retain n value) k = k n value
```

The consumer chooses a single result type `r`. Inside the match it can
use the payload at the stored index. Stage1 compiled the polymorphic
consumer with Core and STG lint. The program read a constant payload
with an exceptional witness. It also read a zero witness with an
exceptional payload. [Generic package](../examples/packed-vector/RetainedNat.hs).
The [Showcase](../Showcase.hs) uses the same
eliminator with both `Vec Char` and `Const Int`. [Existing vector package](../examples/packed-vector/PackedVecLibrary.hs).

The imported selector `getConst` can infer the erased index from its
argument. An ordinary client can use `withRetained p (\_ c -> getConst c)`
without writing a type application. A selector whose result still depends
on the private index cannot export that index as a caller-chosen type.
The callback can also use `case c of Const x -> x`. Checking the first
argument of `withRetained` fixes the payload family to `Const Int`.
The pattern checker reads the actual index from the scrutinee. A
constructor pattern that returns a vector at a caller-chosen index
still fails.
[Interface and escape controls](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

## Project a payload at the length of its input package

Agda's `Σ` record gives the second projection a result type indexed by
the first projection of the same pair. The corresponding source test
for the fork uses the existing `PackedVec` type and an ordinary length
projection. [Agda's built-in Sigma record](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#the-s-type)
and [Eisenberg's thesis, §8.8](https://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf#page=219).

```haskell
payload :: foreach (p :: PackedVec a) -> Vec a (packageLength p)
payload (PackVec _ xs) = xs
```

With an explicit closed `Length` family, the compiler can check
`payload :: foreach (p :: PackedVec a) -> Vec a (Length p)`.
Matching `PackVec` supplies nominal evidence about the actual input.
An ordinary interface-only client can return its vector as a list.
Asking for `Vec a ('S (Length p))` fails at the returned field.
A caller that demands the payload encounters an exception in that
field even if the stored length also raises an exception.
[Projection source](../examples/packed-vector/PackedProjection.hs).

The exported `payload` uses an ordinary imported `packageLength` in its
result type. The compiler checks the worker's case and publishes the
equation that returns its first field. It binds the witness before
checking the kind of the payload. A worker that first forces the vector
has different demand and cannot supply that equation. A rewrite rule
that changes the returned length also prevents its use.
[Ordinary worker](../examples/packed-vector/PackedLength.hs).

- [x] Represent a single retained witness before a payload whose final
  type argument is that witness in the promoted constructor kind.
- [x] Relate the length of the input package to the field bound by the
  match. Only the input package remains in scope in the exported type.
- [x] Check the projection through a compiled interface. Check a
  mismatched index and the two independent field-demand examples.
- [x] Use the ordinary imported first projection in the result signature.

## Abstract over the payload family

The family parameter `p` may itself be a variable. The promoted kind
binds the witness before applying `p` to it. The same `project`
implementation works at `Vec Char` and at `Const Int`.

```haskell
project :: foreach (q :: Retained p) -> p (RetainedWitness.witness q)
project (Retain _ value) = value
```

The checker compares the actual kind of the supplied family with its
declared kind. It requires a lifted payload type. This check preserves
the matchability of the family argument. The result still depends on the
caller’s package when the family is unknown.
The ordinary worker matches `Retain` and returns its first field. The
checker publishes its equation with the declared arrow kind of `p`.
A caller can use the project function through its interface with
`ExperimentalDependentTypes` disabled. Returning the successor
instead of the stored witness fails at the payload.
[Generic projection](../examples/packed-vector/RetainedProjection.hs) and
[ordinary witness](../examples/packed-vector/RetainedWitness.hs).

The upgrade checks rebuild an eligible old witness worker once, then
reuse both the worker and the unchanged consumer. An interface-only
old worker cannot supply the missing equation.
[Upgrade runner](../examples/closed-evaluation/check-readback-upgrade.sh).

## Payload families for the `some` binder

Serokell writes a retained sum as `some (x :: a) | B`. Its payload can
forget the witness. The bootstrap compiler at `60014cbc8e` accepts the
following representation when the constructor specifies both erased kinds.

```haskell
data UPair (a :: Type) (p :: a %U -> Type) where
  MkPair :: forall (a :: Type) (p :: a %U -> Type).
            foreach (x :: a) -> p x -> UPair a p
```

A source-free ordinary client used one payload family that maps `True`
to `Int` and `False` to `Char`. GHC rejected an `Int` payload at
`False` at the payload expression. Another family maps every witness to
`Int`. The client projected `7` at a bottom-valued witness under `-O1`
with Core and STG lint. A type lambda can also apply a captured matchable
constructor, such as `Proxy`, to the witness.

The source experiment elaborates the body of `some` to a type lambda.
The current lambda implementation generates a fresh nominal family.
Two modules that spell the same sum with different binder names can agree
at both Boolean inputs and still fail to exchange their packages. The
acceptance test for first-class sums must pass one module's package to
the other module's consumer. It must also reject a body that gives a
different result at an index. The representation above supplies the
runtime fields and their projection. Type equality across declarations
remains open. [Serokell's R04 card](https://ghc.serokell.io/dh#dependent-sums)
and [type-lambda elaboration](R17-type-level-lambdas.md#source-and-evidence).
