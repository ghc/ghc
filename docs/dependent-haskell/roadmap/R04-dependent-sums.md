# R04: Dependent sums

The second field of a dependent pair has a type that depends on the value
of the first field. Serokell lists dependent Core (R05) and existential
quantification (R22) as dependencies. R04 retains the witness at runtime.
An erased existential can hide an index without providing its value.
[Serokell roadmap](https://ghc.serokell.io/dh#dependent-sums) and
[first-class existentials proposal](https://github.com/ghc-proposals/ghc-proposals/pull/473).

## What source programs can express

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
A direct constructor pattern on this polymorphic `Const` still needs
separate inference work.
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

The version with the ordinary imported `packageLength` function still
fails during function promotion. Checking its source equation
requires the first pattern field to scope over the kind of the second.

- [x] Represent a single retained witness before a payload whose final
  type argument is that witness in the promoted constructor kind.
- [x] Relate the length of the input package to the field bound by the
  match. Only the input package remains in scope in the exported type.
- [x] Check the projection through a compiled interface. Check a
  mismatched index and the two independent field-demand examples.
- [ ] Use the ordinary imported first projection in the result signature.
