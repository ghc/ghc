# R22. First-class existential quantification

Filtering a vector illustrates why a caller might want to hide an
index. Only the filter knows the result length. A caller can still use
the vector even if it never asks for its length. Proposal #473 gives
this signature for the implicit form:

```haskell
filter :: (a -> Bool) -> Vec n a -> exists m. Vec m a
```

The witness `m` erases. The result has the runtime representation of
its payload. R04's `PackedVec` serves a different caller, who can also
read the stored length at runtime. Agda makes the erasure distinction
explicit with `@0` on an argument. Its vector example shows a cons
constructor that carries only the element and tail at runtime.
[Serokell R22](https://ghc.serokell.io/dh#existential-quantification),
[proposal #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
and [Agda erasure](https://agda.readthedocs.io/en/v2.8.0/language/runtime-irrelevance.html).

## What to check first

- [ ] Give an explicit package type such as `exists (a :: Type) | Maybe a`
  a checked Core representation. A pack at witness `Int` must take a
  `Maybe Int`. Reject a `Maybe Bool` payload at that witness.
- [ ] Open an arbitrary package in a separately compiled function. The
  witness is fresh and scoped over the payload and the body. It cannot
  appear free in the result type. Keep this check in Core Lint as well
  as the frontend.
- [ ] Compile a producer and an interface-only client. Check both
  `Nothing` and `Just`. Applying `seq` to a package with an exceptional
  payload must observe the exception. Opening a package and ignoring its
  payload should preserve laziness. Inspect the generated code for an
  extra constructor or witness argument.
- [ ] Infer packing and opening in the vector filter. Test the first
  element of a vector whose tail raises an exception. If the first element
  passes the predicate, the first result should be available immediately.

The explicit slice follows the draft's alternative Core rules for
`Pack` and lexical `Unpack`. Eliminating an immediately constructed
package checks substitution for both binders. A package received from
another module also needs a representation, scope and erasure rule.
The paper gives the inference algorithm for the implicit form.
[Eisenberg, Duboc, Weirich and Lee, *An Existential Crisis Resolved*](https://richarde.dev/papers/2021/exists/exists.pdf).

At `e04f9add03`, the filter signature fails at the dot in `exists m.`
with GHC-68947. An ordinary GADT control uses `Some :: Vec n a -> Some a`.
Its successful branch matches `Some tail` from the recursive call before
returning `Some (Cons x tail)`. Reading the first result in this version
raises the tail exception. This is the demand difference the filter test
must distinguish.
