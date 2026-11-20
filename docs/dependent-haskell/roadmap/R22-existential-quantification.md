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

- [x] Form a lifted erased existential in private Core. The formation
  test checks scope, substitution, alpha-equivalence and Core Lint. It
  writes a real interface signature, reads it back from disk, and
  typechecks it. It also reads the HIE type. The reader rejects a binder or
  payload of kind `Constraint` and a reference outside its scope.
- [x] Construct an explicit package in private Core. Core Lint checks
  that the payload has the type specified by the witness. It rejects
  a `Maybe Bool` payload when the chosen witness requires `Maybe Int`.
- [x] Open an arbitrary package parameter in private Core. A fresh
  compiler reads the function body from an interface. Core Lint
  rejects a witness that escapes through the result type.
- [x] Run the imported function on a defined payload and an exceptional
  payload in the bytecode interpreter. The exception occurs when the
  body demands the payload. The STG checks show that a named payload
  goes directly to its consumer and a named package retains its Id.
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

The private formation checkpoint is `a3bac26492` on the local recovery
ref `refs/archive/dependent-haskell/r22-formation`. Its GHC API test
is `NativeExistentialCore`. The expression checkpoint is `949e49d863`
on the local ref `refs/archive/dependent-haskell/r22-expressions`.
Its `NativeExistentialExpr` test writes and reads the interface in
separate GHC sessions.

## Explicit source packages

With `ExperimentalDependentTypes` and `RequiredTypeArguments`, a
module can import `GHC.Exists` and export an explicit package.

```haskell
package :: exists (a :: Type) | Maybe a
package = Pack (type Bool) (Just True)

present :: Bool
present = case package of
  Pack (type a) payload ->
    case payload of
      Nothing -> False
      Just _ -> True
```

The expected type supplies the package schema. Write `Pack @Schema`
when the application must synthesize its type. A sole Pack alternative
binds a fresh type variable over the payload pattern and right-hand side.
An ordinary variable payload pattern can ignore bottom. A constructor
payload pattern demands the payload.

The frozen source compiler at `750604f34c` compiled the provider and
linked an ordinary consumer after removing the provider source. It
checked Core, STG and HIE, exact constructor identity, both payload
demand paths, witness escape and Template Haskell. The build used a
temporary target package database with the base marker compiled in the
configured base unit. The source test `NativeExistentialSource` runs
those controls against the `GHC.Exists` provided by the built base.

This slice supports one lifted witness, saturated applications and a
sole prefix Pack case alternative. Compile modules that use erased
existential types or expressions at `-O0`. The Showcase module exports
its observation as an ordinary tuple for the optimized Showcase entry
point. The implicit dot form and its vector filter remain open.

At `e04f9add03`, the filter signature fails at the dot in `exists m.`
with GHC-68947. An ordinary GADT control uses `Some :: Vec n a -> Some a`.
Its successful branch matches `Some tail` from the recursive call before
returning `Some (Cons x tail)`. Reading the first result in this version
raises the tail exception. This is the demand difference the filter test
must distinguish.
