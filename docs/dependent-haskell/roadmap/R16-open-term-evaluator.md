# R16. Open term evaluator

The upstream example has a type variable whose value the compiler cannot
know. Identity can return it unchanged. Serokell proposes normalization
by evaluation for this problem. R13 includes this card. The cards for
lambdas, lets and cases in types also depend on R16 and dependent Core
(R05). [Serokell R16](https://ghc.serokell.io/dh#open-term-evaluator)

```haskell
x :: forall a. Num a => id a
x = 42
```

With `ExperimentalDependentTypes` and `ScopedTypeVariables`, the compiler
accepts this signature. At `Int`, the value is `42`. Before M100 the
renamer reported GHC-68947 at `id`. The same program now also works with
`Prelude.id` and with an independently defined function of the same type
whose body returns its argument. [Source examples](../examples/open-evaluation/OpenSource.hs).

With `TypeApplications`, a caller can also write `Proxy @(Prelude.id a)`
for a rigid `a`. GHC checks whether the whole visible argument is complete
before it reduces the call.

## Selecting between two type variables

```haskell
first :: forall a b. a -> b -> const a b
first x _ = x
```

The compiler reads the ordinary definition of Prelude's `const`. It gives
each distinct source type variable a symbolic operand and checks which one
the function returns. The same variable in two argument positions has one
identity. An independently defined `second _ y = y` selects `b`. Changing
the pattern to `second !_ y = y` makes Core inspect the first operand.
When `a` is rigid, evaluation stops at that case and GHC-70566 names `a`
at the call. The source harness runs both projections with `Bool` and
`Int` and checks the strict version under separate compilation and
`--make`. [Producer](../examples/open-evaluation/OpenProducer.hs)
and [consumer](../examples/open-evaluation/OpenSource.hs). The
[strictness references](../REFERENCES.md#strict-and-lazy-open-projections)
give the Haskell and Agda precedents for this check.

## Selecting a type with a known character

An ordinary producer can select between two unknown types by inspecting a
known character. The two exported functions choose different branches.

```haskell
route :: Char -> t -> t -> t
route 'x' l _ = l
route _   _ r = r

routeLeft, routeRight :: t -> t -> t
routeLeft  = route 'x'
routeRight = route '!'
```

In the client, the two operands of `OpenProducer.routeLeft a b` are
distinct rigid type variables. The type checker reads the worker's Core,
matches the character, and returns `a`. With `routeRight`, it returns
`b`. Calling the two projections on `False` and `42 :: Int` returns
`False` and `42`, respectively. If the selected path demands a rigid
operand, GHC reports that operand at the call.
The observer checks the wired-in character data constructor in the
worker's Core. GHC desugars the character alternatives into a case on
its `Char#` field. In GHCi these provider modules retain their unfoldings
in bytecode mode. The observer reads through a debugger breakpoint when
checking the type.
[Worker](../examples/open-evaluation/OpenAlias.hs),
[client](../examples/open-evaluation/OpenSource.hs)
and [separate-compilation test](../examples/open-evaluation/check-source.sh).

## Inferring a character from a pattern

```haskell
upper :: Proxy _c -> Proxy (Data.Char.toUpper _c)
upper (_ :: Proxy 'a') = Proxy @'A'
```

The pattern determines `_c`. GHC runs the imported function on `'a'`
and checks the body against the resulting `Proxy 'A'`. If the body
claims `'Z'`, the error at `Data.Char.toUpper` reports both characters.
If the pattern leaves `_c` unknown, GHC reports the blocked input there.
An ordinary client sees `upper :: Proxy 'a' -> Proxy 'A'` in the
interface. [Source tests](../../../testsuite/tests/dependent/should_run/NativeOpenRetry.hs)
and [interface test](../../../testsuite/tests/dependent/should_run/NativeOpenRetryInterface.sh).

A partial signature with no leading constraints owns the pending calls
on its named wildcards. After solving the binding group's ordinary
constraints, GHC retries the calls whose operands are literals. A result
can make a second call ready. The group must finish every call before
generalization. A binding plan that skips this inference step rejects a
pending call at its source. [Solver and design note](../../../compiler/GHC/Tc/Solver.hs)
and [Agda's constraint ownership](../REFERENCES.md#retrying-a-check-during-inference).

## Reducing a rigid variable

Use the actual typed unfolding for Prelude's `id`. A source skolem
`a :: Type` stands for a type chosen by the caller. Give the evaluator
a fresh neutral variable of the same kind and keep the correspondence
to that skolem. After evaluation, only that variable can be read back
as `a`. A same-spelling variable with a different identity must fail the
check. This experiment has no runtime representation for types.

An unfolding is the typed Core body that a producer makes available in its
interface. It can refer to other bindings. On this build GHC shares the
implementations of `id` and `breakpoint`. The evaluator follows the alias
and records both uses. With a home-module alias, changing only its worker
makes the unchanged client recheck the call. A missing unfolding is an
error at the call. The client can compile at `-O0`. The fixture's ordinary
producer uses `-fno-omit-interface-pragmas` to supply its body.
[Separate compilation and compilation with `--make`](../examples/open-evaluation/check-source.sh).

The GHC API test checks eleven observations. They include following that
alias, distinguishing same-spelling symbolic operands by identity, and
preserving a neutral case. The source test exercises reification to the
original skolem through type checking. Retrying the API observation with a
supplied definition models resolution of a blocked variable. The probe
does not modify the GHC solver. The compiler bounds the number of steps
using `-freduction-depth`. Only a successful reduction to the original
rigid variable enters the source type. A residual case or application
still needs a representation and evidence before it could enter Core.
[Evaluator](../../../compiler/GHC/Core/OpenWhnf.hs) and
[type elaboration](../../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

## Evidence required

- [x] Reduce an ordinary polymorphic identity applied to a rigid variable.
      Preserve its binder identity under shadowing. Test a projection whose
      unused argument diverges.
- [x] At the evaluator API, suspend a case on a rigid scrutinee with its
      binder and alternatives. Observe the same case after supplying a
      previously unknown definition.
- [x] Retry a source call on a partial signature's named wildcard after
      inference determines its literal value. Reject an unresolved input
      at the call before generalization.
- [ ] Keep a call on a rigid parameter in an exported equality constraint.
      Check the equality when an importing client supplies a literal. The
      caller chooses this input after the producer has been compiled.
- [ ] Specify the typed input, lexical environments and which reductions
      produce weak-head normal form. Preserve Haskell's demand behavior.
      A strict pattern cannot discard the need to inspect its argument.
- [ ] Establish the evidence required when an application survives into
      Core. The fork's `TermAppCo` relates corresponding applications of a
      retained function. It supplies congruence alone. A beta step needs a
      checked justification if it changes a type that already appears in Core.

GHC issue #27413 explicitly asks which typed representation to evaluate
and how to handle metavariable holes. Agda's evaluator records why a
reduction is blocked. These are starting points for the first two checks.
[GHC #27413](https://gitlab.haskell.org/ghc/ghc/-/issues/27413)
and [Agda's weak-head reduction](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Reduce.hs#L616).

## When to retry a blocked case

Consider an ordinary Core case that evaluates `f x` and returns `k` in
its default branch. If `f` is an inference variable, assigning `x` alone
gives the checker no reason to retry. If the solver equates `f` with a
second inference variable, the next observation should wait on that
second variable. A rigid `f` stays unknown throughout this check.

A separate API experiment exercised these cases with seven assertions and
actual GHC metavariable cells. The fixture supplied the assignments and
their interpretation as symbolic Core values. M104 checks for ready literals at the existing
generalization step. A general evaluator still needs to identify the
variable that blocks an observation and wake the affected constraints.
GHC already records unification and revisits affected constraints in
`kickOutAfterUnification`. A reduction that survives into FC also needs
checked nominal evidence for its endpoints.
[GHC solver](../../../compiler/GHC/Tc/Solver/Monad.hs),
[Eisenberg's Core Lint](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L498-L548)
and [Agda blockers](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/Syntax/Internal/Blockers.hs#L26).

## An equality that crosses a module boundary

Draft proposal #509 gives this producer. The consumer calls it with
`Proxy @'x'` and expects `'X'`.

```haskell
p3 :: forall c uc. (Data.Char.toUpper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 _ = charVal (Proxy @uc)
```

The producer checks the body with the equality and dictionary supplied
by its caller. The caller must establish the equality after it chooses
`c`. The application must therefore survive in the producer's interface.
M104 rejects this signature at `Data.Char.toUpper`.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509).

Start with an ordinary imported `Char -> Char` that changes one literal
and returns its input in the default branch. This is the shape of the
[Unicode worker](../../../libraries/ghc-internal/src/GHC/Internal/Unicode/Char/UnicodeData/SimpleUpperCaseMapping.hs)
behind the installed library function. This example has three checks.

- A rigid operand must stay unevaluated. In particular, choosing the
  default branch at this point could yield the wrong result.
- For a ground operand, Core Lint must be able to check the claimed
  reduction against the producer's typed definition. Check both the
  literal branch and the default, including a deliberately false result.
- A client must read the application from the interface. Changing the
  producer's selected branch without changing its signature must cause
  the unchanged client to check its equality again.

The [GHC API regression](../../../testsuite/tests/ghc-api/OpenCharBranchWitness.hs)
reads the desugared Core for an ordinary producer and its importer. The producer changed `'x'` to `'X'`, changed `'é'` to `'É'`
and returned its input in the default branch. The observer selects all three results and stays blocked on the exact rigid operand.
The test records a proposed ground reduction and replays its selected
alternatives against the linted definition. It rejects a changed
producer, a missing step and a wrong result. The trace lives in memory.
An exported equality still needs a representation and nominal evidence.

The default branch exposed a demand requirement. GHC shares the default
code in a local function that takes an unboxed unit. The observer could
enter it with the constructor `(##)`. A test substituted an argument of
the same type that first inspects a rigid character. That argument could
not be discarded. Inspect the producer with `-ddump-ds-preopt` to see the
continuation before optimization.
[Desugaring shared pattern-match failure](../../../compiler/GHC/HsToCore/Utils.hs).

The design question is how to represent and check this reduction in
Core. The current `TermAppCo` proves congruence under one local head.
GHC's built-in literal rules give an example of checking proof endpoints.
A checked step for a case must also identify the imported definition and
preserve its demand on the scrutinee. [Core Lint](../../../compiler/GHC/Core/Lint.hs),
[GHC's literal rules](../../../compiler/GHC/Builtin/WiredIn/TypeLits.hs)
and [research references](../REFERENCES.md#checking-a-reduction-across-an-interface).

Fresh GHC API processes replayed the character cases from the ordinary
producer's `.hi` alone. A rebuild with the same body preserved the
declaration hash. Changing one branch invalidated the old hash, even
when a reader inspected another branch. The check failed as expected
when the producer omitted its optional unfolding. Persistent evidence
therefore needs a producer declaration that supplies the checked
information under the supported compilation flags.

The next proposed interface test has three modules. An ordinary worker
changes one character and returns the default input unchanged. A second
module exports a `p3`-style signature over that worker. The third imports
the signature and discharges it at a literal with only interfaces
available. Inspect the produced nominal coercion with Core Lint, then
change the worker and require the unchanged third module to recheck.
GHC's built-in family prover is pure. A design that reuses it must load
and validate its input before checking the coercion endpoints. A private
certificate tied to the resolved producer declaration is one candidate.
The certificate representation and the coercion remain open.
