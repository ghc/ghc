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

## Current experiment

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
- [ ] During source type checking, track which inference variable blocked
      a necessary observation. Retry that observation when unification
      assigns the variable. A rigid skolem has no such assignment.
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
their interpretation as symbolic Core values. The next source milestone
needs a typing judgment that can wait for such a result, a solver wakeup
for the demanded variable, and a source error when the check stays stuck.
GHC already records unification and revisits affected constraints in
`kickOutAfterUnification`. A reduction that survives into FC also needs
checked nominal evidence for its endpoints.
[GHC solver](../../../compiler/GHC/Tc/Solver/Monad.hs),
[Eisenberg's Core Lint](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L498-L548)
and [Agda blockers](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/Syntax/Internal/Blockers.hs#L26).
