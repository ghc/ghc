# Retained Boolean implementation plan

Implement a small singleton frontend for a retained Boolean argument. A source
function can use the argument at runtime and refer to its index in the result
type. The compiler will elaborate the supported forms into ordinary GADT code.
Existing GHC typechecking will check the generated equality evidence.

Last updated: 2026-11-11. Source baseline:
`19ff36f2aede0ca2e554016ff22c71e73aa9803b`.

See [current status](README.md) and [primary references](REFERENCES.md).

## Direction and boundaries

Follow `singletons` for the representation and occurrence translations. Follow
SHE for dependent binder syntax over a singleton argument. Keep Core unchanged
for this experiment. The first implementation uses an experimental opt-in flag;
its spelling is a local implementation choice, not a permanent extension name.

The first version supports one explicit, leading retained `Bool` binder on a
signed top-level function. Other arguments and the result have fixed lifted
runtime representations. The retained argument is unrestricted. Index expressions
are Boolean constructors or variables already backed by a retained witness.
Existing, fully applied type families can determine result types.

Support constructor, variable, and wildcard patterns for the retained argument.
Use explicit Boolean constructor branches for dependent elimination. Defer
nested retained binders, higher-order retained functions, dependent guards,
lazy retained patterns, automatic function promotion, and arbitrary index
expressions such as `choose (not b)`.

Start within one experimental module. Ordinary Haskell can call the generated
singleton interface. Source-level retained imports and exports need a later
interface design. The first version must expose its lowered interface honestly.

## Source behavior and target translation

The source examples are:

```haskell
type family Select (b :: Bool) where
  Select True = Int
  Select False = Bool

choose :: foreach (b :: Bool) -> Select b
choose True = 1
choose False = False

keep :: foreach (b :: Bool) -> Select b -> Select b
keep b x = x

again :: foreach (b :: Bool) -> Select b
again b = keep b (choose b)
```

The target is ordinary Haskell. The names below illustrate generated workers;
the implementation must resolve names and avoid collisions.

```haskell
chooseS :: forall b. SBool b -> Select b
chooseS STrue = 1
chooseS SFalse = False

keepS :: forall b. SBool b -> Select b -> Select b
keepS _ x = x

againS :: forall b. SBool b -> Select b
againS sb = keepS sb (chooseS sb)
```

Each retained source binder has one generated type index and one runtime
witness. The type index erases. The witness remains at runtime.

| Source occurrence | Translation |
| --- | --- |
| `b` in a type | The generated type variable |
| `b` in a retained argument position | The existing singleton witness |
| `True` or `False` in a retained argument position | `STrue` or `SFalse` |
| `b` in an ordinary term position | A shared, lazy `fromSing sb` binding |
| A dependent match on retained `b` | A match on `sb` with singleton constructors |

Matching a demoted `Bool` does not supply GADT refinement. Match the singleton
when the branch result depends on the index. Do not infer nominal equality from
representation equality. Use the existing GADT and type-family role rules.

## Runtime Boolean input

Ordinary runtime input enters through explicit `withSomeSing @Bool`. The
continuation scopes a fresh existential index. Every dependent use in that
scope must reuse the same witness.

```haskell
runtimeUse :: Bool -> String
runtimeUse flag =
  withSomeSing @Bool flag $ \sb ->
    case sb of
      STrue -> show (keepS sb (chooseS sb))
      SFalse -> show (keepS sb (chooseS sb))
```

This input is unknown at compile time. Its selected result is consumed inside
the reification scope. A bare result indexed by the fresh variable cannot escape
that scope. Returning it requires an existential package.

Reification is strict for `Bool`. Do not insert it eagerly at ordinary call
sites. A retained worker that ignores its witness must remain non-strict.
A singleton can be bottom even when its index is known. Do not replace an
arbitrary witness with a constructor solely because of its type index.

Two independent conversions of the same raw `Bool` introduce independent
indices. Do not assume those indices are equal. Reuse one conversion when the
program needs one shared index.

## Implementation milestones

### M0 Record the baseline and research

- [x] Update the source and synchronize recursive submodules.
- [x] Build the default compiler and run focused smoke checks.
- [x] Compare singleton elaboration with native dependent Core.
- [x] Check the singleton reference encoding at `-O0` and `-O2` with Core Lint.
- [x] Record the plan, references, validation results, and next task.

### M1 Define the source subset and reference translation

- [ ] Write down accepted and rejected source forms before parser changes.
- [ ] Retain a reproducible reference from the temporary oracle.
- [ ] Specify binder scope, shadowing, ordinary term use, and explicit singleton
  interoperation. Keep automatic promotion of ordinary `let` bindings out of scope.
- [ ] Give each acceptance check below a concrete source example and expected result.

Completion: the examples and expected translations describe the same subset.

### M2 Provide the singleton runtime support

- [ ] Reuse the foundational `Data.Singletons` runtime at a recorded revision.
- [ ] Provide the small Boolean adapter following `singletons-base`.
- [ ] Confine package compatibility changes to the experimental support package.
- [ ] Check `SingKind`, `fromSing`, `toSing`, and `withSomeSing` on this HEAD.

The direct module probe passed. Full package and TH generator compatibility are
separate checks. The compiler itself does not need the TH generation stack as a
dependency.

### M3 Parse gated retained Boolean binders

- [ ] Add opt-in syntax in `compiler/GHC/Parser/Lexer.x` and
  `compiler/GHC/Parser.y`.
- [ ] Represent the binder in `compiler/Language/Haskell/Syntax/Type.hs`.
- [ ] Update the source AST printers and traversals that handle the new form.
- [ ] Give unsupported binder forms a deliberate diagnostic.

Completion: supported syntax parses with the flag. Unsupported forms report a
source error. Existing syntax retains its meaning when the flag is disabled.

### M4 Elaborate the source into singleton code

- [ ] Use the existing renamer for name identity and scope.
- [ ] Add a small lowering pass before ordinary typechecking. Check the integration
  with `compiler/GHC/Rename/Module.hs` and `compiler/GHC/Tc/Module.hs`.
- [ ] Lower signatures to `forall` plus a singleton argument.
- [ ] Lower retained patterns, applications, and ordinary term occurrences.
- [ ] Share one index and one witness for each retained binder.
- [ ] Preserve source locations for diagnostics and display the lowered form for review.

Completion: `choose`, `keep`, and `again` typecheck through the existing GADT
checker. No new Core type constructor or trusted equality axiom is required.

### M5 Check reification and evaluation behavior

- [ ] Support explicit singleton interoperation at runtime Boolean boundaries.
- [ ] Reject raw ordinary `Bool` arguments that have no retained witness.
- [ ] Check index scope, witness reuse, and shadowing.
- [ ] Keep demotion lazy. Preserve non-strict retained functions.
- [ ] Reject escaping indices and unsupported lazy retained patterns.

Completion: a runtime input works through one explicit reification scope.
The laziness and rejection checks below have the expected results.

### M6 Validate the frontend and update the project status

- [ ] Run the complete acceptance set through the frontend at `-O0` and `-O2`.
- [ ] Run Core Lint on the generated code.
- [ ] Rebuild the affected compiler stage and run the relevant existing frontend tests.
- [ ] Compare behavior with the reference translation. Record allocation or extra
  case analysis if the translation introduces a material runtime cost.
- [ ] Update this plan and the project index with the revision, commands, results,
  and remaining restrictions.

Completion: the source examples execute correctly and the negative examples
fail for their intended reason. The reference encoding alone is not sufficient
to complete this milestone.

## Acceptance checks

| ID | Check | Required result |
| --- | --- | --- |
| B01 | `choose True` | `Int`, value `1` |
| B02 | `choose False` | `Bool`, value `False` |
| B03 | `keep True False` | Type error |
| B04 | `keep b (choose b)` under one retained binder | Compiles with one shared index |
| B05 | Runtime input through one explicit reification scope | Both Boolean inputs execute correctly |
| B06 | Dependent constructor branches | Each branch checks against its refined result type |
| B07 | A retained worker ignores a bottom witness | No extra evaluation of the witness |
| B08 | `withSomeSing @Bool undefined (\_ -> ())` | Bottom; explicit strict boundary |
| B09 | A retained variable shadows another binder | Distinct names and correct scope |
| B10 | An existential index escapes reification | Type error |
| B11 | Two independent reifications need the same index | No equality without evidence |
| B12 | A raw `Bool` or unsupported retained expression is supplied | Clear rejection |
| B13 | Lazy retained pattern or unsupported binder form | Clear rejection |
| B14 | The experimental flag is disabled | No unintended syntax change |
| B15 | `-O0` and `-O2` with Core Lint | Equal observable results; valid generated Core |

The temporary oracle covers parts of B01 through B08. The frontend still has to
pass the full set. Existing GHC evidence checking does not by itself prove that
the source translation preserves evaluation behavior.

## Later work

After this experiment, evaluate source-level imports and exports, aliases,
higher-order retained functions, additional index expressions, and other
retained kinds. Extend the subset only with corresponding translation rules
and acceptance checks.

Native dependent Core remains a later option. Its gates are runtime indices
and substitution, dependent elimination with checked evidence, and preservation
through interfaces and optimization. The historical `nokinds-pi` branch and
System D/DC, System DR, and GraD inform those gates. They do not supply a current
GHC implementation that combines all of them.

Thoralf and related solvers can automate later index equalities. The Boolean
experiment uses existing family reduction and GADT evidence. It does not need
an SMT dependency.
