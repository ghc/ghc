# Historical dependent Core port inventory

Audit date: 2026-11-11. Current compiler baseline:
`19ff36f2aede0ca2e554016ff22c71e73aa9803b`. This file completes the source inventory
for M1 in [the plan](PLAN.md). It does not specify or implement the M2 calculus.
See [the references](REFERENCES.md) for the later D/DC, roles, and usage research.

## Provenance and retained source

The historical source is Richard Eisenberg's `goldfirere/ghc` fork, branch
`nokinds-pi`, pinned at
[`b64ef80f8002727422c05092f429c38d0527ed0b`](https://github.com/goldfirere/ghc/commit/b64ef80f8002727422c05092f429c38d0527ed0b).
The author is Richard Eisenberg. The commit date is 2014-08-19. The local reference
is `codex/reference-nokinds-pi`; development remains on current GHC.

Recreate the reference without checking out the old compiler or its submodules:

```sh
git fetch --no-tags --recurse-submodules=no https://github.com/goldfirere/ghc.git \
  b64ef80f8002727422c05092f429c38d0527ed0b:refs/heads/codex/reference-nokinds-pi
git rev-parse codex/reference-nokinds-pi
```

The initial fetch retained the main Git history. Automatic submodule recursion
could not retrieve some ancient submodule commits. Current submodule checkouts
stayed unchanged. We did not build the historical compiler. Use the command above
to avoid submodule recursion in future source audits.

The merge base with the current compiler baseline is
`2070a8f3565babed12942d2fee517421ee776ad6`, dated 2014-07-25. That common ancestry
does not make the subsequent experimental patches compatible with current GHC.

## Commit range

The bounded retained-Pi range is
`2b381118919585d104a783b1b6e95b41085012de..b64ef80f8002727422c05092f429c38d0527ed0b`.
Its four commits change 54 paths, with 2,926 insertions and 1,831 deletions. These
counts include generated documentation, a library change, and a submodule pointer.
They are not an estimate of the new implementation's size.

| Commit | Date | Contents and port treatment |
| --- | --- | --- |
| [`2b381118919585d104a783b1b6e95b41085012de`](https://github.com/goldfirere/ghc/commit/2b381118919585d104a783b1b6e95b41085012de) | 2014-08-13 | Pre-range checkpoint, titled “Stage 1 compiles.” It belongs to the earlier type/kind work. It does not validate the later retained-Pi changes. |
| [`5dfaaab45e11bb13a18f695af451c94e77a105be`](https://github.com/goldfirere/ghc/commit/5dfaaab45e11bb13a18f695af451c94e77a105be) | 2014-08-18 | “Some stuff toward adding Pi.” Introduces binder attributes and changes type representation, coercions, unification, Lint, and the written Core specification. This is the main design source. |
| [`1582b8692329b10ce1f87e4d9bd6be5d19737d29`](https://github.com/goldfirere/ghc/commit/1582b8692329b10ce1f87e4d9bd6be5d19737d29) | 2014-08-18 | “Re-apply old changes to base.” Adds four lines to the old Typeable implementation. Reassess the purpose against current Typeable; do not copy it as retained-function support. |
| [`2dc8ec73de26a222f2faa4450d1093c2168c878a`](https://github.com/goldfirere/ghc/commit/2dc8ec73de26a222f2faa4450d1093c2168c878a) | 2014-08-18 | “Changes to haddock.” Updates a submodule pointer. The historical Haddock revision is outside this source audit. |
| [`b64ef80f8002727422c05092f429c38d0527ed0b`](https://github.com/goldfirere/ghc/commit/b64ef80f8002727422c05092f429c38d0527ed0b) | 2014-08-19 | “More work toward Pi.” Continues binder, coercion, instantiation, checking, and zonking changes. The final tree remains incomplete. |

Inspect the range locally:

```sh
git log --reverse --format=fuller \
  2b381118919585d104a783b1b6e95b41085012de..codex/reference-nokinds-pi
git diff --stat 2b381118919585d104a783b1b6e95b41085012de..codex/reference-nokinds-pi
git show codex/reference-nokinds-pi:compiler/coreSyn/CoreLint.lhs
```

Earlier `nokinds` history contains promotion, type/kind unification, kind casts,
and coercion work. Current GHC already supplies these facilities through later
implementations. We did not establish patch-by-patch equivalence for that earlier
history. Retain its provenance, but port a required semantic change against current
code rather than replaying all earlier commits.

## Module mapping and current overlap

This table identifies responsibilities. Several old modules split into multiple
current modules, so the mapping is not a file-rename recipe.

| Historical path | Current path or area | Reuse and adaptation |
| --- | --- | --- |
| `compiler/basicTypes/Var.lhs`, `Id.lhs`, `VarEnv.lhs` | `compiler/GHC/Types/Var.hs`, `Id.hs`, `Var/Env.hs` | Reuse the distinction between dependence, visibility, and relevance. Current `ForAllTyFlag` controls visibility; it does not encode retained dependence. Preserve current multiplicity handling. |
| `compiler/types/TyCoRep.lhs` | `compiler/GHC/Core/TyCo/{Rep,Make,Subst,FVs,Compare,Ppr,Tidy}.hs` | Old `PiTy` and binder invariants provide design input. Current `ForAllTy`, `FunTy`, coercion invariants, and optimized constructors must remain consistent. |
| `compiler/types/Type.lhs`, `Kind.lhs` | `compiler/GHC/Core/Type.hs` and `Core/TyCo/` | Current GHC already unifies types and kinds. Add retained application and index handling to current result/kind computation; do not restore old levity rules. |
| `compiler/types/Coercion.lhs`, `OptCoercion.lhs`, `Unify.lhs` | `compiler/GHC/Core/{Coercion,Unify}.hs`, `Core/Coercion/Opt.hs`, `Core/TyCo/` | Old Pi-coercion cases are incomplete design work. Reconcile any replacement with current roles, casts, and equality rather than copying the old coercion calculus. |
| `compiler/coreSyn/CoreLint.lhs`, `CoreUtils.lhs` | `compiler/GHC/Core/{Lint,Utils}.hs` | Port checked lambda/application rules and type computation together. Current Lint also checks usage and runtime-representation invariants. |
| `compiler/coreSyn/CoreSubst.lhs`, `CoreFVs.lhs` | `compiler/GHC/Core/{Subst,FVs}.hs`, `Core/TyCo/{Subst,FVs}.hs` | These historical files expose missing dependency handling even though they are outside the four-commit diff. Both term and type traversal need capture-safe dependent substitution. |
| `compiler/coreSyn/TrieMap.lhs` | `compiler/GHC/Core/Map/{Expr,Type}.hs` | Extend structural keys only after the index syntax and equality rules are fixed. |
| `compiler/typecheck/TcType.lhs`, `Inst.lhs` | `compiler/GHC/Tc/Utils/{TcType,Instantiate}.hs` | Historical phase annotations expose intended dependence checks. Current required-forall instantiation already exists; retained instantiation remains new work. |
| `compiler/typecheck/TcExpr.lhs`, `TcMatches.lhs`, `TcHsType.lhs` | `compiler/GHC/Tc/Gen/{Expr,App,Match,HsType}.hs` | Preserve the current application checker and type-abstraction work. Frontend retained binders come after the Core rules. |
| `compiler/typecheck/TcHsSyn.lhs`, `TcEvidence.lhs` | `compiler/GHC/Tc/Zonk/`, `compiler/GHC/Tc/Types/Evidence.hs` | Reuse current zonking and evidence APIs. Audit new index occurrences and binder scopes explicitly. |
| `compiler/typecheck/TcCanonical.lhs`, `TcInteract.lhs`, `TcSMonad.lhs` | `compiler/GHC/Tc/Solver/{Equality,Rewrite,Solve,Monad}.hs` | The solver has changed substantially. General inference and new theories are outside the initial Core port. |
| `compiler/iface/IfaceType.lhs`, `IfaceSyn.lhs`, `MkIface.lhs`, `TcIface.lhs` | `compiler/GHC/Iface/{Type,Syntax,Make}.hs`, `GHC/{CoreToIface,IfaceToCore}.hs` | A new binder/index form needs current serialization, fingerprints, and scope handling. The four-commit range does not supply a complete interface implementation. |
| `compiler/deSugar/DsBinds.lhs`, `DsUtils.lhs` | `compiler/GHC/HsToCore/{Binds,Utils}.hs` | Adapt elaboration only after hand-built dependent Core passes Lint. |
| `compiler/simplCore/SimplUtils.lhs` | `compiler/GHC/Core/Opt/Simplify/` | The historical change is not a complete optimizer audit. Check substitution, beta/eta rules, cases, joins, and floating separately. |
| `compiler/codeGen/StgCmmClosure.hs` | `compiler/GHC/StgToCmm/Closure.hs` | Reassess calling-convention assumptions. The first retained Boolean must lower to an ordinary runtime argument. |
| `compiler/vectorise/` | No direct current subsystem | Do not port obsolete vectorizer adaptations. |
| `docs/core-spec/{CoreSyn,CoreLint}.ott`, `core-spec.mng` | Same documentation area | Preserve the requirement to document typing rules. Ott output is not a mechanized proof of GHC. |

## Concrete missing rules

1. **Term indices.** The old
   [`Type` grammar](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/types/TyCoRep.lhs#L143)
   has `PiTy` but no general runtime-expression constructor. The old
   [`Id` dependence flag](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/basicTypes/Var.lhs#L189)
   does not itself define term embedding, kinding, or substitution closure.
   M2 must settle those rules before code porting.
2. **Application agreement.** The old
   [`lintValApp`](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L528)
   calls `promoteExpr_maybe` and substitutes its result. A full compiler-tree search
   finds that helper only at this call. In contrast,
   [`applyTypeToArg` and `applyTypeToArgs`](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreUtils.lhs#L157)
   still discard runtime arguments when computing application types. Both the
   embedding and the matching type-computation rule are missing.
3. **Binder checks.** The old
   [`checkBinderInvariants`](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L695)
   calls `isDependentId`, which also has no definition in the compiler tree.
   That function body also contains unresolved `binder` and `bnder` identifiers.
   These source defects are direct evidence of an unfinished final tree. We did
   not compile it to enumerate all errors.
4. **Dependent cases.** The old
   [case checker](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L396)
   passes one unchanged result type to every branch. It supplies no dependent
   motive and no general Boolean constructor-refinement evidence. Current Core has
   the same fixed-result boundary. M2 must define the motive or explicit evidence;
   M4 must check branch instantiation and case-result scope.
5. **Substitution and dependencies.** The old
   [case substitution](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreSubst.lhs#L408)
   substitutes the result type outside the case binder's scope. Current
   [case substitution](../../compiler/GHC/Core/Subst.hs) and
   [free-variable collection](../../compiler/GHC/Core/FVs.hs) retain that boundary.
   A source restriction to Boolean variables and constructors does not remain
   closed when inlining replaces a variable with an arbitrary expression.
6. **Equality and erasure.** Old Pi-coercion changes do not establish sound roles,
   multiplicities, or optimization for the new form. Current
   [`TyCo.Rep`](../../compiler/GHC/Core/TyCo/Rep.hs) still marks homogeneous primitive
   equality as future work. The later D/DC proof cannot be transferred to current
   heterogeneous FC without an explicit account of the differences. No historical
   build or executable establishes the required lazy retained-Boolean behavior.

## Port units and validation boundary

Use the historical commits as provenance for small current-GHC changes. Record the
specific source commit and the adaptations in each port description. Do not copy
the obsolete compiler, library layout, generated PDF, or submodule pointers into
the development base.

The next implementation units remain M2 through M8 in PLAN.md: restricted rules;
binders and dependent application; dependent cases and evidence; erasure/backend;
bounded optimizer support; frontend; and interfaces. Resolve each milestone's
specified rules and validation gate before enabling the dependent behavior.

The retained [singleton comparison programs](reference/README.md) provide executable
evidence for selected values, shared indices, rejection, and strictness. Their
documented recipe uses the current baseline compiler and Core Lint. They establish
neither native Core support nor full singletons-package compatibility.
