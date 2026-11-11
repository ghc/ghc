# Restricted dependent Core

This document specifies the first internal implementation. It is based on the
`nokinds-pi` binder and application work recorded in [PORT.md](PORT.md).
It does not specify the full dependent-Haskell language.

## Representation

Add two distinct type constructors:

```text
TermVarTy b       -- The Boolean index of a runtime identifier.
DepFunTy b result -- A retained dependent function.
```

`b` is a local non-coercion `Id`. Its type is `Bool`. Its multiplicity is `Many`.
It keeps its identity as a runtime variable. It is not a `TyVar` or a `CoVar`.
The term binder and every reference to its index use the same `Unique`.

`TermVarTy b` has kind `Bool`. Constructor indices use the existing promoted
`True` and `False` types. The result of a retained function has kind `Type`.
Dependent runtime representations are outside this subset.

Canonicalize a dependent function whose binder does not occur in its result to
an ordinary unrestricted Boolean arrow. This preserves one representation for a
vacuous retained binder. Lambda typing detects dependence from the result type.
No retainedness flag is added to every ordinary identifier.

## Scope and equality

Check `DepFunTy b result` under a context extended by the runtime binder `b`.
Check every `TermVarTy` occurrence against its binding-site identifier and type.
Free-variable collection includes the runtime index and its type dependencies.
It removes `b` under the dependent function binder. Index occurrences are erased
dependencies. They are not additional runtime uses for linearity analysis.

Compare runtime index variables by scoped identity. Compare dependent functions
modulo alpha-renaming of their binders. Do not inspect identifier unfoldings to
equate separately bound Boolean producers.

Existing type variables, coercion variables, and their substitutions keep their
current meanings. The new forms must use separate mapper and folder callbacks.

## Lambda and application

If a runtime lambda's body type mentions its Boolean binder, its type is the
canonical retained dependent function. Otherwise its type is an ordinary arrow.

For application of `DepFunTy b result`, first check the runtime argument against
`Bool`. Then substitute its index for `b` in the result. The initial accepted
arguments are a valid local Boolean identifier and the exact nullary Boolean
constructor workers. Constructor workers map to their corresponding promoted
constructors. A bare variable remains opaque even if its unfolding is a Boolean
constructor.

Type computation and Core Lint must perform the same instantiation. A wrong
argument type, unsupported expression, or escaping index must fail explicitly.
Do not silently discard the runtime argument or allocate an unrelated index.

## Substitution

Type and coercion substitution must account for the existing `IdSubstEnv` when
types can contain runtime indices. An empty type/coercion environment alone does
not justify skipping substitution.

Entering a dependent binder removes its outer substitution and renames it to
avoid capture. Extend the expression environment with the renamed runtime binder
when required. Every occurrence in the result follows that renaming.

Substitution of a Boolean variable or constructor into an index produces the
corresponding index. Substitution of any other expression must fail explicitly
in the initial experiment. It must not leave a stale variable behind.

`composeTCvSubst` composes type and coercion environments. It also composes
supported Boolean index mappings in the expression environment. It preserves
apply-once substitution and the second environment's priority on shared domains.
It does not compose arbitrary Core expressions. Such expressions cannot replace
native indices in this increment.

This restricted grammar is not closed under general Core substitution. The
experiment therefore cannot enter the ordinary optimizer or backend yet.
Preserving lazy binding boundaries or adding an internal expression-index form
is required before those passes can operate on arbitrary Boolean producers.
The compiler frontend cannot emit these forms in the first increment.

## Evidence and dependent cases

The existing `Case` result annotation remains outside its case binder's scope.
The first increment does not change this rule. It does not implement `choose`.

The next increment must add an explicit motive or checked branch-scoped nominal
evidence. Constructor discrimination must justify the refinement. Bottom does
not justify constructor refinement. No `UnivCo` or plugin axiom is permitted to
substitute for the missing rule.

Non-reflexive coercions over retained dependent functions require their own
congruence rule. Existing `mkPiCo` must not turn a retained dependent binder into
an ordinary function coercion. Reject unsupported coercion construction.

## Initial validation boundary

Check hand-built Core for retained identity and application. Cover alpha-renaming,
capture avoidance, variable and constructor substitution, non-Boolean indices,
wrong applications, and escaped variables. Compare Lint results with type recovery.

The native forms must print in diagnostics. Interface serialization, source syntax,
optimization, dependent cases, and execution remain gated until their plan tasks
pass. Existing ordinary GHC programs must continue to build.
