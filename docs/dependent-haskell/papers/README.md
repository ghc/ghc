# Dependent Haskell paper reading

Read on 2026-11-14. The [manifest](manifest.json) records seven works and nine
downloaded PDF versions. It contains source URLs, checksums, page counts and
the exact reading scope. PDF copies and extraction files are local in
`_build/verification/dependent-haskell-papers-2026-11-14/`.

Page references below identify the downloaded version. Author drafts can have
different pagination from the publisher record. The thesis was read in selected
sections. No Coq development was rebuilt for this reading.

| Work | Publication | Use in this project |
| --- | --- | --- |
| [Dependently Typed Programming with Singletons](https://richarde.dev/papers/2012/singletons/paper.pdf) | Eisenberg and Weirich, Haskell 2012 | Comparison encoding, promotion and the original native-Pi alternative |
| [System FC with Explicit Kind Equality](https://richarde.dev/papers/2013/fckinds/fckinds.pdf) | Weirich, Hsu and Eisenberg, ICFP 2013 | Explicit kind evidence and scoped congruence |
| [Dependent Types in Haskell: Theory and Practice](https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf) | Eisenberg, PhD thesis, 2016; supervised by Weirich | Binder properties, Pico and Bake |
| [A Specification for Dependent Types in Haskell](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf) | Weirich, Voizard, de Amorim and Eisenberg, ICFP 2017 | D/DC, equality assumptions, erasure and nontermination |
| [A Role for Dependent Types in Haskell](https://richarde.dev/papers/2019/dep-roles/dep-roles.pdf) | Weirich, Choudhury, Voizard and Eisenberg, ICFP 2019 | Nominal and representational dependent equality |
| [A Quick Look at Impredicativity](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf) | Serrano, Hage, Peyton Jones and Vytiniotis, ICFP 2020 | Instantiation, known variables and ordinary final checking |
| [A Graded Dependent Type System with a Usage-Aware Semantics](https://richarde.dev/papers/2021/grad/grad.pdf) | Choudhury, Eades, Eisenberg and Weirich, POPL 2021 | Runtime usage, erasure and the limits of resource claims |

## Singletons, Haskell 2012

Read the introduction, relevant parts of sections 2 and 7, and the conclusion.
The author link serves a 13-page submission draft dated 2012-07-23. A second
[university-hosted copy](https://www.cs.tufts.edu/~nr/cs257/archive/richard-eisenberg/dep-singleton.pdf)
uses printed pages 117–129. The institutional citation gives 117–130. The
manifest keeps these records separate.

Sections 1–2 connect runtime singleton witnesses with promoted indices and
type-family computation (draft pages 2–4; mirror pages 118–120). Pattern matching
on a witness refines the type. Promotion and singleton generation remain
different operations. Section 7 explicitly considers both singleton translation
of Pi types and adding Pi types directly to FC (draft page 12; mirror page 128).
The same page distinguishes richer type information from termination guarantees.

Keep `reference/` as the executable comparison for the overlapping subset.
Our native implementation uses an existing runtime Id as the index. It does not
insert a singleton conversion at that call. The paper's 2012 library API is
historical; it is not a claim about the current package API.

## Kind equality, ICFP 2013

Read all 12 pages and inspect Figure 4 on PDF page 6. The extended proofs were
not read. Types and kinds share syntax, but runtime expressions remain separate.
Section 8 leaves direct expressions in types for future work.

The heterogeneous `CT_ALLT` rule checks separate endpoint scopes and transports
the bound variable. The simpler same-domain `CT_ALLTX` is described in section
4.3.1 (PDF pages 6–7). Section 6 gives sufficient consistency conditions for
progress (pages 9–10). These conditions constrain axioms and contexts.

Our fixed-domain `DepFunCo` follows the simpler congruence shape. It does not
implement general domain conversion. The later `nokinds-pi` branch is a distinct
implementation experiment. Neither the paper nor that unfinished branch proves
our runtime-index extension correct.

## Pico and Bake, thesis 2016

Read 42 numbered pages and nine title/contents pages from the 351-page PDF.
The manifest lists all ranges. Printed Arabic page `n` is PDF page `n + 15`.
The appendices were not read. Weirich is the supervisor, not a coauthor.

Section 4.2 separates dependency, relevance, visibility and matchability
(printed pages 54–59). Sections 3.3.3 and 4.4 explain partiality and proof demand
(48–49, 63–65). Pico binding congruence checks a scoped body and needs endpoint
transport for changed domains (5.8.5.1, pages 107 and 109). Bake uses bidirectional
constraints and touchability (6.3–6.4, pages 150–154). Its application rule
synthesizes the function before checking arguments (6.6.1, pages 160–161).

Our visible, retained `Many` binder selects a restricted combination of these
properties. `Many` is an additional restriction. Our immutable matching and
expected-result selection are adaptations. They do not implement all of Bake.
Section 4.4.4 treats dependency-containing impredicative instantiation as a
restriction and discusses a possible visibility-based relaxation. It does not
justify removing our mutable native-meta guard.

## D and DC, ICFP 2017

Read sections 1–9, author-PDF pages 1–27, including the main rules, theorem
statements and proof explanations. Inspect Figures 8 and 10. The downloaded
author PDF has 30 pages; the ACM record gives 29. These page systems were not
assumed to coincide.

D shares term/type syntax. DC adds explicit coercions and decidable checking.
Both accommodate nontermination. Preservation permits inconsistent assumptions.
D progress excludes coercion assumptions; DC progress permits only irrelevant
term assumptions (sections 4–5, author pages 12–14 and 20). Section 6.3 does not
permit complete erasure of coercion abstraction (page 22). Section 9 leaves
primitive datatypes, pattern matching, roles and levity for later work (page 27).

GHC already retains coercion-lambda arity with zero-width tokens. See Note
`[Coercion tokens]` in `GHC.CoreToStg`. Native alternative lowering removes an
extra virtual proof binder while retaining the strict case and real fields.
Core Lint checks branch identity, exact nominal endpoints and local proof scope.
The M32 Given filter is a separate algorithm from the paper's available-evidence
set. The paper's erasure theorem does not certify our lowering, `seq` or sharing.

## Roles, ICFP 2019

Read the introduction and sections 3–7 of the 29-page conference version.
Read selected rules on pages 30 and 37–38 of the 42-page arXiv v1 extended
version. Inspect the appendix rule figures. Other appendix proofs were not
audited. Conference printed page `101:n` is PDF page `n`.

Nominal equality can weaken to representational equality. Unknown-argument
congruence and constructor discrimination need nominal equality (sections 3.1
and 4.3, pages 10–11 and 17). Pi congruence checks the body under the binder
(Figure 2, page 8). The full rules require well-formed endpoints and available
evidence (extended pages 37–38). System DR is Curry-style. An annotated
implementation language remains future work (section 7, page 25).

Keep the `Coercible` rejection and the role of every `DepFunCo` body proof.
The paper does not prove our deep free-Id filter or cache policy. Its coercion
quantifier is not an ordinary Haskell class dictionary. Source type classes are
outside its calculus (section 3, page 7). It therefore does not directly justify
admitting the next qualified-Pi source examples.

## Quick Look, ICFP 2020

Read selected sections on checking, application, instantiation, qualified types
and implementation. The manifest lists the exact pages. The author PDF has
33 pages and reports January 2026 corrections. The published 2020 citation gives
29 pages. These versions are not assumed to be identical. The appendices were
not read and the proofs were not replayed.

The `h-var` rule retrieves the existing variable type (Figure 3, printed page
89:7). With no leading quantifiers, `iresult` preserves that type for an empty
argument list (Figures 4 and 8, pages 89:8 and 89:20). Final checking still emits
result equality (Figure 7, page 89:19). An empty argument list alone is not
sufficient: a leading `forall` still needs instantiation. The empty-list example
uses the expected result to select that instantiation (sections 3.1 and 5.5,
pages 89:5 and 89:14). Qualified types retain constraint entailment and local
assumptions (section 8, pages 89:21–22). Automatic eta expansion changes lazy
program semantics (section 5.8, page 89:15).

With `ImpredicativeTypes`, our fork uses `NoQL` for an actual variable with no
source arguments and no leading `forall` or context. The actual type or an
available expected type must contain native syntax. The types must have no
free QL instantiation variables. This route supports checking and inference
from an existing variable type, including a proof case scrutinee. It preserves
the ordinary `InferResult` path, final subsumption and native evidence scope.
It does not eta-expand the variable. See Note
`[Known native variables with ImpredicativeTypes]` in `GHC.Tc.Gen.App`.

This is a restricted GHC adaptation. It adds no general Quick Look inference
for native types and does not permit native types in mutable type metavariables.
The paper's soundness theorem covers its own language (section 7.2, page 89:21).
It does not certify our runtime indices, `DepFunTy` or `DepFunCo`.

## GraD, POPL 2021

Read selected sections on heap reduction, irrelevance, dependent typing,
soundness and Haskell integration. Exact pages are in the manifest. Inspect
Figure 3 on PDF page 20. The extended version and Coq artifact were not read
or rebuilt in this increment. Printed page `50:n` is PDF page `n`.

The Pi rule separates runtime argument usage from use in the codomain
(section 7, pages 19–21). Type checking does not add those uses to the runtime
count. Resource results need stated semiring conditions (section 6, pages
17–18). The heap semantics uses call by name (section 5, pages 9–11).
Section 9.2 limits the proposed multiplicity-polymorphic extension to relevant
quantities to keep the calling convention static (page 27).

This supports treating relevance and runtime usage as separate design questions.
It does not justify linear retained binders in our fork. Its heap theorem is
not a proof of GHC's call-by-need behavior, cyclic heaps or `StableName` results.
The existing [artifact review](../REFERENCES.md#roles-and-graded-usage) records
which syntactic properties are mechanized. Runtime observations remain separate
implementation evidence.

## Next reading

Follow the primary bibliography and keep unread work marked as queued:

- [Visible Type Application](https://richarde.dev/papers/2016/type-app/visible-type-app.pdf),
  ESOP 2016: specified arguments and source inference.
- [Kinds are Calling Conventions](https://richarde.dev/papers/2020/kinds-are-cc/kinds-are-cc.pdf),
  ICFP 2020: representation restrictions before broader retained domains.
- Oury and Swierstra, *The Power of Pi*, ICFP 2008: source examples that connect
  the singletons comparison with Agda.

These papers guide design choices. They do not establish a preservation theorem
for this GHC fork. See [the implementation plan](../PLAN.md) for the tested subset.
