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

## Roadmap review, 2026-11-15

The [whole-program roadmap](../ROADMAP.md) prompted additional source reading.
The seven-work manifest above preserves the earlier implementation readings.
The additional downloads and extracts are in
`_build/verification/native-known-arguments-roadmap-research-v1/`.
Its `research-provenance.json`, `reading-scope.json`, and `SHA256SUMS` record
versions, source URLs, page counts, and hashes. These records distinguish new
reading from a fresh audit of an entire paper or its proofs.

- [Higher-Order Type-Level Programming in Haskell](https://simon.peytonjones.org/assets/pdfs/higher-order-type.pdf),
  ICFP 2019, DOI `10.1145/3341706`. The 26-page author PDF was fetched.
  Read the abstract, introductory excerpts, and sections 2.1–2.4 on PDF pages
  3–4. Matchability controls when equality can decompose an application.
  Functions and type families do not inherit algebraic-constructor decomposition.
  The later calculus, algorithm, and proofs were not audited.
- [An Existential Crisis Resolved: Type Inference for First-Class Existential Types](https://richarde.dev/papers/2021/exists/exists.pdf),
  ICFP 2021, DOI `10.1145/3473569`. The 58-page author document was fetched.
  Read PDF pages 1–3, including the vector filter and lazy projection examples.
  An inserted strict package match can change productivity. The full inference
  algorithm and appendices were not audited.
- [Constrained Type Families](https://richarde.dev/papers/2017/partiality/partiality-extended.pdf),
  ICFP 2017, DOI `10.1145/3110286`. The 28-page author PDF was fetched.
  Read the abstract and introduction on PDF pages 1–2. Its domain predicates
  address partial type computations. Ordinary qualified native functions do
  not implement this feature. The remaining calculus and proofs were not read.
- [The extended graded paper](https://richarde.dev/papers/2021/grad/grad-extended.pdf),
  39 PDF pages. Read selected introduction passages on pages 1–3, the Pi/Sigma
  rules on pages 19–20, conversion and irrelevance discussion on pages 27–28,
  and Appendix B on pages 38–39. The appendix requires an explicit encoding for
  an efficient Core checker. It does not supply a combined FC/roles/usage
  implementation theorem. The existing 32-page version remains unchanged.

The review also reread selected D/DC passages on PDF pages 3, 7, 9, 12–14 and
27, and Eisenberg's thesis at printed pages 68–71. DC checks explicit evidence
for conversion; arbitrary definitional equivalence is undecidable. Neither
paper transfers a preservation theorem to this fork without a correspondence
argument.

[Gundry's thesis landing page and errata](https://adam.gundry.co.uk/pub/thesis/)
were read. The 2014 errata correct a higher-order pattern-unification rule and
withdraw the original generality lemma. The thesis PDF was not read in this
review. Treat its original algorithm as research to check, rather than an
unchanged implementation recipe.

## C1 comparison: opaque runtime function indices

This comparison extends the roadmap reading above. It does not
change the canonical seven-work, nine-PDF manifest. The 26-page author version
of [Higher-Order Type-Level Programming in Haskell](https://simon.peytonjones.org/assets/pdfs/higher-order-type.pdf)
was reread on selected pages 3–4, 6–7, 9, 11 and 13–16. The reading covers
matchability, application congruence, restricted equality projections, and
substitution and consistency obligations. No proof was replayed.

The comparison also checks the D/DC 30-page author version on pages 16–17 and
19, and the System DR 29-page conference version on pages 8, 11 and 18.
Pi-type injectivity is not injectivity of an arbitrary runtime function.
The pinned proposal #378 and singletons source supply design and implementation
comparisons. Singletons uses a saturated noninjective `Apply` family; its
arbitrary-function `toSing` implementation uses `unsafeCoerce`.

A saturated family with a checked non-arrow function token is a possible safe
direction. M42 selects a dedicated residual-call node for a bounded Core
kernel. The token/family alternative remains research. Keep scalar domains, function-head
eligibility and retained function-binder domains separate. No general evaluation
or preservation result follows. See
[the detailed comparison](../REFERENCES.md#opaque-runtime-function-indices-unimplemented-c1-comparison)
and the exact versions, hashes and reading scope in
`_build/verification/native-next-core-function-indices-v1/`.

## M42 operational source reading

Agda v2.8.0 supplies the neutral-head precedent. Selected ranges in
`Syntax/Internal.hs`221–252 and `TypeChecking/Substitute.hs`827–852 describe
a variable with eliminations and scoped replacement of its head.
The exact blobs, SHA256 values, read ranges and reuse provenance are recorded
in `_build/verification/native-neutral-calls-research-v1/provenance.json`.
The cache contains six full Agda source files. Full-file hashes do not imply
full-file reading or an Agda execution.

This reading supports M42's scoped call representation and substitution.
D/DC application congruence and System DR nominal argument rules remain
separate equality precedents. No inverse call projection or proof of this
GHC implementation follows. The seven-work, nine-PDF ledger is unchanged.
See [the source references](../REFERENCES.md#opaque-native-calls-representation-substitution-and-nominal-congruence).

## M43 source-refinement reading

M43 implements checked opaque-call refinement through a strict case alias.
Agda v2.8.0 provides selected context/generalization readings in With.hs 101–173,
the with-abstraction manual 710–741 and 910–936, LHS 1503–1532, and
Application 774–808. With.hs was fetched and read on 2026-11-15. Its Git blob
`fb84d352f3c6b2e786a0c095c74c5bee15e533cf` matches the earlier reference.
The other three files reuse verified cached bytes. Full sources, hashes,
access/reuse UTC and selected ranges are in
`_build/verification/native-source-neutral-calls-prior-art-v1/`.

This reading supports abstraction and checked substitution. Haskell's strict
case demand, administrative lazy patterns and `seq` identity require separate
implementation and runtime checks. No Agda execution or preservation theorem
is claimed. The canonical seven-work, nine-PDF ledger is unchanged.
See [the primary links and exact identities](../REFERENCES.md#strict-opaque-call-refinement-prior-art)
and the [implemented source contract](../CORE.md#strict-opaque-call-refinement-from-source).
