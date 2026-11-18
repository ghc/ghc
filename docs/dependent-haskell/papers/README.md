# Dependent Haskell paper reading

Read on 2026-11-14 through 2026-11-18. The [manifest](manifest.json) records
twenty-four works and thirty downloaded PDF versions. It contains source URLs,
checksums, page counts, roadmap cards and
the exact reading scope. PDF copies and extraction files are local in
`_build/verification/dependent-haskell-papers-2026-11-14/`. The four new
observational-equality PDFs are in the sibling directory
`native-arrow-matchability-observational-papers-v1/`. The POPL 2014 Trellys PDF
and reading records are in `native-arrow-matchability-trellys-research-v1/`.
The ICFP 2014 lazy-refinement paper and selected-section notes are in
`native-forall-totality-laziness-research-v1/`.
The JFP 2022 ANF paper and selected-page records are in
`native-computed-retained/papers/`.
The OutsideIn(X) author draft and selected-page records are in
`native-let-conversion/`.

Page references below identify the downloaded version. Author drafts can have
different pagination from the publisher record. The thesis was read in selected
sections. No Coq development was rebuilt for this reading.
Historical checkpoint notes below preserve the ledger counts at those dates.
The counts above describe the current collection. A work can have several
versions and several selective readings.

| Work | Publication | Roadmap | Use in this project |
| --- | --- | --- | --- |
| [Dependently Typed Programming with Singletons](https://richarde.dev/papers/2012/singletons/paper.pdf) | Eisenberg and Weirich, Haskell 2012 | R03, R04, R05, R13 | Comparison encoding, promotion and the original native-Pi alternative |
| [System FC with Explicit Kind Equality](https://richarde.dev/papers/2013/fckinds/fckinds.pdf) | Weirich, Hsu and Eisenberg, ICFP 2013 | R05 | Explicit kind evidence and scoped congruence |
| [Dependent Types in Haskell: Theory and Practice](https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf) | Eisenberg, PhD thesis, 2016; supervised by Weirich | R03, R04, R05, R23 | Binder properties, Pico and Bake |
| [A Specification for Dependent Types in Haskell](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf) | Weirich, Voizard, de Amorim and Eisenberg, ICFP 2017 | R05 | D/DC, equality assumptions, erasure and nontermination |
| [A Role for Dependent Types in Haskell](https://richarde.dev/papers/2019/dep-roles/dep-roles.pdf) | Weirich, Choudhury, Voizard and Eisenberg, ICFP 2019 | R05 | Nominal and representational dependent equality |
| [A Quick Look at Impredicativity](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf) | Serrano, Hage, Peyton Jones and Vytiniotis, ICFP 2020 | R03 | Instantiation, known variables and ordinary final checking |
| [OutsideIn(X)](https://simon.peytonjones.org/assets/pdfs/outsideinx.pdf) | Vytiniotis, Peyton Jones, Schrijvers and Sulzmann, JFP 2011 | R05 | Solve simple constraints before implications and preserve local touchables. Conditional alias evidence is a project proposal. |
| [A Graded Dependent Type System with a Usage-Aware Semantics](https://richarde.dev/papers/2021/grad/grad.pdf) | Choudhury, Eades, Eisenberg and Weirich, POPL 2021 | R03, R05 | Runtime usage, erasure and the limits of resource claims |
| [Homotopy Type Theory](https://hott.github.io/book/hott-online-82-g578b85c.pdf) | The Univalent Foundations Program, 2013 | R03, R04, R05 | Dependent application, pairs and identity elimination |
| [Type Inference, Haskell and Dependent Types](https://adam.gundry.co.uk/pub/thesis/) | Gundry, PhD thesis, 2013 | R03, R05, R22 | Phase-indexed contexts, conversion and erasure |
| [An Existential Crisis Resolved](https://richarde.dev/papers/2021/exists/exists.pdf) | Eisenberg, Duboc, Weirich and Lee, ICFP 2021 | R04, R22 | First-class erased witnesses and lazy projection |
| [Higher-Order Type-Level Programming in Haskell](https://simon.peytonjones.org/assets/pdfs/higher-order-type.pdf) | Kiss, Field, Eisenbach and Peyton Jones, ICFP 2019 | R05, R13, R23 | Matchability and restricted inverse application |
| [Constrained Type Families](https://richarde.dev/papers/2017/partiality/partiality.pdf) | Morris and Eisenberg, ICFP 2017 | R07, R12, R24 | Family domains and partial computation |
| [Visible Type Application](https://richarde.dev/papers/2016/type-app/visible-type-app-extended.pdf) | Eisenberg, Weirich and Ahmed, ESOP 2016 | R03, R28, R32 | Specified quantifiers and source instantiation |
| [The Thoralf Plugin](https://richarde.dev/papers/2018/thoralf/thoralf.pdf) | Otwani and Eisenberg, Haskell 2018 | R05 | Solver translation and trusted equality evidence |
| [Promoting Functions to Type Families in Haskell](https://richarde.dev/papers/2014/promotion/promotion.pdf) | Eisenberg and Stolarek, Haskell 2014 | R13, R17, R18, R21 | Lambda lifting, declaration order and defunctionalization |
| [A Reflection on Types](https://richarde.dev/papers/2016/dynamic/dynamic.pdf) | Peyton Jones, Weirich, Eisenberg and Vytiniotis, WadlerFest 2016 | R03, R05 | Runtime type codes, equality and dynamic application |

| [Observational Equality, Now!](https://people.cs.nott.ac.uk/psztxa/publ/obseqnow.pdf) | Altenkirch, McBride and Swierstra, PLPV 2007 | R05, R23 | Explicit extensional equality and dependent transport. The later literature records a normalization-conjecture dependency. |
| [Observational Equality: Now for Good](https://pujet.fr/pdf/OTT_now_for_good.pdf) | Pujet and Tabareau, POPL 2022 | R05, R23 | TTobs, proof irrelevance and conversion metatheory. No proof was replayed. |
| [Implementing a Type Theory with Observational Equality, Using Normalisation by Evaluation](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.TYPES.2024.5) | Sirman, Lennon-Bertrand and Krishnaswami, TYPES 2024; published 2025 | R05, R23 | Implementation experience in Haskell. The object-language prototype deliberately uses inconsistent U:U. It is not dependent Haskell. |
| [Observational Equality Meets CIC](https://pujet.fr/pdf/toplas25_cicobs.pdf) | Pujet, Leray and Tabareau, TOPLAS 2025 | R05, R23 | Indexed transport and cast computation. The formalized conversion proof covers the natural-number fragment. |

| [Combining Proofs and Programs in a Dependently Typed Language](https://www.seas.upenn.edu/~sweirich/papers/popl14-trellys.pdf) | Casinghino, Sjöberg and Weirich, POPL 2014 | R05 | Logical and programmatic fragments. The core proves closed logical CBV termination. Open-term normalization, Zombie's richer feature set and lazy-Haskell totality are separate boundaries. |
| [Refinement Types For Haskell](https://simon.peytonjones.org/assets/pdfs/refinement-types-for-haskell.pdf) | Vazou, Seidel, Jhala, Vytiniotis and Peyton-Jones, ICFP 2014 | R05, optional assurance | Potential divergence, weak-head values, finite values and decreasing recursion metrics. No guarantee is transferred to this compiler. |
| [ANF preserves dependent types up to extensional equality](https://doi.org/10.1017/S0956796822000090) | Koronkevich, Rakow, Ahmed and Bowman, JFP 2022 | R05 | Dependency-preserving naming, continuation typing and the limits of a naive coercion translation. |

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

The 2026-11-14 reading covers all 12 main-paper pages and Figure 4 on PDF page 6.
Types and kinds share syntax, but runtime expressions remain separate.
Section 8 leaves direct expressions in types for future work.

The heterogeneous `CT_ALLT` rule checks separate endpoint scopes and transports
the bound variable. The simpler same-domain `CT_ALLTX` is described in section
4.3.1 (PDF pages 6–7). Section 6 gives sufficient consistency conditions for
progress (pages 9–10). These conditions constrain axioms and contexts.

Our fixed-domain `DepFunCo` follows the simpler congruence shape. It does not
implement general domain conversion. The later `nokinds-pi` branch is a distinct
implementation experiment. Neither the paper nor that unfinished branch proves
our runtime-index extension correct.

The 2026-11-16 reading adds the 25-page
[extended version](https://richarde.dev/papers/2013/fckinds/fckinds-extended.pdf).
Read selected text on PDF pages 5, 9 and 11, plus part of page 7.
Dependent lifting and aligned congruence inform R05 telescope transport.
The extended figures and appendix proofs were not audited. These rules do not
supply arbitrary runtime-index typing or Haskell sharing guarantees.

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
Figure 3 on PDF page 20. This 2026-11-14 reading used the 32-page version.
Printed page `50:n` is PDF page `n`. The Coq artifact was not rebuilt.

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

The 2026-11-16 reading adds selected text from the 39-page
[extended version](https://richarde.dev/papers/2021/grad/grad-extended.pdf):
PDF pages 2–3, 19–21 and 27–28. Application scales usage, and substitution
affects later contexts. Usage soundness needs the paper's resource semantics.
Its November 2020 author header and POPL 2021 publication identify different
dates. No new appendix audit or Coq replay is claimed.

## HoTT, book 2013

Read sections 1.4 and 1.6 on PDF pages 37–38 and 42–44, printed pages 25–26
and 30–32. Read selected parts of sections 1.11 and 1.12 on PDF pages 55–56
and 60–62, printed pages 43–44 and 48–50. The pinned book has 492 PDF pages.

These passages explain dependent application, dependent-pair elimination and
identity elimination. They guide R03, R04 and R05 formation and elimination
rules. They do not specify Haskell evaluation, usage, erasure or sharing.
Only these selected passages were read. No full-book or proof audit is claimed.

## Inference and dependency, thesis 2013

Read Gundry's PDF pages 108–109, 124, 127, 129–130 and 149–150, printed pages
96–97, 112, 115, 117–118 and 137–138. These are selected passages from sections
5.2, 6.2, 6.3, 6.5.4 and 6.6. The thesis has 286 PDF pages. Read the published
errata as a separate source.

Phase-indexed contexts, explicit coercions and erasure inform R03 and R05.
The existential comparison also informs R22. The errata qualify pruning and
rigid-occurrence reasoning. The reading does not validate the complete
metatheory or establish that this fork implements its inference system.

## First-class existentials, ICFP 2021

Read PDF pages 3, 6–8, 17 and 24–25 of the 58-page author document.
Printed coordinates use the article prefix `64:`. These include the opening
of section 3.1; not all of section 3 was read.

Strong existentials permit lazy projection of erased witnesses. Inserting a
strict package match can change productivity. Runtime dictionaries can still
require evaluation and sharing. This guides R22 and the comparison for R04.
Section 9.3 excludes relevant first components from the presented Core.
Retained `some` therefore needs a separate contract. The full algorithm and
appendix proofs were not audited. No native-Pi preservation result follows.

## Matchability, ICFP 2019

Read sections 1–3 on PDF pages 1–9; selected section 4 rules and discussion
on pages 9–16; section 5.1 on pages 17–18; and selected sections 7–8 on pages
22–24. The author PDF has 26 pages, numbered `102:1` through `102:26`.

Matchability restricts inverse application projection while preserving forward
congruence. This guides R23 and the R05 foundation for generic fields.
The paper's initial term-arrow convention differs from the later proposal and
prototype. Its consistency argument assumes restricted family axioms and
termination. No proof replay or current-GHC port correctness is established.

## Constrained families, ICFP 2017

Keep both author files. The 28-page
[main paper](https://richarde.dev/papers/2017/partiality/partiality.pdf)
was read at PDF pages 1–2 on 2026-11-15. The distinct 38-page
[extended paper](https://richarde.dev/papers/2017/partiality/partiality-extended.pdf)
was read selectively on pages 1–4, 7–16 and 20–25 on 2026-11-16.
The manifest preserves the narrower section-level descriptions.

Family-domain predicates constrain formation and separate proper types from
family-bearing pretypes. This informs R24 and its promotion dependencies R07
and R12. CFC omits kinds and source classes. The appendix was not fully read
or replayed. The extended file's January boilerplate is not publication evidence.
Neither author file is asserted to match publisher bytes.

## Visible application, ESOP 2016

Read the 75-page extended author PDF at pages 5–8, 16–18, 21 and 24–25,
plus the first 2,200 extracted characters of page 1. The published chapter
has pages 229–254 and DOI `10.1007/978-3-662-49498-1_10`.
These are separate edition and page records.

Specified and generalized quantifiers give visible instantiation a stable
meaning. The checking systems preserve known polymorphism. This guides R32,
R28 and our R03 source work. Visibility does not imply retention. The results
do not establish native Pi representation, coercions, Quick Look policy or
runtime lowering. Appendix proofs and rendered figures were not audited.

## Thoralf, Haskell 2018

Read body pages 1–12 of the 13-page author PDF, including sections 3.1–3.4,
4.1–4.5 and related work. The reference page was not substantively read.

Thoralf translates supported type constraints into SMT queries and supplies
trusted GHC equality evidence. This informs R05's solver boundary. It does not
provide runtime index formation or general evaluation. Its trusted translation
requires separate checks; SMT expressiveness does not establish support for
retained Pi, scoped runtime Ids or native opaque calls. No plugin execution,
SMT invocation, compatibility build or performance comparison ran for this
reading. The implementation review has its own pinned source records.

## Function promotion, Haskell 2014

Keep the 12-page June 30 author submission and the 21-page technical report.
The published paper has pages 95–106. Read main-PDF pages 3–8 and 11, plus
selected lines on pages 1 and 12. Read selected extended-PDF lines on pages
1 and 14–16, plus extracted page 21. Exact line ranges are in the manifest.

Lambda lifting and defunctionalization encode lambdas, lets, cases and partial
application as type families. This guides R13, R17, R18 and R21. The full
preservation claim depends on an unproved lifting result. It does not establish
arbitrary runtime-closure promotion or a general open evaluator. Publisher-byte
identity and a complete appendix audit are not claimed.

## Type reflection, WadlerFest 2016

Read author-PDF pages 6–12 and 14–17, plus part of page 18. This covers selected
sections 5.2–5.6, 5.8–5.9, 6–7 and the opening of section 8. The author PDF has
26 pages. Published pages 292–317 use a separate coordinate system.

Indexed runtime type codes, heterogeneous equality and dynamic application
inform the runtime-universe work within R03/R05. They do not establish native
Pi or runtime-index reflection. The June author-PDF creation metadata differs
from the March first-online date. Neither the full paper nor the publisher PDF
was read. A runtime universe bridge remains an implementation obligation.

## Next reading

Follow the primary bibliography and keep unread work marked as queued:

- [Kinds are Calling Conventions](https://richarde.dev/papers/2020/kinds-are-cc/kinds-are-cc.pdf),
  ICFP 2020: representation restrictions before broader retained domains.
- Oury and Swierstra, *The Power of Pi*, ICFP 2008: source examples that connect
  the singletons comparison with Agda.

These papers guide design choices. They do not establish a preservation theorem
for this GHC fork. See [the implementation plan](../PLAN.md) for the tested subset.

## Roadmap review, 2026-11-15

The [whole-program roadmap](../ROADMAP.md) prompted additional source reading.
The seven-work manifest at that checkpoint preserved the earlier readings.
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
- [Constrained Type Families](https://richarde.dev/papers/2017/partiality/partiality.pdf),
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

## M44 constructor-call reading and M45 signed beta

M44 reuses Agda v2.8.0 Application.hs lines 774–808 and With.hs lines 101–173. The selected ranges
support checking an argument and abstracting a dependent context. Verified full
bytes and exact identities are recorded in
`_build/verification/native-source-call-constructors-prior-art-v1/`.
The six GHC snapshots there retain their historical M43 source commit.

M45 uses selected Term.hs lines 468–486 and 542–587, plus Application.hs
lines 774–808, for explicitly signed immediate lambda arguments. The document
review also checks Term 491–494 for the target-telescope choice. The actual
checked binder must have assigned Many metadata. The source implementation
keeps the original runtime call and ordinary Strict fallback; it excludes
Strict from the new dependent-beta admission. These Agda readings do not prove
GHC multiplicity inference or Haskell demand.
See [the exact primary links](../REFERENCES.md#signed-source-call-beta-prior-art).
The canonical seven-work, nine-PDF ledger is unchanged. No new paper was read.
Agda was not executed. No preservation theorem is claimed.

## M46 head-only binder reading

M46 implements a bounded local-signature subset. Focused compiler/runtime
validation passes; it does not establish a theorem from these paper readings.
The review reuses Agda v2.8.0 Term 542–587 and
Application 774–808, and verifies Conversion 746–761/827–850 at blob
`69423e2c11b6f35bc5eee2d502cf2830efbc005a`. It checks expected-domain scope
and comparison beneath Pi binders. It also rereads selected cached D/DC
Figure 8 pages 16–17 and the existing matchability comparison. No new whole
paper reading, downloaded version or proof replay is added. The canonical
seven-work/nine-PDF manifest stays unchanged.

In types and coercions, the GHC binder occurs only in dedicated call-head
slots. Agda's general Pi rules and D/DC's binder congruence do not establish
GHC substitution, dictionary/FC scope, Haskell laziness or a codec. The new
research record is `native-source-call-beta-next-function-pi-v1/`.
See [the detailed comparison](../REFERENCES.md#head-only-function-valued-retained-binders-prior-art).

The implementation retains fixed-domain evidence and variable-only head
substitution. Actuals are already-bound unrestricted local function variables
of the exact checked type; they need no separate written local signature.
The retained worker still needs its complete local signature. Scalar indices,
constructor fields and call codecs keep their
separate gates. An ordinary nonreflexive multiplicity-family control found a
Phantom FunCo child-role mismatch. The upstream 19ff36f source and the measured
failure are recorded in native-function-binders-phantom-review-v1/; this is not
an additional conference-paper result.

The next-refinement study reuses pinned Agda With 101–173, Application 774–808,
LHS 1503–1532 and Conversion 746–761/827–850. It rereads D/DC Figure 8 and
Figure 10 An-Push on author-PDF page 19. These readings distinguish an exact
instantiated call from the worker's bound schema. At that study's snapshot, direct application inside
a call-refining case was unimplemented. The later two-stage probe2 reports
two GHC-83865 payload mismatches per stage at lines 33/37, with no runtime
result. The earlier static-study provenance is native-function-binders-next-refinement-v1/. No new work or PDF is added to the
canonical seven-work/nine-PDF manifest. No full-paper reading or proof replay
is claimed. See the [source comparison](../REFERENCES.md#head-only-function-valued-retained-binders-prior-art).


The later scope review is native-function-binders-next-refinement-scope-v1/.
It reuses those primary readings and inspects GHC's free-Id masking and
ordinary evidence-argument paths. Its M47 plan keeps the motive/alias Ids
distinct, carries an explicit equality and limits application transport to a
whole tau residual after actual substitution under NoQL. That static plan
precedes the simpler Given-only M47 implementation.
Neither Agda's contextual checking nor D/DC's cast rule proves the proposed
GHC scope, usage, token arity or Haskell demand behavior. The ledger is unchanged.

## M47 remembered equality

M47 adds no work or PDF to the canonical seven-work/nine-PDF manifest.
Selected Agda v2.8.0 manual lines 634–697 describe `with ... in eq`.
With.hs 175–215 implements the value/equality pair. The retained relation is
the relevant prior art when a later application exposes the original expression
again. See [the exact primary links and blobs](../REFERENCES.md#remembering-an-abstracted-call-prior-art).

GHC uses an ordinary motive equality argument and the existing row Given scope,
including `where` bindings. Its ordinary Wanted/Given path needs no custom App
cast or lexical proof list. Coercion tokens, evidence tracking and Haskell
demand remain GHC-specific obligations. The earlier failed custom variant is
historical evidence, not the selected algorithm. Bootstrap, self-host, API
and selected both-stage CLI checks pass. Final9 and its registered run pass.
Artifact audit and publication remain separate. No new full-paper reading,
Agda execution, proof replay or combined preservation theorem is claimed.
The Agda source provenance is
`native-call-application-refinement-remember-equality-prior-art-v1/`.

## M48 known-worker activation

M48 reuses the existing Agda Application 774–808, With 101–173 and remembered
`in eq` readings. It adds no paper, PDF or full-reading claim. The canonical
seven-work/nine-PDF manifest is unchanged.

The GHC step inspects checked worker metadata to choose an existing case path.
It does not perform Agda-style inference or execute source arguments. Its
leading-retained/whole-tau shape rules constrain this discovery only. Normal
application checking and M47's ordinary Given retain their existing roles.
Bootstrap fresh/freshwhere, quote and selected API1 controls pass. Public
annotations are removed and ten quiet values pass with unchanged goldens.
Self-hosting, selected both-stage source controls and public17 pass.
Legacy, constructor, final9 and registered checks pass. Artifact audit and
publication remain separate. These focused checks do not establish a theorem
or a full GHC suite result. See [the source comparison](../REFERENCES.md#known-worker-case-activation-prior-art).


## M49 constructed-worker activation

M49 reuses the same selected Agda Application 774–808, With 101–173 and
remembered-equality readings. No new paper, PDF or full-reading claim is added.
The canonical manifest remains seven works and nine PDFs.

GHC reads known worker metadata and reconstructs scalar constructor trees at
expected domains. Ordinary checking and motive/row Given evidence retain their
roles. This adds no App/Core/solver or backend rule. Both builds, selected API1,
the source matrix, public21 and final9 pass. Artifact checks are recorded in
[the current checkpoint](../README.md#m49-constructed-worker-case-activation).
These focused results do not establish a theorem or full GHC suite result.
See [the source comparison](../REFERENCES.md#constructed-worker-case-activation-prior-art).


## M50 constructor-local telescope and row scope

Selected Agda v2.8.0 manual reads add constructor-telescope and clause-order
provenance. Data Types1–191 has verified Git blob
`7713c0755a94f784971a637059caaed4c616f04e`; Function Definitions1–64 has
`3c7e86cd7e6b88d67f35e8882455ee28f7da6d4d`. See the
[precise source ranges](../REFERENCES.md#multi-constructor-retained-packages-prior-art).
These sources do not establish Haskell laziness, positivity or totality.

The cached ICFP2017 author PDF was reread at section9, printed31:27/PDF27.
Primitive datatypes and pattern matching remain outside its presented D/DC
proof. No Coq replay, new PDF or whole-paper rereading is claimed. The canonical
ledger stays at seven works and nine PDFs. M50's source/interface/runtime
checks are component evidence; see [the checkpoint](../README.md#m50-multi-constructor-retained-package-patterns)
for the measured source, legacy8, constructor11 and final9 scope. These results
do not replace artifact checks. Freeze and independent audit must pass before
publication.


## M51 function fields and relevant telescopes

M51 revisits the thesis's dependency/relevance distinction at printed54–55
(PDF69–70), and the existing D/DC author PDF at7–9 and14–16. Agda's dependent
constructor/record telescopes and builtin Sigma give the closer field-scope
precedent. The selected primary-source lines, pinned Git blobs, actual web
checks and cached Sigma fetch boundary are recorded in
[the comparison](../REFERENCES.md#function-valued-retained-package-fields-prior-art).
The implementation stores actual lazy function/input fields. It adds no
singleton or call-injectivity rule. Actual source/interface checks are
operational evidence. No new whole-paper reading, proof replay or theorem
transfer is claimed. The ledger stays at seven works and nine cached PDFs.


## Observational equality, 2007 to 2025

The four new PDFs were downloaded and hashed on 2026-11-16. Reading was selective.
The manifest records each version and its exact scope. Automated per-page
extraction does not count as reading every page. No Agda proof, Rocq kernel or
Haskell prototype was rebuilt.

Read the 2007 abstract, introduction, overview, selected function transports,
neutral coercion quotation and conclusion. Read the 2022 introduction, selected
normalization statements and related-work comparison. The later paper records
a conjecture dependency in the earlier normalization argument.

Read selected sections of the TYPES implementation paper, including its
universe choice, syntax, conversion, casts, extensions and conclusion. It
implements an object language in Haskell. Its U:U choice is deliberately
inconsistent and undecidable. It also records a unit-conversion transitivity
limitation. Do not treat it as a verified implementation of Dependent Haskell.

Read the expanded TOPLAS article's introduction, overview, conversion algorithm
and scope, selected theorem statements, implementation overview and conclusion.
Section 5 limits the formalized conversion proof to the natural-number fragment.
It does not claim the full indexed-inductive scheme is covered by that proof.

These sources inform explicit equality and transport. They do not equate
pointwise function equality with contextual equivalence of partial Haskell
programs. Haskell's seq and divergence remain separate obligations. No OTT
design is selected. See REFERENCES.md for the proposed research task.

## Combining Proofs and Programs, POPL 2014

Read the introduction, relevant examples and Figure 1. Read the technical text
and displayed rules on PDF pages 4 to 11. These pages cover evaluation, fragment
interaction, metatheory, the implementation boundary, partial types and future
work. The author PDF has 13 pages. The archive records its URL, redirect, access
time, hash, byte count and extraction method.

The logical fragment has a closed-term CBV termination result. This result does
not prove normalization of arbitrary open terms. Contradictory equality
assumptions can type a looping logical expression. The completed core proof
excludes several features implemented in Zombie. The authors report Coq
checking with functional extensionality. No proof was replayed for this reading.

The interaction rules restrict values and inputs. A lazy Haskell binding or an
outer constructor does not establish those premises. A total Haskell subset
needs its own property and import rules. No total-subset design is selected by
this reading. No theorem is transferred to GHC. See the version-specific scope
in the manifest and REFERENCES.md.

## ANF and dependent types, JFP 2022

Read selected sections of the 53-page [publisher PDF](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/73FC888A23E5E87BAE16B158ABE349C8/S0956796822000090a.pdf/anf-preserves-dependent-types-up-to-extensional-equality.pdf)
on 2026-11-18. Printed page numbers match PDF positions. The reading covers
the title and abstract, the CBV evaluation paragraph on page 15, section 4.1
on pages 15–18, section 7.4 on pages 32–34, and section 7.6 on pages 35–36.
The text was extracted with pypdf and fontTools. No figures were inspected
visually. No full proof audit or artifact execution was performed.

Section 4.1, “Dependent continuation typing,” records the specific computation
expected by a continuation. Its bound variable has a definition in the context.
Knowing only the variable's type loses information needed by dependent uses.
The section proves continuation composition lemmas under this discipline.

Section 7.4, “Recovering decidability,” discusses annotations for the
equality-reflecting target. Its attempted translation with explicit `subst`
coercions fails in conditional branches because the continuation expects an
expression that judgmental equivalence cannot identify with the coerced result.
This counterexample concerns that translation. It does not exclude all designs
with explicit coercions.

Section 7.6, “Call-by-push-value and monadic form,” discusses dependent let
results, GHC's monadic form, and commuting conversions. The paper's runtime
semantics is call-by-value. M81 checks the lazy Core-to-STG translation
separately with partially defined indices.

## OutsideIn(X), JFP 2011

Read the abstract and PDF pages 36–43 of the author draft on 2026-11-18.
The draft is dated 24 March 2011 and has 81 pages. The publisher record gives
pages 333–412. These versions have different pagination. The reading used pypdf
text extraction for the selected pages, including the rules in Figures 11–15.

Sections 5.2 and 5.5 solve simple constraints before nested implications. The
solver applies the outer substitution and adds the local assumptions for each
implication. Only that implication's touchable variables can be unified.
Section 5.6.2 keeps implication classification syntactic.

This order informs the proposed placement of an optional local alias check.
The project proposal would inspect the actual method Wanted evidence before
activating an alias Given in the body implication. Exact dictionary selection,
conditional activation and lazy let identity require separate implementation
and soundness checks in this compiler.
