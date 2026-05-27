# GHC Exact Print Annotations — Technical Overview

## Motivation

A central goal of Haskell tooling — refactoring engines, formatters, language servers, code
generators — is to make targeted edits to a Haskell source file and then emit the result with
_only the intended changes_ applied.  A naive approach of pretty-printing the parsed AST loses
all original layout, comments, and stylistic choices, producing diffs that are far larger than
the semantic change.

GHC's **Exact Print Annotation (EPA)** subsystem solves this by embedding enough positional
information directly into every AST node to allow the tree to be reprinted _exactly_ as the
original source, byte-for-byte (modulo tab expansion).  When a tool modifies the AST it adjusts
only the annotations it needs to change; all surrounding nodes reprint themselves unchanged.

## Background: the syntax tree

The AST of the source program is represented using the data types defined in `Language.Haskell.Syntax.*`.
This data type uses the "Trees That Grow (TTG)" plan;
see [Implementing trees that grow](https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow)

For example, in (GHC-independent) `Language.Haskell.Syntax.Expr`:
```
data HsExpr p
  = ... many constructors including ...
  | HsLet       (XLet p)
                (HsLocalBinds p)
                (LHsExpr  p)

type family XLet p
type LHsExpr p = XRec p (HsExpr p)

```
Note that
* The `(XLet p)` field is the *extension field* of the `HsLet` data constructor,
  where `XLet` is a type family.
* Almost every node is wrapped in an `XRec`, another type family. That makes it easy
  for clients to attach arbitrary information to each node.

GHC specialises this data a type in `GHC.Hs.*`, as follows:
```
data Pass = Parsed | Renamed | Typechecked

data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
  GhcRn :: GhcPass 'Renamed
  GhcTc :: GhcPass 'Typechecked

type family XRec p a = r | r -> a
type instance XRec (GhcPass p) t = XRecGhc t

-- (XRecGhc tree) wraps `tree` in a GHC-specific,
-- but pass-independent, source location
type XRecGhc t = GenLocated (Anno t) t

data GenLocated l e = L l e
type family Anno t
```
So, via `XRec`, every node `e :: t` in the GHC-specific version of HsSyn is wrapped in
a `L ann e`, where `ann :: Anno t` is the annotation on the node.

Notice that there are two places we can hang information:
* (EXT) **Constructor-specific punctuation**: the extension field of each data constructor
  can contain information that is specific to that constructor.  Example: the location
  of the keywords `let` and `in` for the `HsLet` construct.

* (XREC) **Entire-node information**: the `Anno t` field that wraps almost every node in the
  syntax tree can contain information that is needed for *every* node.  Classic example:
  the `SrcSpan` of the node.

**NOTE**: currently there is information in (XREC) that more properly belongs in (EXT).
A refactoring project is under way to put this right.


## File layout

The subsystem lives primarily in `utils/check-exact/` and is exposed through three main layers:

| Layer | Key operation | Purpose |
|---|---|---|
| **Parser** | `getCommentsFor` / `getPriorCommentsFor` | Attaches comments to AST nodes |
| **Printer** | `exactPrint` / `makeDeltaAst` | Reprints the AST; converts absolute spans to relative deltas |
| **Transform** | `Transform` monad | Edits the AST while keeping annotations consistent |

---

## Core Concepts

### Positions: Absolute and Relative

The parser records every token's position as an **absolute** source span (file, line, column).
Before reprinting a modified AST the printer converts these to **relative** (delta) form.

```
EpaLocation
  ├─ EpaSpan   (absolute)  — original (line, col) span from the parser
  └─ EpaDelta  (relative)  — DeltaPos + retained original span + leading comments
```

A `DeltaPos` encodes a position as an offset from a _reference point_:

```
DeltaPos
  ├─ SameLine col          — rightward gap from the end of the preceding token; col ≥ 0
  └─ DifferentLine lines col
                           — lines below the preceding token;
                             col is a SIGNED offset from the enclosing layout block's
                             left margin (layout_lhs + col = absolute column)
```

The different reference points for the two constructors are deliberate and are what makes
layout-preserving moves possible (see _Splice Invariance_ below).

---

## Invariants

### 1. Every Node Is Self-Contained

Each located AST node carries a `NodeAnnotation` (the `EpAnn ann` wrapper) containing:

- **`anchor`** — an `EpaLocation` marking the top-left corner of the node's bounding rectangle.
  This is the coordinate origin for the node's internal layout.
- **`anns`** — node-kind-specific annotation payload (keyword token positions, bracket tokens,
  pragma text, etc.).
- **`comments`** — all `EpaComment` values logically owned by this node.

No separate annotation map exists.  The annotation is embedded directly in the `GenLocated`
wrapper via the `Anno` type family (the "Trees That Grow" mechanism).  The annotation type for
each node kind is determined statically by its Haskell type.

### 2. Trailing Annotations Are Separate from the Bounding Box

Punctuation that separates list elements — trailing commas, semicolons, vertical bars, and
constraint arrows — is stored in a `TrailingAnn` list that is _outside_ the node's bounding box.

```
TrailingAnn
  ├─ semi    — ';'
  ├─ comma   — ','
  ├─ vbar    — '|'
  └─ darrow  — '=>' or '⇒'
```

The `HasTrailing` typeclass exposes a uniform `trailing` / `setTrailing` interface over all
annotation types.  The printer uses this interface to extract trailing glue without inspecting
the concrete annotation type.

**Consequence for tooling:** when moving, copying, or deleting a node, a tool operates on the
anchor span and can transfer or discard trailing annotations independently.  There is no risk
of accidentally duplicating or losing a separator when restructuring a list.

### 3. The Bounding Box Invariant

The `anchor` span is the bounding box of everything the node _owns_:

- **Leading comments** are printed _before_ the anchor and lie _outside_ the bounding box.
  They may appear at any indentation.
- **Trailing annotations** are printed _after_ the anchor's end and also lie outside it.
- The anchor's start position is the top-left corner of the box; the parser widens the span
  (via `widenAnchorT` / `widenAnchorS`) to cover every token that syntactically belongs to
  the node.

```
     leading comment          ← outside bounding box; before anchor
     [anchor start]
       token token token      ← inside bounding box
       token token
     [anchor end]
     ,                        ← trailing annotation; outside bounding box
```

### 4. Splice Invariance

Moving an entire subtree to a different column requires updating _only the root node's entry
delta_.  No descendant delta needs to change.

This works because `DifferentLine` deltas store the column as `absolute_col − layout_lhs`, where
`layout_lhs` is the left margin of the enclosing layout block.  When the root anchor moves, the
printer adopts the new column as the new `layout_lhs`.  Every child that has a `DifferentLine`
delta recomputes its absolute column as `new_layout_lhs + stored_col`, sliding the entire subtree
to the right or left by the same amount.

`SameLine` deltas are unaffected by relocation because they record a gap from the immediately
preceding token, not from the layout margin.

**Known exceptions:** layout-block terminators such as `in`, `where`, and `of` intentionally
appear to the _left_ of the block they close.  Their `DifferentLine.col` is negative.  The
printer's validity check (`isGoodDelta`) accepts this: for `DifferentLine` it only requires
`lines > 0`, not that `col ≥ 0`.

### 5. The Zero-Column Invariant for List Items

Nodes that appear as direct children of a layout list (a `where` clause, a `do` block, an
export list, etc.) always store `DifferentLine(n, 0)` — a zero column offset from the list's
layout origin.

The list's layout origin is recorded in `AnnList.al_anchor`.  The absolute column of each item
is therefore `al_anchor.col + 0 = al_anchor.col`.  This has two consequences:

1. **Reordering is free.** Items can be moved within the list without updating any individual
   delta; all items already share the same column.
2. **Relocation is cheap.** Moving the entire list to a new indentation requires updating only
   `al_anchor`; all items self-correct automatically.

---

## Data Flow

```
Source file
     │
     ▼
┌─────────────┐
│   Parser    │  getCommentsFor / getPriorCommentsFor / getFinalCommentsFor
│  (GHC.Parser│  → allocates EpaComments to each AST node (unbalanced form)
│   + Lexer)  │  → all EpaLocations are absolute (EpaSpan)
└──────┬──────┘
       │ ParsedSource (absolute annotations)
       ▼
┌──────────────────┐
│  makeDeltaAst    │  converts EpaSpan → EpaDelta for every can_update node
│  (exactPrint.hs) │  adjustDeltaForOffset stores col as (absolute - layout_lhs)
└──────┬───────────┘
       │ ParsedSource (relative annotations)
       ▼
┌──────────────────────┐
│  Transform monad     │  structural edits: insert/remove/reorder declarations
│  (Transform.hs)      │  setEntryDP, transferEntryDP, balanceCommentsList, …
└──────┬───────────────┘
       │ modified ParsedSource
       ▼
┌──────────────────┐
│  exactPrint      │  single-pass traversal; undelta(prior_end, dp, layout_lhs)
│  (ExactPrint.hs) │  comment interleaving; layout block tracking
└──────┬───────────┘
       │
       ▼
   Source text (byte-for-byte faithful to the original, modulo edits)
```

---

## The Parser Phase

### Comment Allocation

The GHC parser maintains a comment queue (`comment_q`) of `EpaComment` values seen in the token
stream but not yet attached to any node.  As each grammar production is reduced, the parser drains
matching comments from the queue:

- `getCommentsFor span node` — moves all comments whose span falls _inside_ `span` into the
  node's `EpAnn.comments.prior` list.
- `getPriorCommentsFor span node` — additionally captures comments that fall _just before_ the
  span (used for top-level declarations to capture preceding Haddock comments).
- `getFinalCommentsFor module_node` — at EOF, drains the entire queue into the module node.

**Guarantee:** each comment is allocated to exactly one node; the queue is drained monotonically.
All `EpaComments` produced by the parser are in the `unbalanced` state (only `prior`, no
`following`).

### `Anno` Type Family

The annotation type for each AST node kind is chosen by the `Anno` type family.  Common
specialisations:

| Type alias | Annotation payload | Used for |
|---|---|---|
| `SrcSpanAnnA` | `AnnListItem` | Most expressions, patterns, declarations |
| `SrcSpanAnnN` | `NameAnn` | Name occurrences |
| `SrcSpanAnnL` | `AnnList ()` | Plain layout lists |
| `SrcSpanAnnLW` | `AnnList EpToken` | `where`-clause lists |
| `SrcSpanAnnP` | `AnnPragma` | `{-# … #-}` pragmas |
| `SrcSpanAnnC` | `AnnContext` | Constraint contexts `C a =>` |

---

## The Printing Phase

### The `ExactPrint` Typeclass

Every AST constructor that can appear in a located position has an `ExactPrint` instance:

```haskell
class (Typeable a) => ExactPrint a where
  getAnnotationEntry :: a -> Entry          -- extract anchor + trailing + comments
  setAnnotationAnchor :: a -> EpaLocation
                     -> [TrailingAnn] -> EpAnnComments -> a  -- write anchor back
  exact :: (Monad m, Monoid w) => a -> EP w m a              -- print this constructor
```

`markAnnotated a = enterAnn (getAnnotationEntry a) a` is the single recursive call site.
`enterAnn` handles all the cross-cutting concerns (comments, layout, anchor write-back) and
then delegates to `exact` for the constructor-specific tokens and child traversals.  There are
several hundred `ExactPrint` instances, one for each GHC AST constructor.

The same instance serves both `exactPrint` and `makeDeltaAst`: the output options (`EPOptions`)
determine whether tokens are accumulated into a string or discarded.

### `makeDeltaAst`

Converts every `EpaSpan` anchor to `EpaDelta` form in a single pass over the AST.  The delta
is computed as:

```
raw_delta   = ss2delta(prior_end_pos, anchor_start)
stored_delta = adjustDeltaForOffset(layout_lhs, raw_delta)
             = DifferentLine(lines, absolute_col - layout_lhs)
```

The inverse during printing (`undelta`) recovers the absolute column:

```
absolute_col = layout_lhs + stored_delta.col    -- DifferentLine
absolute_col = prior_col  + stored_delta.col    -- SameLine
```

This shared traversal runs with no-op output options; no source text is accumulated.

### `exactPrint`

Runs the same traversal as `makeDeltaAst` but with string-accumulating options.  For each node:

1. Leading comments (in `EpaDelta.leading_comments`) are printed before the anchor.
2. The anchor position is resolved (absolute from `EpaSpan`, or via `undelta` for `EpaDelta`).
3. If `mark_layout` is set, the anchor column becomes the new `layout_lhs` for the block.
4. Children are visited recursively with the updated layout state.

### Comment Interleaving

Two strategies are used depending on the anchor form:

- **Absolute anchor (`EpaSpan`):** the printer maintains a pool of pending comments sorted by
  source position.  Before each token it emits all pooled comments whose position precedes the
  token (`printCommentsBefore`, via `commentAllocationBefore`).
- **Relative anchor (`EpaDelta`):** comments are pre-attached to the node's annotation by
  `makeDeltaAst` or `balanceCommentsList`.  The entire pool is flushed unconditionally via
  `flushComments`, then the node's own attached comments are printed in order via `printOneComment`.

CPP-injected comments may carry fake filenames ("CPP", "LINE", "SHEBANG") in their spans.
Ordering always uses `ss2pos` (line, column only) rather than the full `RealSrcSpan` comparator,
which would sort by filename first and produce incorrect relative ordering.

### Parentheses in Infix Declaration Heads via the Comment Machinery

Infix type-level declarations — type synonyms, data types, class declarations, type families, and
GADT constructors — may carry optional parentheses around the operator/constructor in the
declaration head.  For example:

```haskell
type (a `MyOp` b) = ...
class (a `MyClass` b) where ...
data (a `MyData` b) = ...
```

These parentheses are stored as _lists_ of `EpToken` values (`ops`, `cps`) on the declaration's
annotation (`AnnSynDecl`, `AnnClassDecl`, `AnnFamilyDecl`, `AnnDataDefn`, `AnnConDeclGADT`, and
`AnnFunRhs` for infix function patterns).  They cannot be emitted at a fixed point in the
structural traversal because the infix head visits the type constructor and its arguments in an
order that does not naturally interleave with the surrounding parens.

The solution is `epTokensToComments "(" ops` / `epTokensToComments ")" cps`: each present
`EpToken` is converted to a synthetic `Comment` (with `keyword_origin = "("` or `")"`) and
injected into the comment pool.  The pool is ordered by source position, so these synthetic
parens are automatically emitted at exactly the right location relative to the other tokens,
without requiring any explicit position tracking in the `exact` instance.

During `makeDeltaAst`, each synthetic paren's computed delta is captured via `applyComment` and
embedded in the adjacent token's `EpaDelta` constructor, so the round-trip preserves paren
positions in delta form too.

### Mixed Bind/Sig Lists

The GHC AST stores function bindings and type signatures in separate sub-lists.  The printer
merges them using a sort key on the `ValBinds` extension point:

- **`NoAnnSortKey` (span order):** sorts by original source span — correct immediately after
  parsing while spans are still trustworthy.
- **`AnnSortKey [BindTag]` (tag order):** a sequence of `bind_tag` / `sig_tag` tokens recording
  the exact interleaving.  Set whenever `replaceDeclarations` is called; the printer follows the
  sequence exactly, enabling caller-controlled reordering.

Class and instance bodies use the same mechanism with four sub-lists and `DeclTag`.

---

## The Transform Phase

The `Transform` monad wraps a state carrying a unique-span counter and a debug log.  It exposes
operations for structural edits while keeping annotations consistent.

The unique-span counter exists to support `uniqueSrcSpanT`, which allocated synthetic `SrcSpan`
values (line = -1) for freshly created AST nodes.  That function is now dead code: it is exported
for API compatibility but is called nowhere in GHC or `check-exact`.  New nodes are given
`EpaDelta` locations directly, which need no unique span.  The counter field in `TransformState`
is therefore vestigial and should be removed together with `uniqueSrcSpanT` and `isUniqueSrcSpan`
in a future cleanup.

### Declaration Access

- **`getDeclarations node`** — returns the declaration list of a module, match, let-binding,
  pattern-binding where clause, or class/instance body as a `DeclarationList`.
- **`replaceDeclarations node new_decls`** — replaces the list; implicitly records the new
  ordering as a tag-order `AnnSortKey`, so the printer will honour the caller's order regardless
  of original source spans.  **Passing an empty list collapses the underlying `HsLocalBinds` to
  `EmptyLocalBinds`, which drops the `where` keyword annotation entirely** — this is not a no-op
  on the container structure.

A key responsibility of `getDeclarations` is **normalising `ValBinds`**.  Inside a
`HsLocalBinds` / `ValBinds` node (a `where` clause, `let` expression, `let` statement, or
pattern-binding where clause), GHC stores function bindings and type signatures in two _separate_
sub-collections: a `Bag` of `LHsBind` and a `[LSig]`.  These are physically disjoint from each
other and have no guaranteed ordering relative to each other.

`getDeclarations` (via `hsDeclsLocalBinds` → `hsDeclsValBinds` → `orderedDeclsBinds`) merges
them back into a single `[LHsDecl]` in source order:

- **`NoAnnSortKey` (freshly parsed):** sorts the combined list by `RealSrcSpan`, recovering the
  original interleaved order from the absolute source positions.
- **`AnnSortKey [BindTag]` (after any structural edit):** replays the tag sequence recorded by
  `captureOrderBinds`, drawing from the binds bag or sigs list according to each tag.

The result is a physically ordered, uniform list of `LHsDecl` values — binds wrapped in `ValD`,
sigs wrapped in `SigD` — that the exact-print and transformation machinery can traverse and edit
without knowing or caring that the underlying storage splits them across two separate containers.
`replaceDeclarations` is the inverse: it re-separates the uniform list back into the two
sub-collections via `decl2Bind` / `decl2Sig` and records a fresh `AnnSortKey` for the new order.

### Positioning Nodes

- **`setEntryDP node dp`** — sets the entry delta of a node, converting its anchor to relative
  form.  Must be called before inserting a freshly constructed node.
- **`getEntryDP node`** — reads the current entry delta (default `SameLine 0` if absent).
- **`transferEntryDP source target`** — moves the entry delta _and_ leading comments from one
  node to another.  Used when a removed declaration's successor needs to inherit its spacing.

### Inserting and Removing Declarations

**`insertDeclaration node decl point`** inserts `decl` at `point` (start, end, before/after a
named item).  The inserted node must already have a relative anchor.  All existing declarations
are comment-balanced before the structural change.

**`removeDeclaration node decl`** removes `decl`.  The caller must then call `transferEntryDP`
or `setEntryDP` on the new first declaration to absorb the gap.

### Comment Balancing

After parsing, all comments are in the `prior` list of the nodes that saw them first.  The
`balanceComments` / `balanceCommentsList` operations redistribute them:

- A comment ≤ 1 blank line below declaration D stays with D as a `following` comment.
- A comment > 1 blank line below D migrates to the `prior` list of D's successor.

The operation is **idempotent**: running it twice produces the same result because `balanced` is
a terminal state (`unbalanced → balanced`; no transition out of `balanced` is defined).

For `FunBind` nodes the balancing is hierarchical: comments are first balanced at the binding
level, then distributed among the individual match equations within the binding.

### Capturing Spacing Before Edits

Before removing or reordering declarations, spacing must be encoded in relative form so it
survives the structural change.  If `makeDeltaAst` has **not** been called, the AST still
carries absolute `EpaSpan` anchors; the spacing functions below derive relative deltas from those
absolute positions and write them back, making subsequent structural edits safe.  If `makeDeltaAst`
**has** already been called, the anchors are already in `EpaDelta` form and these functions are
idempotent — they recompute the same delta values that `makeDeltaAst` already stored.

- **`captureLineSpacing decls`** — sets each declaration's entry delta lines to the actual blank
  lines between it and its predecessor.
- **`captureMatchLineSpacing decl`** — same, within the match equations of a `FunBind`.
- **`captureTypeSigSpacing sig`** — captures spacing within a multi-name type signature.
- **`addModuleCommentOrigDeltas module`** — converts module-level comment spans from absolute to
  relative form before the module's declaration list is replaced.

---

## CPP Support

Source files using `{-# LANGUAGE CPP #-}` present a special challenge: the GHC parser sees
the _preprocessed_ text, which may differ substantially from the original source (directives
removed, macros expanded).

### Pipeline

```
Original source
     │
     ├─ getPreprocessorAsComments  ─→  directive line tokens (real filename)
     │
     ├─ stripLinePragmas / tokeniseOriginalSrc  ─→  directive-stripped token stream
     │
     └─ getPreprocessedSrcDirect  ─→  C-preprocessed text
                                         └─ lexTokenStream  ─→  post-CPP token stream
                                              │
                                              └─ getCppTokens  ─→  three-way merge
                                                                  ─→  MergedCppComments
```

`getCppTokens` identifies directive tokens present in the original source but absent from the
preprocessed output (consumed by CPP) and converts them to synthetic `ITlineComment` tokens.
These are merged with the directive lines extracted directly to form `injected_comments`.

**Limitation:** the three-way merge is correct only in `-nomacro` mode.  Macro expansion
introduces expanded tokens that are not recoverable by span-based matching.

### Insertion

`insertCppComments module injected_comments` splices the synthetic comments back into the parsed
AST using a bottom-up traversal (`everywhereM`): each `EpAnn` node claims the injected comments
whose source span it encloses.  Remaining comments are distributed to module-level positions by
`insertTopLevelCppComments`.  CPP-aware ordering (`ss2pos`, ignoring filenames) is used
throughout.

After `insertCppComments`, the round-trip guarantee extends to CPP-enabled sources:
`exactPrint(insertCppComments(parseModuleEpAnnsWithCpp(f)))` reproduces `f` byte-for-byte.

---

## Correctness Guarantees

| Guarantee | Statement |
|---|---|
| **RoundTripFidelity** | For any file `f` parsed by GHC, `exactPrint(makeDeltaAst(parse(f)))` is byte-identical to `f` (modulo tab expansion). |
| **AllCommentsEmitted** | After a complete `exactPrint` traversal, every comment attached to the AST has been emitted exactly once and the pending pool is empty. |
| **CursorNonDecreasing** | The output cursor only moves forward; tokens are emitted in source order. |
| **CommentsAllocatedAtMostOnce** | Each comment in the parser's queue is moved to exactly one AST node; once removed it is never re-added. |
| **BalanceIsIdempotent** | `balanceCommentsList` can be called multiple times on the same list without duplicating or losing comments. |
| **ReplacePreservesCallerOrder** | After `replaceDeclarations(node, new_decls)`, the printer outputs declarations in the same order as `new_decls`. |
| **RoundTripWithCpp** | For CPP-enabled files, `exactPrint(insertCppComments(parseModuleEpAnnsWithCpp(f)))` reproduces `f` byte-for-byte (no-macro mode only). |

---

## Typical Tool Workflow

A refactoring tool that wants to add a new top-level declaration does the following:

```
1. parseModuleEpAnnsWithCpp opts libdir file   -- parse with CPP support
2. insertCppComments parsed_source comments    -- re-insert CPP/LINE/SHEBANG comments
3. makeDeltaAst parsed_source                  -- convert absolute spans to deltas
4. runTransform do
       addModuleCommentOrigDeltas module       -- protect module-level comments
       decls ← getDeclarations module          -- read current declaration list
       captureLineSpacing decls               -- encode spacing before edits
       balanceCommentsList decls              -- redistribute comments
       setEntryDP new_decl (DifferentLine 2 0) -- position the new declaration
       replaceDeclarations module (decls ++ [new_decl])
5. exactPrint modified_source                  -- emit the result
```

The result is the original source with the new declaration appended, all original comments and
layout preserved, and a diff that contains only the added lines.
