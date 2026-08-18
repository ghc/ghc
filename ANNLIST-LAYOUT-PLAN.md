# Plan: make `AnnList` capture layout only where layout is real

## Status

In progress. Steps 1–3 of §7 are done, step 4 is partly done. See §7 for the
per-step state.

**Out of scope.** The backpack unit body (`unitbody`) and the header-only
parsers (`header_body`, `header_body2`) are excluded: neither produces an AST
that is exact printed, so their `vocurly` alternatives need no `AnnList`.

All references below are by *production name*, *function name*, or *constructor
name* rather than line number, so they survive movement within the tree. Grep
for the quoted identifier.

---

## 1. Ground truth: where the lexer actually opens a layout context

The authoritative list lives in `maybe_layout` in `compiler/GHC/Parser/Lexer.x`.
It is a single `where`-bound function `f` matching on the token just lexed:

```haskell
maybe_layout :: Token -> P ()
maybe_layout t = do
    alr <- getBit AlternativeLayoutRuleBit
    unless alr $ f t
    where f (ITdo _)    = pushLexState layout_do
          f (ITmdo _)   = pushLexState layout_do
          f ITof        = pushLexState layout
          f ITlcase     = pushLexState layout
          f ITlcases    = pushLexState layout
          f ITlet       = pushLexState layout
          f ITwhere     = pushLexState layout
          f ITrec       = pushLexState layout
          f ITif        = pushLexState layout_if
          f _           = return ()
```

plus one further entry point: the alex rule for `"[d|"` guarded by
`ifExtension ThQuotesBit`, whose action is `layout_token ITopenDecQuote`
(`layout_token` is defined next to `token` and simply does
`pushLexState layout >> return (L span t)`).

So there are **eight** layout-opening constructs:

| construct | lexer state | open token emitted |
| --- | --- | --- |
| `do`, `mdo` | `layout_do` (non-strict; see the `strict` arg of `new_layout_context`) | `ITvocurly` |
| `of` | `layout` | `ITvocurly` |
| `\case`, `\cases` | `layout` | `ITvocurly` |
| `let` | `layout` | `ITvocurly` |
| `where` | `layout` | `ITvocurly` |
| `rec` | `layout` | `ITvocurly` |
| `if` (MultiWayIf) | `layout_if` | **`ITvbar`** |
| `[d\|` | `layout` via `layout_token` | `ITvocurly` |

Three properties of this machinery drive everything below.

**(a) The layout column is computed but never attached to a token.**
`new_layout_context` computes

```haskell
    let offset = srcLocCol (psRealLoc l) - len
    ...
        _ -> do setContext (Layout offset gen_semic : ctx)
                return (L span tok)
```

`offset` goes into the `LayoutContext` stack and is then discarded. The
`ITvocurly` token that reaches the parser carries only its own span. Its
*start column* happens to equal `offset` in the common case, which is what
`getVOCURLY` in `compiler/GHC/Parser.y` exploits:

```haskell
getVOCURLY (L (RealSrcSpan l _) ITvocurly) = srcSpanStartCol l
```

**(b) MultiWayIf opens a context with no `vocurly`.** The `layout_if` state has

```
<layout_if> {
  \| / { notFollowedBySymbol }          { new_layout_context True dontGenerateSemic ITvbar }
  ()                                    { pop }
}
```

The token handed to the parser is `ITvbar`, i.e. the guard bar itself. There is
no distinct open-brace token in the stream for MultiWayIf.

**(c) A context can close with no token at all.** The `close` production in
`Parser.y` is

```
close :: { () }
        : vccurly               { () } -- context popped in lexer.
        | error                 {% popContext } -- See Note [Layout and error]
```

so the closing `vccurly` is not guaranteed to exist.

**(d) `dontGenerateSemic`.** MultiWayIf's context is pushed with
`dontGenerateSemic` (contrast `generateSemic` for the `<layout>` /
`<layout_do>` fallthrough rules). So "has layout" and "emits virtual semis"
are *not* the same predicate, and the annotations should not conflate them.

---

## 2. What `Parser.y` currently does

### 2.1 The `AnnList` record

In `compiler/GHC/Parser/Annotation.hs`:

```haskell
data AnnList a
  = AnnList {
      al_anchor    :: !(Maybe EpaLocation), -- ^ start point of a list having layout
      al_brackets  :: !AnnListBrackets,
      al_semis     :: [EpToken ";"], -- decls
      al_rest      :: !a,
      al_trailing  :: ![TrailingAnn]
      }
```

with

```haskell
data AnnListBrackets
  = ListParens (EpToken "(")         (EpToken ")")
  | ListBraces (EpToken "{")         (EpToken "}")
  | ListSquare (EpToken "[")         (EpToken "]")
  | ListBanana (EpUniToken "(|" "⦇") (EpUniToken "|)"  "⦈")
  | ListNone
```

Note that `al_brackets = ListNone` is already the discriminant for "no explicit
brackets were written", and for the layout constructs that means "layout was
used". The bracket field and the anchor field therefore overlap in what they
encode, inconsistently.

### 2.2 The `vocurly` sites

Grep `Parser.y` for `vocurly`. There are fourteen uses in productions (plus the
terminal declaration and `getVOCURLY`). Only **five** populate `al_anchor`:

| production | layout keyword(s) | anchor expression |
| --- | --- | --- |--- |
| X |`where_decls` | `where` | `Just (fstOf3 $ unLoc $3)` |
| X |`decllist` | `let`, `where` | `Just (fstOf3 $ unLoc $2)` |
| X |`dbinds` alternative of `binds` (the `HsIPBinds` one) | `let` with implicit params | `Just $ glR $2` |
| X |`altslist` | `of`, `\case`, `\cases` | `Just $ glR $2` |
|   |`stmtlist` (via `hsDoAnn`) | `do`, `mdo`, `rec` | `Just $ spanAsAnchor (locA ll)` |

The remaining nine record nothing in an `AnnList`:

- `body` / `body2` (module `where`) — uses `EpVirtualBraces (getVOCURLY $1)`
- `decllist_cls` (class `where`) — uses `EpVirtualBraces (getVOCURLY $1)`
- `decllist_inst` (instance `where`) — `noAnn, noAnn`, nothing recorded
- the `vocurly ty_fam_inst_eqns close` and `vocurly '..' close` alternatives
  (type family `where`)
- the `'where' vocurly gadt_constrs close` alternative (GADT `where`) —
  `NoEpTok, NoEpTok`
- `cvtopbody` (`[d| ... |]` declaration quote) — `NoEpTok, NoEpTok`
- the empty-alts alternative `vocurly close` in `altslist` — explicitly `noAnn`

(The backpack `vocurly unitdecls close` and the `header_body` alternatives also
record nothing, but are out of scope — see §Status.)

And MultiWayIf (`ifgdpats`) has no `AnnList` at all; `HsMultiIf` is annotated
with a bare `(EpToken "{", EpToken "}")` pair.

### 2.3 The parallel `EpLayout` mechanism

`compiler/GHC/Parser/Annotation.hs` also defines

```haskell
data EpLayout =
    EpExplicitBraces !(EpToken "{") !(EpToken "}")
  | EpVirtualBraces !Int   -- ^ Layout column (indentation level, begins at 1)
  | EpNoLayout
```

This is the encoding that *faithfully* mirrors `new_layout_context`: it stores
the column, and it cleanly distinguishes braces from layout from neither. It
is currently reachable only from `hsmodLayout` in `compiler/GHC/Hs.hs` and from
`type instance XClassDecl GhcPs = (AnnClassDecl, EpLayout)` in
`compiler/GHC/Hs/Decls.hs` — and it exists primarily for Haddock comment
association, see `Note [Class EpLayout]` in
`compiler/Language/Haskell/Syntax/Decls.hs` and the `layout :: EpLayout` notes
in `compiler/GHC/Parser/PostProcess/Haddock.hs`.

So GHC currently has two unrelated encodings of the same lexer fact, applied to
disjoint sets of constructs, and neither is complete.

### 2.4 The single consumer

`al_anchor` is read in exactly one place: the `HsValBinds` case of the
`ExactPrint (HsLocalBinds GhcPs)` instance in `utils/check-exact/ExactPrint.hs`:

```haskell
  exact (HsValBinds an valbinds) = do
    an0 <- markLensFun' an lal_rest markEpToken -- 'where'
    case al_anchor $ anns an of
      Just anc -> do
        when (not $ isEmptyValBinds valbinds) $ setExtraDP (Just anc)
      _ -> return ()
    ...
```

`setExtraDP` stores into `uExtraDP`, which `enterAnn` consumes to override the
*first child's* entry delta — see the branch in `enterAnn` commented

```haskell
        Just (EpaDelta _ dp _) -> (dp, Nothing)
                   -- Replace original with desired one. Allows all
                   -- list entry values to be DP (1,0)
```

`setExtraDP Nothing` is executed immediately after reading it, so the override
affects only the first element. Only `ss2pos` of the anchor is ever used; the
end of the span is discarded.

---

## 3. Defects

### D1 — Under-capture

Three of the eight layout keywords have no `al_anchor`: `[d|`, MultiWayIf's
`|`, and the class / instance / GADT / type-family / module `where` variants.
Of those, module and class bodies are served by `EpLayout`; instance bodies,
GADT bodies, type-family bodies and declaration quotes are served by nothing.

### D2 — Over-capture

`al_anchor` is set unconditionally in the *explicit-brace* alternatives, where
the lexer opened no layout context whatsoever:

- the `'{' decls '}'` alternative of `decllist`
- the `'{' ... '}'` alternative of `where_decls`
- the `'{' ... '}'` alternative of `altslist`
- the `'{' dbinds '}'` alternative producing `HsIPBinds`
- `hsDoAnn`, which is shared by both `do { ... }` and layout `do`, and sets
  the anchor unconditionally

In every one of these `al_brackets` is already `ListBraces`, so the anchor is
at best redundant and at worst actively misleading.

### D3 — Capture on a construct with no layout at all

The `HsCmdArrForm` production for banana brackets builds

```haskell
    AnnList (glRM $1) (ListBanana (epUniTok $1) (epUniTok $4)) [] noAnn []
```

i.e. `al_anchor` points at the `(|` token. This is the only site where the
field points at a *token* rather than at list contents, and the construct has
nothing to do with layout.

### D4 — Inconsistent meaning of the stored value

The field is documented as "start point of a list having layout", but the value
stored varies:

- `decllist` / `where_decls`: a span *covering all the decls*. The `decls`
  production accumulates it with `glEE (fstOf3 $ unLoc $1) $3`, i.e. first-to-last.
- `altslist`: `glR $2`, the span of the whole `alts` non-terminal.
- `hsDoAnn`: `spanAsAnchor (locA ll)`, the span of the stmts.
- `HsCmdArrForm`: the `(|` token.

This only works today because the sole consumer takes `ss2pos` and throws away
the end. It is also *not* the layout column: it is the start of the first item,
which coincides with the layout column for well-formed layout but is not the
same fact, and diverges under D5.

### D5 — Leading-semicolon skew

The `alts` production has an alternative

```
        | ';' alts(PATS)           { $2 >>= \ $2 -> return $
                                     sLL $1 $> (((mzEpTok $1) : (fst $ unLoc $2) )
                                               ,snd $ unLoc $2) }
```

using `sLL $1 $>`, so for `case x of ; a -> b` the anchor starts at the `;`
rather than at the first alt. `decls` has the analogous `decls ';'` shape.

AZ: This is the way GHC works. The leading semi does indeed introduce layout, and the rest of the case statement clauses have to line up with it.

### D6 — Empty-list repair hack

`add_where` / `patch_anchor` in `compiler/GHC/Parser/PostProcess.hs` exist
solely to repair the anchor when the decl list is empty and the derived span is
invalid:

```haskell
-- If the decl list for where binds is empty, the anchor ends up
-- invalid. In this case, use the parent one
patch_anchor :: RealSrcSpan -> EpaLocation -> EpaLocation
```

This is a direct symptom of deriving the anchor from the list *contents*
instead of from the `vocurly` token.

### D7 — The anchor is silently dropped by transformations

`setAnchorEpaL` in `utils/check-exact/ExactPrint.hs` discards it:

```haskell
setAnchorEpaL (EpAnn _ an _) anc ts cs = EpAnn anc (setTrailing (an {al_anchor = Nothing}) ts) cs
```

so any re-anchoring loses the layout information.

AZ: function no longer exists

### D8 — Layout, braces and semis are conflated

`al_semis` holds the `;` tokens, which in a layout context are *virtual* semis
inserted by `do_bol` and in a brace context are real user-written tokens. The
`AnnList` does not distinguish them. Note also (c)/(d) in §1: MultiWayIf has
layout but `dontGenerateSemic`, and `close` may be an error-triggered
`popContext` with no `vccurly`, so "brackets present", "semis present" and
"layout in force" are three independent facts.

---

## 4. Proposed design

### 4.1 Replace `Maybe EpaLocation` with an explicit three-way discriminant
*DONE*

Introduce a dedicated type in `compiler/GHC/Parser/Annotation.hs` next to
`AnnListBrackets`:

```haskell
-- | How the extent of a list was delimited in the source.
data AnnListLayout
  = AnnListBraces   -- ^ Explicit @{ ; }@ written by the user. The tokens
                    --   live in 'al_brackets' and 'al_semis'.
  | AnnListLayout !EpaLocation
                    -- ^ The lexer opened an implicit layout context.
                    --   The 'EpaLocation' is the @vocurly@ token (or, for
                    --   MultiWayIf, the leading @|@), whose start column is
                    --   the layout column chosen by 'new_layout_context'.
  | AnnListNoLayout -- ^ Neither: a bracketed or compiler-generated list.
  deriving (Data, Eq)
```

and change the field:

```haskell
data AnnList a
  = AnnList {
      al_layout    :: !AnnListLayout,
      al_brackets  :: !AnnListBrackets,
      al_semis     :: ![EpToken ";"],
      al_rest      :: !a,
      al_trailing  :: ![TrailingAnn]
      }
```

This makes D2, D3 and D8 unrepresentable: a list either has braces, or has
layout with a definite origin token, or has neither. It also aligns `AnnList`
with the shape `EpLayout` already uses, which is a prerequisite for §4.5.

Renaming the field (rather than reusing `al_anchor`) is deliberate: it forces
every construction site to be revisited, which is what this plan is for.

*As implemented*, `AnnList` lost its type parameter along with `al_rest` and
`al_trailing`, and `AnnListBrackets` was reduced to `ListBraces` / `ListNone`,
so the record is now just `al_layout` / `al_brackets` / `al_semis`. That makes
D3 structurally impossible rather than merely discouraged.

### 4.2 Derive the layout location from the `vocurly` token, not the contents

*DONE*
Every `vocurly`-bearing production should pass `$1` (the `vocurly` token) into
the `AnnList`, e.g. in `altslist`:

```
        | vocurly    alts(PATS)  close { ... (AnnList (AnnListLayout (glR $1)) ListNone (fst $ unLoc $2) noAnn []) ... }
        | '{' alts(PATS) '}'           { ... (AnnList AnnListBraces (ListBraces (epTok $1) (epTok $3)) (fst $ unLoc $2) noAnn []) ... }
        | '{'              '}'         { ... (AnnList AnnListBraces (ListBraces (epTok $1) (epTok $2)) [] noAnn []) ... }
        | vocurly          close       { ... (AnnList (AnnListLayout (glR $1)) ListNone [] noAnn []) ... }
```

This fixes D4 (one well-defined value: the open token), D5 (the `vocurly`
precedes any leading `;`), and D6 (the token exists even when the list is
empty, so `patch_anchor` becomes unnecessary).

It also means the *layout column* becomes recoverable as
`srcSpanStartCol` of the location — the same value `getVOCURLY` returns —
rather than the start of the first item. For MultiWayIf the location is the
`ITvbar` token, which is exactly the token `new_layout_context` measured.


*NOT DONE*
Note the `decls` production can then stop threading its `EpaLocation`
component (the `fstOf3`, built with `glR`/`glEE`) purely for this purpose;
check whether any other consumer wants it before removing it.

### 4.3 Site-by-site changes in `Parser.y`

Set `AnnListLayout (glR $1)` at, adding an `AnnList` where none exists today:

- `altslist` — `of`, `\case`, `\cases` (already has `AnnList`)
- `stmtlist` / `hsDoAnn` — `do`, `mdo`, `rec` (already has `AnnList`; the
  `vocurly` must be threaded from `stmtlist` into `hsDoAnn`, which currently
  receives only the brace/semi triple and the stmts location)
- `decllist` — `let`, `where` (already has `AnnList`)
- `where_decls` — `where` (already has `AnnList`)
- the `HsIPBinds` alternatives — implicit-param `let` (already has `AnnList`)
- `decllist_inst` — instance `where` (**new**)
- the GADT `'where' vocurly gadt_constrs close` alternative (**new**)
- the type-family `vocurly ty_fam_inst_eqns close` and `vocurly '..' close`
  alternatives (**new**)
- `cvtopbody` — `[d| ... |]` (**new**)
- `ifgdpats` — MultiWayIf (**new**; see §4.4)

Set `AnnListBraces` in the corresponding `'{' ... '}'` alternative of each of
the above.

Set `AnnListNoLayout` at every non-layout `AnnList`:

- `HsCmdArrForm` banana brackets (fixes D3)
- `mkHsExplicitListPV` call sites and `mkHsCompAnns` (already `Nothing` today)
- any other `ListParens` / `ListSquare` / `ListBanana` construction

`body` / `body2` and `decllist_cls` keep their `EpLayout`; see §4.5.

### 4.4 MultiWayIf

`ifgdpats` currently yields `((EpToken "{", EpToken "}"), NonEmpty (LGRHS ...))`
and `HsMultiIf` is annotated with just the token pair. To record its layout,
`XMultiIf GhcPs` needs to become an `AnnList ()` (or a small dedicated record
carrying an `AnnListLayout`). The layout location is the leading `|`, which is
`$1` of the first `gdpat` — the `gdpats` production will need to expose it, or
`ifgdpats` can recover it from the location of the head of the returned
`NonEmpty`.

This is the largest single change in the plan and can be deferred to a
follow-up; the rest of §4.3 is independent of it. If deferred, `HsMultiIf`
should be listed explicitly as a known gap.

### 4.5 Reconcile with `EpLayout` (follow-up)

Once `AnnListLayout` exists, `EpLayout` is a strict subset of it modulo the
column-vs-location representation:

| `EpLayout` | `AnnListLayout` |
| --- | --- |
| `EpExplicitBraces o c` | `AnnListBraces` (+ `al_brackets = ListBraces o c`) |
| `EpVirtualBraces col` | `AnnListLayout loc` where `srcSpanStartCol loc == col` |
| `EpNoLayout` | `AnnListNoLayout` |

Two options, in preference order:

1. **Keep both, derive one from the other.** Leave `hsmodLayout` and
   `XClassDecl`'s `EpLayout` in place (Haddock depends on the `Int` column via
   `Note [Class EpLayout]`), but provide
   `epLayoutFromAnnList :: AnnList a -> EpLayout` and populate both from the
   same `vocurly` token so they cannot drift.
2. **Collapse onto `AnnListLayout`** and teach
   `compiler/GHC/Parser/PostProcess/Haddock.hs` to take the column from the
   location. Cleaner, but touches Haddock comment placement, which has its own
   test surface.

Recommend option 1 for this change and revisiting option 2 separately.

### 4.6 Preserve the field across transformations

`setAnchorEpaL` in `utils/check-exact/ExactPrint.hs` currently reads

```haskell
setAnchorEpaL (EpAnn _ an _) anc ts cs = EpAnn anc (setTrailing (an {al_layout = AnnListNoLayout}) ts) cs
```

Since `al_trailing` was removed from `AnnList`, the `HasTrailing AnnList`
instance is `setTrailing a _ = a`, so the `ts` argument is discarded and the
layout-clearing is the *only* remaining difference from the generic
`setAnchorEpa`. Delete `setAnchorEpaL` and point its single call site (the
`HsValBinds` case of `setAnnotationAnchor`) at `setAnchorEpa`.

This matters because `enterAnn` calls `setAnnotationAnchor` on the value
`exact` has just returned, so the `al_layout = AnnListLayout (EpaDelta ss dp [])`
that the `HsValBinds` case carefully writes back from `getExtraDPReturn` is
overwritten with `AnnListNoLayout` a few steps later, in the same traversal.
Re-anchoring a node does not change whether it was written with layout. If some
caller genuinely needs it cleared, that caller should do so explicitly.
(Fixes D7.)

Note this step is subsumed by §4.7: if `HsValBinds` loses its `EpAnn`, its
`setAnnotationAnchor` becomes the trivial `a _ _ _ = a` case and D7 ceases to
exist rather than being fixed. It is still worth doing first — it is one line
and it unblocks §5.2.

### 4.7 Locate `HsLocalBinds`

`XHsValBinds` and `XHsIPBinds` are the last extension points holding an
`EpAnn AnnList`:

```haskell
type instance XHsValBinds (GhcPass pL) (GhcPass pR) = (EpAnn AnnList, EpToken "where")
type instance XHsIPBinds  (GhcPass pL) (GhcPass pR) = (EpAnn AnnList, EpToken "where")
```

Every other `AnnList` in the tree is now a *bare* `AnnList` in an extension
point, anchored by the enclosing `GenLocated`: `acd_list` (class), `acid_decls`
(instance), `andd_list`, `XClosedTypeFamily`, `MatchGroupAnn`, `XRecStmt`.
That is the target model — the anchor lives in the `x` of `GenLocated x a`, and
the extension point carries only annotations printed *within* the anchored
space.

`HsValBinds` is the outlier for a structural reason, not a stylistic one:
`HsLocalBinds` is never wrapped. `LHsLocalBinds` is defined in
`compiler/Language/Haskell/Syntax/Binds.hs` but has no uses in `compiler/`, and
there is no `Anno (HsLocalBinds …)` instance. The three occurrence sites all
take the unlocated type:

- `grhssLocalBinds :: HsLocalBinds p` in `GRHSs`
- `HsLet (XLet p) (HsLocalBinds p) …`
- `HsCmdLet (XCmdLet id) (HsLocalBinds id) …`

So the `EpAnn` in the extension point is doing double duty — supplying the node
anchor *and* the internal annotations — and
`getAnnotationEntry (HsValBinds (an,_) _) = fromAnn an` reaches into the
extension point for an entry precisely because the `GenLocated` layer is
absent.

**The anchor is largely redundant.** The `where` token carries its own
`EpaLocation` and is printed first; `al_layout = AnnListLayout <vocurly>` now
gives the layout column directly (§4.2); each bound decl is an `LHsBind` with
its own anchor. The redundancy is visible in `setExtraDP`, whose entire job is
to *override* the first decl's own entry delta with the block anchor — two
encodings of one position, one overwriting the other.

**What the anchor uniquely provides is a comment store.** `EpAnn`'s
`EpAnnComments` is where the where-block's comments live; see
`addCommentsList :: EpAnn AnnList -> …` in `utils/check-exact/Utils.hs`. This
is why the extension point cannot simply be demoted to a bare `AnnList` with
`getAnnotationEntry = NoEntryVal` — the comments would have nowhere to go.

**The change.** Use `LHsLocalBinds` at the three occurrence sites above, add
the `Anno` instance (or its replacement), and reduce the extension points to

```haskell
type instance XHsValBinds (GhcPass pL) (GhcPass pR) = (AnnList, EpToken "where")
type instance XHsIPBinds  (GhcPass pL) (GhcPass pR) = (AnnList, EpToken "where")
```

The anchor and comments move into the `XRec` wrapper, where the rest of the
tree keeps them. `AnnList` then retains only brackets, semis and layout — all
printed within the anchored space.

Knock-on sites to revisit, all of which currently thread the `EpAnn`:

- `add_where` / `patch_anchor` / `annBinds` in
  `compiler/GHC/Parser/PostProcess.hs` — `add_where` exists to repair the
  *anchor*, so it should disappear entirely with it, finishing D6.
- `oldWhereAnnotation` / `newWhereAnnotation` in
  `utils/check-exact/Transform.hs`.
- `addCommentsList` in `utils/check-exact/Utils.hs`.
- the `HsValBinds` / `HsIPBinds` cases of `ExactPrint`: `getAnnotationEntry`
  becomes `NoEntryVal`, `setAnnotationAnchor` becomes trivial, and
  `markAnnList` / `markAnnListA` take a bare `AnnList`.
- `annotationAnnListEpAnn` in `compiler/GHC/Hs/Dump.hs`.

This is a `Language.Haskell.Syntax` change touching all three passes, Haddock,
and every `grhssLocalBinds` consumer. The fallout from locating the binds is
being handled separately; it is not sized here.

---

## 5. Consumer-side changes in `utils/check-exact/ExactPrint.hs`

### 5.1 `HsValBinds`

The existing `case al_anchor $ anns an of Just anc -> ...` becomes a match on
`al_layout`, firing only for `AnnListLayout`:

```haskell
    case al_layout (anns an) of
      AnnListLayout anc
        | not (isEmptyValBinds valbinds) -> setExtraDP (Just anc)
      _ -> return ()
```

Behaviourally this is the *same* for layout `where`/`let` and a *change* for
the explicit-brace form, which previously also set the extra DP. That change is
the point (D2), but it will move output for brace-form tests — see §6.

The write-back at the end of the `HsValBinds` case (`getExtraDPReturn`, which
stores an `EpaDelta` back into `al_anchor`) needs the same treatment: it should
write `AnnListLayout (EpaDelta ss dp [])`.

### 5.2 `MatchGroup` — the motivating use case

This is what makes `entryDP = DifferentLine 1 0` work uniformly for `HsCase`,
`HsLam`, and multi-clause `FunBind`. Recall the mechanism:

- `setLayoutBoth` arms `dMarkLayout`/`pMarkLayout`; the *first*
  `printString True` inside the scope captures the output column into `pLHS`
  and clears the flag. `advance` only calls `printWhitespace` (`padUntil`), so
  entering a node does not arm the column — the first real token does.
- `undelta` resolves `DifferentLine dl dc` to `(line + dl, pLHS + dc)`.

So `DifferentLine 1 0` on alternatives 2..n aligns them under alternative 1,
provided `pLHS` was pushed at alternative 1 and nothing re-arms it in between.

`HsCase` already does `setLayoutBoth $ markAnnotated alts`; `HsLam` does not,
which is why `\case` alternatives currently align to the *enclosing* block.
Move the push into the `MatchGroup` instance so all consumers agree:

```haskell
  exact (MG (origin,an) (L l matches)) = do
    (an0,matches') <- markAnnListA' an $ \a -> do
        m' <- setLayoutBoth $ markAnnotated matches
        return (a,m')
    return (MG (origin, an0) (L l matches'))
```

and drop `setLayoutBoth` from the `HsCase` case. Placement matters: the push
must be *inside* the `markAnnListA'` callback, after brackets and semis are
marked, or an explicit `{` would arm the latch and become the alignment column.

To additionally let alternative 1 be `DifferentLine 1 0` — so that *every*
`Match` carries a uniform delta, which is what a transformation naturally
produces — use the `uExtraDP` route, gated on real layout:

```haskell
    case al_layout an of
      AnnListLayout anc | not (null matches) -> setExtraDP (Just anc)
      _ -> return ()
```

`MatchGroupAnn = AnnList ()` (see `type instance XMG GhcPs b = (Origin, MatchGroupAnn)`
in `compiler/GHC/Hs/Expr.hs`) already carries the field, so no extension-point
change is needed. `enterAnn` clears `uExtraDP` on the next entry, so this
affects only the first `Match`.

Gating on `AnnListLayout` rather than on `isJust` is exactly why §4.1 is a
prerequisite: today `al_anchor` is `Just` for `case x of { a -> b }` too, and
overriding the first alt's delta there would be wrong.

### 5.3 Other `AnnList` consumers

Audit `markAnnList`, `markAnnList'`, `markAnnListA`, `markAnnListA'` and the
`lal_brackets` / `lal_semis` / `lal_rest` lenses. None read the anchor today,
so they need only mechanical updates for the field rename.

---

## 6. Testing

Expect churn in the `.stderr` / `.stdout` expected output of AST-dumping and
exact-print tests. The already-modified files in the working tree give the
shape of the blast radius:

- `testsuite/tests/parser/should_compile/DumpParsedAst.stderr` and siblings
  (`DumpRenamedAst`, `DumpTypecheckedAst`, `DumpParsedAstComments`, `DumpSemis`,
  `KindSigs`, `T20718`, `T20846`)
- `testsuite/tests/ghc-api/exactprint/*.stderr`
- `testsuite/tests/printer/*.stdout`
- `testsuite/tests/module/mod185.stderr`

Specific cases to add or verify:

1. `case`/`\case`/`\cases` in both layout and `{ ; }` form, single and
   multi-alternative, including the leading-`;` form `case x of ; a -> b`
   (D5).
2. Empty `where` (`f x = y where`) — confirm `patch_anchor` is no longer
   reached, and remove it if so (D6).
3. `do { ... }` versus layout `do`, and `mdo`/`rec`.
4. Instance, GADT, type-family and declaration-quote bodies, which gain an
   `AnnListLayout` for the first time.
5. Round-trip (`check-exact`) of a multi-clause `FunBind` in a `where` block,
   confirming clause alignment is preserved.
6. A transformation test that sets every `Match` entry delta to
   `DifferentLine 1 0` on a `case`, a `\case`, and a multi-clause `FunBind`,
   and checks all three print aligned.
7. `-XAlternativeLayoutRule` — `maybe_layout` short-circuits on
   `AlternativeLayoutRuleBit`, so no implicit context is opened. Confirm we
   emit `AnnListNoLayout` (or `AnnListBraces`) and never a bogus
   `AnnListLayout` there.

---

## 7. Suggested commit sequence

Each step should build and pass tests on its own.

1. X Add `AnnListLayout`; change `al_anchor :: Maybe EpaLocation` to
   `al_layout :: AnnListLayout`; mechanically update all construction sites to
   preserve current behaviour (`Just x` → `AnnListLayout x`, `Nothing` →
   `AnnListNoLayout`). Pure refactor, no output change.
2. X Switch the layout-bearing productions to take the location from the
   `vocurly` token, and set `AnnListBraces` in the explicit-brace
   alternatives. Fixes D2, D3, D4, D5. Expect test output churn.
3. X Remove `patch_anchor` / simplify `add_where` in
   `compiler/GHC/Parser/PostProcess.hs`. Fixes D6.
4. ~ Add `AnnListLayout` to the instance / GADT / type-family / decl-quote
   bodies. Fixes part of D1.
   Instance, GADT, type-family and class bodies are done;
   `cvtopbody` (`[d| ... |]`) and the `stmtlist` fix below DONE.
   `stmtlist` still derives its location from `stmtsLoc` rather than the
   `vocurly`, and its explicit-brace alternative still emits `AnnListLayout`
   alongside `ListBraces` — the step 2 change never reached it.
5. X Stop clearing the field in `setAnchorEpaL`. Fixes D7.
6. Move `setLayoutBoth` into the `MatchGroup` exact instance; drop it from
   `HsCase`; add the `setExtraDP` gate. Enables the uniform
   `DifferentLine 1 0` scheme.
7. X *(follow-up)* MultiWayIf: give `XMultiIf GhcPs` an `AnnList`. Completes D1.
8. *(follow-up)* Reconcile `EpLayout` per §4.5.
9. X *(follow-up)* Locate `HsLocalBinds` per §4.7, dropping the `EpAnn` from
   `XHsValBinds` / `XHsIPBinds`. Removes the last `EpAnn AnnList`, retires
   `add_where` / `patch_anchor`, and subsumes step 5.

AZ Notes:
- The vocurly location can be bogus. It is sometimes emitted on the
  occurence of the next non-whitespace token. What are the
  implications?
- can we use virtual semi colons for anything useful?
- X Push setLayoutBoth into the AnnList printing, depending on if layout
    is enabled or not

