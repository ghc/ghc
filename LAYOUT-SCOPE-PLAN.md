# Plan: make layout scopes in `ExactPrint` per-scope and `AnnList`-only

Companion to `ANNLIST-LAYOUT-PLAN.md`. That document covers *where the parser
should record layout* (`AnnList` / `EpLayout`). This one covers *how the exact
printer consumes it*: the `setLayoutBoth` mechanism, a correctness bug in it,
and which call sites survive once layout lives in `AnnList` only.

All references are by function, constructor or field name — grep for the quoted
identifier. No line numbers, deliberately.

---

## 1. How the mechanism works today

`setLayoutBoth` (in `utils/check-exact/ExactPrint.hs`) opens a *layout scope*:

```haskell
setLayoutBoth k = do
  oldLHS <- getLayoutOffsetD
  oldAnchorOffset <- getLayoutOffsetP
  modify (\a -> a { dMarkLayout = True, pMarkLayout = True } )
  let reset = modify (\a -> a { dMarkLayout = False
                              , dLHS        = oldLHS
                              , pMarkLayout = False
                              , pLHS        = oldAnchorOffset } )
  k <* reset
```

Four fields of `EPState` are involved, two per phase:

| phase | offset | arm bit | armed by | consumed by |
| --- | --- | --- | --- | --- |
| print | `pLHS` | `pMarkLayout` | `setLayoutBoth` | `printString`, when called with `layout = True` and characters are actually emitted |
| delta | `dLHS` | `dMarkLayout` | `setLayoutBoth` | `setLayoutStartD`, called from `setPriorEndASTPD` (via `setPriorEndASTD`) |

The intent is "the layout column of this block is the column of the first
token printed inside it". The arm bit is a one-shot: the first consumer sets
the offset and clears the bit.

Note what is and is not stacked. The *offsets* are stacked correctly — the
Haskell call stack plus `k <* reset` saves and restores `pLHS` / `dLHS` in
LIFO order. The *arm bits* are two single global `Bool`s shared by every scope
that is currently waiting for its first token. That is the defect.

---

## 2. Three ways the global arm bit is corrupted

### 2.1 An inner scope that prints nothing disarms the outer scope

`reset` unconditionally writes `False` — it clears the bit rather than
restoring the value it had on entry. So:

```
outer arms -> inner arms (no-op, already True) -> inner exits, clears
           -> the outer scope's first token never sets its offset
```

Scopes that legitimately print nothing exist. `ClassDecl` with no declarations
calls `markAnnListA al $ return ()`. Any `AnnList` with a non-`EpNoLayout`
layout and an empty element list does the same thing implicitly: `f = x where`
(empty `HsValBinds`), an empty explicit-brace block, an empty `MatchGroup`.
Whenever one of those is the first thing reached inside an armed enclosing
scope, the enclosing scope silently loses its layout column and its remaining
output is emitted against the grandparent offset.

### 2.2 Two scopes armed at once — only the innermost wins, and it then un-sets the outer

This needs no empty scopes and already happens. `HsCase` does
`setLayoutBoth $ markAnnotated alts`, and `alts` is a `MatchGroup` whose
`exact` calls `markAnnListA`, which arms again — with nothing printed in
between. The first alternative's token sets `pLHS` and clears the bit; the
inner `reset` then restores `pLHS` to the value it had at inner entry, which is
the *grandparent's* offset, because the outer scope never got the chance to
record the column. The outer scope ends with no layout column of its own.

Today this is harmless only by accident, because the outer `setLayoutBoth` in
`HsCase` has no output after the inner scope. The same accident protects
`HsLet` and `HsCmdLet`: `markEpToken tkLet` prints *before* the inner
`setLayoutBoth` arms, so the outer bit is consumed at the `let` keyword and
`in` is indented correctly. Moving layout into `AnnList` removes exactly that
ordering accident — `markAnnListA` arms after `markEpLayoutO` and the semis, so
an `AnnList` nested immediately inside another armed scope becomes the normal
case rather than the exotic one.

### 2.3 The two arm bits are consumed at different points and can drift apart

`pMarkLayout` is consumed inside `printString`, and only when characters are
actually emitted with `layout = True`. `dMarkLayout` is consumed in
`setLayoutStartD`, reached from `setPriorEndASTPD`, which `printStringAtRsC`
runs *after* `printStringAtLsDelta` — and `printStringAtLsDelta` can print
nothing at all when `isGoodDeltaWithOffset` rejects the delta. A token can
therefore resolve `dLHS` while leaving `pMarkLayout` armed. Once the two are
out of step, whichever `reset` fires first clears both, and one of the two
scopes ends with an unset offset.

Comments are consistent and need no change: they resolve neither side, so they
cause no drift between the two. See §4.1.1 for why, and for the one consequence
that does follow — an armed scope stays armed across comment output, so the
resolution point can drift some distance from where the scope began, and any
comment printed in that gap is positioned against the enclosing offset.

---

## 3. Fix: a stack of pending layout frames

Not a stack of columns — that already exists implicitly. A stack of *pending
frames*, with the rule that **a single printed token resolves every currently
pending frame**, since nested scopes that arm with no output between them do in
fact start at the same token.

Replace `pLHS` / `pMarkLayout` and `dLHS` / `dMarkLayout` in `EPState` with two
frame stacks:

```haskell
data LayoutFrame = LayoutFrame
  { lfSaved    :: !LayoutStartCol        -- effective offset at scope entry
  , lfResolved :: !(Maybe LayoutStartCol) -- column of the first token printed in this scope
  }

-- EPState
--   pLayout, dLayout :: ![LayoutFrame]

effectiveLayout :: [LayoutFrame] -> LayoutStartCol
effectiveLayout (f:_) = fromMaybe (lfSaved f) (lfResolved f)
effectiveLayout []    = LayoutStartCol 1
```

Operations:

- **push** (`setLayoutBoth`): push `LayoutFrame (effectiveLayout stack) Nothing`
  onto both stacks, run the action, pop both. Use `bracket`-style sequencing so
  a pop cannot be skipped.
- **resolve** (`printString` for `pLayout`, `setLayoutStartD` for `dLayout`):
  set `lfResolved = Just c` on *every* frame whose `lfResolved` is `Nothing`.
  Leave already-resolved frames alone. On the print side this must stay behind
  the existing `layout = True` guard, so that comment and whitespace output does
  not resolve a frame (§4.1.1).
- **read** (`getLayoutOffsetP` / `getLayoutOffsetD`): `effectiveLayout`.
- **query** (for `HsQuasiQuote`, see §4): "is the top frame unresolved?",
  replacing the current direct read of `pMarkLayout`.

Note that resolution affects more than the top frame. It is bounded, though,
by an invariant worth stating and asserting:

> **Pending frames form a contiguous run at the tip of the stack.**

It holds because `push` always adds `lfResolved = Nothing` on top, and
`resolve` clears *every* pending frame at once, so a resolved frame can never
come to sit above an unresolved one; `pop` only removes from the top, which
preserves it. Two consequences:

- `resolve` can stop at the first resolved frame rather than walking the whole
  stack — `span (isNothing . lfResolved)`, map `Just c` over the prefix,
  reattach the tail. The stacks are shallow either way, so this is about
  intent, not cost, but the shape documents the invariant.
- Every frame in the pending run carries the *same* `lfSaved`, since each push
  copies `effectiveLayout` of a stack whose top is unresolved, which is that
  same saved value. The run is therefore uniform: on resolution the frames all
  take column `c`, and as they pop, each restores the same value to the frame
  below. (This is what makes §2.2 come out right.)

Keep the traversal strict to avoid retaining thunks in `EPState`.

Two things would break the invariant, and each needs a deliberate decision
rather than an accident:

- `setLayoutOffsetP` mutating the top frame while outer frames are pending —
  the `HsQuasiQuote` use. It overwrites an effective value rather than
  resolving, so it leaves the run pending but no longer uniform. It saves and
  restores around a single `printStringAdvance`, so it is safe as written; do
  not generalise it without revisiting this.
- The §4.1 follow-up of keying resolution on the first token of an `AnnList`'s
  first element. That would resolve an inner `AnnList` frame while an outer
  `withTokenAnchor` frame stayed pending, putting a resolved frame above an
  unresolved one. If that follow-up is taken, `resolve` must go back to
  traversing the whole stack, and this invariant must be dropped rather than
  quietly falsified.

What each property buys, matching §2 one for one:

- an empty scope pops unresolved and leaves outer frames pending (fixes §2.1);
- sibling-armed scopes all resolve to the same column, so popping the inner one
  restores the outer's *resolved* value rather than the grandparent's
  (fixes §2.2);
- the `p` and `d` stacks resolve independently at their own consumption sites,
  so drift on one side no longer clears the other (fixes §2.3).

A cheaper half-fix — have `reset` restore the saved arm bits instead of writing
`False` — addresses §2.1 only. It leaves §2.2 wrong: the outer scope would be
re-armed and then resolved by some *later* token, possibly on a later line.
Not recommended, but worth knowing if the full change has to be split.

### 3.1 Migration notes

- `getLayoutOffsetP`, `getLayoutOffsetD`, `setLayoutOffsetP` and
  `setLayoutStartD` are the only accessors; keep their signatures so call sites
  outside this change are untouched. `setLayoutOffsetP` becomes "overwrite the
  top frame's effective value" — it is used only by `HsQuasiQuote`.
- The initial `EPState` currently sets `pLHS = LayoutStartCol 1`,
  `dLHS = LayoutStartCol 1`, `pMarkLayout = False`, `dMarkLayout = False`.
  Start both stacks with a single resolved frame at column 1 so
  `effectiveLayout` never sees an empty stack in practice.
- Keep the existing `debugM` traces, extended to print stack depth; the
  interleaving of arm/resolve/pop is the thing you will actually be reading
  when a test moves.

---

## 4. Which call sites survive

`setLayoutBoth` has seven call sites. They fall into three groups.

### 4.1 Keep: `markAnnListA`

`markAnnListA` opens a scope for any `AnnList` whose `EpLayout` is not
`EpNoLayout`. This is the single `AnnList`-driven entry point and the goal
state of `ANNLIST-LAYOUT-PLAN.md`.

Two open questions belong here rather than in a call site:

- whether `EpExplicitBraces` should open a scope at all — the code currently
  does, with a comment saying it strictly should not;
- whether resolution should be keyed on the first token of the list's *first
  element* rather than on "the next resolving print". A per-`AnnList` frame
  makes that statable; today it is not.

Note that "the next resolving print" is narrower than "the next output".
Comments do not resolve a scope on either side (§4.1.1), and neither does
whitespace: `printWhitespace` / `padUntil` / `newLine` all go through
`printString False`.

#### 4.1.1 Comments do not participate in layout resolution

This is worth stating explicitly, because it is easy to assume otherwise and it
constrains the design in §3.

On the print side, comment output reaches `printString` only via
`printCommentAt`, which calls `printString False`. The arm is consumed under
`when (pMarkLayout && layout)`, so a comment neither clears `pMarkLayout` nor
sets `pLHS`. On the delta side, `printOneComment` and `printQueuedComment`
never call `setPriorEndASTD` / `setPriorEndASTPD`, so `setLayoutStartD` is not
reached and `dLHS` is untouched. (`printString` does call `setPriorEndD`
unconditionally, so comments do advance `dPriorEndPosition` — but that is the
running prior-end, not the layout column.) The two sides agree, so comments
cause no `p`/`d` drift of the kind in §2.3.

The behaviour is right, and the frame stack must preserve it: **resolve only on
`printString` with `layout = True`, never on comment or whitespace output.**

There is a consequence to be aware of rather than to fix here. A comment
printed while a scope is armed is *positioned* against the enclosing offset:
`printQueuedComment` reads `getLayoutOffsetP` and `printOneComment` calls
`adjustDeltaForOffsetM`, which reads `dLHS`, and at that moment neither has
been set to the new block's column yet. So for a block whose first output is a
comment on its own line — a comment as the first thing inside a `where` or `do`
block — the comment is offset relative to the enclosing block while every
token after it is offset relative to the new one. Under the current one-shot
this is invisible in a round trip; it becomes visible when the enclosing
construct is moved. Whether the comment should instead be pulled into the new
scope is a separate question from this plan, and answering it is easier once
resolution is a per-frame decision.

### 4.2 Delete: redundant scopes already covered by an `AnnList`

- **`HsCase`**, which wraps `markAnnotated alts` in `setLayoutBoth`. `alts` is
  a `MatchGroup`, and `MatchGroupAnn = AnnList`, so `markAnnListA` in the
  `MatchGroup` instance already opens the scope. The proof it is vestigial is
  `HsCmdCase`, which has no `setLayoutBoth` and works. This is step 6 of
  `ANNLIST-LAYOUT-PLAN.md`.
- **`IPBinds`** (the `ExactPrint (HsIPBinds GhcPs)` instance). Its parent, the
  `HsIPBinds` case of `HsLocalBinds`, already calls `markAnnListA`, and
  `XHsIPBinds` carries the `AnnList`.
- **The inner `setLayoutBoth` around `binds` in `HsLet` and `HsCmdLet`.**
  `XHsValBinds` and `XHsIPBinds` carry the `AnnList` for those binds.

Deleting these three is a behaviour-preserving simplification once §3 is in
place; without §3 the deletions are individually safe but the double-arming
they currently cause is masking, not avoiding, the bug.

### 4.3 Keep, but rename and re-found: `HsLet` / `HsCmdLet` outer scope

This one cannot use `AnnList` and should not be made to. The scope is anchored
at the `let` *keyword*, not at the layout block:

```haskell
f = let x = 1        -- binds AnnList layout column = column of x
        y = 2
    in x + y         -- `in` aligns with `let`
```

The outer scope is consumed by `markEpToken tkLet`, pinning the offset at the
`let` keyword so that `in` and the body track the `let` when the whole
expression is moved. There is no `AnnList` on `XLet` (it is
`(EpToken "let", EpToken "in")`), and adding one would be wrong twice over: it
would assert a layout context the lexer never opened, and it would sit
alongside the binds' `AnnList` that already describes the real one.

So "layout scope" in `ExactPrint` is doing two jobs, and only one of them is
`AnnList`'s:

1. **A layout context** — a block whose members align at a column the lexer
   chose. After §4.2 this is entirely `AnnList`-driven, via `markAnnListA`.
2. **A token-alignment anchor** — a trailing keyword that must follow its
   opening keyword across lines. `let` / `in` is the only implemented instance.

Give job 2 its own name so the distinction is visible in the code:

```haskell
-- | Anchor subsequent output to the column of the next token printed.
-- NOT a layout context: for let/in, where a trailing keyword must track its
-- opener when the expression is moved. Only valid when the anchored output is
-- at or to the right of the anchor column -- see §4.4 for why if/then/else
-- cannot use this.
withTokenAnchor :: (Monad m, Monoid w) => EP w m a -> EP w m a
```

`withTokenAnchor` pushes the same frame type as `markAnnListA` onto the same
stacks — the frames must interleave, and the stack is what makes the
interleaving correct. The `HsLet` sequence becomes: `withTokenAnchor` arms,
`let` resolves it, the binds' `AnnList` arms and is resolved by the first bind,
popping it restores the `let` column, and `in` prints against that column.
Today that sequence only works because `markEpToken tkLet` happens to print
before the inner scope arms.

### 4.4 A fourth case, previously tried and removed: `if` / `then` / `else`

`HsIf`, `HsCmdIf` and the `DoAndIfThenElse` semis have no anchor today, so
`then` and `else` deltas are taken against the enclosing offset. This is the
same *shape* as `let` / `in`, and it will never have an `AnnList` — but it is
not simply an unfinished case. Alignment based on the `if` keyword existed
previously and was deliberately removed, and the reason it was removed is a
constraint on `withTokenAnchor`, so do not reintroduce it without reading the
rest of this section.

The motivating case is an `if` pushed far to the right — typically introduced
by `$` — with the arms on following lines at a *smaller* column than the `if`
itself:

```haskell
f = g $ if cond
     then a          -- to the left of the `if` keyword
     else b
```

Anchoring at `if` makes those arms sit left of the anchor, and the offset
machinery expresses that as a negative column delta:
`adjustDeltaForOffset` stores `c - colOffset` for a `DifferentLine` delta and
`undelta` replays `colOffset + dc`. Negative `dc` is not rejected anywhere —
`isGoodDelta` checks only the line component for `DifferentLine` — so nothing
fails loudly. It degrades quietly instead: `padUntil` can only pad forward, so
whenever the replayed column falls at or behind the current cursor (including
any column below 1) the token is emitted wherever the cursor already is and the
alignment is lost. Without the anchor, the same arms are expressed against the
enclosing block's offset, which is to their left, and `dc` stays positive.

So `withTokenAnchor` as described in §4.3 is sufficient for `let` / `in`, where
`in` is at or right of the `let` column by construction, and *not* sufficient
here. Reviving `if` / `then` / `else` alignment needs one of:

- making negative column offsets work end to end — every `padUntil` /
  `printWhitespace` path would have to have a defined behaviour for a target
  left of the cursor, which today means "do nothing"; or
- the §6 follow-up approach applied to `if` rather than `let`: store `then` and
  `else` as `EpaDelta` relative to the `if` token directly, so the relationship
  is a token-to-token delta and no layout offset is involved.

The second is the better shape, and it is the same change §6 proposes for
`in` — which is an argument for doing that follow-up before revisiting this.
Either way, out of scope for this plan; recorded here so the history is not
lost and the removal is not mistaken for an oversight.

---

## 5. Two non-`AnnList` touch points to preserve

- **`HsQuasiQuote`** reads `pMarkLayout` directly to decide whether to
  temporarily zero the offset while printing the quote body (a quasiquote does
  not honour layout offsets). It is a reader, not a scope opener, but it must
  be ported to the query described in §3, and it uses `setLayoutOffsetP` to
  save and restore.
- **The module top level.** `hsmodLayout :: EpLayout` lives directly on
  `XModulePs`, not inside an `AnnList`, and the `HsModule` instance never opens
  a scope — `hsmodLayout` is consumed only by `utils/check-exact/Parsers.hs`
  and `utils/check-exact/Utils.hs` for comment and EOF handling. This is
  harmless, because the top-level column is 1 and never moves, but it is the
  one lexer layout context deliberately left without an `AnnList`. Record that
  in a comment rather than leaving it to be rediscovered.

All eight lexer layout contexts listed in §1 of `ANNLIST-LAYOUT-PLAN.md` now
have an `AnnList`: `do` / `mdo` (`DoAnn`), `of` and `\case` / `\cases`
(`MatchGroupAnn`), `let` and `where` (`XHsValBinds` / `XHsIPBinds`), `rec`
(`XRecStmt`), MultiWayIf (`XMultiIf`), `[d|` (`XDecBrL`), plus the class,
instance, GADT and closed-type-family bodies (`acd_list`, `acid_decls`,
`andd_list`, `XClosedTypeFamily`). Modulo the module top level above, no layout
context is unrepresented.

---

## 6. Suggested commit sequence

Each step should build and pass the exactprint tests on its own.

1.X Introduce `LayoutFrame` and the two stacks in `EPState`, with
   `effectiveLayout`, the resolve-all-pending operation, and the pending query.
   Rewrite
   - [X] `setLayoutBoth`, -> becomes `setLayoutBoth'`
   - [X] `getLayoutOffsetP`,
   - [X] `getLayoutOffsetD`,
   - [X] `setLayoutOffsetP`,
   - [X] `setLayoutStartD` and the
   - [X] `printString` arm-consumption
          in terms of them.
   - [ ] Port `HsQuasiQuote` to the query.
   No call-site changes.
   Expect no test churn: for every currently-working nesting the new scheme
   computes the same columns.
2. Delete the redundant `setLayoutBoth` calls in `HsCase`, `IPBinds`, and the
   inner ones in `HsLet` / `HsCmdLet` (§4.2). Coordinate with step 6 of
   `ANNLIST-LAYOUT-PLAN.md`, which is the same change for `HsCase`.
3. Rename the surviving `HsLet` / `HsCmdLet` outer scope to `withTokenAnchor`
   and document it as an alignment anchor, not a layout context (§4.3).
4. Add the module-top-level comment (§5) and settle the `EpExplicitBraces`
   question in `markAnnListA` (§4.1).
5. *(follow-up)* Consider keying `AnnList` resolution on the first token of the
   first element rather than the next print anywhere.
6. *(follow-up)* Remove the last non-`AnnList` layout user entirely by storing
   the `in` token as an `EpaDelta` relative to the `let` token instead of to
   the enclosing offset. That deletes `withTokenAnchor` from `HsLet`, but it is
   a parser / annotation change rather than an `ExactPrint` one.

## 7. Tests worth adding

The bugs in §2 are invisible in round-trip tests, because a faithful round trip
re-derives the same deltas it was given. They show up when a fragment is
*moved*. Target:

- an empty laid-out block as the first element of an enclosing laid-out block —
  `class` with no declarations, `f = x where` with no bindings, an empty
  explicit-brace block — then a transformation that re-indents the enclosing
  block;
- `let` / `in` inside a `do` block, moved to a deeper indent, checking that
  `in` and the body follow the `let`;
- a `case` whose alternatives contain a `where` containing another `case`,
  again under re-indentation, to exercise three nested frames;
- a construct where `printStringAtLsDelta` rejects the delta via
  `isGoodDeltaWithOffset` while a scope is armed, to pin §2.3.
