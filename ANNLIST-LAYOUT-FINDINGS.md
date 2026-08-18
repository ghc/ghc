+# Findings: layout capture in `AnnList` at parse time
+
+2026-10-28, at jj lvnuyovt
+
+File to reproduce problems
+-----------------------------------
+{-# LANGUAGE TemplateHaskell, ImplicitParams, MultiWayIf, LambdaCase, PatternSynonyms #-}
+module Lay2 where
+a = x where
+     p = 1
+     q = 2
+c = case x of
+  ; p -> q
+  r -> s
+d = let ?x = 1
+        ?y = 2 in ?x
+e = [d| |]
+e2 = [d| f = 1 |]
+f = if | x -> y
+       | z -> w
+pattern P x <- Just x where
+  P x = Just x
+-----------------------------------
+
+An audit of every layout-opening production in `compiler/GHC/Parser.y`
+against what it records in `AnnList` (`al_layout :: EpLayout`, `al_semis`).
+It complements `ANNLIST-LAYOUT-PLAN.md`, and corrects it where that plan is out
+of date (see §7).
+
+As in the plan, everything is referenced by production, function or
+constructor name, not by line number. The backpack `unitbody` and the
+header-only parsers stay out of scope.
+
+Probe files live in the session scratchpad:
+`/tmp/claude-1000/-home-alanz-mysrc-git-haskell-org-ghc/b86d6aba-94a8-4d83-992f-c1e5d9e5fae4/scratchpad/probe/`
+(`Gadt.hs`, `TF.hs`, `Do.hs`, `Quote.hs`, `Lay2.hs`, and the dump `lay2.out`).
+These results come from a stage1 build of commit `227da581e66`.
+
+---
+
+## 0. Ground rules
+
+These are the rules the findings are checked against. They come from the lexer
+(`maybe_layout`, `new_layout_context`, `do_layout_left` in
+`compiler/GHC/Parser/Lexer.x`) and from the policy in commit f3b0a5bbe45.
+
+1. **The layout location is the open token.** That is the `vocurly`, or for
+   MultiWayIf the first `|`. The `vocurly` is a zero-width span at the first
+   token after the layout keyword that is neither whitespace nor a comment.
+   Comments are lexed in every start code, so they never trigger the `()`
+   rule. The span's start column is the layout column that
+   `new_layout_context` pushed.
+2. **An empty block records `EpNoLayout`.** The `vocurly` location is only ever
+   misleading for an empty block. That happens in two ways:
+   - the `layout_left` path, where the next token is at or left of the
+     enclosing column, so the lexer emits `vocurly`/`vccurly` at a token
+     *outside* the block;
+   - an immediate `close → error → popContext`, as in `[d| |]` or `let in`,
+     where the `vocurly` lands on the closing token.
+
+   Both cases have empty contents, so "empty ⇒ `EpNoLayout`" is exactly the
+   right guard. `epVirtualBraces` implements it.
+3. **`al_semis` holds every leading `;` the user wrote.** A `;` that follows an
+   item is a trailing annotation on that item. Virtual (zero-width) semis are
+   never recorded.
+
+The exact printer reads only the *start* of the layout location
+(`ss2delta priorEndAfterComments r` in `enterAnn` and `markEpTokenExtra`).
+So findings F1 and F3 do not change printed output today. They do change AST
+dumps, and they matter for consistency and for any future consumer that uses
+the end of the span.
+
+### Sites that already conform
+
+The following take the location from the open token and guard on empty
+contents:
+
+- the two `vocurly` alternatives of `ty_fam_inst_eqn_list`
+- `decllist_cls`
+- `decllist_inst`
+- `gadt_constrlist`
+- `stmtlist` (`do`, `mdo`, `rec`)
+
+`body` uses the `vocurly` too, but `hsmodLayout` is not an `AnnList`.
+
+---
+
+## F1. Four productions take the layout location from the contents
+
+### Finding
+
+| production | construct | location used today |
+| --- | --- | --- |
+| `decllist`, `vocurly` alternative | `let` / `where` binds | `fstOf3 $ unLoc $2`, built up inside `decls` |
+| `where_decls`, `vocurly` alternative | pattern-synonym `where` | `glR $3` (the `decls` span) |
+| `binds`, `vocurly dbinds close` | implicit-parameter `let` | `glR $2` (the `dbinds` span) |
+| `altslist`, `vocurly alts close` | `of`, `\case`, `\cases` | `glR $2` (the `alts` span) |
+
+Observed in `lay2.out`:
+
+```
+where binds      (lines 3-5)   EpVirtualBraces (EpaSpan { Lay2.hs:(4,6)-(5,10) })
+case of ; ...    (lines 6-8)   EpVirtualBraces (EpaSpan { Lay2.hs:(7,3)-(8,8) })
+let ?x .. ?y     (lines 9-10)  EpVirtualBraces (EpaSpan { Lay2.hs:(9,9)-(10,14) })
+pattern .. where (line 16)     EpVirtualBraces (EpaSpan { Lay2.hs:16:3-14 })
+```
+
+For comparison, `e2 = [d| f = 1 |]`, which does use the `vocurly`, records
+the zero-width `Lay2.hs:12:10`.
+
+In each of these four cases the start column agrees with the `vocurly`. That
+holds even for `case x of ; p -> q`, because `pal` widens the location to cover
+the first leading `;`, and the leading `;` is where the lexer measured the
+layout column. But the value is a span over the whole block rather than the
+open token. It is built differently from every other site. And it is the only
+reason `decls` still threads an `EpaLocation` in its first component, which
+is assembled from `glR` / `glEE` / `glEEz`.
+
+`ANNLIST-LAYOUT-PLAN.md` §7 marks step 2 ("take the location from the
+`vocurly` token") as done, but these four sites were not converted.
+
+### Proposed solution
+
+Switch each site to the open token and keep the contents only for the empty
+check:
+
+```haskell
+-- decllist
+        |     vocurly    decls close   { if null (fstOf2 ..) && isNilOL (..)
+                                            then noLoc (pal $ AnnList (epVirtualBraces (glR $1) $2) ...)
+                                            else sLL $1 $2 (pal $ AnnList (epVirtualBraces (glR $1) $2) ...) }
+
+-- where_decls
+        | 'where' vocurly decls close { ... pal $ AnnList (epVirtualBraces (glR $2) $3) ... }
+
+-- binds / dbinds
+        |     vocurly    dbinds close   { ... IPBinds (pal $ AnnList (epVirtualBraces (glR $1) $2) []) ... }
+
+-- altslist
+        | vocurly    alts(PATS)  close { ... (pal $ AnnList (epVirtualBraces (glR $1) $2) (fst $ unLoc $2)) ... }
+```
+
+Follow-up, which is the plan's §4.2 "NOT DONE" item: once `decllist` stops
+reading `fstOf3`, remove the `EpaLocation` component from `decls`. Its type
+becomes `Located ([EpToken ";"], OrdList (LHsDecl GhcPs))`, which matches
+`decls_cls` / `decls_inst`, and the `glR`, `glEE` and `glEEz` calls inside it
+go away. `where_decls` and `mkPatSynMatchGroup` need the corresponding
+`fstOf3/sndOf3/thdOf3` → `fst/snd` updates.
+
+With the location now a zero-width point, `pal`'s widening to the first semi
+still leaves the *start* unchanged, because the `vocurly` is at or before any
+leading `;`. Whether `pal` should keep widening at all is a separate
+simplification. It only exists to cope with content-derived anchors, so it
+could be reduced to the "not a `RealSrcSpan` ⇒ `EpNoLayout`" check.
+
+Expected churn: AST-dump tests (`DumpParsedAst*`, `DumpSemis`, the
+`ghc-api/exactprint` dumps). There should be no `check-exact` output change.
+
+---
+
+## F2. `cvtopbody` records layout for an empty `[d| |]`
+
+### Finding
+
+```haskell
+cvtopbody
+        :  '{'            cvtopdecls0 '}'      { ... }
+        |      vocurly    cvtopdecls0 close    { (AnnList (EpVirtualBraces (glR $1)) [], ...) }
+```
+
+This is the only `vocurly` site that builds `EpVirtualBraces` directly,
+bypassing both `epVirtualBraces` and `pal`. For `[d| |]`:
+
+1. `layout_token ITopenDecQuote` pushes `layout`.
+2. The next token is `|]`, so the `()` rule fires there and opens a context at
+   its column.
+3. `cvtopdecls0` is empty, and `close` matches via `error`, which pops the
+   context.
+
+The result in `lay2.out`:
+
+```
+e = [d| |]   (line 11)   EpVirtualBraces (EpaSpan { Lay2.hs:11:9 })    -- column of `|]`
+```
+
+So we claim a layout block anchored on the closing bracket. That breaks
+rule 0.2 and is inconsistent with every other site after f3b0a5bbe45.
+
+### Proposed solution
+
+Guard on the contents the same way the other sites do:
+
+```haskell
+        |      vocurly    cvtopdecls0 close
+              { (pal $ AnnList (epVirtualBraces (glR $1) (L (locA (listLocation $2)) ())) []
+                , sLLa $1 (listLocation $2) $2) }
+```
+
+`listLocation []` is `noSrcSpan`, so `epVirtualBraces` yields `EpNoLayout` for
+the empty quote.
+
+Add `[d| |]` and `[d|\n  |]` to a printer test and check that the dump shows
+`EpNoLayout` and the round trip is unchanged.
+
+---
+
+## F3. MultiWayIf records the guard block, not the opening `|`
+
+### Finding
+
+```haskell
+ifgdpats
+         |     gdpats close   {% ... sL1 $1 (AnnList (EpVirtualBraces (glR $1)) [], unLoc $1) }
+```
+
+`$1` is the whole `gdpats` span. In `lay2.out`:
+
+```
+if | x -> y
+   | z -> w   (lines 13-14)   EpVirtualBraces (EpaSpan { Lay2.hs:(13,8)-(14,15) })
+```
+
+The start column is correct, because it is the first `|`, which is the token
+`new_layout_context` measured. But the location is a span over the contents,
+the same problem as F1. There is no `vocurly` to use here: see
+`ANNLIST-LAYOUT-PLAN.md` §1(b).
+
+### Proposed solution
+
+Narrow the location to the first character, which is the opening `|`:
+
+```haskell
+         |     gdpats close   {% runPV $1 >>= \ $1 ->
+                                 return $ sL1 $1 (AnnList (EpVirtualBraces (spanAsAnchor $ srcSpanFirstCharacter $ getLoc $1)) []
+                                                 , unLoc $1) }
+```
+
+`srcSpanFirstCharacter` is in `GHC.Types.SrcLoc`. An alternative is to take
+the `ga_vbar` token of the first-parsed `GRHS`'s `GrhsAnn`. Note that `gdpats`
+is accumulated in reverse, so that is the *last* element of the `NonEmpty`
+here. The first-character approach avoids that subtlety.
+
+`gdpats` is non-empty by construction, so no empty-contents guard is needed.
+
+---
+
+## F4. Semicolons dropped in GADT bodies
+
+### Finding
+
+```haskell
+gadt_constr
+    -- TODO:AZ capture the optSemi. Why leading?
+        : optSemi modifiers con_list '::' sigtype
+                {% do { ...; mkGadtDecl (comb3 $2 $3 $>) ... } }   -- $1 never used
+```
+
+```haskell
+gadt_constrlist
+        ...  AnnList (EpExplicitBraces ...) []         -- al_semis hard-coded []
+        ...  pal $ AnnList (epVirtualBraces ...) []    -- likewise
+```
+
+The `optSemi` exists because `gadt_constrs` is right-recursive with a single
+`';'` separator (`gadt_constr ';' gadt_constrs`). The only way to accept `;;`,
+or a leading `;`, is to let each constructor eat one optional `;` in front.
+That token is then discarded. The `check-exact` round trip of `Gadt.hs`
+fails:
+
+```
+data T where { ; C1 :: T ;; C2 :: T }   ->   data T where {   C1 :: T ;  C2 :: T }
+data U where
+  C3 :: U ;; C4 :: U                    ->     C3 :: U ;  C4 :: U
+```
+
+### Proposed solution
+
+Restructure `gadt_constrs` to the left-recursive shape used by `decls_inst`
+and `decls_cls`, and delete `optSemi` from `gadt_constr`:
+
+```haskell
+gadt_constrs :: { Located ([EpToken ";"], OrdList (LConDecl GhcPs)) }
+        : gadt_constrs ';' gadt_constr
+              {% if isNilOL (snd $ unLoc $1)
+                   then return (sLL $1 $> ((fst $ unLoc $1) ++ msemiA $2, unitOL $3))
+                   else case snd $ unLoc $1 of
+                     SnocOL hs t -> do
+                       t' <- addTrailingSemiA t (epTok $2)
+                       return (sLL $1 $> (fst $ unLoc $1, snocOL hs t' `snocOL` $3)) }
+        | gadt_constrs ';'
+              {% if isNilOL (snd $ unLoc $1)
+                   then return (sLZ $1 $> ((fst $ unLoc $1) ++ msemiA $2, nilOL))
+                   else case snd $ unLoc $1 of
+                     SnocOL hs t -> do
+                       t' <- addTrailingSemiA t (epTok $2)
+                       return (sLZ $1 $> (fst $ unLoc $1, snocOL hs t')) }
+        | gadt_constr         { sL1a $1 ([], unitOL $1) }
+        | {- empty -}         { noLoc ([], nilOL) }
+```
+
+Then `gadt_constrlist` passes `fst $ unLoc $3` as `al_semis` in both
+alternatives, and `fromOL . snd` as the constructors.
+
+- The `;;` case becomes two trailing `AddSemiAnn`s on the preceding
+  constructor, as it already does for `decls_inst`.
+- `checkEmptyGADTs` needs only its pattern adjusted.
+- Check for new happy conflicts. `optSemi` is also used by `if … then … else`,
+  so leave that production in place.
+
+`ExactPrint`'s `ConDecl` printing should need no change, since trailing semis
+are already printed from the `SrcSpanAnnA`.
+
+---
+
+## F5. Leading semicolons dropped in closed-type-family bodies
+
+### Finding
+
+```haskell
+ty_fam_inst_eqns :: { Located [LTyFamInstEqn GhcPs] }
+        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
+              {% ... case unLoc $1 of
+                       [] -> return (sLL $1 $> (L loc eqn : unLoc $1))   -- ';' discarded
+                       ... }
+        | ty_fam_inst_eqns ';'
+              {% case unLoc $1 of
+                   [] -> return (sLZ $1 $> (unLoc $1))                    -- ';' discarded
+                   ... }
+```
+
+`ty_fam_inst_eqn_list` then builds `AnnList ... []`. The round trip of
+`TF.hs` fails:
+
+```
+type family F a where { ; F Int = Bool ; F a = Char }
+  ->
+type family F a where {   F Int = Bool ; F a = Char }
+```
+
+### Proposed solution
+
+Make `ty_fam_inst_eqns` carry the leading semis, in the same shape as F4:
+
+```haskell
+ty_fam_inst_eqns :: { Located ([EpToken ";"], [LTyFamInstEqn GhcPs]) }   -- eqns reversed
+        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
+              {% case snd $ unLoc $1 of
+                   [] -> return (sLL $1 $> ((fst $ unLoc $1) ++ msemiA $2, [$3]))
+                   (h:t) -> do
+                     h' <- addTrailingSemiA h (epTok $2)
+                     return (sLL $1 $> (fst $ unLoc $1, $3 : h' : t)) }
+        | ty_fam_inst_eqns ';'
+              {% case snd $ unLoc $1 of
+                   [] -> return (sLZ $1 $> ((fst $ unLoc $1) ++ msemiA $2, []))
+                   (h:t) -> do
+                     h' <- addTrailingSemiA h (epTok $2)
+                     return (sLZ $1 $> (fst $ unLoc $1, h' : t)) }
+        | ty_fam_inst_eqn             { sLL $1 $> ([], [$1]) }
+        | {- empty -}                 { noLoc ([], []) }
+```
+
+In `ty_fam_inst_eqn_list`, pass `fst $ unLoc $2` as `al_semis` in the `'{'`
+and `vocurly` alternatives, and `reverse . snd` as the equations. The `'..'`
+alternatives are unaffected.
+
+This also fixes an existing oddity: in the `[]` case, the old code combined a
+`sLL $1 $>` location with an equation that had lost its preceding `;`.
+
+---
+
+## F6. MultiWayIf under `-XAlternativeLayoutRule` *(unverified)*
+
+### Finding
+
+`maybe_layout` returns early when `AlternativeLayoutRuleBit` is set, so
+`ITif` never pushes `layout_if` and no layout context is opened for the guard
+block. The other layout constructs are safe under ALR, because
+`alternativeLayoutRuleToken` synthesises its own `ITvocurly`. MultiWayIf has
+no synthesised token, yet the `gdpats close` alternative of `ifgdpats`
+unconditionally records `EpVirtualBraces`.
+
+I have not tested whether `if | …` even parses under ALR. This is test 7 of
+`ANNLIST-LAYOUT-PLAN.md` §6.
+
+### Proposed solution
+
+First add the test. If the parse succeeds, gate the annotation:
+
+```haskell
+         |     gdpats close   {% do { alr <- getBit AlternativeLayoutRuleBit
+                                    ; $1 <- runPV $1
+                                    ; let lo = if alr then EpNoLayout
+                                               else EpVirtualBraces (spanAsAnchor $ srcSpanFirstCharacter $ getLoc $1)
+                                    ; return $ sL1 $1 (AnnList lo [], unLoc $1) } }
+```
+
+---
+
+## F7. Leading semicolons are handled three different ways
+
+### Finding
+
+| production | how a leading `;` is added | zero-width `;` becomes |
+| --- | --- | --- |
+| `decls` | `msemiA $2` | omitted |
+| `alts` / `alts1` | `mzEpTok` | a `NoEpTok` element |
+| `decls_cls`, `decls_inst` | `[mzEpTok $2]` | a `NoEpTok` element |
+| `stmts` | `snocOL (epTok $2)` | a zero-width `EpTok` |
+
+`markEpTokenExtra` in `utils/check-exact/ExactPrint.hs` has only an `EpTok`
+clause. That is the function used to print semis inside an
+`EpVirtualBraces` list, so a `NoEpTok` in `al_semis` there would hit an
+incomplete pattern.
+
+This is **not reachable today**. The lexer never emits a virtual `;` before
+the first item of a block, because `do_bol` only fires on later lines. It is
+a latent hazard, and an inconsistency that F4 and F5 would otherwise copy.
+
+### Proposed solution
+
+- Use `msemiA` everywhere a leading semi is accumulated, so virtual semis are
+  never stored, matching rule 0.3:
+  - `alts`: `(mzEpTok $1) : xs` → `msemiA $1 ++ xs`
+  - `alts1`, `decls_cls`, `decls_inst`: `++ [mzEpTok $2]` → `++ msemiA $2`
+  - `stmts`: `` `snocOL` (epTok $2) `` → `` `appOL` toOL (msemiA $2) ``
+- Defensively, add `markEpTokenExtra NoEpTok = return NoEpTok`.
+
+---
+
+## F8. Minor cleanups
+
+- **`pal` is applied twice for `let`/`where` binds.** It runs once in
+  `decllist` and again in `cvBindGroup`, which calls `patchAnnlistLayout`.
+  `cvBindGroup` has a single caller (`binds`). Drop the call there.
+- **`pal` on `dbinds` does nothing useful.** The semis are always `[]`, so the
+  only effect is the non-`RealSrcSpan` fallback that `epVirtualBraces` already
+  performs. This can be dropped after F1.
+- **`stmtsAnchor` is dead.** It is exported from `GHC.Parser.PostProcess` but
+  has no callers. Remove it.
+- **`body2` uses a placeholder location.** The `missing_module_keyword`
+  alternative records `EpVirtualBraces noSpanAnchor`. This is out of
+  `AnnList` scope (`hsmodLayout`), but it is the one remaining
+  `EpVirtualBraces` with no real location. `getEpaLocationCol` falls back to
+  column 1, which happens to be correct. Consider a comment, or the location
+  of the first token of `top`.
+
+---
+
+## 7. Corrections to `ANNLIST-LAYOUT-PLAN.md`
+
+- **§4.1:** the new `AnnListLayout` type was not introduced. `AnnList` reuses
+  `EpLayout`, and `EpVirtualBraces` now carries an `EpaLocation` instead of an
+  `Int`. That also settles most of §4.5.
+- **§2.2 table and §3:** these describe the pre-change state. They are useful
+  as history, but should not be read as current.
+- **§7 step 2:** marked done, but incomplete. See F1 and F3.
+- **§7 step 4 note:** it says `stmtlist` still uses `stmtsLoc` and emits layout
+  alongside `ListBraces`. Both are fixed: it now uses `glR $1` and
+  `EpExplicitBraces`.
+- **AZ note on bogus `vocurly` locations:** answered by rule 0.2. The location
+  is only misleading for empty blocks, and `epVirtualBraces` guards that
+  everywhere except `cvtopbody` (F2).
+
+---
+
+## Suggested order
+
+1. F2 and F4/F5. These are real defects; F4 and F5 change `check-exact`
+   output. Add `Gadt.hs` and `TF.hs`-style cases to `testsuite/tests/printer`.
+2. F1 and F3 together, as one "take the location from the open token" commit,
+   accepting the AST-dump churn. Then remove the `EpaLocation` from `decls`.
+3. F7 and F8, as hygiene.
+4. F6, once a test shows whether it is reachable.
+++++++++++++++++++++++++++++++++++++++ slttkkmz 7d1a3cff "Plan update DO NOT COMMIT" (rebased revision)
# Findings: layout capture in `AnnList` at parse time

2026-10-28, at jj lvnuyovt 

File to reproduce problems
-----------------------------------
{-# LANGUAGE TemplateHaskell, ImplicitParams, MultiWayIf, LambdaCase, PatternSynonyms #-}
module Lay2 where
a = x where
     p = 1
     q = 2
c = case x of
  ; p -> q
  r -> s
d = let ?x = 1
        ?y = 2 in ?x
e = [d| |]
e2 = [d| f = 1 |]
f = if | x -> y
       | z -> w
pattern P x <- Just x where
  P x = Just x
-----------------------------------

An audit of every layout-opening production in `compiler/GHC/Parser.y`
against what it records in `AnnList` (`al_layout :: EpLayout`, `al_semis`).
It complements `ANNLIST-LAYOUT-PLAN.md`, and corrects it where that plan is out
of date (see §7).

As in the plan, everything is referenced by production, function or
constructor name, not by line number. The backpack `unitbody` and the
header-only parsers stay out of scope.

Probe files live in the session scratchpad:
`/tmp/claude-1000/-home-alanz-mysrc-git-haskell-org-ghc/b86d6aba-94a8-4d83-992f-c1e5d9e5fae4/scratchpad/probe/`
(`Gadt.hs`, `TF.hs`, `Do.hs`, `Quote.hs`, `Lay2.hs`, and the dump `lay2.out`).
These results come from a stage1 build of commit `227da581e66`.

---

## 0. Ground rules

These are the rules the findings are checked against. They come from the lexer
(`maybe_layout`, `new_layout_context`, `do_layout_left` in
`compiler/GHC/Parser/Lexer.x`) and from the policy in commit f3b0a5bbe45.

1. **The layout location is the open token.** That is the `vocurly`, or for
   MultiWayIf the first `|`. The `vocurly` is a zero-width span at the first
   token after the layout keyword that is neither whitespace nor a comment.
   Comments are lexed in every start code, so they never trigger the `()`
   rule. The span's start column is the layout column that
   `new_layout_context` pushed.
2. **An empty block records `EpNoLayout`.** The `vocurly` location is only ever
   misleading for an empty block. That happens in two ways:
   - the `layout_left` path, where the next token is at or left of the
     enclosing column, so the lexer emits `vocurly`/`vccurly` at a token
     *outside* the block;
   - an immediate `close → error → popContext`, as in `[d| |]` or `let in`,
     where the `vocurly` lands on the closing token.

   Both cases have empty contents, so "empty ⇒ `EpNoLayout`" is exactly the
   right guard. `epVirtualBraces` implements it.
3. **`al_semis` holds every leading `;` the user wrote.** A `;` that follows an
   item is a trailing annotation on that item. Virtual (zero-width) semis are
   never recorded.

The exact printer reads only the *start* of the layout location
(`ss2delta priorEndAfterComments r` in `enterAnn` and `markEpTokenExtra`).
So findings F1 and F3 do not change printed output today. They do change AST
dumps, and they matter for consistency and for any future consumer that uses
the end of the span.

### Sites that already conform

The following take the location from the open token and guard on empty
contents:

- the two `vocurly` alternatives of `ty_fam_inst_eqn_list`
- `decllist_cls`
- `decllist_inst`
- `gadt_constrlist`
- `stmtlist` (`do`, `mdo`, `rec`)

`body` uses the `vocurly` too, but `hsmodLayout` is not an `AnnList`.

---

## F1. Four productions take the layout location from the contents

### Finding

| production | construct | location used today |
| --- | --- | --- |
| `decllist`, `vocurly` alternative | `let` / `where` binds | `fstOf3 $ unLoc $2`, built up inside `decls` |
| `where_decls`, `vocurly` alternative | pattern-synonym `where` | `glR $3` (the `decls` span) |
| `binds`, `vocurly dbinds close` | implicit-parameter `let` | `glR $2` (the `dbinds` span) |
| `altslist`, `vocurly alts close` | `of`, `\case`, `\cases` | `glR $2` (the `alts` span) |

Observed in `lay2.out`:

```
where binds      (lines 3-5)   EpVirtualBraces (EpaSpan { Lay2.hs:(4,6)-(5,10) })
case of ; ...    (lines 6-8)   EpVirtualBraces (EpaSpan { Lay2.hs:(7,3)-(8,8) })
let ?x .. ?y     (lines 9-10)  EpVirtualBraces (EpaSpan { Lay2.hs:(9,9)-(10,14) })
pattern .. where (line 16)     EpVirtualBraces (EpaSpan { Lay2.hs:16:3-14 })
```

For comparison, `e2 = [d| f = 1 |]`, which does use the `vocurly`, records
the zero-width `Lay2.hs:12:10`.

In each of these four cases the start column agrees with the `vocurly`. That
holds even for `case x of ; p -> q`, because `pal` widens the location to cover
the first leading `;`, and the leading `;` is where the lexer measured the
layout column. But the value is a span over the whole block rather than the
open token. It is built differently from every other site. And it is the only
reason `decls` still threads an `EpaLocation` in its first component, which
is assembled from `glR` / `glEE` / `glEEz`.

`ANNLIST-LAYOUT-PLAN.md` §7 marks step 2 ("take the location from the
`vocurly` token") as done, but these four sites were not converted.

### Proposed solution

Switch each site to the open token and keep the contents only for the empty
check:

```haskell
-- decllist
        |     vocurly    decls close   { if null (fstOf2 ..) && isNilOL (..)
                                            then noLoc (pal $ AnnList (epVirtualBraces (glR $1) $2) ...)
                                            else sLL $1 $2 (pal $ AnnList (epVirtualBraces (glR $1) $2) ...) }

-- where_decls
        | 'where' vocurly decls close { ... pal $ AnnList (epVirtualBraces (glR $2) $3) ... }

-- binds / dbinds
        |     vocurly    dbinds close   { ... IPBinds (pal $ AnnList (epVirtualBraces (glR $1) $2) []) ... }

-- altslist
        | vocurly    alts(PATS)  close { ... (pal $ AnnList (epVirtualBraces (glR $1) $2) (fst $ unLoc $2)) ... }
```

Follow-up, which is the plan's §4.2 "NOT DONE" item: once `decllist` stops
reading `fstOf3`, remove the `EpaLocation` component from `decls`. Its type
becomes `Located ([EpToken ";"], OrdList (LHsDecl GhcPs))`, which matches
`decls_cls` / `decls_inst`, and the `glR`, `glEE` and `glEEz` calls inside it
go away. `where_decls` and `mkPatSynMatchGroup` need the corresponding
`fstOf3/sndOf3/thdOf3` → `fst/snd` updates.

With the location now a zero-width point, `pal`'s widening to the first semi
still leaves the *start* unchanged, because the `vocurly` is at or before any
leading `;`. Whether `pal` should keep widening at all is a separate
simplification. It only exists to cope with content-derived anchors, so it
could be reduced to the "not a `RealSrcSpan` ⇒ `EpNoLayout`" check.

Expected churn: AST-dump tests (`DumpParsedAst*`, `DumpSemis`, the
`ghc-api/exactprint` dumps). There should be no `check-exact` output change.

---

## F2. `cvtopbody` records layout for an empty `[d| |]`

### Finding

```haskell
cvtopbody
        :  '{'            cvtopdecls0 '}'      { ... }
        |      vocurly    cvtopdecls0 close    { (AnnList (EpVirtualBraces (glR $1)) [], ...) }
```

This is the only `vocurly` site that builds `EpVirtualBraces` directly,
bypassing both `epVirtualBraces` and `pal`. For `[d| |]`:

1. `layout_token ITopenDecQuote` pushes `layout`.
2. The next token is `|]`, so the `()` rule fires there and opens a context at
   its column.
3. `cvtopdecls0` is empty, and `close` matches via `error`, which pops the
   context.

The result in `lay2.out`:

```
e = [d| |]   (line 11)   EpVirtualBraces (EpaSpan { Lay2.hs:11:9 })    -- column of `|]`
```

So we claim a layout block anchored on the closing bracket. That breaks
rule 0.2 and is inconsistent with every other site after f3b0a5bbe45.

### Proposed solution

Guard on the contents the same way the other sites do:

```haskell
        |      vocurly    cvtopdecls0 close
              { (pal $ AnnList (epVirtualBraces (glR $1) (L (locA (listLocation $2)) ())) []
                , sLLa $1 (listLocation $2) $2) }
```

`listLocation []` is `noSrcSpan`, so `epVirtualBraces` yields `EpNoLayout` for
the empty quote.

Add `[d| |]` and `[d|\n  |]` to a printer test and check that the dump shows
`EpNoLayout` and the round trip is unchanged.

---

## F3. MultiWayIf records the guard block, not the opening `|`

### Finding

```haskell
ifgdpats
         |     gdpats close   {% ... sL1 $1 (AnnList (EpVirtualBraces (glR $1)) [], unLoc $1) }
```

`$1` is the whole `gdpats` span. In `lay2.out`:

```
if | x -> y
   | z -> w   (lines 13-14)   EpVirtualBraces (EpaSpan { Lay2.hs:(13,8)-(14,15) })
```

The start column is correct, because it is the first `|`, which is the token
`new_layout_context` measured. But the location is a span over the contents,
the same problem as F1. There is no `vocurly` to use here: see
`ANNLIST-LAYOUT-PLAN.md` §1(b).

### Proposed solution

Narrow the location to the first character, which is the opening `|`:

```haskell
         |     gdpats close   {% runPV $1 >>= \ $1 ->
                                 return $ sL1 $1 (AnnList (EpVirtualBraces (spanAsAnchor $ srcSpanFirstCharacter $ getLoc $1)) []
                                                 , unLoc $1) }
```

`srcSpanFirstCharacter` is in `GHC.Types.SrcLoc`. An alternative is to take
the `ga_vbar` token of the first-parsed `GRHS`'s `GrhsAnn`. Note that `gdpats`
is accumulated in reverse, so that is the *last* element of the `NonEmpty`
here. The first-character approach avoids that subtlety.

`gdpats` is non-empty by construction, so no empty-contents guard is needed.

---

## F4. Semicolons dropped in GADT bodies

### Finding

```haskell
gadt_constr
    -- TODO:AZ capture the optSemi. Why leading?
        : optSemi modifiers con_list '::' sigtype
                {% do { ...; mkGadtDecl (comb3 $2 $3 $>) ... } }   -- $1 never used
```

```haskell
gadt_constrlist
        ...  AnnList (EpExplicitBraces ...) []         -- al_semis hard-coded []
        ...  pal $ AnnList (epVirtualBraces ...) []    -- likewise
```

The `optSemi` exists because `gadt_constrs` is right-recursive with a single
`';'` separator (`gadt_constr ';' gadt_constrs`). The only way to accept `;;`,
or a leading `;`, is to let each constructor eat one optional `;` in front.
That token is then discarded. The `check-exact` round trip of `Gadt.hs`
fails:

```
data T where { ; C1 :: T ;; C2 :: T }   ->   data T where {   C1 :: T ;  C2 :: T }
data U where
  C3 :: U ;; C4 :: U                    ->     C3 :: U ;  C4 :: U
```

### Proposed solution

Restructure `gadt_constrs` to the left-recursive shape used by `decls_inst`
and `decls_cls`, and delete `optSemi` from `gadt_constr`:

```haskell
gadt_constrs :: { Located ([EpToken ";"], OrdList (LConDecl GhcPs)) }
        : gadt_constrs ';' gadt_constr
              {% if isNilOL (snd $ unLoc $1)
                   then return (sLL $1 $> ((fst $ unLoc $1) ++ msemiA $2, unitOL $3))
                   else case snd $ unLoc $1 of
                     SnocOL hs t -> do
                       t' <- addTrailingSemiA t (epTok $2)
                       return (sLL $1 $> (fst $ unLoc $1, snocOL hs t' `snocOL` $3)) }
        | gadt_constrs ';'
              {% if isNilOL (snd $ unLoc $1)
                   then return (sLZ $1 $> ((fst $ unLoc $1) ++ msemiA $2, nilOL))
                   else case snd $ unLoc $1 of
                     SnocOL hs t -> do
                       t' <- addTrailingSemiA t (epTok $2)
                       return (sLZ $1 $> (fst $ unLoc $1, snocOL hs t')) }
        | gadt_constr         { sL1a $1 ([], unitOL $1) }
        | {- empty -}         { noLoc ([], nilOL) }
```

Then `gadt_constrlist` passes `fst $ unLoc $3` as `al_semis` in both
alternatives, and `fromOL . snd` as the constructors.

- The `;;` case becomes two trailing `AddSemiAnn`s on the preceding
  constructor, as it already does for `decls_inst`.
- `checkEmptyGADTs` needs only its pattern adjusted.
- Check for new happy conflicts. `optSemi` is also used by `if … then … else`,
  so leave that production in place.

`ExactPrint`'s `ConDecl` printing should need no change, since trailing semis
are already printed from the `SrcSpanAnnA`.

---

## F5. Leading semicolons dropped in closed-type-family bodies

### Finding

```haskell
ty_fam_inst_eqns :: { Located [LTyFamInstEqn GhcPs] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
              {% ... case unLoc $1 of
                       [] -> return (sLL $1 $> (L loc eqn : unLoc $1))   -- ';' discarded
                       ... }
        | ty_fam_inst_eqns ';'
              {% case unLoc $1 of
                   [] -> return (sLZ $1 $> (unLoc $1))                    -- ';' discarded
                   ... }
```

`ty_fam_inst_eqn_list` then builds `AnnList ... []`. The round trip of
`TF.hs` fails:

```
type family F a where { ; F Int = Bool ; F a = Char }
  ->
type family F a where {   F Int = Bool ; F a = Char }
```

### Proposed solution

Make `ty_fam_inst_eqns` carry the leading semis, in the same shape as F4:

```haskell
ty_fam_inst_eqns :: { Located ([EpToken ";"], [LTyFamInstEqn GhcPs]) }   -- eqns reversed
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
              {% case snd $ unLoc $1 of
                   [] -> return (sLL $1 $> ((fst $ unLoc $1) ++ msemiA $2, [$3]))
                   (h:t) -> do
                     h' <- addTrailingSemiA h (epTok $2)
                     return (sLL $1 $> (fst $ unLoc $1, $3 : h' : t)) }
        | ty_fam_inst_eqns ';'
              {% case snd $ unLoc $1 of
                   [] -> return (sLZ $1 $> ((fst $ unLoc $1) ++ msemiA $2, []))
                   (h:t) -> do
                     h' <- addTrailingSemiA h (epTok $2)
                     return (sLZ $1 $> (fst $ unLoc $1, h' : t)) }
        | ty_fam_inst_eqn             { sLL $1 $> ([], [$1]) }
        | {- empty -}                 { noLoc ([], []) }
```

In `ty_fam_inst_eqn_list`, pass `fst $ unLoc $2` as `al_semis` in the `'{'`
and `vocurly` alternatives, and `reverse . snd` as the equations. The `'..'`
alternatives are unaffected.

This also fixes an existing oddity: in the `[]` case, the old code combined a
`sLL $1 $>` location with an equation that had lost its preceding `;`.

---

## F6. MultiWayIf under `-XAlternativeLayoutRule` *(unverified)*

### Finding

`maybe_layout` returns early when `AlternativeLayoutRuleBit` is set, so
`ITif` never pushes `layout_if` and no layout context is opened for the guard
block. The other layout constructs are safe under ALR, because
`alternativeLayoutRuleToken` synthesises its own `ITvocurly`. MultiWayIf has
no synthesised token, yet the `gdpats close` alternative of `ifgdpats`
unconditionally records `EpVirtualBraces`.

I have not tested whether `if | …` even parses under ALR. This is test 7 of
`ANNLIST-LAYOUT-PLAN.md` §6.

### Proposed solution

First add the test. If the parse succeeds, gate the annotation:

```haskell
         |     gdpats close   {% do { alr <- getBit AlternativeLayoutRuleBit
                                    ; $1 <- runPV $1
                                    ; let lo = if alr then EpNoLayout
                                               else EpVirtualBraces (spanAsAnchor $ srcSpanFirstCharacter $ getLoc $1)
                                    ; return $ sL1 $1 (AnnList lo [], unLoc $1) } }
```

---

## F7. Leading semicolons are handled three different ways

### Finding

| production | how a leading `;` is added | zero-width `;` becomes |
| --- | --- | --- |
| `decls` | `msemiA $2` | omitted |
| `alts` / `alts1` | `mzEpTok` | a `NoEpTok` element |
| `decls_cls`, `decls_inst` | `[mzEpTok $2]` | a `NoEpTok` element |
| `stmts` | `snocOL (epTok $2)` | a zero-width `EpTok` |

`markEpTokenExtra` in `utils/check-exact/ExactPrint.hs` has only an `EpTok`
clause. That is the function used to print semis inside an
`EpVirtualBraces` list, so a `NoEpTok` in `al_semis` there would hit an
incomplete pattern.

This is **not reachable today**. The lexer never emits a virtual `;` before
the first item of a block, because `do_bol` only fires on later lines. It is
a latent hazard, and an inconsistency that F4 and F5 would otherwise copy.

### Proposed solution

- Use `msemiA` everywhere a leading semi is accumulated, so virtual semis are
  never stored, matching rule 0.3:
  - `alts`: `(mzEpTok $1) : xs` → `msemiA $1 ++ xs`
  - `alts1`, `decls_cls`, `decls_inst`: `++ [mzEpTok $2]` → `++ msemiA $2`
  - `stmts`: `` `snocOL` (epTok $2) `` → `` `appOL` toOL (msemiA $2) ``
- Defensively, add `markEpTokenExtra NoEpTok = return NoEpTok`.

---

## F8. Minor cleanups

- **`pal` is applied twice for `let`/`where` binds.** It runs once in
  `decllist` and again in `cvBindGroup`, which calls `patchAnnlistLayout`.
  `cvBindGroup` has a single caller (`binds`). Drop the call there.
- **`pal` on `dbinds` does nothing useful.** The semis are always `[]`, so the
  only effect is the non-`RealSrcSpan` fallback that `epVirtualBraces` already
  performs. This can be dropped after F1.
- **`stmtsAnchor` is dead.** It is exported from `GHC.Parser.PostProcess` but
  has no callers. Remove it.
- **`body2` uses a placeholder location.** The `missing_module_keyword`
  alternative records `EpVirtualBraces noSpanAnchor`. This is out of
  `AnnList` scope (`hsmodLayout`), but it is the one remaining
  `EpVirtualBraces` with no real location. `getEpaLocationCol` falls back to
  column 1, which happens to be correct. Consider a comment, or the location
  of the first token of `top`.

---

## 7. Corrections to `ANNLIST-LAYOUT-PLAN.md`

- **§4.1:** the new `AnnListLayout` type was not introduced. `AnnList` reuses
  `EpLayout`, and `EpVirtualBraces` now carries an `EpaLocation` instead of an
  `Int`. That also settles most of §4.5.
- **§2.2 table and §3:** these describe the pre-change state. They are useful
  as history, but should not be read as current.
- **§7 step 2:** marked done, but incomplete. See F1 and F3.
- **§7 step 4 note:** it says `stmtlist` still uses `stmtsLoc` and emits layout
  alongside `ListBraces`. Both are fixed: it now uses `glR $1` and
  `EpExplicitBraces`.
- **AZ note on bogus `vocurly` locations:** answered by rule 0.2. The location
  is only misleading for empty blocks, and `epVirtualBraces` guards that
  everywhere except `cvtopbody` (F2).

---

## Suggested order

1. F2 and F4/F5. These are real defects; F4 and F5 change `check-exact`
   output. Add `Gadt.hs` and `TF.hs`-style cases to `testsuite/tests/printer`.
2. F1 and F3 together, as one "take the location from the open token" commit,
   accepting the AST-dump churn. Then remove the `EpaLocation` from `decls`.
3. F7 and F8, as hygiene.
4. F6, once a test shows whether it is reachable.
