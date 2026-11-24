# PLFH compiler checkpoint

The literate chapters, extracted Haskell and standalone compiler reports live in
[Programming Language Foundations in Haskell](https://github.com/Tritlo/PLFH).
The local `hs/` and `reproducers/` directories are generated mirrors. To
refresh them, run the book's `scripts/extract_and_sync.py --sync-ghc` with the
absolute path of this directory. The proof examples adapt PLFA under CC BY 4.0.
[Attribution and book sources](https://github.com/Tritlo/PLFH)

## Replay the combined book

Start in a full checkout of the GHC fork's `dependent-haskell` branch. Set
`book` to a clone of PLFH containing published batch 31, commit
`21dff64b26b9e447d7dfc9b99717933e897ca716`. These commands import private
refs into that clone. They leave its current branch and files in place.

```sh
bundle="$PWD/docs/dependent-haskell/plfh/checkpoint/book.bundle"
git -C "$book" bundle verify "$bundle"
git -C "$book" fetch "$bundle" \
  'refs/yardsticks/plfh/*:refs/yardsticks/plfh/*' \
  'refs/archive/dependent-haskell/handoff-20261124/*:refs/archive/dependent-haskell/handoff-20261124/*'
book_revision=refs/yardsticks/plfh/bisimulation-bigstep-21dff64
git -C "$book" rev-parse "$book_revision^{tree}"
```

The combined book is `749aaf2388308c68413ecd216ca3453c64217867`.
Its tree is `86ebd8f1cb739c0fa6b7c7c73c53a4dc53566cc0`.
It includes the full Bisimulation renaming proof and the functional BigStep
lookup client. The published manuscript is a separate checkout.
[Verification commands and results](../HANDOFF.md#current-checkpoint)

## Resume unfinished work

The compiler bundle contains the three candidates in the table. Its base
commits are ancestors of this GHC branch. Fetch the refs in the GHC checkout.

```sh
bundle="$PWD/docs/dependent-haskell/plfh/checkpoint/compiler-wip.bundle"
git bundle verify "$bundle"
git fetch "$bundle" \
  'refs/archive/dependent-haskell/handoff-20261124/*:refs/archive/dependent-haskell/handoff-20261124/*'
```

| Card | Compiler revision | Book input and first open check |
| --- | --- | --- |
| R16, simultaneous substitution | `8edc4613f3` | The book at `3dae841c05` contains the ordinary `simExt` client. It checks from a native interface at `-O0`. At default `-O2`, the provider builds but the reader reports GHC-68947 for `M.exts`. The control that uses the wrong substitution fails at both levels. Compare the provider's published equations at the two optimization levels. |
| R21, same-module lookup | `8231bb3c08` | The book at `3a3202b548` has `lookup-controls/`. The functional BigStep chapter, a later declaration, both clause orders and the original report check at both native levels. The three controls reject an incorrect index, an incorrect body and a result-changing rewrite rule. Check constructor visibility across the provider interface and the agreement between early and final equations before integrating. |
| R03, closed family refinement | `00b25199e6` | This revision records dependencies of checked calls reached during refinement of a retained equality. Review the final change and qualify the Adequacy client and existing registered controls at both optimization levels. |

The book ref `substitution-model` at `137f1ac206` includes `Whole.hs`.
It checks all 20 induction cases using explicit type families at both native
levels. The ordinary-function version still depends on the R16 work above.
Use `git bundle list-heads` to print full ref names and revisions.
