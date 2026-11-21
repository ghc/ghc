# Branch consolidation on 21 November 2026

Both development streams reached `02f4f731f4`. The published successor
`26004c02c0` also includes ordered type-level let groups. It passed
the 511-output Showcase on both compiler stages. The two streams had
also left tested changes on branches outside that history.
[Compiler checkpoint](CHECKPOINTS.md#ordered-type-let-source-checkpoint).

## Recovered work

| Card | Recovery point | What distinguishes the change |
| --- | --- | --- |
| R04 retained sums | `10d0e5cd18`, forward port `ea55c8c1d0` | An as-pattern on a retained constructor field binds the name used by later fields. The published compiler rejected the written as-pattern. The port passes the six selected tests, including laziness and rejection of crossed bounds. |
| R05/R16 rewrite rules | `3d0408cfc6` controls, `fd5a526ea1` forward port | Adding a rule in a transitive home module can leave a warm proof accepted while a clean build reports GHC-83605. The port records the home interfaces consulted by an accepted proof. The first-stage native and no-code controls now invalidate the proof. |
| R11 equality | `f8c9aff6dc`, `db329db55a` | A historical point-free equality fixture still gives a Core Lint error on the combined compiler. Compare the homogeneous retained-index rewrite independently of the proposed worker ABI. |
| R19 retained constructors | `19efc07551`, interface marker `c903a3043d` | A constructor can require `(a ~ Bool, b ~ Char)` before its retained Boolean field. The published compiler rejects this context. Check quotation, source-hidden clients, and the old reader's rejection of the new interface. |
| R22 erased existentials | `8d7a0f8845`, `11212858ca`, `178111e9b4` | An ordinary call can open two independent packages. A product can store a filtered vector alongside a reader at its hidden length. A forwarding let or case can repack opened witnesses while they are in scope. These are separate application and binding checks. |
| R23 reflection | `21f280105b`, binary `4cc3fe29c8` | Printing a reflected matchable linear arrow raised an exception. The stack also restores the matchable-head contract of `Type.Reflection.App` and adds `App'` for both matchabilities. Verify both codecs with the matching binary submodule. |

The R04 Quick Look prototype `35ead97235` already has a published
implementation at `10bf2b0c4b`. The R11 worker ABI still needs a
coordinated rebuild. The two-list zip at `3d5c46ec69` supplies a
test case. Its own commit adds no compiler support. The current
evaluator reports GHC-68947 at `zipLength xs ys`.

## Branch and source inventory

The reference point for the inventory is `02f4f731f4`. The main
repository had 319 local branches, 148 registered worktrees, and
three stashes. Of the local branches, 118 pointed into the published
history. Independent checkouts exposed another 355 tips outside the
main ref snapshot, containing 829 commits. The GHC fork advertised
245 remote branches. Only `dependent-haskell` contained recent project
work. The upstream draft uses that same fork branch.

The first cleanup retired 158 local topic names in the main repository.
At the successor checkpoint, 112 were ancestors, 38 had identical
commit diffs after adjusting index hashes and hunk positions, seven
merged with no tree change, and one matched a published tree. A second
cleanup retired 382 topic names in 220 scratch Git databases. Each of
those tips is an ancestor of the published branch. Every retired name
has a local recovery ref at its original tip. Dirty checkouts, stashes,
and branches with unexplained changes remain available.

The local source bundle and its ref map are in the evidence directory
`branches-20261121`. Restore verification found all 19,970 required Git
objects outside `26004c02c0` in the bundle. A separate archive records
the uncommitted source. The branch ledger records ancestry, patch
comparisons, and unresolved cases. To recover a retired branch, run the
following in the repository that owned it, substituting its recorded name.

```sh
git branch "$name" "refs/archive/dependent-haskell/2026-11-21/integrated/$name"
```
