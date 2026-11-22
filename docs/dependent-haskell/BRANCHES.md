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
| R05/R16 rewrite rules | `3d0408cfc6` controls, `fd5a526ea1` forward port | The compiler records the home interfaces consulted by a final Core proof. Adding an applicable rule in a transitive home module now invalidates the cached proof. The self-hosted compiler passed seven registered tests and the native and no-code upgrade controls. Both stages matched 511 Showcase outputs at `-O0` and `-O2`. Self-hosted GHCi matched them too. |
| R11 equality | `47955a207f`, `0a8bc47405` | The first change fixes Core Lint for repeated retained-index transport. The second changes the GADT worker and the native equality selector. A v69 object can refer to the old selector even when its exported type is `Bool -> Int`. |
| R16 imported proofs | `b938c7f7c0`, `9347fe1049` | A client checks applicable rewrite rules when it uses an imported method proof. A provider compiled with rules disabled can export an opaque closed observation. The optimized Showcase uses this for the installed Prelude fold. |
| R19 retained constructors | `9c38f1827a` | A constructor can require `(a ~ Bool, b ~ Char)` before its retained Boolean field. The registered regression checks quotation and source-hidden clients. |
| R22 erased existentials | `8d7a0f8845`, `11212858ca`, `178111e9b4` | An ordinary call can open two independent packages. A product can store a filtered vector alongside a reader at its hidden length. A forwarding let or case can repack opened witnesses while they are in scope. These are separate application and binding checks. |
| R23 reflection | `21f280105b`, binary `4cc3fe29c8` | Printing a reflected matchable linear arrow raised an exception. The stack also restores the matchable-head contract of `Type.Reflection.App` and adds `App'` for both matchabilities. Verify both codecs with the matching binary submodule. |

The R04 Quick Look prototype `35ead97235` already has a published
implementation at `10bf2b0c4b`. The two-list zip at `3d5c46ec69` supplies a
test case. Its own commit adds no compiler support. At consolidation
the evaluator reported GHC-68947 at `zipLength xs ys`. The later R13
implementation checks the ordinary provider and the [zip proof](examples/packed-vector/ZipRows.hs).

## Branch and source inventory

Use `dependent-haskell` for active development. Build a pinned revision in
a detached checkout when the compiler or its libraries need a separate
build. R11 and R16 share one compiler and package database for the combined
rebuild. Check an old native provider, a rebuilt provider and client, and
the reverse interface import before publishing a changed worker ABI.

At the end of an experiment, integrate its verified source or keep its
exact tip under `refs/archive/dependent-haskell/` in the primary local
repository. Record any distinct test, limitation, or design choice that
remains. Save staged and unstaged source before detaching a dirty checkout.
Retire the topic branch after verifying the recovery ref.

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
those tips is an ancestor of the published branch. A source and test
comparison identified 36 more main-repository names with integrated
changes.

The final pass archived 128 main-repository names and 774 scratch names.
It preserved the original tips of distinct experiments as well as
superseded changes. The primary repository now has one checked-out
branch, `dependent-haskell`. Its other head, `master`, tracks upstream.
The archive also contains 42 worktree snapshots and one interrupted
three-patch queue. Each worktree snapshot has a parent for the original
index, so staged and unstaged versions can be recovered separately.

The local source bundle and its ref map are in the evidence directory
`branches-20261121`. The final bundle preserves 1,055 distinct source
tips. A restore check found all 24,907 required Git objects outside the
published prerequisite `d620e2ad78`. A separate archive records the
uncommitted source. The final branch ledger records which changes
entered the compiler and which remain experiments. Use the recovery ref
recorded for the branch to open a detached checkout. For example, restore
the original mixed-package experiment from the primary repository with
the following command.

```sh
git worktree add --detach /tmp/ghc-mixed-packages \
  refs/archive/dependent-haskell/streams-20261121/retired/main/codex/r22-mixed-spine
```

Keep a full checkout only while an experiment uses it. After the last
test, save local edits, verify the recovery revision in the primary
repository, and remove the working copy. Rebuild the compiler on demand.

To replay a retired checkout, find its original path and revision in
`checkout-recovery.tsv` under
`~/.local/state/ghc-build-retention/evidence/source-checkouts-20261121/`.
Use the snapshot ref in that row if you need its uncommitted changes.
Run these commands from the primary repository with the chosen revision.

```sh
git worktree add --detach /tmp/ghc-replay "$revision"
git -C /tmp/ghc-replay submodule update --init --recursive
```

The published commits are on the fork. The additional experimental
revisions have local archive refs and recovery bundles. The source
checkout bundle uses published commit `1c243406e9` as its prerequisite.

Some experiments copied individual files without a Git checkout. Their
manifest records each file's old path, mode, and blob. Read the restore
instructions from the local archive with this command.

```sh
git show refs/archive/dependent-haskell/source-copy-recovery/markerless-20261121-with-fixtures:README.md
```


## 22 November restart

After WSL restarted, Git still had each of the ten topic branch tips
and the HEADs of the 49 missing checkouts in `/tmp`. Each saved index
matched its HEAD. The integration branch contains the ten topics by
ancestry or by equivalent patches. Three cherry-picks had different patch
IDs. Comparing their additions and deletions found the same source changes.
The scoped dictionary test also moved a blank line.

| Original | Integrated | Change |
| --- | --- | --- |
| `a24fddec60` | `134f6abe20` | Unsigned machine fields in closed families |
| `b3bb592ca0` | `52f0f85832` | Recursive substitutions through scoped dictionary types |
| `5e2f428185` | `934c3a1be3` | Installed `Prelude.zipWith` cons proof |

Local recovery refs retain the original tips and reflog revisions. The
stale registrations and ten integrated topic branch names were removed.
The main repository has one development branch, `dependent-haskell`,
and the upstream `master`. To inspect the archived paths and revisions:

```sh
git show refs/archive/dependent-haskell/2026-11-22/restart/ledger:checkouts.tsv
git show refs/archive/dependent-haskell/2026-11-22/restart/ledger:branches.txt
```

The recovered optimizer commit `9011965d43` is available as
`refs/archive/dependent-haskell/2026-11-22/restart/r05-callback-regression`.
Its compiler changes and Core API test match the integrated `3a4e5c237a`.
