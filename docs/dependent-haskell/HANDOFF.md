# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Updated 22 November 2026, 13:25 CET.

| Revision | What to recover | Verification |
| --- | --- | --- |
| `97f6622b03` | Last published checkpoint. An ordinary client opens a sample with a hidden calibration status. | Both compiler stages passed the 538-line Showcase at `-O0` and `-O2`. Stage 2 GHCi and 28 selected registered tests passed. [Record](CHECKPOINTS.md#erased-calibration-status). |
| `70ded7e706` | Consolidated compiler. A retained callback accepts nominally equal argument and result types. CorePrep preserves the index when it names a constructor in a curried call. The evaluator checks the ordered equations of the installed `Prelude.zip` and `Prelude.zipWith`. | Focused tests passed before the restart. The earlier 540-output two-stage run was interrupted. Rebuild this compiler for the combined checks. |
| `e62ddf4e9f` | Same compiler and libraries, with the merged 542-line Showcase and zipper length proofs. The ordinary client proves that each zipper takes the shorter length. | Use these sources and expected output with the rebuilt compiler. Full combined verification is pending. |

Pueue job `379` is building `70ded7e706` in the normal checkout below.
Verify that its label is `normal-70ded7e706` when inspecting the queue.
The bootstrap completed. The process reported CPUs 0–11, a 16 GiB job cap,
and no swap. The priority probe froze the slice while a job in another group
ran, then resumed it. The local branch includes the 542-line fixture.
[Branch recovery ledger](BRANCHES.md#22-november-restart).

The open Core optimizer investigation concerns pushing a dependent function
cast through an application with a cast callback. It is separate from the
CorePrep fix in the consolidated compiler. Record its tested commit here
before including it in a release.

## Resume on the shared WSL host

Use `/home/tritlo/Code/ghc`. Keep source changes in Git and put current
builds and receipts under `_build/on-demand/live-recovery/`. The WSL restart
erased `/tmp`. A Git worktree registration or a saved Pueue task can survive
after its checkout or process has gone.

1. Run `git status -sb`, `git worktree list --porcelain`, and
   `git ls-remote fork refs/heads/dependent-haskell`. Compare the revisions
   with the table above. Run `env -u XDG_RUNTIME_DIR pueue status` to check
   the shared daemon. Inspect the specific job and its checkout before
   retrying it. Keep the queue paused while restoring resource limits.
2. Set the user bus variables in the shell that controls systemd. If
   `/run/user/$(id -u)/bus` is missing, start the user manager from a WSL
   terminal with `sudo systemctl start user@$(id -u).service`.

   ```sh
   export XDG_RUNTIME_DIR=/run/user/$(id -u)
   export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus
   ```

3. If the shared daemon is down, start it on its existing socket.
   Pueue clients on this host use `env -u XDG_RUNTIME_DIR pueue`.
   This preserves the queue shared with local-model jobs.

   ```sh
   systemd-run --user --unit=shared-pueued --property=Restart=on-failure \
       /usr/bin/env -u XDG_RUNTIME_DIR "$HOME/.nix-profile/bin/pueued"
   ```

4. Restore the GHC slice and its priority watcher. Run these commands from
   the repository root. Start the watcher only if its service is inactive.
   Its state marker lives in the user runtime directory. Inspect any queued
   GHC commands from the previous boot before starting the group.

   ```sh
   systemctl --user set-property --runtime ghcbuild.slice \
       CPUQuota=1200% AllowedCPUs=0-11 MemoryMax=64000000000 MemorySwapMax=0
   systemctl --user start ghcbuild.slice
   systemd-run --user --unit=ghc-build-priority --property=Restart=on-failure \
       --setenv="PATH=$PATH" --setenv="XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR" \
       --setenv="DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS" \
       /usr/bin/python3 "$PWD/docs/dependent-haskell/tools/ghc-build-priority.py"
   systemctl --user show ghcbuild.slice -p CPUQuotaPerSecUSec -p MemoryMax -p MemorySwapMax
   env -u XDG_RUNTIME_DIR pueue group
   ```

   The expected values are `12s`, `64000000000` and `0` respectively.
   The `ghc` group has three slots. Each Hadrian invocation uses at most four
   workers. Runnable work in another group takes priority. The watcher
   pauses GHC admissions and freezes the GHC slice until that work finishes.
   When another job has priority, leave GHC paused. Use
   `env -u XDG_RUNTIME_DIR pueue start --group ghc` when ready to resume.
5. Submit the build with [ghc-build-job](tools/ghc-build-job). It runs each
   job in the shared slice and records peak memory with
   `~/Code/heavy-management/bin/capped-record`. Inspect the job result and
   the compiler revision in its receipt.

## Build and verify

The current normal checkout is
`_build/on-demand/live-recovery/normal-70ded7e706/source`. Its sibling
`build.sh` records the commands and writes logs in the parent directory.
On this host the bootstrap tools are GHC 9.14.1, Cabal 3.18.1.0, Happy 2.1.7
and Alex 3.5.4.0. A fresh checkout needs the Git submodules at their recorded
revisions. The normal build runs `./boot`, `./configure`, builds Hadrian
from `hadrian/cabal.project`, and runs Hadrian with
`-j4 --flavour=quick --docs=none`. Reuse the configured checkout and Hadrian
binary if a build stops partway. Keep each compiler with its own libraries.

Hadrian puts the stage 1 compiler in `_build/stage0/bin/ghc` and the
self-hosted stage 2 compiler in `_build/stage1/bin/ghc`. Both use the
target libraries in `_build/stage1/lib`. For example, from the main checkout:

```sh
area="$PWD/_build/on-demand/live-recovery/normal-70ded7e706"
queue="$PWD/docs/dependent-haskell/tools/ghc-build-job"
check="$PWD/docs/dependent-haskell/check-showcase.sh"
lib="$area/source/_build/stage1/lib"
"$queue" showcase-stage1 bash "$check" "$area/source/_build/stage0/bin/ghc" "$lib"
"$queue" showcase-stage1-o2 bash "$check" "$area/source/_build/stage0/bin/ghc" "$lib" --default-o2
"$queue" showcase-stage2-ghci bash "$check" "$area/source/_build/stage1/bin/ghc" "$lib" --ghci
"$queue" showcase-stage2-o2 bash "$check" "$area/source/_build/stage1/bin/ghc" "$lib" --default-o2
```

The runner checks native output, source-hidden clients and Core/STG Lint.
The interactive flag also checks GHCi output. Verify the installed
`GHC.Internal.List` interfaces and the registered tests as separate checks.
Record the number of tests run, failed or skipped. A compiler revision,
a built executable and a passing Showcase answer different questions.

After qualification, update this handoff and [the checkpoint record](CHECKPOINTS.md).
Keep the Git revisions, the commands and any outstanding failure. Release
superseded build products after their final consumer finishes. The main
`_build` cleanup threshold is 200 GB. [Retention rules](CHECKPOINTS.md#clean-up-after-each-experiment).
