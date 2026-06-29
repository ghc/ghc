#!/usr/bin/env python3
"""Run the perf/compiler tests under a *ticky* GHC -> per-function entry counts.

cachegrind (cgwork) and callgrind (cg_callgraph) answer "what is hot" and "how
many *calls*", but both run the *prebuilt, stripped, threaded* ~/ghc/head0 ghc, so
source-level attribution is blocked by laziness (calls arrive via stg_ap_*) and a
.symtab-stripped .so (~37% of one test shows as 0x...). See FINDINGS.md.

This runner uses a dedicated **ticky-instrumented** ghc built at the *same commit*
(3b2a9409ba) as head0:

    ~/ghc/ticky  (flavour perf+ticky_ghc+no_profiled_libs)

The ticky RTS reports, per STG closure, exact **entry counts** and **allocation**,
keyed by the real definition site (module-qualified STG name) -- piercing both the
laziness and the stripping. We reuse cg_batch's self-contained test table / workdir
prep so the inputs match the cachegrind/callgrind runs exactly; the only change is
the compiler binary and `+RTS -r<file>` instead of valgrind.

The ticky ghc is non-threaded (ticky_ghc forces it; the C counters are racy under
the threaded RTS, #23439), so runs are single-process and deterministic.

Usage:
  ticky_run.py NAME [NAME...]      # ticky-profile these tests
  ticky_run.py --force NAME...     # re-run even if the .ticky exists
  ticky_run.py --list             # show the reusable test table
"""
import sys, os, subprocess, argparse, time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, '/home/simon/prog/ghc-personal')
import cg_batch as B

TICKY_GHC = '/home/simon/ghc/ticky/_build/stage1/bin/ghc'
TOUT = os.path.join(B.OUT, '..', 'ticky-out')
TOUT = os.path.normpath(TOUT)

def ghc_cmd(t, out):
    td = '--make' if t['kind'] == 'multimod' else '-c'
    return ([TICKY_GHC, td, t['src']] + t['flags'].split() +
            ['-fforce-recomp', '+RTS', '-V0', '-I0', f'-r{out}', '-RTS'])

def run_one(t, force):
    out = os.path.join(TOUT, t['name'] + '.ticky')
    if os.path.exists(out) and not force:
        print(f"= {t['name']}: skip (.ticky exists)")
        return
    wd = B.prep_workdir(t)
    if t['pre']:
        r = subprocess.run(t['pre'], shell=True, cwd=wd)
        if r.returncode != 0:
            print(f"! {t['name']}: pre_cmd failed ({t['pre']})")
            return
    if os.path.exists(out):
        os.remove(out)
    cmd = ghc_cmd(t, out)
    print(f"+ {t['name']}: {' '.join(cmd)}")
    t0 = time.time()
    r = subprocess.run(cmd, cwd=wd)
    dt = time.time() - t0
    # compile_fail tests exit non-zero by design; the ticky report is still written.
    if not os.path.exists(out):
        print(f"! {t['name']}: no .ticky written (rc={r.returncode})")
        return
    print(f"  -> {out}  ({dt:.0f}s, rc={r.returncode})")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('names', nargs='*')
    ap.add_argument('--force', action='store_true')
    ap.add_argument('--list', action='store_true')
    a = ap.parse_args()
    if a.list:
        for t in B.TESTS:
            print(f"{t['name']:24} {t['kind']}")
        return
    if not os.path.exists(TICKY_GHC):
        sys.exit(f"ticky ghc not found: {TICKY_GHC}")
    os.makedirs(TOUT, exist_ok=True)
    os.makedirs(B.WORK, exist_ok=True)
    sel = [t for t in B.TESTS if t['name'] in a.names] if a.names else B.TESTS
    if a.names and len(sel) != len(set(a.names)):
        missing = set(a.names) - {t['name'] for t in sel}
        sys.exit(f"unknown test(s): {', '.join(sorted(missing))}")
    for t in sel:
        run_one(t, a.force)

if __name__ == '__main__':
    main()
