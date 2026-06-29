#!/usr/bin/env python3
"""Run the perf/compiler tests under a *late-cost-centre profiled* GHC.

This is the last tool in the chain (cachegrind -> callgrind -> ticky -> here; see
FINDINGS.md). ticky gave exact entry counts + allocation *at each definition site*
but no caller context; callgrind had caller edges but the laziness wall hid the
*logical* source caller behind stg_ap_*. A cost-centre profile threads a source
cost-centre down through thunk evaluation, so the JSON stack tree gives caller
context. We use the `perf+profiled_ghc` build at the same commit (3b2a9409ba):

    ~/ghc/prof-late   (profiled_ghc = enableLateCCS . enableProfiledLibs)

`profiled_ghc` adds **-fprof-late**, i.e. cost centres are inserted *after*
optimisation, so attribution is on the real optimised program (not the SCC-blocked
code plain -fprof-auto would produce). We emit the JSON profile (`-pj`); `-V0`
disables the time sampler (ticks become 0) but **alloc and entries stay exact and
deterministic**, which are the levers we care about.

Usage:
  prof_run.py NAME [NAME...]     # profile these tests -> prof-out/<NAME>.prof (JSON)
  prof_run.py --force NAME...    # re-run even if the .prof exists
"""
import sys, os, subprocess, argparse, time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, '/home/simon/prog/ghc-personal')
import cg_batch as B

PROF_GHC = '/home/simon/ghc/prof-late/_build/stage1/bin/ghc'
POUT = os.path.normpath(os.path.join(B.OUT, '..', 'prof-out'))

def ghc_cmd(t, stem):
    td = '--make' if t['kind'] == 'multimod' else '-c'
    # -po<stem> sets the profiling-output stem; -pj writes <stem>.prof as JSON.
    return ([PROF_GHC, td, t['src']] + t['flags'].split() +
            ['-fforce-recomp', '+RTS', '-V0', '-I0', f'-po{stem}', '-pj', '-RTS'])

def run_one(t, force):
    stem = os.path.join(POUT, t['name'])
    out = stem + '.prof'
    if os.path.exists(out) and not force:
        print(f"= {t['name']}: skip (.prof exists)")
        return
    wd = B.prep_workdir(t)
    if t['pre']:
        r = subprocess.run(t['pre'], shell=True, cwd=wd)
        if r.returncode != 0:
            print(f"! {t['name']}: pre_cmd failed ({t['pre']})")
            return
    if os.path.exists(out):
        os.remove(out)
    cmd = ghc_cmd(t, stem)
    print(f"+ {t['name']}: {' '.join(cmd)}")
    t0 = time.time()
    r = subprocess.run(cmd, cwd=wd)
    dt = time.time() - t0
    if not os.path.exists(out):       # compile_fail still writes the profile
        print(f"! {t['name']}: no .prof written (rc={r.returncode})")
        return
    print(f"  -> {out}  ({dt:.0f}s, rc={r.returncode}, {os.path.getsize(out)//1024} KiB)")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('names', nargs='*')
    ap.add_argument('--force', action='store_true')
    a = ap.parse_args()
    if not os.path.exists(PROF_GHC):
        sys.exit(f"profiled ghc not found: {PROF_GHC}")
    os.makedirs(POUT, exist_ok=True)
    os.makedirs(B.WORK, exist_ok=True)
    sel = [t for t in B.TESTS if t['name'] in a.names] if a.names else B.TESTS
    if a.names and len(sel) != len(set(a.names)):
        missing = set(a.names) - {t['name'] for t in sel}
        sys.exit(f"unknown test(s): {', '.join(sorted(missing))}")
    for t in sel:
        run_one(t, a.force)

if __name__ == '__main__':
    main()
