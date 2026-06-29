#!/usr/bin/env python3
"""Call-graph profile of GHC perf tests under callgrind (Ir-only).

cgwork/cg_batch give a *flat* per-symbol Ir profile (cachegrind): enough to see
*which* function is hot, but not *who calls it* — the "use it less" question. This
runner uses callgrind, cachegrind's sibling, which records the full call graph at
the same Ir fidelity, against the same prebuilt ~/ghc/head0 binary (no rebuild).

We reuse cg_batch's self-contained per-test table, command builder and workdir
prep, swapping the cgwork(cachegrind) invocation for callgrind:
  * --cache-sim=no --branch-sim=no  -> Ir only, matching the cgwork data.
  * --dump-instr=no --collect-jumps=no -> function-level call graph, smaller files.
  * --trace-children + the same skip list as cgwork (as/ld/cc/... stay native).
Then we keep only the largest ghc process (same heuristic as cgwork) and move its
raw callgrind file to cg-out/cg/<name>.callgrind for callgrind_annotate / analysis.

Usage:
  cg_callgraph.py run NAME [NAME...]   # callgrind these tests
  cg_callgraph.py run --force NAME...  # re-run even if the file exists
"""
import sys, os, glob, shutil, subprocess, argparse, time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, '/home/simon/prog/ghc-personal')
import cgwork
import cg_batch as B

CG = os.path.join(B.OUT, 'cg')   # raw callgrind files live here

def cg_total(path):
    """Sum of the 'summary:' line(s) of a callgrind file (Ir total)."""
    try:
        with open(path, errors='replace') as f:
            for line in f:
                if line.startswith('summary:'):
                    return int(line.split()[1])
                if line.startswith('totals:'):
                    return int(line.split()[1])
    except OSError:
        return 0
    return 0

def run_one(name, force):
    t = next((t for t in B.TESTS if t['name'] == name), None)
    if t is None:
        sys.exit(f"unknown test: {name}")
    out = os.path.join(CG, name + '.callgrind')
    if os.path.exists(out) and not force:
        print(f"= {name}: skip (callgrind file exists)")
        return
    wd = B.prep_workdir(t)
    if t['pre']:
        r = subprocess.run(t['pre'], shell=True, cwd=wd)
        if r.returncode != 0:
            sys.exit(f"! {name}: pre_cmd failed ({t['pre']})")
    cmd = B.ghc_cmd(t)
    for stale in glob.glob(glob.escape(out) + '.*'):
        os.remove(stale)
    vg = ['valgrind', '--tool=callgrind',
          '--cache-sim=no', '--branch-sim=no',
          '--dump-instr=no', '--collect-jumps=no',
          '--trace-children=yes',
          '--trace-children-skip=' + ','.join(cgwork.SKIP_CHILDREN),
          f'--callgrind-out-file={out}.%p'] + cmd
    print(f"+ {name}: {' '.join(cmd)}")
    t0 = time.time()
    subprocess.call(vg, cwd=wd)
    dt = time.time() - t0

    produced = sorted(glob.glob(glob.escape(out) + '.*'))
    if not produced:
        sys.exit(f"! {name}: no callgrind output produced")
    ghc = [p for p in produced
           if 'ghc' in os.path.basename(cgwork.read_cmd(p).split(' ')[0])]
    chosen = max(ghc or produced, key=cg_total)
    for p in produced:
        if p != chosen:
            print(f"  (ignoring child -> {cg_total(p):,} Ir)")
            os.remove(p)
    shutil.move(chosen, out)
    print(f"  -> {name}: {cg_total(out):,} Ir total ({dt:.0f}s)  {out}")

def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    rp = sub.add_parser('run')
    rp.add_argument('names', nargs='+')
    rp.add_argument('--force', action='store_true')
    a = ap.parse_args()
    os.makedirs(CG, exist_ok=True)
    os.makedirs(B.WORK, exist_ok=True)
    for n in a.names:
        run_one(n, a.force)

if __name__ == '__main__':
    main()
