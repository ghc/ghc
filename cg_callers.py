#!/usr/bin/env python3
"""Invert callgrind call graphs -> caller attribution by *call count*.

Answers the "use it less" question the flat cachegrind profile cannot: for a hot
function, who calls it and how often. Parses cg-out/cg/*.callgrind (Ir-only,
function-level).

Method note (important): GHC's hot workers (Word64Map.$winsert, elems1,
seqType/seqTypes, eq_type_*) are deeply *recursive*. callgrind's inclusive cost
double-counts wildly across recursion cycles (callgrind_annotate prints
inclusive % like 45000% for these), so inclusive Ir is NOT trustworthy here. What
*is* exact: per-function self Ir, and per-edge call counts. So we attribute by
call count, splitting self-recursion from external entry callers. For a recursive
worker the lever is the external entries (who starts the walk), not the self-edge.

Symbols: we canonicalise via cgwork.demangle, drop the callgrind "'N" instance
suffix and the "_info" tail, and merge variants of the same logical function.

Usage:
  cg_callers.py FUNC_SUBSTR [FUNC_SUBSTR...]   # caller breakdown per function
  cg_callers.py --top N                         # top N compiler fns by self Ir
  cg_callers.py --file NAME ...                 # restrict to one test's file
"""
import sys, os, re, glob, argparse
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
CG   = os.path.join(HERE, 'cg-out', 'cg')
sys.path.insert(0, '/home/simon/prog/ghc-personal')
import cgwork

def canon(sym):
    s = cgwork.demangle(sym)
    s = re.sub(r"'\d+$", '', s)     # callgrind instance suffix
    s = re.sub(r'_info$', '', s)
    return s

def parse(path):
    """-> (self_ir: canon->Ir, comp: canon->bool, edges: (cCaller,cCallee)->[calls,rawIr])."""
    fns = {}
    self_ir = defaultdict(int)
    comp    = {}
    edges   = defaultdict(lambda: [0, 0])
    cur_raw = cur = None
    pend = None; pend_calls = 0
    def nm(tok):
        if tok.startswith('('):
            num, _, rest = tok.partition(')')
            rest = rest.strip()
            if rest:
                fns[num + ')'] = rest; return rest
            return fns.get(num + ')', '?')
        return tok
    with open(path, errors='replace') as f:
        for line in f:
            line = line.rstrip('\n')
            if not line or line[0] == '#':
                continue
            c0 = line[0]
            if c0.isdigit() or c0 in '+-*':
                ir = int(line.split()[-1])
                if pend is not None:
                    e = edges[(cur, pend)]; e[0] += pend_calls; e[1] += ir
                    pend = None
                elif cur is not None:
                    self_ir[cur] += ir
                continue
            key, _, rest = line.partition('=')
            if key == 'fn':
                cur_raw = nm(rest.strip()); cur = canon(cur_raw)
                comp[cur] = comp.get(cur, False) or cgwork.is_comp(cur_raw)
                pend = None
            elif key == 'cfn':
                praw = nm(rest.strip()); pend = canon(praw)
            elif key == 'calls':
                pend_calls = int(rest.split()[0])
    return self_ir, comp, edges

def load(files):
    S = defaultdict(int); C = {}; E = defaultdict(lambda: [0, 0])
    for p in files:
        s, c, e = parse(p)
        for k, v in s.items(): S[k] += v
        for k, v in c.items(): C[k] = C.get(k, False) or v
        for k, (cc, ir) in e.items():
            E[k][0] += cc; E[k][1] += ir
    return S, C, E

def callers_of(E, callee):
    return sorted(((ca, cc, ir) for (ca, ce), (cc, ir) in E.items() if ce == callee),
                  key=lambda x: x[1], reverse=True)

def report(fn, S, E, grand):
    sc = S.get(fn, 0)
    cls = callers_of(E, fn)
    total_calls = sum(cc for _, cc, _ in cls)
    selfrec = sum(cc for ca, cc, _ in cls if ca == fn)
    ext = [(ca, cc) for ca, cc, _ in cls if ca != fn]
    ext_calls = sum(cc for _, cc in ext)
    print(f"== {fn}")
    print(f"   self Ir {sc:,} ({100*sc/grand:.2f}% of compiler self Ir)")
    print(f"   calls in: {total_calls:,}  (self-recursion {selfrec:,} = "
          f"{100*selfrec/max(total_calls,1):.0f}%,  external {ext_calls:,})")
    print(f"   external entry callers (by call count -- the 'use it less' lever):")
    for ca, cc in ext[:12]:
        sh = 100*cc/max(ext_calls,1)
        print(f"     {cc:>12,}  {sh:5.1f}%  <- {ca}")
    print()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('funcs', nargs='*')
    ap.add_argument('--top', type=int, default=0)
    ap.add_argument('--file', default=None)
    a = ap.parse_args()
    files = ([os.path.join(CG, a.file + '.callgrind')] if a.file
             else sorted(glob.glob(os.path.join(CG, '*.callgrind'))))
    files = [f for f in files if os.path.exists(f)]
    if not files:
        sys.exit(f"no callgrind files in {CG}")
    print(f"# files: {', '.join(os.path.basename(f)[:-10] for f in files)}\n")
    S, C, E = load(files)
    grand = sum(v for k, v in S.items() if C.get(k))
    targets = []
    if a.top:
        comp_ranked = sorted((k for k in S if C.get(k)), key=S.get, reverse=True)
        targets += comp_ranked[:a.top]
    for sub in a.funcs:
        m = sorted((k for k in S if sub in k and C.get(k)), key=S.get, reverse=True)
        targets += (m[:1] or [sub])
    for fn in targets:
        report(fn, S, E, grand)

if __name__ == '__main__':
    main()
