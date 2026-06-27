#!/usr/bin/env python3
"""Aggregate per-function compiler Ir across all cg-out/*.cg profiles.

Reuses cgwork's parser/demangler/bucketing. For every test we keep only the
deterministic COMPILER bucket (cgwork.is_comp), demangle each symbol, and sum Ir
per readable function name across the suite.

Outputs (stdout + cg-out/aggregate.md):
  * ranked total Ir per function, with % of suite COMPILER Ir and breadth
    (#tests the function appears in);
  * a startup-vs-scaling split: each function's mean Ir in the *trivial* tests
    (smallest COMPILER totals ~ pay-per-invocation startup) vs the *large* tests
    (biggest totals ~ work that scales). Functions flat across the two are
    startup cost; functions that grow are genuine compilation work.
"""
import sys, os, glob, json
from collections import defaultdict

sys.path.insert(0, '/home/simon/prog/ghc-personal')
import cgwork

HERE = os.path.dirname(os.path.abspath(__file__))
OUT  = os.path.join(HERE, 'cg-out')

def load():
    """name -> {fn: Ir} (COMPILER bucket only, demangled)."""
    per_test = {}
    for p in sorted(glob.glob(os.path.join(OUT, '*.cg'))):
        name = os.path.basename(p)[:-3]
        fns, _ = cgwork.parse(p)
        comp = defaultdict(int)
        for k, v in fns.items():
            if cgwork.is_comp(k):
                comp[cgwork.demangle(k)] += v
        per_test[name] = comp
    return per_test

def classify(per_test):
    """Split tests into trivial (smallest COMPILER total) and large (biggest).
    Bottom/top quartile, at least one test each."""
    totals = {n: sum(c.values()) for n, c in per_test.items()}
    order = sorted(totals, key=totals.get)
    n = len(order)
    k = max(1, n // 4)
    return order[:k], order[-k:], totals

def main():
    per_test = load()
    if not per_test:
        sys.exit(f"no .cg files in {OUT} — run cg_batch.py first")
    trivial, large, totals = classify(per_test)

    suite = defaultdict(int)
    breadth = defaultdict(int)
    for c in per_test.values():
        for fn, ir in c.items():
            suite[fn] += ir
            breadth[fn] += 1
    suite_total = sum(suite.values())

    def mean_over(tests, fn):
        return sum(per_test[t].get(fn, 0) for t in tests) / len(tests)

    ranked = sorted(suite, key=suite.get, reverse=True)

    lines = []
    def out(s=''):
        print(s); lines.append(s)

    out(f"# Suite-wide compiler Ir aggregate")
    out()
    out(f"{len(per_test)} tests; suite COMPILER Ir = {suite_total:,}")
    out(f"trivial ({len(trivial)}): {', '.join(trivial)}")
    out(f"large   ({len(large)}): {', '.join(large)}")
    out()
    out(f"{'total Ir':>16}  {'%suite':>7}  {'#t':>3}  "
        f"{'trivial μ':>13}  {'large μ':>14}  flag  function")
    for fn in ranked[:60]:
        tot = suite[fn]
        tmean = mean_over(trivial, fn)
        lmean = mean_over(large, fn)
        if tmean > 0 and lmean > 3 * tmean:
            flag = 'SCALES'
        elif lmean <= 1.5 * max(tmean, 1):
            flag = 'startup'
        else:
            flag = '·'
        out(f"{tot:>16,}  {100*tot/suite_total:6.2f}%  {breadth[fn]:>3}  "
            f"{tmean:>13,.0f}  {lmean:>14,.0f}  {flag:<6}  {fn[:60]}")

    out()
    out("## Per-test COMPILER totals")
    for n in sorted(totals, key=totals.get, reverse=True):
        out(f"  {totals[n]:>16,}  {n}")

    with open(os.path.join(OUT, 'aggregate.md'), 'w') as f:
        f.write('\n'.join(lines) + '\n')
    print(f"\nwrote {os.path.join(OUT, 'aggregate.md')}")

if __name__ == '__main__':
    main()
