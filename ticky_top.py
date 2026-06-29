#!/usr/bin/env python3
"""Rank ticky reports by entry count / allocation, restricted to compiler closures.

Parses ticky-out/*.ticky (produced by ticky_run.py). The detailed table has fixed
columns:

    Entries       Alloc     Alloc'd  Non-void Arguments   <...>   STG Name
    ^int          ^int      ^int     (has internal spaces)        ^col 102 (0-based 101)

so the three leading integers split cleanly and the STG name is `line[101:]`.

What this gives that callgrind could not (see FINDINGS.md):
  * **entry counts** = exact "how often is it entered" (the (B) "use it less" lever),
    attributed to the real definition site even through thunks / generic apply;
  * **Alloc / Alloc'd** = bytes the closure allocates / is allocated -- the (A) lever.

Because there is a *single* ticky binary compiling every test, compiler functions
carry identical STG ids across tests, so summing by STG name aggregates correctly.

Compiler filter: the ghc library's modules are `GHC.*` but not `GHC.Internal.*`
(that prefix is ghc-internal/base). We extract the module from either the dotted
top-level name or the first `(Module.Path)` parenthetical.

Usage:
  ticky_top.py --file NAME [--top N] [--alloc]     # top compiler closures in one test
  ticky_top.py [--top N] [--alloc]                 # aggregated over all .ticky files
  ticky_top.py FUNC_SUBSTR... [--file NAME]        # rows whose STG name matches
"""
import sys, os, re, glob, argparse
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
TOUT = os.path.normpath(os.path.join(HERE, 'ticky-out'))

NAME_COL = 101                       # 0-based start of the STG Name field
MOD_RE   = re.compile(r"^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)+$")
BINDER   = re.compile(r"\{[^}]*\}")  # {v r4ma} unique-binder tags

def module_of(stg):
    """Best-effort defining module of an STG name, or None."""
    # (a) first parenthetical that looks like a module path
    for grp in re.findall(r"\(([^()]*)\)", stg):
        g = grp.strip()
        if MOD_RE.match(g):
            return g
    # (b) dotted prefix of the leading top-level identifier
    head = stg.split('{', 1)[0].split('(', 1)[0].strip()
    if '.' in head:
        mod = head.rsplit('.', 1)[0]
        if MOD_RE.match(mod):
            return mod
    return None

def is_comp(mod):
    return bool(mod) and mod.startswith('GHC.') and not mod.startswith('GHC.Internal.')

def clean(stg):
    """Drop unique-binder tags for stable, readable keys."""
    return re.sub(r'\s+', ' ', BINDER.sub('', stg)).strip()

def parse(path):
    """-> list of (entries, alloc, allocd, stg, module)."""
    rows = []
    in_tbl = False
    with open(path, errors='replace') as f:
        for line in f:
            if not in_tbl:
                if line.lstrip().startswith('Entries') and 'STG Name' in line:
                    in_tbl = True
                continue
            if line.startswith('---'):
                continue
            if not line.strip():
                break                # blank line ends the detailed table
            head = line[:NAME_COL].split()
            if len(head) < 3 or not head[0].lstrip('-').isdigit():
                continue
            try:
                e, a, ad = int(head[0]), int(head[1]), int(head[2])
            except ValueError:
                continue
            stg = line[NAME_COL:].rstrip()
            rows.append((e, a, ad, clean(stg), module_of(stg)))
    return rows

def load(files):
    ent = defaultdict(int); alc = defaultdict(int); ald = defaultdict(int)
    mod = {}
    for p in files:
        for e, a, ad, stg, m in parse(p):
            ent[stg] += e; alc[stg] += a; ald[stg] += ad; mod[stg] = m
    return ent, alc, ald, mod

def files_for(name):
    fs = ([os.path.join(TOUT, name + '.ticky')] if name
          else sorted(glob.glob(os.path.join(TOUT, '*.ticky'))))
    fs = [f for f in fs if os.path.exists(f)]
    if not fs:
        sys.exit(f"no ticky files in {TOUT}" + (f" for {name}" if name else ""))
    return fs

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('funcs', nargs='*')
    ap.add_argument('--file', default=None)
    ap.add_argument('--top', type=int, default=25)
    ap.add_argument('--alloc', action='store_true', help='rank by Alloc instead of Entries')
    ap.add_argument('--all', action='store_true', help='include non-compiler closures')
    a = ap.parse_args()
    files = files_for(a.file)
    print(f"# files: {', '.join(os.path.basename(f)[:-6] for f in files)}\n")
    ent, alc, ald, mod = load(files)
    keys = list(ent)
    if not a.all:
        keys = [k for k in keys if is_comp(mod.get(k))]
    if a.funcs:
        keys = [k for k in keys if any(s in k for s in a.funcs)]
    metric = alc if a.alloc else ent
    keys.sort(key=lambda k: metric[k], reverse=True)
    tot_e = sum(ent[k] for k in keys); tot_a = sum(alc[k] for k in keys)
    lab = 'Alloc' if a.alloc else 'Entries'
    print(f"# ranked by {lab}; {len(keys)} closures; "
          f"sum entries {tot_e:,}, sum alloc {tot_a:,}\n")
    print(f"{'Entries':>15} {'Alloc(B)':>15} {'Allocd(B)':>15}  STG name")
    for k in keys[:a.top]:
        print(f"{ent[k]:>15,} {alc[k]:>15,} {ald[k]:>15,}  {k}")

if __name__ == '__main__':
    main()
