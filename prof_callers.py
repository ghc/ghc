#!/usr/bin/env python3
"""Caller-context attribution from the late-CCS JSON profile (prof-out/*.prof).

The piece callgrind and ticky could not give: for a hot function, *which source
call sites* drive its entries and the allocation incurred under it -- measured
through laziness (the cost-centre stack threads the logical caller through thunks).

The JSON `profile` is a cost-centre *stack* tree: each node = (cc id, entries,
alloc, children), where alloc/entries are the node's own (this-cc-in-this-stack)
values and children are its callees in that stack. We compute, per node, the
*inherited* alloc (node + whole subtree) once, then:

  * flat (--top):  per cc, total individual alloc + entries, ranked. (Sanity-check
    against ticky.)
  * callers (FUNC): find the target cc(s) (compiler module matching FUNC); take the
    target nodes whose *parent* cc != target (external entry points into a possibly
    self-recursive region -- avoids double-counting the recursion) and aggregate by
    parent cc -> (entries into target, inherited alloc under target). Ranked by
    entries (the 'use it less' lever); alloc shown alongside.

Usage:
  prof_callers.py --file NAME --top N            # flat top compiler ccs
  prof_callers.py FUNC_SUBSTR... --file NAME      # caller breakdown for each target
  prof_callers.py FUNC_SUBSTR... --file NAME --by alloc   # rank callers by alloc
"""
import sys, os, json, glob, argparse
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
POUT = os.path.normpath(os.path.join(HERE, 'prof-out'))

def is_comp(mod):
    return bool(mod) and mod.startswith('GHC.') and not mod.startswith('GHC.Internal.')

def load(path):
    d = json.load(open(path))
    cc = {c['id']: (c['label'], c['module']) for c in d['cost_centres']}
    return d['profile'], cc

def compute_inherited(root):
    """-> dict id(node)->inherited alloc. Iterative post-order (trees are deep)."""
    inh = {}
    stack = [(root, False)]
    while stack:
        node, done = stack.pop()
        if done:
            inh[id(node)] = node['alloc'] + sum(inh[id(c)] for c in node['children'])
        else:
            stack.append((node, True))
            for c in node['children']:
                stack.append((c, False))
    return inh

def flat(root):
    """-> (ent: ccid->entries, alc: ccid->individual alloc)."""
    ent = defaultdict(int); alc = defaultdict(int)
    stack = [root]
    while stack:
        n = stack.pop()
        ent[n['id']] += n['entries']; alc[n['id']] += n['alloc']
        stack.extend(n['children'])
    return ent, alc

def walk_callers(root, targets, inh):
    """For nodes whose cc in targets and whose parent cc not in targets, group by
    parent cc -> [entries, inherited_alloc]. Also total entries/individual alloc."""
    by_parent = defaultdict(lambda: [0, 0])
    tot_ent = 0; tot_self_alloc = 0
    stack = [(root, None)]            # (node, parent cc id)
    while stack:
        n, pcc = stack.pop()
        ncc = n['id']
        if ncc in targets:
            tot_ent += n['entries']; tot_self_alloc += n['alloc']
            if pcc not in targets:    # external entry into the target region
                e = by_parent[pcc]; e[0] += n['entries']; e[1] += inh[id(n)]
        for c in n['children']:
            stack.append((c, ncc))
    return by_parent, tot_ent, tot_self_alloc

def files_for(name):
    fs = ([os.path.join(POUT, name + '.prof')] if name
          else sorted(glob.glob(os.path.join(POUT, '*.prof'))))
    fs = [f for f in fs if os.path.exists(f)]
    if not fs:
        sys.exit(f"no .prof in {POUT}" + (f" for {name}" if name else ""))
    return fs

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('funcs', nargs='*')
    ap.add_argument('--file', default=None)
    ap.add_argument('--top', type=int, default=20)
    ap.add_argument('--by', choices=['entries', 'alloc'], default='entries')
    a = ap.parse_args()
    for path in files_for(a.file):
        root, cc = load(path)
        name = os.path.basename(path)[:-5]
        inh = compute_inherited(root)
        if not a.funcs:
            ent, alc = flat(root)
            keys = [k for k in ent if is_comp(cc.get(k, (None, None))[1])]
            keys.sort(key=lambda k: alc[k], reverse=True)
            print(f"\n##### {name}: top compiler cost centres by individual alloc")
            print(f"{'Entries':>14} {'Alloc(B)':>16}  label (module)")
            for k in keys[:a.top]:
                lab, mod = cc[k]
                print(f"{ent[k]:>14,} {alc[k]:>16,}  {lab} ({mod})")
            continue
        ent, alc = flat(root)
        for sub in a.funcs:
            cand = [k for k, (lab, mod) in cc.items()
                    if sub in lab and is_comp(mod) and alc[k] + ent[k] > 0]
            if not cand:
                print(f"\n##### {name}: no compiler cc matching {sub!r}"); continue
            targets = set(cand)
            tgt_lab = sorted({cc[k] for k in cand})
            by, te, tsa = walk_callers(root, targets, inh)
            print(f"\n##### {name}: callers of {sub!r}  "
                  f"[{', '.join(f'{l} ({m})' for l, m in tgt_lab[:3])}"
                  f"{' …' if len(tgt_lab) > 3 else ''}]")
            print(f"   total entries {te:,}, individual alloc {tsa:,} B")
            rows = sorted(by.items(),
                          key=lambda kv: kv[1][1 if a.by == 'alloc' else 0],
                          reverse=True)
            print(f"   external callers (by {a.by}):")
            print(f"   {'Entries':>14} {'Alloc-under(B)':>16}  caller")
            for pcc, (e, al) in rows[:12]:
                lab, mod = cc.get(pcc, ('<root>', ''))
                print(f"   {e:>14,} {al:>16,}  {lab} ({mod})")

if __name__ == '__main__':
    main()
