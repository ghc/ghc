#!/usr/bin/env python3
"""Offline analysis of captured InScopeSet key populations (scratch, not to land).

Reads dump files produced by the throwaway GHC_DUMP_INSCOPE instrumentation in
GHC.Types.Var.Env (unsafeGetFreshLocalUnique): one in-scope set per line, keys as
space-separated decimal Word64.

For each set it measures, for two key orderings of a big-endian PATRICIA trie:
  - tag-major   : key as stored, [tag:8 high][number:56 low]
  - number-major: rotated, ((k & mask56) << 8) | (k >> 56) -> [number:56 high][tag:8 low]
the external path length (sum of leaf depths), average leaf depth, and max depth.
It also reports the tag-strip collision rate (distinct low-56 numbers vs n) to test
the hypothesis that numbers are already globally unique (single genSym counter).

Node count (n-1 branch nodes) is asserted equal across both orderings: a bit
permutation cannot change it.

Usage: analyze_uniq_sets.py FILE [FILE ...]
"""
import sys

MASK56 = (1 << 56) - 1


def to_number_major(k):
    return ((k & MASK56) << 8) | (k >> 56)


def trie_stats(keys_sorted):
    """External path length, max depth, and branch-node count for a big-endian
    PATRICIA trie over the given sorted, distinct keys.

    Iterative split on the highest differing bit (critbit). Each split adds a
    branch node and +1 depth to every leaf beneath it, so EPL accumulates the
    subrange length at each split.
    """
    epl = 0
    branches = 0
    max_depth = 0
    # stack of (lo, hi, depth) half-open index ranges into keys_sorted
    stack = [(0, len(keys_sorted), 0)]
    while stack:
        lo, hi, depth = stack.pop()
        n = hi - lo
        if n == 1:
            if depth > max_depth:
                max_depth = depth
            continue
        klo = keys_sorted[lo]
        khi = keys_sorted[hi - 1]
        b = (klo ^ khi).bit_length() - 1   # highest differing bit
        # threshold = common high prefix (above bit b) with bit b set
        threshold = ((klo >> (b + 1)) << (b + 1)) | (1 << b)
        # binary search for first index in [lo,hi) whose key >= threshold
        a, z = lo, hi
        while a < z:
            m = (a + z) // 2
            if keys_sorted[m] < threshold:
                a = m + 1
            else:
                z = m
        split = a
        branches += 1
        epl += n            # this branch deepens all n leaves below it by 1
        stack.append((lo, split, depth + 1))
        stack.append((split, hi, depth + 1))
    return epl, max_depth, branches


def analyze_set(keys):
    n = len(keys)
    tags = {k >> 56 for k in keys}
    numbers = [k & MASK56 for k in keys]
    distinct_numbers = len(set(numbers))
    collisions = n - distinct_numbers
    span = max(numbers) - min(numbers)

    tag_major = sorted(keys)
    num_major = sorted(to_number_major(k) for k in keys)

    epl_t, maxd_t, br_t = trie_stats(tag_major)
    epl_n, maxd_n, br_n = trie_stats(num_major)
    assert br_t == br_n == n - 1, (br_t, br_n, n)

    return {
        "n": n,
        "tags": len(tags),
        "collisions": collisions,
        "span": span,
        "avg_t": epl_t / n,
        "avg_n": epl_n / n,
        "max_t": maxd_t,
        "max_n": maxd_n,
    }


def main(argv):
    if len(argv) < 2:
        sys.exit(__doc__)
    sys.setrecursionlimit(100000)

    rows = []
    for path in argv[1:]:
        with open(path) as fh:
            for line in fh:
                line = line.strip()
                if not line:
                    continue
                keys = [int(t) for t in line.split()]
                if len(keys) < 2:
                    continue
                rows.append(analyze_set(keys))

    if not rows:
        sys.exit("no sets parsed")

    total_keys = sum(r["n"] for r in rows)
    # size-weighted average leaf depth
    wavg_t = sum(r["avg_t"] * r["n"] for r in rows) / total_keys
    wavg_n = sum(r["avg_n"] * r["n"] for r in rows) / total_keys
    total_collisions = sum(r["collisions"] for r in rows)
    max_max_t = max(r["max_t"] for r in rows)
    max_max_n = max(r["max_n"] for r in rows)

    print(f"sets analysed         : {len(rows)}")
    print(f"total keys            : {total_keys}")
    print(f"set size  min/med/max : {min(r['n'] for r in rows)} / "
          f"{sorted(r['n'] for r in rows)[len(rows)//2]} / "
          f"{max(r['n'] for r in rows)}")
    print(f"distinct tags  min/max: {min(r['tags'] for r in rows)} / "
          f"{max(r['tags'] for r in rows)}")
    print()
    print(f"tag-strip collisions  : {total_collisions} / {total_keys} keys "
          f"({100.0 * total_collisions / total_keys:.4f}%)")
    sets_with_coll = sum(1 for r in rows if r["collisions"] > 0)
    print(f"sets with any collision: {sets_with_coll} / {len(rows)}")
    print()
    print("            size-weighted avg leaf depth   worst-case max depth")
    print(f"tag-major  : {wavg_t:7.3f}                        {max_max_t}")
    print(f"number-major: {wavg_n:6.3f}                        {max_max_n}")
    print()
    improved = sum(1 for r in rows if r["avg_n"] < r["avg_t"])
    print(f"sets where number-major has lower avg depth: {improved} / {len(rows)}")
    print(f"avg-depth reduction (size-weighted): "
          f"{100.0 * (wavg_t - wavg_n) / wavg_t:.2f}%")


if __name__ == "__main__":
    main(sys.argv)
