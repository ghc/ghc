#!/usr/bin/env python3
# usage: T27272_gen.py N > T27272_Lits.hs
import sys

n = int(sys.argv[1])
out = sys.stdout
out.reconfigure(encoding="utf-8")

kinds = [
    ("a", "asciiLits", "A{}-ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    ("u", "utf8Lits", "U{}-λμν-ÄÖÜ-€"),
]

out.write("module T27272_Lits (asciiLits, utf8Lits) where\n")
for prefix, list_name, fmt in kinds:
    out.write("\n")
    for i in range(n):
        out.write(f'{prefix}{i} :: String\n{prefix}{i} = "{fmt.format(i)}"\n')
    out.write(f"\n{list_name} :: [String]\n{list_name} =\n  [ {prefix}0\n")
    for i in range(1, n):
        out.write(f"  , {prefix}{i}\n")
    out.write("  ]\n")
