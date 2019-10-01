#! /usr/bin/env python3

import sys

# See #17264.
#
# Generates a module of Luc Maranget's T series

size = int(sys.argv[1]) if len(sys.argv) > 1 else 3

a = 'A'
b = 'B'
wc = '_'

def horiz_join(M, N):
    return [ row_M + row_N for (row_M, row_N) in zip(M, N) ]

def vert_join(M, N):
    return M + N

def n_cols(M):
    return len(M[0])

def t(n):
    if n == 1:
        return [ [ a ], [ b ] ]

    t_sub = t(n-1)
    left_col = [ [ wc ] ]*len(t_sub)
    top_rows = [ [a]*n, [b]*n ]
    return vert_join(top_rows, horiz_join(left_col, t_sub))

def debug_print(M):
    for row in M:
        print("".join(row))

def format_hs(M):
    hdr = """module T where

data T = A | B

"""

    fun = "t :: " + "T -> "*n_cols(M) + "()\n"
    for row in M:
        fun += "t " + " ".join(row) + "= ()\n"
    return hdr + fun

open("T.hs", "w").write(format_hs(t(size)))
