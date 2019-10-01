#! /usr/bin/env python3

import sys

# See #17264.
#
# Generates a module of Luc Maranget's V series

size = int(sys.argv[1]) if len(sys.argv) > 1 else 3

a = 'A'
b = 'B'
wc = '_'

def horiz_join(M, N):
    return [ row_M + row_N for (row_M, row_N) in zip(M, N) ]

def vert_join(M, N):
    return M + N

def b_m(n):
    if n == 1: return [ [ b ] ]
    b_sub = b_m(n-1)
    top_row = [ [ b ] + [ wc ]*len(b_sub) ]
    left_col = [ [ wc ] ] * len(b_sub)
    return vert_join(top_row, horiz_join(left_col, b_sub))

def n_cols(M):
    return len(M[0])

def v(n):
    if n == 1:
        return [ [ a ], [ b ] ]
    b_sub = b_m(n)
    v_sub = v(n-1)
    top_row = [ [ a ]*n + [ wc ]*n_cols(v_sub) ]
    return vert_join(top_row, horiz_join(b_sub, v_sub))

def debug_print(M):
    for row in M:
        print("".join(row))

def format_hs(M):
    hdr = """module V where

data T = A | B

"""

    fun = "v :: " + "T -> "*n_cols(M) + "()\n"
    for row in M:
        fun += "v " + " ".join(row) + "= ()\n"
    return hdr + fun

open("V.hs", "w").write(format_hs(v(size)))
