#! /usr/bin/env python3

# See #17264.
#
# Generates a module of Luc Maranget's S series
#
#   module S where
#
#   data T = A | B
#
#   s :: T -> T -> T -> T -> T -> T -> T -> T -> ()
#   s A A _ _ _ _ _ _ = ()
#   s _ _ A A _ _ _ _ = ()
#   s _ _ _ _ A A _ _ = ()
#   s _ _ _ _ _ _ A A = ()
#   s A B A B A B A B = ()
#
# Note how each clause splits into 2 uncovered Deltas
# The last clause isn't strictly necessary to exhibit
# exponential behavior in our algorithm, but the number
# of missing matches is 2^n-1, easily demonstrated by
# looking at the missing matches for S_2:
#
#   B _ B _
#   A B B _
#   B _ A B

import sys

size = int(sys.argv[1]) if len(sys.argv) > 1 else 5

a = 'A'
b = 'B'
wc = '_'

def horiz_join(M, N):
    return [ row_M + row_N for (row_M, row_N) in zip(M, N) ]

def vert_join(M, N):
    return M + N

def n_cols(M):
    return len(M[0])

def s_rec(n):
    if n == 1:
        return [ [ a, a ] ]
    s_sub = s_rec(n-1)
    top_row = [ [ a, a ] + [ wc ]*n_cols(s_sub) ]
    left_col = [ [ wc, wc ] ]*len(s_sub)
    return vert_join(top_row, horiz_join(left_col, s_sub))

def s(n):
    return vert_join(s_rec(n), [ [ a, b ]*n ])

def debug_print(M):
    for row in M:
        print("".join(row))

def format_hs(M):
    hdr = """module S where

data T = A | B

"""

    fun = "s :: " + "T -> "*n_cols(M) + "()\n"
    for row in M:
        fun += "s " + " ".join(row) + "= ()\n"
    return hdr + fun

open("S.hs", "w").write(format_hs(s(size)))
