{-# LANGUAGE MagicHash #-}
module T8274 where

import GHC.Prim

data P = Positives Int# Float# Double# Char# Word#
data N = Negatives Int# Float# Double#

p = Positives 42# 4.23# 4.23## '4'# 4##
n = Negatives -4# -4.0# -4.0##
