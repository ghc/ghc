{-# OPTIONS_GHC -fglasgow-exts #-}
import GHC.Exts
data T = C Int# Float# Double# Char# Int Float Double
test = C 1# 1.2# 1.23## 'x'# 1 1.2 1.23
