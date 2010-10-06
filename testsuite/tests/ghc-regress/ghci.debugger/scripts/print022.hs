{-# LANGUAGE MagicHash #-}
import GHC.Exts
data T = C Int# Word# Float# Double# Char# Int Float Double
test = C 1# 32## 1.2# 1.23## 'x'# 1 1.2 1.23

data TwoFields = TwoFields Char Int deriving Show

data T2 = C2 {-# UNPACK #-} !Int {-#UNPACK#-} !Word {-# UNPACK #-} !TwoFields deriving Show
test2 = C2 1 32 (TwoFields 'a' 3)

f x = x