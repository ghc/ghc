-- !!! data ctor (re-)exportation 
module M (T(M1), M2) where

data T = M1 Int | M2 Int

