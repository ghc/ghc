{-# LANGUAGE PatternSynonyms #-}

module Export (A(..,MyB), B(MyA), C(MyC)) where

data A = A

data B = B

pattern MyB = B

pattern MyA = A

data C a = C

pattern MyC = B
