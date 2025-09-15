{-# LANGUAGE PolyKinds  #-}

data D a = A | B

f d@A = const True d
f B = False
