{-# LANGUAGE PatternSynonyms, GADTs #-}
data A x y = (Num x, Eq y) => B

data R = R{ rX :: Int }

pattern P = B
