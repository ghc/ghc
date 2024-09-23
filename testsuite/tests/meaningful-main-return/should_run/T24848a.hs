-- Should emit an ERROR, main does not unify with anything valid
{-# LANGUAGE MeaningfulMainReturn #-}

main :: IO Int
main = pure 1
