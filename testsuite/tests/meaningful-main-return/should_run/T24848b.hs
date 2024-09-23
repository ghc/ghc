-- Should emit a WARNING, as main does not unify with anything valid, but the
-- extension is turned off.
{-# LANGUAGE NoMeaningfulMainReturn #-}

main :: IO Int
main = pure 1
