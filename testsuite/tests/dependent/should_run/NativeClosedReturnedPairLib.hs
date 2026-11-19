module NativeClosedReturnedPairLib where

import Numeric.Natural (Natural)

pair :: Natural -> (Natural, Natural)
pair n = (n + 1, n)

mixed :: Natural -> ((Natural, Bool), [Char])
mixed n = ((n, odd n), ['\n', 'é', '7', '"', '\\'])

score :: ((Natural, Bool), [Char]) -> Natural
score ((n, True), ['\n', 'é', '7', '"', '\\']) = n + 11
score _ = 0

flagged :: Char -> (Char, Bool)
flagged c = (c, True)

checkEmpty :: (a, Bool) -> [a] -> Natural
checkEmpty (_, True) [] = 11
checkEmpty _ _ = 0

ignorePair :: (Natural, Natural) -> Natural
ignorePair _ = 7

bottomField :: Natural -> (Natural, Natural)
bottomField n = (n, error "pair second field forced")

wrongKind :: Natural -> ((Natural, Bool), [Char])
wrongKind _ = error "wrong-kind pair ran"

unsupported :: Natural -> (Natural, Int)
unsupported _ = error "unsupported pair shape ran"
