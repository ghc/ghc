
import GHC.Conc

main = do
  let x = length [1..100]
  numSparks >>= print
  x `par` numSparks >>= print
  x `par` numSparks >>= print
  x `par` numSparks >>= print
  x `par` numSparks >>= print

