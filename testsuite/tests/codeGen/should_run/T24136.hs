-- A taggable (small-family BoxedRep) value in normal form must never be
-- entered: every reference to it has to carry the constructor's pointer tag.
-- That tag is conveyed across module boundaries by the serialized
-- LambdaFormInfo, which is part of the ABI and must be present even at -O0,
-- and it must survive hs-boot indirections.  When the invariant is enforced
-- (the entry code of a taggable constructor panics), each scrutinee below
-- crashes at run time if the tag was lost, so this program exercises:
--
--   * a cross-module nullary constructor          (theE0, theE2)
--   * a cross-module non-nullary constructor       (theE1)
--   * an existential dictionary package            (aBox)
--   * a constructor reached through an hs-boot
--     import cycle                                 (mkT, via cVal)
module Main where

import T24136A (E(..), Box(..), theE0, theE1, theE2, aBox)
import T24136B (T(..), mkT)
import T24136C (cVal)

showE :: E -> String
showE E0     = "E0"
showE (E1 n) = "E1 " ++ show n
showE E2     = "E2"

main :: IO ()
main = do
  putStrLn (showE theE0)
  putStrLn (showE theE2)
  putStrLn (showE theE1)
  case aBox of Box x -> putStrLn (show x)
  putStrLn (case mkT of Tx n -> "Tx " ++ show n; Ty -> "Ty")
  print cVal
