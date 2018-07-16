{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module Main(main) where
import GHC.TypeLits as L
import Data.Proxy

--------------------------------------------------------------------------------
-- Test top-reactions

tsub :: Proxy (x + y) -> Proxy y -> Proxy x
tsub _ _ = Proxy

tsub2 :: Proxy (x + y) -> (Proxy x, Proxy y)
tsub2 _ = (Proxy, Proxy)

tdiv :: Proxy (x L.* y) -> Proxy y -> Proxy x
tdiv _ _ = Proxy

tdiv2 :: Proxy (x L.* y) -> (Proxy x, Proxy y)
tdiv2 _ = (Proxy, Proxy)

troot :: Proxy (x ^ y) -> Proxy y -> Proxy x
troot _ _ = Proxy

tlog :: Proxy (x ^ y) -> Proxy x -> Proxy y
tlog _ _ = Proxy

tleq :: ((x <=? y) ~ True) => Proxy y -> Proxy x
tleq _ = Proxy

main :: IO ()
main = print [ sh (tsub  (Proxy :: Proxy 5) (Proxy :: Proxy 3)) == "2"
             , let (p, q) = tsub2 (Proxy :: Proxy 0)
               in (sh p, sh q) == ("0", "0")
             , sh (tdiv  (Proxy :: Proxy 8) (Proxy :: Proxy 2)) == "4"
             , let (p, q) = tdiv2 (Proxy :: Proxy 1)
               in (sh p, sh q) == ("1", "1")
             , sh (troot (Proxy :: Proxy 9) (Proxy :: Proxy 2)) == "3"
             , sh (tlog  (Proxy :: Proxy 8) (Proxy :: Proxy 2)) == "3"
             , sh (tleq  (Proxy :: Proxy 0))                    == "0"
             ]
  where
  sh x = show (natVal x)
