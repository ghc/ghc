{-# LANGUAGE DataKinds #-}
module T13025 where
import T13025a

type MyRec = Rec '[ '("A",Int), '("B",Int), '("C",Int) ]

getC :: MyRec -> Int
getC = getField (Proxy::Proxy '("C",Int))

doubleC :: MyRec -> MyRec
doubleC r = setC (2 * (getC r)) r
  where setC = set . (Field :: Int -> Field '("C",Int))

main :: IO ()
main = print (getC (Field 1 :& Field 2 :& Field 3 :& Nil :: MyRec))
