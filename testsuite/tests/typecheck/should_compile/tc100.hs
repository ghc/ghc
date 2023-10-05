-- !!! Caused ghc-3.03 and 4.01 tc to enter a 
-- !!! a blackhole (as reported by P. Callaghan.)
module ShouldCompile where

type C a = D a -> a
newtype D a = DD (D_ a)
type D_ a = C (Maybe a)
