-- !!! deriving Ord on d. type with a single nullary constructor.
-- (from ghc-2.10 panic - as reported by Sergey Mechveliani <mechvel@botik.ru>)
--  
module ShouldSucceed where

data D = D deriving (Eq,Ord)
