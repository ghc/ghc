{-# LANGUAGE GADTs #-}

module T5217 where
import Language.Haskell.TH

$([d| data T a b where { T1 :: Int -> T Int Char 
                       ; T2 :: a -> T a a
                       ; T3 :: a -> T [a] a
                       ; T4 :: a -> b -> T b [a] } |])


