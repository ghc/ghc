{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.Fix
import Language.Haskell.TH
import Control.Monad.State

-- Direct variant
$([d|
  f1, f2 :: Integer -> [Integer]
  f1 = \z -> z : f2 (succ z)
  f2 = \z -> z : f1 (z * z)
  |])

-- Using mfix.
-- This is a contrived example, but it fits into a single splice
$(fmap (\(x,x',y,y') ->
    [ ValD (VarP x') (NormalB x) []
    , ValD (VarP y') (NormalB y) []
    ]) $
  mfix $ \ ~(_,x',_,y') -> do
    x <- [| \z -> z : $(return $ VarE y') (succ z) |]
    y <- [| \z -> z : $(return $ VarE x') (z * z)  |]
    x'' <- newName "g1"
    y'' <- newName "g2"
    return (x, x'', y, y'')
 )


main :: IO ()
main = do
    print $ take 11 $ f1 0
    print $ take 11 $ g1 0
