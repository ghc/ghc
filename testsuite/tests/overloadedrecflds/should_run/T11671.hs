{-# LANGUAGE OverloadedLabels, DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
import GHC.OverloadedLabels

data D = A Int | B String | C String deriving Eq

instance IsLabel "A" (Int -> D) where fromLabel = A
instance IsLabel "B" (String -> D) where fromLabel = B
instance IsLabel "C" (String -> D) where fromLabel = C

main :: IO ()
main = mapM_ print $ (==) <$> [#A (5 :: Int), #B "axe", #C "green"]
                          <*> [ A (5 :: Int),  B "axe",  C "green"]
