{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where

import NativeRetainedIndexHpcProvider

data Inputs = Inputs (Tag ('S 'Z)) (Tag ('S 'Z))

readTag :: Tag n -> Int
readTag (Tag n) = n

choose :: Bool -> Inputs -> Int
choose allow (Inputs (Tag 1) second)
    | allow, Tag 2 <- second = 2
choose _ _ = -1

demandedIndex :: Tag ('S 'Z)
demandedIndex = deep (S Z)
{-# NOINLINE demandedIndex #-}

main :: IO ()
main = do
    print (readTag skippedIndex, readTag selectedIndex)
    print (choose False (Inputs selectedIndex guardedIndex))
    print (choose False (Inputs selectedIndex (error "unreached guard")))
    print (choose True (Inputs selectedIndex demandedIndex))
