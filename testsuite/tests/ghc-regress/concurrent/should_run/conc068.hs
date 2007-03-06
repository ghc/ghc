import Data.List
import Data.Char
import Control.Parallel
import System.IO

{-# NOINLINE fool #-}
-- 'fool' just makes sure CSE doesn't meddle with the code below
fool :: [a] -> [a]
fool (x:xs) = xs

main = do
    hSetBuffering stdin NoBuffering
    hs <- getContents
    let -- create copies of the input
        ts = map (:hs) ['a'..'z']
        -- sum the characters of each copy
        qs = map (foldl' (+) 0 . map ord . fool) ts
        -- in parallel
        rs = foldr (\x y -> x `par` y `par` (x:y)) [] qs
    -- compare the results and print 'True' if they are all equal.
    -- This should never print 'False'
    print $ all (uncurry (==)) $ zip rs (tail rs)
