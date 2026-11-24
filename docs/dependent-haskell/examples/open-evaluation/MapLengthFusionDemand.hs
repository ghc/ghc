module MapLengthFusionDemand where

import Control.Exception (ErrorCall, evaluate, try)
import GHC.Exts (build)
import PreludeMapLengthSize (Nat(..), Size, size)

-- | Inspect the prospective first cell before producing False.
{-# NOINLINE producer #-}
producer :: (Bool -> b -> b) -> b -> b
producer cons nil = cons undefined undefined `seq` cons False nil

-- | Keep the source list in the mapper closure. Counting never needs a character.
{-# NOINLINE mapper #-}
mapper :: [Bool] -> Bool -> Char
mapper source _ = source `seq` errorWithoutStackTrace "mapper"

-- | Count the mapped and filtered input at one call site.
{-# NOINLINE mappedCount #-}
mappedCount :: (Bool -> Bool) -> Size
mappedCount p =
    let source = build producer
    in size (map (mapper source) (filter p source))

-- | Count the filtered input at an independent call site.
{-# NOINLINE plainCount #-}
plainCount :: (Bool -> Bool) -> Size
plainCount p = size (filter p (build producer))

-- | Give both counts one shared filtered list.
{-# NOINLINE sharedCounts #-}
sharedCounts :: (Bool -> Bool) -> (Size, Size)
sharedCounts p =
    let source = build producer
        input = filter p source
    in (size (map (mapper source) input), size input)

-- | Inspect only the outer constructor of a count.
observe :: Size -> IO String
observe n = do
    result <- try (evaluate n) :: IO (Either ErrorCall Size)
    pure $ case result of
        Left _ -> "bottom"
        Right Zero -> "Zero"
        Right (Succ _) -> "Succ"

-- | Compare the two call sites with the shared-input control.
main :: IO ()
main = do
    let p = const False
        report label (m, n) = do
            left <- observe m
            right <- observe n
            putStrLn (label ++ "  " ++ left ++ "  " ++ right)
    report "independent" (mappedCount p, plainCount p)
    report "shared" (sharedCounts p)
