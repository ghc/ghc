module Main where

import Control.Exception (ErrorCall, evaluate, try)
import GHC.Exts (build)

-- | A unary count with a lazy predecessor.
data Size = Zero | Succ Size

-- | Count list cells without inspecting their elements.
size :: [a] -> Size
size [] = Zero
size (_ : xs) = Succ (size xs)

-- | Add the two counts by recursion on the first one.
plus :: Size -> Size -> Size
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- | Inspect the seed before emitting a list cell.
{-# NOINLINE producer #-}
producer :: (() -> b -> b) -> b -> b
producer cons seed = seed `seq` cons () seed

-- | Count after appending the right list.
{-# NOINLINE appendCount #-}
appendCount :: [()] -> Size
appendCount ys = size (build producer ++ ys)

-- | Apply 'plus' to the separate counts.
{-# NOINLINE addCounts #-}
addCounts :: [()] -> Size
addCounts ys = plus (size (build producer)) (size ys)

-- | Display the part of the count available before an exception.
observe :: Size -> IO String
observe n = do
    result <- try (evaluate n) :: IO (Either ErrorCall Size)
    case result of
        Left _ -> pure "bottom"
        Right Zero -> pure "Zero"
        Right (Succ rest) -> ("Succ " ++) <$> observe rest

-- | Compare the demands on an undefined list and a defined cons.
main :: IO ()
main = do
    let bottom = errorWithoutStackTrace "right"
    mapM_ report [("bottom right", bottom), ("cons with bottom tail", () : bottom)]
  where
    report (label, ys) = do
        left <- observe (appendCount ys)
        right <- observe (addCounts ys)
        putStrLn (label ++ ": " ++ left ++ " / " ++ right)
