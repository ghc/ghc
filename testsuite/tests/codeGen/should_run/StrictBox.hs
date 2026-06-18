{-# LANGUAGE BangPatterns #-}

-- | Exercises the strict-field/Strict machinery end to end:
--
--   * a strict value field built from a thunk, which must be forced to a
--     properly-tagged value (EPT) when the constructor is built;
--   * a strict field of /function/ type, whose box is unlifted but is applied
--     as a function (the "might be a function" call path);
--   * worker/wrapper unboxing a strict argument through Strict;
--   * a strict field holding another boxed value, passed around by value.
--
-- Run with -dtag-inference-checks so that any strict field that is not
-- evaluated-and-properly-tagged aborts at runtime.
module Main (main) where

-- Strict value field + strict function field
data Box a = Box !a !(Int -> Int)

{-# NOINLINE mkBox #-}
mkBox :: Int -> Box Int
mkBox n = Box (sum [1..n]) (\x -> x + n)   -- field 1 from a thunk; field 2 a closure

{-# NOINLINE useBox #-}
useBox :: Box Int -> Int
useBox (Box a f) = a + f 100               -- applies the strict function field

-- A strict field holding another boxed value
data Wrap = Wrap !(Box Int)

{-# NOINLINE useWrap #-}
useWrap :: Wrap -> Int
useWrap (Wrap b) = useBox b

-- Worker/wrapper unboxes the strict argument through Strict
{-# NOINLINE strictArg #-}
strictArg :: Int -> Int -> Int
strictArg !x y = x * 1000 + y

main :: IO ()
main = do
  print (useBox (mkBox 5))               -- 15 + (100+5)        = 120
  print (strictArg (3 + 4) 9)            -- 7*1000 + 9          = 7009
  print (useWrap (Wrap (mkBox 3)))       -- 6 + (100+3)         = 109
  print (map (useBox . mkBox) [1, 2, 3]) -- [102, 105, 109]
