module T25109 where

data R = R { fld :: Int }

foo :: R -> IO ()
foo r = case fld r of
  !i -> print i

main :: IO ()
main = foo (R 1)
