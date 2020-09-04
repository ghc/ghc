import Data.Ord

doClampInt :: (Int, Int) -> Int -> IO ()
doClampInt bounds a = print $ clamp bounds a

doClampFloat :: (Float, Float) -> Float -> IO ()
doClampFloat bounds a = print $ clamp bounds a

nan :: Float
nan = 0 / 0

main :: IO ()
main = do
  doClampInt (0, 100) 50       -- 50
  doClampInt (0, 100) 200      -- 100
  doClampInt (0, 100) (-5)     -- 0

  doClampFloat (0, 100) 50     -- 50
  doClampFloat (0, 100) 200    -- 100
  doClampFloat (0, 100) (-5)   -- 0
  doClampFloat (0, 100) nan    -- NaN
  doClampFloat (nan, 100) 5    -- 5
  doClampFloat (nan, 100) 105  -- 100
  doClampFloat (5, nan) 105    -- 105
  doClampFloat (5, nan) 3      -- 5

  doClampFloat (nan, nan) 3    -- 3

