import System.Environment
import GHC.Exts

g :: (Int -> (Int, Int)) -> Int
-- Should *not* infer strictness SC(S,P(SL,SL)) for h
-- Otherwise `main` could use CbV on the error exprs below
g h = fst (h 0) + snd (h 1)
{-# NOINLINE g #-}

main = do
  m <- length <$> getArgs
  -- The point here is that we print "1".
  -- That means we may *not* use CbV/let-to-case on err!
  -- GHC.Exts.lazy so that we don't lambda lift and float out the
  -- obviously bottoming RHS (where it is no longer used strictly).
  let err = GHC.Exts.lazy $ error (show m)
  let f n | even n    = (n+m, err)
          | otherwise = (err, n+m)
  print $! g f
