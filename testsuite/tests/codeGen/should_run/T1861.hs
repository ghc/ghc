import System.Environment
main = do [x] <- getArgs
          print (read x < (1e400 :: Double))
          print (read x < (-1e400 :: Double))
          print (read x == (0/0 :: Double))
          -- the last doesn't get constant-folded to NaN, so we're not really
          -- testing properly here.  Still, we might manage to constant fold
          -- this in the future, so I'll leave it in place.

