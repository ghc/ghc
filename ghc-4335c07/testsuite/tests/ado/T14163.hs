{-# language ApplicativeDo #-}

import GHC.Exts

readIt :: IO (Int, Int)
readIt = readLn

main :: IO ()
main = do
  (_, _) <- readIt
  (_, _) <- readIt
  (_, _) <- readIt
  print "Done"
