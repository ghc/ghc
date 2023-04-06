{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.IO
import System.IO

main :: IO ()
main = do
  IO $ \s -> keepAlive# () s (\s' f -> unIO f s')
                             (putStrLn "This should get printed.")
  hFlush stdout
