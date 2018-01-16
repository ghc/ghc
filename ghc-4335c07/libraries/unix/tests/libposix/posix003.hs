
import Control.Monad
import Data.Char
import System.Exit
import System.IO
import System.Process

main = do hw <- openFile "po003.out" WriteMode
          ph <- runProcess "pwd" [] (Just "/dev") Nothing Nothing (Just hw) Nothing
          ec <- waitForProcess ph
          hClose hw
          unless (ec == ExitSuccess) $ error "pwd failed"
          hr <- openFile "po003.out" ReadMode
          output <- hGetContents hr
          putStrLn ("Got: " ++ show (filter (not . isSpace) output))
          hClose hr

