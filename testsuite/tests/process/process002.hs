{-# OPTIONS -cpp #-}
import System.Process
import System.IO

main = do
  h <- openFile "process002.out" WriteMode
  ph <- runProcess "ls" [] Nothing Nothing Nothing (Just h) (Just h)
  waitForProcess ph
  return ()
