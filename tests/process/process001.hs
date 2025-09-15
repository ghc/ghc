{-# OPTIONS -cpp #-}
import System.IO
import System.Process

test = do
  h <- openFile "process001.out" WriteMode
  ph <- runProcess "ls" [] Nothing Nothing Nothing (Just h) Nothing
  waitForProcess ph

main = test >> test >> return ()
