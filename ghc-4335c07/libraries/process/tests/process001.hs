{-# OPTIONS -cpp #-}
import System.IO
import System.Process

#ifdef mingw32_HOST_OS
cmd = "ls"
#else
cmd = "/bin/ls"
#endif

test = do
  h <- openFile "process001.out" WriteMode
  ph <- runProcess cmd [] Nothing Nothing Nothing (Just h) Nothing
  waitForProcess ph

main = test >> test >> return ()
