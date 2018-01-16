{-# OPTIONS -cpp #-}
import System.Process
import System.IO

#ifdef mingw32_HOST_OS
cmd = "ls"
#else
cmd = "/bin/ls"
#endif

main = do
  h <- openFile "process002.out" WriteMode
  ph <- runProcess cmd [] Nothing Nothing Nothing (Just h) (Just h)
  waitForProcess ph
  return ()
