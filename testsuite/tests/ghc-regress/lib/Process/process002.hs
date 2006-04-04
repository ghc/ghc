{-# OPTIONS -cpp #-}
import System.Process
import System.IO

#ifdef mingw32_HOST_OS
cmd = "c:/cygwin/bin/ls"
#else
cmd = "/bin/ls"
#endif

main = do
  h <- openFile "output" WriteMode
  ph <- runProcess cmd [] Nothing Nothing Nothing (Just h) (Just h)
  waitForProcess ph
  return ()
