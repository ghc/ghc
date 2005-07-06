import System.Process
import System.IO

main = do
  h <- openFile "output" WriteMode
  ph <- runProcess "/bin/ls" [] Nothing Nothing Nothing (Just h) (Just h)
  waitForProcess ph
  return ()
