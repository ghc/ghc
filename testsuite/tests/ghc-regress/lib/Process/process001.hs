import System.IO
import System.Process

test = do
  h <- openFile "output" WriteMode
  ph <- runProcess "/bin/ls" [] Nothing Nothing Nothing (Just h) Nothing
  waitForProcess ph

main = test >> test >> return ()
