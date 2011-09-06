import System.IO
import GHC.IO.Handle
import GHC.IO.FD as FD

main = do
  writeFile "4808.test" "This is some test data"
  (fd, _) <- FD.openFile "4808.test" ReadWriteMode False
  hdl <- mkDuplexHandle fd "4808.test" Nothing nativeNewlineMode
  hClose hdl
  (fd2, _) <- FD.openFile "4808.test" ReadWriteMode False
  print (fdFD fd == fdFD fd2) -- should be True
  hGetLine hdl >>= print -- should fail with an exception

