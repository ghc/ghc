import System.IO
import GHC.IO.Handle
import GHC.IO.FD as FD

main = do
  writeFile "T4808.test" "This is some test data"
  (fd, _) <- FD.openFile "T4808.test" ReadWriteMode False
  hdl <- mkDuplexHandle fd "T4808.test" Nothing nativeNewlineMode
  hClose hdl
  (fd2, _) <- FD.openFile "T4808.test" ReadWriteMode False
  print (fdFD fd == fdFD fd2) -- should be True
  hGetLine hdl >>= print -- should fail with an exception

