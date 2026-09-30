import System.IO.Error

main :: IO ()
main =
  modifyIOError (`ioeSetLocation` "outer") $
  modifyIOError (`ioeSetLocation` "inner") $
  ioError (mkIOError doesNotExistErrorType "origin" Nothing (Just "file"))
