{-

Before running this, check that /tmp/test does not exist and
contain something important. Then do:

 $ touch /tmp/test

If you do:

 $ runhaskell Test.hs

it will work. If you do:

 $ runhaskell Test.hs fail

it will fail every time with:

Test.hs: writeFile: /tmp/test: openFile: resource busy (file is locked)

-}

import Control.Monad
import System.Directory
import System.IO
import System.IO.Error
import System.Environment
-- Used by test2:
-- import System.Posix.IO

fp = "T2122-test"

main :: IO ()
main = do
   writeFile fp "test"
   test True

-- fails everytime when causeFailure is True in GHCi, with runhaskell,
-- or when compiled.
test :: Bool -> IO ()
test causeFailure =
    do h1 <- openFile fp ReadMode `catchIOError` (\e -> error ("openFile 1: " ++ show e))
       when causeFailure $ do
         h2 <- openFile fp ReadMode `catchIOError` (\e -> error ("openFile 2: " ++ show e))
         hClose h2
       hClose h1
       removeFile fp
       writeFile fp (show [1..100]) `catchIOError` (\e -> error ("writeFile: " ++ show e))

{-
-- this version never fails (except in GHCi, if test has previously failed).
-- probably because openFd does not try to lock the file
test2 :: Bool -> IO ()
test2 causeFailure =
    do fd1 <- openFd fp ReadOnly Nothing defaultFileFlags `catchIOError` (\e -> error ("openFile 1: " ++ show e))
       when causeFailure $ do
         fd2 <- openFd fp ReadOnly Nothing defaultFileFlags `catchIOError` (\e -> error ("openFile 2: " ++ show e))
         closeFd fd2
       closeFd fd1
       removeFile fp
       writeFile fp (show [1..100]) `catchIOError` (\e -> error ("writeFile: " ++ show e))
-}

{-
-- fails sometimes when run repeated in GHCi, but seems fine with
-- runhaskell or compiled
test3 :: IO ()
test3 =
    do h1 <- openFile fp ReadMode `catchIOError` (\e -> error ("openFile 1: " ++ show e))
       h2 <- openFile fp ReadMode `catchIOError` (\e -> error ("openFile 2: " ++ show e))
       removeFile fp
       writeFile fp (show [1..100]) `catchIOError` (\e -> error ("writeFile: " ++ show e))
       print =<< hGetContents h1
       print =<< hGetContents h2
       hClose h2
       hClose h1
-}

