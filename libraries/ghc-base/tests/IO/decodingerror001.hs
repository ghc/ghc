import Control.Monad
import System.IO
import System.IO.Error
import GHC.IO.Encoding (utf8)
import GHC.IO.Handle (hSetEncoding)

testfiles = ["decodingerror001.in1", "decodingerror001.in2"]

main = mapM_ alltests testfiles

alltests file = mapM (test file)  [NoBuffering,
                                   LineBuffering,
                                   BlockBuffering Nothing,
                                   BlockBuffering (Just 9),
                                   BlockBuffering (Just 23) ]

test file bufmode = do
  h <- openFile file ReadMode
  hSetEncoding h utf8
  hSetBuffering h bufmode
  e <- tryIOError $ forever $ hGetChar h >>= putChar
  print (e :: Either IOError ())
