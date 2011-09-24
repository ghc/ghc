import Control.Monad
import System.IO
import System.IO.Error
import GHC.IO.Handle (hSetEncoding)

main = do
  -- Explicitly set stdout encoding so that the UTF8//ROUNDTRIP
  -- test is always able to write the surrogate byte out without error.
  enc <- mkTextEncoding "UTF8//ROUNDTRIP"
  hSetEncoding stdout enc
  alltests "decodingerror002.in"

alltests file = mapM (test file)  ["UTF8",
                                   "UTF8//IGNORE",
                                   "UTF8//TRANSLIT",
                                   "UTF8//ROUNDTRIP"]

test file enc_name = do
  h <- openFile file ReadMode
  enc <- mkTextEncoding enc_name
  hSetEncoding h enc
  e <- try $ forever $ hGetChar h >>= putChar
  print (e :: Either IOError ())
