import System.IO
import GHC.IO.Handle
import GHC.IO.Encoding
import System.Environment

-- Test switching encodings
-- The test file is built by the Makefile

main = do
  [file] <- getArgs
  test file NoBuffering
  test file (BlockBuffering Nothing)
  test file (BlockBuffering (Just 5))

test file buf = do
  hSetEncoding stdout utf8
  h <- openBinaryFile file ReadMode
  hSetBuffering stdout buf
  putStrLn "no encoding:"
  getUntilX h
  hSetEncoding h utf8
  putStrLn "UTF8:"
  getUntilX h
  hSetEncoding h utf16le
  putStrLn "UTF16LE:"
  getUntilX h
  hSetEncoding h utf16be
  putStrLn "UTF16BE:"
  getUntilX h
  hSetEncoding h utf16
  putStrLn "UTF16:"
  getUntilX h
  hSetEncoding h utf32
  putStrLn "UTF32:"
  getUntilX h
  hSetEncoding h utf32le
  putStrLn "UTF32LE:"
  getUntilX h
  hSetEncoding h utf32be
  putStrLn "UTF32BE:"
  getUntilX h
  hSetEncoding h utf8_bom
  putStrLn "UTF8-BOM:"
  getUntilX h
  hIsEOF h >>= print

getUntilX h = do
  c <- hGetChar h
  if c == 'X' then return () else do putChar c; getUntilX h
