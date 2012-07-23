import Control.Monad

import System.IO
import Control.Exception

import Foreign.Marshal.Array
import Foreign.Ptr

import GHC.Foreign
import GHC.IO.Encoding (TextEncoding, mkTextEncoding)

import Data.Char
import Data.Word


decode :: TextEncoding -> [Word8] -> IO String
decode enc xs = withArrayLen xs (\sz p -> peekCStringLen enc (castPtr p, sz)) `catch` \e -> return (show (e :: IOException))

encode :: TextEncoding -> String -> IO [Word8]
encode enc cs = withCStringLen enc cs (\(p, sz) -> peekArray sz (castPtr p)) `catch` \e -> return (const [] (e :: IOException))

asc :: Char -> Word8
asc = fromIntegral . ord

families = [ ([asc 'H', asc 'i', 0xED, 0xB2, 0x80, asc '!'],
              ["UTF-8",    "UTF-8//IGNORE",    "UTF-8//TRANSLIT",    "UTF-8//ROUNDTRIP"])
           , ([asc 'H', 0, asc 'i', 0, 0xFF, 0xDF, 0xFF, 0xDF, asc '!', 0],
              ["UTF-16LE", "UTF-16LE//IGNORE", "UTF-16LE//TRANSLIT", "UTF-16LE//ROUNDTRIP"])
           , ([0, asc 'H', 0, asc 'i', 0xDF, 0xFF, 0xDF, 0xFF, 0, asc '!'],
              ["UTF-16BE", "UTF-16BE//IGNORE", "UTF-16BE//TRANSLIT", "UTF-16BE//ROUNDTRIP"])
           , ([asc 'H', 0, 0, 0, asc 'i', 0, 0, 0, 0xED, 0xB2, 0x80, 0, asc '!', 0, 0, 0],
              ["UTF-32LE", "UTF-32LE//IGNORE", "UTF-32LE//TRANSLIT", "UTF-32LE//ROUNDTRIP"])
           , ([0, 0, 0, asc 'H', 0, 0, 0, asc 'i', 0, 0x80, 0xB2, 0xED, 0, 0, 0, asc '!'],
              ["UTF-32BE", "UTF-32BE//IGNORE", "UTF-32BE//TRANSLIT", "UTF-32BE//ROUNDTRIP"])
           ]

main = do
  surrogate_enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
    
  -- Test that invalid input is correctly roundtripped as surrogates
  -- This only works for the UTF-8 UTF since it is the only UTF which
  -- is an ASCII superset.
  putStrLn $ "== UTF-8: roundtripping"
  let invalid_bytes = [asc 'H', asc 'i', 0xED, 0xB2, 0x80, asc '!']
  surrogates <- decode surrogate_enc invalid_bytes
  invalid_bytes' <- encode surrogate_enc surrogates
  print invalid_bytes
  print surrogates
  print invalid_bytes'
  print (invalid_bytes == invalid_bytes')
  putStrLn ""
  
  forM families $ \(invalid_bytes, enc_names) -> do
    encs <- mapM mkTextEncoding enc_names
    let name = head enc_names
    
    -- How we deal with decoding errors in the various modes:
    putStrLn $ "== " ++ name ++ ": decoding"
    forM encs $ \enc -> decode enc invalid_bytes >>= print
    
    -- How about encoding errors, particularly those from embedded surrogates?
    putStrLn $ "== " ++ name ++ ": encoding"
    forM encs $ \enc -> encode enc "Hi\xDC80!" >>= print
    
    putStrLn ""
