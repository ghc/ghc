import Control.Monad
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Marshal.Array
import GHC.Foreign (peekCStringLen, withCStringLen)
import GHC.IO.Encoding.Failure (CodingFailureMode(..))
import qualified GHC.IO.Encoding.Latin1 as Latin1
import System.IO
import System.IO.Error

-- Tests for single-byte encodings that map directly to Unicode
-- (module GHC.IO.Encoding.Latin1)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

decode :: TextEncoding -> [Word8] -> IO (Maybe String)
decode enc xs = fmap eitherToMaybe . tryIOError $ withArrayLen xs (\sz p -> peekCStringLen enc (castPtr p, sz))

encode :: TextEncoding -> String -> IO (Maybe [Word8])
encode enc cs = fmap eitherToMaybe . tryIOError $ withCStringLen enc cs (\(p, sz) -> peekArray sz (castPtr p))

testIO :: (Eq a, Show a) => IO a -> a -> IO ()
testIO action expected = do
  result <- action
  when (result /= expected) $
    putStrLn $ "Test failed: expected " ++ show expected ++ ", but got " ++ show result

-- Test char8-like encodings
test_char8 :: TextEncoding -> IO ()
test_char8 enc = do
  testIO (decode enc [0..0xff]) $ Just ['\0'..'\xff']

  testIO (encode enc ['\0'..'\x200']) $ Just ([0..0xff] ++ [0..0xff] ++ [0])

-- Test latin1-like encodings
test_latin1 :: CodingFailureMode -> TextEncoding -> IO ()
test_latin1 cfm enc = do
  testIO (decode enc [0..0xff]) $ Just ['\0'..'\xff']

  testIO (encode enc ['\0'..'\xff']) $ Just [0..0xff]
  testIO (encode enc "\xfe\xff\x100\x101\x100\xff\xfe") $ case cfm of
    ErrorOnCodingFailure -> Nothing
    IgnoreCodingFailure -> Just [0xfe,0xff,0xff,0xfe]
    TransliterateCodingFailure -> Just [0xfe,0xff,0x3f,0x3f,0x3f,0xff,0xfe]
    -- N.B. The argument "LATIN1//TRANSLIT" to mkTextEncoding does not
    -- correspond to "LATIN1//TRANSLIT" in iconv! Instead GHC asks iconv
    -- to encode to "LATIN1" and uses its own "evil hack" to insert '?'
    -- (ASCII 0x3f) in place of failures. See GHC.IO.Encoding.recoverEncode.
    --
    -- U+0100 is LATIN CAPITAL LETTER A WITH MACRON, which iconv would
    -- transliterate to 'A' (ASCII 0x41). Similarly iconv would
    -- transliterate U+0101 LATIN SMALL LETTER A WITH MACRON to 'a'
    -- (ASCII 0x61).
    RoundtripFailure -> Nothing

test_ascii :: CodingFailureMode -> TextEncoding -> IO ()
test_ascii cfm enc = do
  testIO (decode enc [0..0x7f]) $ Just ['\0'..'\x7f']
  testIO (decode enc [0x7e,0x7f,0x80,0x81,0x80,0x7f,0x7e]) $ case cfm of
    ErrorOnCodingFailure -> Nothing
    IgnoreCodingFailure -> Just "\x7e\x7f\x7f\x7e"
    TransliterateCodingFailure -> Just "\x7e\x7f\xfffd\xfffd\xfffd\x7f\x7e"
    -- Another GHC special: decode invalid input to the Char U+FFFD
    -- REPLACEMENT CHARACTER.
    RoundtripFailure -> Just "\x7e\x7f\xdc80\xdc81\xdc80\x7f\x7e"
    -- GHC's PEP383-style String-encoding of invalid input,
    -- see Note [Roundtripping]

  testIO (encode enc ['\0'..'\x7f']) $ Just [0..0x7f]
  testIO (encode enc "\x7e\x7f\x80\x81\x80\x7f\xe9") $ case cfm of
    ErrorOnCodingFailure -> Nothing
    IgnoreCodingFailure -> Just [0x7e,0x7f,0x7f]
    TransliterateCodingFailure -> Just [0x7e,0x7f,0x3f,0x3f,0x3f,0x7f,0x3f]
    -- See comment in test_latin1. iconv -t ASCII//TRANSLIT would encode
    -- U+00E9 LATIN SMALL LETTER E WITH ACUTE as 'e' (ASCII 0x65).
    RoundtripFailure -> Nothing

  -- Test roundtripping for good measure
  case cfm of
    RoundtripFailure -> do
      Just s <- decode enc [0..0xff]
      testIO (encode enc s) $ Just [0..0xff]
    _ -> return ()

main = do
  putStrLn "char8 tests"
  test_char8 char8              -- char8 never fails in either direction

  -- These use GHC's own implementation
  putStrLn "Latin1.ascii tests"
  test_ascii ErrorOnCodingFailure (Latin1.ascii)
  test_ascii IgnoreCodingFailure (Latin1.mkAscii IgnoreCodingFailure)
  test_ascii TransliterateCodingFailure (Latin1.mkAscii TransliterateCodingFailure)
  test_ascii RoundtripFailure (Latin1.mkAscii RoundtripFailure)

  putStrLn "Latin1.latin1_checked tests"
  test_latin1 ErrorOnCodingFailure (Latin1.latin1_checked)
  test_latin1 IgnoreCodingFailure (Latin1.mkLatin1_checked IgnoreCodingFailure)
  test_latin1 TransliterateCodingFailure (Latin1.mkLatin1_checked TransliterateCodingFailure)
  test_latin1 RoundtripFailure (Latin1.mkLatin1_checked RoundtripFailure)

  -- These use iconv (normally, unless it is broken)
  putStrLn "mkTextEncoding ASCII tests"
  test_ascii ErrorOnCodingFailure =<< mkTextEncoding "ASCII"
  test_ascii IgnoreCodingFailure =<< mkTextEncoding "ASCII//IGNORE"
  test_ascii TransliterateCodingFailure =<< mkTextEncoding "ASCII//TRANSLIT"
  test_ascii RoundtripFailure =<< mkTextEncoding "ASCII//ROUNDTRIP"

  putStrLn "mkTextEncoding LATIN1 tests"
  test_latin1 ErrorOnCodingFailure =<< mkTextEncoding "LATIN1"
  test_latin1 IgnoreCodingFailure =<< mkTextEncoding "LATIN1//IGNORE"
  test_latin1 TransliterateCodingFailure =<< mkTextEncoding "LATIN1//TRANSLIT"
  test_latin1 RoundtripFailure =<< mkTextEncoding "LATIN1//ROUNDTRIP"
