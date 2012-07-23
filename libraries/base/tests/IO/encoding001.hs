import Control.Monad
import System.IO
import GHC.IO.Encoding
import GHC.IO.Handle
import Data.Bits
import Data.Word
import Data.Char
import System.FilePath
import System.Exit

file = "encoding001"

encodings = [(utf8,     "utf8"),
             (utf8_bom, "utf8_bom"),
             (utf16,    "utf16"),
             (utf16le,  "utf16le"),
             (utf16be,  "utf16be"),
             (utf32,    "utf32"),
             (utf32le,  "utf32le"),
             (utf32be,  "utf32be")]

main = do
  -- make a UTF-32BE file
  h <- openBinaryFile (file <.> "utf32be") WriteMode
  let expand32 :: Word32 -> [Char]
      expand32 x = [
          chr (fromIntegral (x `shiftR` 24) .&. 0xff),
          chr (fromIntegral (x `shiftR` 16) .&. 0xff),
          chr (fromIntegral (x `shiftR` 8)  .&. 0xff),
          chr (fromIntegral x .&. 0xff) ]
  hPutStr h (concatMap expand32 [ 0, 32 .. 0xD7ff ])
  -- We avoid the private-use characters at 0xEF00..0xEFFF
  -- that reserved for GHC's PEP383 roundtripping implementation.
  --
  -- The reason is that currently normal text containing those
  -- characters will be mangled, even if we aren't using an encoding
  -- created using //ROUNDTRIP.
  hPutStr h (concatMap expand32 [ 0xE000, 0xE000+32 .. 0xEEFF ])
  hPutStr h (concatMap expand32 [ 0xF000, 0xF000+32 .. 0x10FFFF ])
  hClose h

  -- convert the UTF-32BE file into each other encoding
  forM_ encodings $ \(enc,name) -> do
     when (name /=  "utf32be") $ do
       hin <- openFile (file <.> "utf32be") ReadMode
       hSetEncoding hin utf32be
       hout <- openFile (file <.> name) WriteMode
       hSetEncoding hout enc
       hGetContents hin >>= hPutStr hout
       hClose hin
       hClose hout

  forM_ [ (from,to) | from <- encodings, to <- encodings, snd from /= snd to ]
      $ \((fromenc,fromname),(toenc,toname)) -> do
     hin <- openFile (file <.> fromname) ReadMode
     hSetEncoding hin fromenc
     hout <- openFile (file <.> toname <.> fromname) WriteMode
     hSetEncoding hout toenc
     hGetContents hin >>= hPutStr hout
     hClose hin
     hClose hout

     h1 <- openBinaryFile (file <.> toname) ReadMode
     h2 <- openBinaryFile (file <.> toname <.> fromname) ReadMode
     str1 <- hGetContents h1
     str2 <- hGetContents h2
     when (str1 /= str2) $ do
       putStrLn (file <.> toname ++ " and " ++ file <.> toname <.> fromname ++ " differ")
       exitWith (ExitFailure 1)
     hClose h1
     hClose h2
