import qualified Data.ByteString as BS
import System.IO
import GHC.Foreign
import Control.Exception
import Data.Word

decode :: TextEncoding -> BS.ByteString -> IO (Either SomeExceptionWithLocation String)
decode enc bs = try $ BS.useAsCStringLen bs $ peekCStringLen enc

main :: IO ()
main = mapM_ go [ ["01111111"] -- (just fits into 1 byte)
                , ["11000010", "10000000"] -- (just large enough for 2 bytes)
                , ["11000001", "10111111"] -- (overlong: only 7 bits, so should fit into 1 byte)
                , ["11011111", "10111111"] -- (just fits into 2 bytes)
                , ["11100000", "10100000", "10000000"] -- (just large enough for 3 bytes)
                , ["11100000", "10011111", "10111111"] -- (overlong: only 11 bits, so should fit into 2 bytes)
                , ["11101111", "10111111", "10111111"] -- (just fits into 3 bytes)
                , ["11110000", "10010000", "10000000", "10000000"] -- (just large enough for 4 bytes)
                , ["11110000", "10001111", "10111111", "10111111"] -- (overlong: only 16 bits, so should fit into 3 bytes)
                , ["11110100", "10001111", "10111111", "10111111"] -- (largest allowed codepoint)
                , ["11110111", "10111111", "10111111", "10111111"] -- (just fits into 4 bytes but disallowed by RFC3629)
                ]
  where go xs = decode utf8 (BS.pack (map toByte xs)) >>= either (\_ -> putStrLn "Error") print

toByte :: String -> Word8
toByte [] = 0
toByte ('1':xs) = (2 ^ length xs) + toByte xs
toByte ('0':xs) = toByte xs
