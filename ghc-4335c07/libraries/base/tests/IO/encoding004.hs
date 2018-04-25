import System.IO
import System.Directory
import Data.Char
import System.Process
import Control.Monad
import qualified Data.ByteString as BS
import System.Environment
import System.Exit
import System.FilePath
import Data.Maybe
import qualified Data.Map as M
import GHC.Foreign
import Control.Exception


decode :: TextEncoding -> BS.ByteString -> IO (Either SomeException String)
decode enc bs = try $ BS.useAsCStringLen bs $ peekCStringLen enc

encode :: TextEncoding -> String -> IO (Either SomeException BS.ByteString)
encode enc cs = try $ withCStringLen enc cs $ BS.packCStringLen

decodeEncode :: TextEncoding -> BS.ByteString -> IO (Either SomeException BS.ByteString)
decodeEncode enc bs = decode enc bs `bind` encode enc

encodedecode :: TextEncoding -> String -> IO (Either SomeException String)
encodedecode enc bs = encode enc bs `bind` decode enc

bind mx fxmy = do
    ei_e_cs <- mx
    case ei_e_cs of
        Left e   -> return (Left e)
        Right cs -> fxmy cs


main :: IO ()
main = forM_ [ ("CP936",  2, "CP936",      Just "CP936-UTF8")  -- Representative (roundtrippable) DBCS
             , ("CP1251", 1, "CP1251",     Just "CP1251-UTF8") -- Representative SBCS
             , ("UTF-8",  4, "CP936-UTF8", Nothing)            -- Sanity check
             ] $ \(enc_name, max_byte_length, file, mb_utf8_file) -> do
    putStrLn $ "== " ++ enc_name

    let fp = "encoded-data" </> file <.> "txt"
    enc <- mkTextEncoding enc_name
    bs <- BS.readFile fp

    -- In a DBCS you should never fail to encode truncated input for two consecutive truncation points,
    -- assuming that the input file is actually error free:
    testTruncations enc max_byte_length bs

    -- Should be able to roundtrip arbitrary rubbish, as long as we use the right encoding
    roundtrip_enc <- mkTextEncoding (enc_name ++ "//ROUNDTRIP")
    testRoundtripping roundtrip_enc bs

    -- Just check that we actually decode to the right thing, for good measure
    case mb_utf8_file of
      Nothing -> return ()
      Just utf8_file -> do
        utf8_bs <- BS.readFile ("encoded-data" </> utf8_file <.> "txt")
        Right expected <- decode utf8 utf8_bs
        Right actual   <- decode enc  bs
        unless (expected == actual) $ do
            putStrLn (bsDiff 0 actual expected)

forTruncations :: BS.ByteString -> (BS.ByteString -> IO a) -> IO [a]
forTruncations bs f = forSplits bs $ \before _ -> f before

forSplits :: BS.ByteString -> (BS.ByteString -> BS.ByteString -> IO a) -> IO [a]
forSplits bs f = forM [(800 * block) + ix | block <- [0..len `div` 800], ix <- [0..100]] $ \i -> uncurry f (BS.splitAt i bs)
    where len = BS.length bs

testTruncations :: TextEncoding -> Int -> BS.ByteString -> IO ()
testTruncations enc max_byte_length bs = do
    failures <- fmap catMaybes $ forTruncations bs $ testTruncation enc

    let failure_map = M.fromList failures
    forM_ failures $ \(i, e) -> do
        let js = [i+1..i+(max_byte_length - 1)]
        case sequence (map (`M.lookup` failure_map) js) of
            Nothing -> return ()
            Just es -> putStrLn ("Failed on consecutive truncated byte indexes " ++ show (i:js) ++ " (" ++ show (e:es) ++ ")")

testTruncation :: TextEncoding -> BS.ByteString -> IO (Maybe (Int, SomeException))
testTruncation enc expected = do
        --putStr (show i ++ ": ") >> hFlush stdout
        ei_e_actual <- decodeEncode enc expected
        case ei_e_actual of
            Left  e      -> return (Just (BS.length expected, e))
            Right actual | expected /= actual -> error $ "Mismatch on success when truncating at byte index " ++ show (BS.length expected)
                         | otherwise          -> return Nothing

testRoundtripping :: TextEncoding -> BS.ByteString -> IO ()
testRoundtripping roundtrip_enc bs = void $ forSplits bs $ \before after -> do
    let expected = before `BS.append` (fromIntegral (BS.length before `mod` 256) `BS.cons` after)
    Right actual <- decodeEncode roundtrip_enc expected
    when (actual /= expected) $ do
        let i_str = show (BS.length before)
        putStrLn $ "Failed to roundtrip given mutant byte at index " ++ i_str ++ " (" ++ bsDiff 0 (BS.unpack actual) (BS.unpack expected) ++ ")"
        -- Possibly useful for debugging porpoises:
        --BS.writeFile (i_str ++ ".expected") expected
        --BS.writeFile (i_str ++ ".actual")   actual

bsDiff :: (Show a, Eq a) => Int -> [a] -> [a] -> String
bsDiff _ [] [] = error "bsDiff"
bsDiff _ [] bs = "actual " ++ show (length bs) ++ " elements shorter than expected"
bsDiff _ as [] = "expected " ++ show (length as) ++ " elements shorter than actual"
bsDiff i (a:as) (b:bs) | a == b    = bsDiff (i + 1) as bs
                       | otherwise = show a ++ " /= " ++ show b ++ " at index " ++ show i
