{--
This is a script to generate the necessary tables to support Windows code page
encoding/decoding.

License: see libraries/base/LICENSE

The code page tables are available from :
http://www.unicode.org/Public/MAPPINGS/

To run this script, use e.g.
runghc MakeTable.hs <module-name> <output-file> <codepage-dir>/*.TXT

Currently, this script only supports single-byte encodings, since the lookup
tables required for the CJK double-byte codepages are too large to be
statically linked into every executable.  We plan to add support for them once
GHC is able to produce Windows DLLs.
--}

module Main where

import System.FilePath
import qualified Data.Map as Map
import System.IO
import Data.Maybe (mapMaybe)
import Data.List (intersperse)
import Data.Word
import Numeric
import Control.Monad.State
import System.Environment
import Control.Exception(evaluate)

main :: IO ()
main = do
    moduleName:outFile:files <- getArgs
    let badFiles = -- These fail with an error like
                   --     MakeTable: Enum.toEnum{Word8}: tag (33088) is outside of bounds (0,255)
                   -- I have no idea what's going on, so for now we just
                   -- skip them.
                   ["CPs/CP932.TXT",
                    "CPs/CP936.TXT",
                    "CPs/CP949.TXT",
                    "CPs/CP950.TXT"]
    let files' = filter (`notElem` badFiles) files
    sbes <- mapM readMapAndIx files'
    putStrLn "Writing output"
    withBinaryFile outFile WriteMode $ flip hPutStr
        $ unlines $ makeTableFile moduleName files' sbes
  where
    readMapAndIx f = do
        putStrLn ("Reading " ++ f)
        m <- readMap f
        return (codePageNum f, m)

-- filenames are assumed to be of the form "CP1250.TXT"
codePageNum :: FilePath -> Int
codePageNum = read . drop 2 . takeBaseName

readMap :: (Ord a, Enum a) => FilePath -> IO (Map.Map a Char)
readMap f  = withBinaryFile f ReadMode $ \h -> do
    contents <- hGetContents h
    let ms = Map.fromList $ mapMaybe parseLine $ lines contents
    evaluate $ Map.size ms
    return ms

parseLine :: Enum a => String -> Maybe (a,Char)
parseLine s = case words s of
    ('#':_):_           -> Nothing
    bs:"#DBCS":_        -> Just (readHex' bs, toEnum 0xDC00)
    bs:"#UNDEFINED":_   -> Just (readHex' bs, toEnum 0)
    bs:cs:('#':_):_     -> Just (readHex' bs, readCharHex cs)
    _                   -> Nothing

readHex' :: Enum a => String -> a
readHex' ('0':'x':s) = case readHex s of
    [(n,"")] -> toEnum n -- explicitly call toEnum to catch overflow errors.
    _ -> error $ "Can't read hex: " ++ show s
readHex' s = error $ "Can't read hex: " ++ show s

readCharHex :: String -> Char
readCharHex s = if c > fromEnum (maxBound :: Word16)
                    then error "Can't handle non-BMP character."
                    else toEnum c
    where c = readHex' s


-------------------------------------------
-- Writing out the main data values.

makeTableFile :: String -> [FilePath] -> [(Int,Map.Map Word8 Char)] -> [String]
makeTableFile moduleName files maps = concat
    [ languageDirectives, firstComment files, header,
        theImports, theTypes, blockSizeText, tablePart]
  where
    header = [ "module " ++ moduleName ++ " where"
             , ""
             ]
    tablePart = [ "codePageMap :: [(Word32, CodePageArrays)]"
                , "codePageMap = ["
                ] ++ (intersperse "\n    ," $ map mkTableEntry maps)
                ++ ["    ]"]
    mkTableEntry (i,m) = "    (" ++ show i ++ ", " ++ makeSBE m ++ "    )"
    blockSizeText = ["blockBitSize :: Int", "blockBitSize = " ++ show blockBitSize]


makeSBE :: Map.Map Word8 Char -> String
makeSBE m = unlines
                [ "SingleByteCP {"
                , "     decoderArray = " ++ mkConvArray es
                , "     , encoderArray = " ++ mkCompactArray (swapMap m)
                , "   }"
                ]
  where
    es = [Map.findWithDefault '\0' x m | x <- [minBound..maxBound]]

swapMap :: (Ord a, Ord b, Enum a, Enum b) => Map.Map a b -> Map.Map b a
swapMap = Map.insert (toEnum 0) (toEnum 0) . Map.fromList . map swap . Map.toList
  where
    swap (x,y) = (y,x)


mkConvArray :: Embed a => [a] -> String
mkConvArray xs = "ConvArray \"" ++ concatMap mkHex xs ++ "\"#"


-------------------------------------------
-- Compact arrays
--
-- The decoding map (from Word8 to Char) can be implemented with a simple array
-- of 256 Word16's.  Bytes which do not belong to the code page are mapped to
-- '\0'.
--
-- However, a naive table mapping Char to Word8 would require 2^16 Word8's.  We
-- can use much less space with the right data structure, since at most 256 of
-- those entries are nonzero.
--
-- We use "compact arrays", as described in "Unicode Demystified" by Richard
-- Gillam.
--
-- Fix a block size S which is a power of two.  We compress an array of N
-- entries (where N>>S) as follows.  First, split the array into blocks of size
-- S, then remove all repeate blocks to form the "value" array.  Then construct
-- a separate "index" array which maps the position of blocks in the old array
-- to a position in the value array.
--
-- For example, assume that S=32 we have six blocks ABABCA, each with 32
-- elements.
--
-- Then the compressed table consists of two arrays:
-- 1) An array "values", concatenating the unique blocks ABC
-- 2) An array "indices" which equals [0,1,0,1,2,0].
--
-- To look up '\100', first calculate divMod 100 32 = (3,4).  Since
-- indices[3]=1, we look at the second unique block B; thus the encoded byte is
-- B[4].
--
-- The upshot of this representation is that the lookup is very quick as it only
-- requires two array accesses plus some bit masking/shifting.

-- From testing, this is an optimal size.
blockBitSize :: Int
blockBitSize = 6

mkCompactArray :: (Embed a, Embed b) => Map.Map a b -> String
mkCompactArray m = unlines [
            ""
            , " CompactArray {"
            , "        encoderIndices = " ++ mkConvArray is'
            , "        , encoderValues = "
                    ++ mkConvArray (concat $ Map.elems vs)
            , "        , encoderMax = " ++ show (fst $ Map.findMax m)
            , "        }"
            ]
  where
    blockSize = 2 ^ blockBitSize
    (is,(vs,_)) = compress blockSize $ m
    is' = map (* blockSize) is

type CompressState b = (Map.Map Int [b], Map.Map [b] Int)
-- each entry in the list corresponds to a block of size n.
compress :: (Bounded a, Enum a, Ord a, Bounded b, Ord b) => Int -> Map.Map a b
        -> ([Int], CompressState b)
compress n ms = runState (mapM lookupOrAdd chunks) (Map.empty, Map.empty)
    where
        chunks = mkChunks $ map (\i -> Map.findWithDefault minBound i ms)
                    $ [minBound..fst (Map.findMax ms)]
        mkChunks [] = []
        mkChunks xs = take n xs : mkChunks (drop n xs)
        lookupOrAdd xs = do
            (m,rm) <- get
            case Map.lookup xs rm of
                Just i -> return i
                Nothing -> do
                    let i = if Map.null m
                                then 0
                                else 1 + fst (Map.findMax m)
                    put (Map.insert i xs m, Map.insert xs i rm)
                    return i

-------------------------------------------
-- Static parts of the generated module.

languageDirectives :: [String]
languageDirectives = ["{-# LANGUAGE MagicHash, NoImplicitPrelude #-}"]


firstComment :: [FilePath] -> [String]
firstComment files = map ("-- " ++) $
    [ "Do not edit this file directly!"
    , "It was generated by the MakeTable.hs script using the files below."
    , "To regenerate it, run \"make\" in ../../../../codepages/"
    , ""
    , "Files:"
    ] ++ map takeFileName files

theImports :: [String]
theImports = map ("import " ++ )
    ["GHC.Prim", "GHC.Base", "GHC.Word"]

theTypes :: [String]
theTypes = [ "data ConvArray a = ConvArray Addr#"
           , "data CompactArray a b = CompactArray {"
           , "    encoderMax :: !a,"
           , "    encoderIndices :: !(ConvArray Int),"
           , "    encoderValues :: !(ConvArray b)"
           , "  }"
           , ""
           , "data CodePageArrays = SingleByteCP {"
           , "    decoderArray :: !(ConvArray Char),"
           , "    encoderArray :: !(CompactArray Char Word8)"
           , "  }"
           , ""
           ]

-------------------------------------------
-- Embed class and associated functions

class (Ord a, Enum a, Bounded a, Show a) => Embed a where
    mkHex :: a -> String

instance Embed Word8 where
    mkHex = showHex'

instance Embed Word16 where
    mkHex = repDualByte

instance Embed Char where
    mkHex = repDualByte

-- this is used for the indices of the compressed array.
instance Embed Int where
    mkHex = repDualByte

showHex' :: Integral a => a -> String
showHex' s = "\\x" ++ showHex s ""

repDualByte :: Enum c => c -> String
repDualByte c
    | n >= 2^(16::Int) = error "value is too high!"
    -- NOTE : this assumes little-endian architecture.  But we're only using this on Windows,
    -- so it's probably OK.
    | otherwise = showHex' (n `mod` 256) ++ showHex' (n `div` 256)
  where
    n = fromEnum c


