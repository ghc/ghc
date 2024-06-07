-- |
-- Module      : UCD2Haskell.ModuleGenerators
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) 2016-2017 Harendra Kumar
--               (c) 2014-2015 Antonio Nikishaev
--               (c) 2022-2024 Pierre Le Marre
-- License     : BSD-3-Clause
-- Maintainer  : The GHC Developers <ghc-devs@haskell.org>"
-- Stability   : internal

-- Code history:
--
-- This code was adapted from https://github.com/composewell/unicode-data/
-- (around commit c4aa52ed932ad8badf97296858932c3389b275b8) by Pierre Le Marre.
-- The original Unicode database parser was taken from
-- https://github.com/composewell/unicode-transforms but was completely
-- rewritten from scratch to parse from UCD text files instead of XML, only
-- some types remain the same. That code in turn was originally taken from
-- https://github.com/llelf/prose (Antonio Nikishaev) and heavily modified by
-- Harendra Kumar.
--
module UCD2Haskell.ModuleGenerators (genModules) where

import Control.Exception (catch, IOException)
import Data.Bits (Bits(..))
import Data.Word (Word8)
import Data.Char (ord)
import Data.Functor ((<&>), ($>))
import Data.List (intersperse, unfoldr)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>))
import Data.String (IsString)
import Data.Foldable (Foldable(..))

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS

import qualified Unicode.CharacterDatabase.Parser.Common as C
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD
import qualified Unicode.CharacterDatabase.Parser.Properties.Multiple as P

import Prelude hiding (pred)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

unlinesBB :: [BB.Builder] -> BB.Builder
unlinesBB = (<> "\n") . mconcat . intersperse "\n"

unwordsBB :: [BB.Builder] -> BB.Builder
unwordsBB = mconcat . intersperse " "

headerRule :: BB.Builder
headerRule = "-----------------------------------------------------------------------------"

mkModuleHeader :: BB.Builder -> BB.Builder
mkModuleHeader modName =
    unlinesBB
        [ headerRule
        , "-- |"
        , "-- Module      : " <> modName
        , "-- License     : BSD-3-Clause"
        , "-- Maintainer  : The GHC Developers <ghc-devs@haskell.org>"
        , "-- Stability   : internal"
        , headerRule
        ]

genSignature :: BB.Builder -> BB.Builder
genSignature = (<> " :: Char -> Bool")

-- | Check that var is between minimum and maximum of orderList
genRangeCheck :: BB.Builder -> [Int] -> BB.Builder
genRangeCheck var ordList =
    var
        <> " >= "
        <> BB.intDec (minimum ordList)
        <> " && " <> var <> " <= " <> BB.intDec (maximum ordList)

genBitmap :: BB.Builder -> [Int] -> BB.Builder
genBitmap funcName ordList =
    unlinesBB
        [ "{-# INLINE " <> funcName <> " #-}"
        , genSignature funcName
        , funcName <> " = \\c -> let n = ord c in "
              <> genRangeCheck "n" ordList <> " && lookupBit64 bitmap# n"
        , "  where"
        , "    bitmap# = \"" <> bitMapToAddrLiteral (positionsToBitMap ordList) "\"#"
        ]

positionsToBitMap :: [Int] -> [Bool]
positionsToBitMap = go 0

    where

    go _ [] = []
    go i xxs@(x:xs)
        | i < x = False : go (i + 1) xxs
        | otherwise = True : go (i + 1) xs

bitMapToAddrLiteral ::
  -- | Values to encode
  [Bool] ->
  -- | String to append
  BB.Builder ->
  BB.Builder
bitMapToAddrLiteral bs cs = foldr encode cs (unfoldr mkChunks bs)

    where

    mkChunks :: [a] -> Maybe ([a], [a])
    mkChunks [] = Nothing
    mkChunks xs = Just $ splitAt 8 xs

    encode :: [Bool] -> BB.Builder -> BB.Builder
    encode chunk acc = BB.char7 '\\' <> BB.intDec (toByte (padTo8 chunk)) <> acc

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
        | length xs >= 8 = xs
        | otherwise = xs <> replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]

genEnumBitmap ::
  forall a. (Bounded a, Enum a, Show a) =>
  -- | Function name
  BB.Builder ->
  -- | Default value
  a ->
  -- | List of values to encode
  [a] ->
  BB.Builder
genEnumBitmap funcName def as = unlinesBB
    [ "{-# INLINE " <> funcName <> " #-}"
    , funcName <> " :: Char -> Int"
    , funcName <> " c = let n = ord c in if n >= "
               <> BB.intDec (length as)
               <> " then "
               <> BB.intDec (fromEnum def)
               <> " else lookup_bitmap n"

    , "{-# NOINLINE lookup_bitmap #-}"
    , "lookup_bitmap :: Int -> Int"
    , "lookup_bitmap n = lookupIntN bitmap# n"
    , "  where"
    , "    bitmap# = \"" <> enumMapToAddrLiteral as "\"#"
    ]

{-| Encode a list of values as a byte map, using their 'Enum' instance.

__Note:__ 'Enum' instance must respect the following:

* @fromEnum minBound >= 0x00@
* @fromEnum maxBound <= 0xff@
-}
enumMapToAddrLiteral ::
  forall a. (Bounded a, Enum a, Show a) =>
  -- | Values to encode
  [a] ->
  -- | String to append
  BB.Builder ->
  BB.Builder
enumMapToAddrLiteral xs cs = foldr go cs xs

    where

    go :: a -> BB.Builder -> BB.Builder
    go x acc = BB.char7 '\\' <> BB.word8Dec (toWord8 x) <> acc

    toWord8 :: a -> Word8
    toWord8 a = let w = fromEnum a in if 0 <= w && w <= 0xff
        then fromIntegral w
        else error $ "Cannot convert to Word8: " <> show a

genUnicodeVersion :: FilePath -> IO ()
genUnicodeVersion outdir = do
  version <- catch
              (getEnv "UNICODE_VERSION")
              (\(_ :: IOException) -> return "<unknown>")
  runFold f [body version]
  where
    moduleName :: (IsString a) => a
    moduleName = "GHC.Internal.Unicode.Version"
    f = moduleFileEmitter Nothing outdir
          (moduleName, \_ -> Fold (\_ x -> x) mempty id)
    body :: String -> BB.Builder
    body version = unlinesBB
      [ "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# OPTIONS_HADDOCK hide #-}"
      , ""
      , mkModuleHeader moduleName
      , "module " <> moduleName
      , "(unicodeVersion)"
      , "where"
      , ""
      , "import {-# SOURCE #-} GHC.Internal.Data.Version"
      , ""
      , "-- | Version of Unicode standard used by @base@:"
      , "-- [" <> BB.string7 version <> "](https://www.unicode.org/versions/Unicode" <> BB.string7 version <> "/)."
      , "--"
      , "-- @since base-4.15.0.0"
      , "unicodeVersion :: Version"
      , "unicodeVersion = makeVersion [" <> mkVersion version <> "]" ]
    mkVersion = foldMap (\c -> case c of {'.' -> BB.char7 ',' <> BB.char7 ' '; _ -> BB.char7 c})

--------------------------------------------------------------------------------
-- Fold
--------------------------------------------------------------------------------

data Fold a b = forall s. Fold
  { _step :: s -> a -> s
  , _initial :: s
  , _final :: s -> b }

data Pair a b = Pair !a !b

teeWith :: (a -> b -> c) -> Fold x a -> Fold x b -> Fold x c
teeWith f (Fold stepL initialL finalL) (Fold stepR initialR finalR) =
  Fold step initial final
  where
    step (Pair sL sR) x = Pair (stepL sL x) (stepR sR x)
    initial = Pair initialL initialR
    final (Pair sL sR) = f (finalL sL) (finalR sR)

distribute :: [Fold a b] -> Fold a [b]
distribute = foldr (teeWith (:)) (Fold const () (const []))

rmapFold :: (b -> c) -> Fold a b -> Fold a c
rmapFold f (Fold step initial final) = Fold step initial (f . final)

runFold :: Fold a b -> [a] -> b
runFold (Fold step initial final) = final . foldl' step initial

--------------------------------------------------------------------------------
-- Modules generators
--------------------------------------------------------------------------------

data GeneralCategoryAcc = GeneralCategoryAcc
  { _categories :: ![UD.GeneralCategory]
  , _expectedChar :: !Char
  }

genGeneralCategoryModule :: BB.Builder -> Fold UD.Entry BB.Builder
genGeneralCategoryModule moduleName = Fold step initial done

    where

    -- (categories, expected char)
    initial = GeneralCategoryAcc [] '\0'

    step (GeneralCategoryAcc acc p) e@(UD.Entry r d)
      | p < r.start
      -- Fill missing char entry with default category Cn
      -- See: https://www.unicode.org/reports/tr44/#Default_Values_Table
      = step (GeneralCategoryAcc (replicate (ord r.start - ord p) UD.Cn <> acc) r.start) e
      -- Regular entry
      | otherwise = case r of
        C.SingleChar ch -> GeneralCategoryAcc
          (d.generalCategory : acc)
          (succ ch)
        C.CharRange ch1 ch2 -> GeneralCategoryAcc
          (replicate (ord ch2 - ord ch1 + 1) d.generalCategory <> acc)
          (succ ch2)

    done (GeneralCategoryAcc acc _) = unlinesBB
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , mkModuleHeader moduleName
        , "module " <> moduleName
        , "(generalCategory)"
        , "where"
        , ""
        , "import GHC.Internal.Base (Char, Int, Ord(..), ord)"
        , "import GHC.Internal.Unicode.Bits (lookupIntN)"
        , ""
        , genEnumBitmap "generalCategory" UD.Cn (reverse acc)
        ]

genSimpleCaseMappingModule
    :: BB.Builder
    -> BB.Builder
    -> (UD.CharDetails -> Maybe Char)
    -> Fold UD.Entry BB.Builder
genSimpleCaseMappingModule moduleName funcName field =
    Fold step initial done

    where

    genHeader =
        [ "{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , mkModuleHeader moduleName
        , "module " <> moduleName
        , "(" <> funcName <> ")"
        , "where"
        , ""
        , "import GHC.Internal.Base (Char)"
        , ""
        ]
    genSign =
        [ "{-# NOINLINE " <> funcName <> " #-}"
        , funcName <> " :: Char -> Char"
        , funcName <> " = \\case"
        ]
    initial = []

    step ds dc = case mkEntry dc of
        Nothing -> ds
        Just d  -> d : ds

    after = ["  c -> c"]

    done st =
        let body = mconcat [genHeader, genSign, reverse st, after]
        in unlinesBB body

    mkEntry (UD.Entry r dc) = case r of
      C.SingleChar ch -> field dc <&> \c -> mconcat
        [ "  '\\x"
        , showHexChar ch
        , "' -> '\\x"
        , showHexChar c
        , "'"
        ]
      C.CharRange{} -> field dc $> error ("genSimpleCaseMappingModule: unexpected char range: " <> show r)

    showHexChar c = BB.wordHex (fromIntegral (ord c))

data PropertiesAcc = PropertiesAcc
  { _properties :: ![BS.ShortByteString]
  , _bitmaps :: ![BB.Builder]
  , _currentBitmap :: ![[Int]] }

genCorePropertiesModule ::
       BB.Builder -> (BS.ShortByteString -> Bool) -> Fold P.Entry BB.Builder
genCorePropertiesModule moduleName isProp = Fold step initial done
    where
    prop2FuncName x = "is" <> BB.shortByteString x

    initial = PropertiesAcc [] [] []

    step acc@(PropertiesAcc props bitmaps bits) P.Entry{..}
      | not (isProp property) = acc -- property filtered out
      | otherwise = case props of
        prop' : _
          | prop' == property -> PropertiesAcc props bitmaps (rangeToBits range : bits)
          | otherwise -> PropertiesAcc
              { _properties = property : props
              , _bitmaps = genBitmap' prop' bits : bitmaps
              , _currentBitmap = [rangeToBits range] }
        _ -> PropertiesAcc [property] bitmaps [rangeToBits range]

    rangeToBits = \case
      C.SingleChar ch -> [ord ch]
      C.CharRange ch1 ch2 -> [ord ch1 .. ord ch2]

    genBitmap' prop bits = genBitmap (prop2FuncName prop) (mconcat (reverse bits))

    done (PropertiesAcc props bitmaps bits) = unlinesBB (header props <> bitmaps')
      where
        lastProp = case props of
          prop : _ -> prop
          [] -> error "impossible"
        bitmaps' = genBitmap' lastProp bits : bitmaps

    header exports =
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , mkModuleHeader moduleName
        , "module " <> moduleName
        , "(" <> unwordsBB (intersperse "," (map prop2FuncName exports)) <> ")"
        , "where"
        , ""
        , "import GHC.Internal.Base (Bool, Char, Ord(..), (&&), ord)"
        , "import GHC.Internal.Unicode.Bits (lookupBit64)"
        , ""
        ]

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

moduleToFileName :: String -> String
moduleToFileName = map (\x -> if x == '.' then '/' else x)

dirFromFileName :: String -> String
dirFromFileName = reverse . dropWhile (/= '/') . reverse

data FileRecipe a
    = ModuleRecipe
      -- ^ A recipe to create a Haskell module file.
        String
        -- ^ Module name
        (BB.Builder -> Fold a BB.Builder)
        -- ^ Function that generate the module, given the module name.
    | TestOutputRecipe
      -- ^ A recipe to create a test output file.
        String
        -- ^ Test name
        (Fold a BB.Builder)
        -- ^ Test output generator

-- ModuleRecipe is a tuple of the module name and a function that generates the
-- module using the module name
type ModuleRecipe a = (String, BB.Builder -> Fold a BB.Builder)
type TestOutputRecipe a = (FilePath, Fold a BB.Builder)

-- GeneratorRecipe is a list of ModuleRecipe
type GeneratorRecipe a = [FileRecipe a]

moduleFileEmitter :: Maybe FilePath -> FilePath -> ModuleRecipe a -> Fold a (IO ())
moduleFileEmitter mfile outdir (modName, fldGen) = rmapFold action $ fldGen (BB.string7 modName)

    where

    pretext version = case mfile of
      Just file -> mconcat
        [ "-- DO NOT EDIT: This file is automatically generated by the internal tool ucd2haskell,\n"
        , "-- with data from: https://www.unicode.org/Public/"
        , BB.string7 version
        , "/ucd/"
        , BB.string7 file
        ,".\n\n"
        ]
      Nothing -> "-- DO NOT EDIT: This file is automatically generated by the internal tool ucd2haskell.\n\n"
    outfile = outdir </> moduleToFileName modName <.> ".hs"
    outfiledir = dirFromFileName outfile
    action c = do
        version <-
            catch
                (getEnv "UNICODE_VERSION")
                (\(_ :: IOException) -> return "<unknown>")
        createDirectoryIfMissing True outfiledir
        B.writeFile outfile (BL.toStrict (BB.toLazyByteString (pretext version <> c)))

testOutputFileEmitter :: FilePath -> TestOutputRecipe a -> Fold a (IO ())
testOutputFileEmitter outdir (name, fldGen) = rmapFold action fldGen

    where

    outfile = outdir </> "tests" </> name <.> ".stdout"
    outfiledir = dirFromFileName outfile
    action c
        = createDirectoryIfMissing True outfiledir
        *> B.writeFile outfile (BL.toStrict (BB.toLazyByteString c))

runGenerator ::
       FilePath
    -> FilePath
    -> (B.ByteString -> [a])
    -> FilePath
    -> GeneratorRecipe a
    -> IO ()
runGenerator indir file transformLines outdir recipes = do
    raw <- B.readFile (indir <> file)
    sequence_ (runFold combinedFld (transformLines raw))

    where

    generatedFolds = recipes <&> \case
      ModuleRecipe     name f -> moduleFileEmitter (Just file) outdir (name, f)
      TestOutputRecipe name f -> testOutputFileEmitter         outdir (name, f)
    combinedFld = distribute generatedFolds

genModules :: FilePath -> FilePath -> [BS.ShortByteString] -> IO ()
genModules indir outdir props = do
    genUnicodeVersion outdir

    runGenerator
        indir
        "UnicodeData.txt"
        UD.parse
        outdir
        [ generalCategory
        , simpleUpperCaseMapping
        , simpleLowerCaseMapping
        , simpleTitleCaseMapping
        ]

    runGenerator
        indir
        "DerivedCoreProperties.txt"
        P.parse
        outdir
        [ derivedCoreProperties ]

    where

    derivedCoreProperties = ModuleRecipe
        "GHC.Internal.Unicode.Char.DerivedCoreProperties"
        (`genCorePropertiesModule` (`elem` props))

    generalCategory = ModuleRecipe
         "GHC.Internal.Unicode.Char.UnicodeData.GeneralCategory"
         genGeneralCategoryModule

    simpleUpperCaseMapping = ModuleRecipe
         "GHC.Internal.Unicode.Char.UnicodeData.SimpleUpperCaseMapping"
         (\m -> genSimpleCaseMappingModule m "toSimpleUpperCase" UD.simpleUpperCaseMapping)

    simpleLowerCaseMapping = ModuleRecipe
         "GHC.Internal.Unicode.Char.UnicodeData.SimpleLowerCaseMapping"
         (\m -> genSimpleCaseMappingModule m "toSimpleLowerCase" UD.simpleLowerCaseMapping)

    simpleTitleCaseMapping = ModuleRecipe
         "GHC.Internal.Unicode.Char.UnicodeData.SimpleTitleCaseMapping"
         (\m -> genSimpleCaseMappingModule m "toSimpleTitleCase" UD.simpleTitleCaseMapping)
