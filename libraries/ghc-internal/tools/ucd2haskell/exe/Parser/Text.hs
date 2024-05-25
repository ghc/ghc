{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Parser.Text
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
--               (c) 2016-2017 Harendra Kumar
--               (c) 2014-2015 Antonio Nikishaev
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : internal

-- This code was taken from https://github.com/composewell/unicode-data.
-- The original Unicode database parser was taken from
-- https://github.com/composewell/unicode-transforms but was completely
-- rewritten from scratch to parse from UCD text files instead of XML, only
-- some types remain the same. That code in turn was originally taken from
-- https://github.com/llelf/prose (Antonio Nikishaev) and heavily modified by
-- Harendra Kumar.
--
module Parser.Text (genModules) where

import Control.Exception (catch, IOException)
import Control.Monad (void)
import Data.Bits (Bits(..))
import Data.Word (Word8)
import Data.Char (chr, ord, isSpace)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (intersperse, unfoldr)
import Data.List.Split (splitWhen)
import Numeric (showHex)
import Streamly.Data.Fold (Fold)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>))

-- import qualified Data.Set as Set
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified System.IO as Sys

import Prelude hiding (pred)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data GeneralCategory =
    Lu|Ll|Lt|             --LC
    Lm|Lo|                --L
    Mn|Mc|Me|             --M
    Nd|Nl|No|             --N
    Pc|Pd|Ps|Pe|Pi|Pf|Po| --P
    Sm|Sc|Sk|So|          --S
    Zs|Zl|Zp|             --Z
    Cc|Cf|Cs|Co|Cn        --C
    deriving (Show, Bounded, Enum, Read)

data DecompType =
       DTCanonical | DTCompat  | DTFont
     | DTNoBreak   | DTInitial | DTMedial   | DTFinal
     | DTIsolated  | DTCircle  | DTSuper    | DTSub
     | DTVertical  | DTWide    | DTNarrow
     | DTSmall     | DTSquare  | DTFraction
    deriving (Show, Eq)

data Decomp = DCSelf | DC [Char] deriving (Show, Eq)

-- data DType = Canonical | Kompat

data DetailedChar =
    DetailedChar
        { _char :: Char
        , _name :: String
        , _generalCategory :: GeneralCategory
        , _combiningClass :: Int
        , _decompositionType :: Maybe DecompType
        , _decomposition :: Decomp
        , _simpleUppercaseMapping :: Maybe Char
        , _simpleLowercaseMapping :: Maybe Char
        , _simpleTitlecaseMapping :: Maybe Char
        }
    deriving (Show)

{- [NOTE] Used by disabled generator

-- See: https://www.unicode.org/reports/tr44/#Default_Values_Table
mkDefaultDetailedChar :: Char -> DetailedChar
mkDefaultDetailedChar c = DetailedChar
  { _char = c
  , _name = mempty
  , _generalCategory = Cn
  , _combiningClass = 0
  , _decompositionType = Nothing
  , _decomposition = DCSelf
  , _simpleUppercaseMapping = Nothing
  , _simpleLowercaseMapping = Nothing
  , _simpleTitlecaseMapping = Nothing }
-}

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

headerRule :: String
headerRule = "-----------------------------------------------------------------------------"

mkModuleHeader :: String -> String
mkModuleHeader modName =
    unlines
        [ headerRule
        , "-- |"
        , "-- Module      : " <> modName
        , "-- Copyright   : (c) 2020 Composewell Technologies and Contributors"
        , "-- License     : BSD-3-Clause"
        -- [FIXME] Update maintainer
        , "-- Maintainer  : streamly@composewell.com"
        , "-- Stability   : internal"
        , headerRule
        ]

readCodePoint :: String -> Char
readCodePoint = chr . read . ("0x"<>)

readCodePointM :: String -> Maybe Char
readCodePointM "" = Nothing
readCodePointM u  = Just (readCodePoint u)

genSignature :: String -> String
genSignature = (<> " :: Char -> Bool")

-- | Check that var is between minimum and maximum of orderList
genRangeCheck :: String -> [Int] -> String
genRangeCheck var ordList =
    var
        <> " >= "
        <> show (minimum ordList)
        <> " && " <> var <> " <= " <> show (maximum ordList)

genBitmap :: String -> [Int] -> String
genBitmap funcName ordList =
    unlines
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
  String ->
  String
bitMapToAddrLiteral bs cs = foldr encode cs (unfoldr mkChunks bs)

    where

    mkChunks :: [a] -> Maybe ([a], [a])
    mkChunks [] = Nothing
    mkChunks xs = Just $ splitAt 8 xs

    encode :: [Bool] -> String -> String
    encode chunk acc = '\\' : shows (toByte (padTo8 chunk)) acc

    padTo8 :: [Bool] -> [Bool]
    padTo8 xs
        | length xs >= 8 = xs
        | otherwise = xs <> replicate (8 - length xs) False

    toByte :: [Bool] -> Int
    toByte xs = sum $ map (\i -> if xs !! i then 1 `shiftL` i else 0) [0..7]

genEnumBitmap ::
  forall a. (Bounded a, Enum a, Show a) =>
  -- | Function name
  String ->
  -- | Default value
  a ->
  -- | List of values to encode
  [a] ->
  String
genEnumBitmap funcName def as = unlines
    [ "{-# INLINE " <> funcName <> " #-}"
    , funcName <> " :: Char -> Int"
    , funcName <> " c = let n = ord c in if n >= "
               <> show (length as)
               <> " then "
               <> show (fromEnum def)
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
  String ->
  String
enumMapToAddrLiteral xs cs = foldr go cs xs

    where

    go :: a -> String -> String
    go x acc = '\\' : shows (toWord8 x) acc

    toWord8 :: a -> Word8
    toWord8 a = let w = fromEnum a in if 0 <= w && w <= 0xff
        then fromIntegral w
        else error $ "Cannot convert to Word8: " <> show a

{- [NOTE] Disabled generator (normalization)
-- This bit of code is duplicated but this duplication allows us to reduce 2
-- dependencies on the executable.

jamoLCount :: Int
jamoLCount = 19

jamoVCount :: Int
jamoVCount = 21

jamoTCount :: Int
jamoTCount = 28

hangulFirst :: Int
hangulFirst = 0xac00

hangulLast :: Int
hangulLast = hangulFirst + jamoLCount * jamoVCount * jamoTCount - 1

isHangul :: Char -> Bool
isHangul c = n >= hangulFirst && n <= hangulLast
    where n = ord c
-}

genUnicodeVersion :: FilePath -> IO ()
genUnicodeVersion outdir = do
  version <- catch
              (getEnv "UNICODE_VERSION")
              (\(_ :: IOException) -> return "<unknown>")
  Stream.fold f (Stream.fromList (body version))
  where
    moduleName = "GHC.Internal.Unicode.Version"
    f = moduleFileEmitter Nothing outdir
          (moduleName, \_ -> Fold.foldMap (<> "\n"))
    body :: String -> [String]
    body version =
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
      , "-- [" <> version <> "](https://www.unicode.org/versions/Unicode" <> version <> "/)."
      , "--"
      , "-- @since base-4.15.0.0"
      , "unicodeVersion :: Version"
      , "unicodeVersion = makeVersion [" <> mkVersion version <> "]" ]
    mkVersion = foldr (\c acc -> case c of {'.' -> ',':' ':acc; _ -> c:acc}) mempty

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Parsing UnicodeData.txt
-------------------------------------------------------------------------------

genGeneralCategoryModule
    :: Monad m
    => String
    -> Fold m DetailedChar String
genGeneralCategoryModule moduleName =
    done <$> Fold.foldl' step initial

    where

    -- (categories, expected char)
    initial = ([], '\0')

    step (acc, p) a = if p < _char a
        -- Fill missing char entry with default category Cn
        -- See: https://www.unicode.org/reports/tr44/#Default_Values_Table
        then step (Cn : acc, succ p) a
        -- Regular entry
        else (_generalCategory a : acc, succ (_char a))

    done (acc, _) = unlines
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
        , genEnumBitmap "generalCategory" Cn (reverse acc)
        ]

readDecomp :: String -> (Maybe DecompType, Decomp)
readDecomp s =
    if null wrds
    then (Nothing, DCSelf)
    else decmps wrds

    where

    decmps [] = error "Unreachable flow point"
    decmps y@(x:xs) =
        case dtmap x of
            DTCanonical -> (,) (Just DTCanonical) (readCP y)
            other -> (,) (Just other) (readCP xs)

    wrds = words s

    readCP ws = DC $ map readCodePoint ws

    dtmap "<compat>" = DTCompat
    dtmap "<circle>" = DTCircle
    dtmap "<final>" = DTFinal
    dtmap "<font>" = DTFont
    dtmap "<fraction>" = DTFraction
    dtmap "<initial>" = DTInitial
    dtmap "<isolated>" = DTIsolated
    dtmap "<medial>" = DTMedial
    dtmap "<narrow>" = DTNarrow
    dtmap "<noBreak>" = DTNoBreak
    dtmap "<small>" = DTSmall
    dtmap "<square>" = DTSquare
    dtmap "<sub>" = DTSub
    dtmap "<super>" = DTSuper
    dtmap "<vertical>" = DTVertical
    dtmap "<wide>" = DTWide
    dtmap _ = DTCanonical

{- [NOTE] Disabled generators

filterNonHangul :: Monad m => Fold m DetailedChar a -> Fold m DetailedChar a
filterNonHangul = Fold.filter (not . isHangul . _char)

filterDecomposableType ::
       Monad m => DType -> Fold m DetailedChar a -> Fold m DetailedChar a
filterDecomposableType dtype =
    Fold.filter ((/= DCSelf) . _decomposition)
        . Fold.filter (predicate . _decompositionType)

    where

    predicate =
        case dtype of
            Canonical -> (== Just DTCanonical)
            Kompat -> const True

genDecomposableModule ::
       Monad m => String -> DType -> Fold m DetailedChar String
genDecomposableModule moduleName dtype =
    filterNonHangul
        $ filterDecomposableType dtype $ done <$> Fold.foldl' step initial

    where

    initial = []

    step st a = ord (_char a) : st

    done st =
        unlines
            [ "{-# LANGUAGE NoImplicitPrelude #-}"
            , "{-# LANGUAGE MagicHash #-}"
            , "{-# OPTIONS_HADDOCK hide #-}"
            , ""
            , mkModuleHeader moduleName
            , "module " <> moduleName
            , "(isDecomposable)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import GHC.Internal.Unicode.Bits (lookupBit64)"
            , ""
            , genBitmap "isDecomposable" (reverse st)
            ]

genCombiningClassModule :: Monad m => String -> Fold m DetailedChar String
genCombiningClassModule moduleName =
    Fold.filter (\dc -> _combiningClass dc /= 0)
        $ done <$> Fold.foldl' step initial

    where

    initial = ([], [])

    step (st1, st2) a = (genCombiningClassDef a : st1, ord (_char a) : st2)

    done (st1, st2) =
        unlines
            [ "{-# LANGUAGE NoImplicitPrelude #-}"
            , "{-# LANGUAGE MagicHash #-}"
            , "{-# OPTIONS_HADDOCK hide #-}"
            , ""
            , mkModuleHeader moduleName
            , "module " <> moduleName
            , "(combiningClass, isCombining)"
            , "where"
            , ""
            , "import Data.Char (ord)"
            , "import GHC.Internal.Unicode.Bits (lookupBit64)"
            , ""
            , "combiningClass :: Char -> Int"
            , unlines (reverse st1)
            , "combiningClass _ = 0\n"
            , ""
            , genBitmap "isCombining" (reverse st2)
            ]

    genCombiningClassDef dc =
        "combiningClass "
            <> show (_char dc) <> " = " <> show (_combiningClass dc)

genDecomposeDefModule ::
       Monad m
    => String
    -> [String]
    -> [String]
    -> DType
    -> (Int -> Bool)
    -> Fold m DetailedChar String
genDecomposeDefModule moduleName before after dtype pred =
    Fold.filter (pred . ord . _char)
        $ filterNonHangul
        $ filterDecomposableType dtype $ done <$> Fold.foldl' step initial

    where

    decomposeChar c DCSelf = [c]
    decomposeChar _c (DC ds) = ds

    genHeader =
        [ "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
        , ""
        , mkModuleHeader moduleName
        , "module " <> moduleName
        , "(decompose)"
        , "where"
        , ""
        ]
    genSign =
        [ "-- Note: this is a partial function we do not expect to call"
        , "-- this if isDecomposable returns false."
        , "{-# NOINLINE decompose #-}"
        , "decompose :: Char -> [Char]"
        ]
    initial = []

    step st dc = genDecomposeDef dc : st

    done st =
        let body = mconcat [genHeader, before, genSign, reverse st, after]
        in unlines body

    genDecomposeDef dc =
        "decompose "
            <> show (_char dc)
            <> " = " <> show (decomposeChar (_char dc) (_decomposition dc))

genCompositionsModule ::
       Monad m
    => String
    -> [Int]
    -> [Int]
    -> Fold m DetailedChar String
genCompositionsModule moduleName compExclu non0CC =
    Fold.filter (not . flip elem compExclu . ord . _char)
        $ filterNonHangul
        $ Fold.filter (isDecompositionLen2 . _decomposition)
        $ filterDecomposableType Canonical $ done <$> Fold.foldl' step initial

    where

    isDecompositionLen2 DCSelf = False
    isDecompositionLen2 (DC ds) = length ds == 2

    genComposePairDef name dc =
        name
            <> " "
            <> show (head d01)
            <> " " <> show (d01 !! 1) <> " = Just " <> show (_char dc)

        where

        d01 = decompPair dc

    decompPair dc =
        case _decomposition dc of
            DCSelf -> error "toCompFormat: DCSelf"
            (DC ds) ->
                if length ds == 2
                then ds
                else error "toCompFormat: length /= 2"

    initial = ([], [], [])

    step (dec, sp, ss) dc = (dec1, sp1, ss1)

        where

        d01 = decompPair dc
        d1Ord = ord $ d01 !! 1
        dec1 = genComposePairDef "compose" dc : dec
        sp1 =
            if d1Ord `notElem` non0CC
            then genComposePairDef "composeStarters" dc : sp
            else sp
        ss1 =
            if d1Ord `notElem` non0CC
            then d1Ord : ss
            else ss

    header =
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , mkModuleHeader moduleName
        , "module " <> moduleName
        , "(compose, composeStarters, isSecondStarter)"
        , "where"
        , ""
        , "import GHC.Internal.Base (Char, ord)"
        , "import GHC.Internal.Unicode.Bits (lookupBit64)"
        , ""
        ]

    composePair decomps =
        [ "{-# NOINLINE compose #-}"
        , "compose :: Char -> Char -> Maybe Char"
        , unlines decomps
        , "compose _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    composeStarterPair starterPairs =
        [ "composeStarters :: Char -> Char -> Maybe Char"
        , unlines starterPairs
        , "composeStarters _ _ = " <> "Nothing" <> "\n"
        , ""
        ]

    isSecondStarter secondStarters =
        [genBitmap "isSecondStarter" secondStarters]

    done (dec, sp, ss) =
        unlines
            $ header
            <> composePair (reverse dec)
            <> composeStarterPair (reverse sp)
            <> isSecondStarter (Set.toList (Set.fromList ss))
-}
genSimpleCaseMappingModule
    :: Monad m
    => String
    -> String
    -> (DetailedChar -> Maybe Char)
    -> Fold m DetailedChar String
genSimpleCaseMappingModule moduleName funcName field =
    done <$> Fold.foldl' step initial

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
        in unlines body

    mkEntry dc = field dc <&> \c -> mconcat
        [ "  '\\x"
        , showHexChar (_char dc) "' -> '\\x"
        , showHexChar c "'"
        ]

    showHexChar c = showHex (ord c)

genCorePropertiesModule ::
       Monad m => String -> (String -> Bool) -> Fold m (String, [Int]) String
genCorePropertiesModule moduleName isProp =
    Fold.filter (\(name, _) -> isProp name) $ done <$> Fold.foldl' step initial

    where

    prop2FuncName x = "is" <> x

    initial = ([], [])

    step (props, bitmaps) (name, bits) =
        (name : props, genBitmap (prop2FuncName name) bits : bitmaps)

    done (props, bitmaps) = unlines $ header props <> bitmaps

    header exports =
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , mkModuleHeader moduleName
        , "module " <> moduleName
        , "(" <> unwords (intersperse "," (map prop2FuncName exports)) <> ")"
        , "where"
        , ""
        , "import GHC.Internal.Base (Bool, Char, Ord(..), (&&), ord)"
        , "import GHC.Internal.Unicode.Bits (lookupBit64)"
        , ""
        ]

{- [NOTE] Disabled generator
genUnicode002TestResults :: Monad m => Fold m DetailedChar String
genUnicode002TestResults = done <$> Fold.foldl' step initial

    where

    header = "Code C P S U L A D"
    -- (output, expected char)
    initial = ([], '\0')
    -- [TODO] Increase the number of tested char?
    -- maxChar = '\xF0000' -- First codepoint of the last private use areas.
    -- maxChar = '\xFFFF' -- Last codepoint of BMP.
    maxChar = chr 6553 -- Value in GHC 9.2.2 base test

    step (acc, c) dc = if c > maxChar
      then (acc, c)
      else if c < _char dc
        -- Fill missing char entry with default values
        -- See: https://www.unicode.org/reports/tr44/#Default_Values_Table
        then step (mkEntry (mkDefaultDetailedChar c) : acc, succ c) dc
        -- Regular entry
        else (mkEntry dc : acc, succ (_char dc))

    done (acc, _) = unlines (header : reverse acc)

    mkEntry dc = mconcat
        [ showCode (_char dc)
        -- [TODO] General category
        , showBool (isControl' dc)
        , showBool (isPrint'   dc)
        , showBool (isSpace'   dc)
        -- [TODO] isSeparator
        , showBool (isUpper'   dc)
        , showBool (isLower'   dc)
        , showBool (isAlpha'   dc)
        -- [TODO] isAlphaNum
        , showBool (isDigit'   dc)
        -- [TODO] isNumber
        -- [TODO] isMark
        -- [TODO] isPunctuation
        -- [TODO] isSymbol
        ]

    padding = length (show (ord maxChar))
    showCode c = take padding (shows (ord c) (repeat ' '))
    -- [TODO] use showHex
    -- showCode c =
    --   let code = showHex (ord c) mempty
    --   in replicate (padding - length code) '0' <> code
    showBool b = if b then " T" else " F"

    -- [NOTE] The following functions replicates Data.Char. Keep them up to date!

    isControl' dc = case _generalCategory dc of
      Cc -> True   -- Control
      _  -> False

    isPrint' dc = case _generalCategory dc of
      Zl -> False -- LineSeparator
      Zp -> False -- ParagraphSeparator
      Cc -> False -- Control
      Cf -> False -- Format
      Cs -> False -- Surrogate
      Co -> False -- PrivateUse
      Cn -> False -- NotAssigned
      _  -> True

    isSpace' dc = case _char dc of
      '\t' -> True
      '\n' -> True
      '\v' -> True
      '\f' -> True
      '\r' -> True
      _    -> case _generalCategory dc of
        Zs -> True -- Space
        _  -> False

    isUpper' dc = case _generalCategory dc of
      Lu -> True -- UppercaseLetter
      Lt -> True -- TitlecaseLetter
      _  -> False

    isLower' dc = case _generalCategory dc of
      Ll -> True -- LowercaseLetter
      _  -> False

    isAlpha' dc = case _generalCategory dc of
      Lu -> True -- UppercaseLetter
      Ll -> True -- LowercaseLetter
      Lt -> True -- TitlecaseLetter
      Lm -> True -- ModifierLetter
      Lo -> True -- OtherLetter
      _  -> False

    isDigit' dc = let c = _char dc
                  in (fromIntegral (ord c - ord '0') :: Word) <= 9
-}

-------------------------------------------------------------------------------
-- Parsing property files
-------------------------------------------------------------------------------

type PropertyLine = (String, [Int])

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

emptyPropertyLine :: PropertyLine
emptyPropertyLine = ("", [])

combinePropertyLines :: PropertyLine -> PropertyLine -> PropertyLine
combinePropertyLines t1@(n1, o1) t2@(n2, o2)
    | n1 == "" = t2
    | n2 == "" = t1
    | n1 == n2 = (n1, o1 <> o2)
    | otherwise = error $ "Cannot group " <> n1 <> " with " <> n2

parsePropertyLine :: String -> PropertyLine
parsePropertyLine ln
    | null ln = emptyPropertyLine
    | head ln == '#' = emptyPropertyLine
    | otherwise = parseLineJ ln

    where

    parseLineJ :: String -> (String, [Int])
    parseLineJ line =
        let (rangeLn, line1) = span (/= ';') line
            propLn = takeWhile (/= '#') (tail line1)
         in (trim propLn, parseRange (trim rangeLn))

    parseRange :: String -> [Int]
    parseRange rng =
        if '.' `elem` rng
        then let low = read $ "0x" <> takeWhile (/= '.') rng
                 high =
                     read $ "0x" <> reverse (takeWhile (/= '.') (reverse rng))
              in [low .. high]
        else [read $ "0x" <> rng]

isDivider :: String -> Bool
isDivider x = x == "# ================================================"

parsePropertyLines :: (Monad m) => Stream m String -> Stream m PropertyLine
parsePropertyLines =
    Stream.splitOn isDivider
        $ Fold.lmap parsePropertyLine
        $ Fold.foldl' combinePropertyLines emptyPropertyLine

-- | A range entry in @UnicodeData.txt@.
data UnicodeDataRange
    = SingleCode    !DetailedChar
    -- ^ Regular entry for one code point
    | FirstCode     !String !DetailedChar
    -- ^ A partial range for entry with a name as: @\<RANGE_IDENTIFIER, First\>@
    | CompleteRange !String !DetailedChar !DetailedChar
    -- ^ A complete range, requiring 2 continuous entries with respective names:
    --
    -- * @\<RANGE_IDENTIFIER, First\>@
    -- * @\<RANGE_IDENTIFIER, Last\>@

{-| Parse UnicodeData.txt lines

Parse ranges according to https://www.unicode.org/reports/tr44/#Code_Point_Ranges.

__Note:__ this does /not/ fill missing char entries,
i.e. entries with no explicit entry nor within a range.
-}
parseUnicodeDataLines :: forall m. (Monad m) => Stream m String -> Stream m DetailedChar
parseUnicodeDataLines
    = Stream.unfoldMany (Unfold.unfoldr unitToRange)
    . Stream.foldMany ( Fold.lmap parseDetailedChar
                      $ Fold.foldt' step initial id)

    where

    step :: Maybe UnicodeDataRange
         -> DetailedChar
         -> Fold.Step (Maybe UnicodeDataRange) (Maybe UnicodeDataRange)
    step Nothing dc = case span (/= ',') (_name dc) of
        (range, ", First>") -> Fold.Partial (Just (FirstCode range dc))
        _                   -> Fold.Done (Just (SingleCode dc))
    step (Just (FirstCode range1 dc1)) dc2 = case span (/= ',') (_name dc2) of
        (range2, ", Last>") -> if range1 == range2 && _char dc1 < _char dc2
            then Fold.Done (Just (CompleteRange range1 dc1 dc2))
            else error $ "Cannot create range: incompatible ranges" <> show (dc1, dc2)
        _ -> error $ "Cannot create range: missing <range, Last> entry correspong to: " <> show range1
    step _ _ = error "impossible case"

    initial :: Fold.Step (Maybe UnicodeDataRange) (Maybe UnicodeDataRange)
    initial = Fold.Partial Nothing

    unitToRange :: Maybe UnicodeDataRange -> Maybe (DetailedChar, Maybe UnicodeDataRange)
    unitToRange = fmap $ \case
        SingleCode          dc      -> (dc, Nothing)
        FirstCode     _     dc      -> error $ "Incomplete range: " <> show dc
        CompleteRange range dc1 dc2 -> if _char dc1 < _char dc2
            -- [TODO] Create the proper name
            then (dc1{_name="TODO"}, Just (CompleteRange range dc1{_char=succ (_char dc1)} dc2))
            else (dc2{_name="TODO"}, Nothing)

-- | Parse a single entry of @UnicodeData.txt@
parseDetailedChar :: String -> DetailedChar
parseDetailedChar line = case splitWhen (== ';') line of
  char
    :name
    :gc
    :combining
    :_bidi
    :decomposition
    :_decimal
    :_digit
    :_numeric
    :_bidiM
    :_uni1Name
    :_iso
    :sUpper
    :sLower
    :sTitle
    :_ ->
      let (dctype, dcval) = readDecomp decomposition
      in DetailedChar
          { _char = readCodePoint char
          , _name = name
          , _generalCategory = read gc
          , _combiningClass = read combining
          , _decompositionType = dctype
          , _decomposition = dcval
          , _simpleUppercaseMapping = readCodePointM sUpper
          , _simpleLowercaseMapping = readCodePointM sLower
          , _simpleTitlecaseMapping = readCodePointM sTitle
          }
  _ -> error ("Unsupported line: " <> line)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

readLinesFromFile :: String -> Stream IO String
readLinesFromFile file =
    withFile file Sys.ReadMode
        $ \h -> Handle.read h & Unicode.decodeUtf8 & Unicode.lines Fold.toList

    where
    withFile file_ mode =
        Stream.bracketIO (Sys.openFile file_ mode) (Sys.hClose)


moduleToFileName :: String -> String
moduleToFileName = map (\x -> if x == '.' then '/' else x)

dirFromFileName :: String -> String
dirFromFileName = reverse . dropWhile (/= '/') . reverse

data FileRecipe a
    = ModuleRecipe
      -- ^ A recipe to create a Haskell module file.
        String
        -- ^ Module name
        (String -> Fold IO a String)
        -- ^ Function that generate the module, given the module name.
    | TestOutputRecipe
      -- ^ A recipe to create a test output file.
        String
        -- ^ Test name
        (Fold IO a String)
        -- ^ Test output generator

-- ModuleRecipe is a tuple of the module name and a function that generates the
-- module using the module name
type ModuleRecipe a = (String, String -> Fold IO a String)
type TestOutputRecipe a = (FilePath, Fold IO a String)

-- GeneratorRecipe is a list of ModuleRecipe
type GeneratorRecipe a = [FileRecipe a]

moduleFileEmitter :: Maybe FilePath -> FilePath -> ModuleRecipe a -> Fold IO a ()
moduleFileEmitter mfile outdir (modName, fldGen) = Fold.rmapM action $ fldGen modName

    where

    pretext version = case mfile of
      Just file -> mconcat
        [ "-- DO NOT EDIT: This file is automatically generated by the internal tool ucd2haskell,\n"
        , "-- with data from: https://www.unicode.org/Public/"
        , version
        , "/ucd/"
        , file
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
        writeFile outfile (pretext version <> c)

testOutputFileEmitter :: FilePath -> TestOutputRecipe a -> Fold IO a ()
testOutputFileEmitter outdir (name, fldGen) = Fold.rmapM action fldGen

    where

    outfile = outdir </> "tests" </> name <.> ".stdout"
    outfiledir = dirFromFileName outfile
    action c
        = createDirectoryIfMissing True outfiledir
        *> writeFile outfile c

runGenerator ::
       FilePath
    -> FilePath
    -> (Stream IO String -> Stream IO a)
    -> FilePath
    -> GeneratorRecipe a
    -> IO ()
runGenerator indir file transformLines outdir recipes =
    readLinesFromFile (indir <> file) & transformLines & Stream.fold combinedFld

    where

    generatedFolds = recipes <&> \case
      ModuleRecipe     name f -> moduleFileEmitter (Just file) outdir (name, f)
      TestOutputRecipe name f -> testOutputFileEmitter         outdir (name, f)
    combinedFld = void $ Fold.distribute generatedFolds

genModules :: String -> String -> [String] -> IO ()
genModules indir outdir props = do
    genUnicodeVersion outdir

    -- [NOTE] Disabled generator
    -- compExclu <-
    --     readLinesFromFile (indir <> "DerivedNormalizationProps.txt")
    --         & parsePropertyLines
    --         & Stream.find (\(name, _) -> name == "Full_Composition_Exclusion")
    --         & fmap (snd . fromMaybe ("", []))

    -- [NOTE] Disabled generator
    -- non0CC <-
    --     readLinesFromFile (indir <> "extracted/DerivedCombiningClass.txt")
    --         & parsePropertyLines
    --         & Stream.filter (\(name, _) -> name /= "0")
    --         & Stream.map snd
    --         & Stream.fold (Fold.foldl' (<>) [])

    runGenerator
        indir
        "UnicodeData.txt"
        parseUnicodeDataLines
        outdir
        -- [NOTE] Disabled generators
        -- [ uncurry ModuleRecipe compositions compExclu non0CC
        -- , uncurry ModuleRecipe combiningClass
        -- , uncurry ModuleRecipe decomposable
        -- , uncurry ModuleRecipe decomposableK
        -- , uncurry ModuleRecipe decompositions
        -- , uncurry ModuleRecipe decompositionsK2
        -- , uncurry ModuleRecipe decompositionsK
        [ uncurry ModuleRecipe generalCategory
        , uncurry ModuleRecipe simpleUpperCaseMapping
        , uncurry ModuleRecipe simpleLowerCaseMapping
        , uncurry ModuleRecipe simpleTitleCaseMapping
        -- , uncurry TestOutputRecipe unicode002Test
        ]

    -- [NOTE] Disabled generator
    -- runGenerator
    --     indir
    --     "PropList.txt"
    --     parsePropertyLines
    --     outdir
    --     [ uncurry ModuleRecipe propList ]

    runGenerator
        indir
        "DerivedCoreProperties.txt"
        parsePropertyLines
        outdir
        [ uncurry ModuleRecipe derivedCoreProperties ]

    where

    -- [NOTE] Disabled generator
    -- propList =
    --     ("GHC.Internal.Unicode.Char.PropList"
    --     , (`genCorePropertiesModule` (`elem` props)))

    derivedCoreProperties =
        ("GHC.Internal.Unicode.Char.DerivedCoreProperties"
        , (`genCorePropertiesModule` (`elem` props)))

    -- [NOTE] Disabled generator
    -- compositions exc non0 =
    --     ( "GHC.Internal.Unicode.Char.UnicodeData.Compositions"
    --     , \m -> genCompositionsModule m exc non0)

    -- [NOTE] Disabled generator
    -- combiningClass =
    --     ( "GHC.Internal.Unicode.Char.UnicodeData.CombiningClass"
    --     , genCombiningClassModule)

    -- [NOTE] Disabled generator
    -- decomposable =
    --     ( "GHC.Internal.Unicode.Char.UnicodeData.Decomposable"
    --     , (`genDecomposableModule` Canonical))

    -- [NOTE] Disabled generator
    -- decomposableK =
    --     ( "GHC.Internal.Unicode.Char.UnicodeData.DecomposableK"
    --     , (`genDecomposableModule` Kompat))

    -- [NOTE] Disabled generator
    -- decompositions =
    --     ( "GHC.Internal.Unicode.Char.UnicodeData.Decompositions"
    --     , \m -> genDecomposeDefModule m [] [] Canonical (const True))

    -- [NOTE] Disabled generator
    -- decompositionsK2 =
    --     ( "GHC.Internal.Unicode.Char.UnicodeData.DecompositionsK2"
    --     , \m -> genDecomposeDefModule m [] [] Kompat (>= 60000))

    -- [NOTE] Disabled generator
    -- decompositionsK =
    --     let pre = ["import qualified " <> fst decompositionsK2 <> " as DK2", ""]
    --         post = ["decompose c = DK2.decompose c"]
    --      in ( "GHC.Internal.Unicode.Char.UnicodeData.DecompositionsK"
    --         , \m -> genDecomposeDefModule m pre post Kompat (< 60000))

    generalCategory =
         ( "GHC.Internal.Unicode.Char.UnicodeData.GeneralCategory"
         , genGeneralCategoryModule)

    simpleUpperCaseMapping =
         ( "GHC.Internal.Unicode.Char.UnicodeData.SimpleUpperCaseMapping"
         , \m -> genSimpleCaseMappingModule m "toSimpleUpperCase" _simpleUppercaseMapping)

    simpleLowerCaseMapping =
         ( "GHC.Internal.Unicode.Char.UnicodeData.SimpleLowerCaseMapping"
         , \m -> genSimpleCaseMappingModule m "toSimpleLowerCase" _simpleLowercaseMapping)

    simpleTitleCaseMapping =
         ( "GHC.Internal.Unicode.Char.UnicodeData.SimpleTitleCaseMapping"
         , \m -> genSimpleCaseMappingModule m "toSimpleTitleCase" _simpleTitlecaseMapping)

    -- unicode002Test =
    --      ( "unicode002"
    --      , genUnicode002TestResults)
