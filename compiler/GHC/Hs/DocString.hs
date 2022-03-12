-- | An exactprintable structure for docstrings
{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Hs.DocString
  ( LHsDocString
  , HsDocString(..)
  , HsDocStringDecorator(..)
  , HsDocStringChunk(..)
  , LHsDocStringChunk
  , isEmptyDocString
  , unpackHDSC
  , mkHsDocStringChunk
  , mkHsDocStringChunkUtf8ByteString
  , pprHsDocString
  , pprHsDocStrings
  , mkGeneratedHsDocString
  , docStringChunks
  , renderHsDocString
  , renderHsDocStrings
  , exactPrintHsDocString
  , pprWithDocString
  ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Encoding
import GHC.Utils.Outputable as Outputable hiding ((<>))
import GHC.Types.SrcLoc

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)

type LHsDocString = Located HsDocString

-- | Haskell Documentation String
--
-- Rich structure to support exact printing
-- The location around each chunk doesn't include the decorators
data HsDocString
  = MultiLineDocString !HsDocStringDecorator !(NonEmpty LHsDocStringChunk)
     -- ^ The first chunk is preceded by "-- <decorator>" and each following chunk is preceded by "--"
     -- Example: -- | This is a docstring for 'foo'. It is the line with the decorator '|' and is always included
     --          -- This continues that docstring and is the second element in the NonEmpty list
     --          foo :: a -> a
  | NestedDocString !HsDocStringDecorator LHsDocStringChunk
     -- ^ The docstring is preceded by "{-<decorator>" and followed by "-}"
     -- The chunk contains balanced pairs of '{-' and '-}'
  | GeneratedDocString HsDocStringChunk
     -- ^ A docstring generated either internally or via TH
     -- Pretty printed with the '-- |' decorator
     -- This is because it may contain unbalanced pairs of '{-' and '-}' and
     -- not form a valid 'NestedDocString'
  deriving (Eq, Data, Show)

instance Outputable HsDocString where
  ppr = text . renderHsDocString

-- | Annotate a pretty printed thing with its doc
-- The docstring comes after if is 'HsDocStringPrevious'
-- Otherwise it comes before.
-- Note - we convert MultiLineDocString HsDocStringPrevious to HsDocStringNext
-- because we can't control if something else will be pretty printed on the same line
pprWithDocString :: HsDocString -> SDoc -> SDoc
pprWithDocString  (MultiLineDocString HsDocStringPrevious ds) sd = pprWithDocString (MultiLineDocString HsDocStringNext ds) sd
pprWithDocString doc@(NestedDocString HsDocStringPrevious  _) sd = sd <+> pprHsDocString doc
pprWithDocString doc sd = pprHsDocString doc $+$ sd


instance Binary HsDocString where
  put_ bh x = case x of
    MultiLineDocString dec xs -> do
      putByte bh 0
      put_ bh dec
      put_ bh xs
    NestedDocString dec x -> do
      putByte bh 1
      put_ bh dec
      put_ bh x
    GeneratedDocString x -> do
      putByte bh 2
      put_ bh x
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> MultiLineDocString <$> get bh <*> get bh
      1 -> NestedDocString <$> get bh <*> get bh
      2 -> GeneratedDocString <$> get bh
      t -> fail $ "HsDocString: invalid tag " ++ show t

data HsDocStringDecorator
  = HsDocStringNext -- ^ '|' is the decorator
  | HsDocStringPrevious -- ^ '^' is the decorator
  | HsDocStringNamed !String -- ^ '$<string>' is the decorator
  | HsDocStringGroup !Int -- ^ The decorator is the given number of '*'s
  deriving (Eq, Ord, Show, Data)

instance Outputable HsDocStringDecorator where
  ppr = text . printDecorator

printDecorator :: HsDocStringDecorator -> String
printDecorator HsDocStringNext = "|"
printDecorator HsDocStringPrevious = "^"
printDecorator (HsDocStringNamed n) = '$':n
printDecorator (HsDocStringGroup n) = replicate n '*'

instance Binary HsDocStringDecorator where
  put_ bh x = case x of
    HsDocStringNext -> putByte bh 0
    HsDocStringPrevious -> putByte bh 1
    HsDocStringNamed n -> putByte bh 2 >> put_ bh n
    HsDocStringGroup n -> putByte bh 3 >> put_ bh n
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> pure HsDocStringNext
      1 -> pure HsDocStringPrevious
      2 -> HsDocStringNamed <$> get bh
      3 -> HsDocStringGroup <$> get bh
      t -> fail $ "HsDocStringDecorator: invalid tag " ++ show t

type LHsDocStringChunk = Located HsDocStringChunk

-- | A continguous chunk of documentation
newtype HsDocStringChunk = HsDocStringChunk ByteString
  deriving (Eq,Ord,Data, Show)

instance Binary HsDocStringChunk where
  put_ bh (HsDocStringChunk bs) = put_ bh bs
  get bh = HsDocStringChunk <$> get bh

instance Outputable HsDocStringChunk where
  ppr = text . unpackHDSC


mkHsDocStringChunk :: String -> HsDocStringChunk
mkHsDocStringChunk s = HsDocStringChunk (utf8EncodeString s)

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringChunkUtf8ByteString :: ByteString -> HsDocStringChunk
mkHsDocStringChunkUtf8ByteString = HsDocStringChunk

unpackHDSC :: HsDocStringChunk -> String
unpackHDSC (HsDocStringChunk bs) = utf8DecodeByteString bs

nullHDSC :: HsDocStringChunk -> Bool
nullHDSC (HsDocStringChunk bs) = BS.null bs

mkGeneratedHsDocString :: String -> HsDocString
mkGeneratedHsDocString = GeneratedDocString . mkHsDocStringChunk

isEmptyDocString :: HsDocString -> Bool
isEmptyDocString (MultiLineDocString _ xs) = all (nullHDSC . unLoc) xs
isEmptyDocString (NestedDocString _ s) = nullHDSC $ unLoc s
isEmptyDocString (GeneratedDocString x) = nullHDSC x

docStringChunks :: HsDocString -> [LHsDocStringChunk]
docStringChunks (MultiLineDocString _ (x:|xs)) = x:xs
docStringChunks (NestedDocString _ x) = [x]
docStringChunks (GeneratedDocString x) = [L (UnhelpfulSpan UnhelpfulGenerated) x]

-- | Pretty print with decorators, exactly as the user wrote it
pprHsDocString :: HsDocString -> SDoc
pprHsDocString = text . exactPrintHsDocString

pprHsDocStrings :: [HsDocString] -> SDoc
pprHsDocStrings = text . intercalate "\n\n" . map exactPrintHsDocString

-- | Pretty print with decorators, exactly as the user wrote it
exactPrintHsDocString :: HsDocString -> String
exactPrintHsDocString (MultiLineDocString dec (x :| xs))
  = unlines' $ ("-- " ++ printDecorator dec ++ unpackHDSC (unLoc x))
            : map (\x -> "--" ++ unpackHDSC (unLoc x)) xs
exactPrintHsDocString (NestedDocString dec (L _ s))
  = "{-" ++ printDecorator dec ++ unpackHDSC s ++ "-}"
exactPrintHsDocString (GeneratedDocString x) = case lines (unpackHDSC x) of
  [] -> ""
  (x:xs) -> unlines' $ ( "-- |" ++ x)
                    : map (\y -> "--"++y) xs

-- | Just get the docstring, without any decorators
renderHsDocString :: HsDocString -> String
renderHsDocString (MultiLineDocString _ (x :| xs)) = unlines' $ map (unpackHDSC . unLoc) (x:xs)
renderHsDocString (NestedDocString _ ds) = unpackHDSC $ unLoc ds
renderHsDocString (GeneratedDocString x) = unpackHDSC x

-- | Don't add a newline to a single string
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | Just get the docstring, without any decorators
-- Seperates docstrings using "\n\n", which is how haddock likes to render them
renderHsDocStrings :: [HsDocString] -> String
renderHsDocStrings = intercalate "\n\n" . map renderHsDocString
