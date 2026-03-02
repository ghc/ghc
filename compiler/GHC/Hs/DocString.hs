{-# LANGUAGE TypeFamilies #-}

-- | An exactprintable structure for docstrings

module GHC.Hs.DocString
  ( LHsDocString
  , HsDocString(..)
  , HsDocStringGhc
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
  , printDecorator
  ) where

import GHC.Prelude

import GHC.Hs.Extension

import GHC.Utils.Binary
import GHC.Utils.Encoding
import GHC.Utils.Outputable as Outputable hiding ((<>))
import GHC.Types.SrcLoc
import Control.DeepSeq

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)
import Data.Void

import Language.Haskell.Syntax.Doc
import Language.Haskell.Syntax.Extension

type LHsDocString pass = Located (HsDocString pass)

type HsDocStringGhc = HsDocString Void

{-
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
-}

type instance XMultiLineDocString (GhcPass p) = NoExtField
type instance XNestedDocString    (GhcPass p) = NoExtField
type instance XGeneratedDocString (GhcPass p) = NoExtField
type instance XXHsDocString       (GhcPass p) = DataConCantHappen

{-
instance NFData HsDocString where
  rnf (MultiLineDocString a b) = rnf a `seq` rnf b
  rnf (NestedDocString a b) = rnf a `seq` rnf b
  rnf (GeneratedDocString a) = rnf a
-}
deriving stock instance Eq   (HsDocString (GhcPass pass))
-- deriving stock instance Show (HsDocString (GhcPass pass))

instance Binary (HsDocString (GhcPass p)) where
  put_ bh x = case x of
    MultiLineDocString _ dec xs -> do
      putByte bh 0
      put_ bh dec
      put_ bh $ BinLocated <$> xs
    NestedDocString _ dec x -> do
      putByte bh 1
      put_ bh dec
      put_ bh $ BinLocated x
    GeneratedDocString _ x -> do
      putByte bh 2
      put_ bh x
  get bh = do
    tag <- getByte bh
    case tag of
      0 -> MultiLineDocString NoExtField <$> get bh <*> (fmap unBinLocated <$> get bh)
      1 -> NestedDocString    NoExtField <$> get bh <*> (unBinLocated <$> get bh)
      2 -> GeneratedDocString NoExtField <$> get bh
      t -> fail $ "HsDocString: invalid tag " ++ show t

instance NFData (HsDocString (GhcPass pass)) where
  rnf = \case
    MultiLineDocString NoExtField a b -> rnf a `seq` rnf b
    NestedDocString    NoExtField a b -> rnf a `seq` rnf b
    GeneratedDocString NoExtField a   -> rnf a

instance Outputable (HsDocString (GhcPass p)) where
  ppr = text . renderHsDocString

-- | Annotate a pretty printed thing with its doc
-- The docstring comes after if is 'HsDocStringPrevious'
-- Otherwise it comes before.
-- Note - we convert MultiLineDocString HsDocStringPrevious to HsDocStringNext
-- because we can't control if something else will be pretty printed on the same line
pprWithDocString :: HsDocString (GhcPass p) -> SDoc -> SDoc
pprWithDocString  (MultiLineDocString x HsDocStringPrevious ds) sd = pprWithDocString (MultiLineDocString x HsDocStringNext ds) sd
pprWithDocString doc@(NestedDocString _ HsDocStringPrevious  _) sd = sd <+> pprHsDocString doc
pprWithDocString doc sd = pprHsDocString doc $+$ sd

{-
data HsDocStringDecorator
  = HsDocStringNext -- ^ '|' is the decorator
  | HsDocStringPrevious -- ^ '^' is the decorator
  | HsDocStringNamed !String -- ^ '$<string>' is the decorator
  | HsDocStringGroup !Int -- ^ The decorator is the given number of '*'s
  deriving (Eq, Ord, Show, Data)
-}

instance Outputable HsDocStringDecorator where
  ppr = text . printDecorator

{-
instance NFData HsDocStringDecorator where
  rnf HsDocStringNext = ()
  rnf HsDocStringPrevious = ()
  rnf (HsDocStringNamed x) = rnf x
  rnf (HsDocStringGroup x) = rnf x
-}

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

{-
type LHsDocStringChunk = Located HsDocStringChunk

-- | A contiguous chunk of documentation
newtype HsDocStringChunk = HsDocStringChunk ByteString
  deriving stock (Eq,Ord,Data, Show)
  deriving newtype (NFData)
-}

type instance Anno HsDocStringChunk = SrcSpan

instance Binary HsDocStringChunk where
  put_ bh (HsDocStringChunk bs) = put_ bh bs
  get bh = HsDocStringChunk <$> get bh


instance Outputable HsDocStringChunk where
  ppr = text . unpackHDSC

{-
mkHsDocStringChunk :: String -> HsDocStringChunk
mkHsDocStringChunk s = HsDocStringChunk (utf8EncodeByteString s)

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringChunkUtf8ByteString :: ByteString -> HsDocStringChunk
mkHsDocStringChunkUtf8ByteString = HsDocStringChunk

unpackHDSC :: HsDocStringChunk -> String
unpackHDSC (HsDocStringChunk bs) = utf8DecodeByteString bs

nullHDSC :: HsDocStringChunk -> Bool
nullHDSC (HsDocStringChunk bs) = BS.null bs

mkGeneratedHsDocString :: String -> HsDocString
mkGeneratedHsDocString = GeneratedDocString . mkHsDocStringChunk
-}

isEmptyDocString :: HsDocString (GhcPass p) -> Bool
isEmptyDocString (MultiLineDocString _ _ xs) = all (nullHDSC . unLoc) xs
isEmptyDocString (NestedDocString _ _ s) = nullHDSC $ unLoc s
isEmptyDocString (GeneratedDocString _ x) = nullHDSC x

docStringChunks :: HsDocString (GhcPass p) -> [LHsDocStringChunk (GhcPass p)]
docStringChunks (MultiLineDocString _ _ (x:|xs)) = x:xs
docStringChunks (NestedDocString _ _ x) = [x]
docStringChunks (GeneratedDocString _ x) = [L (UnhelpfulSpan UnhelpfulGenerated) x]

-- | Pretty print with decorators, exactly as the user wrote it
pprHsDocString :: HsDocString (GhcPass p) -> SDoc
pprHsDocString = text . exactPrintHsDocString

pprHsDocStrings :: [HsDocString (GhcPass p)] -> SDoc
pprHsDocStrings = text . intercalate "\n\n" . map exactPrintHsDocString

-- | Pretty print with decorators, exactly as the user wrote it
exactPrintHsDocString :: HsDocString (GhcPass p) -> String
exactPrintHsDocString (MultiLineDocString _ dec (x :| xs))
  = unlines' $ ("-- " ++ printDecorator dec ++ unpackHDSC (unLoc x))
            : map (\x -> "--" ++ unpackHDSC (unLoc x)) xs
exactPrintHsDocString (NestedDocString _ dec (L _ s))
  = "{-" ++ printDecorator dec ++ unpackHDSC s ++ "-}"
exactPrintHsDocString (GeneratedDocString _ x) = case lines (unpackHDSC x) of
  [] -> ""
  (x:xs) -> unlines' $ ( "-- |" ++ x)
                    : map (\y -> "--"++y) xs

-- | Just get the docstring, without any decorators
renderHsDocString :: HsDocString (GhcPass p) -> String
renderHsDocString (MultiLineDocString _ _ (x :| xs)) = unlines' $ map (unpackHDSC . unLoc) (x:xs)
renderHsDocString (NestedDocString _ _ ds) = unpackHDSC $ unLoc ds
renderHsDocString (GeneratedDocString _ x) = unpackHDSC x

-- | Don't add a newline to a single string
unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | Just get the docstring, without any decorators
-- Separates docstrings using "\n\n", which is how haddock likes to render them
renderHsDocStrings :: [HsDocString (GhcPass p)] -> String
renderHsDocStrings = intercalate "\n\n" . map renderHsDocString
