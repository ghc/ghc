{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Char (ord)
import Data.List (foldl')
import GHC.Core.Map.Type (TypeMap, emptyTypeMap, extendTypeMap, foldTypeMap)
import GHC.Core.Type (Type, mkStrLitTy)
import GHC.Data.FastString (FastString (..), FastZString (..))
import GHC.Utils.Encoding (zEncodeString)

main :: IO ()
main = do
  let logicalEntries =
        [ ("alpha", "payload-alpha")
        , ("beta",  "payload-beta")
        , ("gamma", "payload-gamma")
        ]
      uniquesOne = [1, 2, 3]
      uniquesTwo = [200, 100, 500]

      tmOne = buildMap logicalEntries uniquesOne
      tmTwo = buildMap logicalEntries uniquesTwo

      foldedOne = foldValues tmOne
      foldedTwo = foldValues tmTwo

  assert "foldTypeMap order independent of FastString uniques" $
    foldedOne == foldedTwo


buildMap :: [(String, a)] -> [Int] -> TypeMap a
buildMap entries uniques =
  foldl' insertEntry emptyTypeMap (zip uniques entries)
  where
    insertEntry :: TypeMap a -> (Int, (String, a)) -> TypeMap a
    insertEntry tm (u, (txt, payload)) =
      extendTypeMap tm (strLiteralWithUnique u txt) payload

foldValues :: TypeMap a -> [a]
foldValues tm = foldTypeMap (:) [] tm

strLiteralWithUnique :: Int -> String -> Type
strLiteralWithUnique u = mkStrLitTy . fakeFastString u

fakeFastString :: Int -> String -> FastString
fakeFastString u s = FastString
  { uniq = u
  , n_chars = length s
  , fs_sbs = SBS.pack (map (fromIntegral . ord) s)
  , fs_zenc = error "unused"
  }

assert :: String -> Bool -> IO ()
assert label condition = unless condition $
  error ("TypeMap string literal test failed: " ++ label)
