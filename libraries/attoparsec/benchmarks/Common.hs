{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common (
      chunksOf
    , pathTo
    , rechunkBS
    , rechunkT
    ) where

import Control.DeepSeq (NFData(rnf))
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Text.Parsec (ParseError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

#if !MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Internal (ByteString(..))

instance NFData ByteString where
    rnf (PS _ _ _) = ()
#endif

instance NFData ParseError where
    rnf = rnf . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where go xs = case splitAt k xs of
                  ([],_)  -> []
                  (y, ys) -> y : go ys

rechunkBS :: Int -> B.ByteString -> BL.ByteString
rechunkBS n = BL.fromChunks . map B.pack . chunksOf n . B.unpack

rechunkT :: Int -> T.Text -> TL.Text
rechunkT n = TL.fromChunks . map T.pack . chunksOf n . T.unpack

pathTo :: String -> IO FilePath
pathTo wat = do
  exists <- doesDirectoryExist "benchmarks"
  return $ if exists
           then "benchmarks" </> wat
           else wat
