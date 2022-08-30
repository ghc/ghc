{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- | Parsing the top of a JS source file to get its options.
--
-----------------------------------------------------------------------------

module GHC.JS.Parser.Header
   ( getOptionsFromJsFile
   , JSOption(..)
   )
where

import GHC.Prelude

import System.IO
import Data.Char (isSpace)
import qualified Data.ByteString as B
import qualified Control.Exception as Exception

getOptionsFromJsFile :: FilePath      -- ^ Input file
                     -> IO [JSOption] -- ^ Parsed options, if any.
getOptionsFromJsFile filename
    = Exception.bracket
              (openBinaryFile filename ReadMode)
              hClose
              getJsOptions

data JSOption = CPP deriving (Eq, Ord)

getJsOptions :: Handle -> IO [JSOption]
getJsOptions handle = do
  hSetEncoding handle utf8
  prefix' <- B.hGet handle prefixLen
  if prefix == prefix'
  then parseJsOptions <$> hGetLine handle
  else pure []
 where
  prefix :: B.ByteString
  prefix = "//#OPTIONS:"
  prefixLen = B.length prefix

parseJsOptions :: String -> [JSOption]
parseJsOptions xs = go xs
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    go [] = []
    go xs = let (tok, rest) = break (== ',') xs
                tok' = trim tok
                rest' = drop 1 rest
            in  if | tok' == "CPP" -> CPP : go rest'
                   | otherwise     -> go rest'
