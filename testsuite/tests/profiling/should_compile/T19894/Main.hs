module Main (main) where

import Data.Char (ord)
import Data.Word (Word8)
import System.IO (openFile, IOMode(..), Handle)
import StreamK (IsStream, MonadAsync)
import qualified Operations
import qualified StreamK
import qualified Fold
import qualified Handle
import qualified Array
import Array (Array)

toarr :: String -> Array Word8
toarr = Array.fromList . map (fromIntegral . ord)

-- | Split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO ()
splitOnSeq str inh =
    Operations.drain $ Operations.splitOnSeq (toarr str) Fold.drain
        $ Operations.unfold Handle.read inh

main :: IO ()
main = do
    inh <- openFile "input.txt" ReadMode
    splitOnSeq "aa" inh
