{-# LANGUAGE TypeApplications #-}

import Control.Monad (mapM_)
import Control.Exception (SomeException, try)
import System.IO (stdin, stdout, stderr)
import System.IO.OS
       (
           withFileDescriptorReadingBiasedRaw,
           withFileDescriptorWritingBiasedRaw,
           withWindowsHandleReadingBiasedRaw,
           withWindowsHandleWritingBiasedRaw
       )

main :: IO ()
main = mapM_ ((>>= print) . try @SomeException) $
       [
           withFileDescriptorReadingBiasedRaw stdin  (return . show),
           withFileDescriptorWritingBiasedRaw stdout (return . show),
           withFileDescriptorWritingBiasedRaw stderr (return . show),
           withWindowsHandleReadingBiasedRaw  stdin  (return . const "_"),
           withWindowsHandleWritingBiasedRaw  stdout (return . const "_"),
           withWindowsHandleWritingBiasedRaw  stderr (return . const "_")
       ]
