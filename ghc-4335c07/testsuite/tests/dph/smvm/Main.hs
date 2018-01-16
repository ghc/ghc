{-# LANGUAGE TypeOperators #-}

import SMVMVect (smvm)

import Control.Exception (evaluate)
import System.IO
import System.Environment

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.PArray as P


-- Load sparse matrix from a file
loadSM  :: String
        -> IO (PArray (PArray (Int, Double)), PArray Double)

loadSM s
  = do
      (segd, m, v) <- loadSM' s
      return $ (nestUSegd segd (fromUArray2 m), fromUArray v)


loadSM' :: String
        -> IO   ( U.Segd
                , U.Array (Int, Double)
                , U.Array Double)
loadSM' fname =
  do
    h <- openBinaryFile fname ReadMode
    lengths <- U.hGet h
    indices <- U.hGet h
    values  <- U.hGet h
    dv      <- U.hGet h
    let segd = U.lengthsToSegd lengths
        m    = U.zip indices values
    evaluate lengths
    evaluate indices
    evaluate values
    evaluate dv
    return (segd, m, dv)

main
 = do   [inFile, outFile]   <- getArgs
        (m, v)              <- loadSM inFile
        let result          = smvm m v

        -- ignore wibbles in low-order bits
        let output
                =  (unlines
                        $ map (take 12)
                        $ map show
                        $ P.toList result)
                ++ ("SUM = "
                        ++ (take 12 $ show $ sum $ P.toList result)
                        ++ "\n")

        -- check our result against the provided outFile
        outputCheck <- readFile outFile
        print $ output == outputCheck
