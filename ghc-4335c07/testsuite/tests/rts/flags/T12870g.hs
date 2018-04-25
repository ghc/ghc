module T12870g where

import GHC.RTS.Flags (getGCFlags, generations)

main :: IO ()
main = do
    gcFlags <- getGCFlags
    putStr . show $ generations gcFlags
