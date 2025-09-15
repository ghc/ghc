--We check the generation count as a way to verify an RTS argument
--was actually parsed and accepted by the RTS.

import GHC.RTS.Flags (getGCFlags, generations)

main :: IO ()
main = do
    gcFlags <- getGCFlags
    putStr . show $ generations gcFlags
