module Main where

import GHC.InfoProv
import qualified X

main = do
    printIp =<< whereFrom cafA1
    printIp =<< whereFrom cafA2
    printIp =<< whereFrom cafB1
    printIp =<< whereFrom cafB2
    printIp =<< whereFrom cafC1
    printIp =<< whereFrom cafC2
    printIp =<< whereFrom (ACon ())
    printIp =<< whereFrom cafXA
    printIp =<< whereFrom X.cafXA1
    printIp =<< whereFrom X.cafXA2
    printIp =<< whereFrom (X.ACon ())
    printIp =<< whereFrom (BCon cafA1)
    printIp =<< whereFrom (CCon (cafA1, BCon (ACon ())))
  where
    -- Get rid of the src file path since it makes test output difficult to diff
    -- on Windows
    printIp = print . stripIpSrc
    stripIpSrc (Just ip) = ip { ipSrcFile = "" }

data A = ACon ()
data B = BCon A
data C = CCon (A, B)

cafA1 = ACon ()
cafA2 = ACon ()
cafB1 = BCon cafA1
cafB2 = BCon cafA2
cafC1 = CCon (cafA1, cafB1)
cafC2 = CCon (cafA2, cafB2)

cafXA = X.ACon ()
