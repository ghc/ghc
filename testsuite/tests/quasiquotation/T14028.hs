{-# LANGUAGE QuasiQuotes #-}

import T14028Quote

s :: String
s = [here|goes nothing|]

main = putStrLn s
