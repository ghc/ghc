--!! Querying for system information.
module Main(main) where

import Posix

main = do
  sid <- getSystemID
  let
   info =
    [ "Node Name: "   , nodeName sid
    , "OS: "	      , systemName sid
    , "Arch: "        , machine sid
    , "Version: "     , version sid
    , "Release: "     , release sid
    ]
  putStrLn2 info

putStrLn2 :: [String] -> IO ()
putStrLn2 []  = return ()
putStrLn2 [x] = putStrLn x
putStrLn2 (x1:x2:xs) = putStrLn (x1++x2) >> putStrLn2 xs
