{-# LANGUAGE ScopedTypeVariables #-}
module Dump
        ( dumpWorld
        , printWorld)
where
import Body
import World
import System.IO
import Data.List
import Text.Printf
import qualified Data.Vector.Unboxed    as V


-- | Dump the bodies in a world to a file.
dumpWorld :: World -> FilePath -> IO ()
dumpWorld world filePath
 = do   h       <- openFile filePath WriteMode
        mapM_ (hWriteBody h)
                $ V.toList 
                $ worldBodies world
        hClose h


-- | Print the bodies in a world to stdout
printWorld :: World -> IO ()
printWorld world
 = do   mapM_ (hWriteBody stdout)
                $ V.toList 
                $ worldBodies world


-- | Write a single body to a file.
hWriteBody :: Handle -> Body -> IO ()
hWriteBody h ((px, py, mass), (vx, vy), (ax, ay))
        = hPutStrLn h 
        $ concat 
        $ (  (padRc 8 ' ' $ show mass)
          :  " " 
          : (intersperse " " 
                $ map (\f -> printf "%15.8f" f) [ px, py, vx, vy, ax, ay ]))


-- | Right justify a doc, padding with a given character.
padRc :: Int -> Char -> String -> String
padRc n c str
        = replicate (n - length str) c ++ str
