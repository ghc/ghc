-- !!! Testing hGetPosn and hSetPosn
module Main(main) where

import IO
import IOExts

getPos :: HandlePosn -> HandlePosition
getPos (HandlePosn _ x) = x

getPosnAndPrint h = do
  x <- hGetPosn h
  v <- hGetChar h
  putStrLn ("At position: " ++ show (getPos x) ++ ", found: " ++ show v)
  return x

recordDoAndRepos h a = do
  x <- getPosnAndPrint h
  a 
  hSetPosn x
  getPosnAndPrint h
  return ()

recordDoAndRepos2 h a = do
  x <- getPosnAndPrint h
  a 
  hSeek h AbsoluteSeek (getPos x)
  getPosnAndPrint h
  return ()

recordDoAndRepos3 h a = do
  x <- getPosnAndPrint h
  a 
  hSeek h SeekFromEnd (negate (getPos x + 1))
  getPosnAndPrint h
  return ()

main :: IO ()
main = do
  h  <- openFile "io001.hs" ReadMode
  recordDoAndRepos h $
   recordDoAndRepos h $
    recordDoAndRepos h $
     recordDoAndRepos h $
      recordDoAndRepos h $
       putStrLn ""
  hClose h
  putStrLn ""
  h  <- openFileEx "io001.hs" (BinaryMode ReadMode)
  recordDoAndRepos h $
   recordDoAndRepos h $
    recordDoAndRepos h $
     recordDoAndRepos h $
      recordDoAndRepos h $
       putStrLn ""
  hClose h
  putStrLn "\nUsing hSeek/AbsoluteSeek: "
  h  <- openFile "io001.hs" ReadMode
  recordDoAndRepos2 h $
   recordDoAndRepos2 h $
    recordDoAndRepos2 h $
     recordDoAndRepos2 h $
      recordDoAndRepos2 h $
       putStrLn ""

  hClose h
  putStrLn "\nUsing hSeek/SeekFromEnd: "
  h  <- openFile "io001.hs" ReadMode
  recordDoAndRepos3 h $
   recordDoAndRepos3 h $
    recordDoAndRepos3 h $
     recordDoAndRepos3 h $
      recordDoAndRepos3 h $
       putStrLn ""
