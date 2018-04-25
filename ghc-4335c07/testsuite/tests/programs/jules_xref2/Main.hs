{-# LANGUAGE ScopedTypeVariables #-}
-- partain: the failure (crashing) was w/ -prof-auto compilation

module Main where

import Control.Exception (IOException, catch)

xreff :: Int -> [String] -> Table -> Int -> String -> String
xreff cc exs stab lineno [] = display (foldl delete stab exs)
xreff cc exs stab lineno ('\n':cs) = xreff cc exs stab (lineno+1) cs
xreff cc exs stab lineno (c:cs) 
  = if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
       case getRestWord cs of
         (word, rest) -> if (cc :: Int) == 0
          then   if stab == stab
                 then
                    xreff 1000 exs 
                          (enter lineno stab (c:word)) lineno rest
                 else error "Force failed?!"
          else      xreff (cc-1) exs 
                        (enter lineno stab (c:word)) lineno rest
      else xreff cc exs stab lineno cs

xref exceptions source = xreff 1000 exceptions ALeaf 1 source

getRestWord [] = ([], [])
getRestWord xs@(x:xs')
   | (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9')
   = case getRestWord xs' of 
        (ys,zs) -> if (x >= 'A' && x <= 'Z')
                   then (toEnum (fromEnum x + (32::Int)):ys, zs)
                   else (x:ys, zs)
   | otherwise 
   = ([],xs)

data Table = ALeaf | ABranch Table String [Int] Table deriving (Eq)

enter n ALeaf w = ABranch ALeaf w [n] ALeaf
enter n (ABranch l k ns r) w
 = if w < k then ABranch (enter n l w) k ns r else
   if w > k then ABranch l k ns (enter n r w) else
                 ABranch l k (n:ns) r

delete ALeaf w              = ALeaf
delete (ABranch l k ns r) w
 = if w < k then ABranch (delete l w) k ns r else
   if w > k then ABranch l k ns (delete r w) else
                 ABranch l k [] r

display :: Table -> String
display t = display_a t ""

display_a :: Table -> String -> String
display_a ALeaf acc = acc
display_a (ABranch l k ns r) acc
 = display_a l (dispLine k ns ++ display_a r acc)

dispLine k [] = ""
dispLine k ns = k ++ ":" ++ dispNos ns ++ "\n"

dispNos :: [Int] -> String
dispNos []     = ""
dispNos (n:ns) = ' ':(show n ++ dispNos ns)

main = do
    input <- getContents
    exceptions <- catch (readFile "exceptions") (\(e :: IOException) -> return "")
    putStr (xref (lines exceptions) input)

{- OLD 1.2:
main = readChan stdin abort (\input ->
       readFile "exceptions"
                (\errors     -> output (xref []                 input))
                (\exceptions -> output (xref (lines exceptions) input)))
       where output s = appendChan stdout s abort done
-}
