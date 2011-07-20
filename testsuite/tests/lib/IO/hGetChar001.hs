import System.IO

main = do
 hSetBuffering stdout NoBuffering
 putStr   "Enter an integer: "
 x1 <- readLine
 putStr   "Enter another integer: "
 x2 <- readLine
 putStr  ("Their sum is " ++ show (read x1 + read x2 :: Int) ++ "\n")

 where readLine = do
           eof <- isEOF
           if eof then return [] else do
           c <- getChar
   	   if c `elem` ['\n','\r'] 
		then return []
                else do cs <- readLine
                        return (c:cs)
