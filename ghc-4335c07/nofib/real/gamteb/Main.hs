-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	GamtebMain
import System.IO

main = do
    hPutStr stderr "Enter the scale of computation: "
    s <- getContents
    let (scale, rest) = (head (reads s)) :: (Int, String)
    putStr (takeWhile ((/=) '\n') s ++ (gamteb scale))
