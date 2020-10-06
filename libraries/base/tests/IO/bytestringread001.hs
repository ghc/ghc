import System.Environment
import qualified Data.ByteString.Lazy as BL
import Data.Word

fold_tailrec :: (a -> b -> a) -> a -> [b] -> a
fold_tailrec _ acc [] =
    acc
fold_tailrec foldFun acc (x : xs) =
    fold_tailrec foldFun (foldFun acc x) xs

fold_tailrec' :: (a -> b -> a) -> a -> [b] -> a
fold_tailrec' _ acc [] =
    acc
fold_tailrec' foldFun acc (x : xs) =
    let acc' = foldFun acc x
    in seq acc' (fold_tailrec' foldFun acc' xs)

main :: IO ()
main =
    do
        args <- getArgs
        let filename = head args

        -- generate file
        let dt = replicate (65 * 1024) 'a'
        writeFile filename dt

        byteString <- BL.readFile filename
        let wordsList = BL.unpack byteString
        -- wordsList is supposed to be lazy (bufferized)
        let bytesCount = fold_tailrec (\acc word -> acc + 1) 0 wordsList
        print ("Total bytes in " ++ filename ++ ": " 
               ++ (show bytesCount))
