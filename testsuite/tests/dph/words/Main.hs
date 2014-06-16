
import WordsVect
import Data.Array.Parallel
import qualified Data.Array.Parallel.Prelude.Word8	as W
import qualified Data.Array.Parallel.PArray 		as P
import qualified Data.Array.Parallel.Unlifted		as U
import Data.Char

main 
 = do	-- take the filename containing the words as the first arg
	let str	=  "When   I   look  into  the   looking glass I'm always sure to see"
		++ " no matter how I dodge         about, me looking      back at me."

	-- convert string to a PArray
	let paStr   :: PArray W.Word8
	    paStr = P.fromUArray $ U.map W.fromInt $ U.fromList $ map ord str
	
	
	-- break the string into words then flatten it back		
	let str' :: String
	    str' = map chr 
		 $ map fromIntegral
		 $ P.toList 
		 $ wordsOfPArray paStr
			

	-- count the number of words in the string, using the vectorised program
	let wordCountVect = fromIntegral $ wordCountOfPArray paStr
	
	-- count the number of words with the ye'olde list way
	let wordCountList = length $ words str
	
	-- 
	putStr 	$  show str' ++ "\n"
		++ "word count vect  = " ++ show wordCountVect ++ "\n"
		++ "word count lists = " ++ show wordCountList ++ "\n"
		