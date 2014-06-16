
-- Break up a string into words in parallel.
-- 	Based on the presentation "Breaking Sequential Habits of Thought", Guy Steele.
--	http://groups.csail.mit.edu/mac/users/gjs/6.945/readings/MITApril2009Steele.pdf
--
-- NOTE: This is a naive implementation, and I haven't benchmarked it.
--       Using parallel arrays in Seg probably isn't helpful for performance,
--       but it's a stress test for the vectoriser.
--
--       If we actually cared about performance we wouldn't want to recursively
--       subdivide the string right down to individual characters.
--
{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

module WordsVect
  ( wordsOfPArray
  , wordCountOfPArray )
where
import qualified Data.Array.Parallel.Prelude.Word8 as W
import Data.Array.Parallel.Prelude.Word8 (Word8)
import Data.Array.Parallel.Prelude.Int as I
import Data.Array.Parallel

import qualified Prelude as Prel


-- We can't use the Prelude Char and String types in vectorised code yet..
type Char	= Word8
char_space	= W.fromInt 32

type String	= [: Char :]


-- | Word state
data State
  = Chunk String
  | Seg   String  -- initial word chunk
    [:String:]    -- complete words in the middle of the segment
    String        -- final word chunk


-- | Compose two wordstates.
plusState :: State -> State -> State
plusState str1 str2
 = case (str1, str2) of
  (Chunk as, Chunk bs)    -> Chunk (as +:+ bs)
  (Chunk as, Seg bl bss br) -> Seg (as +:+ bl) bss br
  (Seg al ass ar, Chunk bs) -> Seg al ass (ar +:+ bs)
  (Seg al ass ar, Seg bl bss br)  -> Seg al (ass +:+ joinEmpty [:ar +:+ bl:] +:+ bss) br

joinEmpty :: [:[:Word8:]:] -> [:[:Word8:]:]
joinEmpty ws 
	| lengthP ws I.== 1 && lengthP (ws !: 0) I.== 0	= [::]
	| otherwise					= ws


-- | Convert a single char to a wordstate.
stateOfChar :: Char -> State
stateOfChar c
	| c W.== char_space	= Seg [::] [::] [::]
	| otherwise		= Chunk [:c:]
	
	
-- | Break this string into words.
stateOfString :: String -> State
stateOfString str
 = let 	len	= lengthP str
   	result
	 | len I.== 0	= Chunk [::]
	 | len I.== 1	= stateOfChar (str !: 0)
	 | otherwise	
	 =  let	half	= len `div` 2
		s1	= sliceP 0    half       str
		s2	= sliceP half (len I.- half) str
	    in	plusState (stateOfString s1) (stateOfString s2)
    in	result


-- | Count the number of words in a string.
countWordsOfState :: State -> Int
countWordsOfState state
 = case state of
	Chunk c		-> wordsInChunkArr c
	Seg c1 ws c2 	-> wordsInChunkArr c1 I.+ lengthP ws I.+ wordsInChunkArr c2
	
wordsInChunkArr :: [:Word8:] -> Int
wordsInChunkArr arr
	| lengthP arr I.== 0	= 0
	| otherwise		= 1


-- | Flatten a state back to an array of Word8s,
--	inserting spaces between the words.
flattenState :: State -> [:Word8:]
flattenState ss
 = case ss of
	Chunk s	-> s

	Seg   w1 ws w2	
		->  w1 
		+:+ [:char_space:]
		+:+ concatP [: w +:+ [:char_space:] | w <- ws :]
		+:+ w2

-- Interface ------------------------------------------------------------------

-- | Break up an array of chars into words then flatten it back.
wordsOfPArray :: PArray Word8 -> PArray Word8
{-# NOINLINE wordsOfPArray #-}
wordsOfPArray arr
 = let	str	= fromPArrayP arr
	state	= stateOfString str
	strOut	= flattenState state
   in	toPArrayP strOut


-- | Count the number of words in an array
wordCountOfPArray :: PArray Word8 -> Int
{-# NOINLINE wordCountOfPArray #-}
wordCountOfPArray arr
 = let	str	= fromPArrayP arr
	state	= stateOfString str
   in	countWordsOfState state

