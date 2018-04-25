{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

module Vectorised
	( searchPA )
where
import qualified Data.Array.Parallel.Prelude.Word8	as W
import Data.Array.Parallel.Prelude.Word8		(Word8)
import Data.Array.Parallel.Prelude.Int                  as I
import Data.Array.Parallel

import qualified Prelude as Prel


-- We can't use the Prelude Char and String types in vectorised code yet..
type Char	= Word8

type String	= [: Char :]

-- | Filter potential occurrences to find matches.
-- For each candidate, checks @i@th character of pattern @w@ against candidate + @i@.
-- If characters are not the same, given @candidate@ is not an occurrence of the pattern.
next_character :: [:Int:] -> String -> String -> Int -> [:Int:]
next_character candidates w s i
 | i I.== lengthP w = candidates
 | otherwise
 = let letter      = w !: i
       next_l      = mapP (\ix -> s !: (ix I.+ i)) candidates
       (candidates',_) = unzipP (filterP (\(_,n) -> n W.== letter) (candidates `zipP` next_l))
   in  next_character candidates' w s (i I.+ 1)

-- | Find indices all occurrences of @w@ inside string @s@.
-- > string_search "bob" "b" = [0, 2]
string_search :: String -> String -> [:Int:]
string_search w s = next_character (enumFromToP 0 (lengthP s I.- lengthP w I.+ 1)) w s 0

-- Interface ------------------------------------------------------------------

-- | SEARCH
{-# NOINLINE searchPA #-}
searchPA :: PArray Word8 -> PArray Word8 -> PArray Int
searchPA w s = toPArrayP (string_search (fromPArrayP w) (fromPArrayP s))

