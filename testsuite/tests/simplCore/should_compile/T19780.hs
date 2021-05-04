{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Search.DFA (strictSearcher) where

import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeIndex)

import Control.Monad (when)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.ST (newArray, newArray_, runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (Bits(..))
import Data.Word (Word8)

------------------------------------------------------------------------------
--                            Searching Function                            --
------------------------------------------------------------------------------

strictSearcher :: Bool -> S.ByteString -> S.ByteString -> [Int]
strictSearcher _ !pat
    | S.null pat = enumFromTo 0 . S.length
    | S.length pat == 1 = let !w = S.head pat in S.elemIndices w
strictSearcher !overlap pat = search
  where
    !patLen = S.length pat
    !auto   = automaton pat
    !p0     = unsafeIndex pat 0
    !ams    = if overlap then patLen else 0
    search str = match 0 0
      where
        !strLen = S.length str
        {-# INLINE strAt #-}
        strAt :: Int -> Int
        strAt !i = fromIntegral (unsafeIndex str i)
        match 0 idx
          | idx == strLen               = []
          | unsafeIndex str idx == p0   = match 1 (idx + 1)
          | otherwise                   = match 0 (idx + 1)
        match state idx
          | idx == strLen   = []
          | otherwise       =
            let !nstate = unsafeAt auto ((state `shiftL` 8) + strAt idx)
                !nxtIdx = idx + 1
            in if nstate == patLen
                then (nxtIdx - patLen) : match ams nxtIdx
                else match nstate nxtIdx

------------------------------------------------------------------------------
--                              Preprocessing                               --
------------------------------------------------------------------------------

{-# INLINE automaton #-}
automaton :: S.ByteString -> UArray Int Int
automaton !pat = runSTUArray (do
    let !patLen = S.length pat
        {-# INLINE patAt #-}
        patAt !i = fromIntegral (unsafeIndex pat i)
        !bord = kmpBorders pat
    aut <- newArray (0, (patLen + 1)*256 - 1) 0
    unsafeWrite aut (patAt 0) 1
    let loop !state = do
            let !base = state `shiftL` 8
                inner j
                    | j < 0     = if state == patLen
                                    then return aut
                                    else loop (state+1)
                    | otherwise = do
                        let !i = base + patAt j
                        s <- unsafeRead aut i
                        when (s == 0) (unsafeWrite aut i (j+1))
                        inner (unsafeAt bord j)
            if state == patLen
                then inner (unsafeAt bord state)
                else inner state
    loop 1)

-- kmpBorders calculates the width of the widest borders of the prefixes
-- of the pattern which are not extensible to borders of the next
-- longer prefix. Most entries will be 0.
{-# INLINE kmpBorders #-}
kmpBorders :: S.ByteString -> UArray Int Int
kmpBorders pat = runSTUArray (do
    let !patLen = S.length pat
        {-# INLINE patAt #-}
        patAt :: Int -> Word8
        patAt i = unsafeIndex pat i
    ar <- newArray_ (0, patLen)
    unsafeWrite ar 0 (-1)
    let dec w j
            | j < 0 || w == patAt j = return $! j+1
            | otherwise = unsafeRead ar j >>= dec w
        bordLoop !i !j
            | patLen < i    = return ar
            | otherwise     = do
                let !w = patAt (i-1)
                j' <- dec w j
                if i < patLen && patAt j' == patAt i
                    then unsafeRead ar j' >>= unsafeWrite ar i
                    else unsafeWrite ar i j'
                bordLoop (i+1) j'
    bordLoop 1 (-1))
