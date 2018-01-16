module System.Console.Haskeline.Backend.WCWidth(
                            gsWidth,
                            splitAtWidth,
                            takeWidth,
                            ) where

-- Certain characters are "wide", i.e. take up two spaces in the terminal.
-- This module wraps the necessary foreign routines, and also provides some convenience
-- functions for width-breaking code.

import System.Console.Haskeline.LineState

import Data.List
import Foreign.C.Types

foreign import ccall unsafe haskeline_mk_wcwidth :: CWchar -> CInt

wcwidth :: Char -> Int
wcwidth c = case haskeline_mk_wcwidth $ toEnum $ fromEnum c of
                -1 -> 0 -- Control characters have zero width.  (Used by the
                        -- "\SOH...\STX" hack in LineState.stringToGraphemes.)
                w -> fromIntegral w

gWidth :: Grapheme -> Int
gWidth g = wcwidth (baseChar g)

gsWidth :: [Grapheme] -> Int
gsWidth = foldl' (+) 0 . map gWidth

-- | Split off the maximal list which is no more than the given width.
-- returns the width of that list.
splitAtWidth :: Int -> [Grapheme] -> ([Grapheme],[Grapheme],Int)
splitAtWidth n xs = case splitAtWidth' n xs of
                        (this,rest,remaining) -> (this,rest,n-remaining)

-- Returns the amount of unused space in the line.
splitAtWidth' :: Int -> [Grapheme] -> ([Grapheme],[Grapheme],Int)
splitAtWidth' w [] = ([],[],w)
splitAtWidth' w (g:gs)
    | gw > w = ([],g:gs,w)
    | otherwise = (g:gs',gs'',r)
  where
    gw = gWidth g
    (gs',gs'',r) = splitAtWidth' (w-gw) gs

-- Returns the longest prefix less than or equal to the given width
-- plus the width of that list.
takeWidth :: Int -> [Grapheme] -> ([Grapheme],Int)
takeWidth n gs = case splitAtWidth n gs of
                    (gs',_,len) -> (gs',len)
