#include "../../../ghc/includes/config.h"

module Main
where
import Char
import List ( isPrefixOf )
import IOExts ( trace )

{-
Copy text from stdin to stdout.  Normalise it to make 
comparison of compiler error messages less troublesome.

-- Remove all whitespace lines
-- Merge contiguous whitespace characters into a single space.
-- Make all letters lowercase.
-- Look for file names and zap the directory part:
      foo/var/xyzzy/somefile  -->  somefile
-- If somefile ends in ".exe" or ".exe:", zap ".exe" (for Windows)
--    the colon is there because it appears in error messages; this
--    hacky solution is used in place of more sophisticated filename
--    mangling

This program is used by the test system (vanilla-test.T) to normalise
actual compilation error messages before comparing them against
expected messages. It can optionally be used on stderr output too.
-}

isFnChar = not . isSpace
isOther  = not . isFnChar

unpathify [] 
   = []
unpathify (c:cs)
   | isFnChar c
   = let cz = takeWhile isFnChar (c:cs) 
     in  zap_path cz ++ unpathify (drop (length cz) (c:cs))
   | otherwise
   = let cz = takeWhile (not.isFnChar) (c:cs)
     in  cz ++ unpathify (drop (length cz) (c:cs))

zap_path p
   = let notDirSep c = not (c `elem` "/\\")
         zapped = (takeWhile (notDirSep) . reverse) p
         -- s/\.exe:?$//
	 exe = ".exe"
	 exe_colon = ".exe:"
         col_zapped = if (reverse exe_colon) `isPrefixOf` zapped
	                then ":" ++ (drop (length exe_colon) zapped)
	                else zapped
     in reverse (if (reverse exe) `isPrefixOf` col_zapped
	           then (drop (length exe) col_zapped)
	           else col_zapped)

main
   = interact clean



clean str
   = let 
         -- convert all non-\n whitespace to space
         toSpace c = if isSpace c then ' ' else c
         spaced = map toSpace str

         -- collapse sequences of spaces into one
         collapse []           = []
         collapse (' ':' ':cs) = collapse (' ':cs)
         collapse (c:cs)       = c : collapse cs
         collapsed = collapse spaced

         -- lowercasify everything
         lowered = map toLower collapsed

         -- zap blank lines
         unblanked 
            = (unlines . map (dropWhile isSpace)
                       . filter (not . (all isSpace)) . lines) 
              lowered

         -- zap directory-parts and .exe in pathnames
         unpathified = unpathify unblanked
     in
         unpathified
