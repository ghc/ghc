
module Main
where
import Char

{-
Copy text from stdin to stdout.  Normalise it to make 
comparison of compiler error messages less troublesome.

-- Remove all whitespace lines
-- Merge all other whitespace into a single space.
-- Make all lowercase.
-- Look for file names and zap the directory part:
      foo/var/xyzzy/somefile.ext  -->  somefile.ext

This program is used by the test system (vanilla-test.T) to normalise
actual compilation error messages before comparing them against
expected messages.
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

zap_path 
   = reverse . takeWhile (/= '/') . reverse

main
   = interact clean



clean str
   = let 
         -- convert all non-\n whitespace to space
         toSpace c = if isSpace c && c /= '\n' then ' ' else c
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

         -- zap directory-parts in pathnames
         unpathified = unpathify unblanked
     in
         unpathified