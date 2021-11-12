{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module FoldrExample where
{-
inplace/bin/ghc-stage0 -O2 -dcore-lint
-}

import GHC.Base
import Data.Maybe

qux :: [Maybe Char] -> String
qux str = foldr (maybe id (:)) "" str

{-

[1 of 1] Compiling FoldrExample     ( linear-tests/Foldr.hs, linear-tests/Foldr.o )

linear-tests/Foldr.hs:11:27: error:
    • Couldn't match type ‘[Char] ⊸ [Char]’ with ‘[Char] -> [Char]’
      Expected type: Char -> [Char] -> [Char]
        Actual type: Char ⊸ [Char] ⊸ [Char]
    • In the second argument of ‘maybe’, namely ‘(:)’
      In the first argument of ‘foldr’, namely ‘(maybe id (:))’
      In the expression: foldr (maybe id (:)) "" str
   |
11 | qux str = foldr (maybe id (:)) "" str
   |                           ^^^

-}
