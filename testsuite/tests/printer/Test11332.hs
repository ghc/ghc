{-# LANGUAGE PatternSynonyms #-}

module Test11332 ( A(.., NoA), Q(F,..), G(T,..,U)) where

data A = A | B

pattern NoA = B

data Q a = Q a

pattern F a = Q a

data G = G | H

pattern T = G

pattern U = H
