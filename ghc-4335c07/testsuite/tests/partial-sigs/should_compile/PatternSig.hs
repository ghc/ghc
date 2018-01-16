{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables #-}
module PatternSig where

bar :: Bool -> Bool
bar (x :: _) = True
