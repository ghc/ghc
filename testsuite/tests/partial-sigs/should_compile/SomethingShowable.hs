{-# LANGUAGE FlexibleContexts, PartialTypeSignatures, NamedWildCards, GHC2021 #-}
module SomethingShowable where

somethingShowable :: Show _x => _x -> _
somethingShowable x = show (not x)
-- Inferred type: Bool -> String
