{-# LANGUAGE FlexibleContexts, PartialTypeSignatures, NamedWildCards #-}
module SomethingShowable where

somethingShowable :: Show _x => _x -> _
somethingShowable x = show (not x)
-- Inferred type: Bool -> String
