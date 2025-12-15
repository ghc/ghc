{-# LANGUAGE PartialTypeSignatures #-}
module UncurryNamed where

unc :: (_a -> _b -> _c) -> (_a, _b) -> _c
unc = uncurry
