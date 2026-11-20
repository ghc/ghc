{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
-- | Select a result type with a retained signature from a quote.
module NativeRetainedTH (Select, quoted, render) where

-- | Select a type from a Boolean index.
type Select (b :: Bool) = case b of { True -> Int; False -> Char }

-- | Keep the Boolean as an argument when splicing the quoted signature.
quoted :: $([t| foreach (b :: Bool) -> Select b |])
quoted True = 11
quoted False = 'x'

-- | Render the selected result at the same runtime index.
render :: foreach (b :: Bool) -> Select b -> String
render True n = show n
render False c = [c]
