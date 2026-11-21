{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
-- | Select a result type with a retained signature from a quote.
module NativeRetainedTH (Select, quoted, quotedCase, render, renderCase) where

-- | Select a type from a Boolean index.
type Select (b :: Bool) = case b of { True -> Int; False -> Char }

-- | Keep the Boolean as an argument when splicing the quoted signature.
quoted :: $([t| foreach (b :: Bool) -> Select b |])
quoted True = 11
quoted False = 'x'

-- | Splice the case that computes the result type after an ordinary argument.
quotedCase :: $([t| foreach (b :: Bool) -> () -> case b of { True -> Int; False -> Char } |])
quotedCase True () = 19
quotedCase False () = 'q'

-- | Render the selected result at the same runtime index.
render :: foreach (b :: Bool) -> Select b -> String
render True n = show n
render False c = [c]

-- | Accept the case result from an independently spliced signature.
renderCase :: $([t| foreach (flag :: Bool) ->
                    (case flag of { True -> Int; False -> Char }) -> String |])
renderCase True i = show i
renderCase False c = [c]
