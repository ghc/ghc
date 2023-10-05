module Main (main) where

import Language.Haskell.TH

main :: IO ()
main = do
    -- type annotations are needed so the monad is not ambiguous.
    -- we also highlight that the monad can be different:
    -- brackets are "just" syntax.
    print $$(const [|| 'x' ||] ([| 'y' |]  :: IO Exp))
    print $( const  [| 'x' |] ([|| 'y' ||] :: Code IO Char))
