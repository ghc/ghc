
module M where

-- GHC's Lexer.x had a piece of code like this

f = if True then do
        case () of
            () -> ()
            else ()

