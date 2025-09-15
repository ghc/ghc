{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
-- Test that a COMPLETE pragma over constructors of different types is a valid
-- declaration, but that it's not suggested in any warning.
module TyMismatch where

data T = A | B | C

{-# COMPLETE Just, A #-}

f A = ()        -- should not suggest 'Just'

g (Just _) = () -- should not suggest 'A'
