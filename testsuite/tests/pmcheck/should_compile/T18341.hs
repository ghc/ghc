{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Lib where

import GHC.Exts

data T = MkT !Int {-# UNPACK #-} !Int Int#

f :: T -> ()
f (MkT  _ _ _) | False = () -- inaccessible
f (MkT !_ _ _) | False = () -- redundant, not only inaccessible!
f _                    = ()

g :: T -> ()
g (MkT _  _ _) | False = () -- inaccessible
g (MkT _ !_ _) | False = () -- redundant, not only inaccessible!
g _                    = ()

h :: T -> ()
h (MkT _ _  _) | False = () -- inaccessible
h (MkT _ _ !_) | False = () -- redundant, not only inaccessible!
h _                    = ()
