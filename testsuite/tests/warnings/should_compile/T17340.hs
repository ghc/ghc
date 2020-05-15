{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T17340 where

data A = A { a :: () }
data B = B
newtype C = C Int
pattern P = B

conPat :: B -> ()
conPat !B = ()

parPat :: A -> ()
parPat !(A _) = ()

asPat :: A -> ()
asPat !a@(A _) = ()

sigPat :: B -> ()
sigPat (!B :: B) = ()

viewPat :: B -> ()
viewPat (id -> !B) = ()

wildPat :: A -> ()
wildPat !A{..} = ()

listPat :: [Int] -> ()
listPat ![] = ()
listPat _ = ()

list2Pat :: [Int] -> ()
list2Pat !(_:_) = ()
list2Pat _ = ()

tuplePat :: (B,B) -> ()
tuplePat !(_, _) = ()

-- no warning expected
newtyPat :: C -> ()
newtyPat !(C _) = ()

-- no warning expected
patSyn :: B -> ()
patSyn !P = ()
