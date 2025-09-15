{- This program crashed GHC 2.03

   From: Marc van Dongen <dongen@cs.ucc.ie>
   Date: Sat, 31 May 1997 14:35:40 +0100 (BST)

  zonkIdOcc: g_aoQ

  panic! (the `impossible' happened):
          lookupBindC:no info!
  for: g_aoQ
  (probably: data dependencies broken by an optimisation pass)
  static binds for:
  Tmp.$d1{-rmM,x-}
  local binds for:
-}

module ShouldFail where

data AB p q = A
            | B p q

g :: (Ord p,Ord q) => (AB p q) -> Bool
g (B _ _) = g A

