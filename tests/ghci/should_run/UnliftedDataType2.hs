{-# LANGUAGE StandaloneKindSignatures, UnliftedDatatypes #-}

import GHC.Exts

type Tree :: forall l. TYPE (BoxedRep l)
data Tree where
  Leaf :: !Word -> Tree @l
  Bin :: Tree @Unlifted -> Tree @Unlifted -> Tree @l

type Set = Tree @Lifted

mseq :: Tree @Lifted -> Tree @Unlifted
mseq (Leaf w)  = Leaf w
mseq (Bin l r) = Bin l r

member :: Word -> Set -> Bool
member w t = wmember w (mseq t)

wmember :: Word -> Tree @Unlifted -> Bool
wmember w (Leaf w2) = w == w2
wmember w (Bin l r) = wmember w l || wmember w r

set :: Set
set = Bin (Leaf 1) (Leaf 42)

main :: IO ()
main = print $ member 42 set

