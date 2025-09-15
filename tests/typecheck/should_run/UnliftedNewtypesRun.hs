{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE RankNTypes #-}

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#)
import GHC.Types

main :: IO ()
main = do
  let a = idIntRep (FooC 6#)
      b = idWordRep (BarC 7##)
      c = idWordRep (PatC 3##)
      d = idIntRep (DarthC 5#)
  print (I# (getFoo a))
  print (W# (case b of BarC w -> w))
  print (W# (case c of PatC w -> w))
  print (I# (case d of DarthC w -> w))
  print (A1 13#)
  print (A2 15##)

newtype Darth = DarthC Int#

newtype Foo = FooC { getFoo :: Int# }

newtype Bar :: TYPE 'WordRep where
  BarC :: Word# -> Bar

newtype Pat where
  PatC :: Word# -> Pat

data A1 :: Type where
  A1 :: Int# -> A1
  deriving (Show)

data A2 = A2 Word#
  deriving (Show)

idIntRep :: forall (a :: TYPE 'IntRep). a -> a
idIntRep x = x

idWordRep :: forall (a :: TYPE 'WordRep). a -> a
idWordRep x = x
