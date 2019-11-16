{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#,(+#))
import GHC.Types

main :: IO ()
main = case (IdentityC 5#) of
  IdentityC x -> case ex of
    IdentityC y -> do
      print (I# x)
      print y
      print (maybeInt# 12 increment# (Maybe# (# 42# | #)))
      print (maybeInt# 27 increment# (Maybe# (# | (# #) #)))

newtype Identity :: forall (r :: RuntimeRep). TYPE r -> TYPE r where
  IdentityC :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Identity a

newtype Maybe# :: forall (r :: RuntimeRep).
    TYPE r -> TYPE (SumRep '[r, TupleRep '[]]) where
  Maybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# a | (# #) #) -> Maybe# a

maybeInt# :: a -> (Int# -> a) -> Maybe# Int# -> a
maybeInt# def _ (Maybe# (# | (# #) #)) = def
maybeInt# _   f (Maybe# (# i | #)) = f i

increment# :: Int# -> Int
increment# i = I# (i +# 1#)

ex :: Identity Bool
ex = IdentityC True
