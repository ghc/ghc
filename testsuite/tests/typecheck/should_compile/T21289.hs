{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}
module T21289 where

type family F (a :: Bool)
type instance F True = Int
type instance F False = Int

type family G (a :: Bool)
type instance G True = Int
type instance G False = Bool

data Rec a = MkR { konst :: F a
                 , change :: G a }

ch :: Rec True -> Rec False
ch r = r { change = False }
