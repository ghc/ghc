{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module T5792 where


data T = TT
type family Compare (m :: T) :: Ordering  
type instance Compare TT = Compare TT

type Compare' a = Compare a
