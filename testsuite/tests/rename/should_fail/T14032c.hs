{-# LANGUAGE GHC2021 #-}

module T14032c where


f $ a = f a

type f $ a = f a

infix 0 type $
infix 0 data $
