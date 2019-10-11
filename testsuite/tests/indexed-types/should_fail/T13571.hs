{-# LANGUAGE TypeFamilies #-}

module T13571 where

type family F a = r where
    F a = a
