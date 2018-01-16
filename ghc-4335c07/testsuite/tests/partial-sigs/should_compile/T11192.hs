{-# LANGUAGE PartialTypeSignatures #-}

module T11192 where

fails :: a
fails =
   let go :: _
       go 0 a = a
   in go (0 :: Int) undefined

succeeds :: a
succeeds =
   let go :: _
       go _ a = a
   in go (0 :: Int) undefined
