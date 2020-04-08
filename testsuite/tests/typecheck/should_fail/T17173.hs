{-# LANGUAGE RankNTypes, TypeApplications #-}

module T17173 where

-- This now fails with eager instantation
foo = (let myId :: forall a. a -> a; myId x = x in myId) @Bool True
