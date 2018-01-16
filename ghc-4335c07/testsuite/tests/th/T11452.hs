{-# LANGUAGE RankNTypes, TemplateHaskell #-}

module T11452 where

impred :: (forall a. a -> a) -> ()
impred = $$( [|| \_ -> () ||] )
