{-# LANGUAGE TemplateHaskell #-}
module T11809 where

{- Test splicing in a data type with records -}

[d|
 data D a = MkD { unD :: a }

 someD = MkD "Hello"
 getD  = unD someD   -- unD should resolve to the record selector above!
 |]

getD' = unD someD    -- ditto here outside of the splice!
