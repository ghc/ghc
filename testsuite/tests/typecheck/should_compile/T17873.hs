{-# LANGUAGE ImplicitParams #-}

module T17873 where

-- With GHC 8.8 this failed with a nonsense message
--    Couldn't match type ‘?x::Bool -> Bool’ with ‘Bool’
--      Expected type: Bool -> Bool -> Bool
--      Actual type: (?x::Bool -> Bool) => Bool -> Bool

-- With eager instantiation, it's fine.

getx :: (?x :: Bool -> Bool) => Bool -> Bool
getx = ?x

z3 = (let ?x = not in getx) False
