{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

-- See Trac #1251 and the comments
-- Note [Pruning dead case alternatives] in types/Unify.lhs

module Main( main ) where

data T a where MkT :: T Int

class C a where ic :: T a

instance C Int where ic = MkT

newtype A = MkA Int deriving( C )

-- axiom CoA : A ~ Int
-- Hence C Int ~ C A

-- instance C A where
--    ic :: T A
--    ic = MkT

icA = ic :: T A  -- There are no (non-bot) values of this type

main = print (icA `seq` "ok")



