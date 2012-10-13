{-# LANGUAGE UndecidableInstances #-}
module Main where

class         L0 a where l0 :: a -> a
class L0 a => L1 a where l1 :: a -> a
class L1 a => L2 a where l2 :: a -> a


data Worksfine = Worksfine deriving Show
instance                                   L0 Worksfine where l0 = id
instance                                   L1 Worksfine where l1 = l2
instance {- undecidable -} L1 Worksfine => L2 Worksfine where l2 = l0


data WorksfineToo = WorksfineToo deriving Show
instance                                      L0 WorksfineToo where l0 = id
instance {- undecidable -} L2 WorksfineToo => L1 WorksfineToo where l1 = l2
instance {- undecidable -} L1 WorksfineToo => L2 WorksfineToo where l2 = id


-- l1 LoopsAtRuntime = <loop>
-- l2 LoopsAtRuntime = <loop>
data LoopsAtRuntime = LoopsAtRuntime deriving Show
instance                                        L0 LoopsAtRuntime where l0 = id
instance {- undecidable -} L2 LoopsAtRuntime => L1 LoopsAtRuntime where l1 = l2
instance {- undecidable -} L1 LoopsAtRuntime => L2 LoopsAtRuntime where l2 = l0

main = do
  print (l1 WorksfineToo)
  print (l2 WorksfineToo)
  print (l1 LoopsAtRuntime)
  print (l2 LoopsAtRuntime)
