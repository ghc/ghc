{-# OPTIONS -fglasgow-exts #-}

-- This bizarre program failed because TcSimplify built a loop of
-- dictionaries, due to an obscure bug in the way in which superclasses
-- were added

module Main where

import Data.List
import Data.FiniteMap

class (Ord oid) => Object o oid | o -> oid where

data Access oid

class (Object o oid) => SecurityModel model o oid | model -> o

class (SecurityModel model o oid) => SecurityPolicy policy model o oid where
    checkAccess :: policy -> model -> Access oid -> Bool
    checkAccess _ _ _ = True
    checkModel :: policy -> model -> Bool
    checkModel _ _ = True

------------------------------------------------------------
-- The Linux instance
------------------------------------------------------------

type LinuxObjectId = Either [String] String

data LinuxObject = File [String] deriving (Eq, Show)

instance Object LinuxObject LinuxObjectId

data LinuxSecurityModel =
    LinuxSecurityModel { lsmObjectSet   :: FiniteMap LinuxObjectId LinuxObject }


-- Now defined in Data.FiniteMap, don't think this affects the bug:
-- instance (Show a, Show b) => Show (FiniteMap a b) where
--     show fm = show (fmToList fm)

instance Show LinuxSecurityModel where
    show lsm = "LSM:" ++ "\tObjects:  " ++ show (lsmObjectSet lsm)

instance SecurityModel LinuxSecurityModel LinuxObject LinuxObjectId

data LinuxSecurityPolicy = LinuxSecurityPolicy
instance SecurityPolicy LinuxSecurityPolicy LinuxSecurityModel LinuxObject LinuxObjectId

model :: FiniteMap LinuxObjectId LinuxObject
model =  listToFM [ (Left [], File []), (Left ["home"], File ["home"]) ]


-- 	works
-- model :: (LinuxObjectId, LinuxObject)
-- model =  (Left [], File [])

main :: IO ()
main = do { putStrLn (show model) }
