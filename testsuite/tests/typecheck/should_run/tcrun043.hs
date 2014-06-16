{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}

import GHC.Prim ( Constraint )

type Showish = Show

f :: (Showish a) => a -> String
f x = show x ++ show x


data T = T
data F = F

data GADT a where
    Tish :: GADT T
    Fish :: GADT F

type family Indexed a b :: Constraint
type instance Indexed T b = Show b
type instance Indexed F b = Num b

g :: (Indexed a b) => GADT a -> b -> Either String b
g Tish x = Left (show x)
g Fish x = Right (x + 1)


type TwoConstraints a = (Show a, Num a)

-- We'll NOINLINE h so that we test the code generation for
-- constraint tuples
{-# NOINLINE h #-}
h :: TwoConstraints a => a -> String
h x = show (x + 1)


main :: IO ()
main = do
    print $ f 9
    print $ f True

    print $ g Tish 10
    print $ g Tish False
    print $ g Fish 11
    print $ g Fish 12.0

    print $ h 13
    print $ h 14.0
