module VRegister where

import Ix
import Register
import Cell
import Trans


-- Begin Signature -------------------------------------------------

{- 
  Given two register sets, VRegister facilitates the mapping
  between them.  See the P6 model for an example of register renaming
  with VRegister
-}

data Virtual r v 
   = Real r
   | Virtual v (Maybe r)
        deriving (Read,Show,Ord)

isReal		:: Virtual a b -> Bool
isVirtual	:: Virtual a b -> Bool

{-instance (Ix a, Ix b,Bounded a,Bounded b) => Ix (Virtual a b)-}
{-instance (Enum a,Bounded a,Enum b,Bounded b) => Enum (Virtual a b)-}
{-instance (Bounded a,Bounded b) => Bounded (Virtual a b)-}
{-instance (Register a,Register b) => Register (Virtual a b)-}
{-instance Eq Virtual r v-}
{-instance Register Int-}




-- End Signature -------------------------------------------------

instance Register Int where
        pc = error "Int does not have a PC"
        specpc = error "Int does not have a SPECPC"
        isspecpc _ = False
        ispc _ = False



virtual x = Virtual x Nothing

isReal (Real _) = True
isReal  _       = False

isVirtual (Virtual _ _) = True
isVirtual  _       = False

-----------------------Instances--------------------------------

instance (Eq a,Eq b) => Eq (Virtual a b) where
  (Virtual x _) == (Virtual y _) = x == y 
  (Real x) == (Real y) = x == y 
  _ == _  = False
  (Virtual x _) /= (Virtual y _) = x /= y 
  (Real x) /= (Real y) = x /= y 
  _ /= _  = True

instance (Register a,Register b) => Register (Virtual a b) where
  readOnly (Virtual v x) = readOnly v
  readOnly (Real v) = readOnly v
  ispc (Virtual n (Just x)) = ispc x
  ispc (Real x) = ispc x
  isspecpc (Virtual n (Just x)) = isspecpc x
  isspecpc (Real x) = isspecpc x
  pc = Real pc
  specpc = Real specpc


instance (Ix a, Ix b,Bounded a,Bounded b) => Ix (Virtual a b) where
--  range :: (a,a) -> [a]
    range (Real r, Real r')       = map Real $ range (r,r')
    range (Virtual r _, Virtual r' _) = map (\x -> Virtual x Nothing) $ range (r,r')
    range (Real r, Virtual r' _) = range1 ++ range2
         where range1 = map Real $ range (r,maxBound)
               range2 = map (\x -> Virtual x Nothing) $ range (minBound,r')
    range (Virtual r _, Real r') = []
--  index :: (a,a) -> a -> Int
    index (Virtual r _,Virtual r' _) (Virtual r'' _) = index (r,r') r''
    index (Real r,Real r') (Real r'') = index (r,r') r''
    index (Virtual r _,Virtual r' _) _ = error "index: Real Reg out of range"
    index (Real r,Real r') _ = error "index: Virtual Reg out of range"
    index (Virtual r _,Real r') _ = error "index: Virtual Reg out of range"
    index (Real r,Virtual r' _) (Real x) = index(r,maxBound) x
    index (Real r,Virtual r' _) (Virtual x _) = index(minBound,r') x
--  inRange :: (a,a) -> a -> Bool
    inRange (Virtual x _,Virtual y _) (Virtual z _) = inRange (x,y) z
    inRange (Real x,Real y) (Real z) = inRange (x,y) z
    inRange (Virtual x _,Virtual y _) _ = False
    inRange (Real x,Real y) _ = False
    inRange (Virtual x _,Real y) _ = False
    inRange (Real y,_) (Real r) = inRange (y,maxBound) r
    inRange (_,Virtual y _) (Virtual r _) = inRange (minBound,y) r


instance (Enum a,Bounded a,Enum b,Bounded b) => Enum (Virtual a b) where
--  toEnum :: Int -> a
--  toEnum = Virtual . toEnum
    toEnum x = error "Virtual.toEnum"

--  fromEnum :: a -> Int
--  fromEnum (Virtual x) = fromEnum x
    fromEnum x = error "Virtual.fromEnum"

--  enumFrom :: a -> [a]
    enumFrom (Virtual x _) = map virtual (enumFrom x)
    enumFrom (Real x) = map Real (enumFrom x) ++ enumFrom (virtual minBound)

--  enumFromThen :: a -> a -> [a]
    enumFromThen  _ _ = error "Virtual.enumFromThen"

--  enumFromTo :: a -> a -> [a]
    enumFromTo (Virtual x _) (Virtual y _)  
              = map virtual (enumFromTo x y)
    enumFromTo (Real x) (Real y)  
              = map Real (enumFromTo x y)
    enumFromTo (Real x) (Virtual y z)  
              = enumFrom (Real x) ++ 
                enumFromTo (Virtual minBound Nothing) (Virtual y z)
--  enumFromThenTo :: a -> a -> a -> [a]
    enumFromThenTo _ _ _ = error "Virtual.enumFromThenTo"

instance (Bounded a,Bounded b) => Bounded (Virtual a b) where
-- minBound :: a
   minBound = Real minBound
-- maxBound :: a
   maxBound = Virtual maxBound Nothing

