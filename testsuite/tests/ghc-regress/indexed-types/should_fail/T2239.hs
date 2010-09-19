{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module T2239 where

data A = A
data B = B

class C a where c :: a -> String
instance C Bool where c _ = "Bool"
instance C Char where c _ = "Char"

-- via TFs
type family TF a
type instance TF A = Char
type instance TF B = Bool

tf :: forall a b. (b ~ TF a,C b) => a -> String
tf a = c (undefined:: b) 

tfa = tf A
tfb = tf B

-- via FDs
class FD a b | a -> b
instance FD A Char
instance FD B Bool

fd :: forall a b. (FD a b,C b) => a -> String
fd a = c (undefined:: b) 

fda = fd A
fdb = fd B


class MyEq a b | a->b, b->a
instance MyEq a a

simpleFD = id :: (forall b. MyEq b Bool => b->b) 

simpleTF = id :: (forall b. b~Bool => b->b)

-- These two both involve impredicative instantiation,
-- and should fail (in the same way)
complexFD = id :: (forall b. MyEq b Bool => b->b)
               -> (forall b. MyEq b Bool => b->b)

complexTF = id :: (forall b. b~Bool => b->b)
               -> (forall b. b~Bool => b->b)
