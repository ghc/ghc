{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
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

-- Actually these two do not involve impredicative instantiation,
-- so they now succeed
complexFD = id :: (forall b. MyEq b Bool => b->b)
               -> (forall c. MyEq c Bool => c->c)

complexTF = id :: (forall b. b~Bool => b->b)
               -> (forall c. c~Bool => c->c)

{- For example, here is how the subsumption check works for complexTF
   when type-checking the expression
      (id :: (forall b. b~Bool => b->b) -> (forall c. c~Bool => c->c))

   First, deeply skolemise the type sig, (level 3) before calling
   tcExpr on 'id'.  Then instantiate id's type:

      b~Bool |-3  alpha[3] -> alpha <= (forall c. c~Bool => c->c) -> b -> b

   Now decompose the ->

      b~Bool |-3  alpha[3] ~ b->b,  (forall c. c~Bool => c->c) <= a

   And this is perfectly soluble.  alpha is touchable; and c is instantiated.
-}