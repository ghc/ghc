{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module Bug where

data family Sing (z :: k)

class SEq k where
  (%==) :: forall (a :: k) (b :: k). Sing a -> Sing b -> ()
  infix 4 %==

data Foo a b c d
  = A a b c d |
    B a b c d |
    C a b c d |
    D a b c d |
    E a b c d |
    F a b c d

data instance Sing (z_awDE :: Foo a b c d) where
    SA :: Sing a -> Sing b -> Sing c -> Sing d -> Sing ('A a b c d)
    SB :: Sing a -> Sing b -> Sing c -> Sing d -> Sing ('B a b c d)
    SC :: Sing a -> Sing b -> Sing c -> Sing d -> Sing ('C a b c d)
    SD :: Sing a -> Sing b -> Sing c -> Sing d -> Sing ('D a b c d)
    SE :: Sing a -> Sing b -> Sing c -> Sing d -> Sing ('E a b c d)
    SF :: Sing a -> Sing b -> Sing c -> Sing d -> Sing ('F a b c d)

instance (SEq a, SEq b, SEq c, SEq d) => SEq (Foo a b c d) where
        (%==) (SA _ _ _ _) (SA _ _ _ _) = ()
        (%==) (SA _ _ _ _) (SB _ _ _ _) = ()
        (%==) (SA _ _ _ _) (SC _ _ _ _) = ()
        (%==) (SA _ _ _ _) (SD _ _ _ _) = ()
        (%==) (SA _ _ _ _) (SE _ _ _ _) = ()
        (%==) (SA _ _ _ _) (SF _ _ _ _) = ()
        (%==) (SB _ _ _ _) (SA _ _ _ _) = ()
        (%==) (SB _ _ _ _) (SB _ _ _ _) = ()
        (%==) (SB _ _ _ _) (SC _ _ _ _) = ()
        (%==) (SB _ _ _ _) (SD _ _ _ _) = ()
        (%==) (SB _ _ _ _) (SE _ _ _ _) = ()
        (%==) (SB _ _ _ _) (SF _ _ _ _) = ()
        (%==) (SC _ _ _ _) (SA _ _ _ _) = ()
        (%==) (SC _ _ _ _) (SB _ _ _ _) = ()
        (%==) (SC _ _ _ _) (SC _ _ _ _) = ()
        (%==) (SC _ _ _ _) (SD _ _ _ _) = ()
        (%==) (SC _ _ _ _) (SE _ _ _ _) = ()
        (%==) (SC _ _ _ _) (SF _ _ _ _) = ()
        (%==) (SD _ _ _ _) (SA _ _ _ _) = ()
        (%==) (SD _ _ _ _) (SB _ _ _ _) = ()
        (%==) (SD _ _ _ _) (SC _ _ _ _) = ()
        (%==) (SD _ _ _ _) (SD _ _ _ _) = ()
        (%==) (SD _ _ _ _) (SE _ _ _ _) = ()
        (%==) (SD _ _ _ _) (SF _ _ _ _) = ()
        (%==) (SE _ _ _ _) (SA _ _ _ _) = ()
        (%==) (SE _ _ _ _) (SB _ _ _ _) = ()
        (%==) (SE _ _ _ _) (SC _ _ _ _) = ()
        (%==) (SE _ _ _ _) (SD _ _ _ _) = ()
        (%==) (SE _ _ _ _) (SE _ _ _ _) = ()
        (%==) (SE _ _ _ _) (SF _ _ _ _) = ()
        (%==) (SF _ _ _ _) (SA _ _ _ _) = ()
        (%==) (SF _ _ _ _) (SB _ _ _ _) = ()
        (%==) (SF _ _ _ _) (SC _ _ _ _) = ()
        (%==) (SF _ _ _ _) (SD _ _ _ _) = ()
        (%==) (SF _ _ _ _) (SE _ _ _ _) = ()
        (%==) (SF _ _ _ _) (SF _ _ _ _) = ()

