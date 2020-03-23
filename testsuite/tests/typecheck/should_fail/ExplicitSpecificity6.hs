{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExplicitSpecificity6 where

class C a where

instance forall {a} {b}. C (Either a b) where
  {-# SPECIALISE instance forall {a}. C (Either a Int) #-}


