{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10100 where

data Zero
data Succ a

class Add a b ab | a b -> ab, a ab -> b
instance Add Zero b b
instance (Add a b ab) => Add (Succ a) b (Succ ab)
