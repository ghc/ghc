{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module T8913 where

import Data.Kind (Type)

class GCat f where
   gcat :: f p -> Int

cat :: (GCat (MyRep a), MyGeneric a) => a -> Int
cat x = gcat (from x)

class MyGeneric a where
   type MyRep a :: Type -> Type
   from :: a -> (MyRep a) p
