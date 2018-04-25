{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module T8913 where

class GCat f where
   gcat :: f p -> Int

cat :: (GCat (MyRep a), MyGeneric a) => a -> Int
cat x = gcat (from x)

class MyGeneric a where
   type MyRep a :: * -> *
   from :: a -> (MyRep a) p
