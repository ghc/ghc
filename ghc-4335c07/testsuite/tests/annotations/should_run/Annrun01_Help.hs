{-# LANGUAGE TemplateHaskell #-}

module Annrun01_Help where

{-# ANN module "Module" #-}
{-# ANN module "Annotations" #-}
{-# ANN module (10 :: Int) #-}
{-# ANN module "Rock!!!!" #-}


{-# ANN foo "Hello" #-}
{-# ANN foo "World!" #-}
{-# ANN bar 'foo #-}
foo = "Never seen"

{-# ANN bar "Hello World Again!" #-}
{-# ANN bar (1 :: Int) #-}
{-# ANN bar 'bar #-}
bar = "Also never seen"

baz = "Especially never seen"


{-# ANN type Baz "Type Annotation" #-}
{-# ANN type Baz (Just True) #-}
{-# ANN type Baz ''Baz #-}
data Baz = Spqr