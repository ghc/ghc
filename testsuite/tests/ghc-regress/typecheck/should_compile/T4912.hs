{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-orphans #-}
module T4912 where

import T4912a


type OurData = TheirData

instance Foo TheirData where
     foo = id

instance Bar OurData where
     bar _ = "Ours"
