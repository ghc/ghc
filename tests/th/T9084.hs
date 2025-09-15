{-# LANGUAGE TemplateHaskell #-}

module T9084 where

$([d|
   class C a where
     meth :: a -> a
     meth = undefined -- give a (silly) default
     {-# MINIMAL meth #-}
   |])
