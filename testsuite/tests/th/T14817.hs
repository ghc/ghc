{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T14817 where

$([d| data family Foo :: *
      data instance Foo :: * |])
