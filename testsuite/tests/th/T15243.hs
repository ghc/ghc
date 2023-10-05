{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
module T15243 where

data Unit = Unit

$([d| type F :: k -> k
      type family F a where
        F 'Unit = 'Unit
        F '(,)  = '(,)
        F '[]   = '[]
        F '(:)  = '(:)
    |])
