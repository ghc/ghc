{-# LANGUAGE TemplateHaskell, PolyKinds, TypeFamilies #-}

module T9160 where

$( [d| class C (a :: k) where
          type F (a :: k) :: *
    |]
 )

