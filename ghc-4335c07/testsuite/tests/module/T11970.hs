{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module T11970(B(recSel), Foo((--.->)), C(C,P,x,Q, B, recSel)) where

pattern D = Nothing

newtype B = B { recSel :: Int }

class Foo a where
  type (--.->) a

newtype C = C Int

pattern P x = C x

pattern Q{x} = C x
