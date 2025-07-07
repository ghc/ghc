{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T26176 where

-- In a previous version of GHC this test led to
-- a very misleading error message

import GHC.TypeNats

data T = X
type family FA t where FA X = 1
type family FB t where FB X = 2

foo :: forall p (t :: T). p t
foo = undefined
 where
  a :: SNat 5
  a = b

  b :: SNat (FA t)
  b = undefined

  c :: SNat 3
  c = d

  d :: SNat (FB t)
  d = undefined

  x = bar @t

bar :: FA t <= FB t => p t
bar = undefined
