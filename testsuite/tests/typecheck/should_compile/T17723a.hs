
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module T17723a (C(..)) where

import GHC.Exts

-- Essentially a stripped-down version of the `Lift` type class from
-- Language.Haskell.TH.Syntax. I have redefined it here both to strip away
-- inessential details and to ensure that this test case will not change if
-- the API of Lift were to change in the future.
class C (a :: TYPE (r :: RuntimeRep)) where
  m :: a -> ()
  default m :: (r ~ LiftedRep) => a -> ()
  m = const ()
