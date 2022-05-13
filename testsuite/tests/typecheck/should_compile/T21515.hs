{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T21515 where

import Data.Kind

type family Code a :: [[Type]]

type IsWrappedType a x = Code a ~ '[ '[ x ] ]
type IsProductType a xs = Code a ~ '[ xs ]

type family Head (xs :: [a]) :: a where
  Head (x : xs) = x

type ProductCode a = Head (Code a)
type WrappedCode a = Head (Head (Code a))

wrappedTypeTo :: IsWrappedType a x => x -> a
wrappedTypeTo = undefined

to :: SOP (Code a) -> a
to = undefined

data SOP (xss :: [[a]])

type WrappedProduct a = (IsWrappedType a (WrappedCode a), IsProductType (WrappedCode a) (ProductCode (WrappedCode a)))

process :: (SOP '[ xs ] -> a) -> a
process = undefined

-- works with 8.10 and 9.0, fails with 9.2
test :: WrappedProduct a => a
test = process (wrappedTypeTo . to)
