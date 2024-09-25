{-# LANGUAGE DataKinds #-}
module T25125 where

import GHC.TypeNats

newtype NonEmptyText (n :: Nat) = NonEmptyText String

toT :: NonEmptyText 10 -> String
toT = undefined

fromT :: forall n. String -> NonEmptyText n
fromT t = undefined

baz = ()
  where
    validate :: forall n. (1 <= n) => NonEmptyText 10 -> (NonEmptyText n)
    validate n = fromT (toT (check n))


    -- Giving a type signature works
    --check :: forall n. (1 <= n) => NonEmptyText n -> AppM (NonEmptyText n)
    check = check2
    -- Eta expanding check works
    --check x = check2 x

    check2 :: forall n. (1 <= n) => NonEmptyText n -> (NonEmptyText n)
    check2 inputText = undefined
