{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts,  ScopedTypeVariables #-}

module T4494 where

type family H s
type family F v

bar :: (forall t. Maybe t -> a) -> H a -> Int
bar _ = error "urk"

call :: F Bool -> Int
call x = bar (\_ -> x) (undefined :: H (F Bool))

{-
  [W] H (F Bool) ~ H alpha
  [W] alpha ~ F Bool
-->
  F Bool  ~ fuv0
  H fuv0  ~ fuv1
  H alpha ~ fuv2

  fuv1 ~ fuv2
  alpha ~ fuv0

flatten
~~~~~~~
fuv0 := alpha
fuv1 := fuv2

alpha := F Bool
-}
