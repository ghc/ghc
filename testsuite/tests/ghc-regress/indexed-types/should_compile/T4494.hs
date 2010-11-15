{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts,  ScopedTypeVariables #-}

module T4494 where

type family H s
type family F v

bar :: (forall t. Maybe t -> a) -> H a -> Int
bar = error "urk"

call :: F Bool -> Int
call x = bar (\_ -> x) (undefined :: H (F Bool))
