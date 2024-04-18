{-# LANGUAGE DataKinds #-}

module T8671bdefs where


data NotMonad t = NotMonad
  {unNotMonad :: t -> Int}

run :: t -> NotMonad t -> Int
run t (NotMonad f) = f t

(>>) :: NotMonad t -> NotMonad t -> NotMonad t
NotMonad l >> NotMonad r = NotMonad $ \t -> l t + r t

bool :: Int -> Int -> NotMonad Bool
bool t f = NotMonad (\b -> if b then t else f)
