{-# LANGUAGE PolyKinds, DeriveFunctor, RankNTypes #-}

module T10561 where

-- Ultimately this should "Just Work",
-- but in GHC 7.10 it gave a Lint failure
-- For now (HEAD, Jun 2015) it gives a kind error message,
-- which is better than a crash

newtype Compose f g a = Compose (f (g a)) deriving Functor

{-
instance forall   (f_ant :: k_ans -> *)
                  (g_anu :: * -> k_ans).
           (Functor f_ant, Functor g_anu) =>
             Functor (Compose f_ant g_anu) where
    fmap f_anv (T10561.Compose a1_anw)
      = Compose (fmap (fmap f_anv) a1_anw)
-}
