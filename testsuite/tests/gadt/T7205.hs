{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module T7205 where

data Abs env g v where
  Abs :: g (a, env) h v -> Abs env (g (a, env) h v) (a -> v)

class Eval g env h v where
  eval :: env -> g env h v -> v

evalAbs :: Eval g2 (a2, env) h2 v2 
   	=> env 
   	-> Abs env (g2 (a2, env) h2 v2) (a2->v2) 
   	-> (a2->v2)
evalAbs env (Abs e) x 
  = eval (x, env) e    -- e :: g (a,env) h v
