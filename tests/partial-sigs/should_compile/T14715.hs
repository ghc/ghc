{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T14715 (bench_mulPublic) where

data Cyc r
data CT zp r'q
class Reduce a b
type family LiftOf b

bench_mulPublic :: forall z zp zq . (z ~ LiftOf zq, _) => Cyc zp -> Cyc z -> IO (zp,zq)
bench_mulPublic pt sk = do
  ct :: CT zp (Cyc zq) <- encrypt sk pt
  undefined ct

encrypt :: forall z zp zq. Reduce z zq => Cyc z -> Cyc zp -> IO (CT zp (Cyc zq))
encrypt = undefined
