module Test where

import Control.Applicative

topEntity :: [((),())]
topEntity = (,) <$> outport1 <*> outport2
  where
    (outport1, outResp1) = gpio (decodeReq 1 req)
    (outport2, outResp2) = gpio (decodeReq 2 req)
    ramResp              = ram  (decodeReq 0 req)

    req = core $ (<|>) <$> ramResp <*> ((<|>) <$> outResp1 <*> outResp2)

core :: [Maybe ()] -> [()]
core = fmap (maybe () id)
{-# NOINLINE core #-}

ram :: [()] -> [Maybe ()]
ram = fmap pure
{-# NOINLINE ram #-}

decodeReq :: Integer -> [()] -> [()]
decodeReq 0 = fmap (const ())
decodeReq 1 = id
decodeReq _ = fmap id
{-# NOINLINE decodeReq #-}

gpio :: [()] -> ([()],[Maybe ()])
gpio i = (i,pure <$> i)
{-# NOINLINE gpio #-}
