{-# OPTIONS -fglasgow-exts #-}

-- A bit more test code for the 2nd boilerplate paper.
-- These are downscaled versions of library functionality or real test cases.
-- We just wanted to typecheck the fragments as shown in the paper.

module Encode () where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM)
import Data.Generics

data Bit = Zero | One

------------------------------------------------------------------------------
-- Sec. 3.2

data2bits :: Data a => a -> [Bit]
data2bits t = encodeCon (dataTypeOf t) (toConstr t)
                ++ concat (gmapQ data2bits t)

-- The encoder for constructors
encodeCon :: DataType -> Constr -> [Bit]
encodeCon ty con = natToBin (max-1) (idx-1)
                  where
                    max = maxConstrIndex ty
                    idx = constrIndex con


natToBin :: Int -> Int -> [Bit]
natToBin = undefined

------------------------------------------------------------------------------
-- Sec. 3.3

data State   -- Abstract
initState  :: State
encodeCon' :: DataType -> Constr
           -> State -> (State, [Bit])

initState  = undefined
encodeCon' = undefined

data2bits' :: Data a => a -> [Bit]
data2bits' t = snd (show_bin t initState)

show_bin :: Data a => a -> State -> (State, [Bit])
show_bin t st = (st2, con_bits ++ args_bits)
   where
    (st1, con_bits)  = encodeCon' (dataTypeOf t)
                                  (toConstr t) st
    (st2, args_bits) = foldr do_arg (st1,[])
                             enc_args

    enc_args :: [State -> (State,[Bit])]
    enc_args = gmapQ show_bin t

    do_arg fn (st,bits) = (st', bits' ++ bits)
      where
        (st', bits') = fn st


------------------------------------------------------------------------------
-- Sec. 3.3 cont'd

data EncM a   -- The encoder monad
instance Functor EncM where
  fmap  = liftM
instance Applicative EncM where
  pure  = return
  (<*>) = ap
instance Monad EncM
 where
  return  = undefined
  c >>= f = undefined

runEnc  :: EncM () -> [Bit]
emitCon :: DataType -> Constr -> EncM ()

runEnc  = undefined
emitCon = undefined

data2bits'' :: Data a => a -> [Bit]
data2bits'' t = runEnc (emit t)

emit :: Data a => a -> EncM ()
emit t = do { emitCon (dataTypeOf t) (toConstr t) 
            ; sequence_ (gmapQ emit t) }
