{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module T11401 where

import Data.Kind (Type)

newtype Value a = Value a
newtype CodeGen r a = CodeGen a

bind :: CodeGen r a -> (a -> CodeGen r b) -> CodeGen r b
bind (CodeGen a) k = k a

class
   (f ~ CalledFunction g, r ~ CallerResult g, g ~ CallerFunction f r) =>
       CallArgs f g r where
   type CalledFunction g :: Type
   type CallerResult g :: Type
   type CallerFunction f r :: Type
   call :: f -> g

instance CallArgs (IO a) (CodeGen r (Value a)) r where
   type CalledFunction (CodeGen r (Value a)) = IO a
   type CallerResult (CodeGen r (Value a)) = r
   type CallerFunction (IO a) r = CodeGen r (Value a)
   call = undefined

instance CallArgs b b' r => CallArgs (a -> b) (Value a -> b') r where
   type CalledFunction (Value a -> b') = a -> CalledFunction b'
   type CallerResult (Value a -> b') = CallerResult b'
   type CallerFunction (a -> b) r = Value a -> CallerFunction b r
   call = undefined

test :: IO a -> (a -> IO ()) -> CodeGen () (Value ())
test start stop  =  bind (call start) (call stop)
