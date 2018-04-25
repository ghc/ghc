{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Crash where

import Data.Proxy (Proxy(..))
import Data.Type.Equality (type (==))
import GHC.Exts
import GHC.Generics

data Dict :: Constraint -> * where
  Dict :: a => Dict a

infixr 0 -->

type family (args :: [*]) --> (ret :: *) :: *
  where
    '[]           --> ret = ret
    (arg ': args) --> ret = arg -> (args --> ret)

type family AllArguments (func :: *) :: [*]
  where
    AllArguments (arg -> func) = arg ': AllArguments func
    AllArguments ret           = '[]

type family FinalReturn (func :: *) :: *
  where
    FinalReturn (arg -> func) = FinalReturn func
    FinalReturn ret           = ret

type IsFullFunction f
  = (AllArguments f --> FinalReturn f) ~ f

type family SConstructor (struct :: *) :: *
  where
    SConstructor struct = GPrependFields (Rep struct ()) '[] --> struct

type family GPrependFields (gstruct :: *) (tail :: [*]) :: [*]
  where
    GPrependFields (M1  i t f p) tail = GPrependFields (f p) tail
    GPrependFields (K1    i c p) tail = c ': tail
    GPrependFields ((:*:) f g p) tail =
      GPrependFields (f p) (GPrependFields (g p) tail)

class (fields1 --> (fields2 --> r)) ~ (fields --> r)
      => AppendFields fields1 fields2 fields r
         | fields1 fields2 -> fields

instance AppendFields '[] fields fields r

instance AppendFields fields1 fields2 fields r
         => AppendFields (f ': fields1) fields2 (f ': fields) r

class Generic struct
      => GoodConstructor (struct :: *)
  where
    goodConstructor :: Proxy struct
                    -> Dict ( IsFullFunction (SConstructor struct)
                            , FinalReturn (SConstructor struct) ~ struct
                            )

instance ( Generic struct
         , GoodConstructorEq (SConstructor struct == struct)
                             (SConstructor struct)
                             struct
         ) => GoodConstructor struct
  where
    goodConstructor _ =
        goodConstructorEq (Proxy :: Proxy (SConstructor struct == struct))
                          (Proxy :: Proxy (SConstructor struct))
                          (Proxy :: Proxy struct)
    {-# INLINE goodConstructor #-}

class GoodConstructorEq (isEqual :: Bool) (ctor :: *) (struct :: *)
  where
    goodConstructorEq :: Proxy isEqual
                      -> Proxy ctor
                      -> Proxy struct
                      -> Dict ( IsFullFunction ctor
                              , FinalReturn ctor ~ struct
                              )

instance ( FinalReturn struct ~ struct
         , AllArguments struct ~ '[]
         ) => GoodConstructorEq True struct struct
  where
    goodConstructorEq _ _ _ = Dict
    {-# INLINE goodConstructorEq #-}

instance GoodConstructorEq (ctor == struct) ctor struct
         => GoodConstructorEq False (arg -> ctor) struct
  where
    goodConstructorEq _ _ _ =
      case goodConstructorEq (Proxy :: Proxy (ctor == struct))
                             (Proxy :: Proxy ctor)
                             (Proxy :: Proxy struct)
      of
        Dict -> Dict
    {-# INLINE goodConstructorEq #-}

data Foo = Foo
  { _01 :: Int
  , _02 :: Int
  , _03 :: Int
  , _04 :: Int
  , _05 :: Int
  , _06 :: Int
  , _07 :: Int
  , _08 :: Int
  , _09 :: Int
  , _10 :: Int
  , _11 :: Int
  , _12 :: Int
  , _13 :: Int
  , _14 :: Int
  , _15 :: Int
  , _16 :: Int
  }
  deriving (Generic)

crash :: () -> Int
crash p1 = x + y
  where
    p2 = p1  -- This indirection is required to trigger the problem.
    x = fst $ case goodConstructor (Proxy :: Proxy Foo) of
      Dict -> (0, p2)
    y = fst $ case goodConstructor (Proxy :: Proxy Foo) of
      Dict -> (0, p2)
{-# INLINE crash #-}  -- Even 'INLINABLE' is not enough to trigger the problem.
