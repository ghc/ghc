{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T21348 where

import qualified Data.Map as M
import Data.Kind (Type)

newtype Parser a = Parser {
      runParser :: () -> (a -> Int) -> Int
    } deriving (Functor)

instance Applicative Parser where
    pure a = Parser $ \_path ks -> ks a
    {-# INLINE pure #-}
    (<*>) m e = Parser $ \path ks -> let ks' a = runParser (a <$> e) path ks
                                     in runParser m path ks'
    {-# INLINE (<*>) #-}

data Value = Object (M.Map String Value) | Unused

class FromJSON a where
    parseJSON :: Value -> Parser a
    _unused :: a -> a

instance FromJSON Bool where
    parseJSON _ = pure False
    _unused = id

data Pa a = MkPa Bool a

class RecordFromJSON f where
    recordParseJSON :: () -> M.Map String Value -> Parser (Pa f)

class RecordFromJSON2 f where
    recordParseJSON2 :: M.Map String Value -> Parser f

instance (RecordFromJSON2 b) => RecordFromJSON b where
    recordParseJSON _ obj = MkPa <$> pure False
                                 <*> recordParseJSON2 obj
    {-# INLINE recordParseJSON #-}

instance (FromJSON a) => RecordFromJSON2 a  where
    recordParseJSON2 obj = pure () *> (id <$> (id <$> parseJSON (obj M.! "x")))
    {-# INLINE recordParseJSON2 #-}

data Rec :: [Type] -> Type where
  RNil :: Rec '[]
  RCons :: Field r -> Rec rs -> Rec (r ': rs)

data Rec2 :: [Type] -> Type where
  RNil2 :: Rec2 '[]
  RCons2 :: DocField r -> Rec2 rs -> Rec2 (r ': rs)

data Field x = Field x

newtype DocField x = DocField (Field x)

instance FromJSON (Rec '[]) where
  parseJSON _ = undefined
  _unused = id

instance (FromJSON t, FromJSON (Rec rs)) => FromJSON (Rec (t ': rs)) where
  parseJSON v = rebuild <$> parseJSON v <*> parseJSON v
    where rebuild m rest = Field m `RCons` rest
  _unused = id

instance (RMap rs, FromJSON (Rec rs)) => FromJSON (Rec2 rs) where
  parseJSON v = rmap DocField <$> parseJSON v
  _unused = id

class RMap rs where
  rmap :: (forall x. Field x -> DocField x) -> Rec rs -> Rec2 rs

instance RMap '[] where
  rmap _ RNil = RNil2
  {-# INLINE rmap #-}

instance RMap xs => RMap (x ': xs) where
  rmap f (x `RCons` xs) = f x `RCons2` rmap f xs
  {-# INLINE rmap #-}

g :: RecordFromJSON a => Value -> Parser (Pa a)
g (Object r) = recordParseJSON () r
g Unused = undefined

bug :: Value -> Parser (Pa (Rec2 '[Bool, Bool, Bool, Bool]))
bug = g
