{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, UndecidableInstances #-}

module T17744A where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString


data Parser t r where
   Failure :: Parser t r
   Result :: ByteString -> r -> Parser t r
   Delay :: Parser t r -> (ByteString -> Parser t r) -> Parser t r

instance Functor (Parser t) where
   fmap f (Result s r) = Result s (f r)
   fmap f p = apply (fmap f) p

instance Applicative (Parser t) where
   pure = return

instance Monad (Parser t) where
   return = Result mempty
   Result s r >>= f = feed s (f r)
   p >>= f = apply (>>= f) p

data LeftBiasedLocal

instance Alternative (Parser LeftBiasedLocal)

instance (Alternative (Parser t)) => LookAheadParsing (Parser t)

class Alternative m => Parsing m where
  unexpected ::  m a

instance (Alternative (Parser t)) => Parsing (Parser t) where
   unexpected = undefined

class Parsing m => LookAheadParsing m

class LookAheadParsing m => InputParsing m where
   takex :: m ByteString

class (Parsing m, InputParsing m) => InputCharParsing m

feed :: ByteString -> Parser t r -> Parser t r
feed s (Result s' r) = Result (mappend s' s) r
feed s (Delay _ f) = f s

completeResults :: Parser t r -> Int
completeResults (Result _ _) = 1
completeResults _ = 0


apply :: (Parser t r -> Parser t r') -> Parser t r -> Parser t r'
apply _ Failure = Failure
apply g (Delay e f) = Delay (g e) (g . f)
apply f p = Delay (f p) (\s-> f $ feed s p)


instance (Alternative (Parser t )) =>
         InputParsing (Parser t ) where
   takex =  p
     where p = Delay Failure f
           f s = if ByteString.null s then p else
                     case ByteString.splitAt 1 s of
                        (first, rest) -> Result rest first


instance (LookAheadParsing (Parser t)) => InputCharParsing (Parser t) where

data Format m n = Format {
   parse :: m ByteString,
   serialize :: n ()
   }

mytake :: (InputParsing m, Alternative n) =>  Format m n
mytake = Format{
   parse = takex,
   serialize = pure ()
   }

mytake2 :: (InputCharParsing m, Alternative n) => Format m n
mytake2 = mytake

satisfy_ :: (Parsing m, Monad m) => Format m n -> Format m n
satisfy_ f = Format{
   parse = parse f >>= pure,
   serialize = undefined
   }

