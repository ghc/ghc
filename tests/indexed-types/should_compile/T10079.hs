
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module T10079 where

import Control.Applicative
import Data.Coerce

broken :: Bizarre (->) w => w a b t -> ()
broken = getConst #. bazaar (Const #. const ())

class Profunctor p where
  (#.) :: Coercible c b => (b -> c) -> p a b -> p a c

instance Profunctor (->) where
  (#.) = (.)

class Bizarre p w | w -> p where 
  bazaar :: Applicative f => p a (f b) -> w a b t -> f t
