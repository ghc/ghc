{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -O #-}

module T9583 where

import Data.Binary      ( Binary(..) )
import Data.Data        ( Data )
import Data.Typeable    ( Typeable )  
import GHC.Generics     ( Generic )

data T = A
       | B
       | C T
       | D T T
       | E T T
  deriving (Data, Generic, Typeable)

instance Binary T
