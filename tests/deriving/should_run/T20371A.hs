module T20371A where

import Data.Data

data A = A deriving Data

data a :*: b = a :.: b deriving Data

