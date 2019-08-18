module TcRnTypes where

import Data.Data

data TcBinder

type TcBinderStack = [TcBinder]

instance Data TcBinder
