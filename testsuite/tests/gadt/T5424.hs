{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module W where

import T5424a

data D a where
     C1 :: D X
     C2 :: D Y

f :: D X -> Int
f C1 = 1
