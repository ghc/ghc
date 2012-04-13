{-# LANGUAGE TemplateHaskell #-}
module T5700 where

import T5700a

data D = D

$(mkC ''D)
