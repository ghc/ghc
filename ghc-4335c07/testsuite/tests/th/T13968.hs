{-# LANGUAGE TemplateHaskell #-}
module T13968 where

import Language.Haskell.TH

$(pure [ValD (VarP 'succ) (NormalB (ConE 'True)) []])
