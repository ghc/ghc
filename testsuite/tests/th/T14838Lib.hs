{-# LANGUAGE TemplateHaskell #-}
module T14838Lib where
import Language.Haskell.TH

qIncompleteCase :: Q [Dec]
qIncompleteCase = [d|
  incompleteCase :: Bool -> ()
  incompleteCase b = case b of
    True -> () |]

qIncompleteFunction :: Q [Dec]
qIncompleteFunction =[d|
  incompleteFunction :: Bool -> ()
  incompleteFunction True = () |]
