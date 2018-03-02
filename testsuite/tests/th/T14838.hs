{-# LANGUAGE TemplateHaskell #-}
module T14838 where
import T14838Lib

$qIncompleteCase

$qIncompleteFunction

incompleteCase' :: Bool -> ()
incompleteCase' b = case b of
  True -> ()

incompleteFunction' :: Bool -> ()
incompleteFunction' True = ()
