{-# LANGUAGE TemplateHaskell #-}

module T23748 where

$([d| a ~ b = ()
      a @ b = ()
    |])