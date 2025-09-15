{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}

module T10810 where

$([d| data Foo = (:!) |])
