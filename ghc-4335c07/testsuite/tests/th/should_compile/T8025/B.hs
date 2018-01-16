-- B.hs
{-# LANGUAGE TemplateHaskell #-}
module B where
import A
x = $(a)
