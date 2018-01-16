{-# LANGUAGE TemplateHaskell #-}

module T7445 where
import T7445a

moo = $(foo)
