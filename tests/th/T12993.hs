{-# LANGUAGE TemplateHaskell #-}
module T12993 where
import T12993_Lib
f = $(q)
