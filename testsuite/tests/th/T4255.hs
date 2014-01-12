
{-# LANGUAGE TemplateHaskell #-}
module T4255 where

f x = $([| x |])
