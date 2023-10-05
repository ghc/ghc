{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module LinearTH4 where

$([d| data T where { MkT :: Int %1 -> T } |])
