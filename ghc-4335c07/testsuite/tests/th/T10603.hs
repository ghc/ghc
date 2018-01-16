{-# LANGUAGE TemplateHaskell #-}

module T10603 where

main = print $ $([| case Just 'a' of Just a -> Just ((\x -> x) a) |])
