{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Bug where

$([d| class C a where
        type F a
        type F a = a |])
