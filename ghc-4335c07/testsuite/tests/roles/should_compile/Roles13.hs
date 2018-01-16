{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- tests axiom roles

module Roles13 where

newtype Age = MkAge Int
newtype Wrap a = MkWrap a

convert :: Wrap Age -> Int
convert (MkWrap (MkAge i)) = i
