{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Test24771 where

data Foo
  =  Int     -- c1
       :*    -- c2
     String  -- c3
