{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module T19482 where

testF :: forall a (b :: [a]). ()
testF = ()

-- Both Test1 and Test2 led to "no skolem info" errors in GHC 9.0

-- Test1
class BugClass k where
    bugList :: ()
instance BugClass ((s : sx) :: [r]) where
    bugList = testF @r @s

-- Test2
foo :: forall r (s::r). ()
foo = testF @r @s
