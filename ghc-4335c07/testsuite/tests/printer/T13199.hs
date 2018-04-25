{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}
module Bug where

class C a b c
data B a b = B { aa :: a, bb :: b }

-- Types requiring parens.
$([d| instance C (Maybe a) (Maybe b) c
    |])

-- ---------------------------------------------------------------------
-- Patterns requiring parens according to hsPatNeedsParens

-- SigPatIn. What about SigPatOut?
$([d| g (a :: (Int -> Int) -> Int) = True |])

-- ViewPat
$([d| h (id -> x) = True |])

-- PrefixCon with non-null args
$([d| f (Just (Just False)) = True |])

-- InfixCon for ConPatIn
$([d| i (B (a `B` c) d) = True |])

-- RecCon does not
$([d| j B { aa = a} = True |])


$([d| k = id @(Maybe Int) |])

$([d| l = case Just 'a' of Just a -> Just ((\x -> x) a) |])
