{-# LANGUAGE PartialTypeSignatures #-}

module ExtraConstraintsWildcardInExpressionSignature where

foo x y = ((==) :: _ => _) x y
