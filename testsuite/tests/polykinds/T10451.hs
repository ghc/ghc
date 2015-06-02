{-# LANGUAGE ConstraintKinds #-}

module T10451 where

type T a = ( Eq a, Eq a, Eq a, Eq a
           , Eq a, Eq a, Eq a, Eq a
           , Eq a, Eq a, Eq a, Eq a
           , Eq a, Eq a, Eq a, Eq a
           , Eq a, Eq a, Eq a, Eq a )

