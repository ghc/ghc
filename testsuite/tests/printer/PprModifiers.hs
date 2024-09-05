{-# LANGUAGE Modifiers #-}

module PprModifiers where

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
data D
  -- comment
  = %A5 {- comment -} %A6 D { d %A7 {- comment -} %A8 :: Int }

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
data D' where
  -- comment
  %A5 {- comment -} %A6 D' :: { d' %A7 {- comment -} %A8 :: Int } -> D'

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
class C a

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
instance C D

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
default (Int)

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
foreign import ccall "test" someImport :: Int

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
foreign export ccall someImport :: Int

%A1 {- comment -} %A2
-- comment
%A3 {- comment -} %A4
sigDecl :: ()

%A1 {- comment -} %A2 patBind = ()

(%A1 {- comment -} %A2 pat) = ()
  where x = \(%A1 {- comment -} %A2 y) -> y

f :: a %A1 {- comment -} %A2 -> b
