{-# LANGUAGE LinearTypes #-}

module PprModifiers where

-- MODS_TODO comments between and around modifiers sometimes get dropped. Not
-- sure if that's a problem with the modifiers or elsewhere.

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
data D
  -- lost
  -- MODS_TODO here, the %A5 becomes just A5:
  -- = %A5 {- lost -} %A6 D { d %A7 {- kept -} %A8 :: Int }
  = D { d %A7 {- kept -} %A8 :: Int }

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
data D' where
  -- lost
  %A5 {- lost -} %A6 D' :: { d' %A7 {- kept -} %A8 :: Int } -> D'

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
class C a

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
instance C D

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
default (Int)

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
foreign import ccall "test" someImport :: Int

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
foreign export ccall someImport :: Int

%A1 {- kept -} %A2
-- kept
%A3 {- lost -} %A4
sigDecl :: ()

%A1 {- kept -} %A2 patBind = ()

(%A1 {- kept -} %A2 pat) = ()
  where x = \(%A1 {- kept -} %A2 y) -> y

f :: a %A1 {- kept -} %A2 -> b

e = %A1 {- kept -} %A2 ()

t :: %A1 {- kept -} %A2 Int
