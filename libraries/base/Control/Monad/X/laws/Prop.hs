module Prop where

data Prop a

infix 1 ===
(===) :: a -> a -> Prop a
(===) = error "==="

