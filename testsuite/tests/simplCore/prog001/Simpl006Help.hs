module Simpl006Help( forever ) where

forever c  = sequence_ (repeat c)
