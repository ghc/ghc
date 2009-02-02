{-# LANGUAGE TypeOperators #-}

-- Trac #2993

module T2993 where

foo b a = a <$> b . b

