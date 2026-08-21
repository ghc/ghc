{-# OPTIONS_GHC -Wimplicit-field-strictness #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LazyFieldAnnotations #-}
module T16836b where

-- fully annotated declarations don't warn
data T a = MkT ~a !Bool
data R = MkR { x, y :: !Int, z :: ~Char }
data G a where
  MkG :: !Int -> ~Bool -> G a
data family F a
data instance F Int = MkF !Char

-- newtypes can't have annotations; exempt
newtype N = MkN Int

-- 'type data' can't have annotations; exempt
type data TD = MkTD Bool

-- no fields, nothing to annotate
data E
data Nullary = A | B
