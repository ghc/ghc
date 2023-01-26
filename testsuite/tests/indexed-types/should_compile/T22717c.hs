{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
module T22717c() where

import T22717d

data Private

-- This is an orphan instance
type instance F T = Private

-- But this is not
type instance F Private = Int
