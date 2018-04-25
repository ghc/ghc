{-# LANGUAGE TypeFamilies #-}
-- Test for no type indices

module T2888 where

class C w where 
  data D:: * -> *
