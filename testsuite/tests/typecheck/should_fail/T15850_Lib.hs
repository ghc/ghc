{-# LANGUAGE RoleAnnotations #-}
module T15850_Lib where

type role LibIdentity nominal
newtype LibIdentity a = LibIdentity a
