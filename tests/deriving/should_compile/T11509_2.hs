{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module T11509_2 where

import GHC.Exts (Constraint)

class C1 (a :: Constraint) where
class C2 where

deriving instance C1 C2
