{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HsPat where

import Data.Data hiding (Fixity)
import Outputable
import HsExtension      ( SourceTextX, DataId, OutputableBndrId, GHC )

import qualified AST

type Pat  pass = AST.Pat  (GHC pass)
type LPat pass = AST.LPat (GHC pass)

instance (DataId p) => Data (Pat p)
instance (SourceTextX pass, OutputableBndrId pass) => Outputable (Pat pass)
