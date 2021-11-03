{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Pat where

import Language.Haskell.Syntax.Extension ( XRec )
import Data.Kind

type role Pat nominal
type role MatchPat nominal
data Pat (i :: Type)
type LPat i = XRec i (Pat i)
data MatchPat i
type LMatchPat i = XRec i (MatchPat i)
