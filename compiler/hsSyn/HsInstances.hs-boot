{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HsInstances where

import Data.Data hiding ( Fixity )
import HsExtension ( DataIdLR )
import HsBinds
import HsDecls

instance (DataIdLR p p) => Data (VectDecl p)
instance (DataIdLR pL pR) => Data (HsLocalBindsLR pL pR)
instance (DataIdLR p p) => Data (HsDecl p)
instance (DataIdLR p p) => Data (HsGroup p)
instance (DataIdLR pL pL) => Data (NHsValBindsLR pL)
instance (DataIdLR p p,Data pats,Data rhs) => Data (FamEqn p pats rhs)
