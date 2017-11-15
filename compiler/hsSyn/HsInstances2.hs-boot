{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HsInstances2 where


import Data.Data hiding ( Fixity )
import HsExtension ( DataIdLR )
import HsDecls
import HsExpr
import HsTypes
import HsPat

instance (DataIdLR p p) => Data (HsExpr p)
instance (DataIdLR p p) => Data (HsTyVarBndr p)
instance (DataIdLR p p) => Data (HsType p)
instance (DataIdLR p p)         => Data (LHsQTyVars p)
instance (DataIdLR p p) => Data (HsImplicitBndrs p (LHsType p))
instance (DataIdLR p p) => Data (HsImplicitBndrs p (FamEqn p (HsTyPats p) (HsDataDefn p)))
instance (DataIdLR p p) => Data (HsImplicitBndrs p (FamEqn p (HsTyPats p) (LHsType p)))
instance (DataIdLR p p) => Data (HsWildCardBndrs p (LHsSigType p))
instance (DataIdLR p p) => Data (ConDeclField p)

instance (DataIdLR p p) => Data (HsSplice p)
instance (DataIdLR p p) => Data (MatchGroup p (LHsExpr p))

instance (DataIdLR p p) => Data (Pat p)

instance (DataIdLR p p) => Data (GRHSs      p (LHsExpr p))
