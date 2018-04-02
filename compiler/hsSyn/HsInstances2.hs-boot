{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HsInstances2 where


import Data.Data hiding ( Fixity )
import HsExtension ( DataIdLR )
import HsExpr
import HsTypes
import HsPat

instance (DataIdLR p p) => Data (HsExpr p)
instance (DataIdLR p p) => Data (HsTyVarBndr p)
instance (DataIdLR p p) => Data (HsType p)
instance (DataIdLR p p)         => Data (LHsQTyVars p)
instance (DataIdLR p p, Data thing) => Data (HsImplicitBndrs p thing)
instance (DataIdLR p p, Data thing) => Data (HsWildCardBndrs p thing)
instance (DataIdLR p p) => Data (ConDeclField p)

instance (DataIdLR p p) => Data (HsSplice p)
instance (DataIdLR p p,Data body) => Data (MatchGroup p body)

instance (DataIdLR p p) => Data (Pat p)

instance (DataIdLR p p,Data body) => Data (GRHSs      p body)
