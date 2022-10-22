{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DCo_Specialise ( rnStmts1 ) where

data RealWorld
newtype M a = M (RealWorld -> a)
fmapM :: (a -> b) -> M a -> M b
fmapM f (M k) = M (f . k)

data HsExpr
data SrcSpanAnnA

type family   Anno a
type instance Anno HsExpr = SrcSpanAnnA

type AnnoBody body0_ = ( Anno body0_ ~ SrcSpanAnnA )

rnStmts1 :: forall body1_ thing1_. AnnoBody body1_ => M (body1_, thing1_)
rnStmts1 = rnStmts2 @body1_ @thing1_

rnStmts2 :: forall body2_ thing2_. AnnoBody body2_ => M (body2_, thing2_)
rnStmts2 = rnStmts3 @(body2_, thing2_)

rnStmts3 :: M thing3_
rnStmts3 = fmapM snd $ rnStmts1 @HsExpr
