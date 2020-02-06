{-# LANGUAGE GADTs, TypeOperators, PolyKinds, DataKinds,
             TypeFamilyDependencies, RankNTypes, LambdaCase, EmptyCase,
             UndecidableInstances #-}

module T13822 where

import Data.Kind

data KIND = STAR | KIND :> KIND

data Ty :: KIND -> Type where
  TInt   :: Ty STAR
  TBool  :: Ty STAR
  TMaybe :: Ty (STAR :> STAR)
  TApp   :: Ty (a :> b) -> (Ty a -> Ty b)

type family
  IK (k :: KIND) = (res :: Type) | res -> k where
  IK STAR   = Type
  IK (a:>b) = IK a -> IK b

type family
  I (t :: Ty k) = (res :: IK k) | res -> t where
  I TInt       = Int
  I TBool      = Bool
  I TMaybe     = Maybe
  I (TApp f a) = (I f) (I a)

data TyRep (k :: KIND) (t :: Ty k) where
  TyInt   :: TyRep STAR         TInt
  TyBool  :: TyRep STAR         TBool
  TyMaybe :: TyRep (STAR:>STAR) TMaybe
  TyApp   :: TyRep (a:>b) f -> TyRep a x -> TyRep b (TApp f x)

zero :: TyRep STAR a -> I a
zero = \case
  TyInt           -> 0
  TyBool          -> False
  TyApp TyMaybe _ -> Nothing


-- Inferred type:
--
-- int :: TyRep STAR TInt -> Int
int rep = zero rep :: Int

-- bool:: TyRep STAR TBool -> Bool
bool rep = zero rep :: Bool

-- Previously failed with:
--
-- v.hs:43:16: error:
--     • Couldn't match kind ‘k’ with ‘'STAR’
--       ‘k’ is a rigid type variable bound by
--         the inferred type of
--         maybeInt :: (I 'TInt ~ Int, I 'TMaybe ~ Maybe) =>
--                     TyRep 'STAR ('TApp 'TMaybe 'TInt) -> Maybe Int
--         at v.hs:25:3
--       When matching the kind of ‘'TMaybe’
--       Expected type: Maybe Int
--         Actual type: I ('TApp 'TMaybe 'TInt)
--     • In the expression: zero rep :: Maybe Int
--       In an equation for ‘maybeInt’: maybeInt rep = zero rep :: Maybe Int
--     • Relevant bindings include
--         rep :: TyRep 'STAR ('TApp 'TMaybe 'TInt) (bound at v.hs:43:10)
--         maybeInt :: TyRep 'STAR ('TApp 'TMaybe 'TInt) -> Maybe Int
--           (bound at v.hs:43:1)
-- Failed, modules loaded: none.
maybeInt rep = zero rep :: Maybe Int
