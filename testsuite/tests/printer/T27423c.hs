{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
module T27423c where

-- Exact-printing regression test
-- Not every declaration there would pass renamer without errors
data G a where
  MkG1 :: a -> G a
  MkG2 :: (a -> G a)
  MkG3 :: forall a. a -> G a
  MkG4 :: forall a. (a -> G a)

data T where
  MkT1 :: forall a. a -> b -> T
  MkT2 :: (forall a. a -> b -> T)

data S a where
  MkS :: (forall a. S a)
  MkS2 :: forall. (forall a. S a)
  MkS3 :: forall. forall a. S a
  MkS4 :: forall a. forall. forall b. forall. forall. forall c. S a

data U a where
  MkU :: (Show a => U a)

data V a where
  MkV1 :: ((a -> V a))
  MkV2 :: forall a. (forall b. V (b, a))
  MkV3 :: (forall a. Show a => a -> V a)
  MkV4 :: forall a. ((forall b. (Show a => a -> (b -> V a))))

data W a b where
  MkW1 :: forall a -> forall b. (Int -> W a b)
  MkW2 :: forall a. (forall b -> (Bool -> W a b))

data P a where
  MkP1 :: Show a => (a -> P a)
  MkP2 :: forall a. Int -> (a -> P a)

-- Comments in and around the parentheses
data C a where
  MkC1 :: -- comment before the parenthesis
          (forall a. C a)
  MkC2 :: ( -- comment after the parenthesis
           forall a. C a)
  MkC3 :: (forall a. {- inline comment -} C a)
