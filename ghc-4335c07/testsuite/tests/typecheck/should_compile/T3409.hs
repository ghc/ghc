{-# LANGUAGE  ExistentialQuantification, TypeFamilies #-}

-- Tests a nasty case where 'exprType' or 'coreAltsType' can
-- return a type that mentions an out-of-scope type variable
-- because of a type synonym that discards one of its arguments
--
-- See Note [Existential variables and silly type synonyms] 
-- in CoreUtils

-- In GHC 6.10, both tests below (independently) give Lint errors

module T3409 where


--------------------------
-- Simpler version not involving type families

data T = forall a. T a (Funny a)
type Funny a = Bool

f :: T -> Bool
f (T x n) = n


--------------------------
-- Cut down version of the original report

newtype Size s = Size Int

data ArrayS d e = ArrayS d e

data Array1 e = forall s . Array1 (Size s) (ArrayS (Size s) e)
-- Array1 :: forall e s. Size s -> ArrayS (Size s) e -> Array1 e

copy ::  Int -> Array1 a -> Array1 a
copy _ (Array1 s a) = Array1 s $ (ArrayS s (bang a))
  -- Array1 s :: ArrayS (Size s) a -> Array1 a

  -- s :: Size s
  -- a :: ArrayS (Size s) a
  -- ArrayS :: Size s -> a -> ArrayS (Size s) a
  -- i :: AccessIx (ArrayS (Size s) a) = Ix s
  -- bang a :: AccessResult (ArrayS (Size s) a) = a

  -- ArrayS s (bang a) :: ArrayS (Size s) (AccessResult (ArrayS (Size s) a))

class Access a where
    type AccessResult a
    bang :: a -> AccessResult a

instance Access (ArrayS d a) where
    type AccessResult (ArrayS d a) = a
    bang = error "urk"
