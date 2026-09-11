-- See #27802.
-- The whole idea is that if we have `(op d) x1 x2 ... xn` even if the dictionary
-- is known we have to account for the size resulting from applying `x1 .. xn`.

-- For this we construct functions containing the pattern `(op d) x .. xn`.
-- with: `d` being either a unary or regular class dictionary, and n either being
-- small enough for the function containing the pattern to inline, or large enough
-- that we surely shouldn't inline the containing function.

-- We test this by grepping -ddump-inlinings

module T27802
  -- We export the functions to avoid unconditional ininlining to fire.
  ( applyUnaryFew, applyRegularFew, applyUnaryMany, applyRegularMany
  , useUnaryFew, useRegularFew, useUnaryMany, useRegularMany
  ) where


class Unary a where
  m_unary :: (a -> b) -> b

-- The super dict makes it a non-unary type class.
class Eq a => Regular a where
  m_regular :: (a -> b) -> b

instance Unary Int where
  m_unary f = f 42

instance Regular Int where
  m_regular f = f 43

-- We instantiate `b` with these function types.
type Args4 = Int -> Int -> Int -> Int -> Int

type Args50 = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
              Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
              Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
              Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
              Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int ->
              Int

-- apply* functions are the ones containing the `op d x1 .. xn` pattern.
applyUnaryFew :: Unary a => (a -> Args4) -> Int
applyUnaryFew g = m_unary g 1 2 3 4

applyRegularFew :: Regular a => (a -> Args4) -> Int
applyRegularFew g = m_regular g 1 2 3 4

applyUnaryMany :: Unary a => (a -> Args50) -> Int
applyUnaryMany g = m_unary g
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      1 1 1 1 1 1 1 1 1 1

applyRegularMany :: Regular a => (a -> Args50) -> Int
applyRegularMany g = m_regular g
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      1 1 1 1 1 1 1 1 1 1

-- The call sites, where the dictionary is known. And we check if we should inline the
-- apply* functions.
useUnaryFew, useRegularFew :: (Int -> Args4) -> Int
useUnaryFew   g = applyUnaryFew g
useRegularFew g = applyRegularFew g

useUnaryMany, useRegularMany :: (Int -> Args50) -> Int
useUnaryMany   g = applyUnaryMany g
useRegularMany g = applyRegularMany g
