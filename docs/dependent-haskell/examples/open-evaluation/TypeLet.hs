{-# LANGUAGE TypeOperators, DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
-- | Bind an intermediate result in a type synonym.
module TypeLet (F, Shadow, Reuse, Outer, Nested, CaseResult, roadmap) where
import Data.Type.Equality ((:~:)(Refl))

-- | Select a flag for each input type in the example.
type family G a where
  G Bool = 'True
  G Char = 'False

-- | Select a result when the two flags agree.
type family H (a :: Bool) (b :: Bool) where
  H 'True 'True = Ordering
  H 'False 'False = Char

-- | Name the result of G and pass it to H twice.
type F a = let b = G a in H b b

-- | Both uses of b refer to the local definition.
type Shadow (b :: Bool) = let b = 'True in H b b

-- | Retain the same unknown Boolean in both fields.
type Reuse (a :: Bool) = let b = a in '(b,b)

-- | The RHS refers to the outer b.
type Outer (b :: Bool) = let b = b in H b b

-- | A nested definition can refer to the enclosing local name.
type Nested (a :: Bool) = let b = a in let c = b in '(b,c)

-- | Match the outer flag and use the selected type on both sides of Either.
type CaseResult (flag :: Bool) =
  let flag = case flag of { True -> Bool; False -> Char } in Either flag flag

-- | Check the result of substituting G Bool in both positions.
roadmap :: F Bool :~: Ordering
roadmap = Refl
