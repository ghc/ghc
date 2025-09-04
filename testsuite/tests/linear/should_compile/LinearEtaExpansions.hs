module LinearEtaExpansions where

-- Keep the imports to a bare minimum in order to make it easier
-- to run this test on the stage 1 compiler.
import Prelude ( Maybe(..), ($), map )

-------------------------------------------------------------------------------
-- Test case 1
--
-- This is reminiscent of T26225: we must deal with the case in which the
-- EXPECTED type is a filled metavariable in tc_sub_type_deep.

data N a = MkN a
fmapN :: (a -> b) -> N a -> N b
fmapN f (MkN a) = MkN (f a)
pureN :: a -> N a
pureN = MkN


data A1 = A1
data A2 = A2
data B = B { fld1 :: A1, fld2 :: A2 }


testFn :: N (A2 -> B)
testFn = fmapN B (pureN A1)
  -- fails at B: expected A2 -> B, actual A2 %1 -> B

-- Test case 1b, same issue, from the linear types proposal
data Identity a = Identity { runIdentity :: a }
foo :: Identity (a -> b) -> a -> b
foo = runIdentity

bar = foo (Identity Just)

-------------------------------------------------------------------------------
-- Test case 2

-- Here we had
--
--   Actual: a %1 -> b
--   Expected :: cat[tau] a b
--
-- And we needed to be able to see that we wanted to perform eta expansion
-- so that it works in the end when we later figure out that cat[tau] := (->).
--
-- So this is about the FunTy/AppTy case in tc_sub_type_deep.

infixr 9 `dot1`
class Category1 cat where
  dot1 :: cat b c -> cat a b -> cat a c
instance Category1 (->) where
  f `dot1` g = \ x -> f (g x)

class Applicative1 f where
  pure1 :: a -> f a

const1 :: forall a b. a -> b -> a
const1 a _ = a

newtype Kleisli1 m a b = Kleisli1 { runKleisli1 :: a -> m b }

instance Applicative1 m => Applicative1 (Kleisli1 m a) where
  pure1 = Kleisli1 `dot1` const1 `dot1` pure1

--- Test 2b, same problem, from the linear types proposal

class Category2 arr where
  dot2 :: b `arr` c -> a `arr` b -> a `arr` c

instance Category2 (->) where
  f `dot2` g = \x -> f (g x)

baz = Just `dot2` Just

-------------------------------------------------------------------------------
-- Test case 3

-- This shows we need to be careful in qlUnify, to avoid
-- unifying multiplicities when in fact the later logic in checkResultTy
-- would do a (more lenient) subtype check.

class Arrow arr where
  first :: arr a b -> arr (a,c) (b,c)
instance Arrow (->) where
  first f (a,c) = (f a, c)

go1 y [] = ([], y)
go1 y (z : zs) = first (y :) (go1 z zs)

go2 :: forall a. a -> [a] -> ([a], a)
go2 y [] = ([], y)
go2 y (z : zs) = first (y :) (go2 z zs)


-------------------------------------------------------------------------------
-- Test case 4

-- This test checks that we are careful to look for linear arrows that are
-- nested under other function arrows, for example:
--
--   ty_actual   = B -> (A %1 -> K)
--   ty_expected = beta[tau] -> alpha[tau]
--
-- where perhaps (but perhaps not) alpha := A -> K
--
-- We need to recur and eta-expand.

data A = A
data C = C A
data K = K A A

newtype D a = MkD a

infixl 4 <$$>
(<$$>) :: (a -> b) -> D a -> D b
f <$$> MkD a = MkD (f a)


(<**>) :: D (a -> b) -> D a -> D b
MkD f <**> MkD a = MkD (f a)
infixl 4 <**>

quux :: D K
quux = mkK <$$> MkD (C A) <**> MkD A
  where
    mkK (C a) = K a


----------------
-- Test case 5

-- This shows that we need to do deep instantiation in the Infer mode
-- of checkResultTy. Very similar to T26225b.

data XY = X | Y

data O = O

data I = I1 O | I2 O

test1 xy =
  case xy of
    X -> (I1 :: O -> I)
    Y -> I2

test2 xy =
  case xy of
    X -> I1
    Y -> (I2 :: O -> I)


----------------
-- Test case 6

-- This shows we need to be careful in qlUnify in cases such as
--
-- qlUnify
--   A %1 -> B
--   kappa[tau:qlinst]
--
-- We should not simply go ahead and unify, because we actually
-- want to eta-expand; in this example in the end kappa will unify with A -> B.

data U = U
data V = V U U

withFun :: Maybe (U -> V) -> ()
withFun _ = ()

test6 :: ()
test6 = withFun just_pap
  where
    just_pap = Just $ V U

----------------
-- Test case 7
--
-- This shows we must be careful in the AppTy case of qlUnify.
--
-- If we have
--
-- arr[tau:qlinst] a b ~ (a %1 -> b)
--
-- we must not go ahead and unify arr ~ FUN One,
-- as we want to eta-expand during deep subsumption.

data P = P
data M = M P

readsN :: [(M, ())]
readsN = map ( first $ M ) []
