{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}

module T23738_fail_pun where

import Data.Function (id)
import GHC.TypeLits

vfun :: forall (a :: k) -> ()
vfun _ = ()

f1 :: Maybe a -> ()
f1 (Just @a a) = vfun a
  -- Punning error (term name in local env,
  --                type name in local env)

b :: ()
b = ()

f2 :: forall b. b -> ()
f2 @b _ = vfun b
  -- Punning error (term name in global env,
  --                type name in local env)

f3 :: forall b. b -> ()
f3 @b _ = vfun T23738_fail_pun.b
  -- No punning error, module qualification makes this unambiguous

f4 :: forall b. b -> ()
f4 @id _ = vfun id
  -- Punning error (imported term name in global env,
  --                type name in local env)

type (#) :: k -> k -> k
type a # b = a

f5 :: (a -> a -> a) -> ()
f5 (#) = vfun (#)
  -- Punning error (term name in local env,
  --                type name in global env)

f6 :: ()
f6 = vfun (+)
  -- Punning error (imported term name in global env,
  --                imported type name in global env)