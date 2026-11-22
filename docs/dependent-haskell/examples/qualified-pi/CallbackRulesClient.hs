{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module CallbackRulesClient where

import CallbackRules
import Data.Proxy (Proxy(..))

-- | Select the callback input domain.
type family Input (b :: Bool) where
    Input 'True = Bool

-- | Select the callback result domain.
type family Result (b :: Bool) where
    Result 'True = Bool

-- | A type with the same representation as Bool.
newtype Wrapped = Wrapped Bool

-- | Use the same callback and input to construct and read the payload.
{-# NOINLINE check #-}
#if defined(NEWTYPE)
check :: (Wrapped -> Bool) -> Bool -> Int -> Int
#elif defined(WRONG)
check :: (Bool -> Bool) -> (Bool -> Bool) -> Bool -> Int -> Int
#elif defined(GIVEN)
check :: forall a r. (a ~ Bool, r ~ Bool)
      => Proxy a -> Proxy r -> (a -> r) -> Bool -> Int -> Int
#elif defined(BARE)
check :: (Bool -> Bool) -> Bool -> Int -> Int
#else
check :: (Input 'True -> Result 'True) -> Bool -> Int -> Int
#endif
#if defined(WRONG)
check f g x value = readKept f x (makeChoice g x value)
#elif defined(GIVEN)
check _ _ f x value = readKept f x (makeChoice f x value)
#else
check f x value = readKept f x (makeChoice f x value)
#endif

#if !defined(NEWTYPE) && !defined(WRONG)
-- | Check the branches that use and ignore the callback.
observation :: (Int, Int, Int, Int)
observation =
    ( call (error "forced callback") (error "forced index") 19
    , call not True 23
    , call id True (-1)
    , call id False (-1)
    )
  where
#if defined(GIVEN)
    call = check (Proxy @Bool) (Proxy @Bool)
#else
    call = check
#endif

-- | Print the results of the demand checks.
main :: IO ()
main = print observation
#endif
