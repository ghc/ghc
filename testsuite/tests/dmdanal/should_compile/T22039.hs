{-# LANGUAGE ExistentialQuantification #-}

module Bug where

import Control.Exception
import Data.Typeable
import Unsafe.Coerce

data Error
  = Error Int String
  | forall e . Exception e => SomeError Int e
  deriving (Typeable)

fromError :: Exception e => Error -> Maybe e
fromError e@(Error _ _)   = cast e
fromError (SomeError _ e) = cast e
-- {-# NOINLINE fromError #-}

instance Eq Error where
  Error i s == Error i' s' = i == i' && s == s'
  SomeError i e == SomeError i' e' = i == i' && show e == show e'
  _ == _ = False

instance Show Error where
  show _ = ""

instance Exception Error

-- newtype
data
  UniquenessError = UniquenessError [((String, String), Int)]
  deriving (Show, Eq)

instance Exception UniquenessError

test :: SomeException -> IO ()
test e = case fromError =<< fromException e :: Maybe UniquenessError of
  Just err -> print err
  _ -> pure ()

--
-- Smaller reproducer by sgraf
--

blarg :: (Int,Int) -> Int
blarg (x,y) = x+y
{-# NOINLINE blarg #-}

f :: Either Int Int -> Int
f Left{} = 0
f e = blarg (unsafeCoerce e)

blurg :: (Int -> Int) -> Int
blurg f = f 42
{-# NOINLINE blurg #-}

g :: Either Int Int -> Int
g Left{} = 0
g e = blurg (unsafeCoerce e)
