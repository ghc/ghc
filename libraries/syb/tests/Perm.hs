{-# OPTIONS -fglasgow-exts #-}

module Perm (tests) where

{-

This module illustrates permutation phrases.
Disclaimer: this is a perhaps naive, certainly undebugged example.

-}

import Test.Tasty.HUnit

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad
import Data.Generics

---------------------------------------------------------------------------
-- We want to read terms of type T3 regardless of the order T1 and T2.
---------------------------------------------------------------------------

data T1 = T1       deriving (Show, Eq, Typeable, Data)
data T2 = T2       deriving (Show, Eq, Typeable, Data)
data T3 = T3 T1 T2 deriving (Show, Eq, Typeable, Data)


---------------------------------------------------------------------------
-- A silly monad that we use to read lists of constructor strings.
---------------------------------------------------------------------------

-- Type constructor
newtype ReadT a = ReadT { unReadT :: [String] -> Maybe ([String],a) }



-- Run a computation
runReadT x y = case unReadT x y of
                 Just ([],y) -> Just y
                 _           -> Nothing

-- Read one string
readT :: ReadT String
readT =  ReadT (\x -> if null x
                        then Nothing
                        else Just (tail x, head x)
               )

instance Functor ReadT where
  fmap  = liftM

instance Applicative ReadT where
  pure  = return
  (<*>) = ap

instance Alternative ReadT where
  (<|>) = mplus
  empty = mzero

-- ReadT is a monad!
instance Monad ReadT where
  return x = ReadT (\y -> Just (y,x))
  c >>= f  = ReadT (\x -> case unReadT c x of
                            Nothing -> Nothing
                            Just (x', a) -> unReadT (f a) x'
                   )

-- ReadT also accommodates mzero and mplus!
instance MonadPlus ReadT where
  mzero = ReadT (const Nothing)
  f `mplus` g = ReadT (\x -> case unReadT f x of
                               Nothing -> unReadT g x
                               y -> y
                      )


---------------------------------------------------------------------------
-- A helper type to appeal to predicative type system.
---------------------------------------------------------------------------

newtype GenM = GenM { unGenM :: forall a. Data a => a -> ReadT a }


---------------------------------------------------------------------------
-- The function that reads and copes with all permutations.
---------------------------------------------------------------------------

buildT :: forall a. Data a => ReadT a
buildT = result

 where
  result = do str <- readT
              con <- string2constr str
              ske <- return $ fromConstr con
              fs  <- return $ gmapQ buildT' ske
              perm [] fs ske

  -- Determine type of data to be constructed
  myType = myTypeOf result
    where
      myTypeOf :: forall a. ReadT a -> a
      myTypeOf =  undefined

  -- Turn string into constructor
  string2constr str = maybe mzero
                            return
                            (readConstr (dataTypeOf myType) str)

  -- Specialise buildT per kid type
  buildT' :: forall a. Data a => a -> GenM
  buildT' (_::a) = GenM (const mzero `extM` const (buildT::ReadT a))

  -- The permutation exploration function
  perm :: forall a. Data a => [GenM] -> [GenM] -> a -> ReadT a
  perm [] [] a = return a
  perm fs [] a = perm [] fs a
  perm fs (f:fs') a = (
                        do a' <- gmapMo (unGenM f) a
                           perm fs fs' a'
                      )
                        `mplus`
                      (
                        do guard (not (null fs'))
                           perm (f:fs) fs' a
                      )


---------------------------------------------------------------------------
-- The main function for testing
---------------------------------------------------------------------------

tests =
     ( runReadT buildT ["T1"] :: Maybe T1           -- should parse fine
   , ( runReadT buildT ["T2"] :: Maybe T2           -- should parse fine
   , ( runReadT buildT ["T3","T1","T2"] :: Maybe T3 -- should parse fine
   , ( runReadT buildT ["T3","T2","T1"] :: Maybe T3 -- should parse fine
   , ( runReadT buildT ["T3","T2","T2"] :: Maybe T3 -- should fail
   ))))) @=? output

output = (Just T1,(Just T2,(Just (T3 T1 T2),(Just (T3 T1 T2),Nothing))))
