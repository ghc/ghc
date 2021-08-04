{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  GenericSpec
-- Copyright   :  (C) 2011-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
--
-- Tests for generically derived 'Distributive' instances.
----------------------------------------------------------------------------
module GenericsSpec (main, spec) where

import Test.Hspec

#if __GLASGOW_HASKELL__ >= 702
import           Data.Distributive (Distributive(..))
import           Data.Distributive.Generic (genericCollect, genericDistribute)

# if __GLASGOW_HASKELL__ >= 706
import           Generics.Deriving.Base hiding (Rep)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll1)
# endif
#endif

main :: IO ()
main = hspec spec

spec :: Spec
#if __GLASGOW_HASKELL__ < 702
spec = return ()
#else
spec = do
  describe "Id" $
    it "distribute idExample = idExample" $
      distribute idExample `shouldBe` idExample
  describe "Stream" $
    it "runId (shead (stail (distribute streamExample))) = 1" $
      runId (shead (stail (distribute streamExample))) `shouldBe` 1
  describe "PolyRec" $
    it "runId (plast (runId (pinit (distribute polyRecExample)))) = 1" $
      runId (plast (runId (pinit (distribute polyRecExample)))) `shouldBe` 1

newtype Id a = Id { runId :: a }
  deriving (Eq, Functor, Show)
instance Distributive Id where
  collect    = genericCollect
  distribute = genericDistribute

idExample :: Id (Id Int)
idExample = Id (Id 42)

data Stream a = (:>) { shead :: a, stail :: Stream a }
  deriving Functor
instance Distributive Stream where
  collect    = genericCollect
  distribute = genericDistribute

streamExample :: Id (Stream Int)
streamExample = Id $ let s = 0 :> fmap (+1) s in s

data PolyRec a = PolyRec { pinit :: Id (PolyRec a), plast :: a }
  deriving Functor
instance Distributive PolyRec where
  collect    = genericCollect
  distribute = genericDistribute

polyRecExample :: Id (PolyRec Int)
polyRecExample = Id $ let p = PolyRec (Id $ fmap (+1) p) 0 in p

# if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 Id
deriving instance Generic1 Stream
deriving instance Generic1 PolyRec
# else
$(Generics.deriveAll1 ''Id)
$(Generics.deriveAll1 ''Stream)
$(Generics.deriveAll1 ''PolyRec)
# endif
#endif
