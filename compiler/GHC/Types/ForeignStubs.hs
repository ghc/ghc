-- | Foreign export stubs
{-# LANGUAGE DerivingVia #-}
module GHC.Types.ForeignStubs
   ( ForeignStubs (..)
   , CHeader(..)
   , CStub(..)
   , appendStubC
   )
where

import GHC.Utils.Outputable
import Data.Monoid
import Data.Semigroup
import Data.Coerce

newtype CStub = CStub { getCStub :: SDoc }

emptyCStub :: CStub
emptyCStub = CStub empty

instance Monoid CStub where
  mempty = emptyCStub
  mconcat = coerce vcat

instance Semigroup CStub where
  (<>) = coerce ($$)

newtype CHeader = CHeader { getCHeader :: SDoc }
  deriving (Monoid, Semigroup) via CStub

-- | Foreign export stubs
data ForeignStubs
  = NoStubs
      -- ^ We don't have any stubs
  | ForeignStubs CHeader CStub
      -- ^ There are some stubs. Parameters:
      --
      --  1) Header file prototypes for
      --     "foreign exported" functions
      --
      --  2) C stubs to use when calling
      --     "foreign exported" functions

appendStubC :: ForeignStubs -> CStub -> ForeignStubs
appendStubC NoStubs         c_code = ForeignStubs mempty c_code
appendStubC (ForeignStubs h c) c_code = ForeignStubs h (c `mappend` c_code)
