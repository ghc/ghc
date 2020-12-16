module T18730_A where

import Control.Monad (ap)
import Data.Word
import Data.Bits

newtype Gen a = MkGen
  { -- | Run the generator on a particular seed.
    -- If you just want to get a random value out, consider using 'generate'.
    unGen :: QCGen -> Int -> a
  }

instance Functor Gen where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

instance Applicative Gen where
  pure x =
    MkGen (\_ _ -> x)
  (<*>) = ap

instance Monad Gen where
  return = pure

  MkGen m >>= k =
    MkGen
      ( \r n ->
          case split r of
            (r1, r2) ->
              let MkGen m' = k (m r1 n)
               in m' r2 n
      )

  (>>) = (*>)

data QCGen = QCGen !Word64 !Word64

split :: QCGen -> (QCGen, QCGen)
split (QCGen seed gamma) =
    (QCGen seed'' gamma, QCGen seed' (mixGamma seed''))
  where
    seed'  = seed + gamma
    seed'' = seed' + gamma

-- This piece appears to be critical
mixGamma :: Word64 -> Word64
mixGamma z0 =
    if z0 >= 24
        then z0
        else z0 `xor` 0xaaaaaaaaaaaaaaaa
