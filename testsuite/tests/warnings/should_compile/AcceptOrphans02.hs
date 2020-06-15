{-# OPTIONS_GHC -faccept-orphans deepseq #-}
module AcceptOrphans02 where

import Control.Monad.Trans.Identity
import Control.DeepSeq
import Data.Binary (Binary (..))
import Data.Monoid (Endo (..))

-- not reported
instance NFData1 f => NFData1 (IdentityT f) where
    liftRnf r (IdentityT x) = liftRnf r x

-- reported
instance Binary (IdentityT f a) where
    get = undefined
    put = undefined

-- not reported
instance NFData1 Endo where
    liftRnf _ (Endo f) = f `seq` ()
