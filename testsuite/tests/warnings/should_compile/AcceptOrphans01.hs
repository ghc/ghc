{-# OPTIONS_GHC -faccept-orphans deepseq -faccept-no-orphans #-}
module AcceptOrphans01 where

import Control.Monad.Trans.Identity (IdentityT (..))
import Control.DeepSeq (NFData1 (..))

instance NFData1 f => NFData1 (IdentityT f) where
    liftRnf r (IdentityT x) = liftRnf r x
