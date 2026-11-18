{-# LANGUAGE DataKinds, ExplicitForAll, RankNTypes #-}
module ForallMatchabilityEmpty where
import GHC.Exts (U)
bad :: forall . @U Int -> Int
bad x = x
