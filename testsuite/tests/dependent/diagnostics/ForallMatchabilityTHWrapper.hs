{-# LANGUAGE DataKinds, TemplateHaskell #-}
module ForallMatchabilityTHWrapper where
import GHC.Exts (U)
import qualified Language.Haskell.TH as TH
type Bad = $(pure (TH.ForallMatchT (TH.ConT ''U) (TH.ForallT [] [] (TH.ConT ''Int))))
