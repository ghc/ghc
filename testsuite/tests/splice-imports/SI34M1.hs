{-# LANGUAGE ImplicitStagePersistence #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module SI34M1 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data T = MkT Int
  deriving Show

instance Lift T where
  lift (MkT n) = [| MkT $(lift n) |]
  liftTyped (MkT n) = [|| MkT $$(liftTyped n) ||]
