{-# LANGUAGE GHC2024, ExperimentalDependentTypes, TemplateHaskell #-}
module Main where

import Data.Type.Equality
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (RetainedGadtArg(..))
import qualified Provider as P

$(do
  let parent = mkName "Copy"
      constructor = mkName "Copy"
      rename (AppT f a) = AppT (rename f) a
      rename (ConT n) | n == ''P.Stored = ConT parent
      rename ty = ty
      copy (ForallMatchC annotation con) = ForallMatchC annotation <$> copy con
      copy (ForallC bs ctx (RetainedGadtC [_] args result))
        | [_, _] <- ctx
        , [GadtIndexArg flag _, GadtValueArg _ _ (AppT _ (IndexVarT use))] <- args
        , flag == use =
            pure (ForallC bs ctx (RetainedGadtC [constructor] args (rename result)))
      copy con = fail (show con)
  original <- reify ''P.Stored
  case original of
    TyConI (DataD ctx _ bs result [con] ds) -> do
      con' <- copy con
      pure [DataD ctx parent bs result [con'] ds]
    _ -> fail (show original))

readBack :: Copy a b -> (a :~: Bool, b :~: Char, String)
readBack (Copy True n) = (Refl, Refl, show n)
readBack (Copy False c) = (Refl, Refl, [c])

main :: IO ()
main = case (readBack (Copy True 71 :: Copy Bool Char),
             readBack (Copy False 'r' :: Copy Bool Char)) of
  ((Refl, Refl, x), (Refl, Refl, y)) -> print (x, y)
