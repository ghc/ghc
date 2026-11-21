{-# LANGUAGE GHC2024, ExperimentalDependentTypes, TemplateHaskell #-}
module Main where

import Data.Type.Equality
import Language.Haskell.TH
import qualified Provider as P

$(do
  declarations <- [d|
    data Quoted a b where
      Quoted :: forall a b. (a ~ Bool, b ~ Char) =>
                foreach (flag :: Bool) -> P.Payload flag -> Quoted a b
    |]
  let isRetainedContext (ForallMatchC _ con) = isRetainedContext con
      isRetainedContext (ForallC _ [_, _] (RetainedGadtC [_] _ _)) = True
      isRetainedContext _ = False
  case declarations of
    [DataD _ _ _ _ [con] _] | isRetainedContext con -> pure declarations
    _ -> fail (show declarations))

readBack :: Quoted a b -> (a :~: Bool, b :~: Char, String)
readBack (Quoted True n) = (Refl, Refl, show n)
readBack (Quoted False c) = (Refl, Refl, [c])

main :: IO ()
main = case (readBack (Quoted True 71 :: Quoted Bool Char),
             readBack (Quoted False 'r' :: Quoted Bool Char)) of
  ((Refl, Refl, x), (Refl, Refl, y)) -> print (x, y)
