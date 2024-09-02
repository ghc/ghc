{-# LANGUAGE TemplateHaskell #-}
import Control.Exception
import Language.Haskell.TH

-- Generate a very large number of declarations
generateDecls :: Int -> Q [Dec]
generateDecls n = mapM (\i -> valD (varP (mkName ("x" ++ show i))) (normalB [| i |]) []) [1..n]

main :: IO ()
main = do
  $(generateDecls 1000000)
  print x1
