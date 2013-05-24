{-# LANGUAGE TemplateHaskell #-}

module T7919A where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Language.Haskell.TH

-- on x86-64 with 7.6.3 it works with size 510 and fails with size 511
size = 512

dataName = mkName "Large"
stepName = mkName "step"

-- data Large = Large Int ... Int  -- generate 'size' fields, not strict
largeData =
  dataD
    (return [])
    (dataName)
    []
    [normalC dataName (replicate size (((,) <$> notStrict) `ap` [t| Int |]))]
    []

conE' :: Name -> [ExpQ] -> ExpQ
conE' n es = foldl appE (conE n) es

varName s i = mkName (s ++ show (((i - 1) `mod` size) + 1))

-- step (Large i1 ... in) =
--   let
--     j1 = i1 + j4
--     ...
--     jn = in + j(3n + 1 `mod` n)
--   in
--     Large j1 ... jn
largeStep =
  funD
    stepName
    [clause
      [conP dataName (map (\ i -> varP (varName "i" i)) [1..size])]
      (normalB
        (letE
          (map (\ i -> valD (varP (varName "j" i)) (normalB [| $(varE (varName "i" i)) + $(varE $ varName "i" (i * 3 + 1)) |]) []) [1..size])
          (tupE [conE' dataName (map (\ i -> varE (varName "j" i)) [1..size]), varE $ varName "j" 1])
        )
      )
      []
    ]

-- test = let step ... in runSteps step 100000 (Large 1 ... 1)
largeLet =
  valD
    (varP (mkName "test"))
    (normalB (letE [largeStep] [| runSteps $(varE stepName) 100000 $(conE' dataName (map (const $ litE $ integerL 1) [1..size]))|]))
    []

allDecs =
  sequence [largeData, largeLet]

runSteps :: (state -> (state, Int)) -> Int -> state -> [Int]
runSteps f n i | n <= 0    = []
               | otherwise = case f i of
                               (i', r) -> {-i' `deepseq`-} (r : runSteps f (n - 1) i')
-- could use deepseq here to avoid the space leak, but it's not necessary
