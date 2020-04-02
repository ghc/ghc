{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import T17151a

main :: IO ()
main = do
  let ys :: Array P Int Int
      ys = computeS (makeArray D 1 (const 5))
      applyStencil ::
           (Source P ix Int, Load D ix Int)
        => Stencil ix Int Int
        -> Array P ix Int
        -> Array P ix Int
      applyStencil s = computeS . mapStencil s
  print (applyStencil (makeConvolutionStencilFromKernel ys) ys `unsafeIndex` 0)
  print (applyStencil (makeConvolutionStencilFromKernel ys) ys `unsafeIndex` 0)
