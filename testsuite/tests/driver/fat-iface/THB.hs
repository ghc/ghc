{-# LANGUAGE TemplateHaskell #-}
module THB where
import THA
import Control.Monad (when)



$(do
  -- Need to verify in both defining module and usage module"
  when (show (StrictConstructor1 123 True 4567) /= "StrictConstructor1 123 True 4567") $ error "TH validation error"
  when (show (StrictConstructor2 123 True 4567) /= "StrictConstructor2 123 True 4567") $ error "TH validation error"
  when (show (StrictConstructor3 123 True 4567) /= "StrictConstructor3 123 True 4567") $ error "TH validation error"
  when (show (classMethod 'z') /= "True") $ error "TH validation error"
  when (show (classMethod 'a') /= "False") $ error "TH validation error"
  th_a)
