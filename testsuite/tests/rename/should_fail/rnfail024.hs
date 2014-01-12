module ShouldFail where

sig_without_a_defn :: a -> b

f :: a -> b
f = sig_without_a_defn
