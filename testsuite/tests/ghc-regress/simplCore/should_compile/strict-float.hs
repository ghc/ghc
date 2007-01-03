
-- Test from Kirsten Chevalier
-- Tests the "strict bindings getting floated to top" bug in the floater

module B (bernoulli) where

powers = [2..] : powers 

neg_powers = map (zip (iterate id True)) powers

bernoulli = head powers 
  where powers = head neg_powers 

