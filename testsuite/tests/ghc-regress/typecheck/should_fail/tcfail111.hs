-- Ambiguity check for instance declarations

module ShouldFail where

class Wob a 

instance Wob a => Wob Bool
