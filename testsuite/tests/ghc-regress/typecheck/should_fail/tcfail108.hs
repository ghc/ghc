-- The instance decl is illegal without -fallow-undecidable-instances

module ShouldFail where

data Rec f = In (f (Rec f))

instance Eq (f (Rec f)) => Eq (Rec f) where
         (In x) == (In y) = x == y

