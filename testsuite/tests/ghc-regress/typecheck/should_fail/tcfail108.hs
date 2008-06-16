-- The instance decl is illegal without UndecidableInstances

module ShouldFail where

data Rec f = In (f (Rec f))

instance Eq (f (Rec f)) => Eq (Rec f) where
         (In x) == (In y) = x == y

