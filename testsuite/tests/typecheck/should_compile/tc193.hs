
-- A newtype representation problem crashed GHC 6.4

module ShouldCompile where


newtype Signal a = Signal Symbol

newtype Symbol = Symbol (S Symbol)

data S s = Bool Bool

liftl :: Signal a -> Symbol
liftl (Signal a) = a 


