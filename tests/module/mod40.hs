-- !!! Cyclic class hierarchy
module M where
class C2 a => C1 a where f :: a
class C1 a => C2 a where g :: a
