-- !!! Class decl clashes with type decl
module M where
type C = Int
class C a where f :: a
