-- !!! Class decl can't use pattern bindings
module M where
class C a where
  x,y :: a
  (x,y) = error "foo"