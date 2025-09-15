-- !!! Default decl for non-method
module M where
class C a where
  x :: a
  y = error "foo"
