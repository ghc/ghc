package Prelude
{ import Foo

; class List extends Mondrian
; class Nil extends List
; class Cons extends List
    { head :: Mondrian
    ; tail :: List
    }
    
; map = \f -> \as ->
    case as of
      { Nil -> new Nil
      ; Cons{ a :: Mondrian; a = head; as :: List; as = tail } ->
          new Cons{ head = f a; tail = map f as }
      }
      
; class Boolean extends Mondrian
; class True extends Boolean
; class False extends Boolean
      
; cond = \b -> \t -> \e ->
    case b of
      { True -> t
      ; False -> e
      }
      
; fac = \n -> cond (n == 0) 1 (n * (fac (n - 1)))

; I :: a -> a
; I = \x -> x

; K :: a -> b -> a
; K = \x -> \y -> x

; S :: (a -> b -> c) -> (a -> b) -> (a -> c)
; S = \f -> \g -> \x -> f x (g x)

; Compose :: (b -> c) -> (a -> b) -> (a -> c)
; Compose = \f -> \g -> \x -> f (g x)

; Twice :: (a -> a) -> (a -> a)
; Twice = \f -> Compose f f

; main = Twice I 3
}