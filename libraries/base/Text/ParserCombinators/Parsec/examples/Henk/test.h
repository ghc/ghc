-- type declarations
data List: * -> *
    = { Nil: \/a. List a
	  ; Cons : \/a. a -> List a -> List a
	  };
							  
data Maybe : * -> * -> *
	= { Left:  \/a,b. a -> Maybe a b
      ; Right:  \/a,b. b -> Maybe a b 
      }
                              
-- value declarations
let id : \/a. a->a 
	= /\a. \x:a. x;
	
letrec { map: \/a,b. a -> b -> List a -> List b 
	= /\a,b. 
      \f: a->b,xs:List a.
	      case (xs) of 
	      { Nil =>Nil
	      ; Cons => \x:a, xx: List a. 
					Cons (f x) (map a b f xx)
	      } 
	      at {a:*}
	  };
	  
letrec { reverse: \/a. List a -> List a
	      = /\a.\xs:List a.
	         case xs of
	         { Nil => Nil
	         ; Cons x,xx => append (reverse xx) (Cons x Nil)
	         }
	         at {a:*}
	   };
	   
letrec { append: \/a. |~|_dummy:List a.|~|_:List a.List a
           = /\a.\xs:List a, ys:List a.
              case xs of
              { Nil  => ys
              ; Cons x:a,xx: List a => Cons x (append xx ys)
              }
              at {a:*}
       }

	  
 
 
