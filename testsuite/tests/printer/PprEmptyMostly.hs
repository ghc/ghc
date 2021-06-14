module EmptyMostly where
   { ;;;
     ;;x=let{;;;;;y=2;;z=3;;;;}in y;
   -- ;;;;
    class Foo a where {;;;;;;
  (--<>--) :: a -> a -> Int  ;
  infixl 5 --<>-- ;
  (--<>--) _ _ = 2 ; -- empty decl at the end.
};
-- ;;;;;;;;;;;;
-- foo = a where {;;;;;;;;;;;;;;;;;;;;;;;a=1;;;;;;;;}
-- ;;
   }
-- really trailing
