package Koe
{
Id =\x -> /* multi-line
Comment_ */ x // the identity function
;
K = \x -> \y_ -> x

;fac = \n -> 
  case n of
    { n -> n
    ; n -> let { m = minus n 1 } in times n (fac m)
    }    
; class Hi extends Mondrian { x = 2}
}
