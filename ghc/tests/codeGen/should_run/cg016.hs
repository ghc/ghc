-- !!! tests calls of `error' (that make calls of `error'...)
--
main = error ("1st call to error\n"++(
       error ("2nd call to error\n"++(
       error ("3rd call to error\n"++(
       error ("4th call to error\n"++(
       error ("5th call to error\n"++(
       error ("6th call to error"
       )))))))))))
