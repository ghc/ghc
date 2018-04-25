-- !!! Double irrefutable pattern (bug in Hugs98, 29/8/2001)
main = print (case (1,2) of ~(~(2,x)) -> x)
