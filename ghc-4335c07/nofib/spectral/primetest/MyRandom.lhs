\section{Random Numbers}
%$Log: MyRandom.lhs,v $
%Revision 1.1  2004/11/30 15:25:33  simonmar
%The compiler now detects local modules that overlap with package
%modules, so we must rename Random to avoid the clash.
%
%Revision 1.1  1996/01/08 20:04:20  partain
%Initial revision
%
%Revision 1.1  92/06/30  15:54:46  dlester
%Initial revision
%

A call to @randomInts@ with two @Int@ arguments generates a
pseudo-random sequence of @Int@'s.

> module MyRandom (randomInts) where

Use seeds s1 in 1..2147483562 and s2 in 1..2147483398 to generate an
infinite list of random Ints. (Algorithm due to Lennart Augustsson)

> randomInts :: Int -> Int -> [Int]
> randomInts s1 s2 = if 1 <= s1 && s1 <= 2147483562 then
>                    if 1 <= s2 && s2 <= 2147483398 then rands s1 s2
>                    else error "randomInts: Bad second seed."
>                    else error "randomInts: Bad first seed."

> rands :: Int -> Int -> [Int]
> rands s1 s2 =
>     let
>       k    = s1 `div` 53668
>       s1'  = 40014 * (s1 - k * 53668) - k * 12211
>       s1'' = if s1' < 0 then s1' + 2147483563 else s1'
>       k'   = s2 `div` 52774
>       s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
>       s2'' = if s2' < 0 then s2' + 2147483399 else s2'
>       z    = s1'' - s2''
>     in  if z < 1 then z + 2147483562 : rands s1'' s2''
>                  else z : rands s1'' s2''

