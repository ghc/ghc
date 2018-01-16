d=25637784375346753+0158409406114519728029864689069987389733-25637784375346753

cox(n)=foldr   (\x(y)->128*      y+x)0(n)
de(n)=un(\x->  (x`mod`128,x    `div`128),(                  ==0))n
coll=    un(\  zs->(          take(   35)zs,          drop(35)zs),(==""))
pe(n,m)x=s(p(  n`div`2,m)x)  `mod`m   --2323         -- john launchbury --
p(n,m)x=if(    n==0)then(1)  else(if(even(n))    then(pe(n,m)x)else(po(n,m)x))
po(n,  m)x=           (x*p(  n-1,m)   x)`mod`                 m
un(f,   p)n=   if(p(n))then  []else   (let(a,       b)=f(n)in(a:un(f,p)b))
(g,s)    =(\x  ->x,\x->x*x)  --v(f)   t*g+172

e=4998372704153806867349631861645896723396264061670520817438963311707989737197
n=6133011105483442903214719346445720362447680717496623906453276570566830154479

a=g
  (concat.map(map(chr.fromIntegral).de.p(d,n).read).lines)
main=interact
  (unlines.map(show.p(e,n).cox.map(fromIntegral.ord)).coll)
b=g
--p::(Integer,Integer)->Integer->Integer
default (Integer)
-- 1.3
ord = (fromEnum :: Char -> Int)
chr = (toEnum   :: Int  -> Char)
