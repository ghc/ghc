-- Generation of BigTuples ... 

module Main where

-- import Other (bigtuple2, untuple2)

bigtuple2 = bigtuple1
untuple2  = untuple1

main = do
    input <- getContents
    putStr (unlines (map dolist (lines input)))

dolist l = untuple1 (bigtuple1 l) ++ ['\n'] ++ untuple2 (bigtuple2 l)

bigtuple1 (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a':b':c':d':e':f':g':h':i':j':k':l':m':n':rest)
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a',b',c',d',e',f',g',h',i',j',k',l',m',n') : bigtuple1 rest
bigtuple1 _ = []

untuple1 ((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a',b',c',d',e',f',g',h',i',j',k',l',m',n'):rest)
  = a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a':b':c':d':e':f':g':h':i':j':k':l':m':n': untuple1 rest
untuple1 []
  = []
