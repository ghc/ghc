module Other (bigtuple2, untuple2) where

bigtuple2 (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a':b':c':d':e':f':g':h':i':j':k':l':m':n':rest)
  = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a',b',c',d',e',f',g',h',i',j',k',l',m',n') : bigtuple2 rest
bigtuple2 _ = []

untuple2 ((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a',b',c',d',e',f',g',h',i',j',k',l',m',n'):rest)
  = a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:a':b':c':d':e':f':g':h':i':j':k':l':m':n': untuple2 rest
untuple2 []
  = []

data ATuple a b c d e f g h i j k l m n o p = ATuple a b c d e f g h i j k l m n o p
