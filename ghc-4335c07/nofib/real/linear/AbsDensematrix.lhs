\section{AbsDensematrix}

This module imports the contents of Densematrix and exports some of the 
functions renaming many of them for use by Matrix. The original %include
statement(from Matrix) is left as a comment at the foot of this file.

\begin{code}
 
module AbsDensematrix(Block,Vec,bmult,bvecmult,vecbmult,vecdot,vecnorm,
                      vecouter,badd,bsub,vecadd,vecsub,bsize,vecsize,bneg,
                      bxpose,bident,vecneg,svecmult,mkblock,bswaprow,bswapcol,
                      bdroprow,bgetrow,bgetcol,bsubscript,vecsubscript,bupdate,
                      vecupdate,vechd,vectl,mergevecs,binverse,showblock,
                     showvec, mkvec,mkrvec,vecpart,update2,veclist,matlist) 
       where
 

import Densematrix 

type Block = Matrix
type Vec = Vector

bmult = mmult
bvecmult = matvecmult
vecbmult = vmmult
vecdot   = vdot
      
vecnorm = norm
vecouter = vouter      

badd = madd
bsub = msub
vecadd = vadd 
vecsub = vsub

bsize = msize
vecsize = vsize

bneg = mneg
bxpose = mxpose
bident = mident

vecneg = vneg
svecmult = svmult

mkblock = mkmat

bswaprow = swaprow
bswapcol = swapcol

bdroprow = droprow
bgetrow = getrow 
bgetcol = getcol

bsubscript = subscript
vecsubscript = vsubscript

bupdate = update
vecupdate = vupdate

vechd = vhd
vectl = vtl

mergevecs = mergevectors

binverse = minverse

showblock = showmatrix 
showvec = displayvector

\end{code}

The original include statement from Matrix :-

 %include "densematrix"
                         block/matrix  vec/vector
                         bmult/mmult   bvecmult/mvmult vecbmult/vmmult
                         vecdot/vdot   vecnorm/norm    vecouter/vouter
r                         badd/madd     bsub/msub  vecadd/vadd vecsub/vsub
                         bsize/msize   vecsize/vsize
                         bneg/mneg     bxpose/mxpose   bident/mident
                         vecneg/vneg   svecmult/svmult
                         mkblock/mkmat
                         -mkrmat -mkcmat -mkcvec
                         bswaprow/swaprow       bswapcol/swapcol
                         bdroprow/droprow       bgetrow/getrow bgetcol/getcol
                         bsubscript/subscript   vecsubscript/vsubscript
                         bupdate/update         vecupdate/vupdate
                         vechd/vhd              vectl/vtl
                         mergevecs/mergevectors
                         binverse/minverse
                         showblock/showmatrix showvec/showvector




