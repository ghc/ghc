--!!! Some simple examples using arrays.

module ArrayEx where
import Array

-- Some applications, most taken from the Gentle Introduction ... -------------

timesTable :: Array (Int,Int) Int
timesTable  = array ((1,1),(10,10)) [ ((i,j), i*j) | i<-[1..10], j<-[1..10] ]

fibs n = a where a = array (0,n) ([ (0,1), (1,1) ] ++
                                  [ (i, a!(i-2) + a!(i-1)) | i <- [2..n] ])

wavefront n = a where a = array ((1,1),(n,n))
                             ([ ((1,j), 1) | j <- [1..n] ] ++
                              [ ((i,1), 1) | i <- [2..n] ] ++
                              [ ((i,j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                                           | i <- [2..n], j <- [2..n] ])

listwave n = [ [wf!(i,j) | j <- [1..n]] | i <- [1..n] ]
             where wf = wavefront n

eg1 :: Array Integer Integer
eg1  = array (1,100) ((1, 1) : [ (i, i * eg1!(i-1)) | i <- [2..100] ])

-------------------------------------------------------------------------------
