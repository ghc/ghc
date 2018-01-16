
\section{Main}
 
 =======================================================================
  Test function: Runs the Linear System Solver (Bi-CGSTAB).

  Provides a variety of data sets and evaluation modes to excercise
  the Bi-CGSTAB iterative algorithm employing the Full Factorization
  preconditioning strategy.

  Written by Brian D. Moe (Summer 1990)
  Modified by Kamini Shenoi (Summer 1991)
  Converted to Haskell by Ian R McLelland and Cordelia V Hall (Nov 1992)
 ========================================================================


   Test may be executed by typing in a value of the form:

        test <eval_type> <data_type> <data_set> <convergence_criterion>

   where

      <eval_type> is one of:  none, bilu

                  none : no preconditioning.
                  bilu : use preconditioner.

      <data_type> is one of:   test_data, gcomp_data

        	    test_data  : Artificial and contrived test data.
                  gcomp_data : Linear system generated from actual
                               reservoir simulations.

      <data_set>  an integer.

                    If test_data is used, it indicates the size of the
                  linear system.  If gcomp_data is used, it indicates
                  which data set to use.

      <convergence_criterion> is one of: conv1, conv2

                  conv1 : (r,r) < 0.000001
                  conv2 : sqrt(r,r) < 0.0000004



   Evaluation Strategy

   none : no preconditioning.
   bilu : use the preconditioner.


   Data Type

   test_data  : a diagonally dominant simple system
   gcomp_data : a "real life" GCOMP linear system

   The data set means different things to different data types.

      test_data  :   Data set determines the size of the system.
                     There will be n^2 rows & columns of blocks.
      gcomp_data :   The data set indicates which set of files
                     to read for data.


  Convergence Criterion

  conv1 : (r,r) < 0.000001
  conv2 : sqrt(r,r) < 0.0000004
 
\begin{code}
import Matrix  -- matrix implementation
import Input   -- read gcomp data files
import Misc    -- for timing function and takeuntil
import System.Environment
\end{code}

AbsCg imports the actual linear system solver which uses a
simple conjugate gradient method.

Absmatlib imports the preconditioner.

\begin{code}

import AbsCg (solve_iters, Cg_state (..), show_state)
import Absmatlib 


 
 
conv1 (Cg_stateC x r p q c) =
    (norm r) < 0.000001
 
conv2 (Cg_stateC x r p q c) =
    sqrt (norm r) < 0.0000004

\end{code}



main resps 
 = [ReadChan stdin,            -- to get input parameters
    ReadFile ... ,             -- all of the data files
    AppendChan stdout result]  -- the final result
    where 
    result 
     = case (resps !! 0) of
        (Str str) -> let (process, data, set, conv) = parseInp str
                         file1 = getFile1 set (tail resps)
                         file2a = getFile2a set (tail resps)
                         ...
                         file6 = getFile6 set (tail resps)
                      in
                      test process data set file1 ... file6a conv
        _ -> error "bad read on input"

 ToDo:
 1) write parseInp (give read a signature which does the parse)
 2) write getFile functions
 3) alter test, test' so that instead of propagating set,
    the files are passed to the appropriate function. For 
    example, pass file5 to soln_vect, but just pass set to
    a_easy.
 4) change soln_vect, gmat, etc. so that they get the file, not 
    set
 5) change readmtx, etc. so that x becomes "file"; no call
    to doread, etc. needed.


\begin{code}
main = do
    m <- getArgs
    let n = read (head m)
    putStr (result n)
          where
          result n = test bilu test_data n conv2

test_data = hard_data 

noscale a b = (a,b)
noprecond a b = b

test process data' set conv =
    run (test' process data' set conv,
         process ++ "/" ++ data' ++ "/" ++ (show set))

test' process data' set conv
   = header ++ output ++ "\n"
     where
        output = 
              concat (map (show_state soln) iterations) 
        iterations =
            takeuntil conv (take maxiters all_iterations)
        all_iterations =
            solve_iters scale precond a b
        (scale,precond)
           = case process of 
              "bilu" -> (doscale,doprecond numwells)
              "none" -> (noscale,noprecond)
              _ ->  error usage
        (a,soln,b,numwells)
           = case data' of
              "easy_data" -> (a_easy set, x1 set, mvmult a soln, 0)
              "hard_data" -> (a_hard set, x1 set, mvmult a soln, 0)
              "gcomp_data" -> (gmat set, soln_vect set, rhside set, wells set)  
              _      -> error usage
        maxiters = 50
        usage =
           "Usage: test (bilu|none) (test_data|gcomp_data)" ++
           " num (conv1|conv2)" 


header :: [Char]
header =
    "\nIteration   norm (x-soln)   norm r  \n" ++
    "=========   =============   ======= \n"



easy_data = "easy_data"
hard_data = "hard_data"
gcomp_data   = "gcomp_data"
bilu      = "bilu"
none      = "none"

\end{code}





               
               

