{-# OPTIONS -XFunctionalDependencies -XUndecidableInstances -XFlexibleInstances #-} 

module T5684 where

class B a b | a -> b where
  op :: a -> b -> ()
  
class A a | -> a

instance A b => B Bool b

{- This used to be a bug in various versions of GHC <= 7.2.2
   The source of the problem is the kicking out of inert Solved goals back to 
   the worklist, which violated our invariant that when two constraints of the
   same class meet (workitem-inert) then the combination (Given-Wanted) is impossible.
   Actually it turns our that it is possible. The order in which the constraints appear
   below is important so we add two combinations to make sure that the testcase is 
   order-insensitive. -}

flop1 =  [ op False False    -- (3) Creates a functional dependency which kicks the solved out
                             --     back in the worklist. Next time round the solved workitem       
                             --     meets the wanted from stage (2) and boom, the assertion fails!
           
         , op 'c' undefined  -- (2) Creates a ([W] B Char beta) permanently in inerts
         , op True undefined -- (1) Creates ([W] B Bool alpha)
                             -- which immediately becomes [S] B Bool alpha
         ]

flop2 =  [ op False False   
         , op True undefined
         , op 'c' undefined 
         ]

        
flop3 =  [ op 'c' undefined  
         , op True undefined
         , op False False 
         ]

flop4 =  [ op 'c' undefined  
         , op False False   
         , op True undefined
         ]


flop5 =  [ op True undefined
         , op 'c' undefined  
         , op False False   
         ]


flop6 =  [ op True undefined
         , op False False   
         , op 'c' undefined  
         ]


{- Now, in HEAD we no longer have cached GivenSolved goals in the inerts and hence
   this situation can no longer appear. If a Given gets kicked out it is only because
   it got rewritten by a given equality: Notice that since Givens now never contain 
   plain old unification variables (since they are not GivenSolveds!) they can never be 
   rewritten by a spontaneously solved either! So our invariant now holds. -}
