-- Time-stamp: <2010-11-03 10:31:32 simonmar>
--
-- Chinese Remainder Algorithm.
-- Works over lists of Integers (2 input lists).
----------------------------------------------------------------------------

-- @node Chinese Remainder Algorithm, , ,
-- @chapter Chinese Remainder Algorithm

module CRA (binCRA, seq_list_CRA0, seq_list_CRA,
	    -- par_binCRA, tree_IMCRA0,
	    isPrime) where 

-- @menu
-- * Imports::			
-- * Auxiliary functions::	
-- * Basic binary CRA operation::  
-- * CRA over lists::		
-- @end menu

-- @node Imports, Auxiliary functions, Chinese Remainder Algorithm, Chinese Remainder Algorithm
-- @section Imports

import ModArithm (modHom, modDif, modProd, modInv) 

#if defined(STRATEGIES)
import Control.Parallel
import Control.Parallel.Strategies
#endif

-- @node Auxiliary functions, Basic binary CRA operation, Imports, Chinese Remainder Algorithm
-- @section Auxiliary functions

--isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n 
 | even n    = False
 | otherwise = isPrime' n 3

--isPrime' :: Integer -> Integer -> Bool
isPrime' n l1     | n < l1*l1        = True
                  | n `mod` l1 == 0  = False
                  | otherwise        = isPrime' n (l1+2)

-- @node Basic binary CRA operation, CRA over lists, Auxiliary functions, Chinese Remainder Algorithm
-- @section Basic binary CRA operation
#if 0 && defined(STRATEGIES)

par_binCRA :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 

par_binCRA m1 m2 inv a1 a2 = 
			 (if d == 0 then a1
                                    else a)
                         `sparking` rnf a
                         where ab = modHom m2 a1
                               d  = modDif m2 a2 a1
                               b  = modProd m2 d inv
                               b0 = if (b+b>m2) then b-m2
                                                    else b
                               a  = m1*b0+a1
#endif

binCRA :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer 

binCRA m1 m2 inv a1 a2 = (if d == 0 then a1
                                    else a)
                         where ab = modHom m2 a1
                               d  = modDif m2 a2 a1
                               b  = modProd m2 d inv
                               b0 = if (b+b>m2) then b-m2
                                                else b
                               a  =  m1*b0+a1             {- not HWL version -}

-- @node CRA over lists,  , Basic binary CRA operation, Chinese Remainder Algorithm
-- @section CRA over lists

-- @node Sequential,  , CRA over lists, CRA over lists
-- @subsection Sequential

seq_list_CRA0 :: [Integer] -> [Integer] -> (Integer,Integer)

seq_list_CRA0 (m:ms) (r:rs) = 
  foldl (\ (m0,r0) (m1,r1) -> (m0*m1, binCRA m0 m1 (modInv m1 m0) r0 r1) )
        (m,r) 
        (zip ms rs)


-- Same as above but applicable for infinite lists

seq_list_CRA :: Integer -> [Integer] -> [Integer] -> [Integer] -> (Integer,Integer)
seq_list_CRA mBound (m:ms) (r:rs) (d:ds)  = seq_list_CRA' mBound ms rs ds m r 

seq_list_CRA' :: Integer -> [Integer] -> [Integer] -> [Integer] -> Integer -> Integer ->
	    (Integer,Integer)
seq_list_CRA' mBound (m:ms) (r:rs) (d:ds) accM accR 
  | accM > mBound = (accM, accR)
  | d == 0     	  = seq_list_CRA' mBound ms rs ds accM accR
  | otherwise  	  = -- trace ("seq_list_CRA': (m,accM) = " ++ (show (m,accM))) $
                         seq_list_CRA' mBound ms rs ds (m*accM) 
                                       (my_cra accM m (modInv m accM) accR r)
                         where my_cra = binCRA {- or par_binCRA -}

-----------------------------------------------------------------------------
-- Note:
--  The CRA on lists must be able to
--   - work on infinite lists (i.e. computing length on them is a *bad* idea)
--   - determine whether the prime in the m-list is lucky
--     that decision must be deferred as long as possible to avoid unnecessary
--     synchronisation; for that a list of det's in the  hom. im.s is
--     provided, too.
-----------------------------------------------------------------------------

-- Note: ms as ds are infinte lists 

tree_IMCRA0 :: Integer -> [Integer] -> [Integer] -> [Integer] -> 
	       (Integer,Integer)


tree_IMCRA0 n ms as ds = 
	let
         res@(m, a, fails) = {-parGlobal 11 11 (forcelist ms')
                                (parGlobal 12 12 (forcelist as')
                                 (parGlobal 13 13 (forcelist ds') -}
                             tree_IMCRA0' ms' as' ds' -- ))
                             where ms' = take (fromInteger n) ms 
                                   as' = take (fromInteger n) as 
                                   ds' = take (fromInteger n) ds
         handle_fails :: Integer -> Integer -> Integer -> 
                         [Integer] -> [Integer] -> [Integer] -> (Integer, Integer)
         handle_fails n m a (m1:ms) (a1:as) (d1:ds) =
            -- parGlobal 46 46 1 0 m $
            -- parGlobal 47 47 1 0 a $
               (if n == 0 
                  then (m, a)
                  else if d1 == 0 
                         then (handle_fails n m a ms as ds)
                         else handle_fails (n-1) m' a' ms as ds
               )
#if defined(STRATEGIES)
               `using` \ x -> do par m (pseq a (return x)) --{ m' <- rpar m; a' <- rseq a; return x }
#endif
               where
                     m'  = m * m1
                     a'  = {-par_binCRA-} binCRA m m1 inv a a1
                     inv = modInv m1 m
       in
         handle_fails fails m a ms as ds

tree_IMCRA0' [m] [a] [0] = (1, 1, 1)   -- FAIL due to unlucky prime
tree_IMCRA0' [m] [a] [_] = (m, a, 0)   -- normal case
tree_IMCRA0' ms as ds =
       let 
         n = length ms
         (left_ms, right_ms) = splitAt (n `div` 2) ms
         (left_as, right_as) = splitAt (n `div` 2) as
         (left_ds, right_ds) = splitAt (n `div` 2) ds
         left@(left_M, left_CRA, left_fails)  = 
              tree_IMCRA0' left_ms left_as left_ds
         right@(right_M, right_CRA, right_fails)  = 
              tree_IMCRA0' right_ms right_as right_ds
         inv = modInv right_M left_M
         cra = {-par_binCRA-} binCRA left_M right_M inv left_CRA right_CRA
       in
       (left_M * right_M, cra, left_fails + right_fails )

-- IMCRA with list of products of the modules as additional argument i.e.
-- in s2IMCRA m M r the following holds M_i = m_1 * ... * m_{i-1}

foldl' :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
foldl' f z [] _ = z
foldl' f z (x:xs) (l:ls) = foldl' f (f z x l) xs ls

--s2IMCRA :: [Integer] -> [Integer] -> [Integer] -> (Integer,Integer)

--s2IMCRA (m:ms) pp (r:rs) = foldl' 
--                        (\ (m0,r0) (m1,r1) -> (p,binCRA m0 m1 (modInv m1 m0) r0 r1) )
--                        (m,r) (zip ms rs) pp

s3IMCRA :: [Integer] -> [Integer] ->[Integer] -> (Integer,Integer)

s3IMCRA (m:ms) pp (r:rs) = foldl 
                        (\ (m0,r0) (m1,r1,p) -> (p,binCRA m0 m1 (modInv m1 m0) r0 r1) )
                        (m,r) (zip3 ms rs pp)

