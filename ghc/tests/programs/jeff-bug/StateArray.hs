module StateArray ( stateArray,updateArray) where

import List
import Signal
import LazyST

import Array
import Memory
import Words
import Array
import Memory
import Signal
import LazyST 
import Words
import Memory

{- 
  these functions accept vectors of Array requests (reads or writes)
  and services them.
-}

stateArray :: (Ix a, Enum a) => ((a,a),[(a,a,b)]) -> 
              Signal [ArrReq a b] -> Signal [ArrResp a b]

updateArray :: Ix a => Signal (Array a b) -> 
                       [(Signal Bool,(Signal a,Signal b))] -> 
                       Signal (Array a b)


-- BEWARE --- unless you're really digging deep into the 
-- library you've probably made a wrong turn.
-- Unless you know what you're doing dont be here.


stateArray (bounds@(loBound,hiBound),initWrites) input =
    runST (
            do arr <- newSTArray bounds initVal
	       initArray arr
               loop (input) $ \input -> performRequests arr input
          )
      where
      -- Determine what the array should be initialized to; remove
      --  some of the writes that would initialize the array to the
      --  same value to speed up the initialization process.
      contigWrites = contigWriteRanges 
                      (loBound,hiBound,
                       error "uninitialized value read from stateArray") 
                      initWrites
      maxRange@(_,_,initVal) = maxWriteRange contigWrites
      reducedInitWrites = removeWriteRange maxRange contigWrites


      -- Initialize the array according to 'initWrites'
      initArray arr
	= strictSequence [ writeSTArray arr index val |
				(lowIdx,hiIdx,val) <- reducedInitWrites,
				index <- range (lowIdx,hiIdx) ]

   -- Perform the requested writes, reads, and freezes for each clock cycle
performRequests arr reqs = performReqs reqs
	  where
	    performReqs reqs
	      = mapM performReq reqs

	    performReq (ReadArr i)
	      = do val <- readSTArray arr i
		   return (ReadVal val)

	    performReq (WriteArr loAddr hiAddr val)
	      = do sequence [ writeSTArray arr loc val |
				loc <- range (loAddr,hiAddr) ]
		   return Written

	    performReq (WriteFn loc f)
	      = do readVal <- readSTArray arr loc
		   let writeVal = f readVal
		   writeSTArray arr loc writeVal
		   return (WrittenFn writeVal)

	    performReq FreezeArr
	      = do arr <- freezeSTArray arr
		   return (ArrayVal arr)

-- Forces each action in its argument list by pattern-matching
--  on the action's output unit. This function is useful in preventing
--  large sequences of actions from being built.
strictSequence :: Monad m => [m ()] -> m ()
strictSequence = foldr (\m n -> do { () <- m; n }) (return ())

{-
	The following functions dealing with write-ranges are
	needed because the hugs interpreter is very slow in evaluating
	lazy monadic expressions involving lots of writes to a MutArr.
	Even simple programs output by dlxgcc ask to have about 16K-words
	of data to be initialized to zero, while other areas of memory
	should be initialized to an error value. These routines
	allow me to isolate what the majority of array locations should
	be initialized to; I can pass this initialization value to
	newArr (which is implemented as a primitive) to avoid most
	of the initial writes.
-}

-- Given a write-range and a list of contiguous sorted write ranges,
--  this function outputs a contiguous sorted write range that would
--  result when the first write range is written to an array after the other
--  write ranges are written to an array. Note that the write-range to
--  be inserted must overlap or be contiguous to the write-range list.
insertWrite :: (Ix i,Enum i) => (i,i,a) -> [(i,i,a)] -> [(i,i,a)]
insertWrite writeRange []
  = [writeRange]
insertWrite writeRange@(lo,hi,v) (first@(firstLo,firstHi,firstVal):rest)
  -- empty writeRange
  | hi < lo			= first : rest
  -- writeRange is completely less than first element
  | hi < firstLo		= writeRange : first : rest
  -- writeRange is completely greater than first element
  | firstHi < lo		= first : insertWrite writeRange rest
  -- writeRange completely overlaps the first element
  | lo <= firstLo && hi >= firstHi = insertWrite writeRange rest
  -- writeRange partially overlaps the first element; the leading
  --  edge of writeRange is less than or equal to the leading edge
  --  of the first element.
  | lo <= firstLo		= writeRange : (succ hi,firstHi,firstVal) : rest
  -- writeRange partially overlaps the first element; the leading
  --  edge of writeRange is greater than the leading edge of the
  --  first element.
  | firstLo < lo		= (firstLo,pred lo,firstVal) : insertWrite writeRange ((lo,firstHi,firstVal):rest)
  | True			= error "bug in insertWrite"


-- Given a write range 'writeRange' and a list of write-ranges 'ranges' whose
--  elements are subranges of 'writeRange', this function outputs a contiguous,
--  non-overlapping list of write-ranges that is equivalent to writing
--  'writeRange' to an array, followed by writing the elements of 'ranges'
--  in order to the same array.
contigWriteRanges :: (Ix i,Enum i) => (i,i,a) -> [(i,i,a)] -> [(i,i,a)]
contigWriteRanges writeRange ranges
  = foldr insertWrite [writeRange] (reverse ranges)


-- Finds the largest write-range in a list of write-ranges.
maxWriteRange :: (Ix i,Enum i) => [(i,i,a)] -> (i,i,a)
maxWriteRange
  = foldr1 (\a@(loA,hiA,_) b@(loB,hiB,_) ->
		if rangeSize (loA,hiA) >= rangeSize (loB,hiB)
		  then a
		  else b)

-- removes a given write-range from a list of write-ranges
removeWriteRange :: (Ix i,Enum i) => (i,i,a) -> [(i,i,a)] -> [(i,i,a)]
removeWriteRange (lo,hi,_) = filter (\(loA,hiA,_) -> lo /= loA || hi /= hiA)
	  


--  Updates an array Signal, given a static list of updaters. Each
--  updater consists of a Boolean enable signal, and a signal pair
--  of the update address and update value.
updateArray arr updaters
  = foldr (\(updateEnable,updater) prevArray ->
		if' updateEnable 
                   then' (lift2 (//) prevArray (singleton (bundle2 updater)))
		   else' prevArray
          )
	  arr
	  updaters
    where singleton = lift1 $ \x -> [x]


