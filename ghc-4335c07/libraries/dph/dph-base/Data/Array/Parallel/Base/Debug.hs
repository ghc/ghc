-- | Debugging infrastructure for the parallel arrays library.
module Data.Array.Parallel.Base.Debug 
        ( check
        , checkCritical
        , checkLen
        , checkSlice
        , checkEq
        , checkNotEmpty
        , uninitialised)
where
import Data.Array.Parallel.Base.Config  (debug, debugCritical)


-- | Throw an index-out-of-bounds error.
errorOfBounds :: String -> Int -> Int -> a
errorOfBounds loc n i 
        =  error $ loc ++ ": Out of bounds "
                ++ "(vector length = "  ++ show n 
                ++ "; index = "         ++ show i ++ ")"


-- | Throw a bad slice error.
errorBadSlice :: String -> Int -> Int -> Int -> a
errorBadSlice loc vecLen sliceStart sliceLen
        = error $ loc ++ ": Bad slice "
                ++ "(vecLen = "         ++ show vecLen
                ++ "; sliceStart = "    ++ show sliceStart
                ++ "; sliceLen = "     ++ show sliceLen ++ ")"


-- | Bounds check, enabled when `debug` = `True`.
-- 
--   The first integer is the length of the array, and the second
--   is the index. The second must be greater or equal to '0' and less than
--   the first integer. If the not then `error` with the `String`.
--
check :: String -> Int -> Int -> a -> a
check loc n i v 
  | debug      
  = if i >= 0 && i < n
        then v 
        else errorOfBounds loc n i

  | otherwise  = v
{-# INLINE check #-}


-- | Bounds check, enabled when `debugCritical` = `True`.
--
--   This version is used to check operations that could corrupt the heap.
-- 
--   The first integer is the length of the array, and the second
--   is the index. The second must be greater or equal to '0' and less than
--   the first integer. If the not then `error` with the `String`.
--
checkCritical :: String -> Int -> Int -> a -> a
checkCritical loc n i v 
  | debugCritical 
  = if i >= 0 && i < n
        then v 
        else errorOfBounds loc n i

  | otherwise     = v
{-# INLINE checkCritical #-}


-- | Length check, enabled when `debug` = `True`.
-- 
--   Check that the second integer is greater or equal to `0' and less or equal
--   than the first integer. If the not then `error` with the `String`.
--
checkLen :: String -> Int -> Int -> a -> a
checkLen loc n i v 
  | debug      
  = if i >= 0 && i <= n 
        then v 
        else errorOfBounds loc n i

  | otherwise  = v
{-# INLINE checkLen #-}


-- | Slice check, enable when `debug` = `True`.
--
--   The vector must contain at least `sliceStart` + `sliceLen` elements.
-- 
checkSlice :: String -> Int -> Int -> Int -> a -> a
checkSlice loc vecLen sliceStart sliceLen v
  | debug        
  = if   (  sliceStart >= 0             && sliceStart            <= vecLen
         && sliceStart + sliceLen >= 0  && sliceStart + sliceLen <= vecLen )
        then v
        else errorBadSlice loc vecLen sliceStart sliceLen

  | otherwise    = v
{-# INLINE checkSlice #-}


-- | Equality check, enabled when `debug` = `True`.
--   
--   The two `a` values must be equal, else `error`.
--
--   The first `String` gives the location of the error,
--   and the second some helpful message.
--
checkEq :: (Eq a, Show a) => String -> String -> a -> a -> b -> b
checkEq loc msg x y v
  | debug     
  = if x == y 
        then v 
        else error $ loc ++ ": " ++ msg
                  ++ " (first = " ++ show x
                  ++ "; second = " ++ show y ++ ")"

  | otherwise = v
{-# INLINE checkEq #-}


-- | Given an array length, check it is not zero.
checkNotEmpty :: String -> Int -> a -> a
checkNotEmpty loc n v
  | debug     
  = if n /= 0 
        then v 
        else error $ loc ++ ": Empty array"

  | otherwise = v
{-# INLINE checkNotEmpty #-}


-- | Throw an error saying something was not intitialised.
--   
--   The `String` must contain a helpful message saying what module
--   the error occured in, and the possible reasons for it.
--   If not then a puppy dies at compile time.
--
uninitialised :: String -> a
uninitialised loc 
        = error $ loc ++ ": Touched an uninitialised value"

