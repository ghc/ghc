---------------------------------------------------------
-- The main program for the hpc-add tool, part of HPC.
-- Andy Gill, Oct 2006
---------------------------------------------------------

module HpcCombine (combine_plugin) where 

import Trace.Hpc.Tix
import Trace.Hpc.Util

import HpcFlags

import Control.Monad
import qualified HpcSet as Set
import qualified HpcMap as Map
import System.Environment

------------------------------------------------------------------------------
combine_options 
        = excludeOpt
        . includeOpt
        . outputOpt
        . combineFunOpt
        . combineFunOptInfo
        . postInvertOpt
       	 
combine_plugin = Plugin { name = "combine"
	      	       , usage = "[OPTION] .. <TIX_FILE> [<TIX_FILE> [<TIX_FILE> ..]]" 
		       , options = combine_options 
		       , summary = "Combine multiple .tix files in a single .tix files"
		       , implementation = combine_main
		       , init_flags = default_flags
		       , final_flags = default_final_flags
		       }

------------------------------------------------------------------------------

combine_main :: Flags -> [String] -> IO ()
combine_main flags (first_file:more_files) = do
  -- combine does not expand out the .tix filenames (by design).

  let f = case combineFun flags of
      	    ADD  -> \ l r -> l + r
            SUB  -> \ l r -> max 0 (l - r)
	    DIFF -> \ g b -> if g > 0 then 0 else min 1 b
	    ZERO -> \ _ _ -> 0

  Just tix <- readTix first_file

  tix' <- foldM (mergeTixFile flags f) 
       	  	(filterTix flags tix)
		more_files

  let (Tix inside_tix') = tix'
  let inv 0 = 1
      inv n = 0
  let tix'' = if postInvert flags
      	      then Tix [ TixModule m p i (map inv t)
	      	       | TixModule m p i t <- inside_tix'
		       ]
	      else tix'

  case outputFile flags of
    "-" -> putStrLn (show tix'')
    out -> writeTix out tix''

mergeTixFile :: Flags -> (Integer -> Integer -> Integer) -> Tix -> String -> IO Tix
mergeTixFile flags fn tix file_name = do
  Just new_tix <- readTix file_name
  return $! strict $ mergeTix fn tix (filterTix flags new_tix)

-- could allow different numbering on the module info, 
-- as long as the total is the same; will require normalization.

mergeTix :: (Integer -> Integer -> Integer) -> Tix -> Tix -> Tix 
mergeTix f
	 (Tix t1)
      	 (Tix t2)  = Tix 
	 [ case (Map.lookup m fm1,Map.lookup m fm2) of
	   -- todo, revisit the semantics of this combination
	    (Just (TixModule _ hash1 len1 tix1),Just (TixModule _ hash2 len2 tix2)) 
	       | hash1 /= hash2 
	       || length tix1 /= length tix2
	       || len1 /= length tix1
	       || len2 /= length tix2
	       	     -> error $ "mismatched in module " ++ m
	       | otherwise      -> 
	       	     TixModule m hash1 len1 (zipWith f tix1 tix2) 
	    (Just (TixModule _ hash1 len1 tix1),Nothing) -> 
	    	  error $ "rogue module " ++ show m
	    (Nothing,Just (TixModule _ hash2 len2 tix2)) -> 
	    	  error $ "rogue module " ++ show m
	    _ -> error "impossible"
	 | m <- Set.toList (m1s `Set.intersection` m2s)
         ]
  where 
   m1s = Set.fromList $ map tixModuleName t1 
   m2s = Set.fromList $ map tixModuleName t2

   fm1 = Map.fromList [ (tixModuleName tix,tix) 
       	 	      | tix <- t1
		      ]
   fm2 = Map.fromList [ (tixModuleName tix,tix) 
       	 	      | tix <- t2
		      ]


-- What I would give for a hyperstrict :-)
-- This makes things about 100 times faster.
class Strict a where
   strict :: a -> a

instance Strict Integer where
   strict i = i

instance Strict Int where
   strict i = i

instance Strict Hash where	-- should be fine, because Hash is a newtype round an Int
   strict i = i

instance Strict Char where
   strict i = i

instance Strict a => Strict [a] where
   strict (a:as) = (((:) $! strict a) $! strict as)
   strict []     = []

instance (Strict a, Strict b) => Strict (a,b) where
   strict (a,b) = (((,) $! strict a) $! strict b)

instance Strict Tix where
  strict (Tix t1) = 
  	    Tix $! strict t1

instance Strict TixModule where
  strict (TixModule m1 p1 i1 t1) = 
  	    ((((TixModule $! strict m1) $! strict p1) $! strict i1) $! strict t1)

