{-# OPTIONS_GHC -fwarn-redundant-constraints #-}

module T9973 where

duplicateDecl :: (Eq t) => t -> IO ()
-- #9973 was a bogus "redundant constraint" here
duplicateDecl sigs
 = do { newSpan <- return typeSig

         -- **** commenting out the next three lines causes the original warning to disappear
       ; let rowOffset = case typeSig of {  _  -> 1 }

       ; undefined }
   where
     typeSig = definingSigsNames sigs


definingSigsNames :: (Eq t) => t -> ()
definingSigsNames x = undefined
  where
    _ = x == x   -- Suppress the complaint on this

