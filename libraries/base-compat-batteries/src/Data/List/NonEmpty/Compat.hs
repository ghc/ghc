{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.List.NonEmpty.Compat (
  -- * The type of non-empty streams
    NonEmpty(..)

  -- * Non-empty stream transformations
  , map         
  , intersperse 
  , scanl       
  , scanr       
  , scanl1      
  , scanr1      
  , transpose   
  , sortBy      
  , sortWith      
  -- * Basic functions
  , length      
  , head        
  , tail        
  , last        
  , init        
  , singleton
  , (<|), cons  
  , uncons      
  , unfoldr     
  , sort        
  , reverse     
  , inits       
  , tails       
  -- * Building streams
  , iterate     
  , repeat      
  , cycle       
  , unfold      
  , insert      
  , some1       
  -- * Extracting sublists
  , take        
  , drop        
  , splitAt     
  , takeWhile   
  , dropWhile   
  , span        
  , break       
  , filter      
  , partition   
  , group       
  , groupBy     
  , groupWith     
  , groupAllWith  
  , group1      
  , groupBy1    
  , groupWith1     
  , groupAllWith1  
  -- * Sublist predicates
  , isPrefixOf  
  -- * \"Set\" operations
  , nub         
  , nubBy       
  -- * Indexing streams
  , (!!)        
  -- * Zipping and unzipping streams
  , zip         
  , zipWith     
  , unzip       
  -- * Converting to and from a list
  , fromList    
  , toList      
  , nonEmpty    
  , xor         
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Data.List.NonEmpty.Compat
#else
import "semigroups" Data.List.NonEmpty
#endif

#if !(MIN_VERSION_base(4,9,0))
-- | Construct a 'NonEmpty' list from a single element.
--
-- /Since: 4.15/
singleton :: a -> NonEmpty a
singleton a = a :| []
#endif
