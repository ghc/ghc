{-# LANGUAGE DisambiguateRecordFields #-}  
module T5372 where  
import qualified T5372a  
notScope (MkS { x = n }) = n  
