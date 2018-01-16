module T10045 where

newtype Meta = Meta ()

foo (Meta ws1) =
     let copy :: _
         copy w from = copy w True
     in copy ws1 False
