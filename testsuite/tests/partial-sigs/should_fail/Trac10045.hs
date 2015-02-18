module Trac10045 where

newtype Meta = Meta ()

foo (Meta ws1) =
    let copy :: _
        copy w from = copy w 1
    in copy ws1 1
