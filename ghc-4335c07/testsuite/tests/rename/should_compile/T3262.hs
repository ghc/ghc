{-# OPTIONS -fwarn-name-shadowing #-}

-- Trac #3262: report shadowing in g but not f
 
module T3262 where

f x = let _ignored = 10 in
      let _ignored = 20 in
      x + _ignored

g x = let not_ignored = 10 in
      let not_ignored = 20 in
      x + not_ignored

mf x = do let _ignored = 10
          let _ignored = 20
          return (x + _ignored)

mg x = do let not_ignored = 10
          let not_ignored = 20
          return (x + not_ignored)
