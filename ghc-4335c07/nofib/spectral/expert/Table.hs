{------------------------------------------------------------------------------
				    TABLES

A Table is a set of entries, each containing a key and an associated value, the
key being used to look up the value.

In database-style applications, the value may be a record, and the key may be a
field in it. The normal effect of sharing of subexpressions should avoid
serious space problems. However, `computed' keys may cause a space problem.

Keys are assumed to be unique. The effect of non-unique keys can be obtained by
associated a list value such as [v1,v2,...] with each key.

With the `enterList' function, the first entry for a key takes precedence over
any later ones with the same key. This allows a table to be built `lazily', the
entries in the list only being evaluated as needed to satisfy `find' calls.

REQUIREMENTS:
   The results module `result.g' must be loaded before this one.
   The key type must be ordered (an instance of class Ord).

EXPORTS:
   Table k v        the type of tables; k and v are the key and value types
   newTable         an empty table
   enter t k v      add entry to t (no effect if old entry for k exists)
   enterList t es   add a list of (key,val) pairs to t
   update t k v     change entry in t (or add new entry if necessary)
   updateList t es  change a list of (key,val) pairs in t
   find t k         lookup k in t giving (success v) or (failure "not found")
   delete t k       remove entry in t for key k (if any)
   entries t        return list of all (key,val) pairs in t in key order
------------------------------------------------------------------------------}

module Table where
import Result

-- The implementation here uses a binary search tree, giving `log n' time
-- operations, provided that the tree remains well-balanced.  Eventually, there
-- should be a constant-time version with the same semantics.

data Table k v = Empty | Fork (Table k v) (k,v) (Table k v)

newTable = Empty

find Empty key = failure "not found"
find (Fork left (k,v) right) key
   | key <  k  =  find left key
   | key == k  =  success v
   | key >  k  =  find right key

enter Empty key val = Fork Empty (key,val) Empty
enter (Fork left (k,v) right) key val
   | key <  k  =  Fork (enter left key val) (k,v) right
   | key == k  =  Fork left (k,v) right
   | key >  k  =  Fork left (k,v) (enter right key val)

update Empty key val  =  Fork Empty (key,val) Empty
update (Fork left (k,v) right) key val
   | key <  k  =  Fork (update left key val) (k,v) right
   | key == k  =  Fork left (key,val) right
   | key >  k  =  Fork left (k,v) (update right key val)

delete Empty key =  Empty
delete (Fork left (k,v) right) key
   | key <  k  =  Fork (delete left key) (k,v) right
   | key == k  =  graft left right
   | key >  k  =  Fork left (k,v) (delete right key)
   where
   graft left Empty = left
   graft left right = Fork left e right' where (e,right') = leftmost right
   leftmost (Fork Empty e r) = (e,r)
   leftmost (Fork l e r) = (e2, Fork l' e r)  where (e2,l') = leftmost l

-- `enterList t es' adds a list of new entries. It is lazy in es (but may build
-- a poorly balanced tree).

enterList t []  =  t
enterList Empty (e:res)  =  Fork left e right  where
   k  =  fst e
   left  =  enterList Empty [e1 | e1<-res, fst e1 < k]
   right  =  enterList Empty [e1 | e1<-res, fst e1 > k]
enterList (Fork left e right) es  =  Fork left' e right'  where
   k  =  fst e
   left'  =  enterList left [e1 | e1<-es, fst e1 < k]
   right'  =  enterList right [e1 | e1<-es, fst e1 > k]

-- `updateList t es' makes a list of updates. It is strict in es, and optimised
-- to produce a well balanced tree. it can be used with es==[] purely to
-- rebalance the tree.

updateList t es = balance (mergeKey (entries t) (unique (sortKey es))) where
   balance [] = Empty
   balance es = Fork left (es!!m) right where
      left  =  balance (take m es)
      right  =  balance (drop (m+1) es)
      m  =  length es `div` 2
   unique [] = []
   unique [e] = [e]
   unique ((k1,v1):(k2,v2):res) =
      if k1==k2 then unique ((k2,v2):res) else (k1,v1) : unique ((k2,v2):res)

sortKey kvs = foldr insertKey [] kvs where
   insertKey kv []          = [kv]
   insertKey (k1,v1) ((k2,v2):res)
        | k1 <= k2  = (k1,v1):(k2,v2):res
        | otherwise = (k2,v2):insertKey (k1,v1) res

mergeKey [] kvs = kvs
mergeKey kvs [] = kvs
mergeKey ((k1,v1):kvs1) ((k2,v2):kvs2)
        | k1 <= k2  = (k1,v1) : mergeKey kvs1 ((k2,v2):kvs2)
        | otherwise = (k2,v2) : mergeKey ((k1,v1):kvs1) kvs2

-- `entries t' returns the list of entries in t, sorted by key. Inefficient
-- unless tree-optimised version of ++ is used.

entries Empty  =  []
entries (Fork left e right)  =  entries left ++ [e] ++ entries right
