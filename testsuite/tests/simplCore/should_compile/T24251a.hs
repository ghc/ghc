module T24251a where

f xs = xs `seq`
       (let t = reverse (reverse (reverse (reverse xs))) in
        case xs of
          [] ->  (t,True)
          (_:_) -> (t,False))

-- We start with an eval of xs, but that should disappear.
