{-# LANGUAGE RecursiveDo #-}
-- From https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html

import Control.Monad.Fix

data RoseTree a = RoseTree a [RoseTree a]
  deriving (Show)

exampleTree :: RoseTree Int
exampleTree = RoseTree 5 [RoseTree 4 [], RoseTree 6 []]

pureMax :: Ord a => RoseTree a -> RoseTree (a, a)
pureMax tree =
  let (t, largest) = go largest tree
  in t
 where
  go :: Ord a => a -> RoseTree a -> (RoseTree (a, a), a)
  go biggest (RoseTree x []) = (RoseTree (x, biggest) [], x)
  go biggest (RoseTree x xs) =
      let sub = map (go biggest) xs
          (xs', largests) = unzip sub
      in (RoseTree (x, biggest) xs', max x (maximum largests))

t = pureMax exampleTree

-- ---------------------------------------------------------------------

impureMin :: (MonadFix m, Ord b) => (a -> m b) -> RoseTree a -> m (RoseTree (a, b))
impureMin f tree = do
  rec (t, largest) <- go largest tree
  return t
 where
  go smallest (RoseTree x []) = do
    b <- f x
    return (RoseTree (x, smallest) [], b)

  go smallest (RoseTree x xs) = do
    sub <- mapM (go smallest) xs
    b <- f x
    let (xs', bs) = unzip sub
    return (RoseTree (x, smallest) xs', min b (minimum bs))

budget :: String -> IO Int
budget "Ada"      = return 10 -- A struggling startup programmer
budget "Curry"    = return 50 -- A big-earner in finance
budget "Dijkstra" = return 20 -- Teaching is the real reward
budget "Howard"   = return 5  -- An frugile undergraduate!

inviteTree = RoseTree "Ada" [ RoseTree "Dijkstra" []
                            , RoseTree "Curry" [ RoseTree "Howard" []]
                            ]

ti = impureMin budget inviteTree

simplemdo = mdo
  return 5
