{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- In this example the simplifer destroyed a join point,
-- namely the `loop` inside `detectLeaks`

module T24768 (detectLeaks) where

import Control.Monad (zipWithM_)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT, evalStateT)
import qualified Data.Map as M
import qualified Data.Set as S

data Debuggee

newtype DebugM a = DebugM (ReaderT Debuggee IO a)
                    deriving (Functor, Applicative, Monad)

runSimple :: Debuggee -> DebugM a -> IO a
runSimple d (DebugM a) = runReaderT a d

cands :: [a]
cands = []
{-# NOINLINE cands #-}

detectLeaks :: Debuggee -> IO ()
detectLeaks e = loop M.empty
  where
    loop :: M.Map () RankInfo -> IO ()
    loop rm = do
      gs <- runSimple e $ mapM (findSlice rm) cands
      zipWithM_ (\n _g -> writeFile
                            ("slices/" ++ show @Int n ++ ".dot")
                            "abcd")
                [0..] gs
      loop rm

data RankInfo = RankInfo !Double !Int

lookupRM :: () -> M.Map () RankInfo -> [((), RankInfo)]
lookupRM k m = M.assocs filtered_map
  where
    (res_map, _) = M.partitionWithKey (\e _ -> e == k) m
    filtered_map = M.filter (\(RankInfo r _) -> r > 0) res_map

findSlice :: forall m a. Monad m => M.Map () RankInfo -> () -> m [a]
findSlice rm _k = evalStateT go S.empty
  where
    go :: StateT s m [a]
    go = do
      let next_edges = lookupRM () rm
      _ss <- concat <$> mapM (\_ -> go) next_edges
      return []
