-- | Check the source spans associated with the expansion of quasi-quotes
module Main (main) where

import GHC
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Env
import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Types.Name.Set
import GHC.Types.Var
import GHC.Types.SrcLoc as SrcLoc

import Data.Data

import System.Environment
import Control.Monad
import Control.Monad.Trans.State
import Data.List (sortBy)
import Data.Function
import Prelude hiding (traverse)

type Traverse a = State (SrcSpan, [(Name, SrcSpan)]) a

traverse :: Data a => a -> Traverse a
traverse a =
    skipNameSet (cast a) a $ do
      updateLoc  (cast a)
      showVar    (cast a)
      showTyVar  (cast a)
      showPatVar (cast a)
      gmapM traverse a
  where
    showVar :: Maybe (HsExpr GhcTc) -> Traverse ()
    showVar (Just (HsVar _ (L _ v))) =
      modify $ \(loc, ids) -> (loc, (varName v, loc) : ids)
    showVar _ =
      return ()

    showTyVar :: Maybe (HsType GhcRn) -> Traverse ()
    showTyVar (Just (HsTyVar _ _ (L _ v))) =
      modify $ \(loc, ids) -> (loc, (v, loc) : ids)
    showTyVar _ =
      return ()

    showPatVar :: Maybe (Pat GhcTc) -> Traverse ()
    showPatVar (Just (VarPat _ (L _ v))) =
      modify $ \(loc, ids) -> (loc, (varName v, loc) : ids)
    showPatVar _
      = return ()

    -- Updating the location in this way works because we see the SrcSpan
    -- before the associated term due to the definition of GenLocated
    updateLoc :: Maybe SrcSpan -> Traverse ()
    updateLoc (Just loc) = modify $ \(_, ids) -> (loc, ids)
    updateLoc _          = return ()

    skipNameSet :: Monad m => Maybe NameSet -> a -> m a -> m a
    skipNameSet (Just _) a _ = return a
    skipNameSet Nothing  _ f = f

test7918 :: Ghc ()
test7918 = do
  dynFlags <- getSessionDynFlags
  void $ setSessionDynFlags (gopt_set dynFlags Opt_BuildDynamicToo)

  unit <- hscActiveUnit <$> getSession

  let target = Target {
                   targetId           = TargetFile "T7918B.hs" Nothing
                 , targetAllowObjCode = True
                 , targetUnitId       = unit
                 , targetContents     = Nothing
                 }
  setTargets [target]
  void $ load LoadAllTargets

  typecheckedB <- getModSummary (mkModuleName "T7918B") >>= parseModule >>= typecheckModule
  let (_loc, ids) = execState (traverse (tm_typechecked_source typecheckedB)) (noSrcSpan, [])
  liftIO . forM_ (sortBy (SrcLoc.leftmost_smallest `on` snd) (reverse ids)) $ putStrLn . showSDoc dynFlags . ppr

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) test7918
