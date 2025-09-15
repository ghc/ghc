{-# LANGUAGE TypeApplications, RecordWildCards #-}
module HoleFitPlugin where

import GHC.Plugins hiding ((<>))

import Data.List (stripPrefix, sortOn)

import GHC.Tc.Types.Constraint

import GHC.Tc.Utils.Monad

import Text.Read
import GHC.Data.Bag



data HolePluginState = HPS { holesChecked :: Int
                           , holesLimit :: Maybe Int}

bumpHolesChecked :: HolePluginState -> HolePluginState
bumpHolesChecked (HPS h l) = HPS (h + 1) l

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
initPlugin [limit] = newTcRef $ HPS 0 $
  case readMaybe @Int limit of
      Just number ->  Just number
      _ -> error $ "Invalid argument to plugin: " <> show limit
initPlugin _ = newTcRef $ HPS 0 Nothing

fromModule :: HoleFitCandidate -> [String]
fromModule (GreHFCand gre) =
  map (moduleNameString . importSpecModule) $ (bagToList (gre_imp gre))
fromModule _ = []

toHoleFitCommand :: TypedHole -> String -> Maybe String
toHoleFitCommand (TypedHole {th_hole = Just (Hole { hole_occ = h })}) str
    = stripPrefix ("_" <> str) $ occNameString (occName h)
toHoleFitCommand _ _ = Nothing


-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
modFilterTimeoutP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
modFilterTimeoutP _ ref hole cands = do
  updTcRef ref bumpHolesChecked
  HPS {..} <- readTcRef ref
  return $ case holesLimit of
    -- If we're out of checks, remove any candidates, so nothing is checked.
    Just limit | holesChecked > limit -> []
    _ -> case toHoleFitCommand hole "only_" of
           Just modName -> filter (inScopeVia modName) cands
           _ -> cands
  where inScopeVia modNameStr cand@(GreHFCand _) =
          elem (toModName modNameStr) $ fromModule cand
        inScopeVia _ _ = False
        toModName = replace '_' '.'
        replace :: Eq a => a -> a -> [a] -> [a]
        replace _ _ [] = []
        replace a b (x:xs) = (if x == a then b else x):replace a b xs


modSortP :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
modSortP _ ref hole hfs = do
  HPS {..} <- readTcRef ref
  return $ case holesLimit of
    Just limit | holesChecked > limit -> [RawHoleFit $ text msg]
    _ -> case toHoleFitCommand hole "sort_by_mod" of
            -- If only_ is on, the fits will all be from the same module.
           Just ('_':'d':'e':'s':'c':_) -> reverse hfs
           Just _ -> orderByModule hfs
           _ ->  hfs
  where orderByModule :: [HoleFit] -> [HoleFit]
        orderByModule = sortOn (fmap fromModule . mbHFCand)
        mbHFCand :: HoleFit -> Maybe HoleFitCandidate
        mbHFCand (TcHoleFit (HoleFit {hfCand = c})) = Just c
        mbHFCand _ = Nothing
        msg = "Error: Too many holes were checked, and the search aborted for"
            <> "this hole. Try again with a higher limit."

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where initP = initPlugin opts
        stopP = const $ return ()
        pluginDef ref = HoleFitPlugin { candPlugin = modFilterTimeoutP opts ref
                                      , fitPlugin  = modSortP opts ref }
