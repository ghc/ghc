
-- | GHC API debugger module for finding and setting breakpoints.
--
-- This module is user facing and is at least used by `GHCi` and `ghc-debugger`
-- to find and set breakpoints.
module GHC.Runtime.Debugger.Breakpoints where

import GHC.Prelude

import Control.Monad.Catch
import Control.Monad
import Data.Array
import Data.Function
import qualified Data.List as List
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as S

import GHC.HsToCore.Breakpoints
import GHC.ByteCode.Breakpoints
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Driver.Session.Inspect
import GHC.Runtime.Eval
import GHC.Runtime.Eval.Utils
import GHC.Types.Name
import Language.Haskell.Textual.Location
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified Data.IntMap.Strict as IntMap
import qualified GHC.Unit.Home.Graph as HUG
import qualified GHC.Unit.Home.PackageTable as HPT

--------------------------------------------------------------------------------
-- Finding Module breakpoints
--------------------------------------------------------------------------------

-- | Find a breakpoint given a Module's 'TickArray' and the line number.
--
-- When a line number is specified, the current policy for choosing
-- the best breakpoint is this:
--    - the leftmost complete subexpression on the specified line, or
--    - the leftmost subexpression starting on the specified line, or
--    - the rightmost subexpression enclosing the specified line
--
findBreakByLine :: Int {-^ Line number -} -> TickArray -> Maybe (BreakTickIndex, RealSrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (List.sortBy (leftmostLargestRealSrcSpan `on` snd)  comp)   `mplus`
    listToMaybe (List.sortBy (compare `on` snd) incomp) `mplus`
    listToMaybe (List.sortBy (flip compare `on` snd) ticks)
  where
        ticks = arr ! line

        starts_here = [ (ix,pan) | (ix, pan) <- ticks,
                        srcSpanStartLine pan == line ]

        (comp, incomp) = List.partition ends_here starts_here
            where ends_here (_,pan) = srcSpanEndLine pan == line

-- | Find a breakpoint in the 'TickArray' of a module, given a line number and a column coordinate.
findBreakByCoord :: (Int, Int) -> TickArray -> Maybe (BreakTickIndex, RealSrcSpan)
findBreakByCoord (line, col) arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (List.sortBy (flip compare `on` snd) contains ++
                 List.sortBy (compare `on` snd) after_here)
  where
        ticks = arr ! line

        -- the ticks that span this coordinate
        contains = [ tick | tick@(_,pan) <- ticks, RealSrcSpan pan Nothing `spans` (line,col) ]

        after_here = [ tick | tick@(_,pan) <- ticks,
                              srcSpanStartLine pan == line,
                              srcSpanStartCol pan >= col ]

leftmostLargestRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> Ordering
leftmostLargestRealSrcSpan = on compare realSrcSpanStart S.<> on (flip compare) realSrcSpanEnd

-- | Returns the span of the largest tick containing the srcspan given
enclosingTickSpan :: TickArray -> SrcSpan -> RealSrcSpan
enclosingTickSpan _ (UnhelpfulSpan _) = panic "enclosingTickSpan UnhelpfulSpan"
enclosingTickSpan ticks (RealSrcSpan src _) =
  assert (inRange (bounds ticks) line) $
    List.minimumBy leftmostLargestRealSrcSpan $ enclosing_spans
  where
    line = srcSpanStartLine src
    enclosing_spans = [ pan | (_,pan) <- ticks ! line
                            , realSrcSpanEnd pan >= realSrcSpanEnd src]

--------------------------------------------------------------------------------
-- Finding Function breakpoints
--------------------------------------------------------------------------------

-- | Process and validate the user string of form @[Module.]function@ into the
-- relevant module information and function name.
--
-- Validation guarantees
--  1. The module exists
--  2. The identifier is in an interpreted module
--  3. The identifier has a breakpoint entry in the module's 'ModBreaks'
--
-- Returns either an error SDoc or the 'Module' and 'ModuleInfo' for the relevant module
-- paired with the function name
--
-- See also Note [Setting Breakpoints by Id]
resolveFunctionBreakpoint :: GhcMonad m => String -> m (Either SDoc (Module, ModuleInfo, String))
resolveFunctionBreakpoint inp = do
  let (mod_str, top_level, fun_str) = splitIdent inp
      mod_top_lvl = combineModIdent mod_str top_level
  mb_mod <- catch (lookupModuleInscope mod_top_lvl)
                  (\(_ :: SomeException) -> lookupModuleInGraph mod_str)
    -- If the top-level name is not in scope, `lookupModuleInscope` will
    -- throw an exception, then lookup the module name in the module graph.
  mb_err_msg <- validateBP mod_str fun_str mb_mod
  case mb_err_msg of
    Just err_msg -> pure . Left $
      text "Cannot set breakpoint on" <+> quotes (text inp)
      <> text ":" <+> err_msg
    Nothing -> do
      -- No errors found, go and return the module info
      let mod = fromMaybe (panic "resolveFunctionBreakpoint") mb_mod
      mb_mod_info  <- getModuleInfo mod
      case mb_mod_info of
        Nothing -> pure . Left $
          text "Could not find ModuleInfo of " <> ppr mod
        Just mod_info -> pure $ Right (mod, mod_info, fun_str)
  where
    -- Try to lookup the module for an identifier that is in scope.
    -- `parseName` throws an exception, if the identifier is not in scope
    lookupModuleInscope :: GhcMonad m => String -> m (Maybe Module)
    lookupModuleInscope mod_top_lvl = do
        names <- parseName mod_top_lvl
        pure $ Just $ NE.head $ nameModule <$> names

    -- Lookup the Module of a module name in the module graph
    lookupModuleInGraph :: GhcMonad m => String -> m (Maybe Module)
    lookupModuleInGraph mod_str = do
        graph <- getModuleGraph
        let hmods = ms_mod <$> mgModSummaries graph
        pure $ List.find ((== mod_str) . moduleNameString . moduleName) hmods

    -- Check validity of an identifier to set a breakpoint:
    --  1. The module of the identifier must exist
    --  2. the identifier must be in an interpreted module
    --  3. the ModBreaks array for module `mod` must have an entry
    --     for the function
    validateBP :: GhcMonad m => String -> String -> Maybe Module
                       -> m (Maybe SDoc)
    validateBP mod_str fun_str Nothing = pure $ Just $ quotes (text
        (combineModIdent mod_str (takeWhile (/= '.') fun_str)))
        <+> text "not in scope"
    validateBP _ "" (Just _) = pure $ Just $ text "Function name is missing"
    validateBP _ fun_str (Just modl) = do
        isInterpr <- moduleIsInterpreted modl
        mb_err_msg <- case isInterpr of
          False -> pure $ Just $ text "Module" <+> quotes (ppr modl) <+> text "is not interpreted"
          True -> do
            mb_modbreaks <- getModBreak modl
            let found = case mb_modbreaks of
                  Nothing -> False
                  Just mb -> fun_str `elem` (List.intercalate "." <$> elems (modBreaks_decls mb))
            if found
              then pure Nothing
              else pure $ Just $ text "No breakpoint found for" <+> quotes (text fun_str)
                                  <+> text "in module" <+> quotes (ppr modl)
        pure mb_err_msg

-- | The aim of this function is to find the breakpoints for all the RHSs of
-- the equations corresponding to a binding. So we find all breakpoints
-- for
--   (a) this binder only (it maybe a top-level or a nested declaration)
--   (b) that do not have an enclosing breakpoint
findBreakForBind :: String {-^ Name of bind to break at -} -> ModBreaks -> [(BreakTickIndex, RealSrcSpan)]
findBreakForBind str_name modbreaks = filter (not . enclosed) ticks
  where
    ticks = [ (index, span)
            | (index, decls) <- assocs (modBreaks_decls modbreaks),
              str_name == List.intercalate "." decls,
              RealSrcSpan span _ <- [modBreaks_locs modbreaks ! index] ]
    enclosed (_,sp0) = any subspan ticks
      where subspan (_,sp) = sp /= sp0 &&
                         realSrcSpanStart sp <= realSrcSpanStart sp0 &&
                         realSrcSpanEnd sp0 <= realSrcSpanEnd sp

--------------------------------------------------------------------------------
-- Mapping line numbers to ticks
--------------------------------------------------------------------------------

-- | Maps line numbers to the breakpoint ticks existing at that line for a module.
type TickArray = Array Int [(BreakTickIndex,RealSrcSpan)]

-- | Construct the 'TickArray' for the given module.
makeModuleLineMap :: GhcMonad m => Module -> m (Maybe TickArray)
makeModuleLineMap m = do
  mi <- getModuleInfo m
  return $ mkTickArray . assocs . modBreaks_locs . imodBreaks_modBreaks <$> (modInfoModBreaks =<< mi)
  where
    mkTickArray :: [(BreakTickIndex, SrcSpan)] -> TickArray
    mkTickArray ticks
      = accumArray (flip (:)) [] (1, max_line)
            [ (line, (nm,pan)) | (nm,RealSrcSpan pan _) <- ticks, line <- srcSpanLines pan ]
        where
            max_line = foldr max 0 [ srcSpanEndLine sp | (_, RealSrcSpan sp _) <- ticks ]
            srcSpanLines pan = [ srcSpanStartLine pan ..  srcSpanEndLine pan ]

-- | Get the 'ModBreaks' of the given 'Module' when available
getModBreak :: GhcMonad m => Module -> m (Maybe ModBreaks)
getModBreak m = do
   mod_info <- fromMaybe (panic "getModBreak") <$> getModuleInfo m
   pure $ imodBreaks_modBreaks <$> modInfoModBreaks mod_info

--------------------------------------------------------------------------------
-- Mapping source-level BreakpointIds to IBI occurrences
-- (See Note [Breakpoint identifiers])
--------------------------------------------------------------------------------

-- | A source-level breakpoint may have been inlined into many occurrences, now
-- referred by 'InternalBreakpointId'. When a breakpoint is set on a certain
-- source breakpoint, it means all *ocurrences* of that breakpoint across
-- modules should be stopped at -- hence we keep a trie from BreakpointId to
-- the list of internal break ids using it.
-- See also Note [Breakpoint identifiers]
type BreakpointOccurrences = ModuleEnv (IntMap.IntMap [InternalBreakpointId])

-- | Lookup all InternalBreakpointIds matching the given BreakpointId
-- Nothing if BreakpointId not in map
lookupBreakpointOccurrences :: BreakpointOccurrences -> BreakpointId -> Maybe [InternalBreakpointId]
lookupBreakpointOccurrences bmp (BreakpointId md tick) =
  lookupModuleEnv bmp md >>= IntMap.lookup tick

-- | Construct a mapping from Source 'BreakpointId's to 'InternalBreakpointId's from the given list of 'ModInfo's
mkBreakpointOccurrences :: forall m. GhcMonad m => m BreakpointOccurrences
mkBreakpointOccurrences = do
  hug <- hsc_HUG <$> getSession
  liftIO $ foldr go (pure emptyModuleEnv) hug
  where
    go :: HUG.HomeUnitEnv -> IO BreakpointOccurrences -> IO BreakpointOccurrences
    go hue mbmp = do
      bmp <- mbmp
      ibrkss <- HPT.concatHpt (\hmi -> maybeToList (getModBreaks hmi))
                             (HUG.homeUnitEnv_hpt hue)
      return $ foldr addBreakToMap bmp ibrkss

    addBreakToMap :: InternalModBreaks -> BreakpointOccurrences -> BreakpointOccurrences
    addBreakToMap ibrks bmp0 = do
      let imod = modBreaks_module $ imodBreaks_modBreaks ibrks
      IntMap.foldrWithKey (\info_ix cgi bmp -> do
          let ibi = InternalBreakpointId imod info_ix
          case cgb_tick_id cgi of
            Right (BreakpointId tick_mod tick_ix)
              -> extendModuleEnvWith (IntMap.unionWith (S.<>)) bmp tick_mod (IntMap.singleton tick_ix [ibi])
            Left _
              -- Do not include internal breakpoints in the visible breakpoint
              -- occurrences!
              -> bmp
        ) bmp0 (imodBreaks_breakInfo ibrks)

--------------------------------------------------------------------------------
-- Getting current breakpoint information
--------------------------------------------------------------------------------

getCurrentBreakSpan :: GhcMonad m => m (Maybe SrcSpan)
getCurrentBreakSpan = do
  hug <- hsc_HUG <$> getSession
  resumes <- getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = resumeHistoryIx r
        if ix == 0
           then return (Just (resumeSpan r))
           else do
                let hist = resumeHistory r !! (ix-1)
                pan <- liftIO $ getHistorySpan hug hist
                return (Just pan)

getCurrentBreakModule :: GhcMonad m => m (Maybe Module)
getCurrentBreakModule = do
  resumes <- getResumeContext
  hug <- hsc_HUG <$> getSession
  liftIO $ case resumes of
    [] -> pure Nothing
    (r:_) -> case resumeHistoryIx r of
      0  -> case resumeBreakpointId r of
        Nothing -> pure Nothing
        Just ibi -> do
          brks <- readIModBreaks hug ibi
          return $ Just $ getBreakSourceMod ibi brks
      ix ->
          Just <$> getHistoryModule hug (resumeHistory r !! (ix-1))

