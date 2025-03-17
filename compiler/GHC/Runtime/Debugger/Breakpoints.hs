-- | GHC API debugger module for finding and setting breakpoints.
--
-- This module is user facing and is at least used by `GHCi` and `ghc-debugger`
-- to find and set breakpoints.
module GHC.Runtime.Debugger.Breakpoints where

import Control.Monad.Catch
import Control.Monad
import Data.Array
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as S

import GHC
import GHC.Prelude
import GHC.Runtime.Eval.Utils
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified GHC.Data.Strict as Strict

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
findBreakByLine :: Int {-^ Line number -} -> TickArray -> Maybe (BreakIndex, RealSrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (leftmostLargestRealSrcSpan `on` snd)  comp)   `mplus`
    listToMaybe (sortBy (compare `on` snd) incomp) `mplus`
    listToMaybe (sortBy (flip compare `on` snd) ticks)
  where
        ticks = arr ! line

        starts_here = [ (ix,pan) | (ix, pan) <- ticks,
                        GHC.srcSpanStartLine pan == line ]

        (comp, incomp) = partition ends_here starts_here
            where ends_here (_,pan) = GHC.srcSpanEndLine pan == line

-- | Find a breakpoint in the 'TickArray' of a module, given a line number and a column coordinate.
findBreakByCoord :: (Int, Int) -> TickArray -> Maybe (BreakIndex, RealSrcSpan)
findBreakByCoord (line, col) arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (flip compare `on` snd) contains ++
                 sortBy (compare `on` snd) after_here)
  where
        ticks = arr ! line

        -- the ticks that span this coordinate
        contains = [ tick | tick@(_,pan) <- ticks, RealSrcSpan pan Strict.Nothing `spans` (line,col) ]

        after_here = [ tick | tick@(_,pan) <- ticks,
                              GHC.srcSpanStartLine pan == line,
                              GHC.srcSpanStartCol pan >= col ]

leftmostLargestRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> Ordering
leftmostLargestRealSrcSpan = on compare realSrcSpanStart S.<> on (flip compare) realSrcSpanEnd

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
      mb_mod_info  <- GHC.getModuleInfo mod
      case mb_mod_info of
        Nothing -> pure . Left $
          text "Could not find ModuleInfo of " <> ppr mod
        Just mod_info -> pure $ Right (mod, mod_info, fun_str)
  where
    -- Try to lookup the module for an identifier that is in scope.
    -- `parseName` throws an exception, if the identifier is not in scope
    lookupModuleInscope :: GHC.GhcMonad m => String -> m (Maybe Module)
    lookupModuleInscope mod_top_lvl = do
        names <- GHC.parseName mod_top_lvl
        pure $ Just $ NE.head $ GHC.nameModule <$> names

    -- Lookup the Module of a module name in the module graph
    lookupModuleInGraph :: GHC.GhcMonad m => String -> m (Maybe Module)
    lookupModuleInGraph mod_str = do
        graph <- GHC.getModuleGraph
        let hmods = ms_mod <$> GHC.mgModSummaries graph
        pure $ find ((== mod_str) . moduleNameString . moduleName) hmods

    -- Check validity of an identifier to set a breakpoint:
    --  1. The module of the identifier must exist
    --  2. the identifier must be in an interpreted module
    --  3. the ModBreaks array for module `mod` must have an entry
    --     for the function
    validateBP :: GHC.GhcMonad m => String -> String -> Maybe Module
                       -> m (Maybe SDoc)
    validateBP mod_str fun_str Nothing = pure $ Just $ quotes (text
        (combineModIdent mod_str (takeWhile (/= '.') fun_str)))
        <+> text "not in scope"
    validateBP _ "" (Just _) = pure $ Just $ text "Function name is missing"
    validateBP _ fun_str (Just modl) = do
        isInterpr <- GHC.moduleIsInterpreted modl
        (_, decls) <- getModBreak modl
        mb_err_msg <- case isInterpr of
          False -> pure $ Just $ text "Module" <+> quotes (ppr modl)
                        <+> text "is not interpreted"
          True -> case fun_str `elem` (intercalate "." <$> elems decls) of
                False -> pure $ Just $
                   text "No breakpoint found for" <+> quotes (text fun_str)
                   <+> text "in module" <+> quotes (ppr modl)
                True  -> pure Nothing
        pure mb_err_msg

-- | The aim of this function is to find the breakpoints for all the RHSs of
-- the equations corresponding to a binding. So we find all breakpoints
-- for
--   (a) this binder only (it maybe a top-level or a nested declaration)
--   (b) that do not have an enclosing breakpoint
findBreakForBind :: String {-^ Name of bind to break at -} -> GHC.ModBreaks -> [(BreakIndex, RealSrcSpan)]
findBreakForBind str_name modbreaks = filter (not . enclosed) ticks
  where
    ticks = [ (index, span)
            | (index, decls) <- assocs (GHC.modBreaks_decls modbreaks),
              str_name == intercalate "." decls,
              RealSrcSpan span _ <- [GHC.modBreaks_locs modbreaks ! index] ]
    enclosed (_,sp0) = any subspan ticks
      where subspan (_,sp) = sp /= sp0 &&
                         realSrcSpanStart sp <= realSrcSpanStart sp0 &&
                         realSrcSpanEnd sp0 <= realSrcSpanEnd sp

--------------------------------------------------------------------------------
-- Mapping line numbers to ticks
--------------------------------------------------------------------------------

-- | Maps line numbers to the breakpoint ticks existing at that line for a module.
type TickArray = Array Int [(GHC.BreakIndex,RealSrcSpan)]

-- | Construct the 'TickArray' for the given module.
makeModuleLineMap :: GhcMonad m => Module -> m (Maybe TickArray)
makeModuleLineMap m = do
  mi <- GHC.getModuleInfo m
  return $
    mkTickArray . assocs . GHC.modBreaks_locs . GHC.modInfoModBreaks <$> mi
  where
    mkTickArray :: [(BreakIndex, SrcSpan)] -> TickArray
    mkTickArray ticks
      = accumArray (flip (:)) [] (1, max_line)
            [ (line, (nm,pan)) | (nm,RealSrcSpan pan _) <- ticks, line <- srcSpanLines pan ]
        where
            max_line = foldr max 0 [ GHC.srcSpanEndLine sp | (_, RealSrcSpan sp _) <- ticks ]
            srcSpanLines pan = [ GHC.srcSpanStartLine pan ..  GHC.srcSpanEndLine pan ]

-- | Get the 'modBreaks_locs' and 'modBreaks_decls' of the given 'Module'
getModBreak :: GHC.GhcMonad m
            => Module -> m (Array Int SrcSpan, Array Int [String])
getModBreak m = do
   mod_info      <- fromMaybe (panic "getModBreak") <$> GHC.getModuleInfo m
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let ticks      = GHC.modBreaks_locs  modBreaks
   let decls      = GHC.modBreaks_decls modBreaks
   return (ticks, decls)



