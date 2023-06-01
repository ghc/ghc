-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Ids
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--  Module to deal with JS identifiers
-----------------------------------------------------------------------------

module GHC.StgToJS.Ids
  ( freshUnique
  , freshIdent
  , makeIdentForId
  , cachedIdentForId
  -- * Helpers for Idents
  , identForId
  , identForIdN
  , identsForId
  , identForEntryId
  , identForDataConEntryId
  , identForDataConWorker
  -- * Helpers for variables
  , varForId
  , varForIdN
  , varsForId
  , varForEntryId
  , varForDataConEntryId
  , varForDataConWorker
  , declVarsForId
  )
where

import GHC.Prelude

import GHC.StgToJS.Types
import GHC.StgToJS.Monad
import GHC.StgToJS.Utils
import GHC.StgToJS.Symbols

import GHC.JS.Unsat.Syntax
import GHC.JS.Make

import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Data.FastString
import GHC.Data.FastMutInt

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map  as M
import Data.Maybe
import qualified Data.ByteString.Char8 as BSC

-- | Get fresh unique number
freshUnique :: G Int
freshUnique = do
  id_gen <- State.gets gsId
  liftIO $ do
    -- no need for atomicFetchAdd as we don't use threads in G
    v <- readFastMutInt id_gen
    writeFastMutInt id_gen (v+1)
    pure v

-- | Get fresh local Ident of the form: h$$unit:module_uniq
freshIdent :: G Ident
freshIdent = do
  i <- freshUnique
  mod <- State.gets gsModule
  let !name = mkFreshJsSymbol mod i
  return (TxtI name)


-- | Generate unique Ident for the given ID (uncached!)
--
-- The ident has the following forms:
--
--    global Id: h$unit:module.name[_num][_type_suffix]
--    local Id: h$$unit:module.name[_num][_type_suffix]_uniq
--
-- Note that the string is z-encoded except for "_" delimiters.
--
-- Optional "_type_suffix" can be:
--  - "_e" for IdEntry
--  - "_con_e" for IdConEntry
--
-- Optional "_num" is passed as an argument to this function. It is used for
-- Haskell Ids that require several JS variables: e.g. 64-bit numbers (Word64#,
-- Int64#), Addr#, StablePtr#, unboxed tuples, etc.
--
makeIdentForId :: Id -> Maybe Int -> IdType -> Module -> Ident
makeIdentForId i num id_type current_module = TxtI ident
  where
    exported = isExportedId i
    name     = getName i
    mod
      | exported
      , Just m <- nameModule_maybe name
      = m
      | otherwise
      = current_module

    !ident   = mkFastStringByteString $ mconcat
      [ mkJsSymbolBS exported mod (occNameMangledFS (nameOccName name))

        -------------
        -- suffixes

        -- suffix for Ids represented with more than one JS var ("_0", "_1", etc.)
      , case num of
          Nothing -> mempty
          Just v  -> mconcat [BSC.pack "_", intBS v]

        -- suffix for entry and constructor entry
      , case id_type of
          IdPlain    -> mempty
          IdEntry    -> BSC.pack "_e"
          IdConEntry -> BSC.pack "_con_e"

        -- unique suffix for non-exported Ids
      , if exported
          then mempty
          else let (c,u) = unpkUnique (getUnique i)
               in mconcat [BSC.pack ['_',c,'_'], word64BS u]
      ]

-- | Retrieve the cached Ident for the given Id if there is one. Otherwise make
-- a new one with 'makeIdentForId' and cache it.
cachedIdentForId :: Id -> Maybe Int -> IdType -> G Ident
cachedIdentForId i mi id_type = do

  -- compute key
  let !key = IdKey (getKey . getUnique $ i) (fromMaybe 0 mi) id_type

  -- lookup Ident in the Ident cache
  IdCache cache <- State.gets gsIdents
  ident <- case M.lookup key cache of
    Just ident -> pure ident
    Nothing -> do
      mod <- State.gets gsModule
      let !ident  = makeIdentForId i mi id_type mod
      let !cache' = IdCache (M.insert key ident cache)
      State.modify (\s -> s { gsIdents = cache' })
      pure ident

  -- Now update the GlobalId cache, if required

  let update_global_cache = isGlobalId i && isNothing mi && id_type == IdPlain
      -- fixme also allow caching entries for lifting?

  when (update_global_cache) $ do
    GlobalIdCache gidc <- getGlobalIdCache
    case elemUFM ident gidc of
      False -> setGlobalIdCache $ GlobalIdCache (addToUFM gidc ident (key, i))
      True  -> pure ()

  pure ident

-- | Retrieve default Ident for the given Id
identForId :: Id -> G Ident
identForId i = cachedIdentForId i Nothing IdPlain

-- | Retrieve default Ident for the given Id with sub index
--
-- Some types, Word64, Addr#, unboxed tuple have more than one corresponding JS
-- var, hence we use the sub index to identify each subpart / JS variable.
identForIdN :: Id -> Int -> G Ident
identForIdN i n = cachedIdentForId i (Just n) IdPlain

-- | Retrieve all the idents for the given Id.
identsForId :: Id -> G [Ident]
identsForId i = case typeSize (idType i) of
  0 -> pure mempty
  1 -> (:[]) <$> identForId i
  s -> mapM (identForIdN i) [1..s]


-- | Retrieve entry Ident for the given Id
identForEntryId :: Id -> G Ident
identForEntryId i = cachedIdentForId i Nothing IdEntry

-- | Retrieve datacon entry Ident for the given Id
--
-- Different name than the datacon wrapper.
identForDataConEntryId :: Id -> G Ident
identForDataConEntryId i = cachedIdentForId i Nothing IdConEntry


-- | Retrieve default variable name for the given Id
varForId :: Id -> G JExpr
varForId i = toJExpr <$> identForId i

-- | Retrieve default variable name for the given Id with sub index
varForIdN :: Id -> Int -> G JExpr
varForIdN i n = toJExpr <$> identForIdN i n

-- | Retrieve all the JS vars for the given Id
varsForId :: Id -> G [JExpr]
varsForId i = case typeSize (idType i) of
  0 -> pure mempty
  1 -> (:[]) <$> varForId i
  s -> mapM (varForIdN i) [1..s]


-- | Retrieve entry variable name for the given Id
varForEntryId :: Id -> G JExpr
varForEntryId i = toJExpr <$> identForEntryId i

-- | Retrieve datacon entry variable name for the given Id
varForDataConEntryId :: Id -> G JExpr
varForDataConEntryId i = ValExpr . JVar <$> identForDataConEntryId i


-- | Retrieve datacon worker entry variable name for the given datacon
identForDataConWorker :: DataCon -> G Ident
identForDataConWorker d = identForDataConEntryId (dataConWorkId d)

-- | Retrieve datacon worker entry variable name for the given datacon
varForDataConWorker :: DataCon -> G JExpr
varForDataConWorker d = varForDataConEntryId (dataConWorkId d)

-- | Declare all js vars for the id
declVarsForId :: Id -> G JStat
declVarsForId  i = case typeSize (idType i) of
  0 -> return mempty
  1 -> decl <$> identForId i
  s -> mconcat <$> mapM (\n -> decl <$> identForIdN i n) [1..s]

