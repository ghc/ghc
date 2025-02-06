{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | The SymbolCache is used to cache lookups for specific symbols when using
-- the interpreter.
module GHC.Runtime.Interpreter.Types.SymbolCache (
     InterpSymbolCache(..)
   , mkInterpSymbolCache
   , lookupInterpSymbolCache
   , updateInterpSymbolCache
   , purgeInterpSymbolCache
   , InterpSymbol(..)
   , SuffixOrInterpreted(..)
   , interpSymbolName
   , interpSymbolSuffix
   , eliminateInterpSymbol
   , interpretedInterpSymbol
   ) where

import GHC.Prelude

import GHC.Types.Unique.FM
import GHC.Types.Name
import GHC.Data.FastString
import Foreign

import Control.Concurrent
import GHC.Utils.Outputable
import GHC.TypeLits


-- The symbols records the suffix which each cache deals with.
newtype SymbolCache (s :: Symbol) = SymbolCache { _getSymbolCache :: UniqFM Name (Ptr ()) }

-- Each cache is keyed by Name, there is one cache for each type of symbol we will
-- potentially lookup. The caches are keyed by 'Name' so that it is not necessary to consult
-- a complicated `FastString` each time.
data InterpSymbolCache = InterpSymbolCache {
        interpClosureCache :: MVar (SymbolCache "closure")
      , interpConInfoCache :: MVar (SymbolCache "con_info")
      , interpStaticInfoCache :: MVar (SymbolCache "static_info")
      , interpBytesCache   :: MVar (SymbolCache "bytes")
      , interpFaststringCache   :: MVar (UniqFM FastString (Ptr ()))
      }

data SuffixOrInterpreted = Suffix Symbol | Interpreted

data InterpSymbol (s :: SuffixOrInterpreted) where
  IClosureSymbol :: Name -> InterpSymbol (Suffix "closure")
  IConInfoSymbol :: Name -> InterpSymbol (Suffix "con_info")
  IStaticInfoSymbol :: Name -> InterpSymbol (Suffix "static_info")
  IBytesSymbol   :: Name -> InterpSymbol (Suffix "bytes")
  IFaststringSymbol   :: FastString -> InterpSymbol Interpreted

instance Outputable (InterpSymbol s) where
  ppr s = eliminateInterpSymbol s
            (\(IFaststringSymbol s) -> text "interpreted:" <> ppr s)
            (\s -> text (interpSymbolSuffix s) <> colon <> ppr (interpSymbolName s))

eliminateInterpSymbol :: InterpSymbol s -> (InterpSymbol Interpreted -> r)
                                        -> (forall x . InterpSymbol (Suffix x) -> r)
                                        -> r
eliminateInterpSymbol s k1 k2 =
  case s of
    IFaststringSymbol {} -> k1 s
    IBytesSymbol {}      -> k2 s
    IStaticInfoSymbol {} -> k2 s
    IConInfoSymbol {}    -> k2 s
    IClosureSymbol {}    -> k2 s


interpSymbolName :: InterpSymbol (Suffix s) -> Name
interpSymbolName (IClosureSymbol n) = n
interpSymbolName (IConInfoSymbol n) = n
interpSymbolName (IStaticInfoSymbol n) = n
interpSymbolName (IBytesSymbol n) = n

interpretedInterpSymbol :: InterpSymbol Interpreted -> FastString
interpretedInterpSymbol (IFaststringSymbol s) = s

interpSymbolSuffix :: InterpSymbol (Suffix s) -> String
interpSymbolSuffix (IClosureSymbol {}) = "closure"
interpSymbolSuffix (IConInfoSymbol {}) = "con_info"
interpSymbolSuffix (IStaticInfoSymbol {}) = "static_info"
interpSymbolSuffix (IBytesSymbol {})      = "bytes"

emptySymbolCache :: SymbolCache s
emptySymbolCache = SymbolCache emptyUFM

lookupSymbolCache :: InterpSymbol (Suffix s) -> SymbolCache s -> Maybe (Ptr ())
lookupSymbolCache s (SymbolCache cache) = lookupUFM cache (interpSymbolName s)

insertSymbolCache :: InterpSymbol (Suffix s) -> Ptr () -> SymbolCache s -> SymbolCache s
insertSymbolCache s v (SymbolCache cache) = SymbolCache (addToUFM cache (interpSymbolName s) v)

lookupInterpSymbolCache :: InterpSymbol s -> InterpSymbolCache -> IO (Maybe (Ptr ()))
lookupInterpSymbolCache = withInterpSymbolCache
                            (\(IFaststringSymbol f) mvar_var -> (\cache -> lookupUFM cache f) <$> readMVar mvar_var)
                            (\s mvar_var -> lookupSymbolCache s <$> readMVar mvar_var)


updateInterpSymbolCache :: InterpSymbol s
                                 -> InterpSymbolCache -> Ptr () -> IO ()
updateInterpSymbolCache = withInterpSymbolCache
                            (\(IFaststringSymbol f) mvar_var v -> modifyMVar_ mvar_var (\cache -> pure $ addToUFM cache f v))
                            (\s mvar_var v -> modifyMVar_ mvar_var (\cache -> pure $ insertSymbolCache s v cache))

withInterpSymbolCache ::
                (InterpSymbol Interpreted -> MVar (UniqFM FastString (Ptr ())) -> r)
                -> (forall x . InterpSymbol (Suffix x) -> MVar (SymbolCache x) -> r)
                -> InterpSymbol s
                -> InterpSymbolCache
                -> r
withInterpSymbolCache k1 k2 key InterpSymbolCache{..} =
  case key of
    IClosureSymbol {} -> k2 key interpClosureCache
    IConInfoSymbol {} -> k2 key interpConInfoCache
    IStaticInfoSymbol {} -> k2 key interpStaticInfoCache
    IBytesSymbol {} -> k2 key interpBytesCache
    IFaststringSymbol {} -> k1 key interpFaststringCache

-- | Clear all symbol caches.
purgeInterpSymbolCache :: InterpSymbolCache -> IO ()
purgeInterpSymbolCache (InterpSymbolCache a b c d e) = do
  modifyMVar_ a (\_ ->  do
    modifyMVar_ b (\_ -> do
      modifyMVar_ c (\_ -> do
        modifyMVar_ d (\_ -> do
          modifyMVar_ e (\_ -> pure emptyUFM)
          pure emptySymbolCache)
        pure emptySymbolCache)
      pure emptySymbolCache)
    pure emptySymbolCache)

mkInterpSymbolCache :: IO InterpSymbolCache
mkInterpSymbolCache = do
  InterpSymbolCache <$> newMVar emptySymbolCache
                    <*> newMVar emptySymbolCache
                    <*> newMVar emptySymbolCache
                    <*> newMVar emptySymbolCache
                    <*> newMVar emptyUFM
