{-# OPTIONS_GHC -O #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

--
--  (c) The University of Glasgow 2002-2006
--
-- Binary interface file support.

module BinIface ( writeBinIface, readBinIface,
                  CheckHiWay(..), TraceBinIFaceReading(..) ) where

#include "HsVersions.h"

import TcRnMonad
import IfaceEnv
import HscTypes
import BasicTypes
import Demand
import Annotations
import IfaceSyn
import Module
import Name
import Avail
import VarEnv
import DynFlags
import UniqFM
import UniqSupply
import CostCentre
import StaticFlags
import Panic
import Binary
import SrcLoc
import ErrUtils
import Config
import FastMutInt
import Unique
import Outputable
import FastString
import Constants

import Data.List
import Data.Word
import Data.Array
import Data.IORef
import Control.Monad

data CheckHiWay = CheckHiWay | IgnoreHiWay
    deriving Eq

data TraceBinIFaceReading = TraceBinIFaceReading | QuietBinIFaceReading
    deriving Eq

-- ---------------------------------------------------------------------------
-- Reading and writing binary interface files

readBinIface :: CheckHiWay -> TraceBinIFaceReading -> FilePath
             -> TcRnIf a b ModIface
readBinIface checkHiWay traceBinIFaceReading hi_path = do
  update_nc <- mkNameCacheUpdater
  dflags <- getDOpts
  liftIO $ readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path update_nc

readBinIface_ :: DynFlags -> CheckHiWay -> TraceBinIFaceReading -> FilePath
              -> NameCacheUpdater (Array Int Name)
              -> IO ModIface
readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path update_nc = do
  let printer :: SDoc -> IO ()
      printer = case traceBinIFaceReading of
                TraceBinIFaceReading -> \sd -> printSDoc sd defaultDumpStyle
                QuietBinIFaceReading -> \_ -> return ()
      wantedGot :: Outputable a => String -> a -> a -> IO ()
      wantedGot what wanted got
          = printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr wanted <> text ",",
                           text "got    " <> ppr got])

      errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
      errorOnMismatch what wanted got
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
          = when (wanted /= got) $ ghcError $ ProgramError
                        (what ++ " (wanted " ++ show wanted
                              ++ ", got "    ++ show got ++ ")")
  bh <- Binary.readBinMem hi_path

        -- Read the magic number to check that this really is a GHC .hi file
        -- (This magic number does not change when we change
        --  GHC interface file format)
  magic <- get bh
  wantedGot "Magic" binaryInterfaceMagic magic
  errorOnMismatch "magic number mismatch: old/corrupt interface file?"
      binaryInterfaceMagic magic

        -- Note [dummy iface field]
        -- read a dummy 32/64 bit value.  This field used to hold the
        -- dictionary pointer in old interface file formats, but now
        -- the dictionary pointer is after the version (where it
        -- should be).  Also, the serialisation of value of type "Bin
        -- a" used to depend on the word size of the machine, now they
        -- are always 32 bits.
        --
  if wORD_SIZE == 4
     then do _ <- Binary.get bh :: IO Word32; return ()
     else do _ <- Binary.get bh :: IO Word64; return ()

        -- Check the interface file version and ways.
  check_ver  <- get bh
  let our_ver = show opt_HiVersion
  wantedGot "Version" our_ver check_ver
  errorOnMismatch "mismatched interface file versions" our_ver check_ver

  check_way <- get bh
  let way_descr = getWayDescr dflags
  wantedGot "Way" way_descr check_way
  when (checkHiWay == CheckHiWay) $
       errorOnMismatch "mismatched interface file ways" way_descr check_way

        -- Read the dictionary
        -- The next word in the file is a pointer to where the dictionary is
        -- (probably at the end of the file)
  dict_p <- Binary.get bh
  data_p <- tellBin bh          -- Remember where we are now
  seekBin bh dict_p
  dict <- getDictionary bh
  seekBin bh data_p             -- Back to where we were before

        -- Initialise the user-data field of bh
  ud <- newReadState dict
  bh <- return (setUserData bh ud)
        
  symtab_p <- Binary.get bh     -- Get the symtab ptr
  data_p <- tellBin bh          -- Remember where we are now
  seekBin bh symtab_p
  symtab <- getSymbolTable bh update_nc
  seekBin bh data_p             -- Back to where we were before
  let ud = getUserData bh
  bh <- return $! setUserData bh ud{ud_symtab = symtab}
  iface <- get bh
  return iface


writeBinIface :: DynFlags -> FilePath -> ModIface -> IO ()
writeBinIface dflags hi_path mod_iface = do
  bh <- openBinMem initBinMemSize
  put_ bh binaryInterfaceMagic

       -- dummy 32/64-bit field before the version/way for
       -- compatibility with older interface file formats.
       -- See Note [dummy iface field] above.
  if wORD_SIZE == 4
     then Binary.put_ bh (0 :: Word32)
     else Binary.put_ bh (0 :: Word64)

        -- The version and way descriptor go next
  put_ bh (show opt_HiVersion)
  let way_descr = getWayDescr dflags
  put_  bh way_descr

	-- Remember where the dictionary pointer will go
  dict_p_p <- tellBin bh
  put_ bh dict_p_p	-- Placeholder for ptr to dictionary

        -- Remember where the symbol table pointer will go
  symtab_p_p <- tellBin bh
  put_ bh symtab_p_p

	-- Make some intial state
  symtab_next <- newFastMutInt
  writeFastMutInt symtab_next 0
  symtab_map <- newIORef emptyUFM
  let bin_symtab = BinSymbolTable {
                      bin_symtab_next = symtab_next,
                      bin_symtab_map  = symtab_map }
  dict_next_ref <- newFastMutInt
  writeFastMutInt dict_next_ref 0
  dict_map_ref <- newIORef emptyUFM
  let bin_dict = BinDictionary {
                      bin_dict_next = dict_next_ref,
                      bin_dict_map  = dict_map_ref }
  ud <- newWriteState (putName bin_symtab) (putFastString bin_dict)

	-- Put the main thing, 
  bh <- return $ setUserData bh ud
  put_ bh mod_iface

	-- Write the symtab pointer at the fornt of the file
  symtab_p <- tellBin bh	        -- This is where the symtab will start
  putAt bh symtab_p_p symtab_p	-- Fill in the placeholder
  seekBin bh symtab_p		-- Seek back to the end of the file

        -- Write the symbol table itself
  symtab_next <- readFastMutInt symtab_next
  symtab_map  <- readIORef symtab_map
  putSymbolTable bh symtab_next symtab_map
  debugTraceMsg dflags 3 (text "writeBinIface:" <+> int symtab_next 
                                <+> text "Names")

        -- NB. write the dictionary after the symbol table, because
        -- writing the symbol table may create more dictionary entries.

	-- Write the dictionary pointer at the fornt of the file
  dict_p <- tellBin bh	        -- This is where the dictionary will start
  putAt bh dict_p_p dict_p	-- Fill in the placeholder
  seekBin bh dict_p		-- Seek back to the end of the file

	-- Write the dictionary itself
  dict_next <- readFastMutInt dict_next_ref
  dict_map  <- readIORef dict_map_ref
  putDictionary bh dict_next dict_map
  debugTraceMsg dflags 3 (text "writeBinIface:" <+> int dict_next
                                 <+> text "dict entries")

	-- And send the result to the file
  writeBinMem bh hi_path

initBinMemSize :: Int
initBinMemSize = 1024 * 1024

-- The *host* architecture version:
#include "../includes/MachDeps.h"

binaryInterfaceMagic :: Word32
#if   WORD_SIZE_IN_BITS == 32
binaryInterfaceMagic = 0x1face
#elif WORD_SIZE_IN_BITS == 64
binaryInterfaceMagic = 0x1face64
#endif
  
-- -----------------------------------------------------------------------------
-- The symbol table

putSymbolTable :: BinHandle -> Int -> UniqFM (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = elems (array (0,next_off-1) (eltsUFM symtab))
  mapM_ (\n -> serialiseName bh n symtab) names

getSymbolTable :: BinHandle -> NameCacheUpdater (Array Int Name)
               -> IO (Array Int Name)
getSymbolTable bh update_namecache = do
  sz <- get bh
  od_names <- sequence (replicate sz (get bh))
  update_namecache $ \namecache ->
    let
        arr = listArray (0,sz-1) names
        (namecache', names) =    
                mapAccumR (fromOnDiskName arr) namecache od_names
    in (namecache', arr)

type OnDiskName = (PackageId, ModuleName, OccName)

fromOnDiskName
   :: Array Int Name
   -> NameCache
   -> OnDiskName
   -> (NameCache, Name)
fromOnDiskName _ nc (pid, mod_name, occ) =
  let
        mod   = mkModule pid mod_name
        cache = nsNames nc
  in
  case lookupOrigNameCache cache  mod occ of
     Just name -> (nc, name)
     Nothing   ->
        case takeUniqFromSupply (nsUniqs nc) of
        (uniq, us) ->
            let
                name      = mkExternalName uniq mod occ noSrcSpan
                new_cache = extendNameCache cache mod occ name
            in
            ( nc{ nsUniqs = us, nsNames = new_cache }, name )

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name _ = do
  let mod = ASSERT2( isExternalName name, ppr name ) nameModule name
  put_ bh (modulePackageId mod, moduleName mod, nameOccName name)


putName :: BinSymbolTable -> BinHandle -> Name -> IO ()
putName BinSymbolTable{ 
            bin_symtab_map = symtab_map_ref,
            bin_symtab_next = symtab_next }    bh name
  = do
    symtab_map <- readIORef symtab_map_ref
    case lookupUFM symtab_map name of
      Just (off,_) -> put_ bh (fromIntegral off :: Word32)
      Nothing -> do
         off <- readFastMutInt symtab_next
         writeFastMutInt symtab_next (off+1)
         writeIORef symtab_map_ref
             $! addToUFM symtab_map name (off,name)
         put_ bh (fromIntegral off :: Word32)


data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM (Int,Name)))
                                -- indexed by Name
  }


putFastString :: BinDictionary -> BinHandle -> FastString -> IO ()
putFastString BinDictionary { bin_dict_next = j_r,
                              bin_dict_map  = out_r}  bh f
  = do
    out <- readIORef out_r
    let uniq = getUnique f
    case lookupUFM out uniq of
        Just (j, _)  -> put_ bh (fromIntegral j :: Word32)
        Nothing -> do
           j <- readFastMutInt j_r
           put_ bh (fromIntegral j :: Word32)
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM out uniq (j, f)


data BinDictionary = BinDictionary {
        bin_dict_next :: !FastMutInt, -- The next index to use
        bin_dict_map  :: !(IORef (UniqFM (Int,FastString)))
                                -- indexed by FastString
  }

-- -----------------------------------------------------------------------------
-- All the binary instances

-- BasicTypes
{-! for IPName derive: Binary !-}
{-! for Fixity derive: Binary !-}
{-! for FixityDirection derive: Binary !-}
{-! for Boxity derive: Binary !-}
{-! for StrictnessMark derive: Binary !-}
{-! for Activation derive: Binary !-}

-- Demand
{-! for Demand derive: Binary !-}
{-! for Demands derive: Binary !-}
{-! for DmdResult derive: Binary !-}
{-! for StrictSig derive: Binary !-}

-- Class
{-! for DefMeth derive: Binary !-}

-- HsTypes
{-! for HsPred derive: Binary !-}
{-! for HsType derive: Binary !-}
{-! for TupCon derive: Binary !-}
{-! for HsTyVarBndr derive: Binary !-}

-- HsCore
{-! for UfExpr derive: Binary !-}
{-! for UfConAlt derive: Binary !-}
{-! for UfBinding derive: Binary !-}
{-! for UfBinder derive: Binary !-}
{-! for HsIdInfo derive: Binary !-}
{-! for UfNote derive: Binary !-}

-- HsDecls
{-! for ConDetails derive: Binary !-}
{-! for BangType derive: Binary !-}

-- CostCentre
{-! for IsCafCC derive: Binary !-}
{-! for IsDupdCC derive: Binary !-}
{-! for CostCentre derive: Binary !-}



-- ---------------------------------------------------------------------------
-- Reading a binary interface into ParsedIface

instance Binary ModIface where
   put_ bh (ModIface {
		 mi_module    = mod,
		 mi_boot      = is_boot,
		 mi_iface_hash= iface_hash,
		 mi_mod_hash  = mod_hash,
		 mi_orphan    = orphan,
		 mi_finsts    = hasFamInsts,
		 mi_deps      = deps,
		 mi_usages    = usages,
		 mi_exports   = exports,
		 mi_exp_hash  = exp_hash,
                 mi_used_th   = used_th,
                 mi_fixities  = fixities,
		 mi_warns     = warns,
		 mi_anns      = anns,
		 mi_decls     = decls,
		 mi_insts     = insts,
		 mi_fam_insts = fam_insts,
		 mi_rules     = rules,
		 mi_orphan_hash = orphan_hash,
                 mi_vect_info = vect_info,
                 mi_hpc       = hpc_info,
                 mi_trust     = trust,
                 mi_trust_pkg = trust_pkg }) = do
	put_ bh mod
	put_ bh is_boot
	put_ bh iface_hash
	put_ bh mod_hash
	put_ bh orphan
	put_ bh hasFamInsts
	lazyPut bh deps
	lazyPut bh usages
	put_ bh exports
	put_ bh exp_hash
        put_ bh used_th
        put_ bh fixities
	lazyPut bh warns
	lazyPut bh anns
        put_ bh decls
	put_ bh insts
	put_ bh fam_insts
	lazyPut bh rules
	put_ bh orphan_hash
        put_ bh vect_info
	put_ bh hpc_info
	put_ bh trust
	put_ bh trust_pkg

   get bh = do
	mod_name  <- get bh
	is_boot   <- get bh
	iface_hash <- get bh
	mod_hash  <- get bh
	orphan    <- get bh
	hasFamInsts <- get bh
	deps	  <- lazyGet bh
	usages	  <- {-# SCC "bin_usages" #-} lazyGet bh
	exports	  <- {-# SCC "bin_exports" #-} get bh
	exp_hash  <- get bh
        used_th   <- get bh
        fixities  <- {-# SCC "bin_fixities" #-} get bh
	warns     <- {-# SCC "bin_warns" #-} lazyGet bh
	anns      <- {-# SCC "bin_anns" #-} lazyGet bh
        decls 	  <- {-# SCC "bin_tycldecls" #-} get bh
	insts     <- {-# SCC "bin_insts" #-} get bh
	fam_insts <- {-# SCC "bin_fam_insts" #-} get bh
	rules	  <- {-# SCC "bin_rules" #-} lazyGet bh
	orphan_hash <- get bh
        vect_info <- get bh
        hpc_info  <- get bh
        trust     <- get bh
        trust_pkg <- get bh
	return (ModIface {
		 mi_module    = mod_name,
		 mi_boot      = is_boot,
		 mi_iface_hash = iface_hash,
		 mi_mod_hash  = mod_hash,
		 mi_orphan    = orphan,
		 mi_finsts    = hasFamInsts,
		 mi_deps      = deps,
		 mi_usages    = usages,
		 mi_exports   = exports,
                 mi_exp_hash  = exp_hash,
                 mi_used_th   = used_th,
                 mi_anns      = anns,
		 mi_fixities  = fixities,
		 mi_warns     = warns,
		 mi_decls     = decls,
		 mi_globals   = Nothing,
		 mi_insts     = insts,
		 mi_fam_insts = fam_insts,
		 mi_rules     = rules,
		 mi_orphan_hash = orphan_hash,
                 mi_vect_info = vect_info,
		 mi_hpc       = hpc_info,
		 mi_trust     = trust,
		 mi_trust_pkg = trust_pkg,
			-- And build the cached values
		 mi_warn_fn   = mkIfaceWarnCache warns,
		 mi_fix_fn    = mkIfaceFixCache fixities,
		 mi_hash_fn   = mkIfaceHashCache decls })

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | cGhcUnregisterised == "YES" = 'u':tag
  | otherwise                   = tag
  where tag = buildTag dflags
	-- if this is an unregisterised build, make sure our interfaces
	-- can't be used by a registerised build.

-------------------------------------------------------------------------
--		Types from: HscTypes
-------------------------------------------------------------------------

instance Binary Dependencies where
    put_ bh deps = do put_ bh (dep_mods deps)
		      put_ bh (dep_pkgs deps)
		      put_ bh (dep_orphs deps)
		      put_ bh (dep_finsts deps)

    get bh = do ms <- get bh 
		ps <- get bh
		os <- get bh
		fis <- get bh
		return (Deps { dep_mods = ms, dep_pkgs = ps, dep_orphs = os,
			       dep_finsts = fis })

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (AvailTC ab ac) = do
	    putByte bh 1
	    put_ bh ab
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (Avail aa)
	      _ -> do ab <- get bh
		      ac <- get bh
		      return (AvailTC ab ac)

instance Binary Usage where
    put_ bh usg@UsagePackageModule{} = do 
        putByte bh 0
	put_ bh (usg_mod usg)
	put_ bh (usg_mod_hash usg)
	put_ bh (usg_safe     usg)
    put_ bh usg@UsageHomeModule{} = do 
        putByte bh 1
	put_ bh (usg_mod_name usg)
	put_ bh (usg_mod_hash usg)
	put_ bh (usg_exports  usg)
	put_ bh (usg_entities usg)
	put_ bh (usg_safe     usg)

    get bh = do
        h <- getByte bh
        case h of
          0 -> do
            nm    <- get bh
            mod   <- get bh
            safe  <- get bh
            return UsagePackageModule { usg_mod = nm, usg_mod_hash = mod, usg_safe = safe }
          _ -> do
            nm    <- get bh
            mod   <- get bh
            exps  <- get bh
            ents  <- get bh
            safe  <- get bh
            return UsageHomeModule { usg_mod_name = nm, usg_mod_hash = mod,
                     usg_exports = exps, usg_entities = ents, usg_safe = safe }

instance Binary Warnings where
    put_ bh NoWarnings     = putByte bh 0
    put_ bh (WarnAll t) = do
            putByte bh 1
            put_ bh t
    put_ bh (WarnSome ts) = do
            putByte bh 2
            put_ bh ts

    get bh = do
            h <- getByte bh
            case h of
              0 -> return NoWarnings
              1 -> do aa <- get bh
                      return (WarnAll aa)
              _ -> do aa <- get bh
                      return (WarnSome aa)

instance Binary WarningTxt where
    put_ bh (WarningTxt w) = do
            putByte bh 0
            put_ bh w
    put_ bh (DeprecatedTxt d) = do
            putByte bh 1
            put_ bh d

    get bh = do
            h <- getByte bh
            case h of
              0 -> do w <- get bh
                      return (WarningTxt w)
              _ -> do d <- get bh
                      return (DeprecatedTxt d)

-------------------------------------------------------------------------
--		Types from: BasicTypes
-------------------------------------------------------------------------

instance Binary Activation where
    put_ bh NeverActive = do
	    putByte bh 0
    put_ bh AlwaysActive = do
	    putByte bh 1
    put_ bh (ActiveBefore aa) = do
	    putByte bh 2
	    put_ bh aa
    put_ bh (ActiveAfter ab) = do
	    putByte bh 3
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return NeverActive
	      1 -> do return AlwaysActive
	      2 -> do aa <- get bh
		      return (ActiveBefore aa)
	      _ -> do ab <- get bh
		      return (ActiveAfter ab)

instance Binary RuleMatchInfo where
    put_ bh FunLike = putByte bh 0
    put_ bh ConLike = putByte bh 1
    get bh = do
            h <- getByte bh
            if h == 1 then return ConLike
                      else return FunLike

instance Binary InlinePragma where
    put_ bh (InlinePragma a b c d) = do
            put_ bh a
            put_ bh b
            put_ bh c
            put_ bh d

    get bh = do
           a <- get bh
           b <- get bh
           c <- get bh
           d <- get bh
           return (InlinePragma a b c d)

instance Binary InlineSpec where
    put_ bh EmptyInlineSpec = putByte bh 0
    put_ bh Inline          = putByte bh 1
    put_ bh Inlinable       = putByte bh 2
    put_ bh NoInline        = putByte bh 3

    get bh = do h <- getByte bh
                case h of
                  0 -> return EmptyInlineSpec
                  1 -> return Inline
                  2 -> return Inlinable
                  _ -> return NoInline

instance Binary HsBang where
    put_ bh HsNoBang        = putByte bh 0
    put_ bh HsStrict        = putByte bh 1
    put_ bh HsUnpack        = putByte bh 2
    put_ bh HsUnpackFailed  = putByte bh 3
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return HsNoBang
	      1 -> do return HsStrict
	      2 -> do return HsUnpack
	      _ -> do return HsUnpackFailed

instance Binary TupleSort where
    put_ bh BoxedTuple      = putByte bh 0
    put_ bh UnboxedTuple    = putByte bh 1
    put_ bh ConstraintTuple = putByte bh 2
    get bh = do
      h <- getByte bh
      case h of
        0 -> do return BoxedTuple
        1 -> do return UnboxedTuple
        _ -> do return ConstraintTuple

instance Binary RecFlag where
    put_ bh Recursive = do
	    putByte bh 0
    put_ bh NonRecursive = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return Recursive
	      _ -> do return NonRecursive

instance Binary DefMethSpec where
    put_ bh NoDM      = putByte bh 0
    put_ bh VanillaDM = putByte bh 1
    put_ bh GenericDM = putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return NoDM
	      1 -> return VanillaDM
	      _ -> return GenericDM

instance Binary FixityDirection where
    put_ bh InfixL = do
	    putByte bh 0
    put_ bh InfixR = do
	    putByte bh 1
    put_ bh InfixN = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return InfixL
	      1 -> do return InfixR
	      _ -> do return InfixN

instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (Fixity aa ab)

instance (Binary name) => Binary (IPName name) where
    put_ bh (IPName aa) = put_ bh aa
    get bh = do aa <- get bh
	        return (IPName aa)

-------------------------------------------------------------------------
--		Types from: Demand
-------------------------------------------------------------------------

instance Binary DmdType where
	-- Ignore DmdEnv when spitting out the DmdType
  put bh (DmdType _ ds dr) = do p <- put bh ds; put_ bh dr; return (castBin p)
  get bh = do ds <- get bh; dr <- get bh; return (DmdType emptyVarEnv ds dr)

instance Binary Demand where
    put_ bh Top = do
	    putByte bh 0
    put_ bh Abs = do
	    putByte bh 1
    put_ bh (Call aa) = do
	    putByte bh 2
	    put_ bh aa
    put_ bh (Eval ab) = do
	    putByte bh 3
	    put_ bh ab
    put_ bh (Defer ac) = do
	    putByte bh 4
	    put_ bh ac
    put_ bh (Box ad) = do
	    putByte bh 5
	    put_ bh ad
    put_ bh Bot = do
	    putByte bh 6
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return Top
	      1 -> do return Abs
	      2 -> do aa <- get bh
		      return (Call aa)
	      3 -> do ab <- get bh
		      return (Eval ab)
	      4 -> do ac <- get bh
		      return (Defer ac)
	      5 -> do ad <- get bh
		      return (Box ad)
	      _ -> do return Bot

instance Binary Demands where
    put_ bh (Poly aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (Prod ab) = do
	    putByte bh 1
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (Poly aa)
	      _ -> do ab <- get bh
		      return (Prod ab)

instance Binary DmdResult where
    put_ bh TopRes = do
	    putByte bh 0
    put_ bh RetCPR = do
	    putByte bh 1
    put_ bh BotRes = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return TopRes
	      1 -> do return RetCPR	-- Really use RetCPR even if -fcpr-off
					-- The wrapper was generated for CPR in 
					-- the imported module!
	      _ -> do return BotRes

instance Binary StrictSig where
    put_ bh (StrictSig aa) = do
	    put_ bh aa
    get bh = do
	  aa <- get bh
	  return (StrictSig aa)


-------------------------------------------------------------------------
--		Types from: CostCentre
-------------------------------------------------------------------------

instance Binary IsCafCC where
    put_ bh CafCC = do
	    putByte bh 0
    put_ bh NotCafCC = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return CafCC
	      _ -> do return NotCafCC

instance Binary IsDupdCC where
    put_ bh OriginalCC = do
	    putByte bh 0
    put_ bh DupdCC = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return OriginalCC
	      _ -> do return DupdCC

instance Binary CostCentre where
    put_ bh NoCostCentre = do
	    putByte bh 0
    put_ bh (NormalCC aa ab ac ad) = do
	    putByte bh 1
	    put_ bh aa
	    put_ bh ab
	    put_ bh ac
	    put_ bh ad
    put_ bh (AllCafsCC ae) = do
	    putByte bh 2
	    put_ bh ae
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return NoCostCentre
	      1 -> do aa <- get bh
		      ab <- get bh
		      ac <- get bh
		      ad <- get bh
		      return (NormalCC aa ab ac ad)
	      _ -> do ae <- get bh
		      return (AllCafsCC ae)

-------------------------------------------------------------------------
--		IfaceTypes and friends
-------------------------------------------------------------------------

instance Binary IfaceBndr where
    put_ bh (IfaceIdBndr aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (IfaceTvBndr ab) = do
	    putByte bh 1
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (IfaceIdBndr aa)
	      _ -> do ab <- get bh
		      return (IfaceTvBndr ab)

instance Binary IfaceLetBndr where
    put_ bh (IfLetBndr a b c) = do
	    put_ bh a
	    put_ bh b
	    put_ bh c
    get bh = do a <- get bh
	        b <- get bh
	        c <- get bh
		return (IfLetBndr a b c)	   

instance Binary IfaceType where
    put_ bh (IfaceForAllTy aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (IfaceTyVar ad) = do
	    putByte bh 1
	    put_ bh ad
    put_ bh (IfaceAppTy ae af) = do
	    putByte bh 2
	    put_ bh ae
	    put_ bh af
    put_ bh (IfaceFunTy ag ah) = do
	    putByte bh 3
	    put_ bh ag
	    put_ bh ah
    
	-- Simple compression for common cases of TyConApp
    put_ bh (IfaceTyConApp IfaceIntTc  [])   = putByte bh 6
    put_ bh (IfaceTyConApp IfaceCharTc [])   = putByte bh 7
    put_ bh (IfaceTyConApp IfaceBoolTc [])   = putByte bh 8
    put_ bh (IfaceTyConApp IfaceListTc [ty]) = do { putByte bh 9; put_ bh ty }
	-- Unit tuple and pairs
    put_ bh (IfaceTyConApp (IfaceTupTc BoxedTuple 0) []) 	 = putByte bh 10
    put_ bh (IfaceTyConApp (IfaceTupTc BoxedTuple 2) [t1,t2]) = do { putByte bh 11; put_ bh t1; put_ bh t2 }
        -- Kind cases
    put_ bh (IfaceTyConApp IfaceLiftedTypeKindTc [])   = putByte bh 12
    put_ bh (IfaceTyConApp IfaceOpenTypeKindTc [])     = putByte bh 13
    put_ bh (IfaceTyConApp IfaceUnliftedTypeKindTc []) = putByte bh 14
    put_ bh (IfaceTyConApp IfaceUbxTupleKindTc [])     = putByte bh 15
    put_ bh (IfaceTyConApp IfaceArgTypeKindTc [])      = putByte bh 16
    put_ bh (IfaceTyConApp IfaceConstraintKindTc [])   = putByte bh 21
    put_ bh (IfaceTyConApp (IfaceAnyTc k) []) 	       = do { putByte bh 17; put_ bh k }

	-- Generic cases
    put_ bh (IfaceTyConApp (IfaceTc tc) tys) = do { putByte bh 18; put_ bh tc; put_ bh tys }
    put_ bh (IfaceTyConApp tc tys) 	     = do { putByte bh 19; put_ bh tc; put_ bh tys }

    put_ bh (IfaceCoConApp cc tys) = do { putByte bh 20; put_ bh cc; put_ bh tys }

    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      return (IfaceForAllTy aa ab)
	      1 -> do ad <- get bh
		      return (IfaceTyVar ad)
	      2 -> do ae <- get bh
		      af <- get bh
		      return (IfaceAppTy ae af)
	      3 -> do ag <- get bh
		      ah <- get bh
		      return (IfaceFunTy ag ah)
	      
		-- Now the special cases for TyConApp
	      6 -> return (IfaceTyConApp IfaceIntTc [])
	      7 -> return (IfaceTyConApp IfaceCharTc [])
	      8 -> return (IfaceTyConApp IfaceBoolTc [])
	      9 -> do { ty <- get bh; return (IfaceTyConApp IfaceListTc [ty]) }
	      10 -> return (IfaceTyConApp (IfaceTupTc BoxedTuple 0) [])
	      11 -> do { t1 <- get bh; t2 <- get bh; return (IfaceTyConApp (IfaceTupTc BoxedTuple 2) [t1,t2]) }
              12 -> return (IfaceTyConApp IfaceLiftedTypeKindTc [])
              13 -> return (IfaceTyConApp IfaceOpenTypeKindTc [])
              14 -> return (IfaceTyConApp IfaceUnliftedTypeKindTc [])
              15 -> return (IfaceTyConApp IfaceUbxTupleKindTc [])
              16 -> return (IfaceTyConApp IfaceArgTypeKindTc [])
              21 -> return (IfaceTyConApp IfaceConstraintKindTc [])
              17 -> do { k <- get bh; return (IfaceTyConApp (IfaceAnyTc k) []) }

	      18 -> do { tc <- get bh; tys <- get bh; return (IfaceTyConApp (IfaceTc tc) tys) }
	      19  -> do { tc <- get bh; tys <- get bh; return (IfaceTyConApp tc tys) }
	      _  -> do { cc <- get bh; tys <- get bh; return (IfaceCoConApp cc tys) }

instance Binary IfaceTyCon where
	-- Int,Char,Bool can't show up here because they can't not be saturated
   put_ bh IfaceIntTc  	      = putByte bh 1
   put_ bh IfaceBoolTc 	      = putByte bh 2
   put_ bh IfaceCharTc 	      = putByte bh 3
   put_ bh IfaceListTc 	      = putByte bh 4
   put_ bh IfacePArrTc 	      = putByte bh 5
   put_ bh IfaceLiftedTypeKindTc   = putByte bh 6
   put_ bh IfaceOpenTypeKindTc     = putByte bh 7
   put_ bh IfaceUnliftedTypeKindTc = putByte bh 8
   put_ bh IfaceUbxTupleKindTc     = putByte bh 9
   put_ bh IfaceArgTypeKindTc      = putByte bh 10
   put_ bh IfaceConstraintKindTc   = putByte bh 15
   put_ bh (IfaceTupTc bx ar)  = do { putByte bh 11; put_ bh bx; put_ bh ar }
   put_ bh (IfaceTc ext)       = do { putByte bh 12; put_ bh ext }
   put_ bh (IfaceIPTc n)       = do { putByte bh 13; put_ bh n }
   put_ bh (IfaceAnyTc k)      = do { putByte bh 14; put_ bh k }

   get bh = do
	h <- getByte bh
	case h of
	  1 -> return IfaceIntTc
	  2 -> return IfaceBoolTc
	  3 -> return IfaceCharTc
	  4 -> return IfaceListTc
	  5 -> return IfacePArrTc
          6 -> return IfaceLiftedTypeKindTc 
          7 -> return IfaceOpenTypeKindTc 
          8 -> return IfaceUnliftedTypeKindTc
          9 -> return IfaceUbxTupleKindTc
          10 -> return IfaceArgTypeKindTc
          15 -> return IfaceConstraintKindTc
	  11 -> do { bx <- get bh; ar <- get bh; return (IfaceTupTc bx ar) }
	  12 -> do { ext <- get bh; return (IfaceTc ext) }
	  13 -> do { n <- get bh; return (IfaceIPTc n) }
          _  -> do { k <- get bh; return (IfaceAnyTc k) }

instance Binary IfaceCoCon where
   put_ bh (IfaceCoAx n)       = do { putByte bh 0; put_ bh n }
   put_ bh IfaceReflCo         = putByte bh 1
   put_ bh IfaceUnsafeCo       = putByte bh 2
   put_ bh IfaceSymCo          = putByte bh 3
   put_ bh IfaceTransCo        = putByte bh 4
   put_ bh IfaceInstCo         = putByte bh 5
   put_ bh (IfaceNthCo d)      = do { putByte bh 6; put_ bh d }
   put_ bh (IfaceIPCoAx ip)    = do { putByte bh 7; put_ bh ip }
  
   get bh = do
	h <- getByte bh
	case h of
          0 -> do { n <- get bh; return (IfaceCoAx n) }
	  1 -> return IfaceReflCo 
	  2 -> return IfaceUnsafeCo
	  3 -> return IfaceSymCo
	  4 -> return IfaceTransCo
	  5 -> return IfaceInstCo
          6 -> do { d <- get bh; return (IfaceNthCo d) }
          _ -> do { ip <- get bh; return (IfaceIPCoAx ip) }

-------------------------------------------------------------------------
--		IfaceExpr and friends
-------------------------------------------------------------------------

instance Binary IfaceExpr where
    put_ bh (IfaceLcl aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (IfaceType ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh (IfaceCo ab) = do
	    putByte bh 2
	    put_ bh ab
    put_ bh (IfaceTuple ac ad) = do
	    putByte bh 3
	    put_ bh ac
	    put_ bh ad
    put_ bh (IfaceLam ae af) = do
	    putByte bh 4
	    put_ bh ae
	    put_ bh af
    put_ bh (IfaceApp ag ah) = do
	    putByte bh 5
	    put_ bh ag
	    put_ bh ah
    put_ bh (IfaceCase ai aj ak) = do
	    putByte bh 6
	    put_ bh ai
	    put_ bh aj
	    put_ bh ak
    put_ bh (IfaceLet al am) = do
	    putByte bh 7
	    put_ bh al
	    put_ bh am
    put_ bh (IfaceNote an ao) = do
	    putByte bh 8
	    put_ bh an
	    put_ bh ao
    put_ bh (IfaceLit ap) = do
	    putByte bh 9
	    put_ bh ap
    put_ bh (IfaceFCall as at) = do
	    putByte bh 10
	    put_ bh as
	    put_ bh at
    put_ bh (IfaceExt aa) = do
	    putByte bh 11
	    put_ bh aa
    put_ bh (IfaceCast ie ico) = do
            putByte bh 12
            put_ bh ie
            put_ bh ico
    put_ bh (IfaceTick m ix) = do
            putByte bh 13
            put_ bh m
            put_ bh ix
    put_ bh (IfaceTupId aa ab) = do
      putByte bh 14
      put_ bh aa
      put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (IfaceLcl aa)
	      1 -> do ab <- get bh
		      return (IfaceType ab)
	      2 -> do ab <- get bh
		      return (IfaceCo ab)
	      3 -> do ac <- get bh
		      ad <- get bh
		      return (IfaceTuple ac ad)
	      4 -> do ae <- get bh
		      af <- get bh
		      return (IfaceLam ae af)
	      5 -> do ag <- get bh
		      ah <- get bh
		      return (IfaceApp ag ah)
	      6 -> do ai <- get bh
		      aj <- get bh
		      ak <- get bh
		      return (IfaceCase ai aj ak)
	      7 -> do al <- get bh
		      am <- get bh
		      return (IfaceLet al am)
	      8 -> do an <- get bh
		      ao <- get bh
		      return (IfaceNote an ao)
	      9 -> do ap <- get bh
		      return (IfaceLit ap)
	      10 -> do as <- get bh
		       at <- get bh
		       return (IfaceFCall as at)
	      11 -> do aa <- get bh
		       return (IfaceExt aa)
              12 -> do ie <- get bh
                       ico <- get bh
                       return (IfaceCast ie ico)
              13 -> do m <- get bh
                       ix <- get bh
                       return (IfaceTick m ix)
              14 -> do aa <- get bh
                       ab <- get bh
                       return (IfaceTupId aa ab)
              _ -> panic ("get IfaceExpr " ++ show h)

instance Binary IfaceConAlt where
    put_ bh IfaceDefault = do
	    putByte bh 0
    put_ bh (IfaceDataAlt aa) = do
	    putByte bh 1
	    put_ bh aa
    put_ bh (IfaceTupleAlt ab) = do
	    putByte bh 2
	    put_ bh ab
    put_ bh (IfaceLitAlt ac) = do
	    putByte bh 3
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return IfaceDefault
	      1 -> do aa <- get bh
		      return (IfaceDataAlt aa)
	      2 -> do ab <- get bh
		      return (IfaceTupleAlt ab)
	      _ -> do ac <- get bh
		      return (IfaceLitAlt ac)

instance Binary IfaceBinding where
    put_ bh (IfaceNonRec aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (IfaceRec ac) = do
	    putByte bh 1
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      return (IfaceNonRec aa ab)
	      _ -> do ac <- get bh
		      return (IfaceRec ac)

instance Binary IfaceIdDetails where
    put_ bh IfVanillaId      = putByte bh 0
    put_ bh (IfRecSelId a b) = do { putByte bh 1; put_ bh a; put_ bh b }
    put_ bh IfDFunId         = putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return IfVanillaId
	      1 -> do a <- get bh
		      b <- get bh
		      return (IfRecSelId a b)
              _ -> return IfDFunId

instance Binary IfaceIdInfo where
    put_ bh NoInfo = putByte bh 0
    put_ bh (HasInfo i) = do
	    putByte bh 1
	    lazyPut bh i			-- NB lazyPut

    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return NoInfo
	      _ -> do info <- lazyGet bh	-- NB lazyGet
		      return (HasInfo info)

instance Binary IfaceInfoItem where
    put_ bh (HsArity aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (HsStrictness ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh (HsUnfold lb ad) = do
	    putByte bh 2
	    put_ bh lb
	    put_ bh ad
    put_ bh (HsInline ad) = do
	    putByte bh 3
	    put_ bh ad
    put_ bh HsNoCafRefs = do
	    putByte bh 4
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (HsArity aa)
	      1 -> do ab <- get bh
		      return (HsStrictness ab)
	      2 -> do lb <- get bh
		      ad <- get bh
                      return (HsUnfold lb ad)
	      3 -> do ad <- get bh
		      return (HsInline ad)
	      _ -> do return HsNoCafRefs

instance Binary IfaceUnfolding where
    put_ bh (IfCoreUnfold s e) = do
	putByte bh 0
	put_ bh s
	put_ bh e
    put_ bh (IfInlineRule a b c d) = do
	putByte bh 1
	put_ bh a
	put_ bh b
	put_ bh c
	put_ bh d
    put_ bh (IfLclWrapper a n) = do
	putByte bh 2
	put_ bh a
	put_ bh n
    put_ bh (IfExtWrapper a n) = do
	putByte bh 3
	put_ bh a
	put_ bh n
    put_ bh (IfDFunUnfold as) = do
	putByte bh 4
	put_ bh as
    put_ bh (IfCompulsory e) = do
	putByte bh 5
	put_ bh e
    get bh = do
	h <- getByte bh
	case h of
	  0 -> do s <- get bh
		  e <- get bh
		  return (IfCoreUnfold s e)
	  1 -> do a <- get bh
		  b <- get bh
		  c <- get bh
		  d <- get bh
		  return (IfInlineRule a b c d)
	  2 -> do a <- get bh
		  n <- get bh
		  return (IfLclWrapper a n)
	  3 -> do a <- get bh
		  n <- get bh
		  return (IfExtWrapper a n)
	  4 -> do as <- get bh
		  return (IfDFunUnfold as)
	  _ -> do e <- get bh
		  return (IfCompulsory e)

instance Binary IfaceNote where
    put_ bh (IfaceSCC aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (IfaceCoreNote s) = do
            putByte bh 4
            put_ bh s
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (IfaceSCC aa)
              4 -> do ac <- get bh
                      return (IfaceCoreNote ac)
              _ -> panic ("get IfaceNote " ++ show h)

-------------------------------------------------------------------------
--		IfaceDecl and friends
-------------------------------------------------------------------------

-- A bit of magic going on here: there's no need to store the OccName
-- for a decl on the disk, since we can infer the namespace from the
-- context; however it is useful to have the OccName in the IfaceDecl
-- to avoid re-building it in various places.  So we build the OccName
-- when de-serialising.

instance Binary IfaceDecl where
    put_ bh (IfaceId name ty details idinfo) = do
	    putByte bh 0
	    put_ bh (occNameFS name)
	    put_ bh ty
	    put_ bh details
	    put_ bh idinfo
    put_ _ (IfaceForeign _ _) = 
	error "Binary.put_(IfaceDecl): IfaceForeign"
    put_ bh (IfaceData a1 a2 a3 a4 a5 a6 a7) = do
	    putByte bh 2
	    put_ bh (occNameFS a1)
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
    put_ bh (IfaceSyn a1 a2 a3 a4 a5) = do
	    putByte bh 3
	    put_ bh (occNameFS a1)
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
    put_ bh (IfaceClass a1 a2 a3 a4 a5 a6 a7) = do
	    putByte bh 4
	    put_ bh a1
	    put_ bh (occNameFS a2)
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do name    <- get bh
		      ty      <- get bh
		      details <- get bh
		      idinfo  <- get bh
                      occ <- return $! mkOccNameFS varName name
		      return (IfaceId occ ty details idinfo)
	      1 -> error "Binary.get(TyClDecl): ForeignType"
	      2 -> do
		    a1 <- get bh
		    a2 <- get bh
		    a3 <- get bh
		    a4 <- get bh
		    a5 <- get bh
		    a6 <- get bh
		    a7 <- get bh
                    occ <- return $! mkOccNameFS tcName a1
		    return (IfaceData occ a2 a3 a4 a5 a6 a7)
	      3 -> do
		    a1 <- get bh
		    a2 <- get bh
		    a3 <- get bh
		    a4 <- get bh
		    a5 <- get bh
                    occ <- return $! mkOccNameFS tcName a1
		    return (IfaceSyn occ a2 a3 a4 a5)
	      _ -> do
		    a1 <- get bh
		    a2 <- get bh
		    a3 <- get bh
		    a4 <- get bh
		    a5 <- get bh
		    a6 <- get bh
		    a7 <- get bh
                    occ <- return $! mkOccNameFS clsName a2
		    return (IfaceClass a1 occ a3 a4 a5 a6 a7)

instance Binary IfaceInst where
    put_ bh (IfaceInst cls tys dfun flag orph) = do
	    put_ bh cls
	    put_ bh tys
	    put_ bh dfun
	    put_ bh flag
	    put_ bh orph
    get bh = do cls  <- get bh
		tys  <- get bh
		dfun <- get bh
		flag <- get bh
		orph <- get bh
		return (IfaceInst cls tys dfun flag orph)

instance Binary IfaceFamInst where
    put_ bh (IfaceFamInst fam tys tycon) = do
	    put_ bh fam
	    put_ bh tys
	    put_ bh tycon
    get bh = do fam   <- get bh
		tys   <- get bh
		tycon <- get bh
		return (IfaceFamInst fam tys tycon)

instance Binary OverlapFlag where
    put_ bh (NoOverlap  b) = putByte bh 0 >> put_ bh b
    put_ bh (OverlapOk  b) = putByte bh 1 >> put_ bh b
    put_ bh (Incoherent b) = putByte bh 2 >> put_ bh b
    get bh = do h <- getByte bh
                b <- get bh
		case h of
		  0 -> return $ NoOverlap b
		  1 -> return $ OverlapOk b
		  2 -> return $ Incoherent b
		  _ -> panic ("get OverlapFlag " ++ show h)

instance Binary IfaceConDecls where
    put_ bh (IfAbstractTyCon d) = do { putByte bh 0; put_ bh d }
    put_ bh IfOpenDataTyCon = putByte bh 1
    put_ bh (IfDataTyCon cs) = do { putByte bh 2
				  ; put_ bh cs }
    put_ bh (IfNewTyCon c)  = do { putByte bh 3
				  ; put_ bh c }
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do { d <- get bh; return (IfAbstractTyCon d) }
	      1 -> return IfOpenDataTyCon
	      2 -> do cs <- get bh
		      return (IfDataTyCon cs)
	      _ -> do aa <- get bh
		      return (IfNewTyCon aa)

instance Binary IfaceConDecl where
    put_ bh (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10) = do
	    put_ bh a1
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
	    put_ bh a8
	    put_ bh a9
	    put_ bh a10
    get bh = do a1 <- get bh
		a2 <- get bh
		a3 <- get bh	      
		a4 <- get bh
		a5 <- get bh
		a6 <- get bh
		a7 <- get bh
		a8 <- get bh
		a9 <- get bh
		a10 <- get bh
	        return (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)

instance Binary IfaceAT where
    put_ bh (IfaceAT dec defs) = do
            put_ bh dec
            put_ bh defs
    get bh = do dec <- get bh
                defs <- get bh
                return (IfaceAT dec defs)

instance Binary IfaceATDefault where
    put_ bh (IfaceATD tvs pat_tys ty) = do
        put_ bh tvs
        put_ bh pat_tys
        put_ bh ty
    get bh = liftM3 IfaceATD (get bh) (get bh) (get bh)

instance Binary IfaceClassOp where
   put_ bh (IfaceClassOp n def ty) = do	
	put_ bh (occNameFS n)
	put_ bh def	
	put_ bh ty
   get bh = do
	n <- get bh
	def <- get bh
	ty <- get bh
        occ <- return $! mkOccNameFS varName n
	return (IfaceClassOp occ def ty)

instance Binary IfaceRule where
    put_ bh (IfaceRule a1 a2 a3 a4 a5 a6 a7 a8) = do
	    put_ bh a1
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
	    put_ bh a8
    get bh = do
	    a1 <- get bh
	    a2 <- get bh
	    a3 <- get bh
	    a4 <- get bh
	    a5 <- get bh
	    a6 <- get bh
	    a7 <- get bh
	    a8 <- get bh
	    return (IfaceRule a1 a2 a3 a4 a5 a6 a7 a8)

instance Binary IfaceAnnotation where
    put_ bh (IfaceAnnotation a1 a2) = do
        put_ bh a1
        put_ bh a2
    get bh = do
        a1 <- get bh
        a2 <- get bh
        return (IfaceAnnotation a1 a2)

instance Binary name => Binary (AnnTarget name) where
    put_ bh (NamedTarget a) = do
        putByte bh 0
        put_ bh a
    put_ bh (ModuleTarget a) = do
        putByte bh 1
        put_ bh a
    get bh = do
        h <- getByte bh
        case h of
          0 -> do a <- get bh
                  return (NamedTarget a)
          _ -> do a <- get bh
                  return (ModuleTarget a)

instance Binary IfaceVectInfo where
    put_ bh (IfaceVectInfo a1 a2 a3 a4 a5) = do
	    put_ bh a1
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
    get bh = do
	    a1 <- get bh
	    a2 <- get bh
	    a3 <- get bh
	    a4 <- get bh
	    a5 <- get bh
	    return (IfaceVectInfo a1 a2 a3 a4 a5)

instance Binary IfaceTrustInfo where
    put_ bh iftrust = putByte bh $ trustInfoToNum iftrust
    get bh = getByte bh >>= (return . numToTrustInfo)

