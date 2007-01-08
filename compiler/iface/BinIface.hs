-- 
--  (c) The University of Glasgow 2002-2006
-- 
-- Binary interface file support.

module BinIface ( writeBinIface, readBinIface, v_IgnoreHiWay ) where

#include "HsVersions.h"

import TcRnMonad
import IfaceEnv
import HscTypes
import BasicTypes
import NewDemand
import IfaceSyn
import Module
import Name
import OccName
import VarEnv
import InstEnv
import Class
import DynFlags
import UniqFM
import UniqSupply
import CostCentre
import StaticFlags
import PackageConfig
import Panic
import Binary
import SrcLoc
import Util
import ErrUtils
import Config
import FastMutInt
import Outputable

import Data.Word
import Data.Array
import Data.IORef
import Control.Exception
import Control.Monad

-- ---------------------------------------------------------------------------
-- Reading and writing binary interface files

readBinIface :: FilePath -> TcRnIf a b ModIface
readBinIface hi_path = do
  nc <- getNameCache
  (new_nc, iface) <- ioToIOEnv $ readBinIface_ hi_path nc
  setNameCache new_nc
  return iface

readBinIface_ :: FilePath -> NameCache -> IO (NameCache, ModIface)
readBinIface_ hi_path nc = do
  bh <- Binary.readBinMem hi_path

	-- Read the magic number to check that this really is a GHC .hi file
	-- (This magic number does not change when we change 
	--  GHC interface file format)
  magic <- get bh
  when (magic /= binaryInterfaceMagic) $
	throwDyn (ProgramError (
	   "magic number mismatch: old/corrupt interface file?"))

        -- Get the dictionary pointer.  We won't attempt to actually
        -- read the dictionary until we've done the version checks below,
        -- just in case this isn't a valid interface.  In retrospect the
        -- version should have come before the dictionary pointer, but this
        -- is the way it was done originally, and we can't change it now.
  dict_p <- Binary.get bh	-- Get the dictionary ptr

        -- Check the interface file version and ways.
  check_ver  <- get bh
  let our_ver = show opt_HiVersion
  when (check_ver /= our_ver) $
        -- This will be caught by readIface which will emit an error
        -- msg containing the iface module name.
    throwDyn (ProgramError (
        "mismatched interface file versions: expected "
        ++ our_ver ++ ", found " ++ check_ver))

  check_way <- get bh
  ignore_way <- readIORef v_IgnoreHiWay
  way_descr <- getWayDescr
  when (not ignore_way && check_way /= way_descr) $
        -- This will be caught by readIface
        -- which will emit an error msg containing the iface module name.
     throwDyn (ProgramError (
  	"mismatched interface file ways: expected "
  	++ way_descr ++ ", found " ++ check_way))

	-- Read the dictionary
	-- The next word in the file is a pointer to where the dictionary is
	-- (probably at the end of the file)
  data_p <- tellBin bh		-- Remember where we are now
  seekBin bh dict_p
  dict <- getDictionary bh
  seekBin bh data_p		-- Back to where we were before

	-- Initialise the user-data field of bh
  ud <- newReadState dict
  bh <- return (setUserData bh ud)
	
  symtab_p <- Binary.get bh	-- Get the symtab ptr
  data_p <- tellBin bh		-- Remember where we are now
  seekBin bh symtab_p
  (nc', symtab) <- getSymbolTable bh nc
  seekBin bh data_p		-- Back to where we were before
  let ud = getUserData bh
  bh <- return $! setUserData bh ud{ud_symtab = symtab}
  iface <- get bh
  return (nc', iface)


writeBinIface :: DynFlags -> FilePath -> ModIface -> IO ()
writeBinIface dflags hi_path mod_iface = do
  bh <- openBinMem initBinMemSize
  put_ bh binaryInterfaceMagic

	-- Remember where the dictionary pointer will go
  dict_p_p <- tellBin bh
  put_ bh dict_p_p	-- Placeholder for ptr to dictionary

        -- The version and way descriptor go next
  put_ bh (show opt_HiVersion)
  way_descr <- getWayDescr
  put  bh way_descr

        -- Remember where the symbol table pointer will go
  symtab_p_p <- tellBin bh
  put_ bh symtab_p_p

	-- Make some intial state
  ud <- newWriteState

	-- Put the main thing, 
  bh <- return $ setUserData bh ud
  put_ bh mod_iface

	-- Write the symtab pointer at the fornt of the file
  symtab_p <- tellBin bh	        -- This is where the symtab will start
  putAt bh symtab_p_p symtab_p	-- Fill in the placeholder
  seekBin bh symtab_p		-- Seek back to the end of the file

        -- Write the symbol table itself
  symtab_next <- readFastMutInt (ud_symtab_next ud)
  symtab_map  <- readIORef (ud_symtab_map  ud)
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
  dict_next <- readFastMutInt (ud_dict_next ud)
  dict_map  <- readIORef (ud_dict_map  ud)
  putDictionary bh dict_next dict_map
  debugTraceMsg dflags 3 (text "writeBinIface:" <+> int dict_next
                                 <+> text "dict entries")

	-- And send the result to the file
  writeBinMem bh hi_path

initBinMemSize       = (1024*1024) :: Int

-- The *host* architecture version:
#include "MachDeps.h"

#if   WORD_SIZE_IN_BITS == 32
binaryInterfaceMagic = 0x1face :: Word32
#elif WORD_SIZE_IN_BITS == 64
binaryInterfaceMagic = 0x1face64 :: Word32
#endif
  
-- -----------------------------------------------------------------------------
-- The symbol table

putSymbolTable :: BinHandle -> Int -> UniqFM (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = elems (array (0,next_off-1) (eltsUFM symtab))
  mapM_ (\n -> serialiseName bh n symtab) names

getSymbolTable :: BinHandle -> NameCache -> IO (NameCache, Array Int Name)
getSymbolTable bh namecache = do
  sz <- get bh
  od_names <- sequence (replicate sz (get bh))
  let 
        arr = listArray (0,sz-1) names
        (namecache', names) =    
                mapAccumR (fromOnDiskName arr) namecache od_names
  --
  return (namecache', arr)

type OnDiskName = (PackageId, ModuleName, OccName)

fromOnDiskName
   :: Array Int Name
   -> NameCache
   -> OnDiskName
   -> (NameCache, Name)
fromOnDiskName arr nc (pid, mod_name, occ) =
  let 
        mod   = mkModule pid mod_name
        cache = nsNames nc
  in
  case lookupOrigNameCache cache  mod occ of
     Just name -> (nc, name)
     Nothing   -> 
        let 
                us        = nsUniqs nc
                uniq      = uniqFromSupply us
                name      = mkExternalName uniq mod occ noSrcLoc
                new_cache = extendNameCache cache mod occ name
        in        
        case splitUniqSupply us of { (us',_) -> 
        ( nc{ nsUniqs = us', nsNames = new_cache }, name )
        }

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name symtab = do
  let mod = nameModule name
  put_ bh (modulePackageId mod, moduleName mod, nameOccName name)

-- -----------------------------------------------------------------------------
-- All the binary instances

-- BasicTypes
{-! for IPName derive: Binary !-}
{-! for Fixity derive: Binary !-}
{-! for FixityDirection derive: Binary !-}
{-! for Boxity derive: Binary !-}
{-! for StrictnessMark derive: Binary !-}
{-! for Activation derive: Binary !-}

-- NewDemand
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
		 mi_mod_vers  = mod_vers,
		 mi_orphan    = orphan,
		 mi_finsts    = hasFamInsts,
		 mi_deps      = deps,
		 mi_usages    = usages,
		 mi_exports   = exports,
		 mi_exp_vers  = exp_vers,
		 mi_fixities  = fixities,
		 mi_deprecs   = deprecs,
		 mi_decls     = decls,
		 mi_insts     = insts,
		 mi_fam_insts = fam_insts,
		 mi_rules     = rules,
		 mi_rule_vers = rule_vers }) = do
	put_ bh mod
	put_ bh is_boot
	put_ bh mod_vers
	put_ bh orphan
	put_ bh hasFamInsts
	lazyPut bh deps
	lazyPut bh usages
	put_ bh exports
	put_ bh exp_vers
	put_ bh fixities
	lazyPut bh deprecs
        put_ bh decls
	put_ bh insts
	put_ bh fam_insts
	lazyPut bh rules
	put_ bh rule_vers

   get bh = do
	mod_name  <- get bh
	is_boot   <- get bh
	mod_vers  <- get bh
	orphan    <- get bh
	hasFamInsts <- get bh
	deps	  <- lazyGet bh
	usages	  <- {-# SCC "bin_usages" #-} lazyGet bh
	exports	  <- {-# SCC "bin_exports" #-} get bh
	exp_vers  <- get bh
	fixities  <- {-# SCC "bin_fixities" #-} get bh
	deprecs   <- {-# SCC "bin_deprecs" #-} lazyGet bh
        decls 	  <- {-# SCC "bin_tycldecls" #-} get bh
	insts     <- {-# SCC "bin_insts" #-} get bh
	fam_insts <- {-# SCC "bin_fam_insts" #-} get bh
	rules	  <- {-# SCC "bin_rules" #-} lazyGet bh
	rule_vers <- get bh
	return (ModIface {
		 mi_module    = mod_name,
		 mi_boot      = is_boot,
		 mi_mod_vers  = mod_vers,
		 mi_orphan    = orphan,
		 mi_finsts    = hasFamInsts,
		 mi_deps      = deps,
		 mi_usages    = usages,
		 mi_exports   = exports,
		 mi_exp_vers  = exp_vers,
		 mi_fixities  = fixities,
		 mi_deprecs   = deprecs,
		 mi_decls     = decls,
		 mi_globals   = Nothing,
		 mi_insts     = insts,
		 mi_fam_insts = fam_insts,
		 mi_rules     = rules,
		 mi_rule_vers = rule_vers,
			-- And build the cached values
		 mi_dep_fn    = mkIfaceDepCache deprecs,
		 mi_fix_fn    = mkIfaceFixCache fixities,
		 mi_ver_fn    = mkIfaceVerCache decls })

GLOBAL_VAR(v_IgnoreHiWay, False, Bool)

getWayDescr :: IO String
getWayDescr = do
  tag <- readIORef v_Build_tag
  if cGhcUnregisterised == "YES" then return ('u':tag) else return tag
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

instance (Binary name) => Binary (GenAvailInfo name) where
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
    put_ bh usg	= do 
	put_ bh (usg_name     usg)
	put_ bh (usg_mod      usg)
	put_ bh (usg_exports  usg)
	put_ bh (usg_entities usg)
	put_ bh (usg_rules    usg)

    get bh = do
	nm    <- get bh
	mod   <- get bh
	exps  <- get bh
	ents  <- get bh
	rules <- get bh
	return (Usage {	usg_name = nm, usg_mod = mod,
			usg_exports = exps, usg_entities = ents,
			usg_rules = rules })

instance Binary a => Binary (Deprecs a) where
    put_ bh NoDeprecs     = putByte bh 0
    put_ bh (DeprecAll t) = do
	    putByte bh 1
	    put_ bh t
    put_ bh (DeprecSome ts) = do
	    putByte bh 2
	    put_ bh ts

    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return NoDeprecs
	      1 -> do aa <- get bh
		      return (DeprecAll aa)
	      _ -> do aa <- get bh
		      return (DeprecSome aa)

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

instance Binary StrictnessMark where
    put_ bh MarkedStrict = do
	    putByte bh 0
    put_ bh MarkedUnboxed = do
	    putByte bh 1
    put_ bh NotMarkedStrict = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return MarkedStrict
	      1 -> do return MarkedUnboxed
	      _ -> do return NotMarkedStrict

instance Binary Boxity where
    put_ bh Boxed = do
	    putByte bh 0
    put_ bh Unboxed = do
	    putByte bh 1
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return Boxed
	      _ -> do return Unboxed

instance Binary TupCon where
    put_ bh (TupCon ab ac) = do
	    put_ bh ab
	    put_ bh ac
    get bh = do
	  ab <- get bh
	  ac <- get bh
	  return (TupCon ab ac)

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

instance Binary DefMeth where
    put_ bh NoDefMeth  = putByte bh 0
    put_ bh DefMeth    = putByte bh 1
    put_ bh GenDefMeth = putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return NoDefMeth
	      1 -> return DefMeth
	      _ -> return GenDefMeth

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
  put bh (DmdType _ ds dr) = do p <- put bh ds; put bh dr; return (castBin p)
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
    put_ bh (IfacePredTy aq) = do
	    putByte bh 5
	    put_ bh aq

	-- Simple compression for common cases of TyConApp
    put_ bh (IfaceTyConApp IfaceIntTc  [])   = putByte bh 6
    put_ bh (IfaceTyConApp IfaceCharTc [])   = putByte bh 7
    put_ bh (IfaceTyConApp IfaceBoolTc [])   = putByte bh 8
    put_ bh (IfaceTyConApp IfaceListTc [ty]) = do { putByte bh 9; put_ bh ty }
	-- Unit tuple and pairs
    put_ bh (IfaceTyConApp (IfaceTupTc Boxed 0) []) 	 = putByte bh 10
    put_ bh (IfaceTyConApp (IfaceTupTc Boxed 2) [t1,t2]) = do { putByte bh 11; put_ bh t1; put_ bh t2 }
        -- Kind cases
    put_ bh (IfaceTyConApp IfaceLiftedTypeKindTc [])   = putByte bh 12
    put_ bh (IfaceTyConApp IfaceOpenTypeKindTc [])     = putByte bh 13
    put_ bh (IfaceTyConApp IfaceUnliftedTypeKindTc []) = putByte bh 14
    put_ bh (IfaceTyConApp IfaceUbxTupleKindTc [])     = putByte bh 15
    put_ bh (IfaceTyConApp IfaceArgTypeKindTc [])      = putByte bh 16

	-- Generic cases

    put_ bh (IfaceTyConApp (IfaceTc tc) tys) = do { putByte bh 18; put_ bh tc; put_ bh tys }
    put_ bh (IfaceTyConApp tc tys) 	     = do { putByte bh 19; put_ bh tc; put_ bh tys }

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
	      5 -> do ap <- get bh
		      return (IfacePredTy ap)

		-- Now the special cases for TyConApp
	      6 -> return (IfaceTyConApp IfaceIntTc [])
	      7 -> return (IfaceTyConApp IfaceCharTc [])
	      8 -> return (IfaceTyConApp IfaceBoolTc [])
	      9 -> do { ty <- get bh; return (IfaceTyConApp IfaceListTc [ty]) }
	      10 -> return (IfaceTyConApp (IfaceTupTc Boxed 0) [])
	      11 -> do { t1 <- get bh; t2 <- get bh; return (IfaceTyConApp (IfaceTupTc Boxed 2) [t1,t2]) }
              12 -> return (IfaceTyConApp IfaceLiftedTypeKindTc [])
              13 -> return (IfaceTyConApp IfaceOpenTypeKindTc [])
              14 -> return (IfaceTyConApp IfaceUnliftedTypeKindTc [])
              15 -> return (IfaceTyConApp IfaceUbxTupleKindTc [])
              16 -> return (IfaceTyConApp IfaceArgTypeKindTc [])

	      18 -> do { tc <- get bh; tys <- get bh; return (IfaceTyConApp (IfaceTc tc) tys) }
	      _  -> do { tc <- get bh; tys <- get bh; return (IfaceTyConApp tc tys) }

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
   put_ bh (IfaceTupTc bx ar) = do { putByte bh 11; put_ bh bx; put_ bh ar }
   put_ bh (IfaceTc ext)      = do { putByte bh 12; put_ bh ext }

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
	  11 -> do { bx <- get bh; ar <- get bh; return (IfaceTupTc bx ar) }
	  _ -> do { ext <- get bh; return (IfaceTc ext) }

instance Binary IfacePredType where
    put_ bh (IfaceClassP aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (IfaceIParam ac ad) = do
	    putByte bh 1
	    put_ bh ac
	    put_ bh ad
    put_ bh (IfaceEqPred ac ad) = do
	    putByte bh 2
	    put_ bh ac
	    put_ bh ad
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      ab <- get bh
		      return (IfaceClassP aa ab)
	      1 -> do ac <- get bh
		      ad <- get bh
		      return (IfaceIParam ac ad)
	      2 -> do ac <- get bh
		      ad <- get bh
		      return (IfaceEqPred ac ad)

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
    put_ bh (IfaceTuple ac ad) = do
	    putByte bh 2
	    put_ bh ac
	    put_ bh ad
    put_ bh (IfaceLam ae af) = do
	    putByte bh 3
	    put_ bh ae
	    put_ bh af
    put_ bh (IfaceApp ag ah) = do
	    putByte bh 4
	    put_ bh ag
	    put_ bh ah
-- gaw 2004
    put_ bh (IfaceCase ai aj al ak) = do
	    putByte bh 5
	    put_ bh ai
	    put_ bh aj
-- gaw 2004
            put_ bh al
	    put_ bh ak
    put_ bh (IfaceLet al am) = do
	    putByte bh 6
	    put_ bh al
	    put_ bh am
    put_ bh (IfaceNote an ao) = do
	    putByte bh 7
	    put_ bh an
	    put_ bh ao
    put_ bh (IfaceLit ap) = do
	    putByte bh 8
	    put_ bh ap
    put_ bh (IfaceFCall as at) = do
	    putByte bh 9
	    put_ bh as
	    put_ bh at
    put_ bh (IfaceExt aa) = do
	    putByte bh 10
	    put_ bh aa
    put_ bh (IfaceCast ie ico) = do
            putByte bh 11
            put_ bh ie
            put_ bh ico
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (IfaceLcl aa)
	      1 -> do ab <- get bh
		      return (IfaceType ab)
	      2 -> do ac <- get bh
		      ad <- get bh
		      return (IfaceTuple ac ad)
	      3 -> do ae <- get bh
		      af <- get bh
		      return (IfaceLam ae af)
	      4 -> do ag <- get bh
		      ah <- get bh
		      return (IfaceApp ag ah)
	      5 -> do ai <- get bh
		      aj <- get bh
-- gaw 2004
                      al <- get bh                   
		      ak <- get bh
-- gaw 2004
		      return (IfaceCase ai aj al ak)
	      6 -> do al <- get bh
		      am <- get bh
		      return (IfaceLet al am)
	      7 -> do an <- get bh
		      ao <- get bh
		      return (IfaceNote an ao)
	      8 -> do ap <- get bh
		      return (IfaceLit ap)
	      9 -> do as <- get bh
		      at <- get bh
		      return (IfaceFCall as at)
	      10 -> do aa <- get bh
		       return (IfaceExt aa)
              11 -> do ie <- get bh
                       ico <- get bh
                       return (IfaceCast ie ico)

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
    put_ bh (HsUnfold ad) = do
	    putByte bh 2
	    put_ bh ad
    put_ bh (HsInline ad) = do
	    putByte bh 3
	    put_ bh ad
    put_ bh HsNoCafRefs = do
	    putByte bh 4
    put_ bh (HsWorker ae af) = do
	    putByte bh 5
	    put_ bh ae
	    put_ bh af
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (HsArity aa)
	      1 -> do ab <- get bh
		      return (HsStrictness ab)
	      2 -> do ad <- get bh
		      return (HsUnfold ad)
	      3 -> do ad <- get bh
		      return (HsInline ad)
	      4 -> do return HsNoCafRefs
	      _ -> do ae <- get bh
		      af <- get bh
		      return (HsWorker ae af)

instance Binary IfaceNote where
    put_ bh (IfaceSCC aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh IfaceInlineMe = do
	    putByte bh 3
    put_ bh (IfaceCoreNote s) = do
            putByte bh 4
            put_ bh s
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do aa <- get bh
		      return (IfaceSCC aa)
	      3 -> do return IfaceInlineMe
              4 -> do ac <- get bh
                      return (IfaceCoreNote ac)

-------------------------------------------------------------------------
--		IfaceDecl and friends
-------------------------------------------------------------------------

-- A bit of magic going on here: there's no need to store the OccName
-- for a decl on the disk, since we can infer the namespace from the
-- context; however it is useful to have the OccName in the IfaceDecl
-- to avoid re-building it in various places.  So we build the OccName
-- when de-serialising.

instance Binary IfaceDecl where
    put_ bh (IfaceId name ty idinfo) = do
	    putByte bh 0
	    put_ bh (occNameFS name)
	    put_ bh ty
	    put_ bh idinfo
    put_ bh (IfaceForeign ae af) = 
	error "Binary.put_(IfaceDecl): IfaceForeign"
    put_ bh (IfaceData a1 a2 a3 a4 a5 a6 a7 a8) = do
	    putByte bh 2
	    put_ bh (occNameFS a1)
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
	    put_ bh a8
    put_ bh (IfaceSyn aq ar as at) = do
	    putByte bh 3
	    put_ bh (occNameFS aq)
	    put_ bh ar
	    put_ bh as
	    put_ bh at
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
	      0 -> do name   <- get bh
		      ty     <- get bh
		      idinfo <- get bh
                      occ <- return $! mkOccNameFS varName name
		      return (IfaceId occ ty idinfo)
	      1 -> error "Binary.get(TyClDecl): ForeignType"
	      2 -> do
		    a1 <- get bh
		    a2 <- get bh
		    a3 <- get bh
		    a4 <- get bh
		    a5 <- get bh
		    a6 <- get bh
		    a7 <- get bh
		    a8 <- get bh
                    occ <- return $! mkOccNameFS tcName a1
		    return (IfaceData occ a2 a3 a4 a5 a6 a7 a8)
	      3 -> do
		    aq <- get bh
		    ar <- get bh
		    as <- get bh
		    at <- get bh
                    occ <- return $! mkOccNameFS tcName aq
		    return (IfaceSyn occ ar as at)
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
    put_ bh NoOverlap  = putByte bh 0
    put_ bh OverlapOk  = putByte bh 1
    put_ bh Incoherent = putByte bh 2
    get bh = do h <- getByte bh
		case h of
		  0 -> return NoOverlap
		  1 -> return OverlapOk
		  2 -> return Incoherent

instance Binary IfaceConDecls where
    put_ bh IfAbstractTyCon = putByte bh 0
    put_ bh IfOpenDataTyCon = putByte bh 1
    put_ bh IfOpenNewTyCon = putByte bh 2
    put_ bh (IfDataTyCon cs) = do { putByte bh 3
				  ; put_ bh cs }
    put_ bh (IfNewTyCon c)  = do { putByte bh 4
				  ; put_ bh c }
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> return IfAbstractTyCon
	      1 -> return IfOpenDataTyCon
	      2 -> return IfOpenNewTyCon
	      3 -> do cs <- get bh
		      return (IfDataTyCon cs)
	      _ -> do aa <- get bh
		      return (IfNewTyCon aa)

instance Binary IfaceConDecl where
    put_ bh (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9) = do
	    put_ bh a1
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
	    put_ bh a8
	    put_ bh a9
    get bh = do a1 <- get bh
		a2 <- get bh
		a3 <- get bh	      
		a4 <- get bh
		a5 <- get bh
		a6 <- get bh
		a7 <- get bh
		a8 <- get bh
		a9 <- get bh
	        return (IfCon a1 a2 a3 a4 a5 a6 a7 a8 a9)

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
    put_ bh (IfaceRule a1 a2 a3 a4 a5 a6 a7) = do
	    put_ bh a1
	    put_ bh a2
	    put_ bh a3
	    put_ bh a4
	    put_ bh a5
	    put_ bh a6
	    put_ bh a7
    get bh = do
	    a1 <- get bh
	    a2 <- get bh
	    a3 <- get bh
	    a4 <- get bh
	    a5 <- get bh
	    a6 <- get bh
	    a7 <- get bh
	    return (IfaceRule a1 a2 a3 a4 a5 a6 a7)


