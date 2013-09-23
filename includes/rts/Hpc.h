/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Haskell Program Coverage
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_HPC_H
#define RTS_HPC_H

// Simple linked list of modules
typedef struct _HpcModuleInfo {
  char *modName;		// name of module
  StgWord32 tickCount;		// number of ticks
  StgWord32 hashNo;             // Hash number for this module's mix info
  StgWord64 *tixArr;		// tix Array; local for this module
  rtsBool from_file;            // data was read from the .tix file
  struct _HpcModuleInfo *next;
} HpcModuleInfo;

void hs_hpc_module (char *modName,
                    StgWord32 modCount,
                    StgWord32 modHashNo,
                    StgWord64 *tixArr);

HpcModuleInfo * hs_hpc_rootModule (void);

void startupHpc(void);
void exitHpc(void);

#endif /* RTS_HPC_H */
