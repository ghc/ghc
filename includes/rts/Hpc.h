/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Haskell Program Coverage
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_HPC_H
#define RTS_HPC_H

// Simple linked list of modules
typedef struct _HpcModuleInfo {
  char *modName;		// name of module
  StgWord32 tickCount;		// number of ticks
  StgWord32 tickOffset;		// offset into a single large .tix Array
  StgWord32 hashNo;		// Hash number for this module's mix info
  StgWord64 *tixArr;		// tix Array; local for this module
  struct _HpcModuleInfo *next;
} HpcModuleInfo;

int hs_hpc_module (char *modName, 
                   StgWord32 modCount, 
                   StgWord32 modHashNo,
                   StgWord64 *tixArr);

HpcModuleInfo * hs_hpc_rootModule (void);

void startupHpc(void);
void exitHpc(void);

#endif /* RTS_HPC_H */
