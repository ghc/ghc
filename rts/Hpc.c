/*
 * (c)2006 Galois Connections, Inc.
 */ 

#include "PosixSource.h"
#include "Rts.h"

#include "Trace.h"
#include "Hash.h"
#include "RtsUtils.h"

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* This is the runtime support for the Haskell Program Coverage (hpc) toolkit,
 * inside GHC.
 *
 */

static int hpc_inited = 0;		// Have you started this component?
static pid_t hpc_pid = 0;		// pid of this process at hpc-boot time.
					// Only this pid will read or write .tix file(s).
static FILE *tixFile;			// file being read/written
static int tix_ch;			// current char

static HashTable * moduleHash = NULL;   // module name -> HpcModuleInfo

HpcModuleInfo *modules = 0;

static char *tixFilename = NULL;

static void GNU_ATTRIBUTE(__noreturn__)
failure(char *msg) {
  debugTrace(DEBUG_hpc,"hpc failure: %s\n",msg);
  fprintf(stderr,"Hpc failure: %s\n",msg);
  if (tixFilename) {
    fprintf(stderr,"(perhaps remove %s file?)\n",tixFilename);
  } else {
    fprintf(stderr,"(perhaps remove .tix file?)\n");
  }
  stg_exit(1);
}

static int init_open(FILE *file) {
  tixFile = file;
 if (tixFile == 0) {
    return 0;
  }
  tix_ch = getc(tixFile);
  return 1;
}

static void expect(char c) {
  if (tix_ch != c) {
    fprintf(stderr,"('%c' '%c')\n",tix_ch,c);
    failure("parse error when reading .tix file");
  }
  tix_ch = getc(tixFile);
}

static void ws(void) {
  while (tix_ch == ' ') {
    tix_ch = getc(tixFile);
  }
}

static char *expectString(void) {
  char tmp[256], *res; // XXX
  int tmp_ix = 0;
  expect('"');
  while (tix_ch != '"') {
    tmp[tmp_ix++] = tix_ch;
    tix_ch = getc(tixFile);
  }
  tmp[tmp_ix++] = 0;
  expect('"');
  res = stgMallocBytes(tmp_ix,"Hpc.expectString");
  strcpy(res,tmp);
  return res;
}

static StgWord64 expectWord64(void) {
  StgWord64 tmp = 0;
  while (isdigit(tix_ch)) {
    tmp = tmp * 10 + (tix_ch -'0');
    tix_ch = getc(tixFile);
  }
  return tmp;
}

static void
readTix(void) {
  unsigned int i;
  HpcModuleInfo *tmpModule, *lookup;

  ws();
  expect('T');
  expect('i');
  expect('x');
  ws();
  expect('[');
  ws();
  
  while(tix_ch != ']') {
    tmpModule = (HpcModuleInfo *)stgMallocBytes(sizeof(HpcModuleInfo),
                                                "Hpc.readTix");
    tmpModule->from_file = rtsTrue;
    expect('T');
    expect('i');
    expect('x');
    expect('M');
    expect('o');
    expect('d');
    expect('u');
    expect('l');
    expect('e');
    ws();
    tmpModule -> modName = expectString();
    ws();
    tmpModule -> hashNo = (unsigned int)expectWord64();
    ws();
    tmpModule -> tickCount = (int)expectWord64();
    tmpModule -> tixArr = (StgWord64 *)calloc(tmpModule->tickCount,sizeof(StgWord64));
    ws();
    expect('[');
    ws();
    for(i = 0;i < tmpModule->tickCount;i++) {
      tmpModule->tixArr[i] = expectWord64();
      ws();
      if (tix_ch == ',') {
	expect(',');
	ws();
      }
    }
    expect(']');
    ws();
    
    lookup = lookupHashTable(moduleHash, (StgWord)tmpModule->modName);
    if (lookup == NULL) {
        debugTrace(DEBUG_hpc,"readTix: new HpcModuleInfo for %s",
                   tmpModule->modName);
        insertHashTable(moduleHash, (StgWord)tmpModule->modName, tmpModule);
    } else {
        ASSERT(lookup->tixArr != 0);
        ASSERT(!strcmp(tmpModule->modName, lookup->modName));
        debugTrace(DEBUG_hpc,"readTix: existing HpcModuleInfo for %s",
                   tmpModule->modName);
        if (tmpModule->hashNo != lookup->hashNo) {
            fprintf(stderr,"in module '%s'\n",tmpModule->modName);
            failure("module mismatch with .tix/.mix file hash number");
            if (tixFilename != NULL) {
                fprintf(stderr,"(perhaps remove %s ?)\n",tixFilename);
            }
            stg_exit(EXIT_FAILURE);
        }
        for (i=0; i < tmpModule->tickCount; i++) {
            lookup->tixArr[i] = tmpModule->tixArr[i];
        }
        stgFree(tmpModule->tixArr);
        stgFree(tmpModule->modName);
        stgFree(tmpModule);
    }

    if (tix_ch == ',') {
      expect(',');
      ws();
    }
  }
  expect(']');
  fclose(tixFile);
}

void
startupHpc(void)
{
  char *hpc_tixdir;
  char *hpc_tixfile;

  if (moduleHash == NULL) {
      // no modules were registered with hs_hpc_module, so don't bother
      // creating the .tix file.
      return;
  }

  if (hpc_inited != 0) {
    return;
  }
  hpc_inited = 1;
  hpc_pid    = getpid();
  hpc_tixdir = getenv("HPCTIXDIR");
  hpc_tixfile = getenv("HPCTIXFILE");

  debugTrace(DEBUG_hpc,"startupHpc");

  /* XXX Check results of mallocs/strdups, and check we are requesting
         enough bytes */
  if (hpc_tixfile != NULL) {
    tixFilename = strdup(hpc_tixfile);
  } else if (hpc_tixdir != NULL) {
    /* Make sure the directory is present;
     * conditional code for mkdir lifted from lndir.c
     */
#ifdef WIN32
    mkdir(hpc_tixdir);
#else
    mkdir(hpc_tixdir,0777);
#endif
    /* Then, try open the file
     */
    tixFilename = (char *) stgMallocBytes(strlen(hpc_tixdir) +
                                          strlen(prog_name) + 12,
                                          "Hpc.startupHpc");
    sprintf(tixFilename,"%s/%s-%d.tix",hpc_tixdir,prog_name,(int)hpc_pid);
  } else {
    tixFilename = (char *) stgMallocBytes(strlen(prog_name) + 6,
                                          "Hpc.startupHpc");
    sprintf(tixFilename, "%s.tix", prog_name);
  }

  if (init_open(fopen(tixFilename,"r"))) {
    readTix();
  }
}

/*
 * Called on a per-module basis, by a constructor function compiled
 * with each module (see Coverage.hpcInitCode), declaring where the
 * tix boxes are stored in memory.  This memory can be uninitized,
 * because we will initialize it with either the contents of the tix
 * file, or all zeros.
 *
 * Note that we might call this before reading the .tix file, or after
 * in the case where we loaded some Haskell code from a .so with
 * dlopen().  So we must handle the case where we already have an
 * HpcModuleInfo for the module which was read from the .tix file.
 */

void
hs_hpc_module(char *modName,
	      StgWord32 modCount,
	      StgWord32 modHashNo,
              StgWord64 *tixArr)
{
  HpcModuleInfo *tmpModule;
  nat i;

  if (moduleHash == NULL) {
      moduleHash = allocStrHashTable();
  }

  tmpModule = lookupHashTable(moduleHash, (StgWord)modName);
  if (tmpModule == NULL)
  {
      // Did not find entry so add one on.
      tmpModule = (HpcModuleInfo *)stgMallocBytes(sizeof(HpcModuleInfo),
                                                  "Hpc.hs_hpc_module");
      tmpModule->modName = modName;
      tmpModule->tickCount = modCount;
      tmpModule->hashNo = modHashNo;

      tmpModule->tixArr = tixArr;
      for(i=0;i < modCount;i++) {
          tixArr[i] = 0;
      }
      tmpModule->next = modules;
      tmpModule->from_file = rtsFalse;
      modules = tmpModule;
      insertHashTable(moduleHash, (StgWord)modName, tmpModule);
  }
  else
  {
      if (tmpModule->tickCount != modCount) {
          failure("inconsistent number of tick boxes");
      }
      ASSERT(tmpModule->tixArr != 0);
      if (tmpModule->hashNo != modHashNo) {
          fprintf(stderr,"in module '%s'\n",tmpModule->modName);
          failure("module mismatch with .tix/.mix file hash number");
          if (tixFilename != NULL) {
              fprintf(stderr,"(perhaps remove %s ?)\n",tixFilename);
          }
          stg_exit(EXIT_FAILURE);
      }
      // The existing tixArr was made up when we read the .tix file,
      // whereas this is the real tixArr, so copy the data from the
      // .tix into the real tixArr.
      for(i=0;i < modCount;i++) {
          tixArr[i] = tmpModule->tixArr[i];
      }

      if (tmpModule->from_file) {
          stgFree(tmpModule->modName);
          stgFree(tmpModule->tixArr);
      }
      tmpModule->from_file = rtsFalse;
  }
}

static void
writeTix(FILE *f) {
  HpcModuleInfo *tmpModule;  
  unsigned int i, inner_comma, outer_comma;

  outer_comma = 0;

  if (f == 0) {
    return;
  }

  fprintf(f,"Tix [");
  tmpModule = modules;
  for(;tmpModule != 0;tmpModule = tmpModule->next) {
    if (outer_comma) {
      fprintf(f,",");
    } else {
      outer_comma = 1;
    }
    fprintf(f," TixModule \"%s\" %u %u [",
	   tmpModule->modName,
	    (nat)tmpModule->hashNo,
	    (nat)tmpModule->tickCount);
    debugTrace(DEBUG_hpc,"%s: %u (hash=%u)\n",
	       tmpModule->modName,
	       (nat)tmpModule->tickCount,
               (nat)tmpModule->hashNo);

    inner_comma = 0;
    for(i = 0;i < tmpModule->tickCount;i++) {
      if (inner_comma) {
	fprintf(f,",");
      } else {
	inner_comma = 1;
      }

      if (tmpModule->tixArr) {
	fprintf(f,"%" FMT_Word64,tmpModule->tixArr[i]);
      } else {
	fprintf(f,"0");
      }
    }
    fprintf(f,"]");
  }
  fprintf(f,"]\n");
  
  fclose(f);
}

static void
freeHpcModuleInfo (HpcModuleInfo *mod)
{
    if (mod->from_file) {
        stgFree(mod->modName);
        stgFree(mod->tixArr);
    }
    stgFree(mod);
}

/* Called at the end of execution, to write out the Hpc *.tix file
 * for this exection. Safe to call, even if coverage is not used.
 */
void
exitHpc(void) {
  debugTrace(DEBUG_hpc,"exitHpc");

  if (hpc_inited == 0) {
    return;
  }

  // Only write the tix file if you are the original process.
  // Any sub-process from use of fork from inside Haskell will
  // not clober the .tix file.

  if (hpc_pid == getpid()) {
    FILE *f = fopen(tixFilename,"w");
    writeTix(f);
  }

  freeHashTable(moduleHash, (void (*)(void *))freeHpcModuleInfo);
  moduleHash = NULL;

  stgFree(tixFilename);
  tixFilename = NULL;
}

//////////////////////////////////////////////////////////////////////////////
// This is the API into Hpc RTS from Haskell, allowing the tixs boxes
// to be first class.

HpcModuleInfo *hs_hpc_rootModule(void) {
  return modules;
}
