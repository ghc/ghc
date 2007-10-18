/*
 * (c)2006 Galois Connections, Inc.
 */ 

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "Rts.h"
#include "Hpc.h"
#include "Trace.h"

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

// This is a cruel hack, we should completely redesign the format specifier handling in the RTS.
#if SIZEOF_LONG == 8
#define PRIuWORD64 "lu"
#else
#define PRIuWORD64 "llu"
#endif

HpcModuleInfo *modules = 0;
HpcModuleInfo *nextModule = 0;
int totalTixes = 0;		// total number of tix boxes.

static char *tixFilename;

static void failure(char *msg) {
  debugTrace(DEBUG_hpc,"hpc failure: %s\n",msg);
  fprintf(stderr,"Hpc failure: %s\n",msg);
  if (tixFilename) {
    fprintf(stderr,"(perhaps remove %s file?)\n",tixFilename);
  } else {
    fprintf(stderr,"(perhaps remove .tix file?)\n");
  }
  exit(-1);
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
  char tmp[256], *res;
  int tmp_ix = 0;
  expect('"');
  while (tix_ch != '"') {
    tmp[tmp_ix++] = tix_ch;
    tix_ch = getc(tixFile);
  }
  tmp[tmp_ix++] = 0;
  expect('"');
  res = malloc(tmp_ix);
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
  HpcModuleInfo *tmpModule;

  totalTixes = 0;
    
  ws();
  expect('T');
  expect('i');
  expect('x');
  ws();
  expect('[');
  ws();
  
  while(tix_ch != ']') {
    tmpModule = (HpcModuleInfo *)calloc(1,sizeof(HpcModuleInfo));
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
    tmpModule -> tickOffset = totalTixes;
    totalTixes += tmpModule -> tickCount;
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
    
    if (!modules) {
      modules = tmpModule;
    } else {
      nextModule->next=tmpModule;
    }
    nextModule=tmpModule;
    
    if (tix_ch == ',') {
      expect(',');
      ws();
    }
  }
  expect(']');
  fclose(tixFile);
}

static void hpc_init(void) {
  if (hpc_inited != 0) {
    return;
  }
  hpc_inited = 1;
  hpc_pid    = getpid();

  tixFilename = (char *) malloc(strlen(prog_name) + 6);
  sprintf(tixFilename, "%s.tix", prog_name);

  if (init_open(fopen(tixFilename,"r"))) {
    readTix();
  }
}

/* Called on a per-module basis, at startup time, declaring where the tix boxes are stored in memory.
 * This memory can be uninitized, because we will initialize it with either the contents
 * of the tix file, or all zeros.
 */

int
hs_hpc_module(char *modName,
	      unsigned int modCount,
	      unsigned int modHashNo,
	      StgWord64 *tixArr) {
  HpcModuleInfo *tmpModule, *lastModule;
  unsigned int i;
  int offset = 0;
  
  debugTrace(DEBUG_hpc,"hs_hpc_module(%s,%d)",modName,modCount);

  hpc_init();

  tmpModule = modules;
  lastModule = 0;
  
  for(;tmpModule != 0;tmpModule = tmpModule->next) {
    if (!strcmp(tmpModule->modName,modName)) {
      if (tmpModule->tickCount != modCount) {
	failure("inconsistent number of tick boxes");
      }
      assert(tmpModule->tixArr != 0);	
      if (tmpModule->hashNo != modHashNo) {
	fprintf(stderr,"in module '%s'\n",tmpModule->modName);
	failure("module mismatch with .tix/.mix file hash number");
	fprintf(stderr,"(perhaps remove %s ?)\n",tixFilename);
	exit(-1);

      }
      for(i=0;i < modCount;i++) {
	tixArr[i] = tmpModule->tixArr[i];
      }
      tmpModule->tixArr = tixArr;
      return tmpModule->tickOffset;
    }
    lastModule = tmpModule;
  }
  // Did not find entry so add one on.
  tmpModule = (HpcModuleInfo *)calloc(1,sizeof(HpcModuleInfo));
  tmpModule->modName = modName;
  tmpModule->tickCount = modCount;
  tmpModule->hashNo = modHashNo;
  if (lastModule) {
    tmpModule->tickOffset = lastModule->tickOffset + lastModule->tickCount;
  } else {
    tmpModule->tickOffset = 0;
  }
  tmpModule->tixArr = tixArr;
  for(i=0;i < modCount;i++) {
    tixArr[i] = 0;
  }
  tmpModule->next = 0;

  if (!modules) {
    modules = tmpModule;
  } else {
    lastModule->next=tmpModule;
  }

  debugTrace(DEBUG_hpc,"end: hs_hpc_module");

  return offset;
}


/* This is called after all the modules have registered their local tixboxes,
 * and does a sanity check: are we good to go?
 */

void
startupHpc(void) {
  debugTrace(DEBUG_hpc,"startupHpc");
 
 if (hpc_inited == 0) {
    return;
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
	    tmpModule->hashNo,
	    tmpModule->tickCount);
    debugTrace(DEBUG_hpc,"%s: %u (offset=%u) (hash=%u)\n",
	       tmpModule->modName,
	       tmpModule->tickCount,
	       tmpModule->hashNo,
	       tmpModule->tickOffset);

    inner_comma = 0;
    for(i = 0;i < tmpModule->tickCount;i++) {
      if (inner_comma) {
	fprintf(f,",");
      } else {
	inner_comma = 1;
      }

      if (tmpModule->tixArr) {
	fprintf(f,"%" PRIuWORD64,tmpModule->tixArr[i]);
      } else {
	fprintf(f,"0");
      }
    }
    fprintf(f,"]");
  }
  fprintf(f,"]\n");
  
  fclose(f);
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
}

//////////////////////////////////////////////////////////////////////////////
// This is the API into Hpc RTS from Haskell, allowing the tixs boxes
// to be first class.

HpcModuleInfo *hs_hpc_rootModule(void) {
  return modules;
}
