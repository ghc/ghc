/*
 * (c)2006 Galois Connections, Inc.
 */ 

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "HsFFI.h"

#include "Rts.h"
#include "Hpc.h"

/* This is the runtime support for the Haskell Program Coverage (hpc) toolkit,
 * inside GHC.
 *
 */

#define DEBUG_HPC 0

static int hpc_inited = 0;		// Have you started this component?
static FILE *tixFile;			// file being read/written
static int tix_ch;			// current char
static StgWord64 magicTixNumber;	// Magic/Hash number to mark .tix files

static int hpc_ticks_inited = 0;	// Have you started the dynamic external ticking?
static FILE *rixFile;			// The tracer file/pipe

typedef struct _Info {
  char *modName;		// name of module
  int tickCount;		// number of ticks
  int tickOffset;		// offset into a single large .tix Array
  StgWord64 *tixArr;		// tix Array from the program execution (local for this module)
  struct _Info *next;
} Info;

// This is a cruel hack, we should completely redesign the format specifier handling in the RTS.
#if SIZEOF_LONG == 8
#define PRIuWORD64 "lu"
#else
#define PRIuWORD64 "llu"
#endif

Info *modules = 0;
Info *nextModule = 0;
StgWord64 *tixBoxes = 0;	// local copy of tixBoxes array, from file.
int totalTixes = 0;		// total number of tix boxes.



static char *tixFilename;

static void failure(char *msg) {
  printf("Hpc failure: %s\n",msg);
  printf("(perhaps remove .tix file?)\n");
  exit(-1);
}


static int init_open(char *filename) 
{
  tixFile = fopen(filename,"r");
 if (tixFile == 0) {
    return 0;
  }
  tix_ch = getc(tixFile);
  return 1;
}

static void expect(char c) {
  if (tix_ch != c) {
    printf("Hpc: parse failed (%c,%c)\n",tix_ch,c);
    exit(-1);
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

static void hpc_init(void) {
  int i;
  Info *tmpModule;  

  if (hpc_inited != 0) {
    return;
  }
  hpc_inited = 1;
  

  tixFilename = (char *) malloc(strlen(prog_name) + 6);
  sprintf(tixFilename, "%s.tix", prog_name);

  if (init_open(tixFilename)) { 
    totalTixes = 0;

    ws();
    expect('T');
    expect('i');
    expect('x');
    ws();
    magicTixNumber = expectWord64();
    ws();
    expect('[');
    ws();
    while(tix_ch != ']') {
      tmpModule = (Info *)calloc(1,sizeof(Info));
      expect('(');
      ws();
      tmpModule -> modName = expectString();
      ws();
      expect(',');
      ws();
      tmpModule -> tickCount = (int)expectWord64();
      ws();
      expect(')');
      ws();
      
      tmpModule -> tickOffset = totalTixes;
      totalTixes += tmpModule -> tickCount;
      
      tmpModule -> tixArr = 0;
      
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
    ws();
    tixBoxes = (StgWord64 *)calloc(totalTixes,sizeof(StgWord64));

    expect('[');
    for(i = 0;i < totalTixes;i++) {
      if (i != 0) {
	expect(',');
	ws();
      }
    tixBoxes[i] = expectWord64();
    ws();
    }
    expect(']');

    fclose(tixFile);
  } else {
    // later, we will find a binary specific 
    magicTixNumber = (StgWord64)0;
  }
}

/* Called on a per-module basis, at startup time, declaring where the tix boxes are stored in memory.
 * This memory can be uninitized, because we will initialize it with either the contents
 * of the tix file, or all zeros.
 */

int
hs_hpc_module(char *modName,int modCount,StgWord64 *tixArr) {
  Info *tmpModule, *lastModule;
  int i;
  int offset = 0;
  
#if DEBUG_HPC
  printf("hs_hpc_module(%s,%d)\n",modName,modCount);
#endif

  hpc_init();

  tmpModule = modules;
  lastModule = 0;
  
  for(;tmpModule != 0;tmpModule = tmpModule->next) {
    if (!strcmp(tmpModule->modName,modName)) {
      if (tmpModule->tickCount != modCount) {
	failure("inconsistent number of tick boxes");
      }
      assert(tmpModule->tixArr == 0);	
      assert(tixBoxes != 0);
      tmpModule->tixArr = tixArr;
      for(i=0;i < modCount;i++) {
	tixArr[i] = tixBoxes[i + tmpModule->tickOffset];
      }
      return tmpModule->tickOffset;
    }
    lastModule = tmpModule;
  }
  // Did not find entry so add one on.
  tmpModule = (Info *)calloc(1,sizeof(Info));
  tmpModule->modName = modName;
  tmpModule->tickCount = modCount;
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

#if DEBUG_HPC
  printf("end: hs_hpc_module\n");
#endif
  return offset;
}


/*
 * Called on *every* exception thrown
 */
void
hs_hpc_throw() {
  // Assumes that we have had at least *one* tick first.
  // All exceptions before the first tick are not reported.
  // The only time this might be an issue is in bootstrapping code,
  // so this is a feature.
  if (hpc_inited != 0 && hpc_ticks_inited != 0) {
    fprintf(rixFile,"Throw\n");
  }
}

/* Called on every tick
 */

void
hs_hpc_tick(int globIx) {
  int threadId = 0;	 // for now, assume single thread
			 // TODO: work out how to get the thread Id to here.

  
#if DEBUG_HPC && DEBUG
  printf("hs_hpc_tick(%d)\n",globIx);
#endif
  if (!hpc_ticks_inited) {
    char* trace_filename;
    int comma;
    Info *tmpModule;  

    assert(hpc_inited);
    hpc_ticks_inited = 1;

    trace_filename = (char *) malloc(strlen(prog_name) + 6);
    sprintf(trace_filename, "%s.rix", prog_name);
    rixFile = fopen(trace_filename,"w+");

    comma = 0;
    
    fprintf(rixFile,"START %s\n",prog_name);
    fprintf(rixFile,"[");
    tmpModule = modules;
    for(;tmpModule != 0;tmpModule = tmpModule->next) {
      if (comma) {
	fprintf(rixFile,",");
      } else {
	comma = 1;
      }
      fprintf(rixFile,"(\"%s\",%u)",
	      tmpModule->modName,
	      tmpModule->tickCount);
#if DEBUG_HPC
      fprintf(stderr,"(tracer)%s: %u (offset=%u)\n",
	      tmpModule->modName,
	      tmpModule->tickCount,
	      tmpModule->tickOffset);
#endif
    }
    fprintf(rixFile,"]\n");
    fflush(rixFile);
  }
  assert(rixFile != 0);

  fprintf(rixFile,"%d\n",globIx);

#if DEBUG_HPC
  printf("end: hs_hpc_tick\n");
#endif
  
}

/* This is called after all the modules have registered their local tixboxes,
 * and does a sanity check: are we good to go?
 */

void
startupHpc(void) {
  Info *tmpModule;
#if DEBUG_HPC
  printf("startupHpc\n");
#endif
 
 if (hpc_inited == 0) {
    return;
  }

  tmpModule = modules;

  if (tixBoxes) {
    for(;tmpModule != 0;tmpModule = tmpModule->next) {
      if (!tmpModule->tixArr) {
	fprintf(stderr,"error: module %s did not register any hpc tick data\n",
		tmpModule->modName);
	fprintf(stderr,"(perhaps remove %s ?)\n",tixFilename);
	exit(-1);
      }
    }
  }
}


/* Called at the end of execution, to write out the Hpc *.tix file  
 * for this exection. Safe to call, even if coverage is not used.
 */
void
exitHpc(void) {
  Info *tmpModule;  
  int i, comma;

#if DEBUG_HPC
  printf("exitHpc\n");
#endif

  if (hpc_inited == 0) {
    return;
  }

  FILE *f = fopen(tixFilename,"w");
  
  comma = 0;

  fprintf(f,"Tix %" PRIuWORD64 " [", magicTixNumber);
  tmpModule = modules;
  for(;tmpModule != 0;tmpModule = tmpModule->next) {
    if (comma) {
      fprintf(f,",");
    } else {
      comma = 1;
    }
    fprintf(f,"(\"%s\",%u)",
	   tmpModule->modName,
	    tmpModule->tickCount);
#if DEBUG_HPC
    fprintf(stderr,"%s: %u (offset=%u)\n",
	   tmpModule->modName,
	   tmpModule->tickCount,
	   tmpModule->tickOffset);
#endif
  }
  fprintf(f,"] [");
  
  comma = 0;
  tmpModule = modules;
  for(;tmpModule != 0;tmpModule = tmpModule->next) {
      if (!tmpModule->tixArr) {
	fprintf(stderr,"warning: module %s did not register any hpc tick data\n",
		tmpModule->modName);
      }

    for(i = 0;i < tmpModule->tickCount;i++) {
      if (comma) {
	fprintf(f,",");
      } else {
	comma = 1;
      }

      if (tmpModule->tixArr) {
	fprintf(f,"%" PRIuWORD64,tmpModule->tixArr[i]);
      } else {
	fprintf(f,"0");
      }

    }
  }
      
  fprintf(f,"]\n");
  fclose(f);

  if (hpc_ticks_inited && rixFile != 0) {
    fclose(rixFile);
  }
  
}

