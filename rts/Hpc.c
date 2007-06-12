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

#define WOP_SIZE 1024	

static int hpc_inited = 0;		// Have you started this component?
static FILE *tixFile;			// file being read/written
static int tix_ch;			// current char

static FILE *rixFile = NULL;		// The tracer file/pipe (to debugger)
static FILE *rixCmdFile = NULL;		// The tracer file/pipe (from debugger)
static StgWord64 rixCounter = 0;	// The global event counter
static int debuggee_pid;

typedef enum {
  RixThreadFinishedOp   = -1,
  RixRaiseOp            = -2,
  RixFinishedOp         = -3
} HpcRixOp;


typedef struct _Info {
  char *modName;		// name of module
  int tickCount;		// number of ticks
  int tickOffset;		// offset into a single large .tix Array
  int hashNo;			// Hash number for this module's mix info
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
  int i;
  Info *tmpModule;

  totalTixes = 0;
    
  ws();
  expect('T');
  expect('i');
  expect('x');
  ws();
  expect('[');
  ws();
  
  while(tix_ch != ']') {
    tmpModule = (Info *)calloc(1,sizeof(Info));
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
	      int modCount,
	      int modHashNo,
	      StgWord64 *tixArr) {
  Info *tmpModule, *lastModule;
  int i;
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
  tmpModule = (Info *)calloc(1,sizeof(Info));
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

static void breakPointCommand(HpcRixOp rixOp, StgThreadID rixTid);

// Breakpointing
static StgThreadID previousTid = 0;
static StgWord64 rixBPCounter = 0;	// The global event breakpoint counter
static int *tixBoxBP;
static HpcRixOp rixOpBack[WOP_SIZE];	// The actual op
static HpcRixOp rixTidBack[WOP_SIZE];	// Tid's before the op

void 
hs_hpc_raise_event(StgTSO *current_tso) {
  hs_hpc_tick(RixRaiseOp,current_tso);
}

void 
hs_hpc_thread_finished_event(StgTSO *current_tso) {
  hs_hpc_tick(RixThreadFinishedOp,current_tso);
}

/* Called on every tick, dynamically, sending to our 
 * external record of program execution.
 */

void
hs_hpc_tick(int rixOp, StgTSO *current_tso) {

  debugTrace(DEBUG_hpc,"hs_hpc_tick(%x)",rixOp);

  if (rixFile == NULL) {
    return;
  }
  assert(rixCmdFile != NULL);
  StgThreadID tid = (current_tso == 0) ? 0 : current_tso->id;

  // now check to see if we have met a breakpoint condition
  if (rixCounter == rixBPCounter 
      || tid != previousTid) {
    breakPointCommand(rixOp,tid);
  } else {
    if (rixOp >= 0) {
      // Tix op
      if (tixBoxBP[rixOp] == 1) {	// reached a bp tixbox
	  breakPointCommand(rixOp,tid);
      }
    } else {
      // record the special operation
      breakPointCommand(rixOp,tid);
    }
  }
  // update the history information.
  previousTid = tid;
  rixOpBack[rixCounter % WOP_SIZE]  = rixOp;
  rixTidBack[rixCounter % WOP_SIZE] = tid;
  rixCounter++;

  debugTrace(DEBUG_hpc, "end: hs_hpc_tick");
}

static void 
printEvent(FILE *out,StgWord64 rixCounter,StgThreadID rixTid,HpcRixOp rixOp) {
  char prefixMsg[128];
  char suffixMsg[128];

  sprintf(prefixMsg,
	  "Event %" PRIuWORD64 " %u ",
	  rixCounter,
	  (unsigned int)rixTid);

  switch(rixOp) {
  case RixThreadFinishedOp:
    sprintf(suffixMsg,"ThreadFinished");
    break;
  case RixRaiseOp:
    sprintf(suffixMsg,"Raise");
    break;
  case RixFinishedOp:
    sprintf(suffixMsg,"Finished");
    break;
  default:
    sprintf(suffixMsg,"%u",rixOp);
  }

  fprintf(out,"%s%s\n",prefixMsg,suffixMsg);
  debugTrace(DEBUG_hpc,"sending %s%s",prefixMsg,suffixMsg);
}

static void
breakPointCommand(HpcRixOp rixOp, StgThreadID rixTid) {
  StgWord64 tmp64 = 0;
  unsigned int tmp = 0;

  if (getpid() != debuggee_pid) {
    // We are not the original process, to do not issue 
    // any events, and do not try to talk to the debugger.
    return;
  }

  debugTrace(DEBUG_hpc,"breakPointCommand %d %x",rixOp,(unsigned int)rixTid);

  printEvent(rixFile,rixCounter,rixTid,rixOp);
  fflush(rixFile);

  /* From here, you can ask some basic questions.
   * 
   *  c<nat>		set the (one) counter breakpoint
   *  s<nat>		set the (many) tickbox breakpoint
   *  u<nat>		unset the (many) tickbox breakpoint
   *  h			history

   * Note that you aways end up here on the first tick
   * because the rixBPCounter starts equal to 0.
   */
  int c = getc(rixCmdFile);
  while(c != 10 && c != -1) {
    switch(c) {
    case 'c': // c1234	-- set counter breakpoint at 1234
      c = getc(rixCmdFile);
      tmp64 = 0;
      while(isdigit(c)) {
	tmp64 = tmp64 * 10 + (c - '0');
	c = getc(rixCmdFile);
      }
      debugTrace(DEBUG_hpc,"setting countBP = %" PRIuWORD64,tmp64);

      rixBPCounter = tmp64;
      break;
    case 's': // s2323  -- set tick box breakpoint at 2323
      c = getc(rixCmdFile);
      tmp = 0;
      while(isdigit(c)) {
	tmp = tmp * 10 + (c - '0');
	c = getc(rixCmdFile);
      }

      debugTrace(DEBUG_hpc,"seting bp for tix %d",tmp);

      tixBoxBP[tmp] = 1;
      break;
    case 'u': // u2323  -- unset tick box breakpoint at 2323
      c = getc(rixCmdFile);
      tmp = 0;
      while(isdigit(c)) {
	tmp = tmp * 10 + (c - '0');
	c = getc(rixCmdFile);
      }

      debugTrace(DEBUG_hpc,"unseting bp for tix %d",tmp);

      tixBoxBP[tmp] = 0;
      break;
    case 'h': // h -- history of the last few (WOP_SIZE) steps 
      if (rixCounter > WOP_SIZE) {
	tmp64 = rixCounter - WOP_SIZE;
      } else {
	tmp64 = 0;
      }
      for(;tmp64 < rixCounter;tmp64++) {
	printEvent(rixFile,
		   tmp64,
		   rixTidBack[tmp64 % WOP_SIZE],
		   rixOpBack[tmp64 % WOP_SIZE]);
      }
      fflush(rixFile);
      c = getc(rixCmdFile);
      break;
    default:

      debugTrace(DEBUG_hpc,"strange command from HPCRIX (%d)",c);

      c = getc(rixCmdFile);
    }
    while (c != 10) {          // the end of the line
	c = getc(rixCmdFile); // to the end of the line
    }
    c = getc(rixCmdFile); // the first char on the next command
  }

  debugTrace(DEBUG_hpc,"leaving breakPointCommand");

}

/* This is called after all the modules have registered their local tixboxes,
 * and does a sanity check: are we good to go?
 */

void
startupHpc(void) {
  char *hpcRix;

  debugTrace(DEBUG_hpc,"startupHpc");
 
 if (hpc_inited == 0) {
    return;
  }
  // HPCRIX contains the name of the file to send our dynamic runtime output to (a named pipe).

  hpcRix = getenv("HPCRIX");
  if (hpcRix) {
    int comma;
    Info *tmpModule;  
    int rixFD, rixCmdFD;
    int tixCount = 0;

    assert(hpc_inited);

    if (sscanf(hpcRix,"%d:%d",&rixFD,&rixCmdFD) != 2) {
      /* Bad format for HPCRIX.
       */
      debugTrace(DEBUG_hpc,"Bad HPCRIX (%s)",hpcRix);
      exit(0);
    }

    debugTrace(DEBUG_hpc,"found HPCRIX pipes: %d:%d",rixFD,rixCmdFD);

    rixFile = fdopen(rixFD,"w");
    assert(rixFile != NULL);

    rixCmdFile = fdopen(rixCmdFD,"r");
    assert(rixCmdFile != NULL);

    // If we fork a process, then we do not want ticks inside
    // the sub-process to talk to the debugger. So we remember
    // our pid at startup time, so we can check if we are still
    // the original process.

    debuggee_pid = getpid();

    comma = 0;
    
    fprintf(rixFile,"Starting %s\n",prog_name);
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

      tixCount += tmpModule->tickCount;

      debugTrace(DEBUG_hpc,"(tracer)%s: %u (offset=%u) (hash=%u)\n",
		 tmpModule->modName,
		 tmpModule->tickCount,
		 tmpModule->hashNo,
		 tmpModule->tickOffset);

    }
    fprintf(rixFile,"]\n");
    fflush(rixFile);

    // Allocate the tixBox breakpoint array
    // These are set to 1 if you want to 
    // stop at a specific breakpoint
    tixBoxBP = (int *)calloc(tixCount,sizeof(int));
  }

}


static void
writeTix(FILE *f) {
  Info *tmpModule;  
  int i, inner_comma, outer_comma;

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

  FILE *f = fopen(tixFilename,"w");
  writeTix(f);

  if (rixFile != NULL) {
    hs_hpc_tick(RixFinishedOp,(StgThreadID)0);
    fclose(rixFile);
  }
  if (rixCmdFile != NULL) {
    fclose(rixCmdFile);
  }
  
}

void hs_hpc_read(char *filename) {
  Info *orig_modules = 0, *tmpModule, *tmpOrigModule;
  int i;

  orig_modules = modules;
  modules = 0;
  if (init_open(fopen(filename,"r"))) {
    readTix();
    // Now we copy across the arrays. O(n^2), but works
    for(tmpModule = modules;
	tmpModule != 0;
	tmpModule = tmpModule->next) {

      for(tmpOrigModule = orig_modules;
	  tmpOrigModule != 0;
	  tmpOrigModule = tmpOrigModule->next) {
	if (!strcmp(tmpModule->modName,tmpOrigModule->modName)) {    
	  assert(tmpModule->tixArr != 0);		
	  assert(tmpOrigModule->tixArr != 0);		
	  assert(tmpModule->tickCount == tmpOrigModule->tickCount);
	  for(i=0;i < tmpModule->tickCount;i++) {
	    tmpOrigModule->tixArr[i] = tmpModule->tixArr[i];
	  }
	  tmpModule->tixArr = tmpOrigModule->tixArr;
	  break;
	}
      }
    }
  }
}

void hs_hpc_write(char *filename) {
  writeTix(fopen(filename,"w"));
}



