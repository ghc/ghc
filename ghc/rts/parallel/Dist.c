#include "Dist.h"

#ifdef DIST /* whole file */

#include "RtsFlags.h"
#include "RtsUtils.h"
#include "ParallelRts.h"
#include "Parallel.h" // nPEs,allPEs,mytid 
#include "HLC.h" //for sendReval
#include "LLC.h" //for pvm stuff
#include "FetchMe.h"     // for BLOCKED_FETCH_info 
#include "Storage.h"       // for recordMutable

/* hopefully the result>0  */
StgWord32 cGetPECount(void)
{ return nPEs;
} 

/* return taskID, n is 1..count, n=1 is always the mainPE */
StgPEId cGetPEId(StgWord32 n)
{ return allPEs[n-1];
}

/* return the taskID */
StgPEId cGetMyPEId(void)
{ return mytid;
}

/* return the taskID of the owning PE of an MVar/TSO:
- MVAR/TSOs get converted to REMOTE_REFs when shipped, and
  there is no mechanism for using these REMOTE_REFs 
  apart from this code.
*/   

StgPEId cGetCertainOwner(StgClosure *mv)
{ globalAddr *ga; 
  switch(get_itbl(mv)->type)
  { case TSO:
    case MVAR:
      return  mytid; // must be local 
    case REMOTE_REF:
      ga = LAGAlookup(mv);
      ASSERT(ga);
      return ga->payload.gc.gtid; // I know its global address
  }   
  barf("Dist.c:cGetCertainOwner() wrong closure type %s",info_type(mv));
}

/* for some additional fun, lets look up a certain host... */
StgPEId cGetHostOwner(StgByteArray h) //okay h is a C string 
{ int nArch,nHost,nTask,i;
  StgPEId dtid;
  struct pvmhostinfo *host;   
  struct pvmtaskinfo *task;
  
  dtid=0;
  pvm_config(&nHost,&nArch,&host); 
  for(i=0;i<nHost;i++)
    if(strcmp(host[i].hi_name,h)==0) 
    { dtid=host[i].hi_tid;
      break;
    } 
  if(dtid==0) return 0; // no host of that name
  
  for(i=0;i<nPEs;i++)
  { pvm_tasks(allPEs[i],&nTask,&task);
    ASSERT(nTask==1); //cause we lookup a single task
    if(task[0].ti_host==dtid)
      return allPEs[i];
  }  
  return 0;  //know host, put no PE on it
}

void cRevalIO(StgClosure *job,StgPEId p)
{ nat size;
  rtsPackBuffer *buffer=NULL;
      
  ASSERT(get_itbl(job)->type==MVAR);  
  job=((StgMVar*)job)->value; // extract the job from the MVar

  ASSERT(closure_THUNK(job)); // must be a closure!!!!!
  ASSERT(p!=mytid);
  
  buffer = PackNearbyGraph(job, END_TSO_QUEUE, &size,p);
  ASSERT(buffer != (rtsPackBuffer *)NULL);
  ASSERT(get_itbl(job)->type==RBH);  
  
  IF_PAR_DEBUG(verbose,
               belch("@;~) %x doing revalIO to %x\n",
		     mytid,p)); 

  sendReval(p,size,buffer);  
  
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.tot_reval_mess++;
  }
  
  /* 
     We turn job into a FETCHME_BQ so that the thread will block
     when it enters it.
     
     Note: it will not receive an ACK, thus no GA.   
  */
  
  ASSERT(get_itbl(job)->type==RBH);  
 
   /* put closure on mutables list, while it is still a RBH */
  recordMutable((StgMutClosure *)job);

  /* actually turn it into a FETCH_ME_BQ */
  SET_INFO(job, &FETCH_ME_BQ_info);
  ((StgFetchMe *)job)->ga = 0;     //hope this won't make anyone barf!!!
  ((StgBlockingQueue*)job)->blocking_queue=END_BQ_QUEUE;
}

#endif
