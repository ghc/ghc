/* ----------------------------------------------------------------------------
 * Time-stamp: <Wed Jan 12 2000 12:29:53 Stardate: [-30]4193.64 hwloidl>
 * $Id: LLComms.c,v 1.2 2000/01/13 14:34:07 hwloidl Exp $
 *
 * GUM Low-Level Inter-Task Communication
 *
 * This module defines PVM Routines for PE-PE  communication.
 * P. Trinder, December 5th. 1994.
 * Adapted for the new RTS 
 * P. Trinder, July 1998
 * H-W. Loidl, November 1999
 --------------------------------------------------------------------------- */

#ifdef PAR /* whole file */

//@node GUM Low-Level Inter-Task Communication, , ,
//@section GUM Low-Level Inter-Task Communication

/*
 *This module defines the routines which communicate between PEs.  The
 *code is based on Kevin Hammond's GRIP RTS. (OpCodes.h defines
 *PEOp1 etc. in terms of sendOp1 etc.).  
 *
 *Routine	&	Arguments 
 *		&		
 *sendOp	&	0			\\
 *sendOp1	&	1			\\
 *sendOp2	&	2			\\
 *sendOpN	&	vector			\\
 *sendOpV	&	variable		\\
 *sendOpNV	&	variable+ vector	\\
 *
 *First the standard include files.
 */

//@menu
//* Macros etc::		
//* Includes::			
//* Auxiliary functions::	
//* Index::			
//@end menu

//@node Macros etc, Includes, GUM Low-Level Inter-Task Communication, GUM Low-Level Inter-Task Communication
//@subsection Macros etc

#define NON_POSIX_SOURCE /* so says Solaris */
#define UNUSED           /* nothing */

//@node Includes, Auxiliary functions, Macros etc, GUM Low-Level Inter-Task Communication
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Parallel.h"
#include "ParallelRts.h"
#if defined(DEBUG)
# include "ParallelDebug.h"
#endif
#include "LLC.h"

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* Cannot use std macro when compiling for SysMan */
/* debugging enabled */
// #define IF_PAR_DEBUG(c,s)  { s; }
/* debugging disabled */
#define IF_PAR_DEBUG(c,s)  /* nothing */

//@node Auxiliary functions, Index, Includes, GUM Low-Level Inter-Task Communication
//@subsection Auxiliary functions

/*
 * heapChkCounter tracks the number of heap checks since the last probe.
 * Not currently used! We check for messages when a thread is resheduled.
 */
int heapChkCounter = 0;

/*
 * Then some miscellaneous functions. 
 * getOpName returns the character-string name of any OpCode.
 */

char *UserPEOpNames[] = { PEOP_NAMES };

//@cindex getOpName
char *
getOpName(nat op)
{
    if (op >= MIN_PEOPS && op <= MAX_PEOPS)
	return (UserPEOpNames[op - MIN_PEOPS]);
    else
	return ("Unknown PE OpCode");
}

/*
 * traceSendOp handles the tracing of messages. 
 */

//@cindex traceSendOp
static void
traceSendOp(OpCode op, GlobalTaskId dest UNUSED,
	     unsigned int data1 UNUSED, unsigned int data2 UNUSED)
{
    char *OpName;

    OpName = getOpName(op);
    IF_PAR_DEBUG(trace,
		 fprintf(stderr," %s [%x,%x] sent from %x to %x", 
		       OpName, data1, data2, mytid, dest));
}

/*
 * sendOp sends a 0-argument message with OpCode {\em op} to
 * the global task {\em task}.
 */

//@cindex sendOp
void
sendOp(OpCode op, GlobalTaskId task)
{
    traceSendOp(op, task,0,0);

    pvm_initsend(PvmDataRaw);
    pvm_send(task, op);
}

/*
 * sendOp1 sends a 1-argument message with OpCode {\em op}
 * to the global task {\em task}.
 */

//@cindex sendOp1
void
sendOp1(OpCode op, GlobalTaskId task, StgWord arg1)
{
    traceSendOp(op, task, arg1,0);

    pvm_initsend(PvmDataRaw);
    PutArg1(arg1);
    pvm_send(task, op);
}


/*
 * sendOp2 is used by the FP code only. 
 */

//@cindex sendOp2
void
sendOp2(OpCode op, GlobalTaskId task, StgWord arg1, StgWord arg2)
{
    traceSendOp(op, task, arg1, arg2);

    pvm_initsend(PvmDataRaw);
    PutArg1(arg1);
    PutArg2(arg2);
    pvm_send(task, op);
}

/*
 *
 * sendOpV takes a variable number of arguments, as specified by {\em n}.  
 * For example,
 *
 *    sendOpV( PP_STATS, StatsTask, 3, start_time, stop_time, sparkcount);
 */

//@cindex sendOpV
void
sendOpV(OpCode op, GlobalTaskId task, int n, ...)
{
    va_list ap;
    int i;
    StgWord arg;

    va_start(ap, n);

    traceSendOp(op, task, 0, 0);

    pvm_initsend(PvmDataRaw);

    for (i = 0; i < n; ++i) {
	arg = va_arg(ap, StgWord);
	PutArgN(i, arg);
    }
    va_end(ap);

    pvm_send(task, op);
}

/*    
 *
 * sendOpNV takes a variable-size datablock, as specified by {\em
 * nelem} and a variable number of arguments, as specified by {\em
 * narg}. N.B. The datablock and the additional arguments are contiguous
 * and are copied over together.  For example,
 *
 *        sendOpNV(PP_RESUME, tsoga.pe, 6, nelem, data,
 *	    (W_) ga.weight, (W_) ga.loc.gc.gtid, (W_) ga.loc.gc.slot, 
 *	    (W_) tsoga.weight, (W_) tsoga.loc.gc.gtid, (W_) tsoga.loc.gc.slot);
 *
 * Important: The variable arguments must all be StgWords.

 sendOpNV(_, tid, m, n, data, x1, ..., xm):

                         |   n elems
     +------------------------------
     | x1 | ... | xm | n | data ....
     +------------------------------
 */

//@cindex sendOpNV
void
sendOpNV(OpCode op, GlobalTaskId task, int nelem, 
	 StgWord *datablock, int narg, ...)
{
    va_list ap;
    int i;
    StgWord arg;

    va_start(ap, narg);

    traceSendOp(op, task, 0, 0);
    IF_PAR_DEBUG(trace,
		 fprintf(stderr,"sendOpNV: op = %x (%s), task = %x, narg = %d, nelem = %d",
		       op, getOpName(op), task, narg, nelem));

    pvm_initsend(PvmDataRaw);

    for (i = 0; i < narg; ++i) {
	arg = va_arg(ap, StgWord);
        IF_PAR_DEBUG(trace,
		     fprintf(stderr,"sendOpNV: arg = %d\n",arg));
	PutArgN(i, arg);
    }
    arg = (StgWord) nelem;
    PutArgN(narg, arg);

/*  for (i=0; i < nelem; ++i) fprintf(stderr, "%d ",datablock[i]); */
/*  fprintf(stderr," in sendOpNV\n");*/

    PutArgs(datablock, nelem);
    va_end(ap);

    pvm_send(task, op);
}

/*    
 * sendOpN take a variable size array argument, whose size is given by
 * {\em n}.  For example,
 *
 *    sendOpN( PP_STATS, StatsTask, 3, stats_array);
 */

//@cindex sendOpN
void
sendOpN(OpCode op, GlobalTaskId task, int n, StgPtr args)
{
    long arg;

    traceSendOp(op, task, 0, 0);

    pvm_initsend(PvmDataRaw);
    arg = (long) n;
    PutArgN(0, arg);
    PutArgs(args, n);
    pvm_send(task, op);
}

/*
 * waitForPEOp waits for a packet from global task {\em who} with the
 * OpCode {\em op}.  Other OpCodes are handled by processUnexpected.
 */
//@cindex waitForPEOp
rtsPacket 
waitForPEOp(OpCode op, GlobalTaskId who)
{
  rtsPacket p;
  int nbytes;
  OpCode opCode;
  GlobalTaskId sender_id;
  rtsBool match;

  do {
    IF_PAR_DEBUG(verbose,
		  fprintf(stderr,"waitForPEOp: op = %x (%s), who = %x\n", 
			  op, getOpName(op), who)); 

    while((p = pvm_recv(ANY_TASK,ANY_OPCODE)) < 0)
      pvm_perror("waitForPEOp: Waiting for PEOp");
      
    pvm_bufinfo( p, &nbytes, &opCode, &sender_id );
    IF_PAR_DEBUG(verbose,
		 fprintf(stderr,"waitForPEOp: received: OpCode = %x, sender_id = %x",
		       opCode, getOpName(opCode), sender_id)); 

    match = (op == ANY_OPCODE || op == opCode) && 
            (who == ANY_TASK || who == sender_id);

    if (match)
      return(p);

    /* Handle the unexpected OpCodes */
    processUnexpected(p);

  } while(rtsTrue);
}

/*
 * processUnexpected processes unexpected messages. If the message is a
 * FINISH it exits the prgram, and PVM gracefully
 */
//@cindex processUnexpected
void
processUnexpected(rtsPacket packet)
{
    OpCode opCode = getOpcode(packet);

    IF_PAR_DEBUG(verbose,
		 GlobalTaskId sender = senderTask(packet); 
		 fprintf(stderr,"== [%x] processUnexpected: Received %x (%s), sender %x\n",
		       mytid, opCode, getOpName(opCode), sender)); 

    switch (opCode) {
    case PP_FINISH:
        stg_exit(EXIT_SUCCESS);
	break;

      /* Anything we're not prepared to deal with.  Note that ALL OpCodes
	 are discarded during termination -- this helps prevent bizarre
	 race conditions.  */
      default:
	if (!GlobalStopPending) {
	  GlobalTaskId errorTask;
	  OpCode opCode;

	  getOpcodeAndSender(packet,&opCode,&errorTask);
	  fprintf(stderr,"Task %x: Unexpected OpCode %x from %x in processUnexpected",
		mytid, opCode, errorTask );
            
	  stg_exit(EXIT_FAILURE);
	}
    }
}

//@cindex getOpcode
OpCode 
getOpcode(rtsPacket p)
{
  int nbytes;
  OpCode OpCode;
  GlobalTaskId sender_id;
  pvm_bufinfo(p, &nbytes, &OpCode, &sender_id);
  return(OpCode);
}

//@cindex getOpcodeAndSender
void
getOpcodeAndSender(rtsPacket p, OpCode *opCodep, GlobalTaskId *senderIdp)
{
  int nbytes;
  pvm_bufinfo(p, &nbytes, opCodep, senderIdp);
}

//@cindex senderTask
GlobalTaskId
senderTask(rtsPacket p)
{
  int nbytes;
  OpCode opCode;
  GlobalTaskId sender_id;
  pvm_bufinfo(p, &nbytes, &opCode, &sender_id);
  return(sender_id);
}

/*
 * PEStartUp does the low-level comms specific startup stuff for a
 * PE. It initialises the comms system, joins the appropriate groups,
 * synchronises with the other PEs. Receives and records in a global
 * variable the task-id of SysMan. If this is the main thread (discovered
 * in main.lc), identifies itself to SysMan. Finally it receives
 * from SysMan an array of the Global Task Ids of each PE, which is
 * returned as the value of the function.
 */

//@cindex startUpPE
GlobalTaskId *
startUpPE(nat nPEs)
{
  int i;
  rtsPacket addr;
  long *buffer = (long *) stgMallocBytes(sizeof(long) * nPEs, 
					 "PEStartUp (buffer)");
  GlobalTaskId *thePEs = (GlobalTaskId *) 
    stgMallocBytes(sizeof(GlobalTaskId) * nPEs, 
		   "PEStartUp (PEs)");

  mytid = _my_gtid;	/* Initialise PVM and get task id into global var.*/

  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"== [%x] PEStartup: Task id = [%x], No. PEs = %d \n", 
		       mytid, mytid, nPEs));
  checkComms(pvm_joingroup(PEGROUP), "PEStartup");
  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"== [%x] PEStartup: Joined PEGROUP\n", mytid));
  checkComms(pvm_joingroup(PECTLGROUP), "PEStartup");
  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"== [%x] PEStartup: Joined PECTLGROUP\n", mytid));
  checkComms(pvm_barrier(PECTLGROUP, nPEs+1), "PEStartup");
  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"== [%x] PEStartup, Passed PECTLGROUP barrier\n", mytid));

  addr = waitForPEOp(PP_SYSMAN_TID, ANY_GLOBAL_TASK);
  SysManTask = senderTask(addr);
  if (IAmMainThread) {         /* Main Thread Identifies itself to SysMan */
    pvm_initsend(PvmDataDefault);
    pvm_send(SysManTask, PP_MAIN_TASK);
  } 
  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"== [%x] Thread waits for %s\n", 
		       mytid, getOpName(PP_PETIDS)));
  addr = waitForPEOp(PP_PETIDS, ANY_GLOBAL_TASK);
  GetArgs(buffer, nPEs);
  for (i = 0; i < nPEs; ++i) {
    thePEs[i] = (GlobalTaskId) buffer[i];
    IF_PAR_DEBUG(verbose,
		 fprintf(stderr,"== [%x] PEStartup: PEs[%d] = %x \n", 
			 mytid, i, thePEs[i])); 
  }
  free(buffer);
  return thePEs;
}

/*
 * PEShutdown does the low-level comms-specific shutdown stuff for a
 * single PE. It leaves the groups and then exits from pvm.
 */
//@cindex shutDownPE
void
shutDownPE(void)
{    
  IF_PAR_DEBUG(verbose,
	       fprintf(stderr, "== [%x] PEshutdown\n", mytid));

  checkComms(pvm_lvgroup(PEGROUP),"PEShutDown");
  checkComms(pvm_lvgroup(PECTLGROUP),"PEShutDown");
  checkComms(pvm_exit(),"PEShutDown");
}

#endif /* PAR -- whole file */

//@node Index,  , Auxiliary functions, GUM Low-Level Inter-Task Communication
//@subsection Index

//@index
//* getOpName::  @cindex\s-+getOpName
//* traceSendOp::  @cindex\s-+traceSendOp
//* sendOp::  @cindex\s-+sendOp
//* sendOp1::  @cindex\s-+sendOp1
//* sendOp2::  @cindex\s-+sendOp2
//* sendOpV::  @cindex\s-+sendOpV
//* sendOpNV::  @cindex\s-+sendOpNV
//* sendOpN::  @cindex\s-+sendOpN
//* waitForPEOp::  @cindex\s-+waitForPEOp
//* processUnexpected::  @cindex\s-+processUnexpected
//* getOpcode::  @cindex\s-+getOpcode
//* getOpcodeAndSender::  @cindex\s-+getOpcodeAndSender
//* senderTask::  @cindex\s-+senderTask
//* startUpPE::  @cindex\s-+startUpPE
//* shutDownPE::  @cindex\s-+shutDownPE
//@end index
