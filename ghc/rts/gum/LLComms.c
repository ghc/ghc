/* -----------------------------------------------------------------------------
 *
 * $Id: LLComms.c,v 1.2 1998/12/02 13:29:06 simonm Exp $
 *
 * GUM Low-Level Inter-Task Communication
 *
 * This module defines PVM Routines for PE-PE  communication.
 *
 *     P. Trinder, December 5th. 1994.
 *     Adapted for the new RTS, P. Trinder July 1998
 *
 ---------------------------------------------------------------------------- */

#ifdef PAR /* whole file */

/*
 *This module defines the routines which communicate between PEs.  The
 *code is based on Kevin Hammond's GRIP RTS. (Opcodes.h defines
 *PEOp1 etc. in terms of SendOp1 etc.).  
 *
 *Routine	&	Arguments 
 *		&		
 *SendOp	&	0			\\
 *SendOp1	&	1			\\
 *SendOp2	&	2			\\
 *SendOpN	&	vector			\\
 *SendOpV	&	variable		\\
 *SendOpNV	&	variable+ vector	\\
 *
 *First the standard include files.
 */

#define NON_POSIX_SOURCE /* so says Solaris */

#include "Rts.h"
#include "RtsUtils.h"
#include "Parallel.h"

#include "LLC.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/*
 *Then some miscellaneous functions. 
 *GetOpName returns the character-string name of any opcode.
 */

char *UserPEOpNames[] = { PEOP_NAMES };

char *
GetOpName(nat op)
{
    if (op >= MIN_PEOPS && op <= MAX_PEOPS)
	return (UserPEOpNames[op - MIN_PEOPS]);

    else
	return ("Unknown PE Opcode");
}

/*
 * trace_SendOp handles the tracing of messages. 
 */

static void
trace_SendOp(OPCODE op, GLOBAL_TASK_ID dest UNUSED,
	     unsigned int data1 UNUSED, unsigned int data2 UNUSED)
{
    char *OpName;

    OpName = GetOpName(op);
/*    fprintf(stderr, " %s [%x,%x] sent from %x to %x\n", OpName, data1, data2, mytid, dest);*/
}

/*
 *SendOp sends a 0-argument message with opcode {\em op} to
 *the global task {\em task}.
 */

void
SendOp(OPCODE op, GLOBAL_TASK_ID task)
{
    trace_SendOp(op, task,0,0);

    pvm_initsend(PvmDataRaw);
    pvm_send( task, op );
}

/*
 *SendOp1 sends a 1-argument message with opcode {\em op}
 *to the global task {\em task}.
 */

void
SendOp1(OPCODE op, GLOBAL_TASK_ID task, StgWord arg1)
{
    trace_SendOp(op, task, arg1,0);

    pvm_initsend(PvmDataRaw);
    PutArg1(arg1);
    pvm_send( task, op );
}


/*
 *SendOp2 is used by the FP code only. 
 */

void
SendOp2(OPCODE op, GLOBAL_TASK_ID task, StgWord arg1, StgWord arg2)
{
    trace_SendOp(op, task, arg1, arg2);

    pvm_initsend(PvmDataRaw);
    PutArg1(arg1);
    PutArg2(arg2);
    pvm_send( task, op );
}

/*
 *
 *SendOpV takes a variable number of arguments, as specified by {\em n}.  
 *For example,
 *
 *    SendOpV( PP_STATS, StatsTask, 3, start_time, stop_time, sparkcount);
 */

void
SendOpV(OPCODE op, GLOBAL_TASK_ID task, int n, ...)
{
    va_list ap;
    int i;
    StgWord arg;

    va_start(ap, n);

    trace_SendOp(op, task, 0, 0);

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
 *SendOpNV takes a variable-size datablock, as specified by {\em
 *nelem} and a variable number of arguments, as specified by {\em
 *narg}. N.B. The datablock and the additional arguments are contiguous
 *and are copied over together.  For example,
 *
 *        SendOpNV(PP_RESUME, tsoga.pe, 6, nelem, data,
 *	    (W_) ga.weight, (W_) ga.loc.gc.gtid, (W_) ga.loc.gc.slot, 
 *	    (W_) tsoga.weight, (W_) tsoga.loc.gc.gtid, (W_) tsoga.loc.gc.slot);
 *
 *Important: The variable arguments must all be StgWords.
 */

void
SendOpNV(OPCODE op, GLOBAL_TASK_ID task, int nelem, 
	 StgWord *datablock, int narg, ...)
{
    va_list ap;
    int i;
    StgWord arg;

    va_start(ap, narg);

    trace_SendOp(op, task, 0, 0);
/*  fprintf(stderr,"SendOpNV: op = %x, task = %x, narg = %d, nelem = %d\n",op,task,narg,nelem); */

    pvm_initsend(PvmDataRaw);

    for (i = 0; i < narg; ++i) {
	arg = va_arg(ap, StgWord);
/*      fprintf(stderr,"SendOpNV: arg = %d\n",arg); */
	PutArgN(i, arg);
    }
    arg = (StgWord) nelem;
    PutArgN(narg, arg);

/*  for (i=0; i < nelem; ++i) fprintf(stderr, "%d ",datablock[i]); */
/*  fprintf(stderr," in SendOpNV\n");*/

    PutArgs(datablock, nelem);
    va_end(ap);

    pvm_send(task, op);
}

/*    
 *SendOpN take a variable size array argument, whose size is given by
 *{\em n}.  For example,
 *
 *    SendOpN( PP_STATS, StatsTask, 3, stats_array);
 */

void
SendOpN(OPCODE op, GLOBAL_TASK_ID task, int n, StgPtr args)
{
    long arg;

    trace_SendOp(op, task, 0, 0);

    pvm_initsend(PvmDataRaw);
    arg = (long) n;
    PutArgN(0, arg);
    PutArgs(args, n);
    pvm_send(task, op);
}

/*
 *WaitForPEOp waits for a packet from global task {\em who} with the
 *opcode {\em op}.  Other opcodes are handled by processUnexpected.
 */
PACKET 
WaitForPEOp(OPCODE op, GLOBAL_TASK_ID who)
{
  PACKET p;
  int nbytes;
  OPCODE opcode;
  GLOBAL_TASK_ID sender_id;
  rtsBool match;

  do {
#if 0
    fprintf(stderr,"WaitForPEOp: op = %x, who = %x\n",op,who); 
#endif
    while((p = pvm_recv(ANY_TASK,ANY_OPCODE)) < 0)
      pvm_perror("WaitForPEOp: Waiting for PEOp");
      
    pvm_bufinfo( p, &nbytes, &opcode, &sender_id );
#if 0
    fprintf(stderr,"WaitForPEOp: received: opcode = %x, sender_id = %x\n",opcode,sender_id); 
#endif
    match = (op == ANY_OPCODE || op == opcode) && (who == ANY_TASK || who == sender_id);

    if(match)
      return(p);

    /* Handle the unexpected opcodes */
    ProcessUnexpected(p);

  } while(rtsTrue);
}

/*
 *ProcessUnexpected processes unexpected messages. If the message is a
 *FINISH it exits the prgram, and PVM gracefully
 */
void
ProcessUnexpected(PACKET packet)
{
    OPCODE opcode = Opcode(packet);

#ifdef 0
    { 
      GLOBAL_TASK_ID sender = Sender_Task(packet); 
      fprintf(stderr,"ProcessUnexpected: Received %s (%x), sender %x\n",GetOpName(opcode),opcode,sender); 
    }
#endif 

    switch (opcode) {

    case PP_FINISH:
        stg_exit(EXIT_SUCCESS);
	break;

      /* Anything we're not prepared to deal with.  Note that ALL opcodes are discarded
	 during termination -- this helps prevent bizarre race conditions.
      */
      default:
	if (!GlobalStopPending) 
	  {
	    GLOBAL_TASK_ID ErrorTask;
	    int opcode;

            get_opcode_and_sender(packet,&opcode,&ErrorTask);
	    fprintf(stderr,"Task %x: Unexpected opcode %x from %x in ProcessUnexpected\n",
		    mytid, opcode, ErrorTask );
            
	    stg_exit(EXIT_FAILURE);
	  }
    }
}

OPCODE 
Opcode(PACKET p)
{
  int nbytes;
  OPCODE opcode;
  GLOBAL_TASK_ID sender_id;
  pvm_bufinfo( p, &nbytes, &opcode, &sender_id );
  return(opcode);
}

GLOBAL_TASK_ID
Sender_Task(PACKET p)
{
  int nbytes;
  OPCODE opcode;
  GLOBAL_TASK_ID sender_id;
  pvm_bufinfo( p, &nbytes, &opcode, &sender_id );
  return(sender_id);
}

void
get_opcode_and_sender(PACKET p, OPCODE *popcode, GLOBAL_TASK_ID *psender_id)
{
  int nbytes;
  pvm_bufinfo( p, &nbytes, popcode, psender_id );
}


/*
 *PEStartUp does the low-level comms specific startup stuff for a
 *PE. It initialises the comms system, joins the appropriate groups,
 *synchronises with the other PEs. Receives and records in a global
 *variable the task-id of SysMan. If this is the main thread (discovered
 *in main.lc), identifies itself to SysMan. Finally it receives
 *from SysMan an array of the Global Task Ids of each PE, which is
 *returned as the value of the function.
 */
GLOBAL_TASK_ID *
PEStartUp(nat nPEs)
{
    int i;
    PACKET addr;
    long *buffer = (long *) stgMallocBytes(sizeof(long) * nPEs, "PEStartUp (buffer)");
    GLOBAL_TASK_ID *PEs
      = (GLOBAL_TASK_ID *) stgMallocBytes(sizeof(GLOBAL_TASK_ID) * nPEs, "PEStartUp (PEs)");

    mytid = _my_gtid;		/* Initialise PVM and get task id into global var.*/

/*    fprintf(stderr,"PEStartup, Task id = [%x], No. PEs = %d \n", mytid, nPEs); */
    checkComms(pvm_joingroup(PEGROUP), "PEStartup");
/*    fprintf(stderr,"PEStartup, Joined PEGROUP\n"); */
    checkComms(pvm_joingroup(PECTLGROUP), "PEStartup");
/*    fprintf(stderr,"PEStartup, Joined PECTLGROUP\n"); */
    checkComms(pvm_barrier(PECTLGROUP, nPEs+1), "PEStartup");
/*    fprintf(stderr,"PEStartup, Passed PECTLGROUP barrier\n"); */

    addr = WaitForPEOp(PP_SYSMAN_TID, ANY_GLOBAL_TASK);
    SysManTask = Sender_Task(addr);
    if (IAmMainThread) {		/* Main Thread Identifies itself to SysMan */
	pvm_initsend(PvmDataDefault);
	pvm_send(SysManTask, PP_MAIN_TASK);
    } 
    addr = WaitForPEOp(PP_PETIDS, ANY_GLOBAL_TASK);
    GetArgs(buffer, nPEs);
    for (i = 0; i < nPEs; ++i) {
	PEs[i] = (GLOBAL_TASK_ID) buffer[i];
#if 0
	fprintf(stderr,"PEs[%d] = %x \n", i, PEs[i]); 
#endif
    }
    free(buffer);
    return PEs;
}

/*
 *PEShutdown does the low-level comms-specific shutdown stuff for a
 *single PE. It leaves the groups and then exits from pvm.
 */
void
PEShutDown(void)
{    
     checkComms(pvm_lvgroup(PEGROUP),"PEShutDown");
     checkComms(pvm_lvgroup(PECTLGROUP),"PEShutDown");
     checkComms(pvm_exit(),"PEShutDown");
}

/*
heapChkCounter tracks the number of heap checks since the last probe.
Not currently used! We check for messages when a thread is resheduled.
*/
int heapChkCounter = 0;

#endif /* PAR -- whole file */

