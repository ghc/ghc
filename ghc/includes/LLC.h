/***********************************************************************
*       Low Level Communications Header (LLC.h)                        *
*       Contains the definitions used by the Low-level Communications  *
*       module of the GUM Haskell runtime environment.                 *
*       Based on the Graph for PVM implementation.                     *
*       Phil Trinder, Glasgow University, 13th Dec 1994                *
************************************************************************/

#ifndef __LLC_H
#define __LLC_H
#ifdef PAR

#include "rtsdefs.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#include "PEOpCodes.h"
#define safemalloc malloc
#include "pvm3.h"

#define	ANY_TASK	(-1)	/* receive messages from any task */
#define ANY_GLOBAL_TASK	ANY_TASK
#define ANY_OPCODE	(-1)	/* receive any opcode */
#define	ALL_GROUP	(-1)	/* wait for barrier from every group member */

#define	PEGROUP		"PE"

#define	MGRGROUP	"MGR"
#define	PECTLGROUP	"PECTL"


#define	PETASK		"PE"

#define	sync(gp,op)		do { broadcast(gp,op); pvm_barrier(gp,ALL_GROUP); } while(0)
#define broadcast(gp,op)	do { pvm_initsend(PvmDataDefault); pvm_bcast(gp,op); } while(0)
#define checkComms(c,s)		do {if((c)<0) { pvm_perror(s); EXIT(EXIT_FAILURE); }} while(0)

#define _my_gtid		pvm_mytid()
#define GetPacket()             pvm_recv(ANY_TASK,ANY_OPCODE)
#define PacketsWaiting()	(pvm_probe(ANY_TASK,ANY_OPCODE) != 0)

#define HandleException(p)      (*ExceptionHandler)(p)
#define _SetMyExceptionHandler(f) ExceptionHandler = f

#define SPARK_THREAD_DESCRIPTOR		1
#define GLOBAL_THREAD_DESCRIPTOR	2

#define _extract_jump_field(v)	(v)

#define MAX_DATA_WORDS_IN_PACKET	1024

#define PutArg1(a)		pvm_pklong(&(a),1,1)
#define PutArg2(a)		pvm_pklong(&(a),1,1)
#define PutArgN(n,a)		pvm_pklong(&(a),1,1)
#define PutArgs(b,n)		pvm_pklong(b,n,1)

#define PutLit(l)		{ int a = l; PutArgN(?,a); }

#define GetArg1(a)		pvm_upklong(&(a),1,1)
#define GetArg2(a)		pvm_upklong(&(a),1,1)
#define GetArgN(n,a)		pvm_upklong(&(a),1,1)
#define GetArgs(b,n)		pvm_upklong(b,n,1)

extern void SendOp PROTO((OPCODE,GLOBAL_TASK_ID)),
            SendOp1 PROTO((OPCODE,GLOBAL_TASK_ID,StgWord)),
            SendOp2 PROTO((OPCODE,GLOBAL_TASK_ID,StgWord,StgWord)),
	    SendOpV PROTO((OPCODE,GLOBAL_TASK_ID,int,...)), 
            SendOpN PROTO((OPCODE,GLOBAL_TASK_ID,int,StgWord *)),
            SendOpNV PROTO((OPCODE,GLOBAL_TASK_ID,int,StgWord*,int,...));

char *GetOpName PROTO((unsigned op));
void NullException(STG_NO_ARGS);

PACKET WaitForPEOp PROTO((OPCODE op, GLOBAL_TASK_ID who));
OPCODE Opcode PROTO((PACKET p));
GLOBAL_TASK_ID Sender_Task PROTO((PACKET p));
void get_opcode_and_sender PROTO((PACKET p, OPCODE *popcode, GLOBAL_TASK_ID *psender_id));
GLOBAL_TASK_ID *PEStartUp PROTO((unsigned nPEs));
void PEShutDown(STG_NO_ARGS);

extern void (*ExceptionHandler)();

#endif /*PAR */
#endif /*defined __LLC_H */
