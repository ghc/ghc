/* --------------------------------------------------------------------------
   Time-stamp: <Sun Mar 18 2001 21:23:50 Stardate: [-30]6349.45 hwloidl>

   Low Level Communications Header (LLC.h)

   Contains the definitions used by the Low-level Communications
   module of the GUM Haskell runtime environment.
   Based on the Graph for PVM implementation.

   Phil Trinder, Glasgow University, 13th Dec 1994
   Adapted for the 4.xx RTS
   H-W. Loidl, Heriot-Watt, November 1999
   ----------------------------------------------------------------------- */

#ifndef __LLC_H
#define __LLC_H

#ifdef PAR

//@node Low Level Communications Header, , ,
//@section Low Level Communications Header

//@menu
//* Includes::			
//* Macros and Constants::	
//* PVM macros::		
//* Externs::			
//@end menu

//@node Includes, Macros and Constants, Low Level Communications Header, Low Level Communications Header
//@subsection Includes

#include "Rts.h"
#include "Parallel.h"

#include "PEOpCodes.h"
#include "pvm3.h"

//@node Macros and Constants, PVM macros, Includes, Low Level Communications Header
//@subsection Macros and Constants

#define	ANY_TASK	(-1)	/* receive messages from any task */
#define ANY_GLOBAL_TASK	ANY_TASK
#define ANY_OPCODE	(-1)	/* receive any opcode */
#define	ALL_GROUP	(-1)	/* wait for barrier from every group member */

#define	PEGROUP		"PE"

#define	MGRGROUP	"MGR"
#define	SYSGROUP	"SYS"


#define	PETASK		"PE"

//@node PVM macros, Externs, Macros and Constants, Low Level Communications Header
//@subsection PVM macros

#define	sync(gp,op)		do { \
                                  broadcast(gp,op); \
                                  pvm_barrier(gp,ALL_GROUP); \
                                } while(0)

#define broadcast(gp,op)	do { \
                                  pvm_initsend(PvmDataDefault); \
                                  pvm_bcast(gp,op); \
                                } while(0)

#define checkComms(c,s)		do { \
                                  if ((c)<0) { \
                                    pvm_perror(s); \
                                    stg_exit(EXIT_FAILURE); \
                                }} while(0)

#define _my_gtid		pvm_mytid()
#define GetPacket()             pvm_recv(ANY_TASK,ANY_OPCODE)
#define PacketsWaiting()	(pvm_probe(ANY_TASK,ANY_OPCODE) != 0)

#define SPARK_THREAD_DESCRIPTOR		1
#define GLOBAL_THREAD_DESCRIPTOR	2

#define _extract_jump_field(v)	(v)

#define MAX_DATA_WORDS_IN_PACKET	1024

/* basic PVM packing */
#define PutArg1(a)		pvm_pklong((long *)&(a),1,1)
#define PutArg2(a)		pvm_pklong((long *)&(a),1,1)
#define PutArgN(n,a)		pvm_pklong((long *)&(a),1,1)
#define PutArgs(b,n)		pvm_pklong((long *)b,n,1)

#define PutLit(l)		{ int a = l; PutArgN(?,a); }

/* basic PVM unpacking */
#define GetArg1(a)		pvm_upklong((long *)&(a),1,1)
#define GetArg2(a)		pvm_upklong((long *)&(a),1,1)
#define GetArgN(n,a)		pvm_upklong((long *)&(a),1,1)
#define GetArgs(b,n)		pvm_upklong((long *)b,n,1)

//@node Externs,  , PVM macros, Low Level Communications Header
//@subsection Externs

/* basic message passing routines */
extern void sendOp   (OpCode,GlobalTaskId),
            sendOp1  (OpCode,GlobalTaskId,StgWord),
            sendOp2  (OpCode,GlobalTaskId,StgWord,StgWord),
	    sendOpV  (OpCode,GlobalTaskId,int,...), 
            sendOpN  (OpCode,GlobalTaskId,int,StgPtr),
            sendOpNV (OpCode,GlobalTaskId,int,StgPtr,int,...);

extern void broadcastOpN(OpCode op, char *group, int n, StgPtr args);

/* extracting data out of a packet */
OpCode        getOpcode (rtsPacket p);
void          getOpcodeAndSender (rtsPacket p, OpCode *popcode, 
			          GlobalTaskId *psender_id);
GlobalTaskId  senderTask (rtsPacket p);
rtsPacket     waitForPEOp(OpCode op, GlobalTaskId who, void(*processUnexpected)(rtsPacket) );

/* Init and shutdown routines */
void          startUpPE (void);
void          shutDownPE(void);
int           getExitCode(int nbytes, GlobalTaskId *sender_idp);

/* aux functions */
char  *getOpName (unsigned op);  // returns string of opcode
void   processUnexpectedMessage (rtsPacket);
//void   NullException(void);

#endif /*PAR */
#endif /*defined __LLC_H */
