
/*
 *         PVM version 3.3:  Parallel Virtual Machine System
 *               University of Tennessee, Knoxville TN.
 *           Oak Ridge National Laboratory, Oak Ridge TN.
 *                   Emory University, Atlanta GA.
 *      Authors:  A. L. Beguelin, J. J. Dongarra, G. A. Geist,
 *    W. C. Jiang, R. J. Manchek, B. K. Moore, and V. S. Sunderam
 *                   (C) 1992 All Rights Reserved
 *
 *                              NOTICE
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted
 * provided that the above copyright notice appear in all copies and
 * that both the copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * Neither the Institutions (Emory University, Oak Ridge National
 * Laboratory, and University of Tennessee) nor the Authors make any
 * representations about the suitability of this software for any
 * purpose.  This software is provided ``as is'' without express or
 * implied warranty.
 *
 * PVM version 3 was funded in part by the U.S. Department of Energy,
 * the National Science Foundation and the State of Tennessee.
 */

/*
 *	pvm3.h
 *
 *	Libpvm3 includes.
 *
$Log: pvm3.h,v $
Revision 1.1  1996/01/08 20:26:27  partain
Initial revision

 */

#ifndef	_PVM3_H_

#define	_PVM3_H_

#include	<sys/time.h>

/*
*	Data packing styles for pvm_initsend()
*/

#define	PvmDataDefault	0
#define	PvmDataRaw		1
#define	PvmDataInPlace	2
#define	PvmDataFoo		3

/*
*	pvm_spawn options
*/

#define	PvmTaskDefault	0
#define	PvmTaskHost		1	/* specify host */
#define	PvmTaskArch		2	/* specify architecture */
#define	PvmTaskDebug	4	/* start task in debugger */
#define	PvmTaskTrace	8	/* process generates trace data */
/* for MPP ports */
#define	PvmMppFront		16	/* spawn task on service node */
#define	PvmHostCompl	32	/* complement host set */

/*
*	pvm_notify types
*/

#define	PvmTaskExit		1	/* on task exit */
#define	PvmHostDelete	2	/* on host fail/delete */
#define	PvmHostAdd		3	/* on host startup */

/*
*	for pvm_setopt and pvm_getopt
*/

#define	PvmRoute			1	/* routing policy */
#define		PvmDontRoute		1	/* don't allow direct task-task links */
#define		PvmAllowDirect		2	/* allow direct links, but don't request */
#define		PvmRouteDirect		3	/* request direct links */
#define	PvmDebugMask		2	/* debugmask */
#define	PvmAutoErr			3	/* auto error reporting */
#define	PvmOutputTid		4	/* stdout destination for children */
#define	PvmOutputCode		5	/* stdout message tag */
#define	PvmTraceTid			6	/* trace destination for children */
#define	PvmTraceCode		7	/* trace message tag */
#define	PvmFragSize			8	/* message fragment size */
#define	PvmResvTids			9	/* allow reserved message tids and codes */
#define	PvmSelfOutputTid	10	/* stdout destination for task */
#define	PvmSelfOutputCode	11	/* stdout message tag */
#define	PvmSelfTraceTid		12	/* trace destination for task */
#define	PvmSelfTraceCode	13	/* trace message tag */
#define	PvmShowTids			14	/* pvm_catchout prints task ids with output */

/*
*	for pvm_[sg]ettmask
*/

#define	PvmTaskSelf		0	/* this task */
#define	PvmTaskChild	1	/* (future) child tasks */

/*
*	Libpvm error codes
*/

#define	PvmOk			0	/* Error 0 */
#define	PvmBadParam		-2	/* Bad parameter */
#define	PvmMismatch		-3	/* Count mismatch */
#define	PvmOverflow		-4	/* Value too large */
#define	PvmNoData		-5	/* End of buffer */
#define	PvmNoHost		-6	/* No such host */
#define	PvmNoFile		-7	/* No such file */
#define	PvmNoMem		-10	/* Malloc failed */
#define	PvmBadMsg		-12	/* Can't decode message */
#define	PvmSysErr		-14	/* Can't contact local daemon */
#define	PvmNoBuf		-15	/* No current buffer */
#define	PvmNoSuchBuf	-16	/* No such buffer */
#define	PvmNullGroup	-17	/* Null group name */
#define	PvmDupGroup		-18	/* Already in group */
#define	PvmNoGroup		-19	/* No such group */
#define	PvmNotInGroup	-20	/* Not in group */
#define	PvmNoInst		-21	/* No such instance */
#define	PvmHostFail		-22	/* Host failed */
#define	PvmNoParent		-23	/* No parent task */
#define	PvmNotImpl		-24	/* Not implemented */
#define	PvmDSysErr		-25	/* Pvmd system error */
#define	PvmBadVersion	-26	/* Version mismatch */
#define	PvmOutOfRes		-27	/* Out of resources */
#define	PvmDupHost		-28	/* Duplicate host */
#define	PvmCantStart	-29	/* Can't start pvmd */
#define	PvmAlready		-30	/* Already in progress */
#define	PvmNoTask		-31	/* No such task */
#define	PvmNoEntry		-32	/* No such entry */
#define	PvmDupEntry		-33	/* Duplicate entry */

/*
*	Data types for pvm_reduce(), pvm_psend(), pvm_precv()
*/

#define	PVM_STR			0	/* string */
#define	PVM_BYTE		1	/* byte */
#define	PVM_SHORT		2	/* short */
#define	PVM_INT			3	/* int */
#define	PVM_FLOAT		4	/* real */
#define	PVM_CPLX		5	/* complex */
#define	PVM_DOUBLE		6	/* double */
#define	PVM_DCPLX		7	/* double complex */
#define	PVM_LONG		8	/* long integer */
#define	PVM_USHORT		9	/* unsigned short int */
#define	PVM_UINT		10	/* unsigned int */
#define	PVM_ULONG		11	/* unsigned long int */

/*
*	returned by pvm_config()
*/

struct pvmhostinfo {
	int hi_tid;			/* pvmd tid */
	char *hi_name;		/* host name */
	char *hi_arch;		/* host arch */
	int hi_speed;		/* cpu relative speed */
};

/*
*	returned by pvm_tasks()
*/

struct pvmtaskinfo {
	int ti_tid;				/* task id */
	int ti_ptid;			/* parent tid */
	int ti_host;			/* pvmd tid */
	int ti_flag;			/* status flags */
	char *ti_a_out;			/* a.out name */
	int ti_pid;				/* task (O/S dependent) process id */
};


#ifdef __ProtoGlarp__
#undef __ProtoGlarp__
#endif
#if defined(__STDC__) || defined(__cplusplus)
#define __ProtoGlarp__(x) x
#else
#define __ProtoGlarp__(x) ()
#endif

#ifdef __cplusplus
extern "C" {
#endif

int pvm_addhosts	__ProtoGlarp__(( char **, int, int * ));
int pvm_archcode	__ProtoGlarp__(( char * ));
int pvm_barrier		__ProtoGlarp__(( char *, int ));
int pvm_bcast		__ProtoGlarp__(( char *, int ));
int pvm_bufinfo		__ProtoGlarp__(( int, int *, int *, int * ));
/*
int pvm_catchout	__ProtoGlarp__(( FILE * ));
*/
int pvm_config		__ProtoGlarp__(( int *, int *,
										struct pvmhostinfo ** ));
int pvm_delete		__ProtoGlarp__(( char *, int ));
int pvm_delhosts	__ProtoGlarp__(( char **, int, int * ));
int pvm_exit		__ProtoGlarp__(( void ));
int pvm_freebuf		__ProtoGlarp__(( int ));
int pvm_gather		__ProtoGlarp__(( void*, void*,
										int, int, int, char*, int));
int pvm_getfds		__ProtoGlarp__(( int ** ));
int pvm_getinst		__ProtoGlarp__(( char *, int ));
int pvm_getmwid		__ProtoGlarp__(( int ));
int pvm_getopt		__ProtoGlarp__(( int ));
int pvm_getrbuf		__ProtoGlarp__(( void ));
int pvm_getsbuf		__ProtoGlarp__(( void ));
int pvm_gettid		__ProtoGlarp__(( char *, int ));
int pvm_gsize		__ProtoGlarp__(( char * ));
int pvm_halt		__ProtoGlarp__(( void ));
int pvm_hostsync	__ProtoGlarp__(( int, struct timeval *,
										struct timeval * ));
int pvm_initsend	__ProtoGlarp__(( int ));
int pvm_insert		__ProtoGlarp__(( char *, int, int ));
int pvm_joingroup	__ProtoGlarp__(( char * ));
int pvm_kill		__ProtoGlarp__(( int ));
int pvm_lookup		__ProtoGlarp__(( char *, int, int * ));
int pvm_lvgroup		__ProtoGlarp__(( char * ));
int pvm_mcast		__ProtoGlarp__(( int *, int, int ));
int pvm_mkbuf		__ProtoGlarp__(( int ));
int pvm_mstat		__ProtoGlarp__(( char * ));
int pvm_mytid		__ProtoGlarp__(( void ));
int pvm_notify		__ProtoGlarp__(( int, int,
										int, int * ));
int pvm_nrecv		__ProtoGlarp__(( int, int ));
int pvm_packf		__ProtoGlarp__(( const char *, ... ));
int pvm_parent		__ProtoGlarp__(( void ));
int pvm_perror		__ProtoGlarp__(( char * ));
int pvm_pkbyte		__ProtoGlarp__(( char *, int, int ));
int pvm_pkcplx		__ProtoGlarp__(( float *, int, int ));
int pvm_pkdcplx		__ProtoGlarp__(( double *, int, int ));
int pvm_pkdouble	__ProtoGlarp__(( double *, int, int ));
int pvm_pkfloat		__ProtoGlarp__(( float *, int, int ));
int pvm_pkint		__ProtoGlarp__(( int *, int, int ));
int pvm_pklong		__ProtoGlarp__(( long *, int, int ));
int pvm_pkshort		__ProtoGlarp__(( short *, int, int ));
int pvm_pkstr		__ProtoGlarp__(( char * ));
int pvm_pkuint		__ProtoGlarp__(( unsigned int *, int, int ));
int pvm_pkulong		__ProtoGlarp__(( unsigned long *, int, int ));
int pvm_pkushort	__ProtoGlarp__(( unsigned short *, int, int ));
int pvm_precv		__ProtoGlarp__(( int, int,
									void *, int, int,
									int *, int *, int * ));
int pvm_probe		__ProtoGlarp__(( int, int ));
int pvm_psend		__ProtoGlarp__(( int, int,
									void *, int, int ));
int pvm_pstat		__ProtoGlarp__(( int ));
int pvm_recv		__ProtoGlarp__(( int, int ));
int (*pvm_recvf		__ProtoGlarp__(( int (*)(int, int, int) )) )();
int pvm_reduce		__ProtoGlarp__(( void (*)(int*, void*, void*, int*, int*),
									void *, int,
									int, int, char *,
									int ));

/*
*	Predefined pvm_reduce functions
*/
void PvmMax			__ProtoGlarp__(( int *, void *, void *,
									int *, int * ));
void PvmMin			__ProtoGlarp__(( int *, void *, void *,
									int *, int * ));
void PvmSum			__ProtoGlarp__(( int *, void *, void *,
									int *, int * ));
void PvmProduct		__ProtoGlarp__(( int *, void *, void *,
									int *, int * ));

int pvm_reg_hoster	__ProtoGlarp__(( void ));
int pvm_reg_rm		__ProtoGlarp__(( struct pvmhostinfo ** ));
int pvm_reg_tasker	__ProtoGlarp__(( void ));
int pvm_scatter		__ProtoGlarp__(( void*, void*,
										int, int, int, char*, int));
int pvm_send		__ProtoGlarp__(( int, int ));
int pvm_sendsig		__ProtoGlarp__(( int, int ));
int pvm_setmwid		__ProtoGlarp__(( int, int ));
int pvm_setopt		__ProtoGlarp__(( int, int ));
int pvm_setrbuf		__ProtoGlarp__(( int ));
int pvm_setsbuf		__ProtoGlarp__(( int ));
int pvm_spawn		__ProtoGlarp__(( char *, char **, int,
										char *, int, int * ));
int pvm_start_pvmd	__ProtoGlarp__(( int, char **, int ));
int pvm_tasks		__ProtoGlarp__(( int, int *,
										struct pvmtaskinfo ** ));
int pvm_tickle		__ProtoGlarp__(( int, int *,
										int *, int * ));
int pvm_tidtohost	__ProtoGlarp__(( int ));
int pvm_trecv		__ProtoGlarp__(( int, int, struct timeval * ));
int pvm_unpackf		__ProtoGlarp__(( const char *, ... ));
int pvm_upkbyte		__ProtoGlarp__(( char *, int, int ));
int pvm_upkcplx		__ProtoGlarp__(( float *, int, int ));
int pvm_upkdcplx	__ProtoGlarp__(( double *, int, int ));
int pvm_upkdouble	__ProtoGlarp__(( double *, int, int ));
int pvm_upkfloat	__ProtoGlarp__(( float *, int, int ));
int pvm_upkint		__ProtoGlarp__(( int *, int, int ));
int pvm_upklong		__ProtoGlarp__(( long *, int, int ));
int pvm_upkshort	__ProtoGlarp__(( short *, int, int ));
int pvm_upkstr		__ProtoGlarp__(( char * ));
int pvm_upkuint		__ProtoGlarp__(( unsigned int *, int, int ));
int pvm_upkulong	__ProtoGlarp__(( unsigned long *, int, int ));
int pvm_upkushort	__ProtoGlarp__(( unsigned short *, int, int ));
char *pvm_version	__ProtoGlarp__(( void ));

#ifdef __cplusplus
}
#endif

#endif	/*_PVM3_H_*/

