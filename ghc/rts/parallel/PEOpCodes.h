#ifndef PEOPCODES_H
#define PEOPCODES_H

/************************************************************************
*                         PEOpCodes.h                                   *
*									*
*	This file contains definitions for all the GUM PE Opcodes       *
*       It's based on the GRAPH for PVM version                         *
*       Phil Trinder, Glasgow University 8th December 1994              *
*									*
   RFPointon, December 1999
     - removed PP_SYSMAN_TID, introduced PP_READY
     - removed PP_MAIN_TASK, introduced PP_NEWPE
     - added PP_REVAL
************************************************************************/

#define REPLY_OK		0x00

/*Startup + Shutdown*/
#define	PP_READY		0x50  /* sent PEs -> SysMan */
#define	PP_NEWPE		0x51  /* sent via newHost notify -> SysMan */
#define	PP_FINISH		0x52  /* sent PEs & via taskExit notfiy -> SysMan */
#define	PP_PETIDS		0x53  /* sent sysman -> PEs */

/* Stats stuff */
#define	PP_STATS		0x54
#define PP_STATS_ON		0x55
#define PP_STATS_OFF		0x56

//#define PP_FAIL		0x57 

/*Garbage Collection*/
#define PP_GC_INIT              0x58
#define PP_FULL_SYSTEM          0x59
#define PP_GC_POLL              0x5a

/*GUM Messages*/
#define PP_FETCH                0x5b
#define PP_RESUME               0x5c
#define PP_ACK                  0x5d
#define PP_FISH                 0x5e
#define PP_SCHEDULE             0x5f
#define PP_FREE			0x60
#define PP_REVAL		0x61


#define	MIN_PEOPS		0x50
#define	MAX_PEOPS		0x61

#define	PEOP_NAMES		"Ready", "NewPE", \
				"Finish", "PETIDS", \
                                "Stats", "Stats_On", "Stats_Off", \
  				"Fail", \
                                "GCInit", "FullSystem", "GCPoll", \
                                "Fetch","Resume","ACK","Fish","Schedule", \
				"Free","REval"

#endif /* PEOPCODES_H */
