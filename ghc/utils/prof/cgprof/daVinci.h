/* ------------------------------------------------------------------------
 * $Id: daVinci.h,v 1.1 2000/04/05 10:06:36 simonmar Exp $
 *									
 *	Copyright (C) 1995-2000 University of Oxford
 *									
 * Permission to use, copy, modify, and distribute this software,
 * and to incorporate it, in whole or in part, into other software,
 * is hereby granted without fee, provided that
 *   (1) the above copyright notice and this permission notice appear in
 *	 all copies of the source code, and the above copyright notice
 *	 appear in clearly visible form on all supporting documentation
 *	 and distribution media;
 *   (2) modified versions of this software be accompanied by a complete
 *	 change history describing author, date, and modifications made;
 *	 and
 *   (3) any redistribution of the software, in original or modified
 *	 form, be without fee and subject to these same conditions.
 * --------------------------------------------------------------------- */

#ifndef _DAVINCI_H_
#define _DAVINCI_H_
#include "symbol.h"
#include "matrix.h"
#include "cgprof.h"

#define PAIRMAX(x,y) (((x)>(y))?(x):(y))

#define SAFEDIV(x,y) (((y)==0.0)?0.0:((x)/(y)))

#define DAVINCI_ERROR        0
#define DAVINCI_OK           1
#define DAVINCI_NODE         2
#define DAVINCI_MENU         3
#define DAVINCI_ICON         4
#define DAVINCI_DOUBLE_CLICK 5
#define DAVINCI_QUIT         6
#define DAVINCI_TCL          7

#define TCL_HREL 0
#define TCL_COMP 1
#define TCL_COMM 2
#define TCL_WAIT 3
#define TCL_EXIT 4

#define INCLUDEDIR "@includedir@"

typedef struct {
  int    type;
  char **list;
  int size;
} davinciCmd;


#define CRITICAL_COMP              0
#define CRITICAL_COMM              1
#define CRITICAL_WAIT              2
#define CRITICAL_HREL              3
#define CRITICAL_SYNCS             4

#define CRITTYPE_ABSOLUTE     0
#define CRITTYPE_ABSDELTA     100
#define CRITTYPE_RELDELTA     200
#define CRITTYPE_WEIGHTDELTA  300

extern void graphToDaVinci(int,Matrix*,Matrix *,int);
davinciCmd  parseDaVinciCmd(char*);
extern void cmdDaVinci(char*,...);
extern void initDaVinci();
extern void activateDaVinciMenu(char *);  
extern void updateColours(int,Matrix*,Matrix*);
extern void tclPieUpdate(object_cost *,int,int);
extern void tclPieInit();


extern char* lastDavinciCmd;
extern int   NodeviewTime;
extern int   NodeviewCompress;
extern double TotalComp;
extern double TotalComm;
extern double TotalCompIdle;
extern int    TotalSyncs;
extern long int TotalH;
extern char  *dateProfiled;
extern char  *machineName;
extern int    bsp_p;
extern double bsp_s,bsp_l,bsp_g;
extern int CriticalPath;
extern int CriticalType;
extern double minNodeSize;
extern int bsp_p;
extern int PrintLogo;
extern int Colour;
extern int DeltaNormalise;
extern int PieCombine;
#endif
