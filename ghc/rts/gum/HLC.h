/********************************************************************
*                 High Level Communications Header (HLC.h)          *
*                                                                   *
*  Contains the high-level definitions (i.e. communication          *
*  subsystem independent) used by GUM                               *
*  Phil Trinder, Glasgow University, 12 December 1994               *
*********************************************************************/

#ifndef __HLC_H
#define __HLC_H
#ifdef PAR

#include "LLC.h"

#define NEW_FISH_AGE        0
#define NEW_FISH_HISTORY    0
#define NEW_FISH_HUNGER     0
#define FISH_LIFE_EXPECTANCY 10

void sendFetch (globalAddr *ga, globalAddr *bqga, int load);
void sendResume (globalAddr *bqga, int nelem, P_ data);
void sendAck (GLOBAL_TASK_ID task, int ngas, globalAddr *gagamap);
void sendFish (GLOBAL_TASK_ID destPE, GLOBAL_TASK_ID origPE, int age, int history, int hunger);
void sendFree (GLOBAL_TASK_ID destPE, int nelem, P_ data);
void sendSchedule (GLOBAL_TASK_ID origPE, int nelem, P_ data);
void processMessages(void);
void processFetches(void);

void prepareFreeMsgBuffers(void);
void freeRemoteGA (int pe, globalAddr *ga);
void sendFreeMessages(void);

GLOBAL_TASK_ID choosePE(void);

void WaitForTermination(void);

void DebugPrintGAGAMap (globalAddr *gagamap, int nGAs);

void CommonUp (P_, P_);

#endif /* PAR */
#endif /* __HLC_H */
