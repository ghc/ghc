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

void sendFetch PROTO((globalAddr *ga, globalAddr *bqga, int load));
void sendResume PROTO((globalAddr *bqga, int nelem, P_ data));
void sendAck PROTO((GLOBAL_TASK_ID task, int ngas, globalAddr *gagamap));
void sendFish PROTO((GLOBAL_TASK_ID destPE, GLOBAL_TASK_ID origPE, int age, int history, int hunger));
void sendFree PROTO((GLOBAL_TASK_ID destPE, int nelem, P_ data));
void sendSchedule PROTO((GLOBAL_TASK_ID origPE, int nelem, P_ data));
void processMessages(STG_NO_ARGS);
void processFetches(STG_NO_ARGS);

void prepareFreeMsgBuffers(STG_NO_ARGS);
void freeRemoteGA PROTO((int pe, globalAddr *ga));
void sendFreeMessages(STG_NO_ARGS);

void Comms_Harness_Exception PROTO((PACKET packet));
void STG_Exception PROTO((PACKET));

GLOBAL_TASK_ID choosePE(STG_NO_ARGS);

void WaitForTermination(STG_NO_ARGS);

void DebugPrintGAGAMap PROTO((globalAddr *gagamap, int nGAs));

void CommonUp PROTO((P_, P_));

#endif /* PAR */
#endif /* __HLC_H */
