#pragma once

#include "Rts.h"

#include "BeginPrivate.h"

#include "MachOTypes.h"

void ocInit_MachO          ( ObjectCode* oc );
void ocDeinit_MachO        ( ObjectCode* oc );
int ocVerifyImage_MachO    ( ObjectCode* oc );
int ocGetNames_MachO       ( ObjectCode* oc );
int ocResolve_MachO        ( ObjectCode* oc );
int ocRunInit_MachO        ( ObjectCode* oc );
int machoGetMisalignment( FILE * );
int ocAllocateSymbolExtras_MachO ( ObjectCode* oc );

#if defined(powerpc_HOST_ARCH)
void machoInitSymbolsWithoutUnderscore( void );
#endif

#include "EndPrivate.h"
