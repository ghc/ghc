#pragma once

#include "Rts.h"
#include "LinkerInternals.h"

#include "BeginPrivate.h"

int ocVerifyImage_ELF    ( ObjectCode* oc );
int ocGetNames_ELF       ( ObjectCode* oc );
int ocResolve_ELF        ( ObjectCode* oc );
int ocRunInit_ELF        ( ObjectCode* oc );
int ocAllocateSymbolExtras_ELF( ObjectCode *oc );

#include "EndPrivate.h"
