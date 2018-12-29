#pragma once

#include "Rts.h"
#include "LinkerInternals.h"

#include "BeginPrivate.h"

#include <linker/ElfTypes.h>

void ocInit_ELF          ( ObjectCode* oc );
void ocDeinit_ELF        ( ObjectCode* oc );
int ocVerifyImage_ELF    ( ObjectCode* oc );
int ocGetNames_ELF       ( ObjectCode* oc );
int ocResolve_ELF        ( ObjectCode* oc );
int ocRunInit_ELF        ( ObjectCode* oc );
int ocAllocateExtras_ELF ( ObjectCode *oc );

#include "EndPrivate.h"
