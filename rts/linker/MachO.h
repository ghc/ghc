#pragma once

#include "Rts.h"
#include "MachOTypes.h"

#include "BeginPrivate.h"

void   ocInit_MachO                 ( ObjectCode* oc );
void   ocDeinit_MachO               ( ObjectCode* oc );
int    ocVerifyImage_MachO          ( ObjectCode* oc );
int    ocBuildSegments_MachO        ( ObjectCode* oc );
int    ocGetNames_MachO             ( ObjectCode* oc );
int    ocResolve_MachO              ( ObjectCode* oc );
int    ocRunInit_MachO              ( ObjectCode* oc );
int    ocRunFini_MachO              ( ObjectCode* oc );
int    machoGetMisalignment         ( FILE *, int* );
int    ocAllocateExtras_MachO       ( ObjectCode* oc );

SectionKind getSectionKind_MachO    ( MachOSection *macho );

#include "EndPrivate.h"
