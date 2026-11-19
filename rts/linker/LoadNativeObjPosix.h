#pragma once

#include "Rts.h"
#include "LinkerInternals.h"

void freeNativeCode_POSIX  ( ObjectCode *nc );
void *loadNativeObj_POSIX  ( pathchar *path, char **errmsg );
