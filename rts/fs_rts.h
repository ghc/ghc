/* -----------------------------------------------------------------------------
 *
 * (c) Tamar Christina 2018
 *
 * Hack to get around linkewhole issues on linux. The FS utilities need to be in
 * a different namespace to allow the linking.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#undef FS_NAMESPACE
#define FS_NAMESPACE rts

#include "fs.h"
