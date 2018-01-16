/* -----------------------------------------------------------------------------
 *
 * (c) The AQUA Project, Glasgow University, 1994-1997
 * (c) The GHC Team, 1998-2006
 *
 * Functions for parsing the argument list.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* Routines that operate-on/to-do-with RTS flags: */

#if defined(mingw32_HOST_OS)
//The returned buffer has to be freed with stgFree()
char* lpcwstrToUTF8(const wchar_t* utf16_str);
char** getUTF8Args(int* argc);
#endif
void initRtsFlagsDefaults (void);
void setupRtsFlags        (int *argc, char *argv[], RtsConfig rtsConfig);
void freeRtsArgs          (void);

extern RtsConfig rtsConfig;

#include "EndPrivate.h"
