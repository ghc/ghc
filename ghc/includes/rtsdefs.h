#ifndef RTSDEFS_H
#define RTSDEFS_H

#ifdef __STG_GCC_REGS__
# if ! (defined(MAIN_REG_MAP) || defined(MARK_REG_MAP) || defined(SCAN_REG_MAP) || defined(SCAV_REG_MAP) || defined(FLUSH_REG_MAP))
#  define NULL_REG_MAP
# endif
#endif

#define IN_GHC_RTS 1

#include "stgdefs.h"

#endif	/* RTSDEFS_H */
