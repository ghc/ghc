/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_LINKER_M32ALLOC
#define RTS_LINKER_M32ALLOC

#if RTS_LINKER_USE_MMAP
#include <fcntl.h>
#include <sys/mman.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#endif

/* MAP_ANONYMOUS is MAP_ANON on some systems, e.g. OS X, OpenBSD etc */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

#include "BeginPrivate.h"

#if RTS_LINKER_USE_MMAP
#define M32_NO_RETURN    /* Nothing */
#else
#define M32_NO_RETURN    GNUC3_ATTRIBUTE(__noreturn__)
#endif

void m32_allocator_init(void) M32_NO_RETURN;

void m32_allocator_flush(void) M32_NO_RETURN;

void m32_free(void *addr, size_t size) M32_NO_RETURN;

void * m32_alloc(size_t size, size_t alignment) M32_NO_RETURN;

void * mmapForLinker (size_t bytes, uint32_t flags, int fd, int offset);

#include "EndPrivate.h"

#endif
