/* Memory allocation routines.

Copyright (C) 1991, 1993, 1994, 2000 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include <stdio.h>
#include <stdlib.h> /* for malloc, realloc, free */

#include "gmp.h"
#include "gmp-impl.h"

#ifdef __NeXT__
#define static
#endif


void *	(*_mp_allocate_func) _PROTO ((size_t)) = _mp_default_allocate;
void *	(*_mp_reallocate_func) _PROTO ((void *, size_t, size_t))
     = _mp_default_reallocate;
void	(*_mp_free_func) _PROTO ((void *, size_t)) = _mp_default_free;


/* Default allocation functions.  In case of failure to allocate/reallocate
   an error message is written to stderr and the program aborts.  */

void *
#if __STDC__
_mp_default_allocate (size_t size)
#else
_mp_default_allocate (size)
     size_t size;
#endif
{
  void *ret;
#ifdef DEBUG
  size_t req_size = size;
  size += 2 * BYTES_PER_MP_LIMB;
#endif
  ret = malloc (size);
  if (ret == 0)
    {
      perror ("cannot allocate in gmp");
      abort ();
    }
  
#ifdef DEBUG
  {
    mp_ptr p = ret;
    p++;
    p[-1] = (0xdeadbeef << 31) + 0xdeafdeed;
    if (req_size % BYTES_PER_MP_LIMB == 0)
      p[req_size / BYTES_PER_MP_LIMB] = ~((0xdeadbeef << 31) + 0xdeafdeed);
    ret = p;
  }
#endif
  return ret;
}

void *
#if __STDC__
_mp_default_reallocate (void *oldptr, size_t old_size, size_t new_size)
#else
_mp_default_reallocate (oldptr, old_size, new_size)
     void *oldptr;
     size_t old_size;
     size_t new_size;
#endif
{
  void *ret;

#ifdef DEBUG
  size_t req_size = new_size;

  if (old_size != 0)
    {
      mp_ptr p = oldptr;
      if (p[-1] != (0xdeadbeef << 31) + 0xdeafdeed)
	{
	  fprintf (stderr, "gmp: (realloc) data clobbered before allocation block\n");
	  abort ();
	}
      if (old_size % BYTES_PER_MP_LIMB == 0)
	if (p[old_size / BYTES_PER_MP_LIMB] != ~((0xdeadbeef << 31) + 0xdeafdeed))
	  {
	    fprintf (stderr, "gmp: (realloc) data clobbered after allocation block\n");
	    abort ();
	  }
      oldptr = p - 1;
    }

  new_size += 2 * BYTES_PER_MP_LIMB;
#endif

  ret = realloc (oldptr, new_size);
  if (ret == 0)
    {
      perror ("cannot allocate in gmp");
      abort ();
    }

#ifdef DEBUG
  {
    mp_ptr p = ret;
    p++;
    p[-1] = (0xdeadbeef << 31) + 0xdeafdeed;
    if (req_size % BYTES_PER_MP_LIMB == 0)
      p[req_size / BYTES_PER_MP_LIMB] = ~((0xdeadbeef << 31) + 0xdeafdeed);
    ret = p;
  }
#endif
  return ret;
}

void
#if __STDC__
_mp_default_free (void *blk_ptr, size_t blk_size)
#else
_mp_default_free (blk_ptr, blk_size)
     void *blk_ptr;
     size_t blk_size;
#endif
{
#ifdef DEBUG
  {
    mp_ptr p = blk_ptr;
    if (blk_size != 0)
      {
	if (p[-1] != (0xdeadbeef << 31) + 0xdeafdeed)
	  {
	    fprintf (stderr, "gmp: (free) data clobbered before allocation block\n");
	    abort ();
	  }
	if (blk_size % BYTES_PER_MP_LIMB == 0)
	  if (p[blk_size / BYTES_PER_MP_LIMB] != ~((0xdeadbeef << 31) + 0xdeafdeed))
	    {
	      fprintf (stderr, "gmp: (free) data clobbered after allocation block\n");
	      abort ();
	    }
      }
    blk_ptr = p - 1;
  }
#endif
  free (blk_ptr);
}
