/* Stack allocation routines.  This is intended for machines without support
   for the `alloca' function.

Copyright (C) 1996, 1997, 1999, 2000 Free Software Foundation, Inc.

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

#include "stack-alloc.h"

#define __need_size_t
#include <stddef.h>
#undef __need_size_t

/* gmp-impl.h and stack-alloc.h conflict when not USE_STACK_ALLOC, so these
   declarations are copied here */
#if __STDC__
extern void *	(*__gmp_allocate_func) (size_t);
extern void	(*__gmp_free_func) (void *, size_t);
#else
extern void *	(*__gmp_allocate_func) ();
extern void	(*__gmp_free_func) ();
#endif

typedef struct tmp_stack tmp_stack;

static unsigned long max_total_allocation = 0;
static unsigned long current_total_allocation = 0;

static tmp_stack xxx = {&xxx, &xxx, 0};
static tmp_stack *current = &xxx;

/* The rounded size of the header of each allocation block.  */
#define HSIZ ((sizeof (tmp_stack) + __TMP_ALIGN - 1) & -__TMP_ALIGN)

/* Allocate a block of exactly <size> bytes.  This should only be called
   through the TMP_ALLOC macro, which takes care of rounding/alignment.  */
void *
#if __STDC__
__gmp_tmp_alloc (unsigned long size)
#else
__gmp_tmp_alloc (size)
     unsigned long size;
#endif
{
  void *that;

  if (size > (char *) current->end - (char *) current->alloc_point)
    {
      void *chunk;
      tmp_stack *header;
      unsigned long chunk_size;
      unsigned long now;

      /* Allocate a chunk that makes the total current allocation somewhat
	 larger than the maximum allocation ever.  If size is very large, we
	 allocate that much.  */

      now = current_total_allocation + size;
      if (now > max_total_allocation)
	{
	  /* We need more temporary memory than ever before.  Increase
	     for future needs.  */
	  now = now * 3 / 2;
	  chunk_size = now - current_total_allocation + HSIZ;
	  current_total_allocation = now;
	  max_total_allocation = current_total_allocation;
	}
      else
	{
	  chunk_size = max_total_allocation - current_total_allocation + HSIZ;
	  current_total_allocation = max_total_allocation;
	}

      chunk = (*__gmp_allocate_func) (chunk_size);
      header = (tmp_stack *) chunk;
      header->end = (char *) chunk + chunk_size;
      header->alloc_point = (char *) chunk + HSIZ;
      header->prev = current;
      current = header;
    }

  that = current->alloc_point;
  current->alloc_point = (char *) that + size;
  return that;
}

/* Typically called at function entry.  <mark> is assigned so that
   __gmp_tmp_free can later be used to reclaim all subsequently allocated
   storage.  */
void
#if __STDC__
__gmp_tmp_mark (tmp_marker *mark)
#else
__gmp_tmp_mark (mark)
     tmp_marker *mark;
#endif
{
  mark->which_chunk = current;
  mark->alloc_point = current->alloc_point;
}

/* Free everything allocated since <mark> was assigned by __gmp_tmp_mark */
void
#if __STDC__
__gmp_tmp_free (tmp_marker *mark)
#else
__gmp_tmp_free (mark)
     tmp_marker *mark;
#endif
{
  while (mark->which_chunk != current)
    {
      tmp_stack *tmp;

      tmp = current;
      current = tmp->prev;
      current_total_allocation -= (((char *) (tmp->end) - (char *) tmp) - HSIZ);
      (*__gmp_free_func) (tmp, (char *) tmp->end - (char *) tmp);
    }
  current->alloc_point = mark->alloc_point;
}
