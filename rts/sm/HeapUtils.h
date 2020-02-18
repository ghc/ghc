/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * General utilities for walking the heap
 *
 * ---------------------------------------------------------------------------*/

#pragma once

typedef void (walk_closures_cb)(StgClosure **, void *);

INLINE_HEADER void
walk_large_bitmap(walk_closures_cb *cb,
                  StgClosure **p,
                  StgLargeBitmap *large_bitmap,
                  StgWord size,
                  void *user)
{
    // Bitmap may have more bits than `size` when scavenging PAP payloads. See
    // comments around StgPAP.
    ASSERT(large_bitmap->size >= size);

    uint32_t b = 0;

    for (uint32_t i = 0; i < size; b++) {
        StgWord bitmap = large_bitmap->bitmap[b];
        uint32_t j = stg_min(size-i, BITS_IN(W_));
        i += j;
        for (; j > 0; j--, p++) {
            if ((bitmap & 1) == 0) {
                cb(p, user);
            }
            bitmap = bitmap >> 1;
        }
    }
}
