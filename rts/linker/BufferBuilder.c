#include "Rts.h"
#include "RtsUtils.h"
#include "BufferBuilder.h"
#include <string.h>

struct BufferBuilder buffer_builder_new(size_t initial_sz)
{
    uint8_t *buffer = stgMallocBytes(initial_sz, "new_buffer_builder");
    return (struct BufferBuilder) {
        .buffer = buffer,
        .head = buffer,
        .end = buffer + initial_sz,
    };
}

void buffer_builder_free(struct BufferBuilder *bb)
{
    stgFree(bb->buffer);
    bb->buffer = NULL;
    bb->head = NULL;
    bb->end = NULL;
}

size_t buffer_builder_filled_size(struct BufferBuilder *bb)
{
    return bb->head - bb->buffer;
}

size_t buffer_builder_reserved_size(struct BufferBuilder *bb)
{
    return bb->end - bb->buffer;
}

void buffer_builder_realloc(struct BufferBuilder *bb, size_t new_sz)
{
    size_t filled_sz = buffer_builder_filled_size(bb);
    ASSERT(filled_sz <= new_sz);
    uint8_t *buffer = stgReallocBytes(bb->buffer, new_sz, "new_buffer_builder");
    bb->buffer = buffer;
    bb->head = buffer + filled_sz;
    bb->end = buffer + new_sz;
}

uint8_t *buffer_builder_push(struct BufferBuilder *bb, uint8_t *x, size_t sz)
{
    if (bb->head + sz > bb->end) {
        buffer_builder_realloc(bb, 2*buffer_builder_reserved_size(bb));
    }

    memcpy(bb->head, x, sz);
    uint8_t *ret = bb->head;
    bb->head += sz;
    return ret;
}

void buffer_builder_append(struct BufferBuilder *bb, struct BufferBuilder *src)
{
    size_t sz = buffer_builder_filled_size(src);
    buffer_builder_push(bb, bb->buffer, sz);
}

