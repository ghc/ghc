#pragma once

#include <stddef.h>
#include <stdint.h>

struct BufferBuilder {
    uint8_t *buffer; // start of buffer
    uint8_t *head;   // next unfilled byte
    uint8_t *end;    // end of buffer
};

struct BufferBuilder buffer_builder_new(size_t initial_sz);
void buffer_builder_free(struct BufferBuilder *bb);
size_t buffer_builder_filled_size(struct BufferBuilder *bb);
size_t buffer_builder_reserved_size(struct BufferBuilder *bb);
void buffer_builder_realloc(struct BufferBuilder *bb, size_t new_sz);
uint8_t *buffer_builder_push(struct BufferBuilder *bb, uint8_t *x, size_t sz);
void buffer_builder_append(struct BufferBuilder *bb, struct BufferBuilder *src);

#define buffer_builder_push_struct(bb, x) \
    (typeof(x)*) buffer_builder_push(bb, (uint8_t *) &x, sizeof(x))

#define DEFINE_BUILDER(ty) \
    static inline ty ## _t* buffer_builder_ ## ty(struct BufferBuilder *bb, ty ## _t x) \
        { return (ty ## _t *) buffer_builder_push(bb, (uint8_t *) &x, sizeof(x)); }

DEFINE_BUILDER(uint8);
DEFINE_BUILDER(uint16);
DEFINE_BUILDER(uint32);
DEFINE_BUILDER(uint64);
DEFINE_BUILDER(int8);
DEFINE_BUILDER(int16);
DEFINE_BUILDER(int32);
DEFINE_BUILDER(int64);

#undef DEFINE_BUILDER

