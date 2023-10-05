#pragma once

#include "BeginPrivate.h"

struct AdjustorPool;

/* N.B. the adjustor assembler snippets rely on the layout of this structure
 */
struct AdjustorContext {
    StgStablePtr hptr;
    StgFunPtr wptr;
};

typedef void (*mk_adjustor_code_fn)(uint8_t *exec_code, const void *context, void *user_data);

struct AdjustorPool *new_adjustor_pool(size_t context_sz, size_t code_sz, mk_adjustor_code_fn make_code, void *user_data);
void *alloc_adjustor(struct AdjustorPool *pool, void *context);
void free_adjustor(void *adjustor, void *context);

/* High-level interface: Adjustors from code template */
struct AdjustorTemplate {
    uint8_t *code_start;
    uint8_t *code_end;
    const struct AdjustorContext **context_ptr;
};

struct AdjustorPool *new_adjustor_pool_from_template(const struct AdjustorTemplate *tmpl);

#include "EndPrivate.h"
