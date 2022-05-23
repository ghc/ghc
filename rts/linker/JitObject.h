#include "LinkerInternals.h"

struct JitObject {
    uint8_t *buffer;
    size_t size;
};

void register_jit_object(ObjectCode *oc);
struct JitObject build_jit_object(ObjectCode *oc);
