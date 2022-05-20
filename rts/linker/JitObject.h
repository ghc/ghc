#include "LinkerInternals.h"
#include "BufferBuilder.h"

void register_jit_object(ObjectCode *oc);
struct BufferBuilder build_jit_object(ObjectCode *oc);
