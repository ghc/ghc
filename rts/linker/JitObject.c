#include "BufferBuilder.h"
#include "JitObject.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"

/*
 * GDB JIT interface
 */

typedef enum
{
  JIT_NOACTION = 0,
  JIT_REGISTER_FN,
  JIT_UNREGISTER_FN
} jit_actions_t;

struct jit_code_entry
{
  struct jit_code_entry *next_entry;
  struct jit_code_entry *prev_entry;
  const char *symfile_addr;
  uint64_t symfile_size;
};

struct jit_descriptor
{
  uint32_t version;
  /* This type should be jit_actions_t, but we use uint32_t
     to be explicit about the bitwidth.  */
  uint32_t action_flag;
  struct jit_code_entry *relevant_entry;
  struct jit_code_entry *first_entry;
};

/* GDB puts a breakpoint in this function.  */
void __jit_debug_register_code(void);
void __attribute__((noinline)) __jit_debug_register_code() { };

/* Make sure to specify the version statically, because the
   debugger may check the version before we can set it.  */
struct jit_descriptor __jit_debug_descriptor = { 1, 0, 0, 0 };

/*
 * Registering a JIT object
 */

void register_jit_object(ObjectCode *oc)
{
    struct BufferBuilder buf = build_jit_object(oc);

    struct jit_code_entry *code_entry = stgMallocBytes(sizeof(struct jit_code_entry), "register_jit_object");
    code_entry->next_entry = __jit_debug_descriptor.first_entry;
    code_entry->prev_entry = NULL;
    code_entry->symfile_addr = (const char *) buf.buffer;
    code_entry->symfile_size = buffer_builder_filled_size(&buf);

    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.first_entry = code_entry;
    __jit_debug_descriptor.relevant_entry = code_entry;
    __jit_debug_register_code();
}

