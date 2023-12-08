/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020
 *
 * Continuations
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "RtsFlags.h"

#include "sm/Storage.h"
#include "sm/Sanity.h"
#include "Continuation.h"
#include "Printer.h"
#include "Threads.h"

#include <string.h>

/* Note [Continuations overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A first-class continuation is represented in the RTS as a closure with type
CONTINUATION (which corresponds to the StgContinuation struct type).
Continuation closures are similar to AP_STACK closures in that they store a
chunk of stack, but while AP_STACK closures are a special type of thunk,
continuation closures are a special type of *function*. More specifically, every
continuation is a function of arity 2, accepting one pointer and one RealWorld
token.

Continuation capture is performed through the use of two cooperating primops,
`prompt#` and `control0#`, which morally have the following types:

    prompt# :: PromptTag a -> IO a -> IO a
    control0# :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b

(In reality, their types use `State# RealWorld` rather than `IO` in the usual
way, but the type of control0# is nearly incomprehensible when presented in
those terms, so thinking in terms of `IO` is a helpful abbreviation.)

GHC implements *delimited* continuations: `prompt#` introduces a delimiter that
`control0#` looks for to determine how much of the local continuation should be
captured. Operationally, each use of `prompt#` pushes a *prompt frame* onto the
stack (annotated with a user-provided *prompt tag*), and each use of `control0#`
copies the portion of the stack up to the nearest prompt frame (with a matching
tag) into the heap to form a new continuation closure. `control0#` then aborts
to the prompt frame and resumes execution by applying the argument to
`control0#` to the continuation. This process is mostly handled in C, via
`captureContinuationAndAbort`.

When a continuation closure is applied, the process occurs in reverse: the chunk
of stack frames stored in the closure are pushed onto the current stack, and
execution resumes by applying the argument to the continuation to a RealWorld
token. This is a non-destructive operation---the caller is free to apply the
continuation arbitrarily many times. This process is handled in Cmm, via
`stg_CONTINUATION_apply` in ContinuationOps.cmm.

For the most part, capture and restoration of continuations is surprisingly
straightforward: the bulk of the work on each side of the process is just doing
the necessary copying. However, there are a few additional subtleties:

  * It is possible for continuation capture to *fail* if no matching prompt
    frame is on the stack or if the continuation would include thunk update or
    STM frames; see Note [When capturing the continuation fails] for details.

  * Special care must be taken to ensure the async exception masking state is
    properly updated across continuation captures and restores, see
    Note [Continuations and async exception masking] for details.

Note [When capturing the continuation fails]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How can continuation capture fail? There are three possible scenarios:

  1. There’s no matching prompt frame *anywhere* on the stack.
  2. The captured continuation would include a thunk update frame.
  3. The captured continuation would include part of an STM transaction.

The first case is fairly self-explanatory: if there’s no matching prompt frame,
we don’t know where to capture up to. The other two cases are important to
protect RTS invariants, as continuations can be applied arbitrarily many times,
but both thunk updates and STM transactions are non-reentrant.

Moreover, any attempt to capture across a thunk update frame is necessarily
ill-defined. Such frames indicate the start of a given `State#` thread (i.e.
they likely correspond to `unsafePerformIO` or `runST`), but the “current
continuation” is only predictable in code with a well-defined evaluation order.
Any attempt to capture across such a boundary would be correspondingly
unpredictable, so we want to be sure to reject it as programmer error. However,
note that we cannot detect and reject *all* such errors, see Note [Detecting
illegal captures is not guaranteed] for why.

To identify these error cases while searching for a matching prompt frame, we
also look for any stack frames that would indicate we’ve gone astray:

  1. If we see a STOP_FRAME, we’ve just plain run out of stack frames.
  2. To identify thunk updates, we can just look for UPDATE_FRAMEs.
  3. To identify STM transactions, we look for STM-related frames, namely
     ATOMICALLY_FRAME, CATCH_RETRY_FRAME, or CATCH_STM_FRAME.

If it finds any of these frames before a matching prompt frame,
`captureContinuationAndAbort` returns NULL, which `stg_control0zh` treats as a
signal that it should raise an exception.

Note [Detecting illegal captures is not guaranteed]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As alluded to in Note [When capturing the continuation fails], the “current
continuation” is only well-defined within a given `State#` thread. For a
concrete example illustrating why, consider the following program:

    do tag <- newPromptTag
       let v = unsafePerformIO (control0 tag (\k -> k ()))
       prompt tag (pure $! v)

If we were to allow this program, what would its result be? The answer depends
on how and when we evaluate `v`. If we allocate a thunk for `v` and force it
once the prompt has been installed, the program would successfully return `()`.
But since GHC can tell that `v` is used strictly, it may very well choose to
evaluate it immediately, before the prompt has been installed, in which case
there would be no matching prompt in scope at the time the call to `control0` is
evaluated, and the program would raise an error.

Without uses of `pseq`, GHC makes no guarantees about the order in which it will
evaluate pure expressions, so the optimizer may rearrange them significantly.
Therefore, any code that attempts such a capture is ill-defined, and we want to
do our best to detect and reject such mistakes. However, we cannot guarantee
that we will catch all such misuses! For example, given the program

    prompt tag (pure (1 + unsafePerformIO (control0 tag f)))

it is extremely unlikely that we will signal an error, despite the erroneous
capture. The reason is Note [Simplification of runRW#] in GHC.CoreToStg.Prep:
when `runRW#` appears in a strict context, there is no reason to allocate a
thunk, so GHC takes care to ensure it will not do so. With no thunk to update,
there is naturally no thunk update frame, so we cannot possibly detect at
runtime that anything was amiss.

Preserving the information necessary to reliably detect all of these sorts of
misuses at runtime in all situations would be disastrous for performance, so it
is the programmer’s responsibility to ensure this does not happen. Any program
for which continuation capture fails is a buggy program---there is NO WAY to
write a safe program that relies upon catching exceptions raised by continuation
capture failure. In other words, such programs invoke undefined behavior.

Given the behavior of such programs is already undefined, one might ask why we
bother detecting and reporting such failure conditions at all. In theory, we
could ignore thunk update frames completely and let the program behave
unpredictably. But detecting the failures we *can* detect is still worthwhile:

  * From the programmer’s point of view, best-effort detection and reporting of
    such misuses is still helpful, and the performance overhead of checking for
    them is minimal.

  * From the runtime’s point of view, detecting and eagerly rejecting such uses
    gives us much more confidence they will not violate internal invariants, so
    even if a buggy program does the wrong thing, it won’t corrupt the runtime.

In summary, the runtime does the best it can, but if it fails to detect and
report a misuse of `control0#`, the bug is in the program, not GHC.

Note [Continuations and async exception masking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It’s possible for a captured continuation to include a frame that alters the
async exception masking state. For example, consider the following program:

    prompt tag $ maskAsyncExceptions $
      control0 tag (\k -> ...) >>= do_something

The captured continuation will look like this:

    \m -> maskAsyncExceptions (m >>= do_something)

This situation requires some additional care:

  1. When aborting to the prompt as part of continuation capture, we need to
     restore the async exception masking state to whatever it was when the
     prompt frame was initially pushed.

  2. When restoring the continuation, we need to update the async exception
     masking state to whatever it was when the continuation was captured.

  3. When restoring the continuation, we need to update the pushed stack frames
     themselves to restore the new context’s async exception masking state when
     they return.

The third point is by far the most subtle, and it stems from the way primops
like `maskAsyncExceptions#` arrange to restore the async exception masking state
when their subcomputation returns. Specifically, when a primop like
`unmaskAsyncExceptions#`, `maskAsyncExceptions#`, or `maskUninterruptible#` is
called, it pushes one of three different frames onto the stack, depending on the
enclosing context’s masking state:

  * If exceptions were unmasked, it pushes `stg_unmaskAsyncExceptionszh_ret`.
  * If exceptions were interruptibly masked, it pushes `stg_maskAsyncExceptionszh_ret`.
  * If exceptions were uninterruptibly masked, it pushes `stg_maskUninterruptiblezh_ret`.

Note that, somewhat confusingly, which frame is pushed depends only on the
*enclosing* context’s masking state, *not* the new masking state installed for
the subcomputation. This works out, since the frame only exists to restore the
previous masking state, but it means the frames on the stack do not themselves
determine how the masking state was modified.

To cooperate with this strategy, we look for the aforementioned return
frames while walking the stack during continuation capture. If we find any of
them, we record two pieces of information:

  1. The captured continuation is necessarily responsible for whatever the
     masking state happens to be currently, so the *current* masking state must
     be restored upon continuation resumption. We set the `apply_mask_frame`
     field to a stack frame info pointer that will update the masking state
     accordingly if returned to.

  2. We set the `mask_frame_offset` field to the word offset of the *outermost*
     stack frame that restores the masking state. This serves a dual purpose:

       a. When we return to Cmm, `stg_control0zh` returns to this frame to
          restore the async exception masking state.

       b. When the continuation is restored, this frame is substituted with one
          that restores the masking state of the new context (i.e. the one in
          which the continuation is restored).

This is all quite subtle, so to illustrate with an example, suppose we have the
following state at the start of a continuation capture:

    ┌───────┐                                       ┌───────────────────┐
    │ STACK │                                       │    tso->flags     │
    ╞═══════╡                                       ╞═══════════════╤═══╡
    │  ...  │                                       │ BLOCKEX       │ 1 │
    ├───────┤                                       ├───────────────┼───┤
    │  RET  │──→ stg_maskAsyncExceptionszh_ret      │ INTERRUPTIBLE │ 0 │
    ├───────┤                                       └───────────────┴───┘
    │  ...  │
    ├───────┤
    │  RET  │──→ stg_unmaskAsyncExceptionszh_ret
    ├───────┤
    │  ...  │
    ├───────┤
    │  RET  │──→ stg_prompt_frame
    ├───────┤

We’ll copy the relevant stack frames into the heap, and we’ll set the
`apply_mask_frame` and `mask_frame_offset` fields accordingly:

    ┌───────────────────┐
    │   CONTINUATION    │
    ╞═══════════════════╡  ╭──→ stg_maskUninterruptiblezh_ret
    │ apply_mask_frame  │──╯
    ├───────────────────┤      ┌───────┐
    │      stack        │─────→│ STACK │
    ├───────────────────┤      ╞═══════╡
    │ mask_frame_offset │──╮   │  ...  │
    └───────────────────┘  │   ├───────┤
                           │   │  RET  │──→ stg_maskAsyncExceptionszh_ret
                           │   ├───────┤
                           │   │  ...  │
                           │   ├───────┤
                           ╰──→│  RET  │──→ stg_unmaskAsyncExceptionszh_ret
                               ├───────┤
                               │  ...  │
                               └───────┘

Next, `captureContinuationAndAbort` returns to Cmm, which sets up the stack so
that it can return to the frame indicated by `mask_frame_offset`, which in this
case is `stg_unmaskAsyncExceptionszh_ret`:

    ┌───────┐                                        ┌───────────────────┐
    │ STACK │                                        │    tso->flags     │
    ╞═══════╡                                        ╞═══════════════╤═══╡
    │  RET  │──→ stg_unmaskAsyncExceptionszh_ret     │ BLOCKEX       │ 1 │
    ├───────┤                                        ├───────────────┼───┤
    │  RET  │──→ stg_ap_pv                           │ INTERRUPTIBLE │ 0 │
    ├───────┤                                        └───────────────┴───┘
    │ cont  │
    ├───────┤
    │  ...  │
    ├───────┤

`stg_control0zh` then returns to `stg_unmaskAsyncExceptionszh`, which restores
the exception masking state appropriately. It then returns to `stg_ap_pv`, which
applies the handler to the captured continuation, and execution continues with
exceptions properly unmasked.

Next, let’s consider what happens when the continuation is restored. Suppose we
start in the following state:

    ┌───────┐                                        ┌───────────────────┐
    │ STACK │                                        │    tso->flags     │
    ╞═══════╡                                        ╞═══════════════╤═══╡
    │  ...  │                                        │ BLOCKEX       │ 1 │
    ├───────┤                                        ├───────────────┼───┤
                                                     │ INTERRUPTIBLE │ 1 │
                                                     └───────────────┴───┘

`stg_CONTINUATION_apply` will start by copying the frames from the continuation
back onto the stack, plus `apply_mask_frame` on top, which in this case is
`stg_maskUninterruptiblezh_ret`:

                         ┌───────┐
                         │ STACK │
                         ╞═══════╡
                         │  RET  │──→ stg_maskUninterruptiblezh_ret
                         ├───────┤
                         │  RET  │──→ stg_ap_v
                         ├───────┤
                         │  ...  │
                         ├───────┤
                         │  RET  │──→ stg_maskAsyncExceptionszh_ret
                         ├───────┤
                         │  ...  │
                         ├───────┤
    mask_frame_offset──→ │  RET  │──→ stg_unmaskAsyncExceptionszh_ret
                         ├───────┤
                         │  ...  │
                         ├───────┤

Next, it will update the frame at `mask_frame_offset` based on the current async
exception masking state. In this case, exceptions are interruptibly masked, so
the frame will be replaced with `stg_maskAsyncExceptionszh_ret`.
`stg_CONTINUATION_apply` will then return to the top of the stack, and
`stg_maskUninterruptiblezh_ret` will update the async exception masking state:

    ┌───────┐                                        ┌───────────────────┐
    │ STACK │                                        │    tso->flags     │
    ╞═══════╡                                        ╞═══════════════╤═══╡
    │  RET  │──→ stg_ap_v                            │ BLOCKEX       │ 1 │
    ├───────┤                                        ├───────────────┼───┤
    │  ...  │                                        │ INTERRUPTIBLE │ 0 │
    ├───────┤                                        └───────────────┴───┘
    │  RET  │──→ stg_maskAsyncExceptionszh_ret
    ├───────┤
    │  ...  │
    ├───────┤
    │  RET  │──→ stg_unmaskAsyncExceptionszh_ret
    ├───────┤
    │  ...  │
    ├───────┤

Control then returns to `stg_ap_v`, which applies the argument in R1 to resume
execution with exceptions re-masked, and we’re done. Phew.

One might naturally wonder why we bother with all this complicated indirection
involving returning to mask/unmask frames rather than just adjusting
`tso->flags` directly ourselves. That would indeed be significantly simpler,
but returning to `stg_unmaskAsyncExceptionszh_ret` has the important side-effect
of checking eagerly for a pending async exception and raising it if one is
available. So we do some tricky trampolining, and that frees us from having to
worry about that in the continuation capture/restore logic as well. */

static bool is_mask_frame_info(const StgInfoTable *info)
{
  return info == &stg_unmaskAsyncExceptionszh_ret_info
      || info == &stg_maskAsyncExceptionszh_ret_info
      || info == &stg_maskUninterruptiblezh_ret_info;
}

static StgStack *pop_stack_chunk(Capability *cap, StgTSO *tso)
{
  StgStack *stack = tso->stackobj;
  stack->sp = stack->stack + stack->stack_size - sizeofW(StgUnderflowFrame);
  threadStackUnderflow(cap, tso);
  return tso->stackobj;
}

// see Note [Continuations overview]
StgClosure *captureContinuationAndAbort(Capability *cap, StgTSO *tso, StgPromptTag prompt_tag)
{
  // We’d better own this thread if we’re doing this!
  ASSERT(tso->cap == cap);

  StgStack *stack = tso->stackobj;
  StgPtr frame = stack->sp;

  // We perform the capture in two phases:
  //
  //   1. We walk the stack to find the prompt frame to capture up to (if any).
  //
  //   2. If we successfully find a matching prompt, we proceed with the actual
  //      capture by allocating space for the continuation, performing the
  //      necessary copying, and unwinding the stack.
  //
  // These variables are modified in Phase 1 to keep track of how far we had to
  // walk before finding the prompt frame. Afterwards, Phase 2 consults them to
  // determine how to proceed.

  StgWord total_words = 0;
  bool in_first_chunk = true;
  StgWord first_chunk_words = 0;
  StgWord last_chunk_words = 0;
  StgWord full_chunks = 0;

  // see Note [Continuations and async exception masking]
  const StgInfoTable *apply_mask_frame = NULL;
  StgWord mask_frame_offset = 0;

  /* --- Phase 1: Find the matching prompt frame ---------------------------- */

  IF_DEBUG(continuation,
    debugBelch("captureContinuationAndAbort: searching for prompt\n");
    debugBelch("  prompt_tag = "); printClosure(prompt_tag));

  while (true) {
    IF_DEBUG(continuation,
      printStackChunk(frame, frame + stack_frame_sizeW((StgClosure *)frame)));

    const StgInfoTable *info_ptr = ((StgClosure *)frame)->header.info;
    const StgRetInfoTable *info = get_ret_itbl((StgClosure *)frame);
    StgWord chunk_words = frame - stack->sp;

    if (info_ptr == &stg_prompt_frame_info
        && ((StgPromptFrame *)frame)->tag == prompt_tag) {
      total_words += chunk_words;
      if (in_first_chunk) {
        first_chunk_words = chunk_words;
      } else {
        last_chunk_words = chunk_words;
      }
      break;
    }

    if (info->i.type == UNDERFLOW_FRAME) {
      total_words += chunk_words;
      if (in_first_chunk) {
        first_chunk_words = chunk_words;
      } else {
        full_chunks++;
      }

      stack = ((StgUnderflowFrame *)frame)->next_chunk;
      frame = stack->sp;
      in_first_chunk = false;
      continue;
    }

    // Finding any of these mean we failed to find the prompt frame;
    // see Note [When capturing the continuation fails] for details
    if (RTS_UNLIKELY(info->i.type == STOP_FRAME
                  || info->i.type == UPDATE_FRAME
                  || info->i.type == ATOMICALLY_FRAME
                  || info->i.type == CATCH_RETRY_FRAME
                  || info->i.type == CATCH_STM_FRAME)) {
      IF_DEBUG(continuation,
        debugBelch("captureContinuationAndAbort: could not find prompt, bailing out\n"));
      return NULL; // Bail out
    }

    // see Note [Continuations and async exception masking]
    if (is_mask_frame_info(info_ptr)) {
      mask_frame_offset = total_words + chunk_words;
      if (apply_mask_frame == NULL) {
        if ((tso->flags & TSO_BLOCKEX) == 0) {
          apply_mask_frame = &stg_unmaskAsyncExceptionszh_ret_info;
        } else if ((tso->flags & TSO_INTERRUPTIBLE) == 0) {
          apply_mask_frame = &stg_maskUninterruptiblezh_ret_info;
        } else {
          apply_mask_frame = &stg_maskAsyncExceptionszh_ret_info;
        }
      }
    }

    // Advance to the next frame.
    frame += stack_frame_sizeW((StgClosure *)frame);
  }

  /* --- Phase 2: Perform the capture --------------------------------------- */

  IF_DEBUG(continuation,
    debugBelch("captureContinuationAndAbort: found prompt, "
               "capturing %" FMT_Word " words of stack\n", total_words));

  dirty_TSO(cap, tso);
  dirty_STACK(cap, stack);

  StgContinuation *cont = (StgContinuation *)allocate(cap, CONTINUATION_sizeW(total_words));
  SET_HDR(cont, &stg_CONTINUATION_info, stack->header.prof.ccs);
  cont->apply_mask_frame = apply_mask_frame;
  cont->mask_frame_offset = mask_frame_offset;
  cont->stack_size = total_words;

  stack = tso->stackobj;
  StgPtr cont_stack = cont->stack;
  memcpy(cont_stack, stack->sp, first_chunk_words * sizeof(StgWord));
  cont_stack += first_chunk_words;

  if (in_first_chunk) {
    stack->sp += first_chunk_words;
  } else {
    stack = pop_stack_chunk(cap, tso);

    for (StgWord i = 0; i < full_chunks; i++) {
      const size_t chunk_words = stack->stack + stack->stack_size - stack->sp - sizeofW(StgUnderflowFrame);
      memcpy(cont_stack, stack->sp, chunk_words * sizeof(StgWord));
      cont_stack += chunk_words;
      stack = pop_stack_chunk(cap, tso);
    }

    memcpy(cont_stack, stack->sp, last_chunk_words * sizeof(StgWord));
    cont_stack += last_chunk_words;
    stack->sp += last_chunk_words;
  }

  ASSERT(cont->stack + total_words == cont_stack);
  ASSERT(((StgClosure *)stack->sp)->header.info == &stg_prompt_frame_info);
  stack->sp += stack_frame_sizeW((StgClosure *)frame);
  IF_DEBUG(sanity,
    checkClosure((StgClosure *)cont);
    checkTSO(tso));

  return TAG_CLOSURE(2, (StgClosure *)cont);
}
