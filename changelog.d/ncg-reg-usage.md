section: codegen
synopsis: Reduce allocation of register usage computation in the native code generator
issues:
mrs: !FIXME
description: {
  The liveness annotation now caches the registers read and written by each
  instruction, so the forward liveness pass and the linear register allocator
  no longer recompute them. The x86 backend also builds its register usage
  lists in one pass without a filter copy. Allocation of the native code
  generator drops by a few percent on large modules; generated code is
  unchanged.
}
