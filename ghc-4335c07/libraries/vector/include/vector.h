#define PHASE_FUSED [1]
#define PHASE_INNER [0]

#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

#ifndef NOT_VECTOR_MODULE
import qualified Data.Vector.Internal.Check as Ck
#endif

#define ERROR          (Ck.error __FILE__ __LINE__)
#define INTERNAL_ERROR (Ck.internalError __FILE__ __LINE__)

#define CHECK(f) (Ck.f __FILE__ __LINE__)
#define BOUNDS_CHECK(f) (CHECK(f) Ck.Bounds)
#define UNSAFE_CHECK(f) (CHECK(f) Ck.Unsafe)
#define INTERNAL_CHECK(f) (CHECK(f) Ck.Internal)

#define PHASE_STREAM  Please use "PHASE_FUSED" instead
#define INLINE_STREAM Please use "INLINE_FUSED" instead
