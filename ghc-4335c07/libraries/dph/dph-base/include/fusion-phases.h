-------------------------------------------------------------------------------
-- These are the main simplifier phases used in DPH.
--  The phase is numbered after the set of bindings that are inlined in that
--  phase. We start from the "outermost" combinators produced by the
--  vectoriser and work our way down to the Data.Vector streams.
--

-- Inline bindings in user code, and closure functions.
-- This is dph-common-vseg:D.A.P.Lifted.Closure
#define PHASE_USER      [4]

-- Inline combinators that work on PArray and PData.
-- This is dph-common-vseg:D.A.P.PArray
#define PHASE_PA        [3]

-- Inline combinators from the unlifted backends
-- This is dph-prim-par:D.A.P.Unlifted.Parallel
--     and dph-prim-seq:D.A.P.Unlifted.Sequential
#define PHASE_BACKEND   [2]
#define PHASE_PAR       PHASE_BACKEND
#define PHASE_SEQ       PHASE_BACKEND

-- Inline combinators for distributed arrays.
-- This is dph-prim-par:D.A.P.Unlifted.Distributed
#define PHASE_DIST      [1]

-- Inline combinators for Data.Vector
#define PHASE_STREAM    [1]

-- Inline stuff in inner loops.
#define PHASE_INNER     [0]


#define UNTIL_PHASE_BACKEND [~2]


-------------------------------------------------------------------------------
-- More fine-grained inliner pragmas that we use to annotate the actual
-- bindings. When debugging it's useful to control exactly what gets inlined.

-- Bindings in hand-crafted example code.
#define INLINE_USER    INLINE PHASE_USER

-- Closure combinators produced by the vectoriser.
-- dph-common-vseg:D.A.P.Lifted.Closure
#define INLINE_CLOSURE INLINE PHASE_USER

-- Stuff that works on PArrays
-- dph-common-vseg:D.A.P.Lifted.Combinators
-- dph-common-vseg:D.A.P.PArray
#define INLINE_PA      INLINE PHASE_USER

-- dph-common-vseg:D.A.P.PArray.PData
#define INLINE_PDATA   INLINE PHASE_PA

-- Generic stuff in the unlifted backend.
#define INLINE_BACKEND INLINE PHASE_BACKEND

-- Unlifted parallel array combinators.
-- dph-prim-par:D.A.P.Unlifted.Parallel
#define INLINE_UP      INLINE PHASE_PAR

-- Unlifted sequential array combinators.
-- dph-prim-seq:D.A.P.Unlifted.Sequential
#define INLINE_U       INLINE PHASE_SEQ

-- dph-prim-par:D.A.P.Unlifted.Distributed
#define INLINE_DIST    INLINE PHASE_DIST

-- dph-prim-seq:D.A.P.Unlifted.Sequential.Vector
#define INLINE_STREAM  INLINE PHASE_STREAM


#define INLINE_INNER   INLINE PHASE_INNER
