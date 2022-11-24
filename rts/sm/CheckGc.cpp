extern "C" {
#include "Rts.h"
#include "StableName.h" /* for FOR_EACH_STABLE_NAME */
#include "StablePtr.h" /* for markStablePtrTable */
#include "Capability.h"
#include "HeapAlloc.h"
#include "STM.h"
#include "sm/NonMoving.h"
}

#include <iostream>
#include <fstream>
#include <set>
#include <vector>
#include <queue>
#include <unordered_set>

extern uint8_t nonmovingMarkEpoch;

class TaggedClosurePtr {
    StgClosure *ptr;
public:
    TaggedClosurePtr(StgClosure* ptr) : ptr(ptr) {}
    TaggedClosurePtr(StgClosure* ptr, uint8_t tag) : TaggedClosurePtr(TAG_CLOSURE(tag, ptr)) {}

    StgClosure *get_tagged() const {
        return ptr;
    }
    StgClosure *untag() const {
        return UNTAG_CLOSURE(ptr);
    }
    uint8_t get_tag() const {
        return (StgWord) ptr & TAG_MASK;
    }

    //inline StgClosure& operator->() { return *untag(); }

    friend inline bool operator==(const TaggedClosurePtr& lhs, const TaggedClosurePtr& rhs) {
        return lhs.ptr == rhs.ptr;
    }
    friend inline bool operator!=(const TaggedClosurePtr& lhs, const TaggedClosurePtr& rhs) { return !(lhs == rhs); }
    friend inline bool operator< (const TaggedClosurePtr& lhs, const TaggedClosurePtr& rhs) {
        return lhs.ptr < rhs.ptr;
    }
    friend inline bool operator> (const TaggedClosurePtr& lhs, const TaggedClosurePtr& rhs) { return rhs < lhs; }
    friend inline bool operator<=(const TaggedClosurePtr& lhs, const TaggedClosurePtr& rhs) { return !(lhs > rhs); }
    friend inline bool operator>=(const TaggedClosurePtr& lhs, const TaggedClosurePtr& rhs) { return !(lhs < rhs); }
};

template<>
struct std::hash<TaggedClosurePtr> {
    std::size_t operator()(TaggedClosurePtr const& p) const noexcept {
        return std::hash<StgClosure*>{}(p.get_tagged());
    }
};

class HeapVisitor {
public:
    // Visit an SRT
    virtual void visit_srt(StgClosure* c);

    // Visit a normal closure
    virtual void visit_closure(TaggedClosurePtr c);

    virtual void visit_thunk(StgThunk *thunk, size_t n_ptrs);
    virtual void visit_fun(StgClosure *constr, size_t n_ptrs);
    virtual void visit_constr(StgClosure *constr, size_t n_ptrs);
    virtual void visit_array(StgMutArrPtrs *arr);
    virtual void visit_small_array(StgSmallMutArrPtrs *arr);
    virtual void visit_bytearray(StgArrBytes* arr);

    virtual void visit_stack(StgPtr sp, StgPtr end);
    virtual void visit_tso(StgTSO* tso);
    virtual void visit_weak(StgWeak* w);
    virtual void visit_mvar(StgMVar* mvar);
    virtual void visit_tvar(StgTVar* tvar);
    virtual void visit_trec_header(StgTRecHeader *trec);
    virtual void visit_trec_chunk(StgTRecChunk* tc);
    virtual void visit_continuation(StgContinuation* tc);

    virtual void visit_small_bitmap(StgClosure *const *payload, StgWord bitmap, StgWord size);
    virtual void visit_large_bitmap(StgClosure *const *payload, const StgLargeBitmap *bitmap, StgWord size);
    void visit_pap_payload(StgClosure *fun, StgClosure **payload, StgWord n_args);

    virtual void visit_invalid(StgClosure *const c);
};

void HeapVisitor::visit_thunk(StgThunk *thunk, size_t n_ptrs)
{
    const StgInfoTable *info = get_itbl((StgClosure *) thunk);
    const StgThunkInfoTable *thunk_info = itbl_to_thunk_itbl(info);
    if (thunk_info->i.srt) {
        StgClosure *srt = (StgClosure*) GET_SRT(thunk_info);
        visit_srt(srt);
    };
    for (size_t i=0; i < n_ptrs; i++) {
        visit_closure(thunk->payload[i]);
    }
}

void HeapVisitor::visit_fun(StgClosure *fun, size_t n_ptrs)
{
    const StgInfoTable *info = get_itbl(fun);
    const StgFunInfoTable *fun_info = itbl_to_fun_itbl(info);
    if (fun_info->i.srt) {
        StgClosure *srt = (StgClosure*) GET_SRT(fun_info);
        visit_srt(srt);
    };
    for (size_t i=0; i < n_ptrs; i++) {
        visit_closure(fun->payload[i]);
    }
}

void HeapVisitor::visit_constr(StgClosure *constr, size_t n_ptrs)
{
    for (size_t i=0; i < n_ptrs; i++) {
        visit_closure(constr->payload[i]);
    }
}

void HeapVisitor::visit_srt(StgClosure* c)
{
    visit_closure(c);
}

void HeapVisitor::visit_invalid(StgClosure *const _c)
{
    abort();
}

void HeapVisitor::visit_weak(StgWeak* w)
{
    visit_closure(w->key);
    visit_closure(w->value);
    visit_closure(w->finalizer);
    visit_closure(w->cfinalizers);
}

void HeapVisitor::visit_mvar(StgMVar* mvar)
{
    visit_closure((StgClosure*) mvar->head);
    visit_closure((StgClosure*) mvar->tail);
    visit_closure(mvar->value);
}

void HeapVisitor::visit_small_array(StgSmallMutArrPtrs *arr)
{
    for (StgWord i=0; i < arr->ptrs; i++) {
        visit_closure(arr->payload[i]);
    }
}

void HeapVisitor::visit_array(StgMutArrPtrs *arr)
{
    for (StgWord i=0; i < arr->ptrs; i++) {
        visit_closure(arr->payload[i]);
    }
}

void HeapVisitor::visit_bytearray(StgArrBytes* _arr) { }

void HeapVisitor::visit_tso(StgTSO *tso)
{
    if (tso->bound != NULL) {

        visit_closure((StgClosure*) tso->bound->tso);
    }
    if (tso->label != NULL) {
        visit_closure({(StgClosure*) tso->label});
    }
    visit_closure((StgClosure*) tso->blocked_exceptions);
    visit_closure((StgClosure*) tso->bq);
    visit_closure((StgClosure*) tso->stackobj);
    visit_closure((StgClosure*) tso->_link);
    visit_trec_header(tso->trec);

    switch (tso->why_blocked) {
    case BlockedOnMVar:
    case BlockedOnMVarRead:
    case BlockedOnBlackHole:
    case BlockedOnMsgThrowTo:
    case NotBlocked:
        visit_closure(tso->block_info.closure);
        break;
    default:
        break;
    }
}

void HeapVisitor::visit_continuation(StgContinuation *cont)
{
    visit_stack(cont->stack, cont->stack + cont->stack_size);
}

void HeapVisitor::visit_tvar(StgTVar *tvar)
{
    visit_closure(tvar->current_value);
    visit_closure((StgClosure*) tvar->first_watch_queue_entry);
}

void HeapVisitor::visit_trec_header(StgTRecHeader *trec)
{
    if (trec == NO_TREC) {
        return;
    }
    visit_trec_chunk(trec->current_chunk);
    visit_closure((StgClosure*) trec->enclosing_trec);
}

void HeapVisitor::visit_trec_chunk(StgTRecChunk *tc)
{
    if (tc->prev_chunk != END_STM_CHUNK_LIST) {
        visit_closure((StgClosure*) tc->prev_chunk);
    }

    for (uint32_t i = 0; i < tc->next_entry_idx; i++) {
        TRecEntry *e = &tc->entries[i];
        visit_closure((StgClosure*)e->tvar);
        visit_closure(e->expected_value);
        visit_closure(e->new_value);
    }
}

void HeapVisitor::visit_stack(StgPtr p, StgPtr stack_end)
{
    while (p < stack_end) {
        const StgRetInfoTable* info = get_ret_itbl((StgClosure *) p);

        auto add_srt_ptrs = [&] () {
            if (info->i.srt) {
                StgClosure *srt = (StgClosure*)GET_SRT(info);
                visit_srt(srt);
            }
        };

        switch (info->i.type) {
  
        case UPDATE_FRAME:
        {
            StgUpdateFrame *frame = (StgUpdateFrame *)p;
            visit_closure(frame->updatee);
            p += sizeofW(StgUpdateFrame);
            continue;
        }
  
        case CATCH_STM_FRAME:
        case CATCH_RETRY_FRAME:
        case ATOMICALLY_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case CATCH_FRAME:
        case RET_SMALL:
        {
            StgWord bitmap = BITMAP_BITS(info->i.layout.bitmap);
            StgWord size   = BITMAP_SIZE(info->i.layout.bitmap);
            // NOTE: the payload starts immediately after the info-ptr, we
            // don't have an StgHeader in the same sense as a heap closure.
            p++;
            visit_small_bitmap((StgClosure**) p, bitmap, size);
            p += size;
            add_srt_ptrs();
            continue;
        }

        case RET_BCO:
        {
            p++;
            StgBCO *bco = (StgBCO *)*p;
            visit_closure((StgClosure *) bco);
            p++;
            StgWord size = BCO_BITMAP_SIZE(bco);
            visit_large_bitmap((StgClosure**) p, BCO_BITMAP(bco), size);
            p += size;
            continue;
        }

        case RET_BIG:
        {
            StgWord size = GET_LARGE_BITMAP(&info->i)->size;
            p++;
            visit_large_bitmap((StgClosure**) p, GET_LARGE_BITMAP(&info->i), size);
            p += size;
            // and don't forget to follow the SRT
            add_srt_ptrs();
            break;
        }

        case RET_FUN:
        {
            StgRetFun *ret_fun = (StgRetFun *)p;
            visit_closure(ret_fun->fun);

            const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
            switch (fun_info->f.fun_type) {
                case ARG_GEN:
                {
                    StgWord bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
                    StgWord size = BITMAP_SIZE(fun_info->f.b.bitmap);
                    visit_small_bitmap(ret_fun->payload, bitmap, size);
                    p = (StgPtr) ((StgClosure **) &ret_fun->payload + size);
                    break;
                }
                case ARG_GEN_BIG:
                {
                    StgWord size = GET_FUN_LARGE_BITMAP(fun_info)->size;
                    visit_large_bitmap(ret_fun->payload, GET_FUN_LARGE_BITMAP(fun_info), size);
                    p = (StgPtr) ((StgClosure **) &ret_fun->payload + size);
                    break;
                }
                default:
                {
                    StgWord bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
                    StgWord size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
                    visit_small_bitmap(ret_fun->payload, bitmap, size);
                    p = (StgPtr) ((StgClosure **) &ret_fun->payload + size);
                    break;
                }
            }
            add_srt_ptrs();
            break;
        }
        default:
            abort();
        }
    }
}

void HeapVisitor::visit_small_bitmap(
        StgClosure *const *payload,
        StgWord bitmap,
        StgWord size)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            visit_closure(*payload);
        }
        payload++;
        bitmap = bitmap >> 1;
        size--;
    }
}

void HeapVisitor::visit_large_bitmap(
        StgClosure *const * payload,
        const StgLargeBitmap *large_bitmap,
        StgWord size)
{
    // Bitmap may have more bits than `size` when scavenging PAP payloads. See
    // comments around StgPAP.
    ASSERT(large_bitmap->size >= size);

    uint32_t b = 0;
    for (uint32_t i = 0; i < size; b++) {
        StgWord bitmap = large_bitmap->bitmap[b];
        uint32_t j = stg_min(size-i, BITS_IN(W_));
        i += j;
        for (; j > 0; j--, payload++) {
            if ((bitmap & 1) == 0) {
                visit_closure(*payload);
            }
            bitmap = bitmap >> 1;
        }
    }
}

void HeapVisitor::visit_pap_payload(
        StgClosure *fun,
        StgClosure **payload,
        StgWord n_args)
{
    fun = UNTAG_CLOSURE(fun);
    const StgFunInfoTable *fun_info = get_fun_itbl(fun);
    ASSERT(fun_info->i.type != PAP);
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        visit_small_bitmap(payload, BITMAP_BITS(fun_info->f.b.bitmap), n_args);
        break;
    case ARG_GEN_BIG:
        visit_large_bitmap(payload, GET_FUN_LARGE_BITMAP(fun_info), n_args);
        break;
    case ARG_BCO:
        visit_large_bitmap(payload, BCO_BITMAP(fun), n_args);
        break;
    default:
    {
        StgWord bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        visit_small_bitmap(payload, bitmap, n_args);
    }
    }
}

void HeapVisitor::visit_closure(TaggedClosurePtr tagged)
{
    StgClosure *c = tagged.untag();
    if (c->header.info == (StgInfoTable *) 0xaaaaaaaaaaaaaaaa || !LOOKS_LIKE_CLOSURE_PTR(c)) {
        visit_invalid(c);
        return;
    }

    const StgInfoTable *info = get_itbl(c);
    auto generic_closure = [&] () {
        for (StgClosure **p = &c->payload[0]; p < &c->payload[info->layout.payload.ptrs]; p++) {
            visit_closure(*p);
        }
    };

    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY:
        visit_mvar((StgMVar *) c);
        break;
    case TVAR:
        visit_tvar((StgTVar *) c);
        break;

    case IND:
    case IND_STATIC:
        visit_closure(((StgInd *) c)->indirectee);
        break;

    case THUNK_0_1:
    case THUNK_0_2:
        visit_thunk((StgThunk*) c, 0);
        break;
    case THUNK_1_1:
    case THUNK_1_0:
        visit_thunk((StgThunk*) c, 1);
        break;
    case THUNK_2_0:
        visit_thunk((StgThunk*) c, 2);
        break;
    case THUNK:
        visit_thunk((StgThunk*) c, info->layout.payload.ptrs);
        break;
    case THUNK_STATIC:
        visit_thunk((StgThunk*) c, 0);
        break;

    case FUN_1_0:
        visit_fun(c, 1);
        break;
    case FUN_0_1:
    case FUN_0_2:
        visit_fun(c, 0);
        break;
    case FUN_1_1:
        visit_fun(c, 1);
        break;
    case FUN_2_0:
        visit_fun(c, 2);
        break;
    case FUN:
    case FUN_STATIC:
        visit_fun(c, info->layout.payload.ptrs);
        break;

    case CONSTR_0_1:
    case CONSTR_0_2:
        visit_constr(c, 0);
        break;
    case CONSTR_1_0:
    case CONSTR_1_1:
        visit_constr(c, 1);
        break;
    case CONSTR_2_0:
        visit_constr(c, 2);
        break;
    case CONSTR:
    case CONSTR_NOCAF:
        visit_constr(c, info->layout.payload.ptrs);
        break;

    case PRIM:
        generic_closure();
        break;
    case WEAK:
        visit_weak((StgWeak*) c);
        break;
    case BCO:
    {
        StgBCO *bco = (StgBCO *)c;
        visit_closure((StgClosure*) bco->instrs);
        visit_closure((StgClosure*) bco->literals);
        visit_closure((StgClosure*) bco->ptrs);
        break;
    }
    case BLACKHOLE:
    {
        StgInd *ind = (StgInd*) c;
        visit_closure(ind->indirectee);
        break;
    }
    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
    {
        StgMutVar *mv = (StgMutVar*) c;
        visit_closure(mv->var);
        break;
    }
    case BLOCKING_QUEUE:
    {
        StgBlockingQueue *bq = (StgBlockingQueue *)c;
        visit_closure((StgClosure*) bq->bh);
        visit_closure((StgClosure*) bq->owner);
        visit_closure((StgClosure*) bq->queue);
        visit_closure((StgClosure*) bq->link);
        break;
    }
    case THUNK_SELECTOR:
    {
        StgSelector *s = (StgSelector *)c;
        visit_closure(s->selectee);
        break;
    }
    case AP_STACK:
    {
        StgAP_STACK *ap = (StgAP_STACK *)c;
        visit_closure(ap->fun);
        visit_stack((StgPtr) ap->payload, (StgPtr) ap->payload + ap->size);
        break;
    }
    case PAP:
    {
        StgPAP *pap = (StgPAP*) c;
        visit_closure(pap->fun);
        visit_pap_payload(pap->fun, (StgClosure**) pap->payload, pap->n_args);
        break;
    }
    case AP:
    {
        StgAP *ap = (StgAP*) c;
        visit_closure(ap->fun);
        visit_pap_payload(ap->fun, (StgClosure**) ap->payload, ap->n_args);
        break;
    }
    case ARR_WORDS:
        visit_bytearray((StgArrBytes *) c);
        break;
    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
        visit_array((StgMutArrPtrs *) c);
        break;
    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
        visit_small_array((StgSmallMutArrPtrs *) c);
        break;
    case TSO:
        visit_tso((StgTSO *) c);
        break;
    case STACK:
    {
        StgStack *stack = (StgStack *) c;
        visit_stack(stack->sp, stack->stack + stack->stack_size);
        break;
    }
    case MUT_PRIM:
        generic_closure();
        break;
    case TREC_CHUNK:
        visit_trec_chunk((StgTRecChunk *) c);
        break;
    case CONTINUATION:
        visit_continuation((StgContinuation *) c);
        break;
    default:
        visit_invalid(c);
        break;
    }
}

class PredicatedHeapVisitor : HeapVisitor {
    bool should_visit(StgClosure *);

    virtual void visit_srt(StgClosure* c) {
        if (should_visit(c)) { HeapVisitor::visit_srt(c); }
    }

    virtual void visit_closure(TaggedClosurePtr c) {
        if (should_visit(c.untag())) { HeapVisitor::visit_closure(c); }
    }
};

// Collect direct pointers
struct CollectPointers : HeapVisitor {
    std::set<TaggedClosurePtr> accum;
    bool invalid;
    CollectPointers() : accum(), invalid(false) {}
    void visit_root(StgClosure *c) {
        HeapVisitor::visit_closure(c);
    }
    void visit_closure(TaggedClosurePtr c) {
        accum.insert(c);
    }
    void visit_invalid(StgClosure *const _c) { invalid = true; }
};

static std::set<TaggedClosurePtr> collect_pointers(StgClosure* c)
{
    CollectPointers v;
    v.visit_root(c);
    return v.accum;
}



struct Error {
    StgClosure *closure;
    std::string what;
    Error(StgClosure *closure, std::string what) : closure(closure), what(what) {}
};

static std::ostream& operator<<(std::ostream& os, const Error& err) {
    os << std::hex << "0x" << (StgWord) err.closure << ": " << err.what << "\n";
    return os;
}

class CheckVisitor : HeapVisitor {
    std::vector<Error> errors;
    uint8_t tag;
    void visit_constr(StgClosure* c) {
        const StgInfoTable *info = get_itbl(c);
        if (tag != 0) {
            uint8_t constr_tag = info->srt;  // zero-based
            if (tag != std::min(TAG_MASK, constr_tag+1)) {
                errors.push_back(Error(c, "invalid tag"));
            }
        }
    }

    void visit_closure(TaggedClosurePtr c) { }
public:
    const std::vector<Error>& get_errors() const { return errors; }

    void check_closure(TaggedClosurePtr c) {
        tag = c.get_tag();
        HeapVisitor::visit_closure(c);
    }
};

struct CheckGc {
    std::queue<TaggedClosurePtr> queue;
    std::unordered_set<TaggedClosurePtr> enqueued;

    void enqueue(TaggedClosurePtr ptr) {
        ASSERT(ptr != NULL);
        if (!is_enqueued(ptr)) {
            queue.push(ptr);
            enqueued.insert(ptr);
        }
    }

    bool finished() {
        return queue.empty();
    }

    TaggedClosurePtr pop() {
        TaggedClosurePtr p = queue.front();
        queue.pop();
        return p;
    }

    bool is_enqueued(TaggedClosurePtr ptr) {
        return enqueued.find(ptr) != enqueued.end();
    }
};

static void enqueue_root(void *user_data, StgClosure **root)
{
    CheckGc* env = (CheckGc*) user_data;
    env->enqueue(*root);
}

static void enqueue_roots(CheckGc& env)
{
    FOR_EACH_STABLE_NAME(p, if (p->sn_obj) env.enqueue(p->sn_obj););
    markStablePtrTable(enqueue_root, &env);
    for (uint32_t n = 0; n < getNumCapabilities(); n++) {
        markCapability(enqueue_root, (void*) &env, getCapability(n), false/*mark sparks*/);
    }
    markCAFs(enqueue_root, &env);

    for (StgWeak *w = nonmoving_weak_ptr_list; w != NULL; w = w->link) {
        env.enqueue((StgClosure *) w);
    }

    for (uint32_t g = 0; g <= N; g++) {
        generation *gen = &generations[g];
        for (StgWeak *w = gen->weak_ptr_list; w != NULL; w = RELAXED_LOAD(&w->link)) {
            env.enqueue((StgClosure *) w);
        }
    }
}

extern "C" {
void check_gc();
}

struct NodeName {
    const StgClosure *c;
    NodeName(const StgClosure *c) : c(c) {}
};

static std::ostream& operator<<(std::ostream& os, const NodeName& n) {
    os << std::hex << "\"" << n.c << "\"" << std::dec;
    return os;
}

static void dump_heap(std::ofstream& of)
{
    of << "digraph {\n";
    CheckGc env;
    enqueue_roots(env);
    while (!env.finished()) {
        TaggedClosurePtr tagged = env.pop();
        StgClosure* c = tagged.untag();
        NodeName n(c);
        if (c->header.info == (StgInfoTable *) 0xaaaaaaaaaaaaaaaa) {
            of << n << " [type=invalid];\n";
            continue;
        }

        const StgInfoTable *info = get_itbl(c);
        switch (info->type) {
            case CONSTR:
            case CONSTR_1_0:
            case CONSTR_0_1:
            case CONSTR_2_0:
            case CONSTR_1_1:
            case CONSTR_0_2:
            case CONSTR_NOCAF:
            {
                const StgConInfoTable *con_info = get_con_itbl(c);
                of << n << " [type=CONSTR constr=\"" << GET_CON_DESC(con_info) << "\"];\n";
                break;
            }
            case FUN:
            case FUN_1_0:
            case FUN_0_1:
            case FUN_2_0:
            case FUN_1_1:
            case FUN_0_2:
                of << n << " [type=FUN];\n";
                break;
            case FUN_STATIC:
                of << n << " [type=FUN_STATIC];\n";
                break;
            case THUNK:
            case THUNK_1_0:
            case THUNK_0_1:
            case THUNK_1_1:
            case THUNK_0_2:
            case THUNK_2_0:
                of << n << " [type=THUNK];\n";
                break;
            case THUNK_STATIC:
                of << n << " [type=THUNK_STATIC];\n";
                break;
            case THUNK_SELECTOR:
                of << n << " [type=THUNK_SEL];\n";
                break;
            case BCO:
                of << n << " [type=BCO];\n";
                break;
            case AP:
                of << n << " [type=AP];\n";
                break;
            case PAP:
                of << n << " [type=PAP];\n";
                break;
            case AP_STACK:
                of << n << " [type=AP_STACK];\n";
                break;
            case IND:
                of << n << " [type=IND];\n";
                break;
            case IND_STATIC:
                of << n << " [type=IND_STATIC];\n";
                break;
            case BLOCKING_QUEUE:
                of << n << " [type=BLOCKING_QUEUE];\n";
                break;
            case BLACKHOLE:
                of << n << " [type=BLACKHOLE];\n";
                break;
            case MVAR_CLEAN:
            case MVAR_DIRTY:
                of << n << " [type=MVAR];\n";
                break;
            case TVAR:
                of << n << " [type=TVAR];\n";
                break;
            case ARR_WORDS:
                of << n << " [type=ARR_WORDS];\n";
                break;
            case MUT_ARR_PTRS_CLEAN:
            case MUT_ARR_PTRS_DIRTY:
            case MUT_ARR_PTRS_FROZEN_CLEAN:
            case MUT_ARR_PTRS_FROZEN_DIRTY:
                of << n << " [type=MUT_ARR_PTRS];\n";
                break;
            case SMALL_MUT_ARR_PTRS_CLEAN:
            case SMALL_MUT_ARR_PTRS_DIRTY:
            case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
            case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY:
                of << n << " [type=SMALL_MUT_ARR_PTRS];\n";
                break;
            case MUT_VAR_CLEAN:
            case MUT_VAR_DIRTY:
                of << n << " [type=MUT_VAR];\n";
                break;
            case WEAK:
                of << n << " [type=WEAK];\n";
                break;
            case PRIM:
                of << n << " [type=PRIM];\n";
                break;
            case MUT_PRIM:
                of << n << " [type=MUT_PRIM];\n";
                break;
            case TSO:
                of << n << " [type=TSO];\n";
                break;
            case STACK:
                of << n << " [type=STACK];\n";
                break;
            case TREC_CHUNK:
                of << n << " [type=TREC_CHUNK];\n";
                break;
            case WHITEHOLE:
                of << n << " [type=WHITEHOLE];\n";
                break;
            case COMPACT_NFDATA:
                of << n << " [type=COMPACT_NFDATA];\n";
                break;
            case CONTINUATION:
                of << n << " [type=CONTINUATION];\n";
                break;
            default:
                of << n << " [type=unknown];\n";
                break;
        }

        if (!HEAP_ALLOCED((StgPtr) c)) {
            of << n << " [static=yes];\n";
        } else {
            bdescr *bd = Bdescr((StgPtr) c);
            of << n << " [gen=" << bd->gen_no << "];\n";
            if (bd->flags & BF_EVACUATED) {
                of << n << " [evacuated=yes];\n";
            }
            if (bd->flags & BF_PINNED) {
                of << n << " [pinned=yes];\n";
            }
            if (bd->flags & BF_LARGE) {
                of << n << " [large=yes];\n";
            } else if (bd->flags & BF_NONMOVING) {
                struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) c);
                nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) c);
                uint8_t mark = nonmovingGetMark(seg, block_idx);
                StgClosure *snapshot_loc =
                  (StgClosure *) nonmovingSegmentGetBlock(seg, nonmovingSegmentInfo(seg)->next_free_snap);
                if (c > snapshot_loc) {
                    of << n << " [nonmoving=yes new=yes mark=" << (StgWord) mark << "];\n";
                } else {
                    of << n << " [nonmoving=yes mark=" << (StgWord) mark << "];\n";
                }
            } else {
                of << n << " [moving=yes];\n";
            }
        }
        for (TaggedClosurePtr p : collect_pointers(c)) {
            of << n << " -> " << NodeName(p.untag()) << ";\n";
            env.enqueue(p);
        }
    }
    of << "}\n";
}

void dump_heap_to(const char *fname);
void dump_heap_to(const char *fname)
{
    std::ofstream out(fname);
    dump_heap(out);
    out.flush();
}

void check_gc()
{
    CheckGc env;
    enqueue_roots(env);
    std::vector<Error> errors;

    while (!env.finished()) {
        TaggedClosurePtr tagged = env.pop();
        StgClosure* c = tagged.untag();

        {
            CheckVisitor check;
            check.check_closure(tagged);
            for (const Error& e : check.get_errors()) {
                errors.push_back(e);
            }
        }

        for (TaggedClosurePtr p : collect_pointers(c)) {
            env.enqueue(p);
        }

        if (c->header.info == (StgInfoTable *) 0xaaaaaaaaaaaaaaaa) {
            errors.push_back(Error(c, "is invalid closure"));
            continue;
        }

        const StgInfoTable *info = get_itbl(c);
        if (!HEAP_ALLOCED((StgPtr) c)) {
            switch (info->type) {
            case THUNK_STATIC:
                if (info->srt != 0) {
            
                }
            }
        } else {
            bdescr *bd = Bdescr((StgPtr) c);
            if (bd->gen_no < 1) {
                /* nothing to check as we are focused on post nonmoving-GC checking */
            } else if (bd->flags & BF_NONMOVING && bd->flags & BF_LARGE) {
                if (bd->flags & BF_NONMOVING_SWEEPING && !(bd->flags & BF_MARKED)) {
                    errors.push_back(Error(c, "is not marked yet being swept"));
                }
            } else if (bd->flags & BF_NONMOVING) {
                struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) c);
                nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) c);
                uint8_t mark = nonmovingGetMark(seg, block_idx);
                StgClosure *snapshot_loc =
                  (StgClosure *) nonmovingSegmentGetBlock(seg, nonmovingSegmentInfo(seg)->next_free_snap);
                if (bd->flags & BF_NONMOVING_SWEEPING) {
                    /* in a swept segment */
                    if (mark != nonmovingMarkEpoch) {
                        errors.push_back(Error(c, "is unmarked nonmoving object being swept"));
                    }
                } else if (c < snapshot_loc) {
                    /* not in a swept segment but in the snapshot */
                    if (mark != nonmovingMarkEpoch) {
                        errors.push_back(Error(c, "is unmarked nonmoving object in the snapshot"));
                    }
                } else {
                    /* not in the snapshot; nothing to assert */
                }
            } else if (bd->flags & BF_LARGE) {
                if (! (bd->flags & BF_MARKED)) {
                    errors.push_back(Error(c, "is unmarked large object"));
                }
            } else {
                if (!(bd->flags & BF_EVACUATED)) {
                    //errors.push_back(Error(c, "is in from-space block"));
                }
            }
        }
    }

    if (!errors.empty()) {
        for (auto err : errors) {
            std::cerr << err << "\n";
        }
        dump_heap_to("heap.dot");
        abort();
    }
}

