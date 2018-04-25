#lang racket
(require redex)

; An STG-like language as described in
; "Making a Fast Curry: Push/Enter vs. Eval/Apply for Higher-order Languages"
; Only lightly-tested.

; A list of differences from STG proper:
;   * Right-hand sides of let-bindings have different syntax
;   * Let-binding is not recursive
;   * Missing semantics for top-level bindings
;   * Missing semantics for primops, e.g.
;       * Exceptions
;       * Concurrency
;       * STM
;   * Missing let-no-escape
;   * Missing IND (for heap indirections after thunk evaluation)

; Some other things that these semantics might want to capture
;   * Selector thunks
;   * Pointer tagging
;   * Stack chunks / STACK_AP
;   * CAFs

; Useful sanity checks:
;   * Formalize Core (with typing), define translation to STG, test for
;     progress given that Core type-checks

(define-language L
  ((x y z f g h) variable-not-otherwise-mentioned)
  (C variable-not-otherwise-mentioned)
  (n integer)
  (lit integer
       real)
  ((a v) x
         lit)
  (k • ; unknown arity
     n)
  (e a
     (f k a ...)
     (⊕ a ...) ; saturated
     (let (x obj) e)
     (case e alt ...)
     )
  (alt ((C x ...) e)
       ((x) e))
  (obj val
       (THUNK e)
       BLACKHOLE)
  (val (FUN (x ...) e)
       (PAP f a ...)
       (CON C a ...)) ; saturated
  )

(define-extended-language Ev L
  (p (e s H))
  (H ((x_!_ obj) ...))
  (κ (case • alt ...)
     (upd x •)
     (• a ...))
  (s (κ ...))
  )

; use the tutorial substitution
(require redex/tut-subst)
(define-metafunction Ev
  subst : (x v) ... e -> e
  [(subst (x v) ... e)
   ,(subst/proc x?
                (term (x ...))
                (term (v ...))
                (term e))])
(define x? (redex-match Ev x))

; We need to do some negative matches, a metafunction will be easiest
(define-metafunction Ev [(lit? e) ,(redex-match? Ev lit (term e))])
(define-metafunction
  Ev
  [(heapval? e H)
   ,(redex-match Ev
                  (x_i ((x_0 obj_0) ... (x_i val_i) (x_i+1 obj_i+1) ...))
                  (term (e H)))
   ])

; eval/apply reduction semantics
(define red
  (reduction-relation
   Ev
   #:domain p
   (--> ((let (x obj) e) s ((x_1 obj_1) ...))
        ((subst (x x_0) e) s ((x_0 obj) (x_1 obj_1) ...))
        (fresh x_0)
        "Let")
   (--> ((case x_i alt_0 ... ((C_i y_i ..._i_) e_i) alt_i+1 ...) s H)
        ((subst (y_i a_i) ... e_i) s H)
        (where ((x_0 obj_0) ... (x_i (CON C_i a_i ..._i_)) (x_i+1 obj_i+1) ...) H)
        "CaseCon")
   (--> ((case x_i alt ... ((x) e)) s H)
        ((subst (x x_i) e) s H)
        (where ((x_0 obj_0) ... (x_i val_i) (x_i+1 obj_i+1) ...) H)
        (side-condition ; this terri-bad side condition
         (not (redex-match ; NB: not redex-match? which is buggy
          Ev
          ((CON C_i a_i ...) alt_0 ... ((C_i y_i ...) e_i) alt_i+1 ...)
          (term (val_i alt ...)))))
        "CaseAnyHeap")
   (--> ((case lit alt ... ((x) e)) s H)
        ((subst (x lit) e) s H)
        "CaseAnyLit")
   ; I kind of like the original presentation, where we have an execution
   ; code that tells us whether or not we need to enter the scrutinee
   (--> ((case e alt ...) (κ ...) H)
        (e ((case • alt ...) κ ...) H)
        (where #f (lit? e))
        (where #f (heapval? e H))
        "Case")
   (--> (lit ((case • alt ...) κ ...) H)
        ((case lit alt ...) (κ ...) H)
        "RetLit")
   (--> (x_i ((case • alt ...) κ ...) H)
        ((case x_i alt ...) (κ ...) H)
        (where ((x_0 obj_0) ... (x_i val_i) (x_i+1 obj_i+1) ...) H)
        "Ret")
   (--> (x_i s
             ((x_0 obj_0) ... (x_i (THUNK e)) (x_i+1 obj_i+1) ...))
        (e ((upd x_i •) ,@(term s)) ; nifty idiom for splicing in
             ((x_0 obj_0) ... (x_i BLACKHOLE) (x_i+1 obj_i+1) ...))
        "Thunk")
   (--> (y_j ((upd x_i •) κ ...) H)
        (y_j (κ ...) ((x_0 obj_x0) ... (x_i val_j) (x_i+1 obj_i+1) ...))
        (where ((x_0 obj_x0) ... (x_i BLACKHOLE) (x_i+1 obj_i+1) ...) H)
        (where ((y_0 obj_y0) ... (y_j val_j) (y_j+1 obj_j+1) ...) H)
        "Update")
   (--> ((f_i n a ..._n_) s H)
        ((subst (x a) ... e) s H)
        (where ((f_0 obj_0) ... (f_i (FUN (x ..._n_) e)) (f_i+1 obj_i+1) ...) H)
        (side-condition (= (length (term (a ...))) (length (term (x ...))) (term n)))
        "KnownCall")
   ; Primop rule is omitted
   ; n.b. named ellipses do not carry over
   (--> ((f_i • a ...) s H)
        ((subst (x a) ... e) s H)
        (where ((f_0 obj_0) ... (f_i (FUN (x ...) e)) (f_i+1 obj_i+1) ...) H)
        (side-condition (= (length (term (a ...))) (length (term (x ...)))))
        "Exact")
   (--> ((f_i k a_1→n ..._1→n a_n+1→m ...)
         (κ ...)
         (name H ((f_0 obj_0) ... (f_i (FUN (x ..._1→n) e)) (f_i+1 obj_i+1) ...)))
        ((subst (x a_1→n) ... e) ((• a_n+1→m ...) κ ...) H)
        (side-condition (> (length (term (a_n+1→m ...))) 0))
        "CallK")
   (--> ((f_i k a ..._1→m)
         s
         (name H ((f_1 obj_1) ... (f_i (FUN (x_1→m ..._1→m x_m+1→n ...) e)) (f_i+1 obj_i+1) ...)))
        (f_0 s ((f_0 (PAP f_i a ...)) ,@(term H)))
        (fresh f_0)
        (side-condition (> (length (term (x_m+1→n ...))) 0))
        "PAP")
   (--> ((f_i • a ...) (κ ...) H)
        (f_i ((• a ...) κ ...) H)
        (where ((f_0 obj_0) ... (f_i (THUNK e)) (f_i+1 obj_i+1) ...) H)
        "TCall")
   (--> ((f_i k a_m ...) s H)
        ((g • a_n ... a_m ...) s H)
        (where ((f_0 obj_0) ... (f_i (PAP g a_n ...)) (f_i+1 obj_i+1) ...) H)
        "PCall")
   (--> (f_i ((• a ...) κ ...) H)
        ((f_i • a ...) (κ ...) H)
        ; technically CON should not be allowed, but we'll get stuck one step later
        (where ((f_0 obj_0) ... (f_i val) (f_i+1 obj_i+1) ...) H)
        "RetFun")
   ))

(define dH (term ((f (FUN (x y z) (⊕ x y z))) (g (THUNK f)) (h (PAP f 0)) (y (THUNK z)) (z (CON C_I 0)))))

; XXX these tests are pretty fragile

(define dHc (term ((f (FUN (x y z) (⊕ x y z))) (g (THUNK f)) (h (PAP f 0)) (y (CON C_I 0)) (z (CON C_I 0)))))

; Case
(test-->> red
          (term ((case z ((C_I x) (⊕ x 1)) ((x) x)) () ,dH))
          (term ((⊕ 0 1) () ,dH)))
(test-->> red
          (term ((case y ((C_I x) (⊕ x 1)) ((x) x)) () ,dH))
          (term ((⊕ 0 1) () ,dHc)))
(test-->> red
          (term ((case z ((C_J x) (⊕ x 1)) ((x) x)) () ,dH))
          (term (z () ,dH)))
(test-->> red
          (term ((case y ((C_J x) (⊕ x 1)) ((x) x)) () ,dH))
          (term (z () ,dHc)))
(test-->> red
          (term ((case 0 ((C_J x) (⊕ x 1)) ((x) x)) () ,dH))
          (term (0 () ,dH)))

; KnownCall/Exact/CallK/PAP
(test-->> red
          (term ((f • 0) () ,dH))
          (term (f_0 () ((f_0 (PAP f 0)) ,@dH))))
(test-->> red
          (term ((f 3 0) () ,dH))
          (term (f_0 () ((f_0 (PAP f 0)) ,@dH))))
(test-->> red
          (term ((f • 0 1 2 3) () ,dH))
          (term ((⊕ 0 1 2) ((• 3)) ,dH)))
(test-->> red
          (term ((f 3 0 1 2 3) () ,dH))
          (term ((⊕ 0 1 2) ((• 3)) ,dH)))
(test-->> red
          (term ((f • 0 1 2) () ,dH))
          (term ((⊕ 0 1 2) () ,dH)))
(test-->> red
          (term ((f 3 0 1 2) () ,dH))
          (term ((⊕ 0 1 2) () ,dH)))

; TCall/Thunk/Update
(define dHe (term ((f (FUN (x y z) (⊕ x y z))) (g (FUN (x y z) (⊕ x y z))) (h (PAP f 0)) (y (THUNK z)) (z (CON C_I 0)))))
(test-->> red
          (term ((g • 0) () ,dH))
          (term (f_0 () ((f_0 (PAP f 0)) ,@dHe))))
(test-->> red
          (term ((g • 0 1 2 3) () ,dH))
          (term ((⊕ 0 1 2) ((• 3)) ,dHe)))
(test-->> red
          (term ((g • 0 1 2) () ,dH))
          (term ((⊕ 0 1 2) () ,dHe)))

; PCall
(test-->> red
          (term ((h • 1) () ,dH))
          (term (f_0 () ((f_0 (PAP f 0 1)) ,@dH))))
(test-->> red
          (term ((h 2 1) () ,dH))
          (term (f_0 () ((f_0 (PAP f 0 1)) ,@dH))))
(test-->> red
          (term ((h • 1 2 3) () ,dH))
          (term ((⊕ 0 1 2) ((• 3)) ,dH)))
(test-->> red
          (term ((h 3 1 2 3) () ,dH))
          (term ((⊕ 0 1 2) ((• 3)) ,dH)))
(test-->> red
          (term ((h • 1 2) () ,dH))
          (term ((⊕ 0 1 2) () ,dH)))
(test-->> red
          (term ((h 3 1 2) () ,dH))
          (term ((⊕ 0 1 2) () ,dH)))