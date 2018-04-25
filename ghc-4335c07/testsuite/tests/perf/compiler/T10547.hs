-- Reported by sjcjoosten in T10547, this was taking forever because of a bug in
-- the implementation. See bottom of the file for some notes.

module Test where

type T12 = T11
type T11 = T10
type T10 = T9
type T9  = T8
type T8  = T7
type T7  = T6
type T6  = T5
type T5  = T4
type T4  = T3
type T3  = T2
type T2  = T1
type T1  = T0
type T0  = Int

type S12 = S11
type S11 = S10
type S10 = S9
type S9  = S8
type S8  = S7
type S7  = S6
type S6  = S5
type S5  = S4
type S4  = S3
type S3  = S2
type S2  = S1
type S1  = S0
type S0  = Int

test :: (T12, Char) -> (S12, Bool) -> Int
test a b = const 1 (f a b)

f :: (a, b) -> (a, b) -> (a, b)
f a _ = a

-- 5416fad, before the fix:
--
--    16,990,408,080 bytes allocated in the heap
--        49,762,144 bytes copied during GC
--         4,295,384 bytes maximum residency (5 sample(s))
--           186,272 bytes maximum slop
--                12 MB total memory in use (0 MB lost due to fragmentation)
--
--                                       Tot time (elapsed)  Avg pause  Max pause
--    Gen  0     26929 colls,     0 par    0.779s   0.779s     0.0000s    0.0009s
--    Gen  1         5 colls,     0 par    0.040s   0.040s     0.0080s    0.0099s
--
--    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)
--
--    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
--
--    INIT    time    0.001s  (  0.001s elapsed)
--    MUT     time    3.409s  (  3.409s elapsed)
--    GC      time    0.819s  (  0.819s elapsed)
--    EXIT    time    0.008s  (  0.012s elapsed)
--    Total   time    4.256s  (  4.240s elapsed)
--
--    Alloc rate    4,984,597,832 bytes per MUT second
--
--    Productivity  80.7% of total user, 81.1% of total elapsed
--
-- After the fix:
--
--        39,165,544 bytes allocated in the heap
--        19,516,400 bytes copied during GC
--         4,460,568 bytes maximum residency (5 sample(s))
--           244,640 bytes maximum slop
--                11 MB total memory in use (0 MB lost due to fragmentation)
--
--                                       Tot time (elapsed)  Avg pause  Max pause
--    Gen  0        44 colls,     0 par    0.009s   0.009s     0.0002s    0.0007s
--    Gen  1         5 colls,     0 par    0.040s   0.040s     0.0080s    0.0099s
--
--    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)
--
--    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
--
--    INIT    time    0.001s  (  0.001s elapsed)
--    MUT     time    0.009s  (  0.009s elapsed)
--    GC      time    0.049s  (  0.049s elapsed)
--    EXIT    time    0.008s  (  0.012s elapsed)
--    Total   time    0.096s  (  0.070s elapsed)
--
--    Alloc rate    4,570,081,011 bytes per MUT second
--
--    Productivity  48.2% of total user, 65.9% of total elapsed
