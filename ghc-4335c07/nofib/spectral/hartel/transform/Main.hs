module Main (main) -- transform
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

type 
    T_test_quad=([Char],[[Char]],[Char],[T_def]);
    f_prepare::[Char] -> T_test_quad;
    f_prepare a_input=
        let { 
            r_tokens=f_lex a_input;
            r_tree=(.) ((.) ((.) f_lab f_use) f_srt) f_parse (f_drop_pragma r_tokens);
            r_dir=f_concat (f_find_pragma "module" [] r_tokens);
            r_includes=[(++) "include \"" ((++) a_f ".m\"\n")|a_f<-f_find_pragma "include" [] r_tokens]
         } in  (r_dir,r_includes,a_input,r_tree);
    f_find_pragma::T_name -> [T_name] -> [T_token] -> [T_name];
    f_find_pragma a_x a_ps []=f_reverse a_ps;
    f_find_pragma a_x a_ps ((F_PRAG a_y):(F_STR a_z):a_ts)=
        if (((==) :: (Int -> Int -> Bool)) (f_cmp_name a_x a_y) c_cmp_equal)
        then (f_find_pragma a_x ((:) a_z a_ps) a_ts)
        else 
            (f_find_pragma a_x a_ps a_ts);
    f_find_pragma a_x a_ps (a_t:a_ts)=f_find_pragma a_x a_ps a_ts;
    f_drop_pragma::[T_token] -> [T_token];
    f_drop_pragma []=[];
    f_drop_pragma ((F_PRAG a_x):a_y:a_ts)=f_drop_pragma a_ts;
    f_drop_pragma (a_t:a_ts)=(:) a_t (f_drop_pragma a_ts);
    f_test::T_test_quad -> [Char];
    f_test a_quad=
        let { 
            r_v6=f_one_test "v6" "t6 v5" ((.) f_inl f_t6) r_v5;
            r_v5=f_one_test "v5" "t5 v3" ((.) f_srt f_t5) r_v3;
            r_v3=f_one_test "v3" "t3 v1" ((.) ((.) ((.) f_lab f_use) f_srt) f_t3) r_v1;
            r_v1=f_one_test "v1" "t1 v0" ((.) ((.) ((.) f_lab f_use) f_srt) f_t1) r_v0;
            r_v0=f_one_test "v0" "inl v" ((.) ((.) ((.) ((.) ((.) 
                ((.) f_lab f_use) f_srt) f_inl) f_inl) f_use) f_srt) r_v;
            r_v=f_one_test "v" "v" f_id a_quad
         } in  f_concat [a_text|(a_dir,a_includes,a_text,a_tree)<-(:) r_v ((:) r_v0 ((:) r_v1 ((:) r_v3 ((:) r_v5 
            ((:) r_v6 [])))))];
    f_one_test::[Char] -> [Char] -> (T_program -> T_program) -> T_test_quad -> T_test_quad;
    f_one_test a_nf a_tf a_f (a_dir,a_includes,a_text,a_tree)=
        let { 
            r_tree'=a_f a_tree;
            r_textm'=(++) "cat - <<'end' > " ((++) a_dir ((++) ".dir/" ((++) a_nf ((++) ".m\n" ((++) "|| " 
                ((++) a_dir ((++) ".dir " ((++) a_nf ((++) " = " ((++) a_tf ((++) "\n" ((++) "||\n" 
                ((++) (f_concat (f_map ((++) "%") a_includes)) ((++) "||\n" ((++) (f_prm r_tree') "\nend\n")))))))))))))))
         } in  (a_dir,a_includes,r_textm',r_tree');
    c_simple=f_prepare c_simpletext;
    c_simpletext=(++) "%module  \"simple\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "v      = map inc output;\n" "output = (:) 1 v;\n")));
    c_fib=f_prepare c_fibtext;
    c_fibtext=(++) "%module  \"fib\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "output = (:) 0 b;\n" ((++) "b      = (:) 1 c;\n" "c      = map (+) output b;\n"))));
    c_fibsum=f_prepare c_fibsumtext;
    c_fibsumtext=(++) "%module  \"fibsum\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "a      = (:) 0 b;\n" ((++) "b      = (:) 1 c;\n" ((++) "c      = map (+) a (tl a);\n" "output = maps (+) (+) 0 a;\n")))));
    c_tailtest=f_prepare c_tailtesttext;
    c_tailtesttext=(++) "%module  \"tailtest\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "b      = tl output;\n" ((++) "a      = (:) 2 b;\n" "output = maps (*) (+) 0 ((:) 1 a);\n"))));
    c_setreset=f_prepare c_setresettext;
    c_setresettext=(++) "%module  \"setreset\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "%include \"~pieter/mira/setreset/setreset\"\n" ((++) "one_of_three x y z = x;\n" ((++) "nand s xs ys       = maps one_of_three notand s xs ys;\n" 
        ((++) "notand s a b       = bnot (band a b);\n" ((++) "as     = nand 0 ds bs;\n" ((++) "bs     = nand 1 as cs;\n" "output = (:) 1 as;\n"))))))));
    c_flipflop=f_prepare c_flipfloptext;
    c_flipfloptext=(++) "%module  \"flipflop\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "%include \"~pieter/mira/setreset/setreset\"\n" ((++) "one_of_two x y     = x;\n" ((++) "two_of_two x y     = y;\n" 
        ((++) "one_of_three x y z = x;\n" ((++) "nand s xs ys       = maps one_of_three notand s xs ys;\n" ((++) "buffer s xs        = maps one_of_two two_of_two s xs;\n" ((++) "notand s a b       = bnot (band a b);\n" ((++) "a1     = buffer 0 data;\n" ((++) "a2     = buffer 0 clock;\n" ((++) "a3     = nand 0 a1 a1;\n" 
        ((++) "a6     = nand 0 (nand 0 a1 a2) a7;\n" ((++) "a7     = nand 1 a6 (nand 0 a3 a2);\n" ((++) "a8     = nand 0 a2 a2;\n" ((++) "a11    = nand 0 (nand 0 a6 a8) output;\n" ((++) "a12    = nand 1 a11 (nand 0 a8 a7);\n" "output = (:) 1 a12;\n")))))))))))))))));
    c_wave4=f_prepare c_wave4text;
    c_wave4text=(++) "%module  \"wave4\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "%include \"~pieter/mira/wave4/wave4\"\n" ((++) "one_of_two x y     = x;\n" ((++) "two_of_two x y     = y;\n" 
        ((++) "a1     = maps one_of_two two_of_two mf0 a7;\n" ((++) "a2     = maps one_of_two two_of_two mg0 a8;\n" ((++) "a3     = map first_borders a2    ;\n" ((++) "a4     = map fu            a1  a3;\n" ((++) "a5     = map gu            a2    ;\n" ((++) "a6     = map last_borders  a4    ;\n" ((++) "a7     = map fvh           a4    ;\n" 
        ((++) "a8     = map gvh           a5  a6;\n" ((++) "a9     = map join          a1  a2;\n" "output = (:) dummy_output a9;\n"))))))))))))));
    c_wavestream=f_prepare c_wavestreamtext;
    c_wavestreamtext=(++) "%module  \"wavestream\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"../synch/misc\"\n" ((++) "%include \"wavematrix\"\n" ((++) "%include \"wavelib\"\n" ((++) "ts1 = (:) tb1 hs1;\n" 
        ((++) "hs1 = map fh vs1 v'b1 ur1;\n" ((++) "vs1 = map fv us1 uo1 ho1 uor1 ur1;\n" ((++) "us1 = map fu ts1 vhl1 vbl1 vb1;\n" ((++) "ur1 = map getur us2;\n" ((++) "uo1 = map getuo us3;\n" ((++) "ho1 = map getho ts3;\n" ((++) "uor1 = map getuor uo2;\n" 
        ((++) "ts2 = (:) tb2 hs2;\n" ((++) "hs2 = map fh vs2 v'b2 ur2;\n" ((++) "vs2 = map fv us2 uo2 ho2 uor2 ur2;\n" ((++) "us2 = map fu ts2 vhl2 vbl2 vb2;\n" ((++) "uo2 = map getuo us4;\n" ((++) "ho2 = map getho ts4;\n" ((++) "vhl2 = map getvhl ts1;\n" 
        ((++) "ts3 = (:) tb3 hs3;\n" ((++) "hs3 = map fh vs3 v'b3 ur3;\n" ((++) "vs3 = map fv us3 uo3 ho3 uor3 ur3;\n" ((++) "us3 = map fu ts3 vhl3 vbl3 vb3;\n" ((++) "vb3 = map getvb ts1;\n" ((++) "v'b3 = map getvb vs1;\n" ((++) "ur3 = map getur us4;\n" 
        ((++) "ts4 = (:) tb4 hs4;\n" ((++) "hs4 = map fh vs4 v'b4 ur4;\n" ((++) "vs4 = map fv us4 uo4 ho4 uor4 ur4;\n" ((++) "us4 = map fu ts4 vhl4 vbl4 vb4;\n" ((++) "vb4 = map getvb ts2;\n" ((++) "v'b4 = map getvb vs2;\n" ((++) "vhl4 = map getvhl ts3;\n" "vbl4 = map getvbl vb3;\n")))))))))))))))))))))))))))))))));
    c_hamming=f_prepare c_hammingtext;
    c_hammingtext=(++) "%module  \"hamming\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "%include \"~pieter/mira/hamming/hamming\"\n" ((++) "output = (:) 1 (maps hamFo hamFs hams0 ham2s ham3s);\n" ((++) "ham2s  = map ((*) 2) output;\n" 
        ((++) "ham3s  = map ((*) 3) output;\n" ((++) "hamFo s a b        = (fst (hamstat s a b));\n" "hamFs s a b        = (snd (hamstat s a b));\n")))))));
    c_gamma=f_prepare c_gammatext;
    c_gammatext=(++) "%module  \"gamma\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "%include \"~pieter/mira/gamma/gamma\"\n" ((++) "mkpair a b         = (a,b)    ;\n" ((++) "perm_2 xs          = perm 2 xs;\n" 
        ((++) "map_divides xs     = map1 divides xs;\n" ((++) "p0     = (:) lownumbers p6   ;\n" ((++) "p1     = map perm_2      p0   ;\n" ((++) "p2     = map map_divides p1   ;\n" ((++) "p3     = map select      p2 p1;\n" ((++) "p4     = map (--)        p0 p3;\n" ((++) "p5     = map remove1     p3   ;\n" 
        ((++) "p6     = map (++)        p5 p4;\n" ((++) "p7     = map or          p2   ;\n" ((++) "p8     = map (~)         p7   ;\n" ((++) "p9     = map mkpair      p8 p0;\n" "output = (:) dummy_output  p9;\n"))))))))))))))));
    c_fft=f_prepare c_ffttext;
    c_ffttext=(++) "%module  \"fft\"\n" ((++) "%include \"~pieter/lib/fast2mira\"\n" ((++) "%include \"~pieter/mira/synch/misc\"\n" ((++) "%include \"~pieter/mira/fft/fftaux\"\n" ((++) "%include \"~pieter/mira/fft/fftnet\"\n" ((++) "ns     = (:) 0 (map incmod5 ns);\n" 
        ((++) "ms     = (:) (# netinput div 4) ms;\n" ((++) "ixs    = (:) (reorderindex (# netinput div 4)) ixs;\n" ((++) "as     = (:) xs0 (map fftstep0 ns ms ixs    as bs cs);\n" ((++) "bs     = (:) xs1 (map fftstep1 ns ms ixs cs bs as ds);\n" ((++) "cs     = (:) xs2 (map fftstep2 ns ms ixs bs cs ds as);\n" ((++) "ds     = (:) xs3 (map fftstep3 ns ms ixs    ds cs bs);\n" "output = map concat4 as bs cs ds;\n")))))))))));
    f_t1::T_program -> T_program;
    f_t1 a_prog=f_concat (f_map f_t2 a_prog);
    f_t2::T_def -> T_program;
    f_t2 (F_Caf (a_v:[]) (F_Tail a_p a_e))=
        let { 
            r_w=f_expr_test a_p a_e
         } in  (:) (F_Caf ((:) a_v []) (F_Tail a_p (F_Var r_w))) (f_t2 
            (F_Caf ((:) r_w []) a_e));
    f_t2 (F_Caf (a_v:[]) (F_Cons a_p a_s a_e))=
        let { 
            r_w=f_expr_test a_p a_e
         } in  (:) (F_Caf ((:) a_v []) (F_Cons a_p a_s (F_Var r_w))) (f_t2 
            (F_Caf ((:) r_w []) a_e));
    f_t2 (F_Caf (a_v:[]) (F_Map a_p a_f a_es))=
        let { 
            r_vs=[f_expr_test a_p a_e|(a_p,a_e)<-f_zip2 [a_p..((-) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_p (length a_es)) (1 :: Int)] a_es]
         } in  (:) (F_Caf ((:) a_v []) (F_Map a_p a_f (f_map F_Var r_vs))) (f_concat [
            f_t2 (F_Caf ((:) a_v []) a_e)|(a_v,a_e)<-f_zip2 r_vs a_es]);
    f_t2 (F_Caf (a_v:[]) (F_Maps a_p a_f a_g a_s a_es))=
        let { 
            r_vs=[f_expr_test a_p a_e|(a_p,a_e)<-f_zip2 [a_p..((-) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_p (length a_es)) (1 :: Int)] a_es]
         } in  (:) (F_Caf ((:) a_v []) (F_Maps a_p a_f a_g a_s (f_map F_Var r_vs))) (f_concat [
            f_t2 (F_Caf ((:) a_v []) a_e)|(a_v,a_e)<-f_zip2 r_vs a_es]);
    f_t2 (F_Caf (a_v:[]) (F_Var a_w))=
        if (((==) :: (Int -> Int -> Bool)) (f_cmp_var a_v a_w) c_cmp_equal)
        then []
        else 
            ((:) (F_Caf ((:) a_v []) (F_Var a_w)) []);
    f_t2 a_def=(:) a_def [];
    f_expr_test::Int -> T_expr -> T_name;
    f_expr_test a_p (F_Var a_v)=a_v;
    f_expr_test a_p a_e=(++) "xxa" (strict_show_i a_p);
    f_t3::T_program -> T_program;
    f_t3 a_prog=f_concat (f_map f_t4 a_prog);
    f_t4::T_def -> T_program;
    f_t4 (F_Caf (a_v:[]) (F_Maps a_p a_f a_g a_s a_xs))=
        let { 
            r_y=(++) "xxb" (strict_show_i a_p);
            r_z=(++) "xxc" (strict_show_i a_p)
         } in  (:) (F_Caf ((:) a_v []) (F_Map a_p a_f ((:) (F_Var r_y) a_xs))) 
            ((:) (F_Caf ((:) r_y []) (F_Cons a_p a_s (F_Var r_z))) ((:) (F_Caf 
            ((:) r_z []) (F_Map a_p a_g ((:) (F_Var r_y) a_xs))) []));
    f_t4 a_def=(:) a_def [];
    f_mapdef a_w a_ds' []=(False,(0 :: Int),"?",(:) (F_Lit "?") [],a_ds');
    f_mapdef a_w a_ds' ((F_Caf (a_v:[]) (F_Map a_k a_f a_xs)):a_ds)=
        if (((==) :: (Int -> Int -> Bool)) (f_cmp_var a_v a_w) c_cmp_equal)
        then (True,a_k,a_f,a_xs,(++) a_ds' a_ds)
        else 
            (f_mapdef a_w ((:) (F_Caf ((:) a_v []) (F_Map a_k a_f a_xs)) a_ds') a_ds);
    f_mapdef a_w a_ds' (a_d:a_ds)=f_mapdef a_w ((:) a_d a_ds') a_ds;
    f_consdef a_w a_ds' []=(False,(0 :: Int),F_Lit "?",F_Lit "?",a_ds');
    f_consdef a_w a_ds' ((F_Caf (a_v:[]) (F_Cons a_k a_s a_x)):a_ds)=
        if (((==) :: (Int -> Int -> Bool)) (f_cmp_var a_v a_w) c_cmp_equal)
        then (True,a_k,a_s,a_x,(++) a_ds' a_ds)
        else 
            (f_consdef a_w ((:) (F_Caf ((:) a_v []) (F_Cons a_k a_s a_x)) a_ds') a_ds);
    f_consdef a_w a_ds' (a_d:a_ds)=f_consdef a_w ((:) a_d a_ds') a_ds;
    f_t5 a_prg=f_t5a [] a_prg [] (1 :: Int);
    f_t5a a_ds' [] a_r a_n=f_t5b [] a_ds' a_r a_n;
    f_t5a a_ds' ((F_Caf ("output":[]) (F_Map a_k a_f a_xs)):a_ds) a_r a_n=
        let { 
            r_new=(:) (F_Caf ((:) "output" []) (F_Cons a_k (F_Ap a_f [F_Ap "hd" 
                ((:) a_x [])|a_x<-a_xs]) (F_Var r_v))) ((:) (F_Caf ((:) r_v []) (F_Tail a_k (F_Var r_w))) 
                ((:) (F_Caf ((:) r_w []) (F_Map a_k a_f a_xs)) ((++) a_ds' a_ds)));
            r_v=(++) "xxv" (strict_show_i a_n);
            r_w=(++) "xxw" (strict_show_i (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)))
         } in  f_t5x "a" r_new a_r (((+) :: (Int -> Int -> Int)) a_n (2 :: Int));
    f_t5a a_ds' (a_other:a_ds) a_r a_n=f_t5a ((:) a_other a_ds') a_ds a_r a_n;
    f_t5b a_ds' [] a_r a_n=f_t5c [] a_ds' a_r a_n;
    f_t5b a_ds' ((F_Caf ("output":[]) (F_Tail a_k (F_Var a_w))):a_ds) a_r a_n=
        let { 
            (r_wok,r_j,r_s,r_x,r_all)=f_consdef a_w [] ((++) a_ds' a_ds);
            r_new=f_sub ((:) (F_Caf ((:) a_w []) (F_Cons r_j r_s (F_Var "output"))) r_all) 
                ((:) ("output",r_x) [])
         } in  
            if r_wok
            then (f_t5x "b" r_new a_r a_n)
            else 
                (f_t5b ((:) (F_Caf ((:) "output" []) (F_Tail a_k (F_Var a_w))) a_ds') a_ds a_r a_n);
    f_t5b a_ds' (a_other:a_ds) a_r a_n=f_t5b ((:) a_other a_ds') a_ds a_r a_n;
    f_t5c a_ds' [] a_r a_n=f_t5d [] a_ds' a_r a_n;
    f_t5c a_ds' ((F_Caf (a_v:[]) (F_Tail a_k (F_Var a_w))):a_ds) a_r a_n=
        let { 
            (r_wok,r_j,r_s,r_x,r_all)=f_consdef a_w [] ((++) a_ds' a_ds);
            r_new=f_sub ((:) (F_Caf ((:) a_w []) (F_Cons r_j r_s r_x)) r_all) ((:) 
                (a_v,r_x) [])
         } in  
            if r_wok
            then (f_t5x "c" r_new a_r a_n)
            else 
                (f_t5c ((:) (F_Caf ((:) a_v []) (F_Tail a_k (F_Var a_w))) a_ds') a_ds a_r a_n);
    f_t5c a_ds' (a_other:a_ds) a_r a_n=f_t5c ((:) a_other a_ds') a_ds a_r a_n;
    f_t5d a_ds' [] a_r a_n=f_t5e [] a_ds' a_r a_n;
    f_t5d a_ds' ((F_Caf (a_v:[]) (F_Tail a_k (F_Var a_w))):a_ds) a_r a_n=
        let { 
            (r_wok,r_j,r_f,r_xs,r_all)=f_mapdef a_w [] ((++) a_ds' a_ds);
            r_new=(:) (F_Caf ((:) a_w []) (F_Cons r_j (F_Ap r_f [F_Ap "hd" 
                ((:) a_x [])|a_x<-r_xs]) (F_Var a_v))) ((:) (F_Caf ((:) a_v []) (F_Map r_j r_f [F_Var a_y|a_y<-r_ys])) 
                ((++) [F_Caf ((:) a_y []) (F_Tail a_k a_x)|(a_y,a_x)<-f_zip2 r_ys r_xs] r_all));
            r_m=((-) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_n (length r_xs)) (1 :: Int);
            r_ys=[(++) "xxy" (strict_show_i a_i)|a_i<-[a_n..r_m]]
         } in  
            if (
                if r_wok
                then (f_notin (r_j,a_k) a_r)
                else 
                    False)
            then (f_t5x "d" r_new ((:) (r_j,a_k) a_r) (((+) :: (Int -> Int -> Int)) r_m (1 :: Int)))
            else 
                (f_t5d ((:) (F_Caf ((:) a_v []) (F_Tail a_k (F_Var a_w))) a_ds') a_ds a_r a_n);
    f_t5d a_ds' (a_other:a_ds) a_r a_n=f_t5d ((:) a_other a_ds') a_ds a_r a_n;
    f_t5e a_ds' a_ds a_r a_n=f_t5x "e" ((++) a_ds' a_ds) a_r a_n;
    f_t5x "e" a_all a_r a_n=a_all;
    f_t5x a_step a_all a_r a_n=f_t5a [] a_all a_r a_n;
    f_notin (a_a,a_b) []=True;
    f_notin (a_a,a_b) ((a_x,a_y):a_xys)=
        if (
            if (((==) :: (Int -> Int -> Bool)) a_a a_x)
            then (((==) :: (Int -> Int -> Bool)) a_b a_y)
            else 
                False)
        then False
        else 
            (f_notin (a_a,a_b) a_xys);
    f_t6::T_program -> T_program;
    f_t6 a_prog=
        let { 
            r_sel_k=(++) "sel_" (strict_show_i (length r_s1_k));
            r_id_h'=(++) "id_" (strict_show_i (((+) :: (Int -> Int -> Int)) (length r_u1_h) (1 :: Int)));
            r_v1_k=[a_v|(F_Caf (a_v:[]) (F_Cons a_p a_s a_x))<-r_conses];
            r_s1_k=[a_s|(F_Caf (a_v:[]) (F_Cons a_p a_s a_x))<-r_conses];
            r_x1_k=[a_x|(F_Caf (a_v:[]) (F_Cons a_p a_s a_x))<-r_conses];
            r_u1_h=f_uniqsort_names ((++) (f_free ((++) r_conses ((++) r_tails r_maps))) (f_bound r_tails));
            (r_conses,r_tails,r_maps,r_rest)=f_conses_tails_maps_rest (f_use a_prog)
         } in  (++) ((:) (F_Caf ((:) "output1" []) (F_Map (1 :: Int) r_sel_k ((:) 
            (F_Maps (1 :: Int) r_id_h' "nextstate" (F_Tuple r_s1_k) (f_map F_Var r_u1_h)) []))) []) ((++) ((:) (F_Def "nextstate" ((:) "s" r_u1_h) 
            (F_Tuple r_x1_k) (F_Where ((++) ((:) (F_Caf r_v1_k (F_Var "s")) []) [F_Caf 
            ((:) a_w []) (F_Ap a_f a_ys)|(F_Caf (a_w:[]) (F_Map a_p a_f a_ys))<-r_maps]))) []) a_prog);
    f_conses_tails_maps_rest::T_program -> (T_program,T_program,T_program,T_program);
    f_conses_tails_maps_rest a_prog=
        let { 
            r_conses=[F_Caf ((:) a_v []) (F_Cons a_p a_s a_e)|(F_Caf (a_v:[]) (F_Cons a_p a_s a_e))<-a_prog];
            r_tails=[F_Caf ((:) a_v []) (F_Tail a_p a_e)|(F_Caf (a_v:[]) (F_Tail a_p a_e))<-a_prog];
            r_maps=[F_Caf ((:) a_v []) (F_Map a_p a_f a_es)|(F_Caf (a_v:[]) (F_Map a_p a_f a_es))<-a_prog]
         } in  (r_conses,r_tails,r_maps,f_remove_defs (f_remove_defs (f_remove_defs a_prog r_conses) r_tails) r_maps);
    c_programs::[T_test_quad];
    c_programs=(:) c_fft ((:) c_fib ((:) c_fibsum ((:) c_flipflop ((:) c_gamma ((:) c_hamming 
        ((:) c_setreset ((:) c_wave4 c_programs)))))));
    f_benchmark_main::Int -> [Char];
    f_benchmark_main a_n=(++) (strict_show_i (f_sum (f_map ((.) length f_test) (f_take a_n c_programs)))) "\n";
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let { 
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
type 
    T_pattern t1 t2=[t2] -> [(t1,[t2])];
    f_patenum a_p a_xs=a_p a_xs;
    f_patlit a_eq a_x []=[];
    f_patlit a_eq a_x (a_x':a_xs)=
        if (a_eq a_x a_x')
        then ((:) (a_x,a_xs) [])
        else 
            [];
    f_patempty a_v a_xs=(:) (a_v,a_xs) [];
    f_patfail a_xs=[];
    f_patmatch a_p a_f []=[];
    f_patmatch a_p a_f (a_x:a_xs)=
        if (a_p a_x)
        then ((:) (a_f a_x,a_xs) [])
        else 
            [];
    f_patalt a_p a_q a_xs=(++) (a_p a_xs) (a_q a_xs);
    f_patalt' a_p a_q=(.) (f_take (1 :: Int)) (f_patalt a_p a_q);
    f_patseq a_f a_p a_q a_xs=[(a_f a_v1 a_v2,a_xs2)|(a_v1,a_xs1)<-a_p a_xs,(a_v2,a_xs2)<-a_q a_xs1];
    f_patrep a_p=f_patalt' (f_patseq (:) a_p (f_patrep a_p)) (f_patempty []);
    f_patrep1 a_p=f_patseq (:) a_p (f_patrep a_p);
    f_patalts a_ps=f_foldr f_patalt' f_patfail a_ps;
    f_patseqs a_ps=f_foldr (f_patseq (:)) (f_patempty []) a_ps;
    f_patlits a_eq a_xs=f_patseqs [f_patlit a_eq a_x|a_x<-a_xs];
    f_patapp a_f a_p a_xs=[(a_f a_v,a_xs')|(a_v,a_xs')<-a_p a_xs];
data 
    T_token=F_NUMB [Char] | F_NAME [Char] | F_SYMB [Char] | F_STR [Char] | F_PRAG [Char] | F_ERR [Char] | C_LAYOUT;
    f_token_ord (F_NUMB a_xs)=(1 :: Int);
    f_token_ord (F_NAME a_xs)=(2 :: Int);
    f_token_ord (F_SYMB a_xs)=(3 :: Int);
    f_token_ord (F_STR a_xs)=(4 :: Int);
    f_token_ord (F_PRAG a_xs)=(5 :: Int);
    f_token_ord (F_ERR a_xs)=(6 :: Int);
    f_token_ord C_LAYOUT=(7 :: Int);
    f_token_cmp a_x a_y=
        let { 
            r_ord=((-) :: (Int -> Int -> Int)) (f_token_ord a_x) (f_token_ord a_y)
         } in  
            if (((==) :: (Int -> Int -> Bool)) r_ord c_cmp_equal)
            then (f_token_cmp' a_x a_y)
            else 
                r_ord;
    f_token_cmp' (F_NUMB a_xs) (F_NUMB a_ys)=f_cmp_string a_xs a_ys;
    f_token_cmp' (F_NAME a_xs) (F_NAME a_ys)=f_cmp_string a_xs a_ys;
    f_token_cmp' (F_SYMB a_xs) (F_SYMB a_ys)=f_cmp_string a_xs a_ys;
    f_token_cmp' (F_STR a_xs) (F_STR a_ys)=f_cmp_string a_xs a_ys;
    f_token_cmp' (F_PRAG a_xs) (F_PRAG a_ys)=f_cmp_string a_xs a_ys;
    f_token_cmp' (F_ERR a_xs) (F_ERR a_ys)=f_cmp_string a_xs a_ys;
    f_token_cmp' C_LAYOUT C_LAYOUT=c_cmp_equal;
    f_lex::[Char] -> [T_token];
    f_lex []=[];
    f_lex (' ':a_cs)=f_lex a_cs;
    f_lex ('\o011':a_cs)=f_lex a_cs;
    f_lex ('\o012':a_cs)=f_lex a_cs;
    f_lex ('\o014':a_cs)=f_lex a_cs;
    f_lex ('|':'|':a_cs)=f_lex (f_dropwhile (f_char_ne '\o012') a_cs);
    f_lex ('(':':':')':a_cs)=(:) (F_NAME "(:)") (f_lex a_cs);
    f_lex ('(':'+':'+':')':a_cs)=(:) (F_NAME "(++)") (f_lex a_cs);
    f_lex ('(':'+':')':a_cs)=(:) (F_NAME "(+)") (f_lex a_cs);
    f_lex ('(':'-':')':a_cs)=(:) (F_NAME "(-)") (f_lex a_cs);
    f_lex ('(':'-':'-':')':a_cs)=(:) (F_NAME "(--)") (f_lex a_cs);
    f_lex ('(':'*':')':a_cs)=(:) (F_NAME "(*)") (f_lex a_cs);
    f_lex ('(':'/':')':a_cs)=(:) (F_NAME "(/)") (f_lex a_cs);
    f_lex ('(':'~':')':a_cs)=(:) (F_NAME "(~)") (f_lex a_cs);
    f_lex ('(':a_cs)=(:) (F_SYMB "(") (f_lex a_cs);
    f_lex (')':a_cs)=(:) (F_SYMB ")") (f_lex a_cs);
    f_lex (',':a_cs)=(:) (F_SYMB ",") (f_lex a_cs);
    f_lex (';':a_cs)=(:) (F_SYMB ";") (f_lex a_cs);
    f_lex ('=':a_cs)=(:) (F_SYMB "=") (f_lex a_cs);
    f_lex ('#':a_cs)=(:) (F_NAME "#") (f_lex a_cs);
    f_lex ('\o042':a_cs)=
        let { 
            (r_s,r_r)=f_substring (f_char_ne '\o042') [] a_cs
         } in  (:) (F_STR r_s) (f_lex (tail r_r));
    f_lex ('%':a_cs)=
        let { 
            (r_n,r_r)=f_substring f_let_git' [] a_cs
         } in  (:) (F_PRAG r_n) (f_lex r_r);
    f_lex (a_c:a_cs)=
        let { 
            (r_ln,r_lr)=f_substring f_let_git' ((:) a_c []) a_cs;
            (r_dn,r_dr)=f_substring f_realdigit ((:) a_c []) a_cs;
            r_e=(++) "illegal char " ((:) a_c [])
         } in  
            if (f_letter a_c)
            then ((:) (F_NAME r_ln) (f_lex r_lr))
            else 
            if (f_digit a_c)
            then ((:) (F_NUMB r_dn) (f_lex r_dr))
            else 
                ((:) (F_ERR r_e) (f_lex a_cs));
    f_char_ne::Char -> Char -> Bool;
    f_char_ne a_x a_y=((/=) :: (Int -> Int -> Bool)) (f_cmp_c a_x a_y) c_cmp_equal;
    f_let_git',f_realdigit::Char -> Bool;
    f_let_git' a_c=
        if (
            if (f_letter a_c)
            then True
            else 
                (f_digit a_c))
        then True
        else 
            (f_member_cmp f_cmp_c ((:) '_' ((:) '\o047' [])) a_c);
    f_realdigit a_c=
        if (f_digit a_c)
        then True
        else 
            (f_member_cmp f_cmp_c ((:) '+' ((:) '-' ((:) 'e' ((:) '.' [])))) a_c);
    f_substring::(t1 -> Bool) -> [t1] -> [t1] -> ([t1],[t1]);
    f_substring a_pred a_as []=(f_reverse a_as,[]);
    f_substring a_pred a_as (a_c:a_cs)=
        if (a_pred a_c)
        then (f_substring a_pred ((:) a_c a_as) a_cs)
        else 
            (f_reverse a_as,(:) a_c a_cs);
    c_cmp_less::Int;
    c_cmp_less=((negate) :: (Int -> Int)) (1 :: Int);
    c_cmp_equal::Int;
    c_cmp_equal=(0 :: Int);
    c_cmp_greater::Int;
    c_cmp_greater=(1 :: Int);
    f_cmp_c::Char -> Char -> Int;
    f_cmp_c a_x a_y=((-) :: (Int -> Int -> Int)) (fromEnum a_x) (fromEnum a_y);
    f_cmp_string::[Char] -> [Char] -> Int;
    f_cmp_string a_xs a_ys=f_list_cmp f_cmp_c a_xs a_ys;
    f_cmp_i::Int -> Int -> Int;
    f_cmp_i a_x a_y=((-) :: (Int -> Int -> Int)) a_x a_y;
    f_cmp_d::Double -> Double -> Int;
    f_cmp_d a_x a_y=floor (entier (((-) :: (Double -> Double -> Double)) a_x a_y));
    f_max_cmp::(t1 -> t1 -> Int) -> [t1] -> t1;
    f_max_cmp a_cmp a_xs=f_foldl1 (f_max2_cmp a_cmp) a_xs;
    f_max2_cmp::(t1 -> t1 -> Int) -> t1 -> t1 -> t1;
    f_max2_cmp a_cmp a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then a_a
        else 
            a_b;
    f_member_cmp::(t1 -> t1 -> Int) -> [t1] -> t1 -> Bool;
    f_member_cmp a_cmp a_x a_a=f_or (f_map ((.) (flip ((==) :: (Int -> Int -> Bool)) c_cmp_equal) (a_cmp a_a)) a_x);
    f_min_cmp::(t1 -> t1 -> Int) -> [t1] -> t1;
    f_min_cmp a_cmp a_xs=f_foldl1 (f_min2_cmp a_cmp) a_xs;
    f_min2_cmp::(t1 -> t1 -> Int) -> t1 -> t1 -> t1;
    f_min2_cmp a_cmp a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then a_b
        else 
            a_a;
    f_mkset_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_mkset_cmp a_cmp []=[];
    f_mkset_cmp a_cmp (a_a:a_x)=(:) a_a (f_filter ((.) (flip ((/=) :: (Int -> Int -> Bool)) c_cmp_equal) (a_cmp a_a)) (f_mkset_cmp a_cmp a_x));
    f_uniqmerge_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_uniqmerge_cmp a_cmp [] a_y=a_y;
    f_uniqmerge_cmp a_cmp (a_a:a_x) []=(:) a_a a_x;
    f_uniqmerge_cmp a_cmp (a_a:a_x) (a_b:a_y)=
        let { 
            r_order=a_cmp a_a a_b
         } in  
            if (((<) :: (Int -> Int -> Bool)) r_order c_cmp_equal)
            then ((:) a_a (f_uniqmerge_cmp a_cmp a_x ((:) a_b a_y)))
            else 
            if (((==) :: (Int -> Int -> Bool)) r_order c_cmp_equal)
            then ((:) a_a (f_uniqmerge_cmp a_cmp a_x a_y))
            else 
                ((:) a_b (f_uniqmerge_cmp a_cmp ((:) a_a a_x) a_y));
    f_uniqsort_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_uniqsort_cmp a_cmp a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_uniqmerge_cmp a_cmp (f_uniqsort_cmp a_cmp (f_take r_n2 a_x)) (f_uniqsort_cmp a_cmp (f_drop r_n2 a_x)));
    f_merge_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_merge_cmp a_cmp [] a_y=a_y;
    f_merge_cmp a_cmp (a_a:a_x) []=(:) a_a a_x;
    f_merge_cmp a_cmp (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then ((:) a_a (f_merge_cmp a_cmp a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge_cmp a_cmp ((:) a_a a_x) a_y));
    f_sort_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_sort_cmp a_cmp a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge_cmp a_cmp (f_sort_cmp a_cmp (f_take r_n2 a_x)) (f_sort_cmp a_cmp (f_drop r_n2 a_x)));
    f_list_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> Int;
    f_list_cmp a_cmp [] []=c_cmp_equal;
    f_list_cmp a_cmp [] a_ys=c_cmp_less;
    f_list_cmp a_cmp a_xs []=c_cmp_greater;
    f_list_cmp a_cmp (a_x:a_xs) (a_y:a_ys)=
        let { 
            r_order=a_cmp a_x a_y
         } in  
            if (((==) :: (Int -> Int -> Bool)) r_order c_cmp_equal)
            then (f_list_cmp a_cmp a_xs a_ys)
            else 
                r_order;
    f_remove_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_remove_cmp a_cmp a_xs []=a_xs;
    f_remove_cmp a_cmp a_xs (a_y:a_ys)=
        let { 
            f_remove' a_y []=[];
            f_remove' a_y (a_x:a_xs)=
                if (((==) :: (Int -> Int -> Bool)) (a_cmp a_x a_y) c_cmp_equal)
                then a_xs
                else 
                    ((:) a_x (f_remove' a_y a_xs))
         } in  f_remove_cmp a_cmp (f_remove' a_y a_xs) a_ys;
    f_cmp_combine::[Int] -> Int;
    f_cmp_combine []=c_cmp_equal;
    f_cmp_combine (a_x:a_xs)=
        if (((==) :: (Int -> Int -> Bool)) a_x c_cmp_equal)
        then (f_cmp_combine a_xs)
        else 
            a_x;
type 
    T_program=[T_def];
type 
    T_lbl=Int;
type 
    T_name=[Char];
data 
    T_def=F_Caf [T_var] T_expr | F_Def T_fun [T_var] T_expr T_whereexpr;
data 
    T_whereexpr=F_Where [T_def] | C_NoWhere;
data 
    T_expr=F_Tail T_lbl T_expr | F_Cons T_lbl T_stat T_expr | F_Map T_lbl T_fun [T_expr] | F_Maps T_lbl T_fun T_fun T_stat [T_expr] | F_Ap T_fun [T_expr] | F_Tuple [T_expr] | F_Var T_var | F_Lit T_name;
type 
    T_var=T_name;
type 
    T_fun=T_name;
type 
    T_stat=T_expr;
    f_prm::T_program -> [Char];
    f_prm a_prog=f_pr_list f_prmdef "\n" a_prog;
    f_prmdef::T_def -> [Char];
    f_prmdef (F_Caf (a_v:[]) a_e)=(++) (f_prmvar a_v) ((++) "\t= " ((++) (f_prmexpr a_e) ";"));
    f_prmdef (F_Caf a_vs a_e)=(++) "(" ((++) (f_pr_list f_prmvar "," a_vs) ((++) ")\t= " ((++) (f_prmexpr a_e) ";")));
    f_prmdef (F_Def a_f a_vs a_e C_NoWhere)=(++) (f_prmfun a_f) ((++) " " ((++) (f_pr_list f_prmvar " " a_vs) ((++) "\t= " 
        ((++) (f_prmexpr a_e) ";"))));
    f_prmdef (F_Def a_f a_vs a_e (F_Where a_ews))=(++) (f_prmfun a_f) ((++) " " ((++) (f_pr_list f_prmvar " " a_vs) ((++) "\n= " 
        ((++) (f_prmexpr a_e) ((++) (f_indent ((++) "\nwhere\n" (f_prm a_ews))) ";")))));
    f_prmexpr::T_expr -> [Char];
    f_prmexpr (F_Tail a_p a_e)=(++) "(tl " ((++) (f_prmexpr a_e) ")");
    f_prmexpr (F_Cons a_p a_s a_e)=(++) "((:) " ((++) (f_prmstat a_s) ((++) " " ((++) (f_prmexpr a_e) ")")));
    f_prmexpr (F_Map a_p a_f a_es)=(++) "(map_" ((++) (strict_show_i (length a_es)) ((++) " " ((++) 
        (f_prmfun a_f) ((++) " " ((++) (f_prmexprlist a_es) ")")))));
    f_prmexpr (F_Maps a_p a_f a_g a_s a_es)=(++) "(maps_" ((++) (strict_show_i (length a_es)) ((++) " " ((++) 
        (f_prmfun a_f) ((++) " " ((++) (f_prmfun a_g) ((++) " " ((++) (f_prmstat a_s) 
        ((++) " " ((++) (f_prmexprlist a_es) ")")))))))));
    f_prmexpr (F_Ap a_f a_es)=(++) "(" ((++) (f_prmfun a_f) ((++) " " ((++) (f_prmexprlist a_es) ")")));
    f_prmexpr (F_Tuple [])="()";
    f_prmexpr (F_Tuple (a_e:[]))=f_prmexpr a_e;
    f_prmexpr (F_Tuple a_es)=(++) "(" ((++) (f_pr_list f_prmexpr "," a_es) ")");
    f_prmexpr (F_Var a_v)=a_v;
    f_prmexpr (F_Lit a_l)=a_l;
    f_prmvar::T_var -> [Char];
    f_prmfun::T_fun -> [Char];
    f_prmstat::T_stat -> [Char];
    f_prmvar a_v=a_v;
    f_prmfun a_f=a_f;
    f_prmstat a_e=f_prmexpr a_e;
    f_prmexprlist::[T_expr] -> [Char];
    f_prmexprlist a_es=f_pr_list f_prmexpr " " a_es;
    f_pr_list::(t1 -> [Char]) -> [Char] -> [t1] -> [Char];
    f_pr_list a_fun a_sep []=[];
    f_pr_list a_fun a_sep (a_x:a_xs)=(++) (a_fun a_x) (f_concat [(++) a_sep (a_fun a_x)|a_x<-a_xs]);
    f_indent::[Char] -> [Char];
    f_indent []=[];
    f_indent "\n"="\n";
    f_indent ('\o012':a_cs)=(:) '\o012' ((:) ' ' ((:) ' ' (f_indent a_cs)));
    f_indent (a_c:a_cs)=(:) a_c (f_indent a_cs);
type 
    T_lab_tuple t1=(t1,Int);
    f_lab::T_program -> T_program;
    f_lab a_prog=
        let { 
            (r_prog',r_q')=f_labdeflist a_prog (1 :: Int)
         } in  r_prog';
    f_labdef::T_def -> Int -> T_lab_tuple T_def;
    f_labdef (F_Caf a_vs a_e) a_q=
        let { 
            (r_e',r_q')=f_labexpr a_e a_q
         } in  (F_Caf a_vs r_e',r_q');
    f_labdef (F_Def a_f a_vs a_e a_ew) a_q=
        let { 
            (r_e',r_q')=f_labexpr a_e a_q;
            (r_ew',r_q'')=f_labwhere a_ew r_q'
         } in  (F_Def a_f a_vs r_e' r_ew',r_q'');
    f_labwhere::T_whereexpr -> Int -> T_lab_tuple T_whereexpr;
    f_labwhere C_NoWhere a_q=(C_NoWhere,a_q);
    f_labwhere (F_Where a_ews) a_q=
        let { 
            (r_ews',r_q')=f_labdeflist a_ews a_q
         } in  (F_Where r_ews',r_q');
    f_labexpr::T_expr -> Int -> T_lab_tuple T_expr;
    f_labexpr (F_Tail a_p a_e) a_q=
        let { 
            (r_e',r_q')=f_labexpr a_e (((+) :: (Int -> Int -> Int)) a_q (2 :: Int))
         } in  (F_Tail a_q r_e',r_q');
    f_labexpr (F_Cons a_p a_s a_e) a_q=
        let { 
            (r_e',r_q')=f_labexpr a_e (((+) :: (Int -> Int -> Int)) a_q (2 :: Int));
            (r_s',r_q'')=f_labstat a_s r_q'
         } in  (F_Cons a_q r_s' r_e',r_q'');
    f_labexpr (F_Map a_p a_f a_es) a_q=
        let { 
            (r_es',r_q')=f_labexprlist a_es (((+) :: (Int -> Int -> Int)) a_q (2 :: Int))
         } in  (F_Map a_q a_f r_es',r_q');
    f_labexpr (F_Maps a_p a_f a_g a_s a_es) a_q=
        let { 
            (r_es',r_q')=f_labexprlist a_es (((+) :: (Int -> Int -> Int)) a_q (2 :: Int));
            (r_s',r_q'')=f_labstat a_s r_q'
         } in  (F_Maps a_q a_f a_g r_s' r_es',r_q'');
    f_labexpr (F_Ap a_f a_es) a_q=
        let { 
            (r_es',r_q')=f_labexprlist a_es a_q
         } in  (F_Ap a_f r_es',r_q');
    f_labexpr (F_Tuple a_es) a_q=
        let { 
            (r_es',r_q')=f_labexprlist a_es a_q
         } in  (F_Tuple r_es',r_q');
    f_labexpr a_e a_q=(a_e,a_q);
    f_labstat::T_stat -> Int -> T_lab_tuple T_stat;
    f_labstat a_s a_q=f_labexpr a_s a_q;
    f_lablist::(t1 -> Int -> T_lab_tuple t1) -> [t1] -> [t1] -> Int -> T_lab_tuple [t1];
    f_lablist a_fun [] a_rs a_q=(f_reverse a_rs,a_q);
    f_lablist a_fun (a_e:a_es) a_rs a_q=
        let { 
            (r_e',r_q')=a_fun a_e a_q
         } in  f_lablist a_fun a_es ((:) r_e' a_rs) r_q';
    f_labdeflist::[T_def] -> Int -> T_lab_tuple [T_def];
    f_labexprlist::[T_expr] -> Int -> T_lab_tuple [T_expr];
    f_labdeflist a_ds a_q=f_lablist f_labdef a_ds [] a_q;
    f_labexprlist a_es a_q=f_lablist f_labexpr a_es [] a_q;
    f_inl::T_program -> T_program;
    f_inl a_prog=
        let { 
            r_r=[F_Def a_f a_vs a_e C_NoWhere|(F_Def a_f a_vs a_e C_NoWhere)<-a_prog]
         } in  [f_inldef a_d (f_remove_defs r_r ((:) a_d []))|a_d<-a_prog];
    f_inldef::T_def -> T_program -> T_def;
    f_inldef (F_Caf a_vs a_e) a_r=F_Caf a_vs (f_inlexpr a_e a_r);
    f_inldef (F_Def a_f a_vs a_e a_w) a_r=F_Def a_f a_vs (f_inlexpr a_e a_r) (f_inlwhere a_w a_r);
    f_inlwhere::T_whereexpr -> T_program -> T_whereexpr;
    f_inlwhere C_NoWhere a_r=C_NoWhere;
    f_inlwhere (F_Where a_ews) a_r=F_Where [f_inldef a_w a_r|a_w<-a_ews];
    f_inlexpr::T_expr -> T_program -> T_expr;
    f_inlexpr (F_Tail a_p a_e) a_r=F_Tail a_p (f_inlexpr a_e a_r);
    f_inlexpr (F_Cons a_p a_s a_e) a_r=F_Cons a_p (f_inlstat a_s a_r) (f_inlexpr a_e a_r);
    f_inlexpr (F_Map a_p a_f a_es) a_r=F_Map a_p a_f (f_inlexprlist a_es a_r);
    f_inlexpr (F_Maps a_p a_f a_g a_s a_es) a_r=F_Maps a_p a_f a_g (f_inlstat a_s a_r) (f_inlexprlist a_es a_r);
    f_inlexpr (F_Tuple a_es) a_r=F_Tuple (f_inlexprlist a_es a_r);
    f_inlexpr (F_Lit a_l) a_r=F_Lit a_l;
    f_inlexpr (F_Var a_v) a_r=F_Var a_v;
    f_inlexpr (F_Ap a_f a_es) a_r=
        let { 
            r_lookup=[(f_zip2 a_vs a_es,a_e)|(F_Def a_fr a_vs a_e C_NoWhere)<-a_r,
                if (((==) :: (Int -> Int -> Bool)) (f_cmp_fun a_fr a_f) c_cmp_equal)
                then (((==) :: (Int -> Int -> Bool)) (length a_vs) (length a_es))
                else 
                    False]
         } in  
            if (null r_lookup)
            then (F_Ap a_f (f_inlexprlist a_es a_r))
            else 
                (f_subexpr' r_lookup);
    f_subexpr'::[(T_sub_env,T_expr)] -> T_expr;
    f_subexpr' ((a_r',a_e'):a_rest)=f_subexpr a_e' a_r';
    f_inlstat::T_stat -> T_program -> T_stat;
    f_inlstat a_s a_r=f_inlexpr a_s a_r;
    f_inlexprlist::[T_expr] -> T_program -> [T_expr];
    f_inlexprlist a_es a_r=[f_inlexpr a_e a_r|a_e<-a_es];
    f_eql::T_program -> T_program;
    f_eql a_prog=
        let { 
            (r_reps,r_outs,r_rest)=f_replacements a_prog
         } in  
            if (null r_outs)
            then (f_sub (f_map f_eqldef r_rest) r_reps)
            else 
                (f_eql' r_outs r_reps r_rest);
    f_eql'::[(T_var,T_expr)] -> [(T_var,T_expr)] -> T_program -> T_program;
    f_eql' (("output",(F_Var a_new)):[]) a_reps a_rest=f_sub (f_map f_eqldef (f_replace_output a_new a_rest)) ((++) a_reps ((:) (a_new,
        F_Var "output") []));
    f_replace_output::T_var -> [T_def] -> [T_def];
    f_replace_output a_new []=[];
    f_replace_output a_new ((F_Caf (a_old:[]) a_e):a_ds)=
        if (((==) :: (Int -> Int -> Bool)) (f_cmp_var a_old a_new) c_cmp_equal)
        then ((:) (F_Caf ((:) "output" []) a_e) a_ds)
        else 
            ((:) (F_Caf ((:) a_old []) a_e) (f_replace_output a_new a_ds));
    f_eqldef::T_def -> T_def;
    f_eqldef (F_Caf a_vs a_e)=F_Caf a_vs a_e;
    f_eqldef (F_Def a_f a_vs a_e C_NoWhere)=F_Def a_f a_vs a_e C_NoWhere;
    f_eqldef (F_Def a_f a_vs a_e (F_Where a_ews))=
        let { 
            (r_reps,r_outs,r_rest)=f_replacements a_ews
         } in  F_Def a_f a_vs (f_subexpr a_e r_reps) (F_Where (f_sub r_rest r_reps));
    f_replacements::T_program -> ([(T_var,T_expr)],[(T_var,T_expr)],T_program);
    f_replacements a_defs=
        let { 
            r_eqls=[F_Caf ((:) a_x []) (F_Var a_y)|(F_Caf (a_x:[]) (F_Var a_y))<-a_defs,((/=) :: (Int -> Int -> Bool)) (f_cmp_var a_x "output") c_cmp_equal];
            r_outs=[F_Caf ((:) a_x []) (F_Var a_y)|(F_Caf (a_x:[]) (F_Var a_y))<-a_defs,((==) :: (Int -> Int -> Bool)) (f_cmp_var a_x "output") c_cmp_equal]
         } in  ([(a_x,a_e)|(F_Caf (a_x:[]) a_e)<-r_eqls],[(a_x,a_e)|(F_Caf (a_x:[]) a_e)<-r_outs],f_remove_defs (f_remove_defs a_defs r_eqls) r_outs);
    f_srt::T_program -> T_program;
    f_srt a_prog=
        let { 
            r_cafs=[F_Caf ((:) a_v []) a_e|(F_Caf (a_v:[]) a_e)<-a_prog];
            r_rest=f_remove_defs a_prog r_cafs
         } in  f_eql ((++) (f_cafsort r_cafs) (f_uniqsort_defs r_rest));
    f_cafsort::T_program -> T_program;
    f_cafsort []=[];
    f_cafsort ((F_Caf (a_v:[]) a_ev):a_ds)=
        let { 
            r_lss=[F_Caf ((:) a_w []) a_ew|(F_Caf (a_w:[]) a_ew)<-a_ds,((<) :: (Int -> Int -> Bool)) (f_cmp_expr a_ew a_ev) c_cmp_equal];
            r_grt=[F_Caf ((:) a_w []) a_ew|(F_Caf (a_w:[]) a_ew)<-a_ds,((>) :: (Int -> Int -> Bool)) (f_cmp_expr a_ew a_ev) c_cmp_equal];
            r_x'=head r_x'_xs;
            r_xs=tail r_x'_xs;
            r_x'_xs=f_uniqsort_names ((:) a_v [a_w|(F_Caf (a_w:[]) a_ew)<-a_ds,((==) :: (Int -> Int -> Bool)) (f_cmp_expr a_ew a_ev) c_cmp_equal])
         } in  (++) (f_cafsort r_lss) ((++) ((:) (F_Caf ((:) r_x' []) a_ev) [
            F_Caf ((:) a_x []) (F_Var r_x')|a_x<-r_xs]) (f_cafsort r_grt));
    f_use::T_program -> T_program;
    f_useprg::[T_name] -> T_program -> T_program;
    f_use a_prog=f_useprg ((:) "output" []) a_prog;
    f_useprg a_always a_prog=
        let { 
            r_used=(++) a_always (f_remove_names (f_uniqsort_names ((++) a_always (f_rhs_vars r_cafs))) a_always);
            r_cafs=[F_Caf ((:) a_v []) a_e|(F_Caf (a_v:[]) a_e)<-a_prog];
            r_rest=f_remove_defs a_prog r_cafs
         } in  f_map f_usedef ((++) [F_Caf ((:) a_v []) a_e|a_w<-r_used,(F_Caf (a_v:[]) a_e)<-a_prog,((==) :: (Int -> Int -> Bool)) (f_cmp_var a_v a_w) c_cmp_equal] r_rest);
    f_usedef::T_def -> T_def;
    f_usedef (F_Caf a_vs a_e)=F_Caf a_vs a_e;
    f_usedef (F_Def a_f a_vs a_e C_NoWhere)=F_Def a_f a_vs a_e C_NoWhere;
    f_usedef (F_Def a_f a_vs a_e (F_Where a_ews))=F_Def a_f a_vs a_e (F_Where (f_useprg [] a_ews));
type 
    T_sub_env=[(T_var,T_expr)];
    f_sub::T_program -> T_sub_env -> T_program;
    f_sub a_prog a_r=f_subdeflist a_prog a_r;
    f_subdef::T_def -> T_sub_env -> T_def;
    f_subdef (F_Caf a_vs a_e) a_r=F_Caf a_vs (f_subexpr a_e a_r);
    f_subdef (F_Def a_f a_vs a_e a_w) a_r=F_Def a_f a_vs (f_subexpr a_e a_r) (f_subwhere a_w a_r);
    f_subwhere::T_whereexpr -> T_sub_env -> T_whereexpr;
    f_subwhere C_NoWhere a_r=C_NoWhere;
    f_subwhere (F_Where a_ews) a_r=F_Where (f_subdeflist a_ews a_r);
    f_subexpr::T_expr -> T_sub_env -> T_expr;
    f_subexpr (F_Tail a_p a_e) a_r=F_Tail a_p (f_subexpr a_e a_r);
    f_subexpr (F_Cons a_p a_s a_e) a_r=F_Cons a_p (f_substat a_s a_r) (f_subexpr a_e a_r);
    f_subexpr (F_Map a_p a_f a_es) a_r=F_Map a_p a_f (f_subexprlist a_es a_r);
    f_subexpr (F_Maps a_p a_f a_g a_s a_es) a_r=F_Maps a_p a_f a_g (f_substat a_s a_r) (f_subexprlist a_es a_r);
    f_subexpr (F_Ap a_f a_es) a_r=F_Ap a_f (f_subexprlist a_es a_r);
    f_subexpr (F_Tuple a_es) a_r=F_Tuple (f_subexprlist a_es a_r);
    f_subexpr (F_Lit a_l) a_r=F_Lit a_l;
    f_subexpr (F_Var a_v) a_r=
        let { 
            r_lookup=[a_er|(a_w,a_er)<-a_r,((==) :: (Int -> Int -> Bool)) (f_cmp_var a_w a_v) c_cmp_equal]
         } in  
            if (null r_lookup)
            then (F_Var a_v)
            else 
                (head r_lookup);
    f_substat::T_stat -> T_sub_env -> T_stat;
    f_substat a_s a_r=f_subexpr a_s a_r;
    f_subexprlist::[T_expr] -> T_sub_env -> [T_expr];
    f_subexprlist a_es a_r=[f_subexpr a_e a_r|a_e<-a_es];
    f_subdeflist a_ds a_r=[f_subdef a_d a_r|a_d<-a_ds];
    f_free::T_program -> [T_name];
    f_rhs_vars a_prog=f_uniqsort_names (f_concat [f_vars a_e|(F_Caf a_v a_e)<-a_prog]);
    f_bound a_prog=f_uniqsort_names [a_v|(F_Caf (a_v:[]) a_e)<-a_prog];
    f_free a_prog=f_remove_names (f_rhs_vars a_prog) (f_bound a_prog);
    f_unused a_prog=f_remove_names (f_bound a_prog) (f_rhs_vars a_prog);
    f_remove_names::[T_name] -> [T_name] -> [T_name];
    f_remove_names a_xs a_ys=f_remove_cmp f_cmp_name a_xs a_ys;
    f_remove_defs::[T_def] -> [T_def] -> [T_def];
    f_remove_defs a_xs a_ys=f_remove_cmp f_cmp_def a_xs a_ys;
    f_uniqsort_names::[T_name] -> [T_name];
    f_uniqsort_names a_xs=f_uniqsort_cmp f_cmp_name a_xs;
    f_uniqsort_defs::[T_def] -> [T_def];
    f_uniqsort_defs a_xs=f_uniqsort_cmp f_cmp_def a_xs;
    f_vars::T_expr -> [T_name];
    f_vars (F_Tail a_p a_e)=f_vars a_e;
    f_vars (F_Cons a_p a_s a_e)=f_vars a_e;
    f_vars (F_Map a_p a_f a_es)=f_concat (f_map f_vars a_es);
    f_vars (F_Maps a_p a_f a_g a_s a_es)=f_concat (f_map f_vars a_es);
    f_vars (F_Ap a_f a_es)=f_concat (f_map f_vars a_es);
    f_vars (F_Tuple a_es)=f_concat (f_map f_vars a_es);
    f_vars (F_Var a_v)=(:) a_v [];
    f_vars (F_Lit a_n)=[];
    f_cmp_program::T_program -> T_program -> Int;
    f_cmp_program a_x a_y=f_list_cmp f_cmp_def a_x a_y;
    f_cmp_lbl::T_lbl -> T_lbl -> Int;
    f_cmp_lbl a_x a_y=f_cmp_i a_x a_y;
    f_cmp_name::T_name -> T_name -> Int;
    f_cmp_name a_x a_y=f_list_cmp f_cmp_c a_x a_y;
    f_cmp_name_list::[T_name] -> [T_name] -> Int;
    f_cmp_name_list a_x a_y=f_list_cmp f_cmp_name a_x a_y;
    f_cmp_def::T_def -> T_def -> Int;
    f_cmp_def (F_Caf a_vs a_e) (F_Caf a_vs' a_e')=f_cmp_combine ((:) (f_cmp_name_list a_vs a_vs') ((:) (f_cmp_expr a_e a_e') []));
    f_cmp_def (F_Caf a_vs a_e) (F_Def a_f a_vs' a_e' a_w)=c_cmp_less;
    f_cmp_def (F_Def a_f a_vs a_e a_w) (F_Def a_f' a_vs' a_e' a_w')=f_cmp_combine ((:) (f_cmp_name a_f a_f') ((:) (f_cmp_name_list a_vs a_vs') ((:) 
        (f_cmp_expr a_e a_e') ((:) (f_cmp_whereexpr a_w a_w') []))));
    f_cmp_def a_x a_y=c_cmp_greater;
    f_cmp_def_list::[T_def] -> [T_def] -> Int;
    f_cmp_def_list a_x a_y=f_list_cmp f_cmp_def a_x a_y;
    f_cmp_whereexpr::T_whereexpr -> T_whereexpr -> Int;
    f_cmp_whereexpr (F_Where a_ds) (F_Where a_ds')=f_cmp_def_list a_ds a_ds';
    f_cmp_whereexpr (F_Where a_ds) C_NoWhere=c_cmp_less;
    f_cmp_whereexpr C_NoWhere C_NoWhere=c_cmp_equal;
    f_cmp_whereexpr a_x a_y=c_cmp_greater;
    f_expr_ord::T_expr -> Int;
    f_expr_ord (F_Tail a_l a_e)=(1 :: Int);
    f_expr_ord (F_Cons a_l a_s a_es)=(2 :: Int);
    f_expr_ord (F_Map a_l a_f a_es)=(3 :: Int);
    f_expr_ord (F_Maps a_l a_f a_g a_s a_es)=(4 :: Int);
    f_expr_ord (F_Ap a_f a_es)=(5 :: Int);
    f_expr_ord (F_Tuple a_es)=(6 :: Int);
    f_expr_ord (F_Var a_v)=(7 :: Int);
    f_expr_ord (F_Lit a_n)=(8 :: Int);
    f_cmp_expr::T_expr -> T_expr -> Int;
    f_cmp_expr a_x a_y=
        let { 
            r_ord=((-) :: (Int -> Int -> Int)) (f_expr_ord a_x) (f_expr_ord a_y)
         } in  
            if (((==) :: (Int -> Int -> Bool)) r_ord c_cmp_equal)
            then (f_cmp_expr' a_x a_y)
            else 
                r_ord;
    f_cmp_expr'::T_expr -> T_expr -> Int;
    f_cmp_expr' (F_Tail a_l a_e) (F_Tail a_l' a_e')=f_cmp_combine ((:) (f_cmp_lbl a_l a_l') ((:) (f_cmp_expr a_e a_e') []));
    f_cmp_expr' (F_Cons a_l a_s a_es) (F_Cons a_l' a_s' a_es')=f_cmp_combine ((:) (f_cmp_lbl a_l a_l') ((:) (f_cmp_stat a_s a_s') ((:) 
        (f_cmp_expr a_es a_es') [])));
    f_cmp_expr' (F_Map a_l a_f a_es) (F_Map a_l' a_f' a_es')=f_cmp_combine ((:) (f_cmp_lbl a_l a_l') ((:) (f_cmp_fun a_f a_f') ((:) 
        (f_cmp_expr_list a_es a_es') [])));
    f_cmp_expr' (F_Maps a_l a_f a_g a_s a_es) (F_Maps a_l' a_f' a_g' a_s' a_es')=f_cmp_combine ((:) (f_cmp_lbl a_l a_l') ((:) (f_cmp_fun a_f a_f') ((:) 
        (f_cmp_fun a_g a_g') ((:) (f_cmp_stat a_s a_s') ((:) (f_cmp_expr_list a_es a_es') [])))));
    f_cmp_expr' (F_Ap a_f a_es) (F_Ap a_f' a_es')=f_cmp_combine ((:) (f_cmp_fun a_f a_f') ((:) (f_cmp_expr_list a_es a_es') []));
    f_cmp_expr' (F_Tuple a_es) (F_Tuple a_es')=f_cmp_expr_list a_es a_es';
    f_cmp_expr' (F_Var a_v) (F_Var a_v')=f_cmp_var a_v a_v';
    f_cmp_expr' (F_Lit a_n) (F_Lit a_n')=f_cmp_name a_n a_n';
    f_cmp_expr_list::[T_expr] -> [T_expr] -> Int;
    f_cmp_expr_list a_x a_y=f_list_cmp f_cmp_expr a_x a_y;
    f_cmp_var::T_var -> T_var -> Int;
    f_cmp_var a_x a_y=f_cmp_name a_x a_y;
    f_cmp_fun::T_fun -> T_fun -> Int;
    f_cmp_fun a_x a_y=f_cmp_name a_x a_y;
    f_cmp_stat::T_stat -> T_stat -> Int;
    f_cmp_stat a_x a_y=f_cmp_expr a_x a_y;
type 
    T_expr_pattern=T_pattern T_expr T_token;
type 
    T_def_pattern=T_pattern T_def T_token;
type 
    T_program_pattern=T_pattern T_program T_token;
    f_parse::[T_token] -> T_program;
    f_parse a_ts=
        let { 
            (r_ps,r_rs)=head (f_patenum c_p_program a_ts)
         } in  r_ps;
    c_p_program::T_program_pattern;
    c_p_program=f_patrep1 (f_patalts ((:) c_p_caf ((:) c_p_def [])));
    c_p_caf::T_def_pattern;
    c_p_def::T_def_pattern;
    c_p_caf=
        let { 
            f_caf ((F_Var a_v):a_eq:a_e:a_s:[])=F_Caf ((:) a_v []) a_e
         } in  f_patapp f_caf (f_patseqs ((:) c_p_ident ((:) c_p_eq ((:) c_p_expr ((:) c_p_semi [])))));
    c_p_def=
        let { 
            f_def ((F_Var a_f):(F_Tuple a_vs):a_eq:a_e:a_s:[])=F_Def a_f [a_v|(F_Var a_v)<-a_vs] a_e C_NoWhere
         } in  f_patapp f_def (f_patseqs ((:) c_p_ident ((:) c_p_ident_list ((:) c_p_eq ((:) c_p_expr 
            ((:) c_p_semi []))))));
    c_p_eq::T_expr_pattern;
    c_p_semi::T_expr_pattern;
    c_p_open::T_expr_pattern;
    c_p_close::T_expr_pattern;
    c_p_map::T_expr_pattern;
    c_p_maps::T_expr_pattern;
    c_p_expr::T_expr_pattern;
    c_p_simp_list::T_expr_pattern;
    c_p_ident_list::T_expr_pattern;
    c_p_cons_expr::T_expr_pattern;
    c_p_map_expr::T_expr_pattern;
    c_p_maps_expr::T_expr_pattern;
    c_p_ap_expr::T_expr_pattern;
    c_p_simp::T_expr_pattern;
    c_p_pack::T_expr_pattern;
    c_p_ident::T_expr_pattern;
    c_p_numb::T_expr_pattern;
    f_token_is::T_token -> T_token -> Bool;
    f_token_is a_x a_y=((==) :: (Int -> Int -> Bool)) (f_token_cmp a_x a_y) c_cmp_equal;
    c_p_close=f_patmatch (f_token_is (F_SYMB ")")) f_literal;
    c_p_comma=f_patmatch (f_token_is (F_SYMB ",")) f_literal;
    c_p_eq=f_patmatch (f_token_is (F_SYMB "=")) f_literal;
    c_p_cons=f_patmatch (f_token_is (F_NAME "(:)")) f_literal;
    c_p_tail=f_patmatch (f_token_is (F_NAME "tl")) f_literal;
    c_p_map=f_patmatch (f_token_is (F_NAME "map")) f_literal;
    c_p_maps=f_patmatch (f_token_is (F_NAME "maps")) f_literal;
    c_p_open=f_patmatch (f_token_is (F_SYMB "(")) f_literal;
    c_p_semi=f_patmatch (f_token_is (F_SYMB ";")) f_literal;
    c_p_expr=f_patalts ((:) c_p_tail_expr ((:) c_p_cons_expr ((:) c_p_map_expr ((:) c_p_maps_expr ((:) c_p_ap_expr 
        ((:) c_p_simp []))))));
    c_p_simp_list=
        let { 
            f_tuple a_es=F_Tuple a_es
         } in  f_patapp f_tuple (f_patrep1 c_p_simp);
    c_p_ident_list=
        let { 
            f_tuple a_is=F_Tuple a_is
         } in  f_patapp f_tuple (f_patrep1 c_p_ident);
    c_p_tail_expr=
        let { 
            f_tail (a_t:a_e:[])=F_Tail (1 :: Int) a_e
         } in  f_patapp f_tail (f_patseqs ((:) c_p_tail ((:) c_p_expr [])));
    c_p_cons_expr=
        let { 
            f_cns (a_c:a_s:a_e:[])=F_Cons (1 :: Int) a_s a_e
         } in  f_patapp f_cns (f_patseqs ((:) c_p_cons ((:) c_p_simp ((:) c_p_expr []))));
    c_p_map_expr=
        let { 
            f_map (a_m:(F_Var a_f):(F_Tuple a_es):[])=F_Map (1 :: Int) a_f a_es
         } in  f_patapp f_map (f_patseqs ((:) c_p_map ((:) c_p_ident ((:) c_p_simp_list []))));
    c_p_maps_expr=
        let { 
            f_maps (a_m:(F_Var a_f):(F_Var a_g):a_s:(F_Tuple a_es):[])=F_Maps (1 :: Int) a_f a_g a_s a_es
         } in  f_patapp f_maps (f_patseqs ((:) c_p_maps ((:) c_p_ident ((:) c_p_ident ((:) c_p_simp 
            ((:) c_p_simp_list []))))));
    c_p_ap_expr=
        let { 
            f_ap ((F_Var a_f):(F_Tuple a_es):[])=F_Ap a_f a_es
         } in  f_patapp f_ap (f_patseqs ((:) c_p_ident ((:) c_p_simp_list [])));
    c_p_simp=f_patalts ((:) c_p_ident ((:) c_p_numb ((:) c_p_pack [])));
    c_p_pack=f_patapp (flip (!!) (1 :: Int)) (f_patseqs ((:) c_p_open ((:) (f_patalts 
        ((:) c_p_tuple ((:) c_p_expr []))) ((:) c_p_close []))));
    c_p_tuple=
        let { 
            f_tuple a_ts a_t=F_Tuple ((++) a_ts ((:) a_t []))
         } in  f_patseq f_tuple c_p_tuple_comma c_p_expr;
    c_p_tuple_comma=
        let { 
            f_comma (a_e:a_c:[])=a_e
         } in  f_patrep1 (f_patapp f_comma (f_patseqs ((:) c_p_expr ((:) c_p_comma []))));
    c_p_ident=f_patmatch f_is_name f_variable;
    c_p_numb=f_patmatch f_is_numb f_literal;
    f_variable::T_token -> T_expr;
    f_literal::T_token -> T_expr;
    f_variable (F_NAME a_x)=F_Var a_x;
    f_variable (F_SYMB a_x)=F_Var a_x;
    f_literal (F_NUMB a_x)=F_Lit a_x;
    f_literal (F_NAME a_x)=F_Lit a_x;
    f_literal (F_SYMB a_x)=F_Lit a_x;
    f_is_numb::T_token -> Bool;
    f_is_name::T_token -> Bool;
    f_is_numb (F_NUMB a_n)=True;
    f_is_numb a_x=False;
    f_is_name (F_NAME a_n)=True;
    f_is_name a_x=False;
    f_abs::Double -> Double;
    f_abs a_x=
        if (((<=) :: (Double -> Double -> Bool)) a_x (0.00000 :: Double))
        then (((negate) :: (Double -> Double)) a_x)
        else 
            a_x;
    f_and::[Bool] -> Bool;
    f_and a_xs=f_foldr (&&) True a_xs;
    f_cjustify::Int -> [Char] -> [Char];
    f_cjustify a_n a_s=
        let { 
            r_margin=((-) :: (Int -> Int -> Int)) a_n (length a_s);
            r_lmargin=((quot) :: (Int -> Int -> Int)) r_margin (2 :: Int);
            r_rmargin=((-) :: (Int -> Int -> Int)) r_margin r_lmargin
         } in  (++) (f_spaces r_lmargin) ((++) a_s (f_spaces r_rmargin));
    f_concat::[[t1]] -> [t1];
    f_concat a_xs=f_foldr (++) [] a_xs;
    f_const::t1 -> t2 -> t1;
    f_const a_x a_y=a_x;
    f_digit::Char -> Bool;
    f_digit a_x=
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum '0') (fromEnum a_x))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_x) (fromEnum '9'))
        else 
            False;
    f_drop::Int -> [t1] -> [t1];
    f_drop 0 a_x=a_x;
    f_drop a_n (a_a:a_x)=f_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x;
    f_drop a_n a_x=[];
    f_dropwhile::(t1 -> Bool) -> [t1] -> [t1];
    f_dropwhile a_f []=[];
    f_dropwhile a_f (a_a:a_x)=
        if (a_f a_a)
        then (f_dropwhile a_f a_x)
        else 
            ((:) a_a a_x);
    c_e::Double;
    c_e=((exp) :: (Double -> Double)) (1.00000 :: Double);
    f_filter::(t1 -> Bool) -> [t1] -> [t1];
    f_filter a_f a_x=[a_a|a_a<-a_x,a_f a_a];
    f_foldl::(t1 -> t2 -> t1) -> t1 -> [t2] -> t1;
    f_foldl a_op a_r []=a_r;
    f_foldl a_op a_r (a_a:a_x)=
        let { 
            f_strict a_f a_x=miraseq a_x (a_f a_x)
         } in  f_foldl a_op (f_strict a_op a_r a_a) a_x;
    f_foldl1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldl1 a_op (a_a:a_x)=f_foldl a_op a_a a_x;
    f_foldr::(t1 -> t2 -> t2) -> t2 -> [t1] -> t2;
    f_foldr a_op a_r []=a_r;
    f_foldr a_op a_r (a_a:a_x)=a_op a_a (f_foldr a_op a_r a_x);
    f_foldr1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldr1 a_op (a_a:[])=a_a;
    f_foldr1 a_op (a_a:a_b:a_x)=a_op a_a (f_foldr1 a_op ((:) a_b a_x));
    f_fst::(t1,t2) -> t1;
    f_fst (a_a,a_b)=a_a;
    f_id::t1 -> t1;
    f_id a_x=a_x;
    f_index::[t1] -> [Int];
    f_index a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(:) a_n (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x)
         } in  f_f (0 :: Int) a_x;
    f_init::[t1] -> [t1];
    f_init (a_a:a_x)=
        if (null a_x)
        then []
        else 
            ((:) a_a (f_init a_x));
    f_iterate::(t1 -> t1) -> t1 -> [t1];
    f_iterate a_f a_x=(:) a_x (f_iterate a_f (a_f a_x));
    f_last::[t1] -> t1;
    f_last a_x=(!!) a_x (((-) :: (Int -> Int -> Int)) (length a_x) (1 :: Int));
    f_lay::[[Char]] -> [Char];
    f_lay []=[];
    f_lay (a_a:a_x)=(++) a_a ((++) "\n" (f_lay a_x));
    f_layn::[[Char]] -> [Char];
    f_layn a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(++) (f_rjustify (4 :: Int) (strict_show_i a_n)) ((++) ") " ((++) a_a ((++) "\n" 
                (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x))))
         } in  f_f (1 :: Int) a_x;
    f_letter::Char -> Bool;
    f_letter a_c=
        if (
            if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'a') (fromEnum a_c))
            then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'z'))
            else 
                False)
        then True
        else 
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'A') (fromEnum a_c))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'Z'))
        else 
            False;
    f_limit::[Double] -> Double;
    f_limit (a_a:a_b:a_x)=
        if (((==) :: (Double -> Double -> Bool)) a_a a_b)
        then a_a
        else 
            (f_limit ((:) a_b a_x));
    f_lines::[Char] -> [[Char]];
    f_lines []=[];
    f_lines (a_a:a_x)=
        let { 
            r_xs=
                if (pair a_x)
                then (f_lines a_x)
                else 
                    ((:) [] [])
         } in  
            if (((==) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum '\o012'))
            then ((:) [] (f_lines a_x))
            else 
                ((:) ((:) a_a (head r_xs)) (tail r_xs));
    f_ljustify::Int -> [Char] -> [Char];
    f_ljustify a_n a_s=(++) a_s (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s)));
    f_map::(t1 -> t2) -> [t1] -> [t2];
    f_map a_f a_x=[a_f a_a|a_a<-a_x];
    f_map2::(t1 -> t2 -> t3) -> [t1] -> [t2] -> [t3];
    f_map2 a_f a_x a_y=[a_f a_a a_b|(a_a,a_b)<-f_zip2 a_x a_y];
    f_max::[Int] -> Int;
    f_max a_xs=f_foldl1 f_max2 a_xs;
    f_max2::Int -> Int -> Int;
    f_max2 a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) a_a a_b)
        then a_a
        else 
            a_b;
    f_member::[Int] -> Int -> Bool;
    f_member a_x a_a=f_or (f_map (flip ((==) :: (Int -> Int -> Bool)) a_a) a_x);
    f_merge::[Int] -> [Int] -> [Int];
    f_merge [] a_y=a_y;
    f_merge (a_a:a_x) []=(:) a_a a_x;
    f_merge (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) a_a a_b)
        then ((:) a_a (f_merge a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge ((:) a_a a_x) a_y));
    f_min::[Int] -> Int;
    f_min a_xs=f_foldl1 f_min2 a_xs;
    f_min2::Int -> Int -> Int;
    f_min2 a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) a_a a_b)
        then a_b
        else 
            a_a;
    f_mkset::[Int] -> [Int];
    f_mkset []=[];
    f_mkset (a_a:a_x)=(:) a_a (f_filter (flip ((/=) :: (Int -> Int -> Bool)) a_a) (f_mkset a_x));
    f_or::[Bool] -> Bool;
    f_or a_xs=f_foldr (||) False a_xs;
    c_pi::Double;
    c_pi=((*) :: (Double -> Double -> Double)) (4.00000 :: Double) (((atan) :: (Double -> Double)) (1.00000 :: Double));
    f_postfix::t1 -> [t1] -> [t1];
    f_postfix a_a a_x=(++) a_x ((:) a_a []);
    f_product::[Int] -> Int;
    f_product a_xs=f_foldl ((*) :: (Int -> Int -> Int)) (1 :: Int) a_xs;
    f_rep::Int -> t1 -> [t1];
    f_rep a_n a_x=f_take a_n (f_repeat a_x);
    f_repeat::t1 -> [t1];
    f_repeat a_x=(:) a_x (f_repeat a_x);
    f_reverse::[t1] -> [t1];
    f_reverse a_xs=f_foldl (flip (:)) [] a_xs;
    f_rjustify::Int -> [Char] -> [Char];
    f_rjustify a_n a_s=(++) (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s))) a_s;
    f_scan::(t1 -> t2 -> t1) -> t1 -> [t2] -> [t1];
    f_scan a_op=
        let { 
            f_g a_r []=(:) a_r [];
            f_g a_r (a_a:a_x)=(:) a_r (f_g (a_op a_r a_a) a_x)
         } in  f_g;
    f_snd::(t1,t2) -> t2;
    f_snd (a_a,a_b)=a_b;
    f_sort::[Int] -> [Int];
    f_sort a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge (f_sort (f_take r_n2 a_x)) (f_sort (f_drop r_n2 a_x)));
    f_spaces::Int -> [Char];
    f_spaces a_n=f_rep a_n ' ';
    f_subtract::Int -> Int -> Int;
    f_subtract a_x a_y=((-) :: (Int -> Int -> Int)) a_y a_x;
    f_sum::[Int] -> Int;
    f_sum a_xs=f_foldl ((+) :: (Int -> Int -> Int)) (0 :: Int) a_xs;
data 
    T_sys_message=F_Stdout [Char] | F_Stderr [Char] | F_Tofile [Char] [Char] | F_Closefile [Char] | F_Appendfile [Char] | F_System [Char] | F_Exit Int;
    f_take::Int -> [t1] -> [t1];
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_takewhile::(t1 -> Bool) -> [t1] -> [t1];
    f_takewhile a_f []=[];
    f_takewhile a_f (a_a:a_x)=
        if (a_f a_a)
        then ((:) a_a (f_takewhile a_f a_x))
        else 
            [];
    f_transpose::[[t1]] -> [[t1]];
    f_transpose a_x=
        let { 
            r_x'=f_takewhile pair a_x
         } in  
            if (null r_x')
            then []
            else 
                ((:) (f_map head r_x') (f_transpose (f_map tail r_x')));
    f_until::(t1 -> Bool) -> (t1 -> t1) -> t1 -> t1;
    f_until a_f a_g a_x=
        if (a_f a_x)
        then a_x
        else 
            (f_until a_f a_g (a_g a_x));
    f_zip2::[t1] -> [t2] -> [(t1,t2)];
    f_zip2 (a_a:a_x) (a_b:a_y)=(:) (a_a,a_b) (f_zip2 a_x a_y);
    f_zip2 a_x a_y=[];
    f_zip3 (a_a:a_x) (a_b:a_y) (a_c:a_z)=(:) (a_a,a_b,a_c) (f_zip3 a_x a_y a_z);
    f_zip3 a_x a_y a_z=[];
    f_zip4 (a_a:a_w) (a_b:a_x) (a_c:a_y) (a_d:a_z)=(:) (a_a,a_b,a_c,a_d) (f_zip4 a_w a_x a_y a_z);
    f_zip4 a_w a_x a_y a_z=[];
    f_zip5 (a_a:a_v) (a_b:a_w) (a_c:a_x) (a_d:a_y) (a_e:a_z)=(:) (a_a,a_b,a_c,a_d,a_e) (f_zip5 a_v a_w a_x a_y a_z);
    f_zip5 a_v a_w a_x a_y a_z=[];
    f_zip6 (a_a:a_u) (a_b:a_v) (a_c:a_w) (a_d:a_x) (a_e:a_y) (a_f:a_z)=(:) (a_a,a_b,a_c,a_d,a_e,a_f) (f_zip6 a_u a_v a_w a_x a_y a_z);
    f_zip6 a_u a_v a_w a_x a_y a_z=[];
    f_zip::([t1],[t2]) -> [(t1,t2)];
    f_zip (a_x,a_y)=f_zip2 a_x a_y;
    f_main a_x=f_benchmark_main a_x;
    main = do (n:_) <- getArgs; putStr (f_main (read n :: Int))
}
