module Main (main) -- sched
where {
    import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

data 
    T_jobdef=C_EMPTYJOBDEF | F_JOBDEF Int Int Int T_jobdef T_jobdef;
    f_sel_1_JOBDEF (F_JOBDEF a_int_1 a_int_2 a_int_3 a_jobdef_4 a_jobdef_5)=a_int_1;
    f_sel_2_JOBDEF (F_JOBDEF a_int_1 a_int_2 a_int_3 a_jobdef_4 a_jobdef_5)=a_int_2;
    f_sel_3_JOBDEF (F_JOBDEF a_int_1 a_int_2 a_int_3 a_jobdef_4 a_jobdef_5)=a_int_3;
    f_sel_4_JOBDEF (F_JOBDEF a_int_1 a_int_2 a_int_3 a_jobdef_4 a_jobdef_5)=a_jobdef_4;
    f_sel_5_JOBDEF (F_JOBDEF a_int_1 a_int_2 a_int_3 a_jobdef_4 a_jobdef_5)=a_jobdef_5;
    f_isEMPTYJOBDEF::T_jobdef -> Bool;
    f_isEMPTYJOBDEF C_EMPTYJOBDEF=True;
    f_isEMPTYJOBDEF a_any=False;
data 
    T_jobstat=C_EMPTYJOBSTAT | F_JOBSTAT Int Int Int Int T_jobdef;
    f_sel_1_JOBSTAT (F_JOBSTAT a_int_1 a_int_2 a_int_3 a_int_4 a_jobdef_5)=a_int_1;
    f_sel_2_JOBSTAT (F_JOBSTAT a_int_1 a_int_2 a_int_3 a_int_4 a_jobdef_5)=a_int_2;
    f_sel_3_JOBSTAT (F_JOBSTAT a_int_1 a_int_2 a_int_3 a_int_4 a_jobdef_5)=a_int_3;
    f_sel_4_JOBSTAT (F_JOBSTAT a_int_1 a_int_2 a_int_3 a_int_4 a_jobdef_5)=a_int_4;
    f_sel_5_JOBSTAT (F_JOBSTAT a_int_1 a_int_2 a_int_3 a_int_4 a_jobdef_5)=a_jobdef_5;
    f_isEMPTYJOBSTAT::T_jobstat -> Bool;
    f_isEMPTYJOBSTAT C_EMPTYJOBSTAT=True;
    f_isEMPTYJOBSTAT a_any=False;
data 
    T_tree t1=F_LEAF Int | F_TREE (T_tree t1) (T_tree t1);
    f_sel_1_TREE (F_TREE a_tree_1 a_tree_2)=a_tree_1;
    f_sel_2_TREE (F_TREE a_tree_1 a_tree_2)=a_tree_2;
    f_sel_1_LEAF (F_LEAF a_int_1)=a_int_1;
    f_mktree a_t1 a_t2=F_TREE a_t1 a_t2;
    f_isLEAF::(T_tree t1) -> Bool;
    f_isLEAF (F_LEAF a_n)=True;
    f_isLEAF a_any=False;
data 
    T_proc=F_PROC Int T_jobstat;
    f_sel_1_PROC (F_PROC a_int_1 a_jobstat_2)=a_int_1;
    f_sel_2_PROC (F_PROC a_int_1 a_jobstat_2)=a_jobstat_2;
data 
    T_paar t1=F_PAAR t1 t1;
    f_sel_1_PAAR (F_PAAR a_arg_1 a_arg_2)=a_arg_1;
    f_sel_2_PAAR (F_PAAR a_arg_1 a_arg_2)=a_arg_2;
    c_solo=F_JOBDEF (1 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_trio=F_JOBDEF (1 :: Int) (10 :: Int) (20 :: Int) c_job31 c_job32;
    c_job31=F_JOBDEF (2 :: Int) (0 :: Int) (30 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job32=F_JOBDEF (3 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_septiem=F_JOBDEF (1 :: Int) (20 :: Int) (20 :: Int) c_job72 c_job73;
    c_job72=F_JOBDEF (2 :: Int) (19 :: Int) (10 :: Int) c_job74 c_job75;
    c_job73=F_JOBDEF (3 :: Int) (18 :: Int) (30 :: Int) c_job76 c_job77;
    c_job74=F_JOBDEF (4 :: Int) (0 :: Int) (38 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job75=F_JOBDEF (5 :: Int) (0 :: Int) (50 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job76=F_JOBDEF (6 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job77=F_JOBDEF (7 :: Int) (0 :: Int) (40 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_deciem=F_JOBDEF (1 :: Int) (20 :: Int) (20 :: Int) c_job1002 c_job1003;
    c_job1002=F_JOBDEF (2 :: Int) (19 :: Int) (10 :: Int) c_job1004 c_job1005;
    c_job1003=F_JOBDEF (3 :: Int) (18 :: Int) (30 :: Int) c_job1006 c_job1007;
    c_job1004=F_JOBDEF (4 :: Int) (17 :: Int) (15 :: Int) c_job1008 c_job1009;
    c_job1005=F_JOBDEF (5 :: Int) (0 :: Int) (50 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1006=F_JOBDEF (6 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1007=F_JOBDEF (7 :: Int) (0 :: Int) (40 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1008=F_JOBDEF (8 :: Int) (0 :: Int) (30 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1009=F_JOBDEF (9 :: Int) (0 :: Int) (60 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_undeciem=F_JOBDEF (1 :: Int) (20 :: Int) (20 :: Int) c_job1102 c_job1103;
    c_job1102=F_JOBDEF (2 :: Int) (19 :: Int) (10 :: Int) c_job1104 c_job1105;
    c_job1103=F_JOBDEF (3 :: Int) (18 :: Int) (30 :: Int) c_job1106 c_job1107;
    c_job1104=F_JOBDEF (4 :: Int) (17 :: Int) (15 :: Int) c_job1108 c_job1109;
    c_job1105=F_JOBDEF (5 :: Int) (25 :: Int) (12 :: Int) c_job1110 c_job1111;
    c_job1106=F_JOBDEF (6 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1107=F_JOBDEF (7 :: Int) (0 :: Int) (40 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1108=F_JOBDEF (8 :: Int) (0 :: Int) (20 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1109=F_JOBDEF (9 :: Int) (0 :: Int) (50 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1110=F_JOBDEF (10 :: Int) (0 :: Int) (30 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1111=F_JOBDEF (11 :: Int) (0 :: Int) (60 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_duodeciem=F_JOBDEF (1 :: Int) (20 :: Int) (20 :: Int) c_job1202 c_job1203;
    c_job1202=F_JOBDEF (2 :: Int) (19 :: Int) (10 :: Int) c_job1204 c_job1205;
    c_job1203=F_JOBDEF (3 :: Int) (18 :: Int) (30 :: Int) c_job1206 c_job1207;
    c_job1204=F_JOBDEF (4 :: Int) (17 :: Int) (15 :: Int) c_job1208 c_job1209;
    c_job1205=F_JOBDEF (5 :: Int) (25 :: Int) (12 :: Int) c_job1210 c_job1211;
    c_job1206=F_JOBDEF (6 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1207=F_JOBDEF (7 :: Int) (0 :: Int) (40 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1208=F_JOBDEF (8 :: Int) (0 :: Int) (30 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1209=F_JOBDEF (9 :: Int) (0 :: Int) (60 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1210=F_JOBDEF (10 :: Int) (0 :: Int) (30 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1211=F_JOBDEF (11 :: Int) (0 :: Int) (60 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_quadrideciem=F_JOBDEF (1 :: Int) (20 :: Int) (20 :: Int) c_job1402 c_job1403;
    c_job1402=F_JOBDEF (2 :: Int) (19 :: Int) (10 :: Int) c_job1412 c_job1413;
    c_job1403=F_JOBDEF (3 :: Int) (18 :: Int) (30 :: Int) c_job1404 c_job1405;
    c_job1404=F_JOBDEF (4 :: Int) (17 :: Int) (15 :: Int) c_job1407 c_job1408;
    c_job1405=F_JOBDEF (5 :: Int) (25 :: Int) (12 :: Int) c_job1410 c_job1411;
    c_job1406=F_JOBDEF (6 :: Int) (0 :: Int) (10 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1407=F_JOBDEF (7 :: Int) (0 :: Int) (40 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1408=F_JOBDEF (8 :: Int) (19 :: Int) (10 :: Int) c_job1406 c_job1409;
    c_job1409=F_JOBDEF (9 :: Int) (0 :: Int) (60 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1410=F_JOBDEF (10 :: Int) (0 :: Int) (30 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1411=F_JOBDEF (11 :: Int) (0 :: Int) (60 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1412=F_JOBDEF (12 :: Int) (0 :: Int) (28 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_job1413=F_JOBDEF (13 :: Int) (0 :: Int) (50 :: Int) C_EMPTYJOBDEF C_EMPTYJOBDEF;
    c_procs::[T_proc];
    c_procs=(:) (F_PROC (1 :: Int) C_EMPTYJOBSTAT) ((:) (F_PROC (2 :: Int) C_EMPTYJOBSTAT) []);
    f_sched::T_jobdef -> Int -> T_tree T_jobstat;
    f_sched a_root a_threshold=
        let { 
            r_rootadm=(:) (f_addch a_root (0 :: Int)) [];
            r_totcyc=(0 :: Int);
            r_level=(0 :: Int)
         } in  f_alloc a_threshold r_rootadm c_procs [] [] r_totcyc r_level;
    f_alloc::Int -> [T_jobstat] -> [T_proc] -> [T_jobstat] -> [T_proc] -> Int -> Int -> T_tree T_jobstat;
    f_alloc a_threshold a_jobnew [] a_jobold a_prold a_totcyc a_level=f_process a_threshold ((++) a_jobold a_jobnew) a_prold a_totcyc a_level;
    f_alloc a_threshold a_jobnew (a_pr:a_prnew) a_jobold a_prold a_totcyc a_level=
        let { 
            r_pid=f_sel_1_PROC a_pr;
            r_pjstat=f_sel_2_PROC a_pr;
            r_sPAAR_jobold1_jobnew1=f_anyjob a_jobnew a_jobold r_pid;
            r_jobold1=f_sel_1_PAAR r_sPAAR_jobold1_jobnew1;
            r_jobnew1=f_sel_2_PAAR r_sPAAR_jobold1_jobnew1;
            r_job1=head r_jobnew1;
            r_jobrest1=tail r_jobnew1;
            r_sPAAR_jobold2_jobnew2=f_anyjob r_jobrest1 ((:) r_job1 r_jobold1) r_pid;
            r_jobold2=f_sel_1_PAAR r_sPAAR_jobold2_jobnew2;
            r_jobnew2=f_sel_2_PAAR r_sPAAR_jobold2_jobnew2;
            r_nextlevel=((+) :: (Int -> Int -> Int)) a_level (1 :: Int);
            r_allocjob1=f_alloc a_threshold ((++) r_jobold1 r_jobrest1) a_prnew [] ((:) (F_PROC r_pid r_job1) a_prold) a_totcyc r_nextlevel;
            r_allocjob2=f_alloc a_threshold r_jobnew2 ((:) a_pr a_prnew) r_jobold2 a_prold a_totcyc r_nextlevel
         } in  
            if (null a_jobnew)
            then (f_alloc a_threshold a_jobold a_prnew [] ((:) a_pr a_prold) a_totcyc a_level)
            else 
            if (not (f_isEMPTYJOBSTAT r_pjstat))
            then (f_alloc a_threshold ((++) a_jobold a_jobnew) a_prnew [] ((:) a_pr a_prold) a_totcyc a_level)
            else 
            if (null r_jobnew1)
            then (f_alloc a_threshold ((++) a_jobold a_jobnew) a_prnew [] ((:) a_pr a_prold) a_totcyc a_level)
            else 
            if (null r_jobnew2)
            then (f_alloc a_threshold ((++) r_jobold1 r_jobrest1) a_prnew [] ((:) (F_PROC r_pid r_job1) a_prold) a_totcyc a_level)
            else 
            if (((>=) :: (Int -> Int -> Bool)) a_level a_threshold)
            then (F_TREE r_allocjob1 r_allocjob2)
            else 
                (f_sandwich'' f_mktree (F_SANDWARG r_allocjob1 (((*) :: (Int -> Int -> Int)) (2 :: Int) r_nextlevel)) (F_SANDWARG r_allocjob2 (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (2 :: Int) r_nextlevel) (1 :: Int))));
    f_anyjob::[T_jobstat] -> [T_jobstat] -> Int -> T_paar [T_jobstat];
    f_anyjob [] a_jobold a_pid=F_PAAR a_jobold [];
    f_anyjob (a_job:a_jobnew) a_jobold a_pid=
        let { 
            r_mark=f_sel_1_JOBSTAT a_job;
            r_proc=f_sel_3_JOBSTAT a_job
         } in  
            if (
                if (
                    if (f_isEMPTYJOBSTAT a_job)
                    then True
                    else 
                        (((==) :: (Int -> Int -> Bool)) r_mark (1 :: Int)))
                then True
                else 
                if (((==) :: (Int -> Int -> Bool)) r_mark (4 :: Int))
                then (
                    if (((==) :: (Int -> Int -> Bool)) a_pid r_proc)
                    then True
                    else 
                        (((==) :: (Int -> Int -> Bool)) r_proc (0 :: Int)))
                else 
                    False)
            then (F_PAAR a_jobold ((:) a_job a_jobnew))
            else 
                (f_anyjob a_jobnew ((:) a_job a_jobold) a_pid);
    f_process::Int -> [T_jobstat] -> [T_proc] -> Int -> Int -> T_tree T_jobstat;
    f_process a_threshold a_jobs a_procs a_totcyc a_level=
        let { 
            r_s=f_mincyc a_procs;
            r_sPAAR_prterm_prnew=f_perform a_procs r_s;
            r_prterm=f_sel_1_PAAR r_sPAAR_prterm_prnew;
            r_prnew=f_sel_2_PAAR r_sPAAR_prterm_prnew;
            r_jobnew=f_addjob r_prterm a_jobs
         } in  
            if (((==) :: (Int -> Int -> Bool)) r_s (0 :: Int))
            then (F_LEAF a_totcyc)
            else 
                (f_alloc a_threshold r_jobnew r_prnew [] [] (((+) :: (Int -> Int -> Int)) a_totcyc r_s) a_level);
    f_mincyc::[T_proc] -> Int;
    f_mincyc []=(0 :: Int);
    f_mincyc (a_proc:a_prest)=
        let { 
            r_jobst=f_sel_2_PROC a_proc;
            r_steps=f_sel_2_JOBSTAT r_jobst
         } in  
            if (f_isEMPTYJOBSTAT r_jobst)
            then (f_mincyc a_prest)
            else 
                (f_min0 r_steps (f_mincyc a_prest));
    f_min0::Int -> Int -> Int;
    f_min0 0 a_x=a_x;
    f_min0 a_x 0=a_x;
    f_min0 a_x a_y=
        if (((<) :: (Int -> Int -> Bool)) a_x a_y)
        then a_x
        else 
            a_y;
    f_addjob::[T_proc] -> [T_jobstat] -> [T_jobstat];
    f_addjob [] a_jobs=a_jobs;
    f_addjob ((F_PROC a_pid (F_JOBSTAT a_mark a_zero a_x a_parent a_jdef)):a_prest) a_jobs=
        let { 
            r_jid=f_sel_1_JOBDEF a_jdef;
            r_js=f_sel_3_JOBDEF a_jdef;
            r_ch1=f_sel_4_JOBDEF a_jdef;
            r_ch2=f_sel_5_JOBDEF a_jdef
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_mark (1 :: Int))
            then ((:) (F_JOBSTAT (2 :: Int) r_js a_pid a_parent a_jdef) ((:) (f_addch r_ch1 r_jid) ((:) (f_addch r_ch2 r_jid) 
                (f_addjob a_prest a_jobs))))
            else 
                (f_addjob a_prest (f_ackn a_parent a_jobs));
    f_perform::[T_proc] -> Int -> T_paar [T_proc];
    f_perform [] a_s=F_PAAR [] [];
    f_perform ((F_PROC a_pid a_jobst):a_prest) a_s=
        let { 
            r_mark=f_sel_1_JOBSTAT a_jobst;
            r_steps=f_sel_2_JOBSTAT a_jobst;
            r_pr=f_sel_3_JOBSTAT a_jobst;
            r_par=f_sel_4_JOBSTAT a_jobst;
            r_job=f_sel_5_JOBSTAT a_jobst;
            r_sPAAR_prterm_prnew=f_perform a_prest a_s;
            r_prterm=f_sel_1_PAAR r_sPAAR_prterm_prnew;
            r_prnew=f_sel_2_PAAR r_sPAAR_prterm_prnew
         } in  
            if (f_isEMPTYJOBSTAT a_jobst)
            then (F_PAAR r_prterm ((:) (F_PROC a_pid C_EMPTYJOBSTAT) r_prnew))
            else 
            if (((==) :: (Int -> Int -> Bool)) r_steps a_s)
            then (F_PAAR ((:) (F_PROC a_pid (F_JOBSTAT r_mark (0 :: Int) r_pr r_par r_job)) r_prterm) ((:) (F_PROC a_pid C_EMPTYJOBSTAT) r_prnew))
            else 
                (F_PAAR r_prterm ((:) (F_PROC a_pid (F_JOBSTAT r_mark (((-) :: (Int -> Int -> Int)) r_steps a_s) r_pr r_par r_job)) r_prnew));
    f_addch::T_jobdef -> Int -> T_jobstat;
    f_addch a_jobdef a_parent=
        let { 
            r_fs=f_sel_2_JOBDEF a_jobdef;
            r_js=f_sel_3_JOBDEF a_jobdef
         } in  
            if (f_isEMPTYJOBDEF a_jobdef)
            then C_EMPTYJOBSTAT
            else 
            if (((==) :: (Int -> Int -> Bool)) r_fs (0 :: Int))
            then (F_JOBSTAT (4 :: Int) r_js (0 :: Int) a_parent a_jobdef)
            else 
                (F_JOBSTAT (1 :: Int) r_fs (0 :: Int) a_parent a_jobdef);
    f_ackn::Int -> [T_jobstat] -> [T_jobstat];
    f_ackn a_n []=[];
    f_ackn a_parent (a_job:a_jobrest)=
        let { 
            r_mark=f_sel_1_JOBSTAT a_job;
            r_js=f_sel_2_JOBSTAT a_job;
            r_pid=f_sel_3_JOBSTAT a_job;
            r_par=f_sel_4_JOBSTAT a_job;
            r_jdef=f_sel_5_JOBSTAT a_job;
            r_jid=f_sel_1_JOBDEF r_jdef
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_parent r_jid)
            then ((:) (F_JOBSTAT (((+) :: (Int -> Int -> Int)) r_mark (1 :: Int)) r_js r_pid r_par r_jdef) a_jobrest)
            else 
                ((:) a_job (f_ackn a_parent a_jobrest));
    f_optim::(T_tree T_jobstat) -> Int;
    f_optim (F_LEAF a_tree)=a_tree;
    f_optim (F_TREE a_left a_right)=f_min0 (f_optim a_left) (f_optim a_right);
    f_main'::Int -> Int -> [Char];
    f_main' a_threshold a_size=
        let { 
            r_data=
                if (((==) :: (Int -> Int -> Bool)) a_size (1 :: Int))
                then c_solo
                else 
                if (((==) :: (Int -> Int -> Bool)) a_size (3 :: Int))
                then c_trio
                else 
                if (((==) :: (Int -> Int -> Bool)) a_size (7 :: Int))
                then c_septiem
                else 
                if (((==) :: (Int -> Int -> Bool)) a_size (10 :: Int))
                then c_deciem
                else 
                if (((==) :: (Int -> Int -> Bool)) a_size (11 :: Int))
                then c_undeciem
                else 
                if (((==) :: (Int -> Int -> Bool)) a_size (12 :: Int))
                then c_duodeciem
                else 
                if (((==) :: (Int -> Int -> Bool)) a_size (14 :: Int))
                then c_quadrideciem
                else 
                    C_EMPTYJOBDEF
         } in  (++) (strict_show_i (f_optim (f_sched r_data a_threshold))) "\n";
    f_benchmark_main::Int -> [Char];
    f_benchmark_main a_size=
        let { 
            r_threshold=
                if (((<=) :: (Int -> Int -> Bool)) a_size (3 :: Int))
                then (0 :: Int)
                else 
                    (((-) :: (Int -> Int -> Int)) a_size (3 :: Int))
         } in  (++) (f_sumcode (f_main' r_threshold a_size)) "\n";
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let { 
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
data 
    T_sandwarg t1=F_SANDWARG t1 Int;
    f_force' a_x=a_x;
    f_sandwich''::(t1 -> t2 -> t3) -> (T_sandwarg t1) -> (T_sandwarg t2) -> t3;
    f_sandwich'' a_f (F_SANDWARG a_vap1 a_gsd1) (F_SANDWARG a_vap2 a_gsd2)=f_force' (miraseq (f_force' (miraseq (f_force' a_vap1) a_vap2)) (a_f a_vap1 a_vap2));
    f_sandwich'::(t1 -> t2) -> (T_sandwarg t1) -> t2;
    f_sandwich' a_f (F_SANDWARG a_vap1 a_gsd1)=f_force' (miraseq (f_force' a_vap1) (a_f a_vap1));
    f_prenorm::t1 -> t2 -> t2;
    f_prenorm a_a a_b=miraseq (f_force' a_a) a_b;
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
