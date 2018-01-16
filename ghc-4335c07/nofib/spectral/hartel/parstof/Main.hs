module Main (main) -- parstof
where {
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

type 
    T_list t1=[t1];
type 
    T_string=[Char];
type 
    T_token=(Int,T_t_tokv,Int);
data 
    T_ptbindlhs=F_Pbindlhs_name T_string | F_Pbindlhs_pvl T_ptpatvarli;
data 
    T_ptcelse=F_Pcelse_expr T_ptexpr | C_Pcelse_empty;
data 
    T_ptdeftype=F_Pdeftype T_string T_pttparlist T_ptrhs;
data 
    T_ptexpli=F_Pexpli_C T_ptexpr T_ptexpli | C_Pexpli_N;
data 
    T_ptexpr=F_Pexpr_APPLY T_ptexpr T_ptexpr | F_Pexpr_TUPLE T_ptexpli | F_Pexpr_UNION T_string T_ptexpli | F_Pexpr_CASE T_ptexpr T_ptpatli T_ptcelse | F_Pexpr_LET T_ptletbinds T_ptexpr | F_Pexpr_name T_string | F_Pexpr_int Int | F_Pexpr_char Char;
data 
    T_ptftype=F_Pftype T_string T_pttype;
data 
    T_ptfunction=F_Pfunction T_string T_ptfunparli T_ptexpr;
data 
    T_ptfunparli=F_Pfunparli_C T_string T_ptfunparli | C_Pfunparli_N;
data 
    T_ptletbinds=F_Pletbinds_C T_ptbindlhs T_ptexpr T_ptletbinds | C_Pletbinds_N;
data 
    T_ptpatli=F_Ppatli_C T_string T_ptpatvarli T_ptexpr T_ptpatli | C_Ppatli_N;
data 
    T_ptpatvarli=F_Ppatvarli_C T_string T_ptpatvarli | C_Ppatvarli_N;
data 
    T_ptpr=F_Ppr_deftype T_ptdeftype | F_Ppr_ftype T_ptftype | F_Ppr_func T_ptfunction;
data 
    T_ptproglist=F_Pproglist T_ptpr T_ptproglist | C_Pproglist_empty;
data 
    T_ptrhs=F_Prhs_union T_ptunionli | F_Prhs_type T_pttype;
data 
    T_pttparlist=F_Ptparlist T_string T_pttparlist | C_Ptparlist_empty;
data 
    T_pttype=F_Ptype_fun T_pttype T_pttype | F_Ptype_tuple T_pttypli | F_Ptype_parazd T_string T_pttypli | F_Ptype_tparname T_string;
data 
    T_pttypli=F_Ptypli T_pttype T_pttypli | C_Ptypli_empty;
data 
    T_ptunionli=F_Punion T_string T_pttypli T_ptunionli | C_Punion_empty;
data 
    T_t_commt=C_Commt_fail | F_Commt_succ (T_list Char) Int;
data 
    T_t_emess=C_Emess_uk | F_Emess_known (T_list Char) Int;
data 
    T_t_lex t1=C_Lex_fail | F_Lex_succ t1 (T_list Char);
data 
    T_t_par t1=F_Par_fail T_t_emess | F_Par_succ t1 (T_list T_token);
data 
    T_t_tokv=C_Tokv_E | F_Tokv_ch Char | F_Tokv_i Int | F_Tokv_str (T_list Char) | F_Tokv_err Char (T_list Char);
    f_ceq a_a a_b=((==) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum a_b);
    c_char0val=fromEnum '0';
    f_cle a_a a_b=((<=) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum a_b);
    f_clt a_a a_b=((<) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum a_b);
    f_fpar_bestmess a_m1 a_m2=
        let { 
            f_fpar_bestmess_SWI_24 C_Emess_uk=a_m2;
            f_fpar_bestmess_SWI_24 (F_Emess_known a_errr a_line1)=
                let { 
                    f_fpar_bestmess_SWI_23 C_Emess_uk=a_m1;
                    f_fpar_bestmess_SWI_23 (F_Emess_known a_er2 a_line2)=
                        if (((<) :: (Int -> Int -> Bool)) a_line1 a_line2)
                        then a_m2
                        else 
                            a_m1
                 } in  f_fpar_bestmess_SWI_23 a_m2
         } in  f_fpar_bestmess_SWI_24 a_m1;
    f_do_paralt a_failmess a_parli a_tokli=
        let { 
            f_do_paralt_SWI_22 []=F_Par_fail a_failmess;
            f_do_paralt_SWI_22 (a_par:a_parli_tl)=
                let { 
                    f_do_paralt_SWI_21 (F_Par_fail a_mess)=f_do_paralt (f_fpar_bestmess a_failmess a_mess) a_parli_tl a_tokli;
                    f_do_paralt_SWI_21 a_do_paralt_SWI_21_non=r_parres;
                    r_parres=a_par a_tokli
                 } in  f_do_paralt_SWI_21 r_parres
         } in  f_do_paralt_SWI_22 a_parli;
    f_i2str_0 a_i a_s=
        let { 
            r_d=((quot) :: (Int -> Int -> Int)) a_i (10 :: Int);
            r_r=((+) :: (Int -> Int -> Int)) c_char0val (((rem) :: (Int -> Int -> Int)) a_i (10 :: Int))
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_i (0 :: Int))
            then a_s
            else 
                (f_i2str_0 r_d ((:) (toEnum r_r) a_s));
    f_i2str a_i=
        if (((==) :: (Int -> Int -> Bool)) a_i (0 :: Int))
        then "0"
        else 
        if (((<) :: (Int -> Int -> Bool)) a_i (0 :: Int))
        then ((:) '-' (f_i2str (((-) :: (Int -> Int -> Int)) (0 :: Int) a_i)))
        else 
            (f_i2str_0 a_i []);
    f_ptokval a_tokval=
        let { 
            f_ptokval_SWI_109 C_Tokv_E=[];
            f_ptokval_SWI_109 (F_Tokv_ch a_ch)=(:) a_ch [];
            f_ptokval_SWI_109 (F_Tokv_i a_ii)=f_i2str a_ii;
            f_ptokval_SWI_109 (F_Tokv_str a_str)=a_str;
            f_ptokval_SWI_109 (F_Tokv_err a_ch a_mess)=(++) a_mess ((++) " at char: " ((:) a_ch []))
         } in  f_ptokval_SWI_109 a_tokval;
    f_do_printtok a_tfun a_tok=
        let { 
            f_do_printtok_SWI_112 (a_dscr:a_tfun_tl)=
                let { 
                    (r_tokdef,r_what,r_end)=a_dscr
                 } in  
                    if (((==) :: (Int -> Int -> Bool)) r_rtokdef r_tokdef)
                    then ((++) r_what ((++) (f_ptokval r_tok_val) r_end))
                    else 
                        (f_do_printtok a_tfun_tl a_tok);
            f_do_printtok_SWI_112 []="Unknown token in ptoken !?!\n";
            (r_rtokdef,r_tok_val,r_lineno)=a_tok
         } in  f_do_printtok_SWI_112 a_tfun;
    f_smatch a_stri a_li=
        let { 
            f_smatch_SWI_107 []=True;
            f_smatch_SWI_107 (a_stri_e:a_stri_tl)=
                let { 
                    f_smatch_SWI_106 []=False;
                    f_smatch_SWI_106 (a_li_e:a_li_tl)=
                        if (f_ceq a_stri_e a_li_e)
                        then (f_smatch a_stri_tl a_li_tl)
                        else 
                            False
                 } in  f_smatch_SWI_106 a_li
         } in  f_smatch_SWI_107 a_stri;
    f_scancomm a_commprog a_line=
        let { 
            f_scancomm_SWI_105 []=C_Commt_fail;
            f_scancomm_SWI_105 (a_fst:a_tailprog)=
                let { 
                    f_scancomm_SWI_104 (a_head:a_tailprog_tl)=F_Commt_succ a_tailprog_tl a_line
                 } in  
                    if (f_ceq a_fst '\o012')
                    then (f_scancomm a_tailprog (((+) :: (Int -> Int -> Int)) a_line (1 :: Int)))
                    else 
                    if (f_smatch ((:) '*' "/") a_commprog)
                    then (f_scancomm_SWI_104 a_tailprog)
                    else 
                        (f_scancomm a_tailprog a_line)
         } in  f_scancomm_SWI_105 a_commprog;
    f_tokskipper a_todo=
        let { 
            f_tokskipper_SWI_103 C_Commt_fail=a_todo;
            f_tokskipper_SWI_103 (F_Commt_succ a_prog a_line)=
                let { 
                    f_tokskipper_SWI_102 []=F_Commt_succ [] a_line;
                    f_tokskipper_SWI_102 (a_prog_fst:a_prog_tail)=
                        if ((||) ((||) (f_ceq ' ' a_prog_fst) (f_ceq '\o011' a_prog_fst)) (f_ceq '\o014' a_prog_fst))
                        then (f_tokskipper (F_Commt_succ a_prog_tail a_line))
                        else 
                        if (f_ceq '\o012' a_prog_fst)
                        then (f_tokskipper (F_Commt_succ a_prog_tail (((+) :: (Int -> Int -> Int)) a_line (1 :: Int))))
                        else 
                        if (f_smatch ((:) '/' ((:) '*' [])) a_prog)
                        then (f_tokskipper (f_scancomm a_prog a_line))
                        else 
                            (F_Commt_succ a_prog a_line)
                 } in  f_tokskipper_SWI_102 a_prog
         } in  f_tokskipper_SWI_103 a_todo;
    f_mvltok_error a_toktag a_ch a_message a_line=(a_toktag,F_Tokv_err a_ch a_message,a_line);
    c_tok_error=(60 :: Int);
    c_err_comment="Illegal comment";
    c_tok_EOF=(61 :: Int);
    f_mvltok_EOF a_line=(c_tok_EOF,C_Tokv_E,a_line);
    c_err_lexical="Lexical error";
    f_maketoken a_dolex a_prog a_line=
        let { 
            f_maketoken_SWI_9 C_Lex_fail=f_maketoken_SWI_7 a_prog;
            f_maketoken_SWI_9 (F_Lex_succ a_tok a_prog_tail)=
                let { 
                    (r_toktag,r_tokval)=a_tok
                 } in  ((r_toktag,r_tokval,a_line),a_prog_tail);
            f_maketoken_SWI_7 (a_prog_hd:a_prog_tl)=(f_mvltok_error c_tok_error a_prog_hd c_err_lexical a_line,a_prog_tl)
         } in  f_maketoken_SWI_9 (a_dolex a_prog);
    f_lexer a_lexitem a_stream=
        let { 
            (r_frst_chars,r_pattfun)=a_lexitem
         } in  r_pattfun a_stream;
    f_lalt_getstart a_stfuntup=
        let { 
            (r_st,r_fun)=a_stfuntup
         } in  r_st;
    f_lalt_uniq_do a_chl a_prev=
        let { 
            f_lalt_uniq_do_SWI_67 []=[];
            f_lalt_uniq_do_SWI_67 (a_chl_hd:a_chl_tl)=
                if (f_ceq a_chl_hd a_prev)
                then (f_lalt_uniq_do a_chl_tl a_prev)
                else 
                    ((:) a_chl_hd (f_lalt_uniq_do a_chl_tl a_chl_hd))
         } in  f_lalt_uniq_do_SWI_67 a_chl;
    f_lalt_uniq a_chlist=
        let { 
            f_lalt_uniq_SWI_66 []=[];
            f_lalt_uniq_SWI_66 (a_chl_hd:a_chl_tl)=(:) a_chl_hd (f_lalt_uniq_do a_chl_tl a_chl_hd)
         } in  f_lalt_uniq_SWI_66 a_chlist;
    f_sort_split_do a_len a_li=
        let { 
            f_sort_split_do_SWI_99 (a_li_hd:a_li_tl)=
                let { 
                    (r_tailfst,r_scnd)=f_sort_split_do (((-) :: (Int -> Int -> Int)) a_len (1 :: Int)) a_li_tl
                 } in  ((:) a_li_hd r_tailfst,r_scnd)
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_len (0 :: Int))
            then ([],a_li)
            else 
                (f_sort_split_do_SWI_99 a_li);
    f_sort_split a_li=
        let { 
            r_len=length a_li
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_len (1 :: Int))
            then (a_li,[])
            else 
                (f_sort_split_do (((quot) :: (Int -> Int -> Int)) r_len (2 :: Int)) a_li);
    f_sort_ch_merge a_l1 a_l2=
        let { 
            f_sort_ch_merge_SWI_97 (a_l1_hd:a_l1_tl)=
                let { 
                    f_sort_ch_merge_SWI_96 (a_l2_hd:a_l2_tl)=
                        if (f_cle a_l1_hd a_l2_hd)
                        then ((:) a_l1_hd (f_sort_ch_merge a_l1_tl a_l2))
                        else 
                            ((:) a_l2_hd (f_sort_ch_merge a_l1 a_l2_tl));
                    f_sort_ch_merge_SWI_96 []=a_l1
                 } in  f_sort_ch_merge_SWI_96 a_l2;
            f_sort_ch_merge_SWI_97 []=a_l2
         } in  f_sort_ch_merge_SWI_97 a_l1;
    f_sort_ch a_chl=
        let { 
            f_sort_ch_SWI_95 []=[];
            f_sort_ch_SWI_95 (a_a:a_b)=
                let { 
                    f_sort_ch_SWI_94 []=a_chl;
                    f_sort_ch_SWI_94 a_sort_ch_SWI_94_non=
                        let { 
                            r_sl2=f_sort_ch r_l2;
                            r_sl1=f_sort_ch r_l1;
                            (r_l1,r_l2)=f_sort_split a_chl
                         } in  f_sort_ch_merge r_sl1 r_sl2
                 } in  f_sort_ch_SWI_94 a_b
         } in  f_sort_ch_SWI_95 a_chl;
    f_isin a_feq a_l a_elem=
        let { 
            f_isin_SWI_87 []=False;
            f_isin_SWI_87 (a_l_elem:a_l_tl)=
                if (a_feq a_l_elem a_elem)
                then True
                else 
                    (f_isin a_feq a_l_tl a_elem)
         } in  f_isin_SWI_87 a_l;
    f_lalt_funselect_do a_altlist a_ch=
        let { 
            f_lalt_funselect_do_SWI_64 []=[];
            f_lalt_funselect_do_SWI_64 (a_al_hd:a_al_tl)=
                let { 
                    (r_ch_li,r_ch_fun)=a_al_hd
                 } in  
                    if (f_isin f_ceq r_ch_li a_ch)
                    then ((:) r_ch_fun (f_lalt_funselect_do a_al_tl a_ch))
                    else 
                        (f_lalt_funselect_do a_al_tl a_ch)
         } in  f_lalt_funselect_do_SWI_64 a_altlist;
    f_lalt_funselect a_altlist a_ch=(a_ch,f_lalt_funselect_do a_altlist a_ch);
    f_lalt_stree_fail a_stream=C_Lex_fail;
    f_lalt_stree_mustbe_do a_fli a_stream=
        let { 
            f_lalt_stree_mustbe_do_SWI_73 []=C_Lex_fail;
            f_lalt_stree_mustbe_do_SWI_73 (a_f:a_f_tl)=
                let { 
                    f_lalt_stree_mustbe_do_SWI_72 C_Lex_fail=f_lalt_stree_mustbe_do a_f_tl a_stream;
                    f_lalt_stree_mustbe_do_SWI_72 (F_Lex_succ a_val a_str_tl)=r_res;
                    r_res=a_f a_stream
                 } in  f_lalt_stree_mustbe_do_SWI_72 r_res
         } in  f_lalt_stree_mustbe_do_SWI_73 a_fli;
    f_lalt_stree_mustbe a_chf a_stream=
        let { 
            f_lalt_stree_mustbe_SWI_71 (a_str_hd:a_str_tl)=
                if (f_ceq a_str_hd r_ch)
                then (f_lalt_stree_mustbe_do r_fli a_stream)
                else 
                    C_Lex_fail;
            (r_ch,r_fli)=a_chf
         } in  f_lalt_stree_mustbe_SWI_71 a_stream;
    f_lalt_stree_split a_len a_chfli=
        let { 
            f_lalt_stree_split_SWI_76 (a_chf:a_chfli_tl)=
                let { 
                    (r_ch,r_fli)=a_chf;
                    (r_onehalf,r_pivot,r_twohalf)=f_lalt_stree_split (((-) :: (Int -> Int -> Int)) a_len (1 :: Int)) a_chfli_tl
                 } in  
                    if (((==) :: (Int -> Int -> Bool)) a_len (1 :: Int))
                    then ((:) a_chf [],r_ch,a_chfli_tl)
                    else 
                        ((:) a_chf r_onehalf,r_pivot,r_twohalf)
         } in  f_lalt_stree_split_SWI_76 a_chfli;
    f_lalt_stree_choose a_left a_lastleft a_right a_stream=
        let { 
            f_lalt_stree_choose_SWI_77 (a_str_hd:a_str_tl)=
                if (f_clt a_lastleft a_str_hd)
                then (a_right a_stream)
                else 
                    (a_left a_stream)
         } in  f_lalt_stree_choose_SWI_77 a_stream;
    f_lalt_stree a_chfli=
        let { 
            f_lalt_stree_SWI_69 []=f_lalt_stree_fail;
            f_lalt_stree_SWI_69 (a_chf_hd:a_chf_tl)=
                let { 
                    (r_onehalf,r_pivot,r_twohalf)=f_lalt_stree_split (((quot) :: (Int -> Int -> Int)) r_len (2 :: Int)) a_chfli;
                    r_len=length a_chfli
                 } in  
                    if (((==) :: (Int -> Int -> Bool)) r_len (1 :: Int))
                    then (f_lalt_stree_mustbe a_chf_hd)
                    else 
                        (f_lalt_stree_choose (f_lalt_stree r_onehalf) r_pivot (f_lalt_stree r_twohalf))
         } in  f_lalt_stree_SWI_69 a_chfli;
    f_lexalt a_altlist=
        let { 
            r_altfun_list=f_map (f_lalt_funselect a_altlist) r_sin_list;
            r_sin_list=f_lalt_uniq (f_sort_ch r_flat_list);
            r_flat_list=f_foldr (++) [] r_st_list;
            r_st_list=f_map f_lalt_getstart a_altlist
         } in  (r_sin_list,f_lalt_stree r_altfun_list);
    f_lexcvt_do a_cvtfun a_patt_fun a_stream=
        let { 
            f_lexcvt_do_SWI_52 C_Lex_fail=C_Lex_fail;
            f_lexcvt_do_SWI_52 (F_Lex_succ a_val a_str_tl)=F_Lex_succ (a_cvtfun a_val) a_str_tl
         } in  f_lexcvt_do_SWI_52 (a_patt_fun a_stream);
    f_lexcvt a_cvtfun a_patt=
        let { 
            (r_patt_st,r_patt_fun)=a_patt
         } in  (r_patt_st,f_lexcvt_do a_cvtfun r_patt_fun);
    f_mvtok_char a_toktag a_ch=(a_toktag,F_Tokv_ch a_ch);
    c_tok_char=(11 :: Int);
    f_lexseq_do a_join a_fst a_scnd a_stream=
        let { 
            f_lexseq_do_SWI_57 C_Lex_fail=C_Lex_fail;
            f_lexseq_do_SWI_57 (F_Lex_succ a_fstres a_strtail)=
                let { 
                    f_lexseq_do_SWI_56 C_Lex_fail=C_Lex_fail;
                    f_lexseq_do_SWI_56 (F_Lex_succ a_scndres a_strtltl)=F_Lex_succ (a_join a_fstres a_scndres) a_strtltl
                 } in  f_lexseq_do_SWI_56 (a_scnd a_strtail)
         } in  f_lexseq_do_SWI_57 (a_fst a_stream);
    f_lexseq a_join a_fst a_scnd=
        let { 
            (r_scnd_start,r_scnd_fun)=a_scnd;
            (r_fst_start,r_fst_fun)=a_fst
         } in  (r_fst_start,f_lexseq_do a_join r_fst_fun r_scnd_fun);
    f_flex_sel2 a_a a_b=a_b;
    f_lexlit_do a_matchch a_stream=
        let { 
            f_lexlit_do_SWI_53 []=C_Lex_fail;
            f_lexlit_do_SWI_53 (a_fst:a_str_tail)=
                if (f_ceq a_fst a_matchch)
                then (F_Lex_succ a_matchch a_str_tail)
                else 
                    C_Lex_fail
         } in  f_lexlit_do_SWI_53 a_stream;
    f_lexlit a_matchch=((:) a_matchch [],f_lexlit_do a_matchch);
    f_flex_sel1 a_a a_b=a_a;
    c_llex_num=f_lexalt (f_map f_lexlit "0123456789");
    c_llex_upper=f_lexalt (f_map f_lexlit "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    c_llex_lower=f_lexalt (f_map f_lexlit "abcdefghijklmnopqrstuvwyxz");
    c_llex_alphanum=f_lexalt ((:) c_llex_num ((:) c_llex_upper ((:) c_llex_lower [])));
    f_flex_escape_transl a_backsla a_escch=
        if (f_ceq a_escch 'n')
        then '\o012'
        else 
        if (f_ceq a_escch 't')
        then '\o011'
        else 
        if (f_ceq a_escch 'f')
        then '\o014'
        else 
        if (f_ceq a_escch '\o134')
        then '\o134'
        else 
        if (f_ceq a_escch '\o047')
        then '\o047'
        else 
        if (f_ceq a_escch '\o042')
        then '\o042'
        else 
            '@';
    c_llex_escaped_char=f_lexalt (f_map f_lexlit ((:) 'n' ((:) 't' ((:) 'f' ((:) '\o134' 
        ((:) '\o047' ((:) '\o042' [])))))));
    f_flex_escape_hnums a_hnum1 a_hnum2=toEnum (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (16 :: Int) (((-) :: (Int -> Int -> Int)) (fromEnum a_hnum1) (fromEnum '0'))) 
        (((-) :: (Int -> Int -> Int)) (fromEnum a_hnum2) (fromEnum '0')));
    c_llex_hnum=f_lexalt ((:) c_llex_num ((:) (f_lexlit 'A') ((:) (f_lexlit 'B') 
        ((:) (f_lexlit 'C') ((:) (f_lexlit 'D') ((:) (f_lexlit 'E') ((:) 
        (f_lexlit 'F') [])))))));
    c_llex_escape=f_lexalt ((:) (f_lexseq f_flex_escape_transl (f_lexlit '\o134') c_llex_escaped_char) ((:) (f_lexseq f_flex_sel2 
        (f_lexlit '\o134') (f_lexseq f_flex_escape_hnums c_llex_hnum c_llex_hnum)) []));
    c_llex_weird=f_lexalt (f_map f_lexlit ((:) '!' ((:) '#' ((:) '$' ((:) '%' 
        ((:) '&' ((:) '(' ((:) ')' ((:) '*' ((:) '+' ((:) ',' ((:) '-' 
        ((:) '.' ((:) '/' ((:) ':' ((:) ';' ((:) '<' ((:) '=' ((:) '>' 
        ((:) '?' ((:) '@' ((:) '[' ((:) ']' ((:) '^' ((:) '`' ((:) '{' 
        ((:) '|' ((:) '}' ((:) '~' ((:) ' ' ((:) '\o011' [])))))))))))))))))))))))))))))));
    c_llex_chchar=f_lexalt ((:) c_llex_alphanum ((:) c_llex_escape ((:) c_llex_weird ((:) (f_lexlit '\o042') []))));
    c_lex_char=f_lexseq f_flex_sel2 (f_lexlit '\o047') (f_lexseq f_flex_sel1 c_llex_chchar (f_lexlit '\o047'));
    f_mvtok_int a_toktag a_i=(a_toktag,F_Tokv_i a_i);
    c_tok_int=(12 :: Int);
    f_lexrep_do a_join a_init a_patt a_stream=
        let { 
            f_lexrep_do_SWI_60 C_Lex_fail=F_Lex_succ a_init a_stream;
            f_lexrep_do_SWI_60 (F_Lex_succ a_pattres a_strtail)=f_lexrep_do a_join (a_join a_init a_pattres) a_patt a_strtail
         } in  f_lexrep_do_SWI_60 (a_patt a_stream);
    f_lexrepplus_do a_join a_init a_patt a_stream=
        let { 
            f_lexrepplus_do_SWI_59 C_Lex_fail=C_Lex_fail;
            f_lexrepplus_do_SWI_59 (F_Lex_succ a_pattres a_strtail)=f_lexrep_do a_join (a_join a_init a_pattres) a_patt a_strtail
         } in  f_lexrepplus_do_SWI_59 (a_patt a_stream);
    f_lexrepplus a_join a_init a_patt=
        let { 
            (r_patt_st,r_patt_fun)=a_patt
         } in  (r_patt_st,f_lexrepplus_do a_join a_init r_patt_fun);
    f_flex_muladd a_total a_digit=((-) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (10 :: Int) a_total) (fromEnum a_digit)) (fromEnum '0');
    c_lex_int=f_lexrepplus f_flex_muladd (0 :: Int) c_llex_num;
    f_mvtok_empty a_toktag a_val=(a_toktag,C_Tokv_E);
    c_tok_TYPE=(15 :: Int);
    f_lexmatch_do a_str a_stream=
        let { 
            f_lexmatch_do_SWI_62 []=F_Lex_succ [] a_stream;
            f_lexmatch_do_SWI_62 (a_str_fst:a_str_tl)=f_lexseq_do f_flex_sel2 (f_lexlit_do a_str_fst) (f_lexmatch_do a_str_tl) a_stream
         } in  f_lexmatch_do_SWI_62 a_str;
    f_lexmatch a_str=
        let { 
            f_lexmatch_SWI_61 (a_str_hd:a_str_tl)=((:) a_str_hd [],f_lexmatch_do a_str)
         } in  f_lexmatch_SWI_61 a_str;
    c_lex_TYPE=f_lexmatch "TYPE";
    c_tok_FTYPE=(16 :: Int);
    c_lex_FTYPE=f_lexmatch "FTYPE";
    c_tok_LET=(17 :: Int);
    c_lex_LET=f_lexmatch "LET";
    c_tok_IN=(18 :: Int);
    c_lex_IN=f_lexmatch "IN";
    c_tok_IF=(19 :: Int);
    c_lex_IF=f_lexmatch "IF";
    c_tok_THEN=(20 :: Int);
    c_lex_THEN=f_lexmatch "THEN";
    c_tok_ELIF=(21 :: Int);
    c_lex_ELIF=f_lexmatch "ELIF";
    c_tok_ELSE=(22 :: Int);
    c_lex_ELSE=f_lexmatch "ELSE";
    c_tok_FI=(23 :: Int);
    c_lex_FI=f_lexmatch "FI";
    c_tok_CASE=(24 :: Int);
    c_lex_CASE=f_lexmatch "CASE";
    f_mvtok_string a_toktag a_str=(a_toktag,F_Tokv_str a_str);
    c_tok_uppid=(25 :: Int);
    f_flex_cons a_head a_tail=(:) a_head a_tail;
    f_flex_addchar a_stri a_cha=(++) a_stri ((:) a_cha []);
    f_flex_tostr a_ch=(:) a_ch [];
    c_lex_uppid=f_lexalt ((:) (f_lexseq f_flex_cons c_llex_upper (f_lexrepplus f_flex_addchar [] c_llex_alphanum)) ((:) (f_lexcvt f_flex_tostr c_llex_upper) []));
    c_tok_str=(13 :: Int);
    f_flex_estr a_ch=[];
    c_llex_strchar=f_lexalt ((:) c_llex_alphanum ((:) c_llex_escape ((:) c_llex_weird ((:) (f_lexlit '\o047') []))));
    c_llex_endstr=f_lexalt ((:) (f_lexcvt f_flex_estr (f_lexlit '\o042')) ((:) (f_lexseq f_flex_sel1 
        (f_lexrepplus f_flex_addchar [] c_llex_strchar) (f_lexlit '\o042')) []));
    c_lex_str=f_lexseq f_flex_sel2 (f_lexlit '\o042') c_llex_endstr;
    c_tok_lowid=(14 :: Int);
    c_lex_lowid=f_lexalt ((:) (f_lexseq f_flex_cons c_llex_lower (f_lexrepplus f_flex_addchar [] c_llex_alphanum)) ((:) (f_lexcvt f_flex_tostr c_llex_lower) []));
    c_tok_opfunc=(26 :: Int);
    c_lex_opfunc=f_lexseq f_flex_sel1 (f_lexlit '-') (f_lexlit '>');
    c_tok_opapnd=(31 :: Int);
    c_lex_opapnd=f_lexseq f_flex_sel1 (f_lexlit '+') (f_lexlit '+');
    c_tok_opcolon=(32 :: Int);
    c_lex_opcolon=f_lexlit ':';
    c_tok_opand=(33 :: Int);
    c_lex_opand=f_lexlit '&';
    c_tok_opsep=(34 :: Int);
    c_lex_opsep=f_lexlit '|';
    c_tok_ople=(35 :: Int);
    c_lex_ople=f_lexseq f_flex_sel1 (f_lexlit '<') (f_lexlit '=');
    c_tok_opneq=(37 :: Int);
    c_lex_opneq=f_lexseq f_flex_sel1 (f_lexlit '~') (f_lexlit '=');
    c_tok_opeq=(36 :: Int);
    c_lex_opeq=f_lexseq f_flex_sel1 (f_lexlit '=') (f_lexlit '=');
    c_tok_opge=(38 :: Int);
    c_lex_opge=f_lexseq f_flex_sel1 (f_lexlit '>') (f_lexlit '=');
    c_tok_oplt=(39 :: Int);
    c_lex_oplt=f_lexlit '<';
    c_tok_opgt=(40 :: Int);
    c_lex_opgt=f_lexlit '>';
    c_tok_opdef=(30 :: Int);
    c_lex_opdef=f_lexlit '=';
    c_tok_opadd=(41 :: Int);
    c_lex_opadd=f_lexlit '+';
    c_tok_opdash=(42 :: Int);
    c_lex_opdash=f_lexlit '-';
    c_tok_opmul=(43 :: Int);
    c_lex_opmul=f_lexlit '*';
    c_tok_opdiv=(44 :: Int);
    c_lex_opdiv=f_lexlit '/';
    c_tok_opperc=(45 :: Int);
    c_lex_opperc=f_lexlit '%';
    c_tok_optwig=(46 :: Int);
    c_lex_optwig=f_lexlit '~';
    c_tok_btup=(47 :: Int);
    c_lex_btup=f_lexlit '[';
    c_tok_etup=(50 :: Int);
    c_lex_etup=f_lexlit ']';
    c_tok_begin=(51 :: Int);
    c_lex_begin=f_lexlit '{';
    c_tok_end=(52 :: Int);
    c_lex_end=f_lexlit '}';
    c_tok_obrack=(53 :: Int);
    c_lex_obrack=f_lexlit '(';
    c_tok_cbrack=(54 :: Int);
    c_lex_cbrack=f_lexlit ')';
    c_tok_semi=(55 :: Int);
    c_lex_semi=f_lexlit ';';
    c_tok_comma=(56 :: Int);
    c_lex_comma=f_lexlit ',';
    c_tok_hash=(57 :: Int);
    c_lex_hash=f_lexlit '#';
    c_matchtoklist3=(:) (f_lexcvt (f_mvtok_empty c_tok_opdash) c_lex_opdash) ((:) (f_lexcvt (f_mvtok_empty c_tok_opmul) c_lex_opmul) 
        ((:) (f_lexcvt (f_mvtok_empty c_tok_opdiv) c_lex_opdiv) ((:) (f_lexcvt (f_mvtok_empty c_tok_opperc) c_lex_opperc) ((:) 
        (f_lexcvt (f_mvtok_empty c_tok_optwig) c_lex_optwig) ((:) (f_lexcvt (f_mvtok_empty c_tok_btup) c_lex_btup) ((:) (f_lexcvt 
        (f_mvtok_empty c_tok_etup) c_lex_etup) ((:) (f_lexcvt (f_mvtok_empty c_tok_begin) c_lex_begin) ((:) (f_lexcvt (f_mvtok_empty c_tok_end) c_lex_end) 
        ((:) (f_lexcvt (f_mvtok_empty c_tok_obrack) c_lex_obrack) ((:) (f_lexcvt (f_mvtok_empty c_tok_cbrack) c_lex_cbrack) ((:) 
        (f_lexcvt (f_mvtok_empty c_tok_semi) c_lex_semi) ((:) (f_lexcvt (f_mvtok_empty c_tok_comma) c_lex_comma) ((:) (f_lexcvt 
        (f_mvtok_empty c_tok_hash) c_lex_hash) [])))))))))))));
    c_matchtoklist2=(:) (f_lexcvt (f_mvtok_string c_tok_lowid) c_lex_lowid) ((:) (f_lexcvt (f_mvtok_empty c_tok_opfunc) c_lex_opfunc) 
        ((:) (f_lexcvt (f_mvtok_empty c_tok_opapnd) c_lex_opapnd) ((:) (f_lexcvt (f_mvtok_empty c_tok_opcolon) c_lex_opcolon) ((:) 
        (f_lexcvt (f_mvtok_empty c_tok_opand) c_lex_opand) ((:) (f_lexcvt (f_mvtok_empty c_tok_opsep) c_lex_opsep) ((:) (f_lexcvt 
        (f_mvtok_empty c_tok_ople) c_lex_ople) ((:) (f_lexcvt (f_mvtok_empty c_tok_opneq) c_lex_opneq) ((:) (f_lexcvt (f_mvtok_empty c_tok_opeq) c_lex_opeq) 
        ((:) (f_lexcvt (f_mvtok_empty c_tok_opge) c_lex_opge) ((:) (f_lexcvt (f_mvtok_empty c_tok_oplt) c_lex_oplt) ((:) 
        (f_lexcvt (f_mvtok_empty c_tok_opgt) c_lex_opgt) ((:) (f_lexcvt (f_mvtok_empty c_tok_opdef) c_lex_opdef) ((:) (f_lexcvt 
        (f_mvtok_empty c_tok_opadd) c_lex_opadd) c_matchtoklist3)))))))))))));
    c_matchtoklist=(:) (f_lexcvt (f_mvtok_char c_tok_char) c_lex_char) ((:) (f_lexcvt (f_mvtok_int c_tok_int) c_lex_int) 
        ((:) (f_lexcvt (f_mvtok_empty c_tok_TYPE) c_lex_TYPE) ((:) (f_lexcvt (f_mvtok_empty c_tok_FTYPE) c_lex_FTYPE) ((:) 
        (f_lexcvt (f_mvtok_empty c_tok_LET) c_lex_LET) ((:) (f_lexcvt (f_mvtok_empty c_tok_IN) c_lex_IN) ((:) (f_lexcvt 
        (f_mvtok_empty c_tok_IF) c_lex_IF) ((:) (f_lexcvt (f_mvtok_empty c_tok_THEN) c_lex_THEN) ((:) (f_lexcvt (f_mvtok_empty c_tok_ELIF) c_lex_ELIF) 
        ((:) (f_lexcvt (f_mvtok_empty c_tok_ELSE) c_lex_ELSE) ((:) (f_lexcvt (f_mvtok_empty c_tok_FI) c_lex_FI) ((:) 
        (f_lexcvt (f_mvtok_empty c_tok_CASE) c_lex_CASE) ((:) (f_lexcvt (f_mvtok_string c_tok_uppid) c_lex_uppid) ((:) (f_lexcvt 
        (f_mvtok_string c_tok_str) c_lex_str) c_matchtoklist2)))))))))))));
      c_matchtok=f_lexalt c_matchtoklist;
-- TEMP: when testing
--    c_matchtok = f_lexalt [f_lexcvt (f_mvtok_string c_tok_lowid) c_lex_lowid];

    f_do_tokenize a_prog a_lineno=
        let { 
            f_do_tokenize_SWI_2 (a_prog_hd:a_prog_tl)=(:) (f_mvltok_error c_tok_error a_prog_hd c_err_comment a_lineno) [];
            f_do_tokenize_SWI_5 C_Commt_fail=f_do_tokenize_SWI_2 a_prog;
            f_do_tokenize_SWI_5 (F_Commt_succ a_prog_nospace a_lineno_ns)=
                let { 
                    f_do_tokenize_SWI_4 []=(:) (f_mvltok_EOF a_lineno_ns) [];
                    f_do_tokenize_SWI_4 a_do_tokenize_SWI_4_non=
                        let { 
                            (r_token,r_prog_tail)=f_maketoken (f_lexer c_matchtok) a_prog_nospace a_lineno_ns
                         } in  (:) r_token (f_do_tokenize r_prog_tail a_lineno_ns)
                 } in  f_do_tokenize_SWI_4 a_prog_nospace
         } in  f_do_tokenize_SWI_5 (f_tokskipper (F_Commt_succ a_prog a_lineno));
    c_err_CASE="Illegal CASE expression";
    c_err_CASEpatt="Illegal CASE-pattern";
    c_err_ELSE="Illegal ELSE-part";
    c_err_FI="FI expected";
    c_err_THEN="Illegal THEN-part";
    c_err_THENELIF="Illegal THEN part for ELIF";
    c_err_cbrack=(:) 'C' ((:) 'l' ((:) 'o' ((:) 's' ((:) 'e' ((:) ' ' 
        ((:) 'b' ((:) 'r' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:) 't' 
        ((:) ' ' ((:) '(' ((:) '\o047' ((:) ')' ((:) '\o047' ") expected")))))))))))))))));
    c_err_closbr=(:) 'C' ((:) 'l' ((:) 'o' ((:) 's' ((:) 'e' ((:) ' ' 
        ((:) 'b' ((:) 'r' ((:) 'a' ((:) 'c' ((:) 'k' ((:) 'e' ((:) 't' 
        ((:) ' ' ((:) '(' ((:) '\o047' ((:) ')' ((:) '\o047' ") expected")))))))))))))))));
    c_err_constr=(:) 'I' ((:) 'l' ((:) 'l' ((:) 'e' ((:) 'g' ((:) 'a' 
        ((:) 'l' ((:) ' ' ((:) 't' ((:) 'u' ((:) 'p' ((:) 'l' ((:) 'e' 
        ((:) ' ' ((:) 'o' ((:) 'r' ((:) ' ' ((:) 'u' ((:) 'n' ((:) 'i' 
        ((:) 'o' ((:) 'n' ((:) ' ' ((:) 'c' ((:) 'o' ((:) 'n' ((:) 's' 
        ((:) 't' ((:) 'r' ((:) 'u' ((:) 'c' ((:) 't' ((:) 'o' ((:) 'r' 
        ((:) '\o012' []))))))))))))))))))))))))))))))))));
    c_err_endCASE=(:) 'E' ((:) 'n' ((:) 'd' ((:) '-' ((:) 'C' ((:) 'A' 
        ((:) 'S' ((:) 'E' ((:) ' ' ((:) '(' ((:) '\o047' ((:) '}' ((:) '\o047' ") expected"))))))))))));
    c_err_endtup=(:) 'e' ((:) 'n' ((:) 'd' ((:) ' ' ((:) 'o' ((:) 'f' 
        ((:) ' ' ((:) 't' ((:) 'u' ((:) 'p' ((:) 'l' ((:) 'e' ((:) ' ' 
        ((:) '(' ((:) '\o047' ((:) ']' ((:) '\o047' ") expected"))))))))))))))));
    c_err_endtupl=(:) 'E' ((:) 'n' ((:) 'd' ((:) ' ' ((:) 'o' ((:) 'f' 
        ((:) ' ' ((:) 't' ((:) 'u' ((:) 'p' ((:) 'l' ((:) 'e' ((:) ' ' 
        ((:) '(' ((:) '\o047' ((:) ']' ((:) '\o047' ") expected"))))))))))))))));
    c_err_enduni=(:) 'E' ((:) 'n' ((:) 'd' ((:) ' ' ((:) 'o' ((:) 'f' 
        ((:) ' ' ((:) 'u' ((:) 'n' ((:) 'i' ((:) 'o' ((:) 'n' ((:) ' ' 
        ((:) '(' ((:) '\o047' ((:) ']' ((:) '\o047' ") expected"))))))))))))))));
    c_err_explist="Illegal expression list";
    c_err_funcdef="Error in function definition";
    c_err_funtyp="Error in function type definition";
    c_err_funtype="Illegal function-type definition";
    c_err_letlist="Illegal LET-definition list";
    c_err_nest="Illegal nested expression";
    c_err_nesttype="Illegal nested type definition";
    c_err_progterm=(:) 'P' ((:) 'r' ((:) 'o' ((:) 'g' ((:) 'r' ((:) 'a' 
        ((:) 'm' ((:) ' ' ((:) 'd' ((:) 'o' ((:) 'e' ((:) 's' ((:) 'n' 
        ((:) '\o047' "t terminate with EOF")))))))))))));
    c_err_rhsop="Illegal rhs of operator expression";
    c_err_rhstype="Illegal rhs of type definition";
    c_err_rhsunop="Illegal rhs of unary operator expression";
    c_err_semicol=(:) 'S' ((:) 'e' ((:) 'm' ((:) 'i' ((:) 'c' ((:) 'o' 
        ((:) 'l' ((:) 'o' ((:) 'n' ((:) ' ' ((:) '(' ((:) '\o047' ((:) ';' 
        ((:) '\o047' ") expected")))))))))))));
    c_err_tuptype="Illegal tuple-type definition";
    c_err_typedef="Error in type definition";
    c_err_unitype="Illegal union-type definition";
    f_first a_n a_li=
        let { 
            f_first_SWI_86 []=[];
            f_first_SWI_86 (a_elem:a_li_tl)=
                if (((>) :: (Int -> Int -> Bool)) a_n (0 :: Int))
                then ((:) a_elem (f_first (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_li_tl))
                else 
                    []
         } in  f_first_SWI_86 a_li;
    c_fparNILname=(:) 'N' [];
    f_m_pexpr_CASE a_casexpr a_patli a_cels=F_Pexpr_CASE a_casexpr a_patli a_cels;
    f_fpar_case_mk a_caseexp a_mali=
        let { 
            (r_patli,r_celse)=a_mali
         } in  f_m_pexpr_CASE a_caseexp r_patli r_celse;
    f_m_punion a_thetag a_typli a_unili=F_Punion a_thetag a_typli a_unili;
    f_fpar_deftype_rhs_mk a_nam_tyli a_tagli=
        let { 
            (r_tagnam,r_ttypli)=a_nam_tyli
         } in  f_m_punion r_tagnam r_ttypli a_tagli;
    f_m_pexpli_C a_expli a_eli=F_Pexpli_C a_expli a_eli;
    c_m_pexpli_N=C_Pexpli_N;
    f_fpar_exprlist_mk a_expli=f_foldr f_m_pexpli_C c_m_pexpli_N a_expli;
    f_m_pfunction a_funname a_funparli a_expr=F_Pfunction a_funname a_funparli a_expr;
    f_fpar_function_mk a_nam_par a_expr=
        let { 
            (r_fname,r_parli)=a_nam_par
         } in  f_m_pfunction r_fname r_parli a_expr;
    f_m_pexpr_APPLY a_fexpr a_pexpr=F_Pexpr_APPLY a_fexpr a_pexpr;
    f_m_pexpr_name a_name=F_Pexpr_name a_name;
    f_tok_findname a_tokid a_table=
        let { 
            f_tok_findname_SWI_11 []=(:) 'H' ((:) 'u' ((:) 'h' ((:) '?' ((:) '!' []))));
            f_tok_findname_SWI_11 (a_tok_nam:a_table_tl)=
                let { 
                    (r_tok,r_name)=a_tok_nam
                 } in  
                    if (((==) :: (Int -> Int -> Bool)) r_tok a_tokid)
                    then r_name
                    else 
                        (f_tok_findname a_tokid a_table_tl)
         } in  f_tok_findname_SWI_11 a_table;
    c_tok_tripnametable=(:) (c_tok_IF,"if") [];
    f_tok_tripname a_tokid=f_tok_findname a_tokid c_tok_tripnametable;
    f_fpar_ifexpr_mk_do a_elifli a_elsep=
        let { 
            f_fpar_ifexpr_mk_do_SWI_45 []=a_elsep;
            f_fpar_ifexpr_mk_do_SWI_45 (a_elifp:a_elli)=
                let { 
                    (r_ifp,r_thp)=a_elifp
                 } in  f_m_pexpr_APPLY (f_m_pexpr_APPLY (f_m_pexpr_APPLY (f_m_pexpr_name (f_tok_tripname c_tok_IF)) r_ifp) r_thp) (f_fpar_ifexpr_mk_do a_elli a_elsep)
         } in  f_fpar_ifexpr_mk_do_SWI_45 a_elifli;
    f_fpar_ifexpr_mk a_if_th_elif a_elsep=
        let { 
            (r_thenp,r_elifli)=r_th_elif;
            (r_ifp,r_th_elif)=a_if_th_elif
         } in  f_fpar_ifexpr_mk_do ((:) (r_ifp,r_thenp) r_elifli) a_elsep;
    f_parlit a_cvtf a_tokiden a_tokli=
        let { 
            f_parlit_SWI_18 (a_tok:a_tokli_tl)=
                let { 
                    (r_tok_id,r_tok_val,r_line)=a_tok
                 } in  
                    if (((==) :: (Int -> Int -> Bool)) r_tok_id a_tokiden)
                    then (F_Par_succ (a_cvtf r_tok_val) a_tokli_tl)
                    else 
                        (F_Par_fail C_Emess_uk)
         } in  f_parlit_SWI_18 a_tokli;
    f_fparoper a_oper a_tok=a_oper;
    f_fpar_opermk a_tok_iden=f_parlit (f_fparoper a_tok_iden) a_tok_iden;
    f_m_ppatli_C a_thetag a_pvl a_expr a_pli=F_Ppatli_C a_thetag a_pvl a_expr a_pli;
    f_fpar_pattli_mk a_patt a_pli=
        let { 
            (r_name,r_varli)=r_nam_vli;
            (r_nam_vli,r_expr)=a_patt
         } in  f_m_ppatli_C r_name r_varli r_expr a_pli;
    c_m_pproglist_empty=C_Pproglist_empty;
    f_fpar_prog_mkempty a_eofval=c_m_pproglist_empty;
    f_m_ptype_parazd a_name a_typli=F_Ptype_parazd a_name a_typli;
    c_m_ptypli_empty=C_Ptypli_empty;
    f_fpar_simptype_eparaz a_tname=f_m_ptype_parazd a_tname c_m_ptypli_empty;
    f_m_pexpr_UNION a_thetag a_expli=F_Pexpr_UNION a_thetag a_expli;
    c_tok_binnametable=(:) (c_tok_opapnd,"append") ((:) (c_tok_opand,"and") ((:) (c_tok_opsep,"or") 
        ((:) (c_tok_ople,"le") ((:) (c_tok_opeq,"eq") ((:) (c_tok_opneq,"neq") ((:) 
        (c_tok_opge,"ge") ((:) (c_tok_oplt,"lt") ((:) (c_tok_opgt,"gt") ((:) (c_tok_opadd,"add") 
        ((:) (c_tok_opdash,"sub") ((:) (c_tok_opmul,"mul") ((:) (c_tok_opdiv,(:) 'd' 
        ((:) 'i' "v")) ((:) (c_tok_opperc,"rem") ((:) (c_tok_opcolon,(:) 'C' []) []))))))))))))));
    f_tok_binname a_tokid=f_tok_findname a_tokid c_tok_binnametable;
    f_m_pexpr_char a_thechar=F_Pexpr_char a_thechar;
    f_fpar_str_mk a_str=
        let { 
            f_fpar_str_mk_SWI_49 []=f_m_pexpr_UNION c_fparNILname c_m_pexpli_N;
            f_fpar_str_mk_SWI_49 (a_c:a_str_tl)=f_m_pexpr_UNION (f_tok_binname c_tok_opcolon) (f_m_pexpli_C (f_m_pexpr_char a_c) (f_m_pexpli_C (f_fpar_str_mk a_str_tl) c_m_pexpli_N))
         } in  f_fpar_str_mk_SWI_49 a_str;
    f_m_pdeftype a_name a_tparli a_rhs=F_Pdeftype a_name a_tparli a_rhs;
    f_fpardeftype_mk a_nam_pars a_rhs=
        let { 
            (r_tpname,r_tpars)=a_nam_pars
         } in  f_m_pdeftype r_tpname r_tpars a_rhs;
    f_fpardroptok a_tok=[];
    c_ptokfuns=(:) (c_tok_char,[],[]) ((:) (c_tok_int,[],[]) ((:) (c_tok_str,
        (:) '\o042' [],(:) '\o042' []) ((:) (c_tok_lowid,[],[]) ((:) (c_tok_TYPE,(:) 'T' 
        ((:) 'Y' "PE"),[]) ((:) (c_tok_FTYPE,"FTYPE",[]) ((:) (c_tok_LET,"LET",[]) ((:) (c_tok_IN,"IN",[]) 
        ((:) (c_tok_IF,"IF",[]) ((:) (c_tok_THEN,"THEN",[]) ((:) (c_tok_ELIF,"ELIF",[]) ((:) 
        (c_tok_ELSE,"ELSE",[]) ((:) (c_tok_FI,"FI",[]) ((:) (c_tok_CASE,"CASE",[]) ((:) (c_tok_uppid,[],[]) 
        ((:) (c_tok_opfunc,(:) '-' ((:) '>' []),[]) ((:) (c_tok_opdef,(:) '=' [],[]) 
        ((:) (c_tok_opapnd,(:) '+' ((:) '+' []),[]) ((:) (c_tok_opcolon,(:) ':' [],[]) 
        ((:) (c_tok_opand,(:) '&' [],[]) ((:) (c_tok_opsep,(:) '|' [],[]) ((:) 
        (c_tok_ople,(:) '<' ((:) '=' []),[]) ((:) (c_tok_opeq,(:) '=' ((:) '=' []),[]) 
        ((:) (c_tok_opge,(:) '>' ((:) '=' []),[]) ((:) (c_tok_oplt,(:) '<' [],[]) 
        ((:) (c_tok_opgt,(:) '>' [],[]) ((:) (c_tok_opadd,(:) '+' [],[]) ((:) 
        (c_tok_opdash,(:) '-' [],[]) ((:) (c_tok_opmul,(:) '*' [],[]) ((:) (c_tok_opdiv,
        (:) '/' [],[]) ((:) (c_tok_opperc,(:) '%' [],[]) ((:) (c_tok_optwig,(:) '~' [],[]) 
        ((:) (c_tok_btup,(:) '[' [],[]) ((:) (c_tok_etup,(:) ']' [],[]) ((:) 
        (c_tok_begin,(:) '{' [],[]) ((:) (c_tok_end,(:) '}' [],[]) ((:) (c_tok_obrack,
        (:) '(' [],[]) ((:) (c_tok_cbrack,(:) ')' [],[]) ((:) (c_tok_semi,(:) ';' [],[]) 
        ((:) (c_tok_comma,(:) ',' [],[]) ((:) (c_tok_hash,(:) '#' [],[]) ((:) 
        (c_tok_error,[],[]) ((:) (c_tok_EOF,"EOF",[]) []))))))))))))))))))))))))))))))))))))))))));
    f_printtok a_tok=f_do_printtok c_ptokfuns a_tok;
    f_printtoklist a_tokl=
        let { 
            f_printtoklist_SWI_108 []=[];
            f_printtoklist_SWI_108 (a_tok:a_tok_tl)=(++) (f_printtok a_tok) ((++) " " (f_printtoklist a_tok_tl))
         } in  f_printtoklist_SWI_108 a_tokl;
    f_fparerr a_errmes a_tokli=
        let { 
            f_fparerr_SWI_83 (a_tok:a_tokli_tl)=
                let { 
                    (r_tok_id,r_tokval,r_line)=a_tok
                 } in  (++) "line " ((++) (f_i2str r_line) ((++) ((:) ',' ((:) ' ' 
                    ((:) '\o042' []))) ((++) (f_first (25 :: Int) (f_printtoklist a_tokli)) ((++) ((:) '.' ((:) '.' 
                    ((:) '.' ((:) '\o042' ": ")))) ((++) a_errmes ((:) '\o012' []))))))
         } in  f_fparerr_SWI_83 a_tokli;
    f_fpargetchar a_tokval=
        let { 
            f_fpargetchar_SWI_81 (F_Tokv_ch a_i)=a_i
         } in  f_fpargetchar_SWI_81 a_tokval;
    f_fpargetint a_tokval=
        let { 
            f_fpargetint_SWI_80 (F_Tokv_i a_i)=a_i
         } in  f_fpargetint_SWI_80 a_tokval;
    f_fpargetname a_tokval=
        let { 
            f_fpargetname_SWI_79 (F_Tokv_str a_name)=a_name
         } in  f_fpargetname_SWI_79 a_tokval;
    f_fparoptoapp_left_do a_ope_li a_ope_ex=
        let { 
            f_fparoptoapp_left_do_SWI_39 []=a_ope_ex;
            f_fparoptoapp_left_do_SWI_39 (a_ope_li_hd:a_ope_li_tl)=
                let { 
                    (r_operat,r_ri_of_ri)=a_ope_li_hd
                 } in  f_m_pexpr_APPLY (f_m_pexpr_APPLY (f_m_pexpr_name (f_tok_binname r_operat)) (f_fparoptoapp_left_do a_ope_li_tl a_ope_ex)) r_ri_of_ri
         } in  f_fparoptoapp_left_do_SWI_39 a_ope_li;
    f_reverse_do a_li a_newli=
        let { 
            f_reverse_do_SWI_78 []=a_newli;
            f_reverse_do_SWI_78 (a_elem:a_li_tl)=f_reverse_do a_li_tl ((:) a_elem a_newli)
         } in  f_reverse_do_SWI_78 a_li;
    f_fparoptoapp_left a_ope=
        let { 
            (r_ope_ex,r_ope_li)=a_ope
         } in  f_fparoptoapp_left_do (f_reverse r_ope_li) r_ope_ex;
    f_isupper a_c=
        let { 
            r_ci=fromEnum a_c
         } in  
            if ((&&) (((>=) :: (Int -> Int -> Bool)) r_ci (fromEnum 'A')) (((<=) :: (Int -> Int -> Bool)) r_ci (fromEnum 'Z')))
            then True
            else 
                False;
    f_select a_n a_li=
        let { 
            f_select_SWI_85 (a_head:a_tail)=
                if (((==) :: (Int -> Int -> Bool)) a_n (1 :: Int))
                then a_head
                else 
                    (f_select (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_tail)
         } in  f_select_SWI_85 a_li;
    f_fparoptoapp_right a_ope=
        let { 
            f_fparoptoapp_right_SWI_36 []=r_ope_ex;
            f_fparoptoapp_right_SWI_36 (a_ope_li_hd:a_ope_li_tl)=
                let { 
                    r_rhs=f_fparoptoapp_right (r_ri_of_left,a_ope_li_tl);
                    r_operatname=f_tok_binname r_operat;
                    (r_operat,r_ri_of_left)=a_ope_li_hd
                 } in  
                    if (f_isupper (f_select (1 :: Int) r_operatname))
                    then (f_m_pexpr_UNION r_operatname (f_m_pexpli_C r_ope_ex (f_m_pexpli_C r_rhs c_m_pexpli_N)))
                    else 
                        (f_m_pexpr_APPLY (f_m_pexpr_APPLY (f_m_pexpr_name (f_tok_binname r_operat)) r_ope_ex) r_rhs);
            (r_ope_ex,r_ope_li)=a_ope
         } in  f_fparoptoapp_right_SWI_36 r_ope_li;
    f_fparsel1 a_a a_b=a_a;
    f_fparsel2 a_a a_b=a_b;
    f_fparttuplety_empty a_name=(a_name,c_m_ptypli_empty);
    f_fpartup2 a_a a_b=(a_a,a_b);
    f_m_pbindlhs_name a_name=F_Pbindlhs_name a_name;
    f_m_pbindlhs_pvl a_pvl=F_Pbindlhs_pvl a_pvl;
    c_m_pcelse_empty=C_Pcelse_empty;
    f_m_pcelse_expr a_expr=F_Pcelse_expr a_expr;
    f_m_pexpr_LET a_letbi a_expr=F_Pexpr_LET a_letbi a_expr;
    f_m_pexpr_TUPLE a_expli=F_Pexpr_TUPLE a_expli;
    f_m_pexpr_int a_i=F_Pexpr_int a_i;
    f_m_pftype a_fname a_typ=F_Pftype a_fname a_typ;
    f_m_pfunparli_C a_parname a_fpl=F_Pfunparli_C a_parname a_fpl;
    c_m_pfunparli_N=C_Pfunparli_N;
    f_m_pletbinds_C a_lhs a_expr a_plbli=F_Pletbinds_C a_lhs a_expr a_plbli;
    c_m_pletbinds_N=C_Pletbinds_N;
    c_m_ppatli_N=C_Ppatli_N;
    f_m_ppatvarli_C a_patnam a_pvl=F_Ppatvarli_C a_patnam a_pvl;
    c_m_ppatvarli_N=C_Ppatvarli_N;
    f_m_ppr_deftype a_defty=F_Ppr_deftype a_defty;
    f_m_ppr_ftype a_fty=F_Ppr_ftype a_fty;
    f_m_ppr_func a_fun=F_Ppr_func a_fun;
    f_m_pproglist a_part a_proli=F_Pproglist a_part a_proli;
    f_m_prhs_type a_typ=F_Prhs_type a_typ;
    f_m_prhs_union a_unili=F_Prhs_union a_unili;
    f_m_ptparlist a_parnam a_tpli=F_Ptparlist a_parnam a_tpli;
    c_m_ptparlist_empty=C_Ptparlist_empty;
    f_m_ptype_fun a_fty a_pty=F_Ptype_fun a_fty a_pty;
    f_m_ptype_tparname a_name=F_Ptype_tparname a_name;
    f_m_ptype_tuple a_typli=F_Ptype_tuple a_typli;
    f_m_ptypli a_typ a_typli=F_Ptypli a_typ a_typli;
    c_m_punion_empty=C_Punion_empty;
    f_paralt a_parli a_tokli=f_do_paralt C_Emess_uk a_parli a_tokli;
    f_parseq a_joinf a_parone a_partwo a_tokli=
        let { 
            f_parseq_SWI_20 (F_Par_fail a_mess)=F_Par_fail a_mess;
            f_parseq_SWI_20 (F_Par_succ a_valone a_moretok)=
                let { 
                    f_parseq_SWI_19 (F_Par_fail a_twomess)=F_Par_fail a_twomess;
                    f_parseq_SWI_19 (F_Par_succ a_valtwo a_yetmoretok)=F_Par_succ (a_joinf a_valone a_valtwo) a_yetmoretok
                 } in  f_parseq_SWI_19 (a_partwo a_moretok)
         } in  f_parseq_SWI_20 (a_parone a_tokli);
    f_parcvt a_cvtf a_patt a_tokli=
        let { 
            f_parcvt_SWI_12 (F_Par_fail a_mess)=F_Par_fail a_mess;
            f_parcvt_SWI_12 (F_Par_succ a_val a_tokli_tl)=F_Par_succ (a_cvtf a_val) a_tokli_tl
         } in  f_parcvt_SWI_12 (a_patt a_tokli);
    f_parfail a_errfun a_patt a_tokli=
        let { 
            f_parfail_SWI_16 (F_Par_fail a_mess)=f_parfail_SWI_15 a_mess;
            f_parfail_SWI_16 (F_Par_succ a_val a_tokli_tl)=r_parres;
            f_parfail_SWI_15 C_Emess_uk=f_parfail_SWI_14 a_tokli;
            f_parfail_SWI_15 (F_Emess_known a_reason a_line)=r_parres;
            f_parfail_SWI_14 (a_tok:a_tokli_tl)=
                let { 
                    (r_tok_id,r_tok_val,r_line)=a_tok
                 } in  F_Par_fail (F_Emess_known (a_errfun a_tokli) r_line);
            r_parres=a_patt a_tokli
         } in  f_parfail_SWI_16 r_parres;
    f_parrep_right a_join a_init a_par a_tokli=
        let { 
            f_parrep_right_SWI_28 (F_Par_fail a_mess)=F_Par_succ a_init a_tokli;
            f_parrep_right_SWI_28 (F_Par_succ a_val a_tokli_tl)=
                let { 
                    f_parrep_right_SWI_27 (F_Par_succ a_lival a_tokli_tltl)=F_Par_succ (a_join a_val a_lival) a_tokli_tltl
                 } in  f_parrep_right_SWI_27 (f_parrep_right a_join a_init a_par a_tokli_tl)
         } in  f_parrep_right_SWI_28 (a_par a_tokli);
    c_par_tparname=f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_hash) (f_parcvt f_m_ptype_tparname (f_parlit f_fpargetname c_tok_lowid));
    c_par_typelist=f_parseq f_m_ptypli c_par_type (f_parrep_right f_m_ptypli c_m_ptypli_empty (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_comma) c_par_type));
    c_par_type_sub=f_paralt ((:) (f_parseq f_m_ptype_parazd (f_parlit f_fpargetname c_tok_lowid) (f_parrep_right f_m_ptypli c_m_ptypli_empty c_par_simptype)) ((:) c_par_simptype 
        ((:) (f_parcvt f_m_ptype_tuple (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_btup) (f_parfail (f_fparerr c_err_tuptype) (f_parseq f_fparsel1 c_par_typelist 
        (f_parlit f_fpardroptok c_tok_etup))))) [])));
    c_par_type=f_paralt ((:) (f_parseq f_m_ptype_fun c_par_type_sub (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_opfunc) (f_parfail 
        (f_fparerr c_err_funtype) c_par_type))) ((:) c_par_type_sub []));
    c_par_simptype=f_paralt ((:) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_obrack) (f_parseq f_fparsel1 (f_parfail 
        (f_fparerr c_err_nesttype) c_par_type) (f_parfail (f_fparerr c_err_closbr) (f_parlit f_fpardroptok c_tok_cbrack)))) ((:) c_par_tparname ((:) (f_parcvt f_fpar_simptype_eparaz 
        (f_parlit f_fpargetname c_tok_lowid)) [])));
    c_par_ttuplety=f_paralt ((:) (f_parseq f_fpartup2 (f_parlit f_fpargetname c_tok_uppid) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_comma) c_par_typelist)) 
        ((:) (f_parcvt f_fparttuplety_empty (f_parlit f_fpargetname c_tok_uppid)) []));
    c_par_deftype_rhs=f_paralt ((:) (f_parcvt f_m_prhs_type c_par_type) ((:) (f_parcvt f_m_prhs_union (f_parseq f_fparsel2 
        (f_parlit f_fpardroptok c_tok_btup) (f_parfail (f_fparerr c_err_unitype) (f_parseq f_fparsel1 (f_parseq f_fpar_deftype_rhs_mk c_par_ttuplety (f_parrep_right f_fpar_deftype_rhs_mk c_m_punion_empty (f_parseq f_fparsel2 
        (f_parlit f_fpardroptok c_tok_opsep) c_par_ttuplety))) (f_parfail (f_fparerr c_err_endtup) (f_parlit f_fpardroptok c_tok_etup)))))) []));
    c_par_deftype=f_parseq f_fpardeftype_mk (f_parseq f_fpartup2 (f_parlit f_fpargetname c_tok_lowid) (f_parrep_right f_m_ptparlist c_m_ptparlist_empty (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_hash) 
        (f_parlit f_fpargetname c_tok_lowid)))) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_opdef) (f_parfail (f_fparerr c_err_rhstype) c_par_deftype_rhs));
    c_par_ftype=f_parseq f_m_pftype (f_parlit f_fpargetname c_tok_lowid) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_opdef) c_par_type);
    f_parrepplus_right a_join a_init a_par a_tokli=
        let { 
            f_parrepplus_right_SWI_31 (F_Par_fail a_mess)=F_Par_fail a_mess;
            f_parrepplus_right_SWI_31 (F_Par_succ a_val a_tokli_tl)=
                let { 
                    f_parrepplus_right_SWI_30 (F_Par_succ a_lival a_tokli_tltl)=F_Par_succ (a_join a_val a_lival) a_tokli_tltl
                 } in  f_parrepplus_right_SWI_30 (f_parrep_right a_join a_init a_par a_tokli_tl)
         } in  f_parrepplus_right_SWI_31 (a_par a_tokli);
    f_par_letlist_mk a_lhs_exp a_letli=
        let { 
            (r_letlhs,r_letexp)=a_lhs_exp
         } in  f_m_pletbinds_C r_letlhs r_letexp a_letli;
    c_par_take_apart=f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_oplt) (f_parseq f_fparsel1 (f_parrepplus_right f_m_ppatvarli_C c_m_ppatvarli_N (f_parlit f_fpargetname c_tok_lowid)) (f_parlit f_fpardroptok c_tok_opgt));
    c_tok_unnametable=(:) (c_tok_optwig,"not") ((:) (c_tok_opdash,"neg") []);
    f_tok_unname a_tokid=f_tok_findname a_tokid c_tok_unnametable;
    f_par_unopexpr_mk a_unop a_expr=f_m_pexpr_APPLY (f_m_pexpr_name (f_tok_unname a_unop)) a_expr;
    c_par_unop=f_paralt (f_map f_fpar_opermk ((:) c_tok_opdash ((:) c_tok_optwig [])));
    f_par_appexpr_mk_do a_appleft a_li=
        let { 
            f_par_appexpr_mk_do_SWI_41 []=a_appleft;
            f_par_appexpr_mk_do_SWI_41 (a_app_more:a_li_tl)=f_par_appexpr_mk_do (f_m_pexpr_APPLY a_appleft a_app_more) a_li_tl
         } in  f_par_appexpr_mk_do_SWI_41 a_li;
    f_par_appexpr_mk a_appli=
        let { 
            f_par_appexpr_mk_SWI_40 (a_app:a_li_tl)=f_par_appexpr_mk_do a_app a_li_tl
         } in  f_par_appexpr_mk_SWI_40 a_appli;
    f_paropt a_noval a_par a_tokli=
        let { 
            f_paropt_SWI_25 (F_Par_fail a_mess)=F_Par_succ a_noval a_tokli;
            f_paropt_SWI_25 a_paropt_SWI_25_non=r_parres;
            r_parres=a_par a_tokli
         } in  f_paropt_SWI_25 r_parres;
    c_par_pattern=f_parfail (f_fparerr c_err_CASEpatt) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_oplt) (f_parseq f_fpartup2 (f_parlit f_fpargetname c_tok_uppid) 
        (f_parseq f_fparsel1 (f_parrep_right f_m_ppatvarli_C c_m_ppatvarli_N (f_parlit f_fpargetname c_tok_lowid)) (f_parlit f_fpardroptok c_tok_opgt))));
    c_par_const=f_paralt ((:) (f_parcvt f_m_pexpr_int (f_parlit f_fpargetint c_tok_int)) ((:) (f_parcvt f_m_pexpr_char 
        (f_parlit f_fpargetchar c_tok_char)) ((:) (f_parcvt f_fpar_str_mk (f_parlit f_fpargetname c_tok_str)) [])));
    c_par_op0=f_paralt (f_map f_fpar_opermk ((:) c_tok_opmul ((:) c_tok_opdiv ((:) c_tok_opperc []))));
    c_par_op1=f_paralt (f_map f_fpar_opermk ((:) c_tok_opadd ((:) c_tok_opdash [])));
    c_par_op2=f_paralt (f_map f_fpar_opermk ((:) c_tok_ople ((:) c_tok_oplt ((:) c_tok_opeq ((:) c_tok_opneq 
        ((:) c_tok_opgt ((:) c_tok_opge [])))))));
    c_par_op3=f_paralt (f_map f_fpar_opermk ((:) c_tok_opsep ((:) c_tok_opand [])));
    c_par_op4=f_fpar_opermk c_tok_opcolon;
    c_par_ope=f_fpar_opermk c_tok_opapnd;
    c_par_bind=f_parseq f_fpartup2 (f_paralt ((:) (f_parcvt f_m_pbindlhs_name (f_parlit f_fpargetname c_tok_lowid)) ((:) 
        (f_parcvt f_m_pbindlhs_pvl c_par_take_apart) []))) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_opdef) c_par_expr);
    c_par_unopexpr=f_paralt ((:) (f_parseq f_par_unopexpr_mk c_par_unop (f_parfail (f_fparerr c_err_rhsunop) c_par_appexpr)) ((:) c_par_appexpr []));
    c_par_matchlist=f_parseq f_fpartup2 (f_parfail (f_fparerr c_err_CASE) (f_parrepplus_right f_fpar_pattli_mk c_m_ppatli_N c_par_match)) (f_paropt c_m_pcelse_empty (f_parseq f_fparsel2 
        (f_parlit f_fpardroptok c_tok_ELSE) (f_parfail (f_fparerr c_err_ELSE) (f_parcvt f_m_pcelse_expr c_par_expr))));
    c_par_match=f_parseq f_fpartup2 c_par_pattern (f_parseq f_fparsel1 c_par_expr (f_parfail (f_fparerr c_err_semicol) (f_parlit f_fpardroptok c_tok_semi)));
    c_par_case=f_parseq f_fpar_case_mk (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_CASE) (f_parfail (f_fparerr c_err_CASE) c_par_expr)) (f_parfail 
        (f_fparerr c_err_CASE) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_begin) (f_parseq f_fparsel1 c_par_matchlist (f_parfail (f_fparerr c_err_endCASE) (f_parlit f_fpardroptok c_tok_end)))));
    c_par_structor=f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_btup) (f_parfail (f_fparerr c_err_constr) (f_parseq f_fparsel1 (f_parseq f_m_pexpr_UNION 
        (f_parlit f_fpargetname c_tok_uppid) (f_paropt c_m_pexpli_N (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_comma) c_par_exprlist))) (f_parfail (f_fparerr c_err_enduni) (f_parlit f_fpardroptok c_tok_etup))));
    c_par_letlist=f_parfail (f_fparerr c_err_letlist) (f_parrepplus_right f_par_letlist_mk c_m_pletbinds_N (f_parseq f_fparsel1 c_par_bind (f_parfail (f_fparerr c_err_semicol) 
        (f_parlit f_fpardroptok c_tok_semi))));
    c_par_ifexpr=f_parseq f_fpar_ifexpr_mk (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_IF) (f_parfail (f_fparerr c_err_FI) (f_parseq f_fpartup2 c_par_expr 
        (f_parfail (f_fparerr c_err_THEN) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_THEN) (f_parseq f_fpartup2 c_par_expr (f_parrep_right (:) [] c_par_elifpart))))))) (f_parfail 
        (f_fparerr c_err_ELSE) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_ELSE) (f_parseq f_fparsel1 c_par_expr (f_parfail (f_fparerr c_err_FI) (f_parlit f_fpardroptok c_tok_FI)))));
    c_par_exprlist=f_parfail (f_fparerr c_err_explist) (f_parcvt f_fpar_exprlist_mk (f_parseq (:) c_par_expr (f_parrep_right (:) [] (f_parseq f_fparsel2 
        (f_parlit f_fpardroptok c_tok_comma) c_par_expr))));
    c_par_expr=f_paralt ((:) (f_parseq f_m_pexpr_LET (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_LET) c_par_letlist) (f_parseq f_fparsel2 
        (f_parlit f_fpardroptok c_tok_IN) c_par_expr)) ((:) c_par_opexpr []));
    c_par_opexpr4=f_parcvt f_fparoptoapp_right (f_parseq f_fpartup2 c_par_opexpr3 (f_parrep_right (:) [] (f_parseq f_fpartup2 c_par_op4 (f_parfail (f_fparerr c_err_rhsop) c_par_opexpr3))));
    c_par_opexpr3=f_parcvt f_fparoptoapp_left (f_parseq f_fpartup2 c_par_opexpr2 (f_parrep_right (:) [] (f_parseq f_fpartup2 c_par_op3 (f_parfail (f_fparerr c_err_rhsop) c_par_opexpr2))));
    c_par_opexpr2=f_parcvt f_fparoptoapp_left (f_parseq f_fpartup2 c_par_opexpr1 (f_parrep_right (:) [] (f_parseq f_fpartup2 c_par_op2 (f_parfail (f_fparerr c_err_rhsop) c_par_opexpr1))));
    c_par_opexpr1=f_parcvt f_fparoptoapp_left (f_parseq f_fpartup2 c_par_opexpr0 (f_parrep_right (:) [] (f_parseq f_fpartup2 c_par_op1 (f_parfail (f_fparerr c_err_rhsop) c_par_opexpr0))));
    c_par_opexpr0=f_parcvt f_fparoptoapp_left (f_parseq f_fpartup2 c_par_unopexpr (f_parrep_right (:) [] (f_parseq f_fpartup2 c_par_op0 (f_parfail (f_fparerr c_err_rhsop) c_par_unopexpr))));
    c_par_opexpr=f_parcvt f_fparoptoapp_left (f_parseq f_fpartup2 c_par_opexpr4 (f_parrep_right (:) [] (f_parseq f_fpartup2 c_par_ope c_par_opexpr4)));
    c_par_elifpart=f_parseq f_fpartup2 (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_ELIF) c_par_expr) (f_parfail (f_fparerr c_err_THENELIF) (f_parseq f_fparsel2 
        (f_parlit f_fpardroptok c_tok_THEN) c_par_expr));
    c_par_tuplctor=f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_btup) (f_parseq f_fparsel1 (f_parcvt f_m_pexpr_TUPLE c_par_exprlist) (f_parfail (f_fparerr c_err_endtupl) 
        (f_parlit f_fpardroptok c_tok_etup)));
    c_par_simexpr=f_paralt ((:) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_obrack) (f_parfail (f_fparerr c_err_nest) 
        (f_parseq f_fparsel1 c_par_expr (f_parfail (f_fparerr c_err_cbrack) (f_parlit f_fpardroptok c_tok_cbrack))))) ((:) c_par_structor ((:) c_par_tuplctor ((:) c_par_ifexpr 
        ((:) c_par_case ((:) (f_parcvt f_m_pexpr_name (f_parlit f_fpargetname c_tok_lowid)) ((:) c_par_const [])))))));
    c_par_appexpr=f_parcvt f_par_appexpr_mk (f_parrepplus_right (:) [] c_par_simexpr);
    c_par_function=f_parseq f_fpar_function_mk (f_parseq f_fpartup2 (f_parlit f_fpargetname c_tok_lowid) (f_parrep_right f_m_pfunparli_C c_m_pfunparli_N (f_parlit f_fpargetname c_tok_lowid))) (f_parfail 
        (f_fparerr c_err_funcdef) (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_opdef) (f_parseq f_fparsel1 c_par_expr (f_parfail (f_fparerr c_err_semicol) (f_parlit f_fpardroptok c_tok_semi)))));
    c_par_progpart=f_paralt ((:) (f_parcvt f_m_ppr_deftype (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_TYPE) (f_parfail 
        (f_fparerr c_err_typedef) (f_parseq f_fparsel1 c_par_deftype (f_parfail (f_fparerr c_err_semicol) (f_parlit f_fpardroptok c_tok_semi)))))) ((:) (f_parcvt f_m_ppr_ftype 
        (f_parseq f_fparsel2 (f_parlit f_fpardroptok c_tok_FTYPE) (f_parfail (f_fparerr c_err_funtyp) (f_parseq f_fparsel1 c_par_ftype (f_parfail (f_fparerr c_err_semicol) 
        (f_parlit f_fpardroptok c_tok_semi)))))) ((:) (f_parcvt f_m_ppr_func c_par_function) [])));
    c_par_proglist=f_paralt ((:) (f_parseq f_m_pproglist c_par_progpart c_par_proglist) ((:) (f_parfail (f_fparerr c_err_progterm) 
        (f_parcvt f_fpar_prog_mkempty (f_parlit f_fpardroptok c_tok_EOF))) []));
    f_tokenize a_theprog=f_do_tokenize a_theprog (1 :: Int);
    f_benchmark_main a_n=
        let { 
            f_main_SWI_1 (F_Par_fail a_mess)=f_main_SWI_0 a_mess;
            f_main_SWI_1 (F_Par_succ a_attval a_pr_tl)="Program correctly parsed!\n";
            f_main_SWI_0 C_Emess_uk="Unknown error\n";
            f_main_SWI_0 (F_Emess_known a_kmess a_line)=a_kmess
         } in  (++) (f_sumcode (f_concat (f_map ((.) ((.) f_main_SWI_1 c_par_proglist) f_tokenize) [c_the_program|a_i<-
            [(1 :: Int)..a_n]]))) "\n";
-- TEMP: when testing
--    c_the_program = "main ip";
      c_the_program=(++) "main ip =\n" ((++) "  i2str (optim (myMain deciem))\n" ((++) ";\n" ((++) "\n" ((++) "TYPE tJobdef    = [ JOBDEF, int, int, int, tJobdef, tJobdef ] ;\n" ((++) "TYPE tJobstat   = [ JOBSTAT, int, int, int, int, tJobdef ] ;\n" 
        ((++) "TYPE tTree      = [ LEAF, int |\n" ((++) "                    TREE, tTree, tTree ] ;\n" ((++) "TYPE tProc      = [ PROC, int, tJobstat ] ;\n" ((++) "\n" ((++) "\n" ((++) "\n" ((++) "emptyjobdef     = [JOBDEF, 0     , 0 , 0, emptyjobdef, emptyjobdef] ;\n" 
        ((++) "solo            = [JOBDEF, 1     , 0, 10, emptyjobdef, emptyjobdef] ;\n" ((++) "trio            = [JOBDEF, 1    , 10, 20     , child1     , child2] ;\n" ((++) "\n" ((++) "child1          = [JOBDEF, 2     , 0, 30, emptyjobdef, emptyjobdef] ;\n" ((++) "child2          = [JOBDEF, 3     , 0, 10, emptyjobdef, emptyjobdef] ;\n" ((++) "\n" ((++) "septiem         = [JOBDEF, 1    , 20, 20, job2    , job3              ] ;\n" 
        ((++) "\n" ((++) "job2            = [JOBDEF, 2    , 19, 10, job4    , job5              ] ;\n" ((++) "job3            = [JOBDEF, 3    , 18, 30, job6    , job7              ] ;\n" ((++) "job4            = [JOBDEF, 4     , 0, 38, emptyjobdef, emptyjobdef] ;\n" ((++) "job5            = [JOBDEF, 5     , 0, 50, emptyjobdef, emptyjobdef] ;\n" ((++) "job6            = [JOBDEF, 6     , 0, 10, emptyjobdef, emptyjobdef] ;\n" ((++) "job7            = [JOBDEF, 7     , 0, 40, emptyjobdef, emptyjobdef] ;\n" 
        ((++) "\n" ((++) "deciem          = [JOBDEF, 1    , 20, 20, job12   , job13      ] ;\n" ((++) "\n" ((++) "job12           = [JOBDEF, 2    , 19, 10, job14   , job15      ] ;\n" ((++) "job13           = [JOBDEF, 3    , 18, 30, job16   , job17      ] ;\n" ((++) "job14           = [JOBDEF, 4    , 17, 15, job18   , job19      ] ;\n" ((++) "job15           = [JOBDEF, 5     , 0, 50, emptyjobdef, emptyjobdef] ;\n" 
        ((++) "job16           = [JOBDEF, 6     , 0, 10, emptyjobdef, emptyjobdef] ;\n" ((++) "job17           = [JOBDEF, 7     , 0, 40, emptyjobdef, emptyjobdef] ;\n" ((++) "job18           = [JOBDEF, 8     , 0, 30, emptyjobdef, emptyjobdef] ;\n" ((++) "job19           = [JOBDEF, 9     , 0, 60, emptyjobdef, emptyjobdef] ;\n" ((++) "\n" ((++) "duodeciem       = [JOBDEF, 101  , 20, 20, job102          , job103     ] ;\n" ((++) "\n" 
        ((++) "job102          = [JOBDEF, 102  , 19, 10, job104          , job105     ] ;\n" ((++) "job103          = [JOBDEF, 103  , 18, 30, job106          , job107     ] ;\n" ((++) "job104          = [JOBDEF, 104  , 17, 15, job108          , job109     ] ;\n" ((++) "job105          = [JOBDEF, 105  , 25, 12, job110          , job111     ] ;\n" ((++) "job106          = [JOBDEF, 106   , 0, 10, emptyjobdef, emptyjobdef] ;\n" ((++) "job107          = [JOBDEF, 107   , 0, 40, emptyjobdef, emptyjobdef] ;\n" ((++) "job108          = [JOBDEF, 108   , 0, 30, emptyjobdef, emptyjobdef] ;\n" 
        ((++) "job109          = [JOBDEF, 109   , 0, 60, emptyjobdef, emptyjobdef] ;\n" ((++) "job110          = [JOBDEF, 110   , 0, 30, emptyjobdef, emptyjobdef] ;\n" ((++) "job111          = [JOBDEF, 111   , 0, 60, emptyjobdef, emptyjobdef] ;\n" ((++) "\n" ((++) "quadrideciem    = [JOBDEF, 201  , 20, 20, job202          , job203     ] ;\n" ((++) "\n" ((++) "job202          = [JOBDEF, 202  , 19, 10, job212          , job213     ] ;\n" 
        ((++) "job203          = [JOBDEF, 203  , 18, 30, job204          , job205     ] ;\n" ((++) "job204          = [JOBDEF, 204  , 17, 15, job207          , job208     ] ;\n" ((++) "job205          = [JOBDEF, 205  , 25, 12, job210          , job211     ] ;\n" ((++) "job206          = [JOBDEF, 206   , 0, 10, emptyjobdef, emptyjobdef] ;\n" ((++) "job207          = [JOBDEF, 207   , 0, 40, emptyjobdef, emptyjobdef] ;\n" ((++) "job208          = [JOBDEF, 208  , 19, 10, job206          , job209     ] ;\n" ((++) "job209          = [JOBDEF, 209   , 0, 60, emptyjobdef, emptyjobdef] ;\n" 
        ((++) "job210          = [JOBDEF, 210   , 0, 30, emptyjobdef, emptyjobdef] ;\n" ((++) "job211          = [JOBDEF, 211   , 0, 60, emptyjobdef, emptyjobdef] ;\n" ((++) "job212          = [JOBDEF, 212   , 0, 28, emptyjobdef, emptyjobdef] ;\n" ((++) "job213          = [JOBDEF, 213   , 0, 50, emptyjobdef, emptyjobdef] ;\n" ((++) "\n" ((++) "/*  Job administration */\n" ((++) "emptyjobstat    = [JOBSTAT, 0, 0, 0 , 0, emptyjobdef] ;\n" 
        ((++) "\n" ((++) "/*  Processor definitions */\n" ((++) "procs =\n" ((++) "  [PROC, 1, emptyjobstat] :\n" ((++) "  [PROC, 2, emptyjobstat] :\n" ((++) "  [N]\n" ((++) ";\n" 
        ((++) "\n" ((++) "\n" ((++) "/*  Main expression */\n" ((++) "myMain root =\n" ((++) "  LET\n" ((++) "    rootadm = ((addch root  0):[N]) ;\n" ((++) "    totcyc  = 0 ;\n" 
        ((++) "    level   = 0 ;\n" ((++) "  IN\n" ((++) "    alloc rootadm procs [N] [N] totcyc level\n" ((++) ";\n" ((++) "\n" ((++) "\n" ((++) "/*  Job allocator */\n" 
        ((++) "/*  */\n" ((++) "\n" ((++) "/*  No more processes to allocate: */\n" ((++) "/*  */\n" ((++) "alloc jobnew prlist jobold prold totcyc level =\n" ((++) "  CASE prlist {\n" ((++) "    <N>\n" 
        ((++) "        process (append jobold jobnew) prold totcyc level ;\n" ((++) "    <C pr prnew>\n" ((++) "        IF (null jobnew) THEN\n" ((++) "                  /*  There are no jobs to allocate to processor `pr' */\n" ((++) "                  /*  try the next processor: */\n" ((++) "          (alloc jobold prnew [N] (pr:prold) totcyc level)\n" ((++) "        ELSE\n" 
        ((++) "                        /*  Try to allocate jobs to processors until\n" ((++) "                                either all processors */\n" ((++) "                        /*  are active, or the processors which are still\n" ((++) "                                available cannot take up */\n" ((++) "                        /*  the (join) jobs left over: */\n" ((++) "          CASE pr {\n" ((++) "            <PROC pid jstat>\n" 
        ((++) "              CASE jstat {\n" ((++) "                <JOBSTAT jb dum1 dum2 dum3 dum4>\n" ((++) "                  LET\n" ((++) "                    <jobold1 jobnew1> = anyjob jobnew jobold pid ;\n" ((++) "                    job1     = head jobnew1 ;\n" ((++) "                    jobrest1 = tail jobnew1 ;\n" ((++) "                    <jobold2 jobnew2> = anyjob jobrest1 (job1:jobold1) pid ;\n" 
        ((++) "                    nextlevel = level + 1 ;\n" ((++) "                  IN\n" ((++) "                    IF (jb ~= 0) THEN   /* processor busy */\n" ((++) "                        (alloc (append jobold jobnew) prnew [N]\n" ((++) "                               (pr:prold) totcyc level)\n" ((++) "                    ELIF (null jobnew1) THEN    /* no suitable job */\n" ((++) "                        (alloc (append jobold jobnew) prnew [N]\n" 
        ((++) "                               (pr:prold) totcyc level)\n" ((++) "                    ELIF (null jobnew2) THEN    /* no suitable next-job */\n" ((++) "                        (alloc (append jobold1 jobrest1) prnew [N]\n" ((++) "                               ([PROC, pid, job1]:prold) totcyc level)\n" ((++) "                    ELSE [TREE, (alloc (append jobold1 jobrest1) prnew [N]\n" ((++) "                                  ([PROC, pid, job1]:prold) totcyc nextlevel),\n" ((++) "                                (alloc jobnew2 (pr:prnew) jobold2\n" 
        ((++) "                                       prold totcyc nextlevel) ]\n" ((++) "                    FI ;\n" ((++) "              } ;\n" ((++) "          }\n" ((++) "      FI ;\n" ((++) "  }\n" ((++) ";\n" 
        ((++) "\n" ((++) "/*  scan along the list jobold++jobnew to find a job that can run on pid */\n" ((++) "/*  and deliver the pair jobold1,jobnew1 such that the disired job is the */\n" ((++) "/*  hd of jobnew1 and the jobs hot yet scanned are IN the tl of jobnew1 */\n" ((++) "/*  All jobs already scanned are IN jobold1 */\n" ((++) "\n" ((++) "anyjob joblist jobold pid  =\n" 
        ((++) "  CASE joblist {\n" ((++) "    <N>\n" ((++) "      [jobold, [N]] ;\n" ((++) "    <C job jobnew>\n" ((++) "      CASE job {\n" ((++) "        <JOBSTAT mark steps proc dum1 dum2>\n" ((++) "          IF ( (mark == 1) | (mark == 4) &\n" 
        ((++) "               ((pid == proc) | (proc == 0)) ) THEN\n" ((++) "            [jobold, (job : jobnew)]\n" ((++) "          ELSE\n" ((++) "            (anyjob jobnew (job : jobold) pid)\n" ((++) "          FI ;\n" ((++) "      } ;\n" ((++) "  }\n" 
        ((++) ";\n" ((++) "\n" ((++) "/*  Update the job statuses now allocation is complete */\n" ((++) "/*  IF there is no more work to be done, THEN allocation terminates, */\n" ((++) "/*  otherwise the next level of allocation is started: */\n" ((++) "/*  */\n" ((++) "\n" 
        ((++) "process jobs procss totcyc level =\n" ((++) "  LET\n" ((++) "    s                   = mincyc procss ;\n" ((++) "    <prterm prnew>      = perform procss s ;\n" ((++) "    jobnew              = addjob prterm jobs ;\n" ((++) "  IN\n" ((++) "    IF (s == 0) THEN\n" 
        ((++) "      [LEAF, totcyc]\n" ((++) "    ELSE\n" ((++) "      (alloc jobnew  prnew [N] [N] (totcyc + s) level)\n" ((++) "    FI\n" ((++) ";\n" ((++) "\n" ((++) "\n" 
        ((++) "\n" ((++) "/*  Find the minimum number of steps to be done IN parallel: */\n" ((++) "/*  */\n" ((++) "\n" ((++) "/*  Skip idle processor: */\n" ((++) "/*  */\n" ((++) "  mincyc proclist =\n" 
        ((++) "    CASE proclist {\n" ((++) "      <N>\n" ((++) "        0 ;\n" ((++) "      <C proc prest>\n" ((++) "        CASE proc {\n" ((++) "          <PROC dum1 jobstat>\n" ((++) "            CASE jobstat {\n" 
        ((++) "              <JOBSTAT mark steps dum2 dum3 dum4>\n" ((++) "                  IF (mark == 0) THEN (mincyc prest)\n" ((++) "                  ELSE (min steps (mincyc prest))\n" ((++) "                  FI ;\n" ((++) "            } ;\n" ((++) "        } ;\n" ((++) "     }\n" 
        ((++) "   ;\n" ((++) "\n" ((++) "\n" ((++) "min x y =\n" ((++) "  IF x == 0 THEN y\n" ((++) "  ELIF y == 0 THEN x\n" ((++) "  ELIF x < y THEN x\n" 
        ((++) "  ELSE y\n" ((++) "  FI\n" ((++) ";\n" ((++) "\n" ((++) "/*  Add a job to the administration and perform acknowledgements of terminating */\n" ((++) "/*  child jobs to their parents: */\n" ((++) "/*  */\n" 
        ((++) "\n" ((++) "   addjob proclist          jobs =\n" ((++) "     CASE proclist {\n" ((++) "       <N>\n" ((++) "         jobs ;\n" ((++) "       <C proc prest>\n" ((++) "         CASE proc {\n" 
        ((++) "           <PROC pid jobstat>\n" ((++) "           CASE jobstat {\n" ((++) "             <JOBSTAT mark steps dum1 parent jdef>\n" ((++) "             CASE jdef {\n" ((++) "             <JOBDEF jid fs js ch1 ch2>\n" ((++) "              IF steps == 0 THEN\n" ((++) "                IF mark == 1 THEN\n" 
        ((++) "                  [JOBSTAT, 2, js, pid, parent, jdef] :\n" ((++) "                  (addch ch1 jid) :\n" ((++) "                  (addch ch2 jid) :\n" ((++) "                  (addjob prest jobs)\n" ((++) "                ELSE\n" ((++) "                  (addjob prest (ackn parent jobs))\n" ((++) "                FI\n" 
        ((++) "              ELSE\n" ((++) "                [N]\n" ((++) "              FI ; } ;\n" ((++) "           } ;\n" ((++) "         } ;\n" ((++) "      }\n" ((++) "    ;\n" 
        ((++) "\n" ((++) "\n" ((++) "/*  Perform a number of steps, such that at least one processor becomes */\n" ((++) "/*  available: */\n" ((++) "/*  */\n" ((++) "\n" ((++) "   perform proclist          s =\n" 
        ((++) "     CASE proclist {\n" ((++) "       <N>\n" ((++) "         [ [N], [N] ] ;\n" ((++) "       <C proc prest>\n" ((++) "         CASE proc {\n" ((++) "         <PROC pid jobstat>\n" ((++) "           CASE jobstat {\n" 
        ((++) "             <JOBSTAT mark steps pr par job>\n" ((++) "             LET\n" ((++) "               <prterm prnew> = perform prest s ;\n" ((++) "             IN\n" ((++) "              IF (mark == 0) THEN\n" ((++) "                [ prterm , [PROC, pid, emptyjobstat]:prnew ]\n" ((++) "              ELIF (steps == s) THEN\n" 
        ((++) "                [([PROC, pid, [JOBSTAT, mark, 0, pr, par, job]]:prterm),\n" ((++) "                 ([PROC, pid, emptyjobstat]:prnew)]\n" ((++) "              ELSE\n" ((++) "                [ prterm,\n" ((++) "                 [ PROC, pid, [JOBSTAT,mark,(steps-s),pr,par,job]]: prnew ]\n" ((++) "              FI ;\n" ((++) "            } ;\n" 
        ((++) "        } ;\n" ((++) "    }\n" ((++) "  ;\n" ((++) "\n" ((++) "\n" ((++) "/*  Add children: */\n" ((++) "/*  A non forking job is treated as a join job, plus 2 acknowledges, with */\n" 
        ((++) "/*  processor 0. */\n" ((++) "\n" ((++) "addch jobdef parent =\n" ((++) "  CASE jobdef {\n" ((++) "    <JOBDEF jid fs js ch1 ch2>\n" ((++) "      IF (fs == 0) THEN\n" ((++) "        [JOBSTAT, 4, js, 0, parent, jobdef]\n" 
        ((++) "      ELSE\n" ((++) "        [JOBSTAT, 1, fs, 0, parent, jobdef]\n" ((++) "      FI ;\n" ((++) "  }\n" ((++) ";\n" ((++) "\n" ((++) " \n" 
        ((++) "/*  Process acknowledgements: */\n" ((++) "\n" ((++) "ackn parent joblist =\n" ((++) "  CASE joblist {\n" ((++) "    <N>\n" ((++) "      [N] ;\n" ((++) "    <C job jobrest>\n" 
        ((++) "      CASE job {\n" ((++) "        <JOBSTAT mark js pid par jdef>\n" ((++) "          CASE jdef {\n" ((++) "            <JOBDEF jid dum1 dum2 dum3 dum4>\n" ((++) "              IF (parent == jid) THEN\n" ((++) "                [ JOBSTAT, (mark+1), js, pid, par, jdef] : jobrest\n" ((++) "              ELSE\n" 
        ((++) "                (job : ackn parent jobrest)\n" ((++) "              FI ;\n" ((++) "          } ;\n" ((++) "      } ;\n" ((++) "  }\n" ((++) ";\n" ((++) "\n" 
        ((++) "optim tree =\n" ((++) "  CASE tree {\n" ((++) "    <LEAF l>\n" ((++) "      l ;\n" ((++) "    <TREE l r>\n" ((++) "      min (optim l) (optim r) ;\n" ((++) "  }\n" 
        ((++) ";\n" "\n")))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let { 
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
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
    c_input=(40 :: Int);
    main = putStr (f_main c_input)
}
