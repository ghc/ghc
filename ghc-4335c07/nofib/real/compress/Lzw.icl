MODULE Lzw;

IMPORT deltaC,deltaI,deltaIO;

TYPE

::  PrefixTree -> PT PrefixElement PrefixTree PrefixTree | PTNil;

::  PrefixElement -> PTE CHAR INT PrefixTree;

MACRO

    Max_entries -> 4096;

RULE

    Create_code_table -> Create_code_table2 0 256;
    
        Create_code_table2 first_code 0 -> PTNil;
        Create_code_table2 first_code 1 -> PT (PTE (ITOC first_code) first_code PTNil) PTNil PTNil;
        Create_code_table2 first_code n_codes
        ->  PT (PTE (ITOC m_code) m_code PTNil) left right,
            left: Create_code_table2 first_code (- m_code first_code),
            right: Create_code_table2 m_code2: (++ m_code) (- (+ first_code n_codes) m_code2),
            m_code: SHIFTR% (+ first_code (-- (+ first_code n_codes))) 1;

    Lzw_code_file [] code_table next_code
    ->  [];
    Lzw_code_file input code_table next_code
    ->  [!n | Lzw_code_file input2 code_table2 (++ next_code)],
        (input2,n,code_table2): Code_string input 0 next_code code_table;

::  Code_string [CHAR] INT INT PrefixTree -> (![CHAR],!INT,!PrefixTree);

    Code_string input: [c | input2] old_code next_code (PT p: (PTE k v t) l r)
    ->  F1 r1 p r, IF <C c k
    ->  F2 r2 p l, IF >C c k
    ->  F3 r3 k v l r, IF =C c k,
        r1: Code_string input old_code next_code l,
        r2: Code_string input old_code next_code r,
        r3: Code_string input2 v next_code t;
<<
    Code_string input: [c | input2] old_code next_code (PT p: (PTE k v t) l r)
    ->  (input_l,nl,PT p l' r), IF <C c k
    ->  (input_r,nr,PT p l r'), IF >C c k
    ->  (input',n,PT (PTE k v t') l r), IF =C c k,
        (input_l,nl,l'): Code_string input old_code next_code l,
        (input_r,nr,r'): Code_string input old_code next_code r,
        (input',n,t'): Code_string input2 v next_code t;
>>
    Code_string input: [c | input_file2] old_code next_code PTNil
    ->  (input, old_code, PTNil), IF >= next_code Max_entries
    ->  (input, old_code, PT (PTE c next_code PTNil) PTNil PTNil);
    Code_string [] old_code next_code code_table
    ->  ([], old_code, PTNil);

    F1 (input_l,nl,l') p r -> (input_l,nl,PT p l' r);
    F2 (input_r,nr,r') p l -> (input_r,nr,PT p l r');
    F3 (input',n,t') k v l r -> (input',n,PT (PTE k v t') l r);

    File_to_list input_file
    ->  [], IF IsEndOfFile input_file
    ->  [!c | File_to_list input_file2],
        (c,input_file2): FGetC input_file;

    Code_list_to_file [code | l] output_file
    ->  Code_list_to_file2 code l output_file2,
        output_file2: FPutC (ITOC (SHIFTR% code 4)) output_file;
    Code_list_to_file [] output_file
    ->  output_file;

   Code_list_to_file2 c [code | l] output_file
    ->  Code_list_to_file l output_file3,
        output_file3: FPutC (ITOC code) output_file2,
        c2: OR% (AND% 15 (SHIFTR% code 8)) (AND% 240 (SHIFTL% c 4)),
        output_file2: FPutC (ITOC c2) output_file;
    Code_list_to_file2 c [] output_file
    ->  FPutC (ITOC (SHIFTL% c 4)) output_file;

    Start
    ->  Code_list_to_file code_list output_file,
        code_list: Lzw_code_file (File_to_list input_file) Create_code_table 256,
        input_file: StdIn,
        output_file: StdOut;
<<
partain: original
    Start
    ->  Code_list_to_file code_list output_file,
        code_list: Lzw_code_file (File_to_list input_file) Create_code_table 256,
        input_file: FOpen input_file_name "r",
        output_file: FOpen output_file_name "w",
        input_file_name: "csh.man",
        output_file_name: "csh.man.z";
>>
