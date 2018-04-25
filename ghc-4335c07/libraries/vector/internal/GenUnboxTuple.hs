{-# LANGUAGE ParallelListComp #-}
module Main where

import Text.PrettyPrint

import System.Environment ( getArgs )

main = do
         [s] <- getArgs
         let n = read s
         mapM_ (putStrLn . render . generate) [2..n]

generate :: Int -> Doc
generate n =
  vcat [ text "#ifdef DEFINE_INSTANCES"
       , data_instance "MVector s" "MV"
       , data_instance "Vector" "V"
       , class_instance "Unbox"
       , class_instance "M.MVector MVector" <+> text "where"
       , nest 2 $ vcat $ map method methods_MVector
       , class_instance "G.Vector Vector" <+> text "where"
       , nest 2 $ vcat $ map method methods_Vector
       , text "#endif"
       , text "#ifdef DEFINE_MUTABLE"
       , define_zip "MVector s" "MV"
       , define_unzip "MVector s" "MV"
       , text "#endif"
       , text "#ifdef DEFINE_IMMUTABLE"
       , define_zip "Vector" "V"
       , define_zip_rule
       , define_unzip "Vector" "V"
       , text "#endif"
       ]

  where
    vars  = map char $ take n ['a'..]
    varss = map (<> char 's') vars
    tuple xs = parens $ hsep $ punctuate comma xs
    vtuple xs = parens $ sep $ punctuate comma xs
    con s = text s <> char '_' <> int n
    var c = text (c : "_")

    data_instance ty c
      = hang (hsep [text "data instance", text ty, tuple vars])
             4
             (hsep [char '=', con c, text "{-# UNPACK #-} !Int"
                   , vcat $ map (\v -> char '!' <> parens (text ty <+> v)) vars])

    class_instance cls
      = text "instance" <+> vtuple [text "Unbox" <+> v | v <- vars]
                        <+> text "=>" <+> text cls <+> tuple vars


    define_zip ty c
      = sep [text "-- | /O(1)/ Zip" <+> int n <+> text "vectors"
            ,name <+> text "::"
                  <+> vtuple [text "Unbox" <+> v | v <- vars]
                  <+> text "=>"
                  <+> sep (punctuate (text " ->") [text ty <+> v | v <- vars])
                  <+> text "->"
                  <+> text ty <+> tuple vars
             ,text "{-# INLINE_FUSED"  <+> name <+> text "#-}"
             ,name <+> sep varss
                   <+> text "="
                   <+> con c
                   <+> text "len"
                   <+> sep [parens $ text "unsafeSlice"
                                     <+> char '0'
                                     <+> text "len"
                                     <+> vs | vs <- varss]
             ,nest 2 $ hang (text "where")
                            2
                     $ text "len ="
                       <+> sep (punctuate (text " `delayed_min`")
                                          [text "length" <+> vs | vs <- varss])
             ]
      where
        name | n == 2    = text "zip"
             | otherwise = text "zip" <> int n

    define_zip_rule
      = hang (text "{-# RULES" <+> text "\"stream/" <> name "zip"
              <> text " [Vector.Unboxed]\" forall" <+> sep varss <+> char '.')
             2 $
             text "G.stream" <+> parens (name "zip" <+> sep varss)
             <+> char '='
             <+> text "Bundle." <> name "zipWith" <+> tuple (replicate n empty)
             <+> sep [parens $ text "G.stream" <+> vs | vs <- varss]
             $$ text "#-}"
     where
       name s | n == 2    = text s
              | otherwise = text s <> int n
       

    define_unzip ty c
      = sep [text "-- | /O(1)/ Unzip" <+> int n <+> text "vectors"
            ,name <+> text "::"
                  <+> vtuple [text "Unbox" <+> v | v <- vars]
                  <+> text "=>"
                  <+> text ty <+> tuple vars
                  <+> text "->" <+> vtuple [text ty <+> v | v <- vars]
            ,text "{-# INLINE" <+> name <+> text "#-}"
            ,name <+> pat c <+> text "="
                  <+> vtuple varss
            ]
      where
        name | n == 2    = text "unzip"
             | otherwise = text "unzip" <> int n

    pat c = parens $ con c <+> var 'n' <+> sep varss
    patn c n = parens $ con c <+> (var 'n' <> int n)
                              <+> sep [v <> int n | v <- varss]

    qM s = text "M." <> text s
    qG s = text "G." <> text s

    gen_length c _ = (pat c, var 'n')

    gen_unsafeSlice mod c rec
      = (var 'i' <+> var 'm' <+> pat c,
         con c <+> var 'm'
               <+> vcat [parens
                         $ text mod <> char '.' <> text rec
                                    <+> var 'i' <+> var 'm' <+> vs
                                        | vs <- varss])


    gen_overlaps rec = (patn "MV" 1 <+> patn "MV" 2,
                        vcat $ r : [text "||" <+> r | r <- rs])
      where
        r : rs = [qM rec <+> v <> char '1' <+> v <> char '2' | v <- varss]

    gen_unsafeNew rec
      = (var 'n',
         mk_do [v <+> text "<-" <+> qM rec <+> var 'n' | v <- varss]
               $ text "return $" <+> con "MV" <+> var 'n' <+> sep varss)

    gen_unsafeReplicate rec
      = (var 'n' <+> tuple vars,
         mk_do [vs <+> text "<-" <+> qM rec <+> var 'n' <+> v
                        | v  <- vars | vs <- varss]
               $ text "return $" <+> con "MV" <+> var 'n' <+> sep varss)

    gen_unsafeRead rec
      = (pat "MV" <+> var 'i',
         mk_do [v <+> text "<-" <+> qM rec <+> vs <+> var 'i' | v  <- vars
                                                              | vs <- varss]
               $ text "return" <+> tuple vars)

    gen_unsafeWrite rec
      = (pat "MV" <+> var 'i' <+> tuple vars,
         mk_do [qM rec <+> vs <+> var 'i' <+> v | v  <- vars | vs <- varss]
               empty)

    gen_clear rec
      = (pat "MV", mk_do [qM rec <+> vs | vs <- varss] empty)

    gen_set rec
      = (pat "MV" <+> tuple vars,
         mk_do [qM rec <+> vs <+> v | vs <- varss | v <- vars] empty)

    gen_unsafeCopy c q rec
      = (patn "MV" 1 <+> patn c 2,
         mk_do [q rec <+> vs <> char '1' <+> vs <> char '2' | vs <- varss]
               empty)

    gen_unsafeMove rec
      = (patn "MV" 1 <+> patn "MV" 2,
         mk_do [qM rec <+> vs <> char '1' <+> vs <> char '2' | vs <- varss]
               empty)

    gen_unsafeGrow rec
      = (pat "MV" <+> var 'm',
         mk_do [vs <> char '\'' <+> text "<-"
                                <+> qM rec <+> vs <+> var 'm' | vs <- varss]
               $ text "return $" <+> con "MV"
                                 <+> parens (var 'm' <> char '+' <> var 'n')
                                 <+> sep (map (<> char '\'') varss))

    gen_unsafeFreeze rec
      = (pat "MV",
         mk_do [vs <> char '\'' <+> text "<-" <+> qG rec <+> vs | vs <- varss]
               $ text "return $" <+> con "V" <+> var 'n'
                                 <+> sep [vs <> char '\'' | vs <- varss])

    gen_unsafeThaw rec
      = (pat "V",
         mk_do [vs <> char '\'' <+> text "<-" <+> qG rec <+> vs | vs <- varss]
               $ text "return $" <+> con "MV" <+> var 'n'
                                 <+> sep [vs <> char '\'' | vs <- varss])

    gen_basicUnsafeIndexM rec
      = (pat "V" <+> var 'i',
         mk_do [v <+> text "<-" <+> qG rec <+> vs <+> var 'i'
                        | vs <- varss | v <- vars]
               $ text "return" <+> tuple vars)

    gen_elemseq rec
      = (char '_' <+> tuple vars,
         vcat $ r : [char '.' <+> r | r <- rs])
      where
        r : rs = [qG rec <+> parens (text "undefined :: Vector" <+> v)
                         <+> v | v <- vars]

    mk_do cmds ret = hang (text "do")
                          2
                          $ vcat $ cmds ++ [ret]

    method (s, f) = case f s of
                      (p,e) ->  text "{-# INLINE" <+> text s <+> text " #-}"
                                $$ hang (text s <+> p)
                                   4
                                   (char '=' <+> e)
                             

    methods_MVector = [("basicLength",            gen_length "MV")
                      ,("basicUnsafeSlice",       gen_unsafeSlice "M" "MV")
                      ,("basicOverlaps",          gen_overlaps)
                      ,("basicUnsafeNew",         gen_unsafeNew)
                      ,("basicUnsafeReplicate",   gen_unsafeReplicate)
                      ,("basicUnsafeRead",        gen_unsafeRead)
                      ,("basicUnsafeWrite",       gen_unsafeWrite)
                      ,("basicClear",             gen_clear)
                      ,("basicSet",               gen_set)
                      ,("basicUnsafeCopy",        gen_unsafeCopy "MV" qM)
                      ,("basicUnsafeMove",        gen_unsafeMove)
                      ,("basicUnsafeGrow",        gen_unsafeGrow)]

    methods_Vector  = [("basicUnsafeFreeze",      gen_unsafeFreeze)
                      ,("basicUnsafeThaw",        gen_unsafeThaw)
                      ,("basicLength",            gen_length "V")
                      ,("basicUnsafeSlice",       gen_unsafeSlice "G" "V")
                      ,("basicUnsafeIndexM",      gen_basicUnsafeIndexM)
                      ,("basicUnsafeCopy",        gen_unsafeCopy "V" qG)
                      ,("elemseq",                gen_elemseq)]
