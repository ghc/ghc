#!/usr/bin/env sh

fail() {
    echo >&2
    echo "$1" >&2
    exit 1
}

# $1 = dir
# $2 = distdir
# $3 = way
# $4 = extra flags
# $5 = object files to link
# $6 = output filename
# $7 = link command
process_dll_link() {
    ext="${6##*.}"
    base="${6%.*}"
    exports="$base.lst"
    max=65535

    # Build the DLL first
    cmd="$7 $5 -o $6"
    echo "$cmd"
    eval "$cmd"
    # rm -f "$6.a"

    # Now measure the DLL
    nm -g $6 | sed -r 's/^.+\s[A-Z]\s(.+)/\1/' | sed -r 's/^.+:.*$//g' | sed '/^\s*$/d' | sort | uniq -u > $exports
    SYMBOLS_DLL=`cat $exports | wc -l | cut -d' ' -f1`

    echo "Number of symbols in $6: $SYMBOLS_DLL"
    # Now check that the DLL doesn't have too many symbols. See trac #5987.
    case $(($SYMBOLS_DLL / $max)) in
        0)
            echo DLL $6 OK, no need to split
            exit 0
            ;;
        [0-9]*)
            echo Too many symbols in DLL $6
            echo "We'll have to split the dll..."
            ;; 
        *)
            echo bad DLL $6
            exit 1
            ;;
    esac

    # We need to know how many symbols came from other static archives
    # So take the total number of symbols and remove those we know came
    # from the object files. Use this to lower the max amount of symbols.
    #
    # This granularity is the best we can do without --print-map like info.
    nm -g $5 | sed -r 's/^.+\s[A-Z]\s(.+)/\1/' | sed -r 's/^.+:.*$//g' | sed '/^\s*$/d' | sort | uniq -u > $exports
    SYMBOLS_OBJ=`cat $exports | wc -l | cut -d' ' -f1`
    echo "Number of symbols in object files for $6: $SYMBOLS_OBJ"
    max=$(($max - ($SYMBOLS_DLL - $SYMBOLS_OBJ)))
    echo "OK, we only have space for $max symbols from object files when building $6"

    if [ "$max" -lt "0" ]
    then
        echo "Uhm.. Split is impossible, too many symbols from static archives."
        exit 1;
    fi

    # First split the dlls up by whole object files
    i=0
    count=0
    declare -A buffer
    declare -A obj_files

    for obj in $5
    do
        obj_symbols=`nm -g $obj | sed -r 's/^.+\s[A-Z]\s(.+)/\1/' | sed -r 's/^.+:.*$//g' | sed '/^\s*$/d'`
        obj_count=`echo $obj_symbols | wc -w | cut -d' ' -f1`
        echo "Using $obj ($obj_count)"
        count=$(($count + $obj_count))

        if [ "$count" -gt "$max" ]
        then
            echo ">> DLL split at $(($count - $obj_count)) symbols."
            i=$(($i + 1))
            echo "$buffer"    | sed 's/\s/\n/g' | sed '/^\s*$/d' | sort | uniq -u > "$base-pt$i.lst"
            echo "$obj_files" | sed 's/\s/\n/g' | sed '/^\s*$/d' | sort | uniq -u > "$base-pt$i.objs"
            buffer="$obj_symbols"
            obj_files="$obj"
            count=$obj_count
        else
            buffer=`echo "$buffer" "$obj_symbols" | sort`
            obj_files=`echo "$obj_files" "$obj" | sort`
        fi
    done

    # Write the rest in the buffer
    i=$(($i + 1))
    echo "$buffer"    | sed 's/\s/\n/g' | sed '/^\s*$/d' | sort | uniq -u > "$base-pt$i.lst"
    echo "$obj_files" | sed 's/\s/\n/g' | sed '/^\s*$/d' | sort | uniq -u > "$base-pt$i.objs"

    count=$i
    echo "OK, based on the amount of symbols we'll split the DLL into $count"

    items=$(seq 1 $count)
    for i in $items
    do
        file="$base-pt$i.def"
        lstfile="$base-pt$i.lst"
        awk -v root="$file" '{def=root;}{print "    \"" $0 "\""> def}' $lstfile
        echo "Processing $file..."
        basefile="$(basename $file)"
        DLLfile="${basefile%.*}.$ext"
        DLLimport="${file%.*}.dll.a"
        sed -i "1i\LIBRARY \"$DLLfile\"\\nEXPORTS" $file
        dlltool -d $file -l $DLLimport
    done

    for i in $items
    do
        def="$base-pt$i.def"
        objfile="$base-pt$i.objs"
        objs=`cat "$objfile" | tr "\n" " "`
        basefile="$(basename $def)"
        DLLfile="${basefile%.*}.$ext"
        declare -A imports
        for j in $items
        do
            if [ "$j" -ne "$i" ]
            then
                imports=`echo "$imports" "$base-pt$j.dll.a"`
            fi
        done
        cmd="$7 $objs $def $imports -o $2/$DLLfile"
        echo "$cmd"
        eval "$cmd"
    done

    # do some cleanup and create merged lib
    implibs="$base*.dll.a"
    arscript="$base.mri"
    impLib="$base.dll.a"
    echo "create $impLib" > $arscript
    for file in $implibs
    do
        echo "addlib $file" >> $arscript
    done
    echo "save" >> $arscript
    echo "end" >> $arscript
    ar -M < $arscript

    # now exit
    exit 0
}

test() {
    echo "Running tests..."
    objs="compiler/stage2/build/Annotations.dyn_o compiler/stage2/build/ApiAnnotation.dyn_o compiler/stage2/build/Avail.dyn_o compiler/stage2/build/Bag.dyn_o compiler/stage2/build/BasicTypes.dyn_o compiler/stage2/build/Binary.dyn_o compiler/stage2/build/BooleanFormula.dyn_o compiler/stage2/build/BufWrite.dyn_o compiler/stage2/build/Class.dyn_o compiler/stage2/build/CmdLineParser.dyn_o compiler/stage2/build/CmmType.dyn_o compiler/stage2/build/CoAxiom.dyn_o compiler/stage2/build/ConLike.dyn_o compiler/stage2/build/Coercion.dyn_o compiler/stage2/build/Config.dyn_o compiler/stage2/build/Constants.dyn_o compiler/stage2/build/CoreArity.dyn_o compiler/stage2/build/CoreFVs.dyn_o compiler/stage2/build/CoreSubst.dyn_o compiler/stage2/build/CoreSyn.dyn_o compiler/stage2/build/CoreTidy.dyn_o compiler/stage2/build/CoreUnfold.dyn_o compiler/stage2/build/CoreUtils.dyn_o compiler/stage2/build/CoreSeq.dyn_o compiler/stage2/build/CoreStats.dyn_o compiler/stage2/build/CostCentre.dyn_o compiler/stage2/build/DataCon.dyn_o compiler/stage2/build/Demand.dyn_o compiler/stage2/build/Digraph.dyn_o compiler/stage2/build/DriverPhases.dyn_o compiler/stage2/build/DynFlags.dyn_o compiler/stage2/build/Encoding.dyn_o compiler/stage2/build/ErrUtils.dyn_o compiler/stage2/build/Exception.dyn_o compiler/stage2/build/FamInstEnv.dyn_o compiler/stage2/build/FastFunctions.dyn_o compiler/stage2/build/FastMutInt.dyn_o compiler/stage2/build/FastString.dyn_o compiler/stage2/build/FastStringEnv.dyn_o compiler/stage2/build/FieldLabel.dyn_o compiler/stage2/build/Fingerprint.dyn_o compiler/stage2/build/FiniteMap.dyn_o compiler/stage2/build/ForeignCall.dyn_o compiler/stage2/build/FV.dyn_o compiler/stage2/build/Hooks.dyn_o compiler/stage2/build/HsBinds.dyn_o compiler/stage2/build/HsDecls.dyn_o compiler/stage2/build/HsDoc.dyn_o compiler/stage2/build/HsExpr.dyn_o compiler/stage2/build/HsImpExp.dyn_o compiler/stage2/build/HsLit.dyn_o compiler/stage2/build/PlaceHolder.dyn_o compiler/stage2/build/PmExpr.dyn_o compiler/stage2/build/HsPat.dyn_o compiler/stage2/build/HsSyn.dyn_o compiler/stage2/build/HsTypes.dyn_o compiler/stage2/build/HsUtils.dyn_o compiler/stage2/build/HscTypes.dyn_o compiler/stage2/build/IOEnv.dyn_o compiler/stage2/build/Id.dyn_o compiler/stage2/build/IdInfo.dyn_o compiler/stage2/build/IfaceSyn.dyn_o compiler/stage2/build/IfaceType.dyn_o compiler/stage2/build/InstEnv.dyn_o compiler/stage2/build/Kind.dyn_o compiler/stage2/build/Lexeme.dyn_o compiler/stage2/build/ListSetOps.dyn_o compiler/stage2/build/Literal.dyn_o compiler/stage2/build/Maybes.dyn_o compiler/stage2/build/MkCore.dyn_o compiler/stage2/build/MkId.dyn_o compiler/stage2/build/Module.dyn_o compiler/stage2/build/MonadUtils.dyn_o compiler/stage2/build/Name.dyn_o compiler/stage2/build/NameEnv.dyn_o compiler/stage2/build/NameSet.dyn_o compiler/stage2/build/OccName.dyn_o compiler/stage2/build/OccurAnal.dyn_o compiler/stage2/build/OptCoercion.dyn_o compiler/stage2/build/OrdList.dyn_o compiler/stage2/build/Outputable.dyn_o compiler/stage2/build/PackageConfig.dyn_o compiler/stage2/build/Packages.dyn_o compiler/stage2/build/Pair.dyn_o compiler/stage2/build/Panic.dyn_o compiler/stage2/build/PatSyn.dyn_o compiler/stage2/build/PipelineMonad.dyn_o compiler/stage2/build/Platform.dyn_o compiler/stage2/build/PlatformConstants.dyn_o compiler/stage2/build/PprCore.dyn_o compiler/stage2/build/PrelNames.dyn_o compiler/stage2/build/PrelRules.dyn_o compiler/stage2/build/Pretty.dyn_o compiler/stage2/build/PrimOp.dyn_o compiler/stage2/build/RdrName.dyn_o compiler/stage2/build/Rules.dyn_o compiler/stage2/build/SrcLoc.dyn_o compiler/stage2/build/StaticFlags.dyn_o compiler/stage2/build/StringBuffer.dyn_o compiler/stage2/build/TcEvidence.dyn_o compiler/stage2/build/TcRnTypes.dyn_o compiler/stage2/build/TcType.dyn_o compiler/stage2/build/TrieMap.dyn_o compiler/stage2/build/TyCon.dyn_o compiler/stage2/build/Type.dyn_o compiler/stage2/build/TyCoRep.dyn_o compiler/stage2/build/TysPrim.dyn_o compiler/stage2/build/TysWiredIn.dyn_o compiler/stage2/build/Unify.dyn_o compiler/stage2/build/UniqDFM.dyn_o compiler/stage2/build/UniqDSet.dyn_o compiler/stage2/build/UniqFM.dyn_o compiler/stage2/build/UniqSet.dyn_o compiler/stage2/build/UniqSupply.dyn_o compiler/stage2/build/Unique.dyn_o compiler/stage2/build/Util.dyn_o compiler/stage2/build/Var.dyn_o compiler/stage2/build/VarEnv.dyn_o compiler/stage2/build/VarSet.dyn_o compiler/stage2/build/ByteCodeTypes.dyn_o compiler/stage2/build/InteractiveEvalTypes.dyn_o   compiler/stage2/build/parser/cutils.dyn_o compiler/stage2/build/ghci/keepCAFsForGHCi.dyn_o compiler/stage2/build/cbits/genSym.dyn_o"
    out="compiler/stage2/build/libHSghc-8.1-0-ghc8.1.20160612.dll"
    distdir="compiler/stage2/build"
    dir="compiler/stage2/build"
    way=""
    flags=""
    link_cmd="\"inplace/bin/ghc-stage1.exe\" -hisuf dyn_hi -osuf  dyn_o -hcsuf dyn_hc -fPIC -dynamic  -O0 -H64m -Wall      -this-unit-id ghc-8.1 -hide-all-packages -i -icompiler/basicTypes -icompiler/cmm -icompiler/codeGen -icompiler/coreSyn -icompiler/deSugar -icompiler/ghci -icompiler/hsSyn -icompiler/iface -icompiler/llvmGen -icompiler/main -icompiler/nativeGen -icompiler/parser -icompiler/prelude -icompiler/profiling -icompiler/rename -icompiler/simplCore -icompiler/simplStg -icompiler/specialise -icompiler/stgSyn -icompiler/stranal -icompiler/typecheck -icompiler/types -icompiler/utils -icompiler/vectorise -icompiler/stage2/build -icompiler/stage2/build/autogen -Icompiler/stage2/build -Icompiler/stage2/build/autogen -Icompiler/. -Icompiler/parser -Icompiler/utils -Icompiler/../rts/dist/build -Icompiler/stage2   -optP-DGHCI -optP-include -optPcompiler/stage2/build/autogen/cabal_macros.h -package-id Win32-2.3.1.1 -package-id array-0.5.1.1 -package-id base-4.9.0.0 -package-id binary-0.8.3.0 -package-id bytestring-0.10.8.1 -package-id containers-0.5.7.1 -package-id directory-1.2.6.2 -package-id filepath-1.4.1.0 -package-id ghc-boot-8.1 -package-id ghci-8.1 -package-id hoopl-3.10.2.1 -package-id hpc-0.6.0.3 -package-id process-1.4.2.0 -package-id template-haskell-2.11.0.0 -package-id time-1.6.0.1 -package-id transformers-0.5.2.0 -Wall -fno-warn-name-shadowing -this-unit-id ghc -XHaskell2010 -optc-DTHREADED_RTS -DGHCI_TABLES_NEXT_TO_CODE -DSTAGE=2 -Rghc-timing -O0  -no-user-package-db -rtsopts      -Wnoncanonical-monad-instances  -odir compiler/stage2/build -hidir compiler/stage2/build -optl-L'E:\msys64\home\Tamar\ghc2\libraries\process\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\hpc\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\hoopl\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\ghci\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\transformers\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\template-haskell\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\pretty\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\ghc-boot\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\ghc-boot-th\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\directory\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\time\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\filepath\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\binary\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\containers\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\Win32\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\bytestring\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\deepseq\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\array\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\base\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\integer-gmp\dist-install\build' -optl-L'E:\msys64\home\Tamar\ghc2\libraries\ghc-prim\dist-install\build' -optl-L'E:/msys64/home/Tamar/ghc2/rts/dist/build' -optl-lkernel32 -optl-luser32 -optl-lgdi32 -optl-lwinmm -optl-ladvapi32 -optl-lshell32 -optl-lshfolder -optl-lwsock32 -optl-luser32 -optl-lshell32 -optl-lmsvcrt -optl-lmingw32 -optl-lmingwex -optl-luser32 -optl-lmingw32 -optl-lmingwex -optl-lm -optl-lwsock32 -optl-lgdi32 -optl-lwinmm -optl-lmingwex -fPIC -dynamic  -O0 -H64m -Wall      -this-unit-id ghc-8.1 -hide-all-packages -i -icompiler/basicTypes -icompiler/cmm -icompiler/codeGen -icompiler/coreSyn -icompiler/deSugar -icompiler/ghci -icompiler/hsSyn -icompiler/iface -icompiler/llvmGen -icompiler/main -icompiler/nativeGen -icompiler/parser -icompiler/prelude -icompiler/profiling -icompiler/rename -icompiler/simplCore -icompiler/simplStg -icompiler/specialise -icompiler/stgSyn -icompiler/stranal -icompiler/typecheck -icompiler/types -icompiler/utils -icompiler/vectorise -icompiler/stage2/build -icompiler/stage2/build/autogen -Icompiler/stage2/build -Icompiler/stage2/build/autogen -Icompiler/. -Icompiler/parser -Icompiler/utils -Icompiler/../rts/dist/build -Icompiler/stage2   -optP-DGHCI -optP-include -optPcompiler/stage2/build/autogen/cabal_macros.h -package-id Win32-2.3.1.1 -package-id array-0.5.1.1 -package-id base-4.9.0.0 -package-id binary-0.8.3.0 -package-id bytestring-0.10.8.1 -package-id containers-0.5.7.1 -package-id directory-1.2.6.2 -package-id filepath-1.4.1.0 -package-id ghc-boot-8.1 -package-id ghci-8.1 -package-id hoopl-3.10.2.1 -package-id hpc-0.6.0.3 -package-id process-1.4.2.0 -package-id template-haskell-2.11.0.0 -package-id time-1.6.0.1 -package-id transformers-0.5.2.0 -Wall -fno-warn-name-shadowing -this-unit-id ghc -XHaskell2010 -optc-DTHREADED_RTS -DGHCI_TABLES_NEXT_TO_CODE -DSTAGE=2 -Rghc-timing -O0  -no-user-package-db -rtsopts -Wnoncanonical-monad-instances -shared -dynamic -dynload deploy  -no-auto-link-packages"

    process_dll_link "$dir" "$distdir" "$way" "$flags" "$objs" "$out" "$link_cmd"
}

usage() {
    echo "$0 - Split a dll is required and perform the linking"
    echo
    echo "Usage: $0 <action>"
    echo
    echo "Where <action> is one of,"
    echo "    test    perform a test link of libGHC"
    echo "    link    perform a real link of dll, arguments: dir distdir way flags objs out link_cmd"
}

case $1 in
    test)
        test
        exit 0
        ;;
    link)
        process_dll_link "$2" "$3" "$4" "$5" "$6" "$7" "$8"
        exit 0
        ;;
    *)
        usage
        exit 1
        ;;
esac
