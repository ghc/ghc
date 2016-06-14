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
process_dll_link() {
    ext="${6##*.}"
    base="${6%.*}"
    exports="$base.lst"
    nm -g $5 | sed -r 's/^.+\s[A-Z]\s(.+)/\1/' | sed -r 's/^.+:.*$//g' | sed '/^\s*$/d' > $exports
    SYMBOLS=`cat $exports | wc -l`
    echo "Number of symbols in $6: $SYMBOLS"
    # Now check that the DLL doesn't have too many symbols. See trac #5987.
    case $(($SYMBOLS / 65536)) in 
        0) 
            echo DLL $6 OK
            ;;
        [0-9]*) 
            echo Too many symbols in DLL $6
            tail $exports
            exit 1
            ;; 
        *) 
            echo bad DLL $6
            exit 1
            ;;
    esac

    awk '\
        NR%65536==1 \
        {def="$BASE_"++i+".def";} \
        {print "    " $0 > def} \
        ' $exports

    defs="$BASE_*.def"
    for file in $defs
    do
        DLLfile="$(shell $$(basename $file)).$ext"
        DLLimport="$(shell $$(basename $file)).dll.a"
        sed -i '1i\LIBRARY "$DLLfile"\EXPORTS' $file
        dlltool -d $file -l $DLLimport
    done
}

test() {
    echo "Running tests..."
    objs="compiler/stage2/build/Annotations.dyn_o compiler/stage2/build/ApiAnnotation.dyn_o compiler/stage2/build/Avail.dyn_o compiler/stage2/build/Bag.dyn_o compiler/stage2/build/BasicTypes.dyn_o compiler/stage2/build/Binary.dyn_o compiler/stage2/build/BooleanFormula.dyn_o compiler/stage2/build/BufWrite.dyn_o compiler/stage2/build/Class.dyn_o compiler/stage2/build/CmdLineParser.dyn_o compiler/stage2/build/CmmType.dyn_o compiler/stage2/build/CoAxiom.dyn_o compiler/stage2/build/ConLike.dyn_o compiler/stage2/build/Coercion.dyn_o compiler/stage2/build/Config.dyn_o compiler/stage2/build/Constants.dyn_o compiler/stage2/build/CoreArity.dyn_o compiler/stage2/build/CoreFVs.dyn_o compiler/stage2/build/CoreSubst.dyn_o compiler/stage2/build/CoreSyn.dyn_o compiler/stage2/build/CoreTidy.dyn_o compiler/stage2/build/CoreUnfold.dyn_o compiler/stage2/build/CoreUtils.dyn_o compiler/stage2/build/CoreSeq.dyn_o compiler/stage2/build/CoreStats.dyn_o compiler/stage2/build/CostCentre.dyn_o compiler/stage2/build/DataCon.dyn_o compiler/stage2/build/Demand.dyn_o compiler/stage2/build/Digraph.dyn_o compiler/stage2/build/DriverPhases.dyn_o compiler/stage2/build/DynFlags.dyn_o compiler/stage2/build/Encoding.dyn_o compiler/stage2/build/ErrUtils.dyn_o compiler/stage2/build/Exception.dyn_o compiler/stage2/build/FamInstEnv.dyn_o compiler/stage2/build/FastFunctions.dyn_o compiler/stage2/build/FastMutInt.dyn_o compiler/stage2/build/FastString.dyn_o compiler/stage2/build/FastStringEnv.dyn_o compiler/stage2/build/FieldLabel.dyn_o compiler/stage2/build/Fingerprint.dyn_o compiler/stage2/build/FiniteMap.dyn_o compiler/stage2/build/ForeignCall.dyn_o compiler/stage2/build/FV.dyn_o compiler/stage2/build/Hooks.dyn_o compiler/stage2/build/HsBinds.dyn_o compiler/stage2/build/HsDecls.dyn_o compiler/stage2/build/HsDoc.dyn_o compiler/stage2/build/HsExpr.dyn_o compiler/stage2/build/HsImpExp.dyn_o compiler/stage2/build/HsLit.dyn_o compiler/stage2/build/PlaceHolder.dyn_o compiler/stage2/build/PmExpr.dyn_o compiler/stage2/build/HsPat.dyn_o compiler/stage2/build/HsSyn.dyn_o compiler/stage2/build/HsTypes.dyn_o compiler/stage2/build/HsUtils.dyn_o compiler/stage2/build/HscTypes.dyn_o compiler/stage2/build/IOEnv.dyn_o compiler/stage2/build/Id.dyn_o compiler/stage2/build/IdInfo.dyn_o compiler/stage2/build/IfaceSyn.dyn_o compiler/stage2/build/IfaceType.dyn_o compiler/stage2/build/InstEnv.dyn_o compiler/stage2/build/Kind.dyn_o compiler/stage2/build/Lexeme.dyn_o compiler/stage2/build/ListSetOps.dyn_o compiler/stage2/build/Literal.dyn_o compiler/stage2/build/Maybes.dyn_o compiler/stage2/build/MkCore.dyn_o compiler/stage2/build/MkId.dyn_o compiler/stage2/build/Module.dyn_o compiler/stage2/build/MonadUtils.dyn_o compiler/stage2/build/Name.dyn_o compiler/stage2/build/NameEnv.dyn_o compiler/stage2/build/NameSet.dyn_o compiler/stage2/build/OccName.dyn_o compiler/stage2/build/OccurAnal.dyn_o compiler/stage2/build/OptCoercion.dyn_o compiler/stage2/build/OrdList.dyn_o compiler/stage2/build/Outputable.dyn_o compiler/stage2/build/PackageConfig.dyn_o compiler/stage2/build/Packages.dyn_o compiler/stage2/build/Pair.dyn_o compiler/stage2/build/Panic.dyn_o compiler/stage2/build/PatSyn.dyn_o compiler/stage2/build/PipelineMonad.dyn_o compiler/stage2/build/Platform.dyn_o compiler/stage2/build/PlatformConstants.dyn_o compiler/stage2/build/PprCore.dyn_o compiler/stage2/build/PrelNames.dyn_o compiler/stage2/build/PrelRules.dyn_o compiler/stage2/build/Pretty.dyn_o compiler/stage2/build/PrimOp.dyn_o compiler/stage2/build/RdrName.dyn_o compiler/stage2/build/Rules.dyn_o compiler/stage2/build/SrcLoc.dyn_o compiler/stage2/build/StaticFlags.dyn_o compiler/stage2/build/StringBuffer.dyn_o compiler/stage2/build/TcEvidence.dyn_o compiler/stage2/build/TcRnTypes.dyn_o compiler/stage2/build/TcType.dyn_o compiler/stage2/build/TrieMap.dyn_o compiler/stage2/build/TyCon.dyn_o compiler/stage2/build/Type.dyn_o compiler/stage2/build/TyCoRep.dyn_o compiler/stage2/build/TysPrim.dyn_o compiler/stage2/build/TysWiredIn.dyn_o compiler/stage2/build/Unify.dyn_o compiler/stage2/build/UniqDFM.dyn_o compiler/stage2/build/UniqDSet.dyn_o compiler/stage2/build/UniqFM.dyn_o compiler/stage2/build/UniqSet.dyn_o compiler/stage2/build/UniqSupply.dyn_o compiler/stage2/build/Unique.dyn_o compiler/stage2/build/Util.dyn_o compiler/stage2/build/Var.dyn_o compiler/stage2/build/VarEnv.dyn_o compiler/stage2/build/VarSet.dyn_o compiler/stage2/build/ByteCodeTypes.dyn_o compiler/stage2/build/InteractiveEvalTypes.dyn_o   compiler/stage2/build/parser/cutils.dyn_o compiler/stage2/build/ghci/keepCAFsForGHCi.dyn_o compiler/stage2/build/cbits/genSym.dyn_o"
    out="compiler/stage2/build/libHSghc-8.1-0-ghc8.1.20160612.dll"
    distdir="compiler/stage2/build"
    dir="compiler/stage2/build"
    way=""
    flags=""
    
    process_dll_link "$dir" "$distdir" "$way" "$flags" "$objs" "$out"
}


usage() {
    echo "$0 - Split a dll is required and perform the linking"
    echo
    echo "Usage: $0 <action> <arch>"
    echo
    echo "Where <action> is one of,"
    echo "    download     download the necessary tarballs for the given architecture"
    echo "    verify       verify the existance and correctness of the necessary tarballs"
    echo "and <arch> is one of i386, x86_64, or all"
}

case $1 in
    test)
        test
        ;;
    link)
        link=0
        ;;
    *)
        usage
        exit 1
        ;;
esac