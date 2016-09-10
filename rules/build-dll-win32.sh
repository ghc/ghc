#!/usr/bin/env sh

fail() {
    echo >&2
    echo "$1" >&2
    exit 1
}

# Builds a delay import lib at the very end which is used to
# be able to delay the picking of a DLL on Windows.
# This function is called always and decided internally
# what to do.
#
# $1 = input def file
# $2 = Output import delayed import lib
# $3 = flag to indicate if delay import lib should be created.
build_delay_import_lib()
{
    # Check if we have to create a delay loaded DLL
    if [ "$3" == "YES" ];
    then
        dlltool -d $1 -y $2
    fi
}

# $1  = dir
# $2  = distdir
# $3  = way
# $4  = extra flags
# $5  = object files to link
# $6  = output filename
# $7  = link command
# $8  = create delay load import lib
# $9  = SxS Name
# $10 = SxS Version
process_dll_link() {
    ext="${6##*.}"
    base="${6%.*}"
    exports="$base.lst"

    # Maximum number of symbols to allow into
    # one DLL. This is the split factor.
    DLL_MAX_SYMBOLS=65535

    # We need to know how many symbols came from other static archives
    # So take the total number of symbols and remove those we know came
    # from the object files. Use this to lower the max amount of symbols.
    #
    # This granularity is the best we can do without --print-map like info.
    RAW_EXPORTS=`nm -g $5 | sed -nr 's/^[a-z,A-Z,0-9]+\s([A-Z])\s(.+)$/\1 \2\n/p' | sed -r 's/^.+:.*$//g' | sed '/^\s*$/d' | sort | uniq -u`
    echo -e "${RAW_EXPORTS}" | sed -nr 's/^[A-Z]+\s(.+)$/\1/p' > $exports
    SYMBOLS_OBJ=`cat $exports | wc -l | cut -d' ' -f1`
    echo "Number of symbols in object files for $6: $SYMBOLS_OBJ"

    # Side-by-Side assembly generation flags for GHC. Pass these along so the DLLs
    # get the proper manifests generated.
    SXS_OPTS="-fgen-sxs-assembly -dylib-abi-name \"$9\" -dylib-abi-version \"${10}\""
    
    # echo "Number of symbols in $6: $SYMBOLS_DLL"
    # Now check that the DLL doesn't have too many symbols. See trac #5987.
    case $(($SYMBOLS_OBJ / $DLL_MAX_SYMBOLS)) in
        0)
            echo DLL $6 OK, no need to split
            defFile="$base.def"
            elst="$base.elst"

            # Create a def file hiding symbols not in original object files
            # because --export-all is re-exporting things from static libs
            # we need to separate out data from functions. So first create two temporaries
            globals=`echo -e "${RAW_EXPORTS}" | sed -nr 's/^[DdGgrRSs]\s(.+)$/\1\n/p'1 | sed '/^\s*$/d'`
            functions=`echo -e "${RAW_EXPORTS}" | sed -nr 's/^[^DdGgrRSs]\s(.+)$/\1\n/p'1 | sed '/^\s*$/d'`

            # This split is important because for DATA entries the compiler should not generate
            # a trampoline since CONTS DATA is directly referenced and not executed. This is not very
            # important for mingw-w64 which would generate both the trampoline and direct referecne
            # by default, but for libtool is it and even for mingw-w64 we can trim the output.
            echo -e "${globals}" | awk -v root="$defFile" '{def=root;}{print "    \"" $0 "\" DATA"> def}'
            echo -e "${functions}" | awk -v root="$defFile" '{def=root;}{print "    \"" $0 "\"">> def}'
            sed -i "1i\LIBRARY \"${6##*/}\"\\nEXPORTS" $defFile

            DLLimport="$base.dll.a"
            dlltool -d $defFile -l $DLLimport
            
            cmd="$7 $DLLimport $5 ${SXS_OPTS} -optl-Wl,--retain-symbols-file=$exports -o $6"
            echo "$cmd"
            eval "$cmd" || exit 1            
            build_delay_import_lib $defFile $DLLimport $8
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

    echo "OK, we only have space for $DLL_MAX_SYMBOLS symbols from object files when building $6"

    # First split the dlls up by whole object files
    # To do this, we iterate over all object file and
    # generate a the partitions based on allowing a
    # maximum of $DLL_MAX_SYMBOLS in one DLL.
    i=0
    count=0
    declare -A buffer
    declare -A obj_files

    for obj in $5
    do
        obj_symbols=`nm -g $obj | sed -nr 's/^[a-z,A-Z,0-9]+\s[A-Z]\s(.+)$/\1/p' | sed -r 's/^.+:.*$//g' | sed '/^\s*$/d'`
        obj_count=`echo $obj_symbols | wc -w | cut -d' ' -f1`
        echo "Using $obj ($obj_count)"
        count=$(($count + $obj_count))

        if [ "$count" -gt "$DLL_MAX_SYMBOLS" ]
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
    echo "OK, based on the amount of symbols we'll split the DLL into $count pieces."

    items=$(seq 1 $count)
    for i in $items
    do
        file="$base-pt$i.def"
        lstfile="$base-pt$i.lst"
        elstfile="$base-pt$i.elst"
        basefile="$(basename $file)"
        DLLfile="${basefile%.*}.$ext"

        # Now do the same split and partitioning as above.
        # I'm wondering if these can't be refactored to share code with above
        # but my shell foo is weak. So I'll leave it as is for now.
        globals=`echo -e "${RAW_EXPORTS}" | sed -nr 's/^[DdGgrRSs]\s(.+)$/\1\n/p'1 | sed '/^\s*$/d'`
        functions=`echo -e "${RAW_EXPORTS}" | sed -nr 's/^[^DdGgrRSs]\s(.+)$/\1\n/p'1 | sed '/^\s*$/d'`
        echo -e "${globals}" | awk -v root="$defFile" '{def=root;}{print "    \"" $0 "\" DATA"> def}'
        echo -e "${functions}" | awk -v root="$defFile" '{def=root;}{print "    \"" $0 "\"">> def}'
        sed -i "1i\LIBRARY \"$DLLfile\"\\nEXPORTS" $file

        echo "Processing $file..."
        DLLimport="${file%.*}.dll.a"
        dlltool -d $file -l $DLLimport
    done

    for i in $items
    do
        def="$base-pt$i.def"
        objfile="$base-pt$i.objs"
        elstfile="$base-pt$i.lst"
        objs=`cat "$objfile" | tr "\n" " "`
        basefile="$(basename $def)"
        DLLfile="$base-pt$i.$ext"
        declare -A imports
        for j in $items
        do
            if [ "$j" -ne "$i" ]
            then
                imports=`echo "$imports" "$base-pt$j.dll.a"`
            fi
        done
        cmd="$7 $objs $def $imports ${SXS_OPTS} -optl-Wl,--retain-symbols-file=$elstfile -o $DLLfile"
        echo "$cmd"
        eval "$cmd" || exit 1
        build_delay_import_lib $def "$base-pt$j.dll.a" $8
    done

    # Do some cleanup and create merged lib.
    # Because we have no split the DLL we need
    # to provide a way for the linker to know about the split
    # DLL. Also the compile was supposed to produce a DLL
    # foo.dll and import library foo.dll.a. However we've actually
    # produced foo-pt1.dll, foo-pt2.dll etc. What we don't want is to have
    # To somehow convey back to the compiler that we split the DLL in x pieces
    # as this would require a lot of changes.
    #
    # Instead we produce a merged import library which contains the union of
    # all the import libraries produced. This works because import libraries contain
    # only .idata section which point to the right dlls. So LD will do the right thing.
    # And this means we don't have to do any special handling for the rest of the pipeline.
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

usage() {
    echo "$0 - Split a dll is required and perform the linking"
    echo
    echo "Usage: $0 <action>"
    echo
    echo "Where <action> is one of,"
    echo "    link    perform a real link of dll, arguments: dir distdir way flags objs out link_cmd"
}

case $1 in
    link)
        process_dll_link "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9" "${10}" "${11}"
        exit 0
        ;;
    *)
        usage
        exit 1
        ;;
esac
