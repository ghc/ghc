# Darwin / Mac OS X only
#   sh fix_install_names.sh directory binary
#
# Changes 'binary' to assume that all libHS*_dyn.dylib libraries
# are to be found in 'directory'.

prefix=$1
file=$2

if `test x${prefix%/} != x"" `
then
    prefix=${prefix%/}/
fi

for i in `otool -L $file \
         | grep 'libHS.*_dyn.dylib' \
         | sed 's/.\(.*libHS.*_dyn.dylib\).*/\1/'`
do
    install_name_tool -change $i $prefix`basename $i` $file
done

