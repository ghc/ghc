if [ ! -f "$DOCBOOK_CATALOG" ] && [ ! -f "$SGML_CATALOG_FILES" ] ; then
  echo "CATALOG file not set up; see installation guide for details."
  exit 1
fi

if [ -f "$DOCBOOK_CATALOG" ] ; then
  CATALOG_OPTION="-c $DOCBOOK_CATALOG"
fi

HTML_STYLESHEET=$HTML_DIR/html/docbook.css
ADMON_GRAPHICS=$HTML_DIR/html/images/*.gif

output=db2html-dir
TMPDIR=DBTOHTML_OUTPUT_DIR$$

echo TMPDIR is $TMPDIR

# Dave Mason's option to specify a different stylesheet
case $1 in
    -d) DB_STYLESHEET=$2
        shift 2
	;;
esac

echo "Using stylesheet: \"${DB_STYLESHEET}\""

if [ $# -gt 2 ]
then
  echo "Usage: `basename $0` [filename.sgml]" >&2
  exit 1
fi

if [ $# -eq 1 ]
then
  if [ ! -r $1 ]
  then
    echo Cannot read \"$1\".  Exiting. >&2
    exit 1
  fi
  if echo $1 | egrep -i '\.sgml$|\.sgm$' >/dev/null 2>&1
  then
    # now make sure that the output directory is always a subdirectory
    # of hte current directory
    echo
    input_file=`basename $1`
    output="`echo $input_file | sed 's,\.sgml$,,;s,\.sgm$,,'`"
    echo "input file was called $input_file -- output will be in $output"
    echo
  fi
fi

# we used to generate a single file, but with the modular DB_STYLESHEETs
# it's best to make a new directory with several html files in it
#cat $* | jade -d $DB_STYLESHEET -t sgml -V nochunks > $TMPFN

mkdir $TMPDIR
SAVE_PWD=`pwd`
if [ $1 = `basename $1` ]; then
  echo "working on ../$1"
  (cd $TMPDIR; $JADE -t sgml -ihtml -d ${DB_STYLESHEET}\#html $CATALOG_OPTION ../$1; cd $SAVE_PWD)
else
  echo "working on $1"
  (cd $TMPDIR; $JADE -t sgml -ihtml -d ${DB_STYLESHEET}\#html $CATALOG_OPTION $1; cd $SAVE_PWD)
fi

if [ $# -eq 1 ]
then
  if [ -d ${output}.junk ]
  then
    rm -rf ${output}.junk
  fi
  if [ -d ${output} ]
  then
    mv $output ${output}.junk
  fi
  echo "about to copy cascading stylesheet and admon graphics to temp dir"
  cp ${HTML_STYLESHEET} ${TMPDIR}/
  mkdir ${TMPDIR}/stylesheet-images
  cp ${ADMON_GRAPHICS} ${TMPDIR}/stylesheet-images
  echo "about to rename temporary directory to $output"
  mv ${TMPDIR} $output
else
  cat $TMPDIR/*
fi

rm -rf $TMPDIR

exit 0
