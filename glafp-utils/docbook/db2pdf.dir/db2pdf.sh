if [ -z "$SGML_CATALOG_FILES"]
then
  if [ ! -f "$FPTOOLS_CATALOG_FILE" ]
  then
    echo "CATALOG file not set up; see installation guide for details."
    exit 1
  fi
  SGML_CATALOG_FILES=$FPTOOLS_CATALOG_FILE
fi

TMPFN=`echo $1 | sed 's/\.sgml//'`

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
    output="`echo $1 | sed 's,\.sgml$,.pdf,;s,\.sgm$,.pdf,'`"
  fi
fi

$JADE -t tex -d ${DB_STYLESHEET}\#print -o ${TMPFN}.tex -c $SGML_CATALOG_FILES $1

pdf$JADEtex $TMPFN

if egrep '^LaTeX Warning: There were undefined references.$' ${TMPFN}.log >/dev/null 2>&1
then
  pdfjadetex $TMPFN
  pdfjadetex $TMPFN
fi

exit 0
