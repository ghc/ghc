if [ ! -f "$DOCBOOK_CATALOG" ] && [ ! -f "$SGML_CATALOG_FILES" ] ; then
  echo "CATALOG file not set up; see installation guide for details."
  exit 1
fi

if [ -f "$DOCBOOK_CATALOG" ] ; then
  CATALOG_OPTION="-c $DOCBOOK_CATALOG"
fi

output=db2rtf.rtf

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
    output="`echo $1 | sed 's,\.sgml$,.rtf,;s,\.sgm$,.rtf,'`"
  fi
fi

cat $* | $JADE -t rtf -d ${DB_STYLESHEET}\#print $CATALOG_OPTION

if [ $# -eq 1 ]
then
  mv jade-out.rtf $output
else
  cat jade-out.rtf
fi

exit 0
