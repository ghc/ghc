# Dave Mason's option to specify a different stylesheet
case $1 in
    -d) DB_STYLESHEET=$2
        shift 2
	;;
esac

if [ $# -gt 2 ]
then
  echo "Usage: `basename $0` [filename.sgml]" >&2
  exit 1
fi

output="`echo $1 | sed 's,\.sgml$,.ps,;s,\.sgm$,.ps,'`"
outdvi="`echo $1 | sed 's,\.sgml$,.dvi,;s,\.sgm$,.dvi,'`"
make $outdvi
dvips $outdvi -o $output

exit 0
