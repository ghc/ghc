# FP_GEN_DOCBOOK_XML
# ------------------
# Generates a DocBook XML V4.2 document in conftest.xml.
AC_DEFUN([FP_GEN_DOCBOOK_XML],
[rm -f conftest.xml
cat > conftest.xml << EOF
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.2//EN"
   "http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd">
<book id="test">
  <title>A DocBook Test Document</title>
  <chapter id="id-one">
    <title>A Chapter Title</title>
    <para>This is a paragraph, referencing <xref linkend="id-two"/>.</para>
  </chapter>
  <chapter id="id-two">
    <title>Another Chapter Title</title>
    <para>This is another paragraph, referencing <xref linkend="id-one"/>.</para>
  </chapter>
</book>
EOF
]) # FP_GEN_DOCBOOK_XML


# FP_PROG_XSLTPROC
# ----------------
# Sets the output variable XsltprocCmd to the full path of the XSLT processor
# xsltproc. XsltprocCmd is empty if xsltproc could not be found.
AC_DEFUN([FP_PROG_XSLTPROC],
[AC_PATH_PROG([XsltprocCmd], [xsltproc])
if test -z "$XsltprocCmd"; then
  AC_MSG_WARN([cannot find xsltproc in your PATH, you will not be able to build the documentation])
fi
])# FP_PROG_XSLTPROC


# FP_DIR_DOCBOOK_XSL(XSL-DIRS)
# ----------------------------
# Check which of the directories XSL-DIRS contains DocBook XSL stylesheets. The
# output variable DIR_DOCBOOK_XSL will contain the first usable directory or
# will be empty if none could be found.
AC_DEFUN([FP_DIR_DOCBOOK_XSL],
[AC_REQUIRE([FP_PROG_XSLTPROC])dnl
if test -n "$XsltprocCmd"; then
  AC_CACHE_CHECK([for DocBook XSL stylesheet directory], fp_cv_dir_docbook_xsl,
  [FP_GEN_DOCBOOK_XML
  fp_cv_dir_docbook_xsl=no
  for fp_var in $1; do
     if $XsltprocCmd ${fp_var}/html/docbook.xsl conftest.xml > /dev/null 2>&1; then
        fp_cv_dir_docbook_xsl=$fp_var
        break
     fi
  done
  rm -rf conftest*])
fi
if test x"$fp_cv_dir_docbook_xsl" = xno; then
  AC_MSG_WARN([cannot find DocBook XSL stylesheets, you will not be able to build the documentation])
  DIR_DOCBOOK_XSL=
else
  DIR_DOCBOOK_XSL=$fp_cv_dir_docbook_xsl
fi
AC_SUBST([DIR_DOCBOOK_XSL])
])# FP_DIR_DOCBOOK_XSL


# FP_PROG_XMLLINT
# ----------------
# Sets the output variable XmllintCmd to the full path of the XSLT processor
# xmllint. XmllintCmd is empty if xmllint could not be found.
AC_DEFUN([FP_PROG_XMLLINT],
[AC_PATH_PROG([XmllintCmd], [xmllint])
if test -z "$XmllintCmd"; then
  AC_MSG_WARN([cannot find xmllint in your PATH, you will not be able to validate your documentation])
fi
])# FP_PROG_XMLLINT


# FP_CHECK_DOCBOOK_DTD
# --------------------
AC_DEFUN([FP_CHECK_DOCBOOK_DTD],
[AC_REQUIRE([FP_PROG_XMLLINT])dnl
if test -n "$XmllintCmd"; then
  AC_MSG_CHECKING([for DocBook DTD])
  FP_GEN_DOCBOOK_XML
  if $XmllintCmd --valid --noout conftest.xml > /dev/null 2>&1; then
    AC_MSG_RESULT([ok])
  else
    AC_MSG_RESULT([failed])
    AC_MSG_WARN([cannot find a DTD for DocBook XML V4.2, you will not be able to validate your documentation])
    AC_MSG_WARN([check your XML_CATALOG_FILES environment variable and/or /etc/xml/catalog])
  fi
  rm -rf conftest*
fi
])# FP_CHECK_DOCBOOK_DTD


# FP_GEN_FO
# ------------------
# Generates a formatting objects document in conftest.fo.
AC_DEFUN([FP_GEN_FO],
[rm -f conftest.fo
cat > conftest.fo << EOF
<?xml version="1.0"?>
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
  <fo:layout-master-set>
    <fo:simple-page-master master-name="blank">
      <fo:region-body/>
    </fo:simple-page-master>
  </fo:layout-master-set>
  <fo:page-sequence master-reference="blank">
    <fo:flow flow-name="xsl-region-body">
      <fo:block>
        Test!
      </fo:block>
    </fo:flow>
  </fo:page-sequence>
</fo:root>
EOF
]) # FP_GEN_FO


# FP_PROG_FOP
# -----------
# Set the output variable 'FopCmd' to the first working 'fop' in the current
# 'PATH'. Note that /usr/bin/fop is broken in SuSE 9.1 (unpatched), so try
# /usr/share/fop/fop.sh in that case (or no 'fop'), too.
AC_DEFUN([FP_PROG_FOP],
[AC_PATH_PROGS([FopCmd1], [fop])
if test -n "$FopCmd1"; then
  AC_CACHE_CHECK([for $FopCmd1 usability], [fp_cv_fop_usability],
    [FP_GEN_FO
    if "$FopCmd1" -fo conftest.fo -ps conftest.ps > /dev/null 2>&1; then
      fp_cv_fop_usability=yes
    else
      fp_cv_fop_usability=no
    fi
    rm -rf conftest*])
  if test x"$fp_cv_fop_usability" = xyes; then
     FopCmd=$FopCmd1
  fi
fi
if test -z "$FopCmd"; then
  AC_PATH_PROGS([FopCmd2], [fop.sh], , [/usr/share/fop])
  FopCmd=$FopCmd2
fi
AC_SUBST([FopCmd])
])# FP_PROG_FOP


# FP_PROG_FO_PROCESSOR
# --------------------
# Try to find an FO processor. PassiveTeX output is sometimes a bit strange, so
# try FOP first. Sets the output variables FopCmd, XmltexCmd, DvipsCmd, and
# PdfxmltexCmd.
AC_DEFUN([FP_PROG_FO_PROCESSOR],
[AC_REQUIRE([FP_PROG_FOP])
AC_PATH_PROG([XmltexCmd], [xmltex])
AC_PATH_PROG([DvipsCmd], [dvips])
if test -z "$FopCmd"; then
  if test -z "$XmltexCmd"; then
    AC_MSG_WARN([cannot find an FO => DVI converter, you will not be able to build DVI or PostScript documentation])
  else
    if test -z "$DvipsCmd"; then
      AC_MSG_WARN([cannot find a DVI  => PS converter, you will not be able to build PostScript documentation])
    fi
  fi
  AC_PATH_PROG([PdfxmltexCmd], [pdfxmltex])
  if test -z "$PdfxmltexCmd"; then
    AC_MSG_WARN([cannot find an FO => PDF converter, you will not be able to build PDF documentation])
  fi
elif test -z "$XmltexCmd"; then
  AC_MSG_WARN([cannot find an FO => DVI converter, you will not be able to build DVI documentation])
fi
])# FP_PROG_FO_PROCESSOR
