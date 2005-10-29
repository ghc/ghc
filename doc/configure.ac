
AC_INIT([Haddock docs], [1.0], [simonmar@microsoft.com], [])

AC_CONFIG_SRCDIR([Makefile])

dnl ** check for DocBook toolchain
FP_CHECK_DOCBOOK_DTD
FP_DIR_DOCBOOK_XSL([/usr/share/xml/docbook/stylesheet/nwalsh/current /usr/share/xml/docbook/stylesheet/nwalsh /usr/share/sgml/docbook/docbook-xsl-stylesheets* /usr/share/sgml/docbook/xsl-stylesheets* /opt/kde?/share/apps/ksgmltools2/docbook/xsl /usr/share/docbook-xsl /usr/share/sgml/docbkxsl /usr/local/share/xsl/docbook /sw/share/xml/xsl/docbook-xsl])
FP_PROG_FO_PROCESSOR

AC_CONFIG_FILES([config.mk])
AC_OUTPUT
