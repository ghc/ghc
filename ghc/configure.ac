dnl GHC-only part of fptools configuration
dnl 
dnl Copyright (c) 1999 Manuel M. T. Chakravarty <chak@acm.org>
dnl 
dnl This file is subject to the same free software license as GHC.

dnl ######################################################################
dnl Process this file with autoconf to produce a configure script.
dnl ######################################################################


dnl * Initialise and check sanity.
AC_INIT(ghc.spec.in)

dnl * Compute the version number.
version=`sed -e 's/.*\([[0-9]]\)\.\([[0-9]]*\).*/\1.\2/' VERSION`
patchlevel=1

dnl These are needed by the .spec file.
AC_SUBST(version)
AC_SUBST(patchlevel)

dnl Uncomment links to unix docs if necessary dnl NOTE: This is a slight
dnl hack: It needs to be synchronized with fptools/configure.ac and
dnl fptools/libraries/Makefile and should probably use target features
dnl instead of host.
AC_CANONICAL_HOST

case $host in
i[3456]86-*-mingw32*)
	unixHTMLStart="<!--"
	unixHTMLEnd="-->"
	;;
*)
	unixHTMLStart=""
	unixHTMLEnd=""
	;;
esac

AC_SUBST(unixHTMLStart)
AC_SUBST(unixHTMLEnd)

dnl Uncomment links to ObjectIO docs if necessary
AC_ARG_ENABLE(objectio,
[  --enable-objectio
        Build ObjectIO, a portable GUI library for Haskell.
],
[
ObjectIOHTMLStart=""
ObjectIOHTMLEnd=""
],
[
ObjectIOHTMLStart="<!--"
ObjectIOHTMLEnd="-->"
]
)

AC_SUBST(ObjectIOHTMLStart)
AC_SUBST(ObjectIOHTMLEnd)

dnl Uncomment links to HOpenGL docs if necessary
AC_ARG_ENABLE(hopengl,
[  --enable-hopengl
        Build HOpenGL, a Haskell binding for OpenGL/GLUT.
],
[
HOpenGLHTMLStart=""
HOpenGLHTMLEnd=""
],
[
HOpenGLHTMLStart="<!--"
HOpenGLHTMLEnd="-->"
]
)

AC_SUBST(HOpenGLHTMLStart)
AC_SUBST(HOpenGLHTMLEnd)

dnl * Write the results...
AC_OUTPUT(ghc.spec docs/libraries.html)
