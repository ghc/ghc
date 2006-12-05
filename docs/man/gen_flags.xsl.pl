#!/usr/bin/perl -w

use strict;

if ($#ARGV ne 1) {
    die "Usage: $0 <ghc commands> <libdir>\n"
}

my @ghc_commands = split / /, $ARGV[0];
my $libdir = $ARGV[1];

print <<'EOF';
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE xsl:stylesheet [
]>

<xsl:stylesheet version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns="http://www.w3.org/TR/xhtml1/strict">

<xsl:output method="text" omit-xml-declaration="yes" />

<xsl:template match="/">.\"
.\" This is a generated file.  Changes might get clobbered.  Edit at own's risk.
.\"
.TH GHC 1 "2002-10-25" "Glasgow FP Suite" "Glasgow Haskell Compiler"
.SH NAME
GHC \- the Glasgow Haskell Compiler


.SH SYNOPSIS
EOF

my $started = 0;

for my $ghc_command (@ghc_commands) {
    print ".br\n" if $started;
    $started = 1;
    print <<"EOF";
.B $ghc_command
.RI [ option | filename ]...
EOF
}

print <<'EOF';

.SH DESCRIPTION
This manual page documents briefly the
.B ghc
and
.B ghci
commands.
Note that
.B ghci
is not yet available on all architectures.
Extensive documentation is available in various other formats
including DVI, PostScript and HTML; see below.

.PP
Each of GHC's command line options is classified as either
.IR static " or " dynamic .
A static flag may only be specified on the command line, whereas a
dynamic flag may also be given in an \f(CROPTIONS\fP pragma in a
source file or set from the GHCi command-line with \f(CR:set\fP.

As a rule of thumb, all the language options are dynamic, as are the
warning options and the debugging options.

The rest are static, with the notable exceptions of
.BR \-v ", " \-cpp ", " \-fasm ", " \-fvia\-C ", and " \-#include .
The OPTIONS sections lists the status of each flag.

.PP
Common suffixes of file names for Haskell are:
.TP
.B .hs
Haskell source code; preprocess, compile
.TP
.B .lhs
literate Haskell source; unlit, preprocess, compile
.TP
.B .hi
Interface file; contains information about exported
symbols
.TP
.B .hc
intermediate C files
.TP
.BI . x _o
way
.I x
object files; common ways are:
.BR p ", " u ", " s
.TP
.BI . x _hi
way
.I x
interface files


.SH OPTIONS

<xsl:apply-templates select="sect1/sect2" mode="overview"/>

<xsl:apply-templates select="sect1/sect2"/>


.SH FILES
EOF
print ".I $libdir";
print <<'EOF';

.SH COPYRIGHT

Copyright 2002, The University Court of the University of Glasgow.
.br
All rights reserved.


.SH AUTHOR

This manual page was generated from the XML documentation of GHC with blood,
sweat, tears and a breaks-if-you-look-at-it-the-wrong-way XSL
stylesheet originally written by Michael Weber &lt;michaelw@debian.org&gt;
for the Debian GNU/Linux system (but may be used by others).

.\" End
</xsl:template>


<xsl:template match="sect1/sect2" mode="overview">
<xsl:choose>
<xsl:when test="contains(title/.,' (')">
.SS <xsl:value-of select="substring-before(title/.,' (')"/>
</xsl:when>
<xsl:otherwise>
.SS <xsl:value-of select="title/."/>
</xsl:otherwise>
</xsl:choose>
.nh
<xsl:apply-templates select="informaltable/tgroup/tbody/row" mode="overview"/>
.hy
</xsl:template>


<xsl:template match="sect1/sect2">
<xsl:choose>
<xsl:when test="contains(title/.,' (')">

.SH <xsl:value-of select='translate(substring-before(title/.," ("),"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")'/>

</xsl:when>
<xsl:otherwise>

.SH <xsl:value-of select='translate(title/.,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")'/>

</xsl:otherwise>
</xsl:choose><xsl:text>
</xsl:text>
<xsl:apply-templates select="informaltable/tgroup/tbody/row"/>
</xsl:template>


<xsl:template match="informaltable/tgroup/tbody/row" mode="overview">
  <xsl:apply-templates select="entry[1]|entry[4]" mode="overview"/>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="informaltable/tgroup/tbody/row">
.TP
<xsl:apply-templates select="entry[1]"/><xsl:text>
</xsl:text>
<xsl:variable name="x">
<xsl:apply-templates select="entry[2]"/>
</xsl:variable>
<xsl:value-of select="normalize-space($x)"/>
.rj
[<xsl:apply-templates select="entry[3]"/>]
<!-- IGNORE NEGATIVE OPTIONS
<xsl:if test="not(entry[4]='-')">
  <xsl:text>.TP
</xsl:text>
  <xsl:apply-templates select="entry[4]/option"/>
</xsl:if>
 -->
</xsl:template>


<xsl:template match="option" mode="escape-dash">
  <xsl:variable name="x">
    <xsl:value-of select="."/>
  </xsl:variable>
  <xsl:variable name="y">
    <xsl:call-template name="replace-string">
      <xsl:with-param name="text" select="$x"/>
      <xsl:with-param name="from" select="'-'"/>
      <xsl:with-param name="to" select="'\-'"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:value-of select="$y"/>
</xsl:template>

<xsl:template match="option" mode="overview">
  <xsl:apply-templates select="." mode="escape-dash"/>
</xsl:template>

<xsl:template match="option">
  <xsl:text>\fB</xsl:text>
  <xsl:apply-templates select="." mode="escape-dash"/>
  <xsl:text>\fP</xsl:text>
</xsl:template>


<xsl:template match="entry[1]" mode="overview">
  <xsl:apply-templates mode="overview"/>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="entry[1]">
  <xsl:apply-templates/><xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="entry[4]" mode="overview">
  <xsl:if test="not(.='-')">
    <xsl:apply-templates select="option" mode="overview"/>
    <xsl:text> </xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template match="entry[4]">
  <xsl:if test="not(.='-')">
    <xsl:value-of select="."/><xsl:text> </xsl:text>
  </xsl:if>
</xsl:template>


<xsl:template match="replaceable" mode="overview">
  <xsl:apply-templates select="."/>
</xsl:template>

<xsl:template match="replaceable">
  <xsl:text>\fI</xsl:text>
  <xsl:value-of select='.'/>
  <xsl:text>\fP</xsl:text>
</xsl:template>


<xsl:template match="literal">
  <xsl:text>\f(CR</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>\fP</xsl:text>
</xsl:template>



<!-- reusable replace-string function -->
  <xsl:template name="replace-string">
     <xsl:param name="text"/>
     <xsl:param name="from"/>
     <xsl:param name="to"/>

     <xsl:choose>
       <xsl:when test="contains($text, $from)">

         <xsl:variable name="before" select="substring-before($text, $from)"/>
         <xsl:variable name="after" select="substring-after($text, $from)"/>
         <xsl:variable name="prefix" select="concat($before, $to)"/>

         <xsl:value-of select="$before"/>
         <xsl:value-of select="$to"/>
         <xsl:call-template name="replace-string">
           <xsl:with-param name="text" select="$after"/>
           <xsl:with-param name="from" select="$from"/>
           <xsl:with-param name="to" select="$to"/>
         </xsl:call-template>
       </xsl:when>
       <xsl:otherwise>
         <xsl:value-of select="$text"/>
       </xsl:otherwise>
     </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
EOF

