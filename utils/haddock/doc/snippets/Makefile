# \newcommand{\haddockbegindoc}{\hfill\\[1ex]}
HDK_BEGIN := \\\\newcommand{\\haddockbegindoc}{\\hfill\\\\\[1ex]}

# \newcommand{\haddockverb}{\small}
HDK_VERB := \\\\newcommand{\\haddockverb}{\\small}

# \newcommand{\haddocktt}[1]{{\small \texttt{#1}}}
HDK_TT := \\\\newcommand{\\haddocktt}[1]{{\\small \\\\texttt{\#1}}}

all: \
  Snippet-List-Bulleted.html \
  Snippet-List-Bulleted.tex \
  Snippet-List-Not-Separated.html \
  Snippet-List-Not-Separated.tex \
  Snippet-List-Enumerated.html \
  Snippet-List-Enumerated.tex \
  Snippet-List-Not-Newline.html \
  Snippet-List-Not-Newline.tex \
  Snippet-List-Multiline-Item.html \
  Snippet-List-Multiline-Item.tex \
  Snippet-List-Nested-Item.html \
  Snippet-List-Nested-Item.tex \
  Snippet-List-Indentation.html \
  Snippet-List-Indentation.tex \
  Snippet-List-Definition.html \
  Snippet-List-Definition.tex

clean:
	rm -f Lists*.html

Lists.html: Lists.hs
	haddock --html $^

Lists.tex: Lists.hs
	haddock --latex $^

# Using pandoc-3.1.3
Lists-Pretty.html: Lists.html
	pandoc $^ -o $@

# Using tex-fmt-0.4.1
# nix profile install github:wgunderwood/tex-fmt
Lists-Pretty.tex: Lists.tex
	cp $^ $@
	tex-fmt $@

Snippet-List-Bulleted.html: Lists-Pretty.html
	sed -n '48,52p;53q' $^ > $@

Snippet-List-Bulleted.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	sed -n '18,26p;27q' $^ >> $@

Snippet-List-Not-Separated.html: Lists-Pretty.html
	sed -n '59,60p;61q' $^ > $@

Snippet-List-Not-Separated.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	sed -n '33,36p;37q' $^ >> $@

Snippet-List-Enumerated.html: Lists-Pretty.html
	sed -n '68,72p;73q' $^ > $@

Snippet-List-Enumerated.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	sed -n '43,51p;52q' $^ >> $@

Snippet-List-Not-Newline.html: Lists-Pretty.html
	sed -n '80,89p;90q' $^ > $@

Snippet-List-Not-Newline.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	sed -n '58,74p;75q' $^ >> $@

Snippet-List-Multiline-Item.html: Lists-Pretty.html
	sed -n '97,100p;101q' $^ > $@

Snippet-List-Multiline-Item.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	sed -n '81,90p;91q' $^ >> $@

Snippet-List-Nested-Item.html: Lists-Pretty.html
	sed -n '108,127p;128q' $^ > $@

Snippet-List-Nested-Item.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	echo "$(HDK_VERB)" >> $@
	sed -n '97,141p;142q' $^ >> $@

Snippet-List-Indentation.html: Lists-Pretty.html
	sed -n '135,138p;139q' $^ > $@

Snippet-List-Indentation.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	sed -n '148,155p;156q' $^ >> $@

Snippet-List-Definition.html: Lists-Pretty.html
	sed -n '146,156p;157q' $^ > $@

Snippet-List-Definition.tex: Lists-Pretty.tex
	echo "$(HDK_BEGIN)" > $@
	echo "$(HDK_TT)" >> $@
	sed -n '162,170p;171q' $^ >> $@
