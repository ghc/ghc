OTT_FILES = StgSyn.ott CostSem.ott
OTT_TEX   = StgOtt.tex
OTT_OPTS  = -tex_show_meta false
TARGET    = stg-spec

$(TARGET).pdf: $(TARGET).tex $(OTT_TEX)
	latex -output-format=pdf $<
	latex -output-format=pdf $<

$(TARGET).tex: $(TARGET).mng $(OTT_FILES)
	ott $(OTT_OPTS) -tex_filter $< $@ $(OTT_FILES)

$(OTT_TEX): $(OTT_FILES)
	ott -tex_wrap false $(OTT_OPTS) -o $@ $^

.PHONY: clean
clean:
	rm -f $(TARGET).pdf $(TARGET).tex $(OTT_TEX) *.aux *.fdb_latexmk *.log
