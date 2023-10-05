TARGET    = fc-normalization-rta

$(TARGET).pdf: $(TARGET).tex $(OTT_TEX)
	latex -output-format=pdf $<
	latex -output-format=pdf $<

.PHONY: clean
clean:
	rm -f $(TARGET).pdf $(TARGET).tex $(OTT_TEX) *.aux *.fdb_latexmk *.log *.fls
