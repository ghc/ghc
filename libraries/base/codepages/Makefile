
.PHONY: default
default:
	$(MAKE) getCodepages
	$(MAKE) genTable

.PHONY: getCodepages
getCodepages:
	rm -rf www.unicode.org
	rm -rf CPs
	mkdir CPs
	wget -r -np http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/
	find www.unicode.org -name 'CP*' -exec cp {} CPs \;

.PHONY: genTable
genTable:
	ghc --make MakeTable
	./MakeTable GHC.IO.Encoding.CodePage.Table ../GHC/IO/Encoding/CodePage/Table.hs CPs/*

