SOURCE = manuscript.qmd

all: pdf docx html

pdf: $(SOURCE)
	quarto render $< --to apaquarto-pdf \
	--output manuscript-$@.pdf \
	-M documentmode:man

docx: $(SOURCE)
	quarto render $< --to apaquarto-docx \
	--output manuscript-$@.docx

html: $(SOURCE)
	quarto render $< --to apaquarto-html \
	--output manuscript-$@.html
