.PHONY: all
all: main.pdf

main.pdf: figures_knitr.tex
	latexmk main

figures_knitr.tex:
	Rscript -e 'library(knitr); knit("figures_knitr.Rnw")'


.PHONY: recompile_knitr
recompile_knitr:
	Rscript -e 'library(knitr); knit("figures_knitr.Rnw")'
	latexmk main


###################################
# Cleaning

.PHONY: clean_tex
clean_tex:
	for latextext in aux bbl blg log out pdf ; do \
    	rm -f main.$$latextext; \
	done

.PHONY: clean_knitr
clean_knitr:
	rm -f figures_knitr.tex
	rm -f figure/*
	rm -f cache/*

.PHONY: clean
clean: clean_knitr clean_tex