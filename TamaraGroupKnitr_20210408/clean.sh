#!/bin/bash
#
# This isn't a file I would usually have in a knitr project, but for
# a tutorial repo it seems useful.  This deletes everything that is
# automatically generated so you can start afresh.

rm *.pdf
rm *.log
rm *.aux
rm *.out
rm experiment_one.tex
rm experiment_two.tex
rm -Rf figure
rm R_scripts/data/*.Rdata
