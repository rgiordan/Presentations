#!/bin/bash

Rscript -e 'library(knitr); knit("experiment_one.Rnw")'
Rscript -e 'library(knitr); knit("experiment_two.Rnw")'
