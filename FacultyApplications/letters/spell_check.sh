#!/bin/bash
#aspell --per-conf=./aspell.conf --dont-backup -t -c _letter.tex;
aspell --per-conf=./aspell.conf --dont-backup -t -c $1;
