#!/bin/bash
#aspell --per-conf=./aspell.conf --dont-backup -t -c main.tex;
aspell --per-conf=./aspell.conf --dont-backup -t -c $1
