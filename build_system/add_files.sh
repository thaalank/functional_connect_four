#!/bin/bash
# find all files with .ml extension, then replace ml with cmo.
# Allows us to compile program in Makefile
ls . | grep ".*\.ml$" | sed 's/ml$/cmo/' > ml_files.txt
mv ml_files.txt ./build_system/ml_files.txt