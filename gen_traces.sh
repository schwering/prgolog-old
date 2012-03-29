#!/bin/bash

if [ "$*" == "" ]
then
        files=$(ls -1 traces/*.dat)
else
        files="$*"
fi

sed --in-place -e 's/set terminal .\+$/set terminal png size 2048, 1536/g' trace.gnuplot
for f in $files
do
        g=$(echo $f | sed -e 's/\//\\\//g')
        sed --in-place -e "s/\".\+\\.dat\"/\"$g\"/g" trace.gnuplot
        gnuplot trace.gnuplot >$f.trace.png
done

sed --in-place -e 's/set terminal .\+$/set terminal postscript/g' trace.gnuplot
for f in $files
do
        g=$(echo $f | sed -e 's/\//\\\//g')
        sed --in-place -e "s/\".\+\\.dat\"/\"$g\"/g" trace.gnuplot
        gnuplot trace.gnuplot >$f.trace.ps
done

