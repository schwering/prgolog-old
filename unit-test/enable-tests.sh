#!/bin/bash

TESTS="$(find ../ -maxdepth 2 -name \*.test.m)"

for TEST in $TESTS
do
        PARENT=$(echo $TEST | sed -e 's/test\.//g')
        MODULE=$(echo $PARENT | sed -e 's/^.\+\///g' | sed -e 's/\.m$//g')
        if [ "$(grep ':- include_module test\.' $PARENT)" == "" ]
        then
                sed --in-place -e "s/:- implementation\\./:- include_module test. :- implementation\\./g" $PARENT
        fi
done

