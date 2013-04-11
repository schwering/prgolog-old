#!/bin/bash

TESTS="$(find ../ -maxdepth 2 -name \*.test.m)"

for TEST in $TESTS
do
        PARENT=$(echo $TEST | sed -e 's/test\.//g')
        sed --in-place -e "s/:- include_module test\\. //g" $PARENT
done

