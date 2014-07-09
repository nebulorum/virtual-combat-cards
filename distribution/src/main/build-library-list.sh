#!/bin/sh

pwd
cd ../../vcc-main

if [ `dirname $0` != "distribution/src/main" ]; then
    echo "Run from base build directory"
    exit 1
fi

cd vcc-main
mvn dependency:list -Pvcc-main -DincludeScope=runtime | grep compile | awk -F ":" '{ print $2 ".jar" }' > ../distribution/src/main/resources
