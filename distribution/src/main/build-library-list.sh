#!/bin/sh

if [ `dirname $0` != "distribution/src/main" ]; then
    echo "Run from base build directory, where the main pom.xml is housed"
    exit 1
fi


cd vcc-main
TEMP=../distribution/target/library-list.building
TARGET=../distribution/src/main/resources/library.lst

echo "vcc-main.jar" > ${TEMP}
mvn dependency:list -Pvcc-main -DincludeScope=runtime | grep compile | awk -F ":" '{ print $2 ".jar" }' >> ${TEMP}

#echo $TARGET
sort ${TEMP}  > ${TARGET}
rm ${TEMP}
echo "Create ${TARGET}."