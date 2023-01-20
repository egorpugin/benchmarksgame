#!/bin/bash

# git clone ...
cd public/program

#for i in `find . -name "*.html"`; do
    #lynx --dump "$i" > "$i.txt"
#done

#for i in `find . -name "*.html.txt"`; do
for i in `find . -name "*.html"`; do
    echo processing $i
    lynx --dump "$i" > "$i.txt"
    i=$i.txt
    FROM=`grep -n "source code" "$i" | head -n 1 | cut -f1 -d:`
    TO=`grep -n "notes, command-line, and program output" "$i" | head -n 1 | cut -f1 -d:`
    PROG=`echo $i | cut -d- -f1`
    TYPE=`echo $i | cut -d- -f2`
    DIR=$PROG/$TYPE
    FN=`echo $i | cut -d- -f3`
    mkdir -p "$DIR" &> /dev/null
    #echo "$FROM,$TO"
    awk "NR>=$FROM+2&&NR<=$TO-2" "$i" > "$DIR/$FN.src"
    exit 0
done
