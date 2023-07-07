#!/bin/bash
if [ $# -ne 1 ]; then
        echo "Usage: $0 <path>"
        exit 1
fi

if [ ! -e $1 ]; then
        echo "No such path $1" 
        exit 2
fi

if [ -f $1 ]; then
        echo "1"
else
        dir=$1
        numFiles=`source num-files.sh`
        dirList=`./list-dirs.sh $dir`
        numItems=$numFiles
        for d in $dirList
        do
        numItems_rec=`$0 $dir/$d`
        ((numItems+=numItems_rec))
        done
        echo $numItems
fi
