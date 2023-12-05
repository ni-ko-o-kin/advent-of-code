#!/usr/bin/env bash

FILE=$1

LIB="Maybe.roc Result.roc Stack.roc"

function rocDev {
    clear
    roc dev "${FILE}"
}

rocDev

while inotifywait -e close_write $1 $LIB
do
    rocDev
done
