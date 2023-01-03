#!/usr/bin/env bash

FILE=$1

function rocDev {
    clear
    roc dev "${FILE}"
}

rocDev

while inotifywait -e close_write $1
do
    rocDev
done
