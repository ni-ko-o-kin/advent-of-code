#!/usr/bin/env bash

FILENAME=$1

function cleanup {
    rm "${FILENAME%.*}"
}
trap cleanup EXIT


function rocDev {
    clear
    roc test "${FILENAME}"
}

rocDev

while inotifywait -e close_write $FILENAME
do
    rocDev
done
