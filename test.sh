#!/usr/bin/env bash

FILE=$1

function rocTest {
    clear
    roc dev "${FILE}"
}

rocTest

while inotifywait -e close_write $1
do
    rocTest
done
