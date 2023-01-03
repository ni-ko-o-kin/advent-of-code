#!/usr/bin/env sh

clear

while inotifywait -e close_write $1
do
    clear
    roc dev $1
done
