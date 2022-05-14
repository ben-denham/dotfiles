#!/bin/bash

LOG_FILE="$HOME/.emacs.d/pylsp-sh.log"
echo '' > $LOG_FILE
DIR_TO_PYLSP_FILE="$HOME/.emacs.d/dir-to-pylsp.txt"
DIR="$PWD"

if [ -f "$DIR_TO_PYLSP_FILE" ]; then
    CUSTOM_PYLSP=`grep "^$DIR" "$DIR_TO_PYLSP_FILE" | awk '{$1=""; print $0}'`
    if [ ! -z "$CUSTOM_PYLSP" ]; then
        echo "Found custom pylsp for dir '$DIR': $CUSTOM_PYLSP" >> $LOG_FILE
        bash -c "$CUSTOM_PYLSP" 2>> $LOG_FILE | tee $LOG_FILE
        exit
    fi
fi

echo "No custom pylsp found for dir '$DIR'" 2>> $LOG_FILE | tee $LOG_FILE
pylsp 2>> $LOG_FILE | tee $LOG_FILE
