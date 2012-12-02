#! /bin/zsh

if [ $# -eq 1 ]
then
    wmctrl -d | while read line;
    do
        WSNUM=`echo $line | awk '{ print $1 }'`
        WSTAG=`echo $line | awk '{ print $10 }'`
        if [ "$1" = "$WSTAG" ]
        then
            wmctrl -s $WSNUM
        fi
    done
fi
