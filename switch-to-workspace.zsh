#! /bin/zsh

# Check that the number of arguments is equal to one.
if [ $# -eq 1 ]
then
    wmctrl -d | while read line;
    do
        WSNUM=`echo $line | awk '{ print $1 }'`
        WSTAG=`echo $line | awk '{ print $NF }'`
        if [ "$1" = "$WSTAG" ]
        then
            wmctrl -s $WSNUM
        fi
    done
fi
