#!/bin/bash

OBF=dist/build/zimmerman14/zimmerman14
i=1

while : do
    $OBF -R $i
    if [ $? -eq 0 ]; then
        echo OK
    else
        echo FAIL
        mail -s "FAIL: depth $i" carmerb
        break
    fi
done
