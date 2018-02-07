#!/bin/bash
set -e # exit script on any error

VERSION=v$(date +%Y.%m.%d.%H%M)
PREFIX=ultibo-users-demo-also-needs-firmware
ZIPFILE=$PREFIX-$VERSION.zip
PATH=$HOME/hub-linux-arm-2.3.0-pre10/bin:$PATH

./build.sh
cp kernel7.img ultibo-users-demo-kernel7.img
zip $ZIPFILE ultibo-users-demo-kernel7.img ultibo-users-demo-config.txt ultibo-users-demo-cmdline.txt

hub release create -d -p -m "ultibo-users-demo $VERSION" -a $ZIPFILE $VERSION
