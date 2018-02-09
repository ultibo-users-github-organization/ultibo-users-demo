#!/bin/bash

# on raspbian, build the program and reboot to it

set -ex
ULTIBO=$HOME/ultibo/core
ULTIBOBIN=$ULTIBO/fpc/bin
export PATH=$ULTIBOBIN:$PATH
rm -rf lib/
fpc -B -O2 -Tultibo -Parm -CpARMV7a -WpRPI2B -Fi$ULTIBO/source/rtl/ultibo/core @$ULTIBOBIN/RPI2.CFG ultibousersdemo.lpr >& errors.log
sudo cp kernel7.img /boot/test-kernel7.img
sudo cp /boot/test-config.txt /boot/config.txt
sudo reboot
