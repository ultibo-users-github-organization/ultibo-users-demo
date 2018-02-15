#!/bin/bash
set -x

rm -rf testing/*
cp release/*.zip testing
pushd testing
unzip *.zip
rm *.zip
sudo cp test.h264 ultibo-users-demo-* config.txt /boot
sleep 5
sudo reboot
