#!/bin/sh

~/bin/lock.sh &
sleep 3s;
systemctl suspend
