#!/bin/sh

cat /etc/asusd/asusd.conf | jq -r '.gfx_mode'
