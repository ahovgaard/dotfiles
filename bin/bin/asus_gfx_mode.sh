#!/bin/sh

cat /etc/supergfxd.conf | jq -r '.mode'
