#!/bin/sh

# Map escape to caps lock and set US international layout.
setxkbmap -option caps:escape
setxkbmap -layout us -variant altgr-intl

if [ $(hostname) == 'zdk02-4g43' ]; then
  setxkbmap -option ctrl:menu_rctrl
  setxkbmap -option ctrl:swap_rwin_rctl
fi
