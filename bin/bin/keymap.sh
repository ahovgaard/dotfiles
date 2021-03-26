#!/bin/bash

# Map escape to caps lock and set US international layout.

if lsusb | grep Ergodox; then
    echo "Skipping keymap config since Ergodox is connected..."
else
    echo "Set key mapping since Ergo not connected..."
    setxkbmap -option caps:escape
    setxkbmap -layout us -variant altgr-intl

    if [ "$(hostname)" = 'zdk02-4g43' ]; then
        setxkbmap -option ctrl:menu_rctrl
        setxkbmap -option ctrl:swap_rwin_rctl
    fi
fi

