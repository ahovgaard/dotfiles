#!/bin/bash

# Map escape to caps lock and set US international layout.

if lsusb | grep -i ergodox; then
    echo "Setting us-intl keymap config since Ergodox is connected..."
    setxkbmap -layout us -variant altgr-intl
else
    echo "Set key mapping since Ergo not connected..."
    setxkbmap -option caps:escape
    setxkbmap -layout us -variant altgr-intl

    if [ "$(cat /etc/hostname)" = 'arch-zf14g8' ]; then
        setxkbmap -option ctrl:menu_rctrl
        setxkbmap -option ctrl:swap_rwin_rctl
    fi
fi

