#!/bin/bash
SCREEN_WIDTH=$(xrandr | grep -Po --color=never "(?<=\ connected )[\d]+(?=x[\d]+)")
XPOS=600
TRAY_WIDTH=35
BAR_WIDTH=$(($SCREEN_WIDTH - $XPOS - $TRAY_WIDTH))
HEIGHT=16
FONT='-*-terminus-*-r-normal-*-*-80-*-*-*-*-iso8859-*'
#FONT='-*-inconsolata-medium-r-normal-*-12-*-*-*-*-*-*-*'
#FONT="-*-fixed-medium-r-s*--12-87-*-*-*-*-iso10???-1"

killall conky
conky ~/.conkyrc | dzen2 -x $XPOS -ta r -w $BAR_WIDTH \
  -fn $FONT -fg '#ffffff' -bg '#000000' -h $HEIGHT  &

killall trayer
trayer --edge top --align right --widthtype pixel --width $TRAY_WIDTH \
  --SetDockType true --SetPartialStrut true --transparent true --alpha 0 \
  --tint 0x000000 --expand true --heighttype pixel --height $HEIGHT &
