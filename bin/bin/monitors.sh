#!/bin/bash
TOGGLE=/tmp/monitors_toggle

if [ $(hostname) == 't520-arch' ]; then
  LAPTOP=LVDS1
  LEFT=VGA1
  RIGHT=DP1
elif [ $(hostname) == 'zdk02-4g43' ]; then
  LAPTOP=eDP-1
  LEFT=DP-5
  RIGHT=DP-4
else
  echo "Unknown host"
  exit
fi

# toggle monitors if connected and not already toggled
if [[ $(xrandr | grep $RIGHT) != *disconnected* ]] && [ ! -f $TOGGLE ]; then
  touch $TOGGLE
  xrandr --output $LAPTOP --off
  xrandr --output $LEFT --auto --output $RIGHT --auto --primary --right-of $LEFT
else
  rm -f $TOGGLE
  xrandr --output $LEFT --off --output $RIGHT --off
  xrandr --output $LAPTOP --auto
fi

if [ $(hostname) == 't520-arch' ]; then
  # set wallpaper again
  sh ~/.fehbg
fi

# restart i3
i3-msg restart

# fix keymap in case broken
keymap.sh
