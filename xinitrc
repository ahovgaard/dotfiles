#!/bin/sh
#
# ~/.xinitrc
#

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Load ~/.Xresources file
xrdb ~/.Xresources

# Run systemd as user instance
/usr/lib/systemd/systemd --user &

# Autostart urxvt daemon
urxvtd &

# Autostart dropboxd
dropboxd &

# Map escape to caps lock, set the cursor and set us international layout
setxkbmap -option caps:escape
xsetroot -cursor_name left_ptr
setxkbmap -layout us -variant altgr-intl

# Toggle ext monitor if connected
if (xrandr | grep "VGA1" | grep " connected"); then
  xrandr --output "LVDS1" --off --output "VGA1" --auto
  xrandr --newmode "1680x1050_60.00" \
  146.25 1680 1784 1960 2240  1050 1053 1059 1089 -hsync +vsync;
  xrandr --addmode VGA1 1680x1050_60.00;
  xrandr --output VGA1 --mode 1680x1050_60.00
fi

# Wallpaper
feh --no-fehbg --bg-center ~/.bg.jpg

# Start xmonad
exec dbus-launch --exit-with-session xmonad
