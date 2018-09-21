#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# autostart urxvt daemon and kill it when the X server is reset on log-out
urxvtd -q -f -o

# set the cursor
xsetroot -cursor_name left_ptr

# map escape to caps lock and set US international layout
setxkbmap -option caps:escape
setxkbmap -layout us -variant altgr-intl

# disable PC speaker beep
xset -b

if [ $(hostname) == 't520-arch' ]; then
  # set wallpaper
  sh ~/.fehbg

  # set up environment variables for fcitx
  #export GTK_IM_MODULE=fcitx
  #export QT_IM_MODULE=fcitx
  #export XMODIFIERS=@im=fcitx

  # start fcitx
  #fcitx > /dev/null 2>&1

  # MPD daemon start (if no other user instance exists)
  #[ ! -s ~/.config/mpd/pid ] && mpd

  # redshift
  killall redshift-gtk
  redshift-gtk &

  # gnome keyring
  dbus-update-activation-environment --systemd DISPLAY
  eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
  export SSH_AUTH_SOCK
fi

# start window manager
exec i3
