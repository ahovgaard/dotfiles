#!/bin/bash
tmpbg='/tmp/screen.png'

scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
#[[ -f $1 ]] && convert "$tmpbg" $1 -gravity center -composite -matte "$tmpbg"
i3lock -i "$tmpbg"
rm "$tmpbg"
