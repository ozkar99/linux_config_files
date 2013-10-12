#!/bin/sh

xrdb ~/.Xresources
feh --bg-fill ~/.wallpaper.*
setxkbmap us_intl

while sleep 1
  do xsetroot -name "$(dwm_status)";
done & exec dwm;
