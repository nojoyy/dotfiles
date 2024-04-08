#!/bin/sh
if [ -f $HOME/.cache/waybar-disabled ] ;then
    killall waybar
    pkill waybar
    exit 1
fi

killall waybar
pkill waybar
sleep 0.2

waybar -c ~/.config/waybar/config.jsonc -s ~/.config/waybar/style.css &
