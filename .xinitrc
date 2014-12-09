#!/bin/sh #
#User's .xinitrc file

#Merge custom X resources
xrdb -merge "${HOME}/.Xresources"

#Play a startup sound, in the background
#ogg123 -q "${HOME}/.config/awesome/login.ogg" &

#Start a terminal emulator in the background
urxvt -T Terminal &

#Start the window manager
#exec awesome &
exec xmodad &

# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

xrdb -load ~/.Xresources
export CLICOLOR=TRUE

#slim
login_cmd exec ck-launch-session /bin/bash -login /etc/X11/Xsession %session
