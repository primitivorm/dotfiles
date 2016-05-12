# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/src/julia:$HOME/bin:$PATH"
fi

# set environment variable for Latino
#PATH="$HOME/src/latino/codeblocks/bin:$PATH"

#set environment variables for bison
BISON_LOCALEDIR=/usr/local/share/locale/es/LC_MESSAGES
LATINO_HOME=~/src/latino/codeblocks/bin
LANG=es_MX.utf8
LANGUAGE=es_MX.utf8
NODE_PATH=$HOME/local/lib/node_modules
JULIA_DIR=$HOME/src/julia
LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu
