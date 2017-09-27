#!/bin/bash

## cf from http://programster.blogspot.com/2014/05/ubuntu-14-desktop-install-fira-sans-and.html

if [ ! -d ~/.fonts/Fira ]; then
	mkdir ~/.fonts/Fira
	cd ~/.fonts/Fira
	# install unzip just in case the user doesn't already have it.
	if [[ `uname` = Linux ]]; then
		sudo apt-get install unzip -y
		wget "http://www.carrois.com/downloads/fira_4_1/FiraFonts4106.zip"
		wget "http://www.carrois.com/downloads/fira_mono_3_2/FiraMonoFonts3206.zip"
		unzip FiraSans4106.zip
		unzip FiraMono3206.zip
		sudo mkdir -p /usr/share/fonts/truetype/FiraSans
		sudo mkdir -p /usr/share/fonts/opentype/FiraSans
		sudo cp Fira*/WEB/*.ttf /usr/share/fonts/truetype/FiraSans/
		sudo cp Fira*/OTF/Fira* /usr/share/fonts/opentype/FiraSans/
		sudo fc-cache -fv
	else
		wget https://github.com/mozilla/Fira/archive/4.202.zip
		unzip 4.202.zip
		font_dir="$HOME/Library/Fonts"
		mkdir -p $font_dir
		cp Fira-4.202/otf/* $font_dir/
	fi
fi
