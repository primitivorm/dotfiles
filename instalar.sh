#!/bin/bash

#update sources
echo "actualizando fuentes..."
sudo apt update -qq -y

echo "instalando paquetes..."
#install packages
sudo apt install aptitude build-essential gcc g++ automake -y
sudo apt install make cmake flex bison libreadline-dev konsole -y
sudo apt install valgrind dos2unix net-tools libgtk-3-dev libglib2.0-dev -y
sudo apt install python python-dev python-urllib3 libxml2-dev libxslt-dev libssl-dev -y
sudo apt install tmux ack-grep astyle libtool libunistring-dev -y
sudo apt install dpkg-dev libgnome-keyring-dev -y
sudo apt install python-pip lcov ruby-coffee-script nodejs -y
sudo apt install libjansson-dev libcurl4-openssl-dev curl -y
sudo apt install libgccjit-5-dev xd mono-xbuild graphviz -y
sudo apt install dump gwhere gnuplot plotutils libappindicator1 -y
sudo apt install ninja-build uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev -y
sudo apt install libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config -y
sudo apt install libblocksruntime-dev autoconf systemtap-sdt-dev tzdata -y

# not essentials
# sudo apt install exuberant-ctags gnome-system-monitor clisp gnome-disk-utility -y
# sudo apt install java default-jre default-jdk indent texinfo libjpeg-dev libgif-dev giflib-tools libtiff5-dev -y
# sudo apt install lua5.2 liblua5.2-dev meld libz3 clang-format -y
# sudo apt install k3b vlc brasero libdvdcss libdvdread4 libdvdnav4 -y

# node
sudo apt install npm -y
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.1/install.sh | bash

# install docker
sudo apt install docker -y
sudo apt install docker.io -y

# install powerline
sudo apt install fonts-powerline powerline -y
sudo pip install powerline-gitstatus

#install nutty
sudo apt-add-repository ppa:bablu-boy/nutty-daily
sudo apt-get update
sudo apt-get install nutty

#to build ninja
sudo apt install re2c

#extras
sudo apt install ttf-liberation ttf-mscorefonts-installer -y

#remover paquetes temporales
sudo apt autoremove -qq -y

#install pip packages
sudo pip install --upgrade pip
sudo pip install --user cpp-coveralls --upgrade
sudo pip install flake8
sudo pip install powerline-status
sudo pip install pygments
sudo pip install sphinx sphinx-autobuild

#install ruby gems
sudo gem install grammars
sudo gem install pry

#########################################################
#install gnome-keyring
#########################################################
cd /usr/share/doc/git/contrib/credential/gnome-keyring
sudo make -j$(nproc)

#########################################################
#install atom
#########################################################
# if [ ! -f ~/Descargas/atom-amd64.deb ]; then
# 	cd ~/Descargas
# 	wget https://atom.io/download/deb/
# 	mv index.html atom-amd64.deb
# 	sudo dpkg -i atom-amd64.deb
# fi
