#!/bin/bash

#update sources
echo "actualizando fuentes..."
sudo apt-get update -qq -y

#remove some packages
sudo apt-get remove vim vim-runtime vim-tiny vim-common vim-gui-common -y

echo "instalando paquetes..."
#install packages
sudo apt-get install aptitude build-essential gcc g++ automake -y
sudo apt-get install git make cmake flex bison libreadline-dev -y
sudo apt-get install valgrind dos2unix exuberant-ctags -y
sudo apt-get install python python-dev libxml2-dev libxslt-dev libssl-dev -y
sudo apt-get install tmux ack-grep astyle libtool libunistring-dev -y
sudo apt-get install dpkg-dev libgnome-keyring-dev java -y
sudo apt-get install python-pip lcov npm ruby-coffee-script nodejs -y
sudo apt-get install default-jre default-jdk indent texinfo libjpeg-dev libgif-dev giflib-tools libtiff5-dev -y
sudo apt-get install lua5.2 liblua5.2-dev meld libz3 clang-format -y
sudo apt-get install gnome-system-monitor clisp libgtk-3-dev -y
sudo apt-get install libjansson-dev libcurl4-openssl-dev curl -y
sudo apt-get install libgccjit-5-dev xd mono-xbuild graphviz -y
sudo apt-get install konsole dump gwhere gnuplot plotutils -y
sudo apt-get install k3b vlc brasero libdvdcss libdvdread4 libdvdnav4 gnome-disk-utility -y
sudo apt-get install ninja-build uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev -y 
sudo apt-get install libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config -y
sudo apt-get install libblocksruntime-dev autoconf systemtap-sdt-dev tzdata -y

#necesarios para compilar gvim
sudo apt-get install libncurses5-dev libgnome2-dev libgnomeui-dev \
        libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
        libcairo2-dev libx11-dev libxpm-dev libxt-dev python-dev \
        ruby-dev mercurial -y

#extras
sudo apt-get install ttf-liberation ttf-mscorefonts-installer -y

#remover paquetes temporales
sudo apt-get autoremove -qq -y

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

if [ ! -d ~/src ]; then
    mkdir ~/src
fi
#########################################################
#install vim
#########################################################
if [ ! -d ~/src/vim ]; then
    	echo "compilando e instalando vim..."
	cd ~/src
	git clone https://github.com/vim/vim
	cd vim
	hg pull
	hg update
	cd src
	make distclean  # if you build Vim before
	autoreconf -i	# optional
	bash configure --with-features=huge \
		    --enable-multibyte \
		    --enable-rubyinterp \
		    --enable-pythoninterp \
		    --with-python-config-dir=/usr/lib/python2.7/config-x86_64-linux-gnu/ \
		    --enable-perlinterp \
		    --enable-luainterp \
		    --enable-gui=gnome2 --enable-cscope

	make -j$(nproc)
	sudo make install
fi

#########################################################
#install emacs
#########################################################
if [ ! -d ~/src/emacs ]; then
	echo "compilando e instalando emacs..."
	cd ~/src
	git clone https://github.com/emacs-mirror/emacs
	cd emacs
	bash configure
	make -j$(nproc)
	sudo make install
fi

#########################################################
#install atom
#########################################################
if [ ! -f ~/Descargas/atom-amd64.deb ]; then
	cd ~/Descargas
	wget https://atom.io/download/deb/
	mv index.html atom-amd64.deb
	sudo dpkg -i atom-amd64.deb
fi
