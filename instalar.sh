#install packages
echo "instalando paquetes..."
apt-get update -qq -y
apt-get install aptitude build-essential gcc g++ automake clang-format -y
apt-get install git make cmake flex bison clang llvm llvm-dev lldb libc++-dev -y
apt-get install valgrind vim-gnome dos2unix exuberant-ctags -y
apt-get install python python-dev libxml2-dev libxslt-dev -y
apt-get install tmux ack-grep astyle libtool libunistring-dev -y
apt-get install dpkg-dev libgnome-keyring-dev java -y
apt-get install python-pip lcov npm ruby-coffee-script nodejs -y
apt-get install default-jre default-jdk indent -y
apt-get install lua5.2 liblua5.2-dev meld libz3 -y
apt-get install gnome-system-monitor clisp libgtk-3-dev -y
apt-get install libjansson-dev libcurl4-openssl-dev curl -y
apt-get install libgccjit-5-dev xd mono-xbuild -y
apt-get install konsole dump gwhere gnuplot plotutils -y
apt-get install k3b vlc brasero libdvdcss libdvdread4 libdvdnav4 gnome-disk-utility -y
apt-get install ninja-build uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev -y 
apt-get install libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config libblocksruntime-dev autoconf systemtap-sdt-dev tzdata -y
apt-get autoremove -qq -y

#install pip packages
pip install --upgrade pip
pip install --user cpp-coveralls --upgrade
pip install flake8
pip install powerline-status
pip install pygments
pip install sphinx sphinx-autobuild

gem install grammars
gem install pry

#########################################################
#install gnome-keyring
#########################################################
cd /usr/share/doc/git/contrib/credential/gnome-keyring
make -j$(nproc)


#########################################################
#install vim
#########################################################
if [ ! -d ~/src/vim ]; then
    echo "compilando e instalando vim..."
    apt-get install libncurses5-dev libgnome2-dev libgnomeui-dev \
        libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
        libcairo2-dev libx11-dev libxpm-dev libxt-dev python-dev \
        ruby-dev mercurial -y
    apt-get remove vim vim-runtime gvim -y
    apt-get remove vim-tiny vim-common vim-gui-common -y
	cd ~/src
	git clone https://github.com/vim/vim
	cd vim
	hg pull
	hg update
	cd src
	make distclean  # if you build Vim before
	autoreconf -i	# optional
	./configure --with-features=huge \
		    --enable-multibyte \
		    --enable-rubyinterp \
		    --enable-pythoninterp \
		    --with-python-config-dir=/usr/lib/python2.7/config-x86_64-linux-gnu/ \
		    --enable-perlinterp \
		    --enable-luainterp \
		    --enable-gui=gnome2 --enable-cscope

	make -j$(nproc)
	make install
fi
